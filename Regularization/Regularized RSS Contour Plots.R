#################################################################
### Generate Regularized Contour Plots for Regression Weights ###
#################################################################
library(tidyverse)

####################
#### Parameters ####
####################
# cor: correlation between IVS
# beta: standardized regression coefficients
# range: numeric vector of length 2 defining xmin/ymin and xmax/ymax (only supports square plots)
# type: character value specifying regularization method either LASSO or Ridge
# t: constraint for betas LASSO: sum(abs(beta))<= t or sum(beta^2) <= t
# binwidth_rss: contour binwidth for RSS contours
# bindwithd_const: bindwith for constraint contrours


two_beta_RSS_contour<-function(cor, n, beta, range, by, type = c("LASSO", "Ridge"), t, binwidth_rss= 20, binwidth_const = 1){
  sigma<-matrix(c(1,cor,
                  cor, 1), byrow = TRUE, nrow = 2)
  
  x<-MASS::mvrnorm(n = n, mu = c(0,0), Sigma = sigma )
  x<-as.matrix(x)
  beta<-beta
  y<-x%*%beta+rnorm(n, 0, 1-(sum(beta^2)))
  
  
  beta_hat<-expand.grid(b1 = seq(range[1],range[2], by = by), b2 = seq(range[1],range[2], by = by))
  
  out<-pmap_dfr(beta_hat, function(b1, b2){
    
    betas<-c(b1, b2)
    
    y_hat<-x%*%betas
    
    r<-y_hat-y
    
    data.frame(beta1 = betas[[1]], beta2 = betas[[2]], rss = sum(r^2))
  })
  
  const<-out%>%
    rowwise()%>%
    mutate(abs_beta = if_else(type == "LASSO", 
                              sum(abs(c(beta1, beta2))),
                              sum(c(beta1, beta2)^2)))%>%
    filter(abs_beta<t)%>%
    ungroup()%>%
    mutate(min_x = if_else(rss == min(rss), beta1, as.numeric(NA)),
           min_y = if_else(rss == min(rss), beta2, as.numeric(NA)),
           min_rss = if_else(rss == min(rss), rss, as.numeric(NA)))
  
  beta1_min<-round(const$min_x[!is.na(const$min_x)], 2)
  beta2_min<-round(const$min_y[!is.na(const$min_x)], 2)
  rss_min<-round(const$min_rss[!is.na(const$min_x)], 2)
  
suppressMessages(  ggplot2::ggplot(out, aes(x=beta1, y = beta2, z = rss))+
    geom_contour(binwidth = binwidth_rss)+
    geom_contour(data = const, aes(x = beta1, y = beta2, z = rss), color = "green", binwidth = binwidth_const)+
    geom_point(data = NULL, aes(x = beta1_min, y = beta2_min))+
    geom_text(data = NULL, aes(x = range[2]*.75, y = range[1]*.75, label = paste("Beta 1:", 
                                                                    beta1_min,
                                                                    "\nBeta 2",
                                                                    beta2_min, 
                                                                    "\nRSS:", 
                                                                    rss_min)),
              hjust = "left")+
    labs(title = paste("RSS Contour Plot with", type, "Constraint"),
         subtitle = paste("Collinearity:", cor, "; t =", t), 
         x = "Beta 1", 
         y = "Beta 2"))
}


two_beta_RSS_contour(.3, 100, c(.3, .8), c(-5, 5), by = .05, type = "Ridge", t = .5, binwidth_rss = 20)
two_beta_RSS_contour(.7, 100, c(.3, .8), c(-5, 5), by = .05, type = "Ridge", t = .5, binwidth_rss = 20)
two_beta_RSS_contour(.8, 100, c(.3, .8), c(-5, 5), by = .05, type = "Ridge", t = .5, binwidth_rss = 20)
two_beta_RSS_contour(.9, 100, c(.3, .8), c(-5, 5), by = .05, type = "Ridge", t = .5, binwidth_rss = 20)


two_beta_RSS_contour(.3, 100, c(.3, .8), c(-5, 5), by = .05, type = "LASSO", t = .5, binwidth_rss = 20)
two_beta_RSS_contour(.7, 100, c(.3, .8), c(-5, 5), by = .05, type = "LASSO", t = .5, binwidth_rss = 20)
two_beta_RSS_contour(.8, 100, c(.3, .8), c(-5, 5), by = .05, type = "LASSO", t = .5, binwidth_rss = 20)
two_beta_RSS_contour(.9, 100, c(.3, .8), c(-5, 5), by = .05, type = "LASSO", t = .5, binwidth_rss = 20)
