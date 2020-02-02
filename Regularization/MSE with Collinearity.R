two_beta_mse<-function(cor, n, beta, range, by, type = c("LASSO", "Ridge"), t, reps){
  sigma<-matrix(c(1,cor,
                  cor, 1), byrow = TRUE, nrow = 2)
  
  x<-MASS::mvrnorm(n = n, mu = c(0,0), Sigma = sigma )
  x<-as.matrix(x)
  beta<-beta
  y<-x%*%beta+rnorm(n, 0, sd = sqrt(1-(sum(beta^2))))
  

  
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
  
  beta_reg<-c(beta1_min, beta2_min)
  
 mse<- map_dfr(1:reps, function(rep){
    
    x_out<-MASS::mvrnorm(n = n, mu = c(0,0), Sigma = sigma )
    y_out<-x_out%*%beta+rnorm(n, 0, sqrt(1-(sum(beta^2))))
    
    y_hat<-x_out%*%beta_reg
    
    mse<-sqrt(mean((y_out-y_hat)^2))
    
    data.frame(Iteration = rep, 
               mse = mse)
    
  })
 true_mse<-sqrt(1-(sum(beta^2)))
 message("TRUE MSE = ", true_mse)
 mse
  
}


map(seq(0, .9, by = .1), ~two_beta_mse(., 100, c(.3, .8), c(-5, 5), by = .05, type = "Ridge", t = .5, reps = 1))

