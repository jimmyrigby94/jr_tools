library(tidyverse)
sq_exp_kernel<-function(x1, x2, l = 1){
  Sigma<-matrix(rep(0, length(x1)*length(x2)), nrow = length(x1))
  
  for (i in seq_along(x1)){
    for (j in seq_along(x2)){
      
      Sigma[i, j]<-exp(-0.5*(abs(x2[i]-x1[j])/l)^2)
      
    }
  }
  
  Sigma
}




draw<-function(n_draws, x, mu, cov){
  values<-map_dfr(1:n_draws, function(sample){
    
    
    draw<-t(MASS::mvrnorm(1, 
                          mu = mu, 
                          Sigma = cov))
    
    draw<-as.vector(draw)
    
    tmp<-data.frame(sample = sample, 
                    x = x,
                    value = draw)
    
    tmp
  })
  
  values
}

post<-function(x, x.star, y, sigma.n=0, kernel = sq_exp_kernel){
  
  k.xx <- kernel(x,x)
  k.xxs <- kernel(x,x.star)
  k.xsx <- kernel(x.star,x)
  k.xsxs <- kernel(x.star,x.star)
  
  f.star.bar<-k.xsx%*%solve(k.xx+sigma.n^2*diag(1, ncol(k.xx)))%*%y
  
  f.star.cov<- k.xsxs - k.xsx%*%solve(k.xx+sigma.n^2*diag(1, ncol(k.xx)))%*%k.xxs
  
  f.star.bar<-as.vector(f.star.bar)
  
  list(post_mean = f.star.bar,
       post_var = f.star.cov)
}

cisco_cfs <- read_delim("Gaussian Proccesses/Cisco_cfs.tab",
                        "\t", 
                        escape_double = FALSE, 
                        trim_ws = TRUE)

cisco_cfs<-cisco_cfs%>%
  mutate(year = lubridate::year(date))%>%
  dplyr::select(date, year, cfs)%>%
  arrange(date)%>%
  group_by(year)%>%
  mutate(day = 1:n())%>%
  ungroup()

missing<-(1:nrow(cisco_cfs))[is.na(cisco_cfs$cfs)]

cisco_cfs$cfs[missing]<-cisco_cfs$cfs[missing-1]

cisco_cfs<-split(cisco_cfs, f = cisco_cfs$year)


test<-sample_n(cisco_cfs[[1]], 50)
x.star<-1:365
# Calculate the covariance matrix
Sigma <- sq_exp_kernel(x.star,x.star)

prior<-draw(n_draws = 20, x.star, rep(0, length(x.star)), cov = Sigma)

prior%>%ggplot(aes(y = value, x = x, color = as.factor(sample)))+geom_line()

post_params<-post(test$day, 1:365, test$cfs)


  ggplot(data = NULL, aes(x = test$day, y = post_params$post_mean))+
    geom_point()

post<-draw(50, x = x.star, mu = post_params$post_mean, cov = post_params$post_var)


ggplot(draws,aes(x=x,y=value)) +
  geom_line(aes(group=sample), colour="grey80")+
  geom_line(data = cisco_cfs, aes(x = day, y = cfs_m, color = as.factor(year)))+
  geom_line(data = data.frame(x = cisco_cfs$day,
                              y = post_draw$post_mean),
            aes(x=x,y=y, group = NULL),colour="red", size=1, inherit.aes = FALSE) + 
  theme_bw() +
  labs(x="Days",
       y = "CFS")
