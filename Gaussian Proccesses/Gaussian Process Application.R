sq_exp_kernel<-function(x1, x2, l = 1){
  Sigma<-matrix(rep(0, length(x1)*length(x2)), nrow = length(x1))
  
  for (i in seq_along(x1)){
    for (j in seq_along(x2)){
      
      Sigma[i, j]<-exp(-0.5*(abs(x2[j]-x1[i])/l)^2)
      
    }
  }
  
  Sigma
}


seasonality_kernel<-function(x1, x2, a=1, b=12, l = 1){
  Sigma<-matrix(rep(0, length(x1)*length(x2)), nrow = length(x1))
  
  for (i in seq_along(x1)){
    for (j in seq_along(x2)){
      
      Sigma[i, j]<-a^2*exp(-0.5*(sin(b*(x2[j]-x1[i]))/l)^2)
      
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

cisco_cfs <- read_delim("C:/Users/jimmy/Downloads/Cisco_cfs.tab",
                        "\t", 
                        escape_double = FALSE, 
                        trim_ws = TRUE)

cisco_cfs<-cisco_cfs%>%
  mutate(day = 1:n(), 
         year = lubridate::year(date))%>%
  dplyr::select(date, year, day, cfs)

missing<-(1:nrow(cisco_cfs))[is.na(cisco_cfs$cfs)]

cisco_cfs$cfs[missing]<-cisco_cfs$cfs[missing-1]


post_draw<-post(x = cisco_cfs$day, x.star = cisco_cfs$day, y = cisco_cfs$cfs)


draws<-draw(20, x = cisco_cfs$day, post_draw$post_mean, cov = post_draw$post_var)


ggplot(draws,aes(x=x,y=value)) +
  geom_line(aes(group=sample), colour="grey80")+
  geom_line(data = cisco_cfs, aes(x = day, y = cfs_m, color = as.factor(year)))+
  geom_line(data = data.frame(x = cisco_cfs$day,
                              y = post_draw$post_mean),
            aes(x=x,y=y, group = NULL),colour="red", size=1, inherit.aes = FALSE) + 
  theme_bw() +
  labs(x="Days",
       y = "CFS")
