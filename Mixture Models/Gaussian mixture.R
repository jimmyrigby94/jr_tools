library(tidyverse)

y<-c(rnorm(1000), rnorm(1000, -10), rnorm(1000, 10))


gauss_mix<-function(y, k){
  
  n<-length(y)
  
  y<-y[order(y)]
  
  bins<-split(y, c(rep(1:k, each = length(y)%/%k), rep(k, length(y)%%k)))
  
  mu<-map_dbl(bins, mean)
  sd<-map_dbl(bins, ~sd(.))
  lambda<-rep(1/k, times = k)
  
  post<-matrix(nrow = length(y), ncol = k)
  for(i in 1:k){
    post[,i]<- lambda[i]*dnorm(y, mu[i], sd[i])
  }
  
  ll<-sum(rowSums(post))
  post<-post/rowSums(post)
  diff<-1
  iter<-0
  
  while (abs(diff)>1e-8){
    
   if(iter ==1390){
     browser()
   }
   ll_old<-ll
   lambda<-colMeans(post)

   for(i in 1:k){
     
     mu[i]<-sum(post[,i]*y)/sum(post[,i])
     
     sd[i]<-sqrt(sum((post[,i]*(y-mu[i]))^2)/(n-k))
     
     post[,i]<- lambda[i]*dnorm(y, mu[i], sd[i])
    
   }
   
   ll<-sum(rowSums(post))
   post<-post/rowSums(post)
   diff<-ll-ll_old
   iter<-iter+1
  }
  
  vcov_o<-list()
  se<-vector("numeric", k)
  # # Observed information
  # for(i in 1:k){
  #   vcov_o[[i]]<-sum(post[,i])/sd[i]^2
  #   se[[i]]<-sqrt(1/vcov_o[[i]])
  # }
  
  for (i in 1:k) {
    vcov_o[[i]]<-t(post[,i]*(y-mu[i])/sd[i]^2)%*%(post[,i]*(y-mu[i])/sd[i]^2)
    se[[i]]<-sqrt(1/vcov_o[[i]])
  }
  
  
  list(mu = mu, 
       sd = sd,
       se = se,
       vcov = vcov_o,
       post = post, 
       ll = ll, 
       iter = iter)
}

s<-gauss_mix(y, 3)

fit<-mixtools::normalmixEM(y,k = 3)
se<- mixtools::boot.se(fit, B = 1000)

se$mu.se
s$se
