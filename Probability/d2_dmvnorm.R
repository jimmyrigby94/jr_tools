source("dmvnorm.R")

d2_dmvnorm<-function(x, mu, Sigma){
  S_neg1<-solve(Sigma)
  dmvnorm(x, mu, Sigma)*(S_neg1%*%(x-mu)%*%t(x-mu)%*%S_neg1-S_neg1)
}


# Example
obs_x<-c(15, 70)
mu<-c(15, 70)
sigma<-matrix(c(25, 0,
                0 , 25), byrow = TRUE, ncol = 2)

d2_dmvnorm(obs_x, mu, sigma)
