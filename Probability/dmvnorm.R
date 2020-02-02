# Function for the density of a multivariate normal
dmvnorm<-function(x, mu, Sigma){
  
  p_x<-1/(sqrt(det(2*pi*Sigma)))*exp(-1/2*t(x-mu)%*%Sigma%*%(x-mu))
  
  as.vector(p_x)
}

# Example
obs_x<-c(15, 70)
mu<-c(15, 70)
sigma<-matrix(c(25, 13,
                 13 , 25), byrow = TRUE, ncol = 2)


dmvnorm(obs_x, mu, Sigma = sigma)


# Verification
mixtools::dmvnorm(obs_x, mu, sigma)

library(tidyverse)
data.frame(x1 = seq(13, 17, by = .01))%>%
  rowwise()%>%
  mutate(d = dmvnorm(c(x1, 70), mu, sigma))%>%
  ggplot(aes(x = x1, y = d))+
  geom_line()
