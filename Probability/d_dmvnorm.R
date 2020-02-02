# Depends on the multivariatenormal density
source("dmvnorm.R")

# Calcuate first derivatives of multivariate normal
d_dmvnorm<-function(x, mu, Sigma){
  -dmvnorm(x=x, mu = mu, Sigma = Sigma)*(solve(Sigma)%*%(x-mu))
}

# Example
obs_x<-c(14, 70)
mu<-c(15, 70)
sigma<-matrix(c(15, 0,
                0 , 15), byrow = TRUE, ncol = 2)


d_dmvnorm(x = obs_x, mu = mu, Sigma = sigma)


library(tidyverse)
data.frame(x1 = seq(14.5, 15.5, by = .01))%>%
  rowwise()%>%
  mutate(d = dmvnorm(c(x1, 70), mu, sigma),
         d1 = d_dmvnorm(c(x1, 70), mu, sigma)[1]*20)%>%
  ggplot(aes(x = x1, y = d))+
  geom_line()+
  geom_line(aes(x = x1, y = d1), color = "red")+
  labs(x = "X1", 
       y = "Density", 
       title = "Plot of density and First Derivates",
       subtitle = "Derivative rescaled by 20")



