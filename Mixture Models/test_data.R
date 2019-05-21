library(tidyverse)
library(lme4)
library(purrr)
# tau1 = .5 tau2 = .5

# Standard normal pred
x1<-rnorm(5000)
x2<-rnorm(5000)

# Fixed effect
b0 <- 20
b1 <- .40

# Group Effects
v01<- 5
v11<- .4
v02<- -5
v12<- -.5

# Covariance parameters
u001<- 1
u002<- 1

u101<- 1
u102<- 1

u011<-.33
u012<-.77

# Common residuals variance
sigma<-rnorm(10000, 0, .74)

# simming random effects based on above params
ref1<-MASS::mvrnorm(500, c(v01, v11), Sigma = matrix(c(u001,u011,
                                                       u011, u101),2,2))
ref2<-MASS::mvrnorm(500, c(v02, v12), Sigma = matrix(c(u002,u012,
                                                       u012, u102),2,2))


#Generating Dataframe for each level
l1<-data.frame(id = rep(1:1000, each = 10), x = c(x1, x2))
l2<-data.frame(id = rep(1:1000), 
               r_int=c(ref1[,1],ref2[,1]),
               r_slope=c(ref1[,2], ref2[,2]))

# Joining and creating outcomes
test<-left_join(l1,l2)%>%
  mutate(y = b0+r_int+x*b1+x*r_slope+sigma)
