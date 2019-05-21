library(foreach)
library(doParallel)
library(tidyverse)
library(lme4)

### Implements a general bagged estimates function that can use parallel processing ###
# Capitalizes on match call and elipse to pass arguments to the focal model
# iter controls the number of iterations
# loo_boot (leave one out boot) requests MSE to be calculated for a given iteration observations NOT included in the training sample

detectCores()
registerDoParallel(4)
bagged_estimates<-function(data, iter = 10, fun = "lm", packages = NULL, ...){
  my_call<-match.call(expand.dots = TRUE)
  my_call[1]<-call(fun)
  my_call$iter<-my_call$fun<-my_call$loo_boot<-NULL
  if(fun == "lmer"){
    re_mod<-TRUE
  }else{
    re_mod<-NULL
  }
  packages<-c(packages)
  
  n<-nrow(data)
  pred<-list()
  
  
    out_val<- foreach(i=1:iter, .packages = packages, .combine = cbind) %dopar% {
      cat("Iteration", i, "\r", sep = " ")
      temp<-dplyr::sample_n(data, size = n, replace = TRUE)
      my_call$data<-temp
      mod<-eval(my_call)
      predict(mod, data, allow.new.levels = re_mod)
    }

  rowMeans(out_val)
}


bagged_estimates(test, fun = "lmer", formula = y~x+(1+x|id), iter = 20, loo_boot = TRUE, packages = "lme4")


stopImplicitCluster()

