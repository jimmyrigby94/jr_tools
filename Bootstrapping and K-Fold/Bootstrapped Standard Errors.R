library(foreach)
library(doParallel)
library(tidyverse)
library(lme4)

detectCores()
registerDoParallel(4)

# Generates bootstrapped SE for any model that is compatible with the coef() function
# Capitalizes on match call and elipse to pass arguments to the focal model
# iter controls the number of iterations
# loo_boot (leave one out boot) requests MSE to be calculated for a given iteration observations NOT included in the training sample
# packages argument defines the packages that contains the defined fun
# packages are not available within the parallel environments and thus need to be loaded

boot_param_pl<-function(data, iter = 10, loo_boot = TRUE, fun = "lm", packages = NULL, ...){
  my_call<-match.call(expand.dots = TRUE)
  my_call[1]<-call(fun)
  my_call$iter<-my_call$fun<-my_call$loo_boot<-NULL
  if(fun == "lmer"){
    re_mod<-TRUE
  }else{
    re_mod<-NULL
  }
  
  packages<-c(packages, "dplyr")
  
  n<-nrow(data)
  

    mcoef<- foreach(i=1:iter, .combine = rbind, .packages = packages) %dopar% {
      temp<-sample_n(data, size = n, replace = TRUE)
      my_call$data<-temp
      mod<-eval(my_call)
      coef(mod)
    }
  
  as.data.frame(mcoef)%>%
    gather(key = "Parameter", value = "value")%>%
    group_by(Parameter)%>%
    dplyr::summarise(Estimate = mean(value),
                     SE = sd(value, na.rm = TRUE))%>%
    mutate(t_value = Estimate/SE,
           df = n-ncol(mcoef),
           p_value = dt(t_value, df = df))
  
}

boot_param_pl(test, fun = "lm", formula = y~x, iter = 1000, loo_boot = TRUE)
