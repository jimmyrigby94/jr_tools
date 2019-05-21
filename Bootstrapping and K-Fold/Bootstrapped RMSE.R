# Function that returns a bootstrapped root mean squared  error for any model
# Capitalizes on match call and elipse to pass arguments to the focal model
# iter controls the number of iterations
# loo_boot (leave one out boot) requests MSE to be calculated for a given iteration observations NOT included in the training sample

boot_cv<-function(data, iter = 10, loo_boot = TRUE, fun = "lm", ...){
  my_call<-match.call(expand.dots = TRUE)
  my_call[1]<-call(fun)
  my_call$iter<-my_call$fun<-my_call$loo_boot<-NULL
  if(fun == "lmer"){
    re_mod<-TRUE
  }else{
    re_mod<-NULL
  }
  
  
  n<-nrow(data)
  error<-c()
  
  if(loo_boot == TRUE){
    for(i in 1:iter){
      cat("Iteration", i, "\r", sep = " ")
      temp<-sample_n(data, size = n, replace = TRUE)
      suppressMessages(eval_set<-anti_join(data,temp))
      my_call$data<-temp
      mod<-eval(my_call)
      error[i]<-sum((eval_set[,as.character(my_call$formula)[2]]-predict(mod, eval_set, allow.new.levels = re_mod))^2)/nrow(eval_set)
    }
  }else{
    for(i in 1:iter){
      cat("Iteration", i, "\r", sep = " ")
      temp<-sample_n(data, size = n, replace = TRUE)
      my_call$data<-temp
      mod<-eval(my_call)
      error[i]<-sum((data[,as.character(my_call$formula)[2]]-predict(mod, data, allow.new.levels = re_mod))^2)/n
    }
  }
  

  cat("\n")
  list(rmse = sqrt(sum(error)/(iter-1)))
}

# Example code
boot_cv(test, fun = "lmer", formula = y~x+(1+x|id), iter = 50, loo_boot = TRUE)
