library(lme4)

# Function returns k-fold cross validation estimate of RMSE
# Capitalizes on match call and elipse to pass arguments to the focal model
# k controls the number of folds


k_fold<-function(data, k = 10, fun = "lm", ...){
  fold_size = nrow(data)/k
  
  my_call<-match.call(expand.dots = TRUE)
  my_call$k<-my_call$fun<-NULL
  
  my_call[1]<-call(fun)
  
# Partitioning Data into K-Folds ------------------------------------------

  #Initializing Data to be overwritten
  d<-data
  fold_dat<-list()
  for(i in 1:(k-1)){
    temp<-sample_n(d,fold_size)
    fold_dat[[i]]<-temp
    d<-suppressMessages(anti_join(d,temp))
  }
  fold_dat[[k]]<-d

# Applying model to k-1 Folds ---------------------------------------------

  models<-list()
  SE<-c()
  model_call<-my_call
  for(j in 1:k){
    cat("Running fold", j, sep = " ", append = TRUE, "\r")
    temp<-fold_dat
    temp[[j]]<-NULL
    temp<-do.call(rbind,temp)
    model_call$data<-temp
    
    models[[j]]<-eval(model_call)

    SE[j]<-sum((fold_dat[[j]][,as.character(model_call$formula)[2]]-predict(models[[j]], fold_dat[[j]]))^2)
  }
  
  cat("\n")
  list(fold_size=fold_size,
       call = my_call,
       RMSE =sqrt(sum(SE)/nrow(data)))
}


k_fold(test, fun = "lm", formula = y~x, k = 10)


caret::train(y ~ x,
      data = test, 
      method = "lm",
      trControl=caret::trainControl(
        method = "cv",
        number=10,
        savePredictions = TRUE,
        verboseIter = TRUE))
      