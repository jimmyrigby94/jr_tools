adaboost_objective<-function(y, x, w, data){
  hat<-x>.5
  y<-as.logical(pull(data, y))
  sum(w*(y!=hat))/sum(w)
}

