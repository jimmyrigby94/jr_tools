w_update<-function(cost, weights, misclass, alpha){
  weights*exp(alpha*misclass)
}
