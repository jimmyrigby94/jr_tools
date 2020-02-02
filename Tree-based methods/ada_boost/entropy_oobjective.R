entropy<-function(y, x, weights){
 -sum(y*log(x)+(1-y)*log(1-x), na.rm = TRUE)
}

