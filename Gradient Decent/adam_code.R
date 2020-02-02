library(tidyverse)

dec<-read_rds("logit_data.rds")

# Defining Helper Functions -----------------------------------------------

# Linear combination of predictors
v<-function(X, beta){
  v<-X%*%beta
  v
}

# Estimated Probability through sigmoid transformation
p_hat<-function(v){
  
  p<-1/(1+exp(-v))
  
  p
}

# Derivative wrt beta
deriv<-function(X, y, p){
  if(length(X)>1){
   - t(X)%*%(y-p)
  }else{
   - X*(y-p)
  }
}

# neg log likelihood
ll<-function(y, p){
  -sum(y*log(p)+(1-y)*log(1-p))
}


y<-rbinom(100, 1, .5)

ggplot(data = NULL, aes(x = c(.2,.9)))+
  stat_function(fun = Vectorize(function(x){ll(y, rep(x, length(y)))}), )


adam<-function(a = .001, b1 = .9, b2 = .999, eps = 10e-8, minibatch = 128, iter = 100, warmup_pass= 500, X, y){
  
  # Initializing Parameters
  i<-1
  grad<-eps+1
  theta<-numeric(ncol(X))
  v_hat<-numeric(length = minibatch)
  p<-rep(.5, times = minibatch)

  m1<-numeric(ncol(X))
  m2<-numeric(ncol(X))
  
  ll_new<-ll(y, p)
  
  ll_old<-ll(y, .5)
  
  ll_hist<-c()
  ll_hist[i]<-ll_new
  ll_prop<-0
  j<-1
  # Outer loop if more iterations over entire data are wanted
  while((ll_prop<.99999|j<warmup_pass) & j<iter ){
    
    remain<-1:length(y)
    
    while(length(remain)>0){
      
      # Subsetting into minibatches
      use<-sample(remain, size = min(c(length(remain),minibatch)))
      remain<-remain[!remain %in% use]
      
      use_X<- X[use, ]
      use_y<- y[use, ]
      
      v_hat<-v(use_X, theta)
      p<-p_hat(v_hat)
      
      # Calculating Gradient with respect to batch
      batch_grad<-deriv(X = use_X, y = use_y, p = p)
      
      # Calculating moments 
      m1<-b1*m1+(1-b1)*batch_grad
      m2<-b2*m2+(1-b2)*(batch_grad*batch_grad)
      
      # Correcting for bias towards 0 from early iterations
      m1_bias_cor<-m1/(1-b1^i)
      m2_bias_cor<-m2/(1-b2^i)

      # Updating theta
      theta<-theta-a*m1_bias_cor/(sqrt(m2)+eps)
      
      # alpha_t<-a*sqrt(1-b2^i)/(1-b1^i)
      # theta<-theta-alpha_t*m1/(sqrt(m2)+eps)
      
      v_whole<-v(X, theta)
      p_whole<-p_hat(v_whole)
      ll_whole<-ll(y, p_whole)
      ll_prop<-ll_old/ll_whole
      ll_old <- ll_whole
      i<-i+1
      ll_hist[i]<-ll_whole
    }
    j<-j+1
  }
 list(theta = theta,
      ll_hist = ll_hist)
}

# Prep Matrices
X<-dec[3:7]
y<-dec["decision"]

X<-cbind(1, X)
X<-as.matrix(X)
y<-as.matrix(y)

test<-adam(minibatch = 10, X = X, y = y, iter = 10000, a  = .001)

test$theta
plot(test$ll_hist, type ="l")

glm(data = dec, decision ~ salary+job_alt+difficulty+flexibility+market_sal, family = "binomial")
