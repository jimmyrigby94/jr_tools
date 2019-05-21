# Gradient Decent Implimentation for Multivariate Regression
library(tidyverse)


# Generating Test Data ----------------------------------------------------
test<-data_frame(x = rnorm(1000),
                 )%>%
  mutate(y1 = 20+x*15+rnorm(1000,0,.8),
         y2 = 15+20*x+rnorm(1000, 0, .2))

# Gradient Decent Function -----------------------------------------------
### Arguments ###
## formula: a formula expression of the model of interest
## data: a dataframe containg predictor and outcome
## y x: optional arguments defining outcomes and predictors if a formula is not supplied
## a: learning parameter
## center: should the predictors be centered?
## epsilon: the convergence criterea
## max_iter: maximum iterations
grad_reg<-function(formula=NULL, data, y=NULL, x=NULL, a = 1, center = TRUE, epsilon = 1e-20, max_iter = 1000){

  
  # Designing Model Matricies
  if(!is.null(y) & !is.null(x)){
    x<-as.matrix(cbind(1, x))
    colnames(x)[1]<-"(Intercept)"
    y<-as.matrix(y)
  } else if(!is.null(formula) & !is.null(data)){
    x<-model.matrix(formula, data)
    y<-as.matrix(model.frame(formula, data)[,!all.vars(formula)   %in% all.vars(formula[[3]])])
  }else{
    stop("Please provide either a formula and data or x and y.")
  }
  
  if(center){
    x<-x-c(0,apply(x[,2:ncol(x), drop = FALSE],2,mean))
  }
  
  # initializing values
  # Regression Weights
  theta <- matrix(runif(ncol(x)*ncol(y), min = min(x), max = max(x)),nrow = ncol(x), ncol = ncol(y))
  # Number of observations
  n<-nrow(x)
  
  # Number of Iterations
  iter = 1
  
  # Initial Cost function value
  j_theta<- 1/(2*n)*sum((y-x%*%theta)^2)
  
  # List storing iterative parameters
  param<-list()

  #initializing difference
  dif<-j_theta
  
  # vector storing iterative cost
  cost<-data.frame(iteration = iter,
                  cost = j_theta,
                  dif = dif)
  
   while(dif>epsilon && iter<max_iter){
     # Partial derivatives of theta_j
     dir<-t(t((x%*%theta-y))%*%x)
     
     # Updated theta
     theta<-theta-a*(1/n)*dir
     
     # New Cost
     j_theta.new<-1/(2*n)*sum((y-x%*%theta)^2)
     
     # Difference in j_theta between iter-1 and iter
     dif<-j_theta-j_theta.new
     

     # Storing iterations 
     param[[iter]]<-theta
     
     # Overwriting j_theta
     j_theta<-j_theta.new
     
     # Increasing Iter
     iter<-iter+1
     
     cost[iter,1]<-iter
     cost[iter,2]<-j_theta
     cost[iter,3]<-dif
     
   }
   
   return(list(iteration_log = cost,
               paramters = theta))

}


# Calling functions and comparing results to lm() -----------------------------


res<-grad_reg(y = test[,c('y1',"y2")], x = test[,"x"], a = .1)

res

lm(cbind(y1,y2)~x, data = test)


# Plotting Cost Function --------------------------------------------------

ggplot(res$iter, aes(x = iteration, y = cost))+
  geom_line()
