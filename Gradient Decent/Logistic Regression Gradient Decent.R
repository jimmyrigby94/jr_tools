library(ISLR)
test<-Smarket

test<-test%>%
  mutate(Up = if_else(Direction=="Up", 1, 0 ))

my_glm<-function(formula=NULL, y=NULL, x=NULL, data =NULL){
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
  
  theta<-rep(0, ncol(x))
  
  model<- function(theta) 1/(1+exp(-(x%*%theta)))
  cost<- function(theta){
    g<-model(theta)
    -(1/nrow(x))* sum((y*log(g))+((1-y)*log(1-g)))
  }

 # optim(par = theta, fn = cost)
  
optim(theta, cost)
}


# R bloggers implementation -----------------------------------------------


X<-as.matrix(test%>%mutate(one = 1)%>%select(one, Lag1, Lag2, Lag3))
Y<-as.matrix(test%>%select(Up))
sigmoid <- function(z)
{
  g <- 1/(1+exp(-z))
  return(g)
}

cost <- function(theta)
{
  m <- nrow(X)
  g <- sigmoid(X%*%theta)
  J <- (1/m)*sum((-Y*log(g)) - ((1-Y)*log(1-g)))
  return(J)
}



initial_theta <- rep(0,ncol(X))

cost(initial_theta)
#
optim(initial_theta, fn = cost)

# Comprison of Approaches -------------------------------------------------
my_glm(Up ~ Lag1+Lag2+Lag3, data = test)

glm(Up ~ Lag1+Lag2+Lag3 ,data = test, family = "binomial")
