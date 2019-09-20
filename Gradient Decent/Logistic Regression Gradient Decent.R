
# Loading dependencies and data -------------------------------------------
library(ISLR)

test<-Smarket

# Coding outcome as 1
test<-test%>%
  mutate(Up = if_else(Direction=="Up", 1, 0 ))

# Writing GLM function
my_glm<-function(formula=NULL, y=NULL, x=NULL, data =NULL){
  # Designing Model Matricies x and y using x and y arguments
  if(!is.null(y) & !is.null(x)){
    x<-as.matrix(cbind(1, x))
    colnames(x)[1]<-"(Intercept)"
    y<-as.matrix(y)
    
  #Designing model Matrices using formula
  } else if(!is.null(formula) & !is.null(data)){
    x<-model.matrix(formula, data)
    y<-as.matrix(model.frame(formula, data)[,!all.vars(formula) %in% all.vars(formula[[3]])])
    
  # Throws an error if neither x and y are provided nor formula and data
  }else{
    stop("Please provide either a formula and data or x and y.")
  }
  
  # Initialized theta
  theta<-rep(0, ncol(x))
  
  # Defines model
  model<- function(theta) 1/(1+exp(-(x%*%theta)))
  
  # Defines loss function
  cost<- function(theta){
    g<-model(theta)
    -(1/nrow(x)) * sum((y*log(g))+((1-y)*log(1-g)))
  }

# Optimizes theta
optim_theta<-optim(theta, cost, method = "BFGS", hessian = TRUE)

par <- optim_theta$par
vcov <- optim_theta$hessian
se <- sqrt(diag(vcov))


names(par)<-colnames(x)

return(list(parameters = par,
            se = se,
            z = par/se,
            hess = vcov))
}


# R bloggers implementation -----------------------------------------------


X<-test%>%
  mutate(one = 1)%>%
  select(one, Lag1, Lag2, Lag3)%>%
  as.matrix(.)

Y<-test%>%
  select(Up)%>%
  as.matrix(.)

# Defining model
sigmoid <- function(z)
{
  g <- 1/(1+exp(-z))
  return(g)
}

# Defining loss function
cost <- function(theta)
{
  m <- nrow(X)
  g <- sigmoid(X%*%theta)
  J <- (1/m)*sum((-Y*log(g)) - ((1-Y)*log(1-g)))
  return(J)
}

# initializing theta
initial_theta <- rep(0,ncol(X))

# Optimizing theta
optim(initial_theta, fn = cost, method = "BFGS", hessian = TRUE)

# Comprison of Approaches -------------------------------------------------
my_glm(Up ~ Lag1+Lag2+Lag3, data = test)

summary(glm(Up ~ Lag1+Lag2+Lag3 ,data = test, family = "binomial"))
