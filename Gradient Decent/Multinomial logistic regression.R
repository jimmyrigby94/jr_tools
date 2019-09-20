library(decisionr)
library(evd)
library(tidyverse)
library(mlogit)

test<-decisionr::sim_people(c("cost" = -.05, "morning" = 2), n_people = 1000, n_blocks = 1, p_blocks = 1)%>%
  sim_dec(design = data.frame(BLOCK = 1,
                              QES = 1,
                              alt = 1:4,
                              cost = c(20, 13, 13, 20),
                              morning = c(0, 1, 0, 1)), people = ., scale = 1, location = 0, shape = 0)
  


# Multinomial Regression from Scratch -------------------------------------
my_mnl<-function(formula=NULL, y=NULL, x=NULL, id = NULL, data =NULL, se = TRUE){
  

# Error Catching: Formula and Data arguments required ---------------------

  if(!is.null(formula) & !is.null(data)){
    x<-model.matrix(formula, data)
    y<-as.matrix(model.frame(formula, data)[,!all.vars(formula) %in% all.vars(formula[[3]])])
    
    # Throws an error if neither x and y are provided nor formula and data
  }else{
    stop("Please provide either a formula and data or x and y.")
  }


# Error Catching: ID required to identify alternative sets --------------------------------
  
  if(is.null(id)){
    stop("Please provide a vector that defines the id for the data")
  }
  
  # Defines model: Model function will return p hats
  model<- function(theta){
    v<-exp(x%*%theta)
    
    data.frame(v=v, id=id)%>%
      group_by(id)%>%
      mutate(denom = sum(v),
             p = v/denom)%>%
      pull(p)
  } 
  
  i<-1
  
  # Defines loss function
  cost<- function(theta){
    g<-model(theta)
    ll<--sum(y*log(g))
    message(paste("Iteration ", i, " Log Likelihood: ", -ll, "\r"))
  i<<-i+1
  ll
  }

  # Initializing Theta
  theta<- rep(0, times = ncol(x))


# Closed form gradient not working
# grad<-function(par){
#   sapply(1:length(par),function(i) sum((y-y*model(par))*x[i]))
# }

# fin_grad<-grad(par = par)
  

  # Applies BFGS optimization to cost functions applied above ---------------
  
  my_res<-optim(par = theta, fn = cost,  method = "BFGS", hessian = TRUE)
  
  par<-my_res$par
  
  names(par)<-colnames(x)
  
  pred_dat<-data
  
  pred_dat$v<- exp(as.vector(as.matrix(pred_dat[names(par)])%*%par))
  
  pred_dat<-pred_dat%>%
    group_by(id)%>%
    mutate(denom = sum(v), 
           p = v/denom)
  
  list(parameters = par,
       pred_dat = pred_dat,
       opt = my_res)


}







out<-my_mnl(decision ~ -1+ morning + cost, data = test, id = test$id)
out

mlogit_data<-mlogit.data(test, choice = "decision",  alt.var = "alt")


m1<-mlogit(decision ~ morning+cost|0, data = mlogit_data)

summary(m1)
