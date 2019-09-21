library(decisionr)
library(evd)
library(tidyverse)
library(mlogit)

test<-decisionr::sim_people(c("cost" = -.05, "morning" = 2), n_people = 1000, n_blocks = 1, p_blocks = 1)%>%
  sim_dec(design = data.frame(BLOCK = 1,
                              QES = 1,
                              alt = 1:4,
                              cost = c(20, 13, 13, 20),
                              morning = c(0, 1, 0, 1)), people = ., location = 0, scale = 1, shape = 0)
  


# Multinomial Regression from Scratch -------------------------------------
my_mnl<-function(formula=NULL, id = NULL, data =NULL, tol = 1e-05){
  

# Error Catching: Formula and Data arguments required ---------------------
# Throws an error if neither x and y are provided nor formula and data
  
  if(is.null(formula) | is.null(data)){
    stop("Please provide either a formula and data or x and y.")
  }
  

# Handles formula and Preps Model Matrices --------------------------------
  var_parse<-function(formula){
    
    # Parses forumla by splitting on pipe and +
  xvar<-trimws(str_split(trimws(str_split(as.character(formula[3]), "\\|", simplify = TRUE)), "\\+", simplify = TRUE))
    
    # Identifies alt_gen coefficients (alternative specific variables generic coefs)
    alt_gen<-xvar[1,]
    if(str_count(as.character(formula[3]), "\\|")==0){
      ind_alt<-c(NULL)
      alt_alt<-c(NULL)
      
    # Identifies ind_alt coefficients (individual specific variables alternative specific coefficients)
    } else if(str_count(as.character(formula[3]), "\\|")==1){
      ind_alt<-xvar[2,]
      alt_alt<-c(NULL)
      
    # Identifies alt_alt coefficients (alternative specific variables alternative specific coeficients)
    }else if(str_count(as.character(formula[3]), "\\|")==2){
      ind_alt<-xvar[2,]
      alt_alt<-xvar[3,]
    }
    
    list(alt_gen = alt_gen[nchar(alt_gen)>0],
         ind_alt = ind_alt[nchar(ind_alt)>0],
         alt_alt = alt_alt[nchar(alt_alt)>0])
  }
  
  
  terms<-var_parse(formula = formula)
  
  
    x<-as.matrix(data[terms$alt_gen])
    y<-as.matrix(model.frame(formula, data)[,!all.vars(formula) %in% all.vars(formula[[3]])])
    



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
  
  # initializes i
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

  # Solve for p
  p<-model(theta)
  
  # Initialize ll and dif
  ll_old<-cost(theta)
  dif <-ll_old
  

  
  # Initiaize first deriviative and second derivative
  d1<-t(x)%*%(y-p)
  
  W<-matrix(0, nrow = nrow(x), ncol = nrow(x))
  diag(W)<-p*(1-p)
  d2<--t(x)%*%W%*%x
  
  # House cleaning
  rm(W)
  
  # Update Theta for first 
  theta_up<- theta - solve(d2)%*%d1
  
  while(dif>tol){
    # Update theta
    theta<-as.vector(theta_up)
    
    # Solve for p
    p<-model(theta)
    
    # Calculate new ll
    ll_new<-cost(theta)
    
    # Calculate first d
    d1<-t(x)%*%(y-p)
    
    # Calculate second d
    W<-matrix(0, nrow = nrow(x), ncol = nrow(x))
    diag(W)<-p*(1-p)
    
    d2<--t(x)%*%W%*%x
    
    # House Cleaning
    rm(W)
    
    # Update Theta
    theta_up<- theta - solve(d2)%*%d1
    
    # Update dif and overwrite old ll
    dif<-ll_old-ll_new
    ll_old<-ll_new
  }
  
  # Calculating SE from Hessian
  # Note that the information matrix (i.e., inverse of standard errors) is a product of the variance observed in alternative k
  # and the variance observed in option j
  se<-sqrt(diag(round(solve(-d2),6)))
  
 # Saving Pars
 pars<- data.frame(Parameters = theta_up, 
             SE = se)%>%
    mutate(z = Parameters/SE)
  
 # Assigning Row Names
  rownames(pars)<-colnames(x)
  
  # Createing Predicted Output
  pred_dat<-data
  pred_dat$phat<-p
  
  grad<-t(d1)%*%solve(d2)%*%d1
  if(grad<1e-05){
    message(paste0("Gradient close to zero suggesting convergence. Gradient = ", formatC(grad, format = "e", digits = 2)))
  }else{
    message(paste0("Gradient has deviated from 0. Check for convergence. Gradient = ", formatC(grad, format = "e", digits = 2)))
  }
  message()
  # Outputting as List
  list(parameters = pars,
       pred_dat = pred_dat,
       first_der = d1,
       second_der = d2,
       likelihood = ll_new)
}


out<-my_mnl(decision ~  morning + cost, data = test, id = test$id)
out


mlogit_data<-mlogit.data(test, choice = "decision",  alt.var = "alt")
m1<-mlogit(decision ~ morning+cost|0, data = mlogit_data)

summary(m1)
