library(decisionr)
library(evd)
library(tidyverse)
library(mlogit)

test<-decisionr::sim_people(c("cost" = -.05, "morning" = 2), n_people = 1000, n_blocks = 1, p_blocks = 1)%>%
  sim_dec(design = data.frame(BLOCK = 1,
                              QES = 1,
                              alt = 1:4,
                              cost = c(20, 13, 13, 20),
                              morning = c(0, 1, 0, 1)), people = ., location = 0, scale = 1, shape = 0)%>%
  ungroup()
  

# Multinomial Regression from Scratch -------------------------------------
my_mnl<-function(formula=NULL, int = TRUE, id = NULL, alt = NULL, ref = NULL, data =NULL, grad_tol = 1e-06){
  

# Error Catching: Formula and Data arguments required ---------------------
# Throws an error if neither x and y are provided nor formula and data
  
  if(is.null(formula) | is.null(data)){
    stop("Please provide either a formula and data or x and y.")
  }
  
  if(is.null(ref)){
    stop("Please define the reference level of the outcome.")
  }

  if(is.null(alt)){
    stop("Please define the outcome lable. ")
  }
# Handles Formula --------------------------------
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
    
    list(alt_gen = alt_gen[nchar(alt_gen)>0 & alt_gen != 0],
         ind_alt = ind_alt[nchar(ind_alt)>0 & ind_alt != 0],
         alt_alt = alt_alt[nchar(alt_alt)>0 & alt_alt != 0])
  }
  
  
  # Applying the Terms Function to Separate out Terms
  terms<-var_parse(formula = formula)
  
  if(int == TRUE){
    terms$ind_alt<-c("Intercept", terms$ind_alt)
    
    data<-data%>%
      mutate(Intercept = 1)
  }
  
  # Converts the data into list format with each element being associated with 1 level
    ids<-split(data[,c(id, alt), drop = FALSE], f = data[alt])
    
    x<-split(data[c(terms$alt_gen, terms$ind_alt, terms$alt_alt)], f =  data[alt])
    
    y<-as.matrix(model.frame(formula, data)[,!all.vars(formula) %in% all.vars(formula[[3]])])[x[alt]!=ref]
  
    alt_lvls<-names(x)
  
  # Normalizes the different list elements so that for alternative variables 0 is associated with the values of the variable at the reference level
  # This is because only differences are identified in mnl
    x<-lapply(alt_lvls[alt_lvls!=ref], function(lvls){
      tmp<-x[[lvls]]
      
      tmp[,c(terms$alt_gen, terms$alt_alt)]<-tmp[,c(terms$alt_gen, terms$alt_alt)]-x[[ref]][,c(terms$alt_gen, terms$alt_alt)]
      
      as.matrix(tmp)
    })

  # Since all predictors are normalized to the reference, theta is the association between preds and NOT the reference
    y<- y[alt_lvls!=ref]
    ref_ids<- ids[alt_lvls==ref]
    ids<- ids[alt_lvls!=ref]
    
    
  
# Error Catching: ID required to identify alternative sets --------------------------------

  if(is.null(id)){
    stop("Please provide a vector that defines the id for the data")
  }

    # Extracting alternative frequencies and numbers of different coefficients
    n_alt<-length(x)
    
    n_alt_gen<-length(terms$alt_gen)
    n_ind_alt<-length(terms$ind_alt)
    n_alt_alt<-length(terms$alt_alt)  
    
    
  # Defines model: Model function will return p hats
  model<- function(theta){
    
    ### Theta is ordered as followed ###
    # i alt_gen coefficients in order of specification#
    # j by k ind_alt coefficients in order of specification but Intercept comes first#
    # l by k alt_alt coefficients in order of specification#
    
    # Extracts alt_gen Coefficients
    if(n_alt_gen>0){
      alt_gen_coef<-theta[1:n_alt_gen]
    }else{
      alt_gen_coef<-vector(mode = "numeric")
    }
    
    # Parsing Coefficients
    parsed_coefs<- lapply(1:n_alt, 
                          function(i){
                            
                            if(n_ind_alt>0){
                              start_ind_alt<-n_alt_gen+1+(i-1)*n_ind_alt
                              end_ind_alt<- n_alt_gen+n_ind_alt+(i-1)*n_ind_alt
                            }else{
                              start_ind_alt<-0
                              end_ind_alt<-0
                            }

                            if(n_alt_alt>0){
                            start_alt_alt<-n_alt_gen+n_ind_alt*n_alt+1+(i-1)*n_alt_alt
                            end_alt_alt<- n_alt_gen+n_ind_alt*n_alt+n_alt_alt+(i-1)*n_alt_alt
                            }else{
                              start_alt_alt<-0
                              end_alt_alt<-0
                            }

                            
                            ind_alt_coef<- theta[start_ind_alt:end_ind_alt]
                            alt_alt_coef<- theta[start_alt_alt:end_alt_alt]
                            
                            c(alt_gen_coef, ind_alt_coef, alt_alt_coef)
                          })
    
    
    v<-map2(x, parsed_coefs, function(x, coef){
      v<-exp(x%*%coef)
      data.frame(v = v)
    })
    
    v<-map2_dfr(ids, v, ~bind_cols(.x, .y))%>%
      arrange(.data[[id]])
    
    v%>%
      group_by(.data[[id]])%>%
      mutate(denom = 1+sum(v),
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
  theta<- rep(0, times = sum(n_alt_gen+n_ind_alt*n_alt+n_alt_alt*n_alt))
  
  # Relies on optim() to calculate parameters and SEs without closed form
  my_opt<-optim(theta, cost, method = "BFGS", hessian = TRUE)
  
  # Uses analytic method to solve for theta for generic coefs only! Needs tweaks to get to analytic MNL
  # # Solve for p
  # p<-model(theta)
  # 
  # # Initialize ll and dif
  # ll_old<-cost(theta)
  # dif <-ll_old
  # 
  # dx<-do.call(rbind,x)
  # # Initiaize first deriviative and second derivative
  # d1<-t(dx)%*%(y-p)
  # 
  # W<-matrix(0, nrow = nrow(dx), ncol = nrow(dx))
  # diag(W)<-p*(1-p)
  # 
  # d2<--t(dx)%*%W%*%dx
  # 
  # # House cleaning
  # rm(W)
  # 
  # # Update Theta for first
  # theta_up<- theta - solve(d2)%*%d1
  # 
  # # Intialize gradient
  # grad<--(t(d1)%*%solve(d2)%*%d1)
  # 
  # while(grad > grad_tol){
  #   # Update theta
  #   theta<-as.vector(theta_up)
  # 
  #   # Solve for p
  #   p<-model(theta)
  # 
  #   # Calculate new ll
  #   ll_new<-cost(theta)
  # 
  #   # Calculate first d
  #   d1<-t(x)%*%(y-p)
  # 
  #   # Calculate second d
  #   W<-matrix(0, nrow = nrow(x), ncol = nrow(x))
  #   diag(W)<-p*(1-p)
  # 
  #   d2<--t(x)%*%W%*%x
  # 
  #   # House Cleaning
  #   rm(W)
  # 
  #   # Update Theta
  #   theta_up<- theta - solve(d2)%*%d1
  # 
  #   # Update dif and overwrite old ll
  #   dif<-ll_old-ll_new
  #   ll_old<-ll_new
  #   grad<--(t(d1)%*%solve(d2)%*%d1)
  # }

  # Calculating SE from Hessian
  # Note that the information matrix (i.e., inverse of standard errors) is a product of the variance observed in alternative k
  # and the variance observed in option j
  pars<-my_opt$par
  se<-sqrt(diag(round(solve(my_opt$hessian),6)))

 # Saving Parameters as data frame
 pars<- data.frame(Parameters = pars,
             SE = se)%>%
    mutate(z = abs(Parameters)/SE,
           p = round(2*(1-pnorm(z)), 2))

 # Assigning Row Names
  # rownames(pars)<-colnames(x)

  # Createing Predicted Output
  pred_dat<-bind_rows(ids)%>%arrange(.data[[id]])
  pred_dat["p"]<-model(my_opt$par)
  
  pred_dat<-pred_dat%>%
    bind_rows(ref_ids)%>%
    group_by(.data[[id]])%>%
    mutate(p=if_else(is.na(p), 1-sum(p, na.rm = TRUE), p))%>%
    arrange(.data[[id]], .data[[alt]])
  
  # grad<-t(my_opt$)%*%solve(d2)%*%d1
  # if(grad<1e-05){
  #   message(paste0("Gradient close to zero suggesting convergence. Gradient = ", formatC(grad, format = "e", digits = 2)))
  # }else{
  #   message(paste0("Gradient has deviated from 0. Check for convergence. Gradient = ", formatC(grad, format = "e", digits = 2)))
  # }

  # Outputting as List
  list(parameters = pars,
       pred_dat = pred_dat,
       # first_der = d1,
       second_der = my_opt$hessian,
       likelihood = my_opt$value)
}


out<-my_mnl(decision ~  morning + cost, data = test, id = "id", alt = "alt", ref = "1", int = FALSE)

out

mlogit_data<-mlogit.data(test, choice = "decision",  alt.var = "alt")
m1<-mlogit(decision ~ morning+cost|0, data = mlogit_data)

summary(m1)

data("Fishing", package = "mlogit")
Fish <- mlogit.data(Fishing, varying = c(2:9), shape = "wide", choice = "mode")
Fish<-as.data.frame(Fish)

my_fish_model<-my_mnl(formula = mode ~ 0|income, data = Fish, id = "chid", alt = "alt", ref = "beach")
my_fish_model

Fish <- mlogit.data(Fishing, varying = c(2:9), shape = "wide", choice = "mode")
summary(mlogit(mode ~ 0 | income, data = Fish))
