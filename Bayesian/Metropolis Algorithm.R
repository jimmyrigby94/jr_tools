# Metropolis Hastings


# Generating Data ---------------------------------------------------------
set.seed(1234)
library(tidyverse)

# B0 = 15, B1 = .3, B2 = .6, sd = .74
my_dat<-data.frame(x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100))%>%
  mutate(y = 15 +.3 * x1+.6 * x2 + .15*x3+ rnorm(100, mean = 0, sd = sqrt(1-(.6^2+.3^2+.15^2))))

plot(function(x)MCMCpack::dinvgamma(x, .01, .01), from = 1, to = 100)

my_mh<-function(formula, data, iterations = 10000, burnin = 1000, thin = 5, chains){

# Data Prep ---------------------------------------------------------------
  f<-formula
  
  x<-model.matrix(f, data)
  
  y<-data%>%pull(as.character(f[2]))
  

# Parameter Extraction ----------------------------------------------------
  
  p<-ncol(x)
  iter <- (iterations*thin)+burnin+1
  theta<-vector(mode = "numeric", p+1)
  
  # Sets theta to 1 as arbitrary starting value for ML starting point estimation
  theta[p+1]<-1
  
  # Private function for regression likelihood 
  like_reg<-function(theta, x, y){
    p<-ncol(x)
    
    beta<-theta[1:p]
    
    sigma<-theta[p+1]
    
    y_hat<-x%*%beta
    
    ll<- sum(dnorm(y, mean = y_hat, sd = sigma, log = TRUE))
    
    -ll
  }
  

# Initializing Parameters -------------------------------------------------
  
  # Uses ML estimates as starting values for iter 1
  ml_est<-optim(par = theta, fn = like_reg, x = x, y = y)
  
  trace<-matrix(0, nrow = p+1, ncol = iter)
  
  trace[,1]<-ml_est$par
  
  acc_count<-vector("numeric", length = nrow(trace))
  
  prior_old<-sum(dnorm(trace[1:p, 1], mean = 0, sd = 1e6, log = TRUE))+log(MCMCpack::dinvgamma(trace[p+1, 1], shape = .01, scale = .01))
  ll_old<--like_reg(trace[,1], x, y)
  post_old<-prior_old+ll_old
  

# Defining Private function for each chain --------------------------------
  chain<-function(chain){
    message("Chain ", chain )
  for(i in 2:iter){
    # Setting theta to previous iteration
    theta<-trace[, i-1]
    for(j in 1:(p+1)){
      # For regression coefficients
      if(j <= p){
        # Add rnorm
        theta[j]<-theta[j]+rnorm(1, mean = 0, sd = 1)
        # For sigma
      }else{
        # Add rnorm (really truncated normal) so positive
        theta[j]<-theta[j]+rnorm(1, mean = 0, sd = 1)
        while(theta[j]<0){
          theta[j]<-trace[j, i-1]+rnorm(1, mean = 0, sd = 1)
        }
      }
      # Update priors, likelihood and posterior
      prior<-sum(dnorm(theta[1:p], mean = 0, sd = .001, log = TRUE))+log(MCMCpack::dinvgamma(theta[p+1], shape = .01, scale = .01))
      ll<--like_reg(theta, x, y)
      post<- ll+prior
      acc<-post-post_old
      acc_p<-exp(acc)
      d<-runif(1)
      # If accepted
      if(d <= acc_p){
        # Update posterior old
        post_old<-post
        # Update acceptance rate
        if((i-2)%%thin == 0){
          acc_count[j]<-acc_count[j]+1
        }
      }else{
        # otherwise revert back to previous iteration
        theta[j]<-trace[j, i-1]
      }
    }
    # Save theta in the trace matrix
    trace[,i]<-theta
  }
    
  message("Acceptance Rate = ", paste(acc_count/iterations, collapse = ", "))
  
  # Drops the burnin time
  trace<-trace[,-c(1:burnin)]
  
  # Thins the trace plot
  trace[, seq(2, ncol(trace), by = thin)]
  }
  
  # Applies chain function across requested chains
  chains<-lapply(1:chains, chain)
  
  # Creates meaps
  meap<-map_dfr(chains, function(i){
    meap_tmp<-apply(i, 1, FUN = mean)
    names(meap_tmp)<-c(colnames(x), "sigma")
    meap_tmp<-as.data.frame(t(meap_tmp))
    meap_tmp
  })%>%
    mutate(chain = 1:nrow(.))%>%
    select(chain, everything())
  
  # Creates sds
  sd<-map_dfr(chains, function(i){
    sd_tmp<-apply(i, 1, FUN = sd)
    names(sd_tmp)<-c(colnames(x), "sigma")
    sd_tmp<-as.data.frame(t(sd_tmp))
    sd_tmp
  })%>%
    mutate(chain = 1:nrow(.))%>%
    select(chain, everything())
  
  # Lists them all
  list(chains = chains, 
       meap = meap,
       sd = sd)
}

# Acceptance probability only works properly when thin is 5
# Iteration level is set to MH adaptive default, but added thin and chains
trace<-my_mh(y~x1+x2+x3, my_dat, 10000, burnin = 1000, thin = 7, chains = 4)

for(i in 1:4){
  walk(1:4, ~plot(trace$chains[[.x]][i,], type = "l"))
}

round(cov(t(trace$chains[[1]])), 10)

for(i in 1:4){
  walk(1:4, ~hist(trace$chains[[.x]][i,]))
}

vcov(lm(y~x1+x2+x3, my_dat))

trace$meap
trace$sd




