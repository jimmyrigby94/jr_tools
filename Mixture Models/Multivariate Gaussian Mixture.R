dat<-as.data.frame(rbind(mixtools::rmvnorm(1000, mu = c(20,15, 5), sigma = matrix(c(1, .2, .7,
                                                                                   .2, 1, .5,
                                                                                   .1, .5, 1), 3, 3)),
      mixtools::rmvnorm(2000, mu = c(80,2, 7), sigma = matrix(c(17, .0, 0,
                                                                0, 21, 0,
                                                                0, 0, 33), 3, 3)),
      mixtools::rmvnorm(1000, mu = c(1,13, 22), sigma = matrix(c(1, .8, .2,
                                                               .8, 1, .7,
                                                               .2, .7, 1), 3, 3))))



my_mix_init<-function(x, k = 2){
 
  #Initializing important values
  n<-nrow(x)
  p<-ncol(x)
  # Binning observations
  ## Arrange observations by row mean
  x<-x[order(rowMeans(x)),]

  ## Split Data in half
  x.bin<-split(x, f = c(rep(1:k, each = n%/%k), rep(k, n%%k)))

  sigma<-lapply(1:k, function(i) diag(apply(x.bin[[i]], 2, function(x) rexp(1, rate = 1/var(x)))))
  
  
  ## Getting Initial means
  mu<-lapply(1:k, function(i) mixtools::rmvnorm(1, 
                                                mu = lapply(x.bin, function(x) apply(x, 2, mean))[[i]], 
                                                sigma = sigma[[i]]))
  
  
  ## Gettingin Initial mixing proportions
  lambda<-runif(k)
  lambda<-lambda/sum(lambda)
  
  list(lambda = lambda, mu = mu, sigma = sigma, k = k)
  
}

my_mix_init(dat, k = 3)


my_mix<-function(x, k = 2, epsilon = 1e-08, maxit = 10000, verb = TRUE){
  n <- nrow(x) #observations
  p <- ncol(x) #variables
  

  # Running initialization helper function
  sv<-my_mix_init(x = x, k = k)
  
  # Extracting initialized values
  lambda<-sv$lambda
  mu<- sv$mu
  sigma<-sv$sigma
  
  #Converting so that x works with dmvnorm
  x<-as.matrix(x)
  
  # Initalizing while() values
  diff <- 1
  iter <- 0 
  
  # Getting responsibilities
  # Individual Liklihoods
  ind_ll<- lapply(1:k, function(i) lambda[i] * mixtools::dmvnorm(x,
                                                                 mu[[i]], 
                                                                 sigma[[i]]))
  
  # Combining into df
  ind_ll<-do.call(cbind, ind_ll)
  
  # Getting liklihoods
  ll<-sum(log(rowSums(ind_ll)))
  
  #Getting posteriors
  post<- ind_ll/rowSums(ind_ll)
  restarts<-0
  
 while(abs(diff)>epsilon && iter < maxit){
   cat("iteration=", iter, "diff=", diff, "log-likelihood", 
       ll, "\n")
    # Updating Means
    mean<-lapply(1:k, function(i) colSums(x*post[,i])/sum(post[,i, drop = FALSE]))
    
    sigma<-lapply(1:k, function(j) matrix(rowSums(sapply(1:n, function(i) post[i,j]*(crossprod(x[i, , drop =FALSE]-mean[[j]])))),p,p)/sum(post[,j, drop =FALSE]))

    ####Why doesn't this work?####
    # sigma<-lapply(1:k, function(j) (t(post[,j]^.5*(x-mean[[j]]))%*%(post[,j]^.5*(x-mean[[j]])))/sum(post[,j]))
    
    # Getting new responsibilities
    ind_ll<-lapply(1:k, function(i) lambda[i] * mixtools::dmvnorm(x,
                                                                  mu[[i]], 
                                                                  sigma[[i]]))
    
    # Combining into df
    ind_ll<-do.call(cbind, ind_ll)
    new_ll<-sum(log(rowSums(ind_ll)))
    
    diff<-new_ll-ll
    
    ll<-new_ll
    iter<-iter+1
    
    # Updating posteriors
    post<- ind_ll/rowSums(ind_ll)
    
    sing <- sum(is.nan(post))
    
    # Updating lambda
    lambda<-apply(post, 2, mean)
    
    if (sum(lambda < 1e-08) > 0 || is.na(sum(lambda))) {
      sing <- 1
    }
    
      if (sing > 0 || is.na(new_ll) || abs(new_ll) == 
          Inf) {
        cat("Need new starting values due to singularity...", 
            "\n")
        # Restarts the model due to singularity
        restarts <- restarts + 1
        if (restarts > 15) 
          stop("Too many tries!")
        x<-as.data.frame(x)
        sv<-my_mix_init(x = x, k = k)
        
        # Extracting initialized values
        lambda<-sv$lambda
        mu<- sv$mu
        sigma<-sv$sigma
        diff <- 1
        iter <- 0
        
        x<-as.matrix(x)
        
        # Getting responsibilities
        # Individual Liklihoods
        ind_ll<- lapply(1:k, function(i) lambda[i] * mixtools::dmvnorm(x,
                                                                       mu[[i]], 
                                                                       sigma[[i]]))
        
        # Combining into df
        ind_ll<-do.call(cbind, ind_ll)
        
        # Getting liklihoods
        ll<-sum(log(rowSums(ind_ll)))
        
        #Getting posteriors
        post<- ind_ll/rowSums(ind_ll)
        
      }
    
    
    }
    
    list(run_stats = data.frame(likelihood = ll,
                                iterations = iter),
         mean = mean, 
         sigma = sigma,
         lambda = lambda)
}

my_mix(x = dat, k = 3, maxit = 100, epsilon = 1e-08)

