gen_lc<-function(n, p1, p2, p3, p4, p5, p6, p7, p8){
  data.frame(x1 = rbinom(n, 1, p1), 
             x2 = rbinom(n, 1, p2),
             x3 = rbinom(n, 1, p3), 
             x4 = rbinom(n, 1, p4), 
             x5 = rbinom(n, 1, p5), 
             x6 = rbinom(n, 1, p6), 
             x7 = rbinom(n, 1, p7), 
             x8 = rbinom(n, 1, p8))

}


params<-data.frame(n = c(1000, 1000, 1000), 
           p1 = c(.5, .9, .1),
           p2 = c(.9, .1, .5), 
           p3 = c(.1, .5, .9),
           p4 = c(.1, .5, .9),
           p5 = c(.5, .9, .1),
           p6 = c(.9, .1, .5), 
           p7 = c(.1, .5, .9),
           p8 = c(.1, .5, .9))

mixture_list<-purrr::pmap(params, gen_lc)

mixture_df<-purrr::map_dfr(1:3,~dplyr::mutate(mixture_list[[.]], class = .))


my_mix_init<-function(x, k = 2){
  
  #Initializing important values
  n<-nrow(x)
  p<-ncol(x)
  # Binning observations
  ## Arrange observations by row mean
  x<-x[order(rowMeans(x)),]
  
  ## Split Data in half
  x.bin<-split(x, f = c(rep(1:k, each = n%/%k), rep(k, n%%k)))
  
  ## Getting Initial means
  mu<-lapply(1:k, function(i) lapply(x.bin, function(x) apply(x, 2, mean))[[i]])
  
  
  ## Gettingin Initial mixing proportions
  lambda<-runif(k)
  lambda<-lambda/sum(lambda)
  
  list(lambda = lambda, mu = mu, k = k)
  
}



my_mix_init(mixture_df%>%select(-class), k = 4)


my_mix<-function(x, k = 2, epsilon = 1e-08, maxit = 10000, verb = TRUE){
  n <- nrow(x) #observations
  p <- ncol(x) #variables
  
  
  # Running initialization helper function
  sv<-my_mix_init(x = x, k = k)
  
  # Extracting initialized values
  lambda<-sv$lambda
  mu<- sv$mu

  #Converting so that x works with dmvnorm
  x<-as.matrix(x)
  
  # Initalizing while() values
  diff <- 1
  iter <- 0 
  
  # Getting responsibilities
  # Individual Liklihoods
  ind_ll<- map_dfc(1:k, function(j) {
    map_dfr(1:n, function(i){
      tmp<-data.frame(id = 1)
      tmp[paste0("class_",j)] <- lambda[j] * prod(dbinom(x[i,], size = 1, prob = mu[[j]]))
      tmp[paste0("class_",j)]
      
    }
    )
  })
  
  
  # Getting liklihoods
  ll<-sum(log(rowSums(ind_ll)))
  
  # #Getting posteriors
  post<- ind_ll/rowSums(ind_ll)
  restarts<-0

  while(abs(diff)>epsilon && iter < maxit){
    cat("iteration=", iter, "diff=", diff, "log-likelihood",
        ll, "\n")
    
  # Updating Means
    mu<-lapply(1:k, function(i) colSums(x*post[,i])/sum(post[,i, drop = FALSE]))
 
    # Getting responsibilities
    # Individual Liklihoods
    ind_ll<- map_dfc(1:k, function(j) {
      map_dfr(1:n, function(i){
        tmp<-data.frame(id = 1)
        tmp[paste0("class_",j)] <- lambda[j] * prod(dbinom(x[i,], size = 1, prob = mu[[j]]))
        tmp[paste0("class_",j)]
        
      }
      )
    })
    
    
    # Getting liklihoods
    new_ll<-sum(log(rowSums(ind_ll)))
    diff<-new_ll-ll
    ll<-new_ll
    iter<-iter+1
    
    # Getting posteriors
    post<- ind_ll/rowSums(ind_ll)

 
    sing <- sum(is.nan(as.matrix(post)))

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
      diff <- 1
      iter <- 0

      x<-as.matrix(x)

      # Getting responsibilities
      # Individual Liklihoods
      ind_ll<- map_dfc(1:k, function(j) {
        map_dfr(1:n, function(i){
          tmp<-data.frame(id = 1)
          tmp[paste0("class_",j)] <- lambda[j] * prod(dbinom(x[i,], size = 1, prob = mu[[j]]))
          tmp[paste0("class_",j)]
          
        }
        )
      })
      
      
      # Getting liklihoods
      ll<-sum(log(rowSums(ind_ll)))
      
      # #Getting posteriors
      post<- ind_ll/rowSums(ind_ll)

    }


  }

  list(run_stats = data.frame(likelihood = ll,
                              iterations = iter),
       mean = mu,
       lambda = lambda,
       post = post)
}


model<-my_mix(x = mixture_df%>%select(-class), k = 3)

post<-model$post%>%
  bind_cols(mixture_df)%>%
  mutate(max_c = case_when(
    class_1 > class_2 & class_1 >class_3 ~ 1,
    class_2 > class_1 & class_2 >class_3 ~ 2,
    TRUE ~ 3
         ))


table(post$class, post$max_c)
