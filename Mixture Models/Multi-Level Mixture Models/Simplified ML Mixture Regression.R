
# Starting Values ---------------------------------------------------------


my_mix_ml_init <- function (y,
                            x,
                            w = NULL,
                            sigma = NULL,
                            arb.sigma = TRUE,
                            alpha = NULL,
                            lambda = NULL,
                            mu = NULL,
                            R = NULL,
                            arb.R = TRUE,
                            k = 2,
                            mixed = FALSE,
                            addintercept.fixed = FALSE,
                            addintercept.random = TRUE)
{
  N <- length(y)
  n <- sapply(y, length)
  p <- ncol(x[[1]])
  if (addintercept.random) {
    x = lapply(1:N, function(i)
      as.matrix(x[[i]][,-1]))
  }
  else
    x = x
  if (is.null(w) == FALSE && sum(sapply(w, sum)) != 0) {
    q <- ncol(w[[1]])
  }
  if (mixed == TRUE && is.null(alpha) == TRUE) {
    if (addintercept.fixed) {
      w.1 = list()
      w.1 = lapply(1:N, function(i)
        w[[i]][,-1])
      lm.out = lapply(1:N, function(i)
        lm(y[[i]] ~ w.1[[i]]))
      alpha.hyp = apply(matrix(sapply(lm.out, coef), ncol = N),
                        1, mean)
      sd.hyp = lapply(lm.out, anova)
      sd.hyp = mean(as.vector(sqrt(
        sapply(1:N, function(i)
          sd.hyp[[i]]$Mean[length(sd.hyp[[i]]$Mean)])
      )))
      alpha = rnorm(q, mean = alpha.hyp, sd = sd.hyp)
    }
    else {
      w.1 = w
      lm.out = lapply(1:N, function(i)
        lm(y[[i]] ~ w.1[[i]] -
             1))
      alpha.hyp = apply(matrix(sapply(lm.out, coef), ncol = N),
                        1, mean)
      sd.hyp = lapply(lm.out, anova)
      sd.hyp = mean(as.vector(sqrt(
        sapply(1:N, function(i)
          sd.hyp[[i]]$Mean[length(sd.hyp[[i]]$Mean)])
      )))
      alpha = rnorm(q, mean = alpha.hyp, sd = sd.hyp)
    }
  }
  if (mixed == FALSE) {
    alpha = 0
  }
  y.x = lapply(1:N, function(i)
    cbind(y[[i]], x[[i]]))
  a = order(sapply(1:N, function(i)
    mean(y[[i]])))
  y.x = lapply(1:N, function(i)
    y.x[[a[i]]])
  y.x.bin.list = list()
  y.x.bin = list()
  for (j in 1:k) {
    y.x.bin.list[[j]] <- y.x[max(1, floor((j - 1) * N / k)):ceiling(j *
                                                                      N /
                                                                      k)]
    y.x.2 <- NULL
    for (i in 1:length(y.x.bin.list[[j]])) {
      y.x.2 <- rbind(y.x.2, y.x.bin.list[[j]][[i]])
    }
    y.x.bin[[j]] <- y.x.2
  }
  if (addintercept.random) {
    lm.out <- lapply(1:k, function(i)
      lm(y.x.bin[[i]][, 1] ~
           y.x.bin[[i]][, 2:p]))
    lm.out.beta <-
      lapply(1:k, function(j)
        lapply(1:length(y.x.bin.list[[j]]),
               function(i)
                 lm(y.x.bin.list[[j]][[i]][, 1] ~ y.x.bin.list[[j]][[i]][,
                                                                         2:p])))
    beta <- lapply(1:k, function(j)
      matrix(sapply(lm.out.beta[[j]],
                    coef), nrow = p))
  }
  else {
    lm.out <- lapply(1:k, function(i)
      lm(y.x.bin[[i]][, 1] ~
           y.x.bin[[i]][, 2:(p + 1)] - 1))
    lm.out.beta <-
      lapply(1:k, function(j)
        lapply(1:length(y.x.bin.list[[j]]),
               function(i)
                 lm(y.x.bin.list[[j]][[i]][, 1] ~ y.x.bin.list[[j]][[i]][,
                                                                         2:(p + 1)] - 1)))
    beta <- lapply(1:k, function(j)
      matrix(sapply(lm.out.beta[[j]],
                    coef), nrow = p))
  }
  if (is.null(sigma)) {
    sigma.hyp = lapply(lm.out, anova)
    sigma.hyp = as.vector(sqrt(sapply(1:k, function(i)
      sigma.hyp[[i]]$Mean[length(sigma.hyp[[i]]$Mean)])))
    if (arb.sigma) {
      sigma = 1 / rexp(k, rate = sigma.hyp)
    }
    else {
      sigma.hyp = mean(sigma.hyp)
      sigma = 1 / rexp(1, rate = sigma.hyp)
    }
  }
  if (is.null(sigma) == FALSE && arb.sigma == TRUE) {
    k = length(sigma)
  }
  if (is.null(R)) {
    if (arb.R) {
      R.hyp = lapply(1:k, function(i)
        (apply(beta[[i]],
               1, var)) ^ -1)
      R = lapply(1:k, function(i)
        diag(1 / rexp(p, rate = R.hyp[[i]]),
             p))
    }
    else {
      R.hyp = apply(matrix(sapply(1:k, function(i)
        (
          apply(beta[[i]],
                1, var)
        ) ^ -1), ncol = k), 2, mean)
      R = diag(1 / rexp(p, rate = sigma.hyp), p)
    }
  }
  if (is.null(R) == FALSE && arb.R == TRUE) {
    k = length(R)
  }
  if (is.null(mu)) {
    mu.hyp = lapply(1:k, function(i)
      apply(beta[[i]], 1,
            mean))
    mu = matrix(ncol = k, nrow = p)
    if (arb.R == TRUE) {
      for (j in 1:k) {
        mu[, j] = mixtools::rmvnorm(1, mu = as.vector(mu.hyp[[j]]),
                                    sigma = R[[j]])
      }
    }
    else {
      for (j in 1:k) {
        mu[, j] = mixtools::rmvnorm(1, mu = as.vector(mu.hyp[[j]]),
                                    sigma = R)
      }
    }
  }
  else
    k = ncol(mu)
  if (is.null(lambda)) {
    lambda = runif(k)
    lambda = lambda / sum(lambda)
  }
  else
    k = length(lambda)
  list(
    sigma = sigma,
    alpha = alpha,
    lambda = lambda,
    mu = mu,
    R = R,
    k = k
  )
}


# Function ---------------------------------------------------------------


my_mix_ml2 <- function(form = NULL,
                      data = NULL,
                      nest = NULL,
                      k,
                      epsilon = 1e-08,
                      maxit = 1000,
                      margin_id = NULL) {


  # Adding Intercept
  data[, "(Intercept)"] <- 1
  
  # Building Model Matrices (list form)
  y <- purrr::map(split(data[, as.character(form[2])], data[, nest]), as.matrix)
  x <- purrr::map(split(data[, c("(Intercept)", as.character(form[3]))], data[, nest]), as.matrix)
  
  
  # Getting Starting Values
  
  start <-  my_mix_ml_init(
    y = y,
    x = x,
    k = k,
    arb.sigma = TRUE,
    arb.R = TRUE,
    addintercept.random = FALSE
  )
  
  # Extracting Starting Values
  
  ## Residual SD
  sigma <- start$sigma
  ## Mixing Proportions
  lambda <- start$lambda
  
  ## Mean of Random Effects
  mu <- start$mu
  
  ## Var/cov of random effects
  R <- start$R
  
  ## Number of clusters
  k <- start$k
  
  ## Number of level 2 units
  N <- length(x)
  
  ## Number of Random Effects
  q <- ncol(x[[1]])
  
  ## Residual Variance
  s.2 <- sigma ^ 2
  
  #Initalizing Within subject Matrix (Compound Symetry)
  Omega.k <- purrr::map(1:N, function(i) diag(nrow(y[[N]])))
  
  #Initalizing While loop parameters
  ## Difference between liklihoods
  diff <- 1
  ## Number of iterations
  iter <- 0
  ## Likelihood of Iteration
  obsloglik = -Inf
  ## Likelihood vector
  ll <- vector("double", length = maxit)
  ## Restarts due to singularity
  restarts <- 0
  
  # Initializing commonly used variables that are NOT updated
  ## crossproduct of x
  txx<- purrr::map(1:N, function(i) crossprod(x[[i]]))
  ## Nesting unit vector
  n_seq<- 1:N
  ## Mixture vector
  k_seq<- 1:k
  
  while (diff > epsilon && iter < maxit) {
    
    # Initializing Commonly Used parameters that ARE updated
    resid_ij<-purrr::map(n_seq, function(i){
      purrr::map(k_seq, function(j){
        y[[i]] - x[[i]] %*% mu[, j]
      })
    })
    
    margin_cov<-purrr::map(n_seq, function(i){
      purrr::map(k_seq, function(j){
      x[[i]] %*% R[[j]] %*% t(x[[i]]) + s.2[j] * Omega.k[[i]]
      })
    })
    
    
    # Getting V and Beta (C step)
    V <- purrr::map(n_seq, function(i){
      purrr::map(k_seq, function(j) {solve(txx[[i]]/s.2[j] + solve(R[[j]]))})
      }
      )

      # Note that this solution differs from the solution provided in the Young and Hunter
      # This actually removes solve(R[[j]])%*%mu[,j] from the model the full solution is commented out below
      # Different approaches provide different solutions although it remains to be tested whether
      # one is more appropriate
    Bij <- purrr::map(n_seq, function(i) {
      purrr::map(k_seq, function(j) {
        (V[[i]][[j]] %*% (t(x[[i]])/s.2[[j]]) %*%  (resid_ij[[i]][[j]])) + mu[, j]
        })
      }
      )
    
    # Bij <- lapply(1:N, function(i) {
    #   lapply(1:k, function(j) {
    #     (V[[i]][[j]]/s.2[[j]]) %*% (solve(R[[j]])%*% mu[,j] + (t(x[[i]])/s.2[j]) %*%  (y[[i]] - x[[i]] %*% mu[, j])) + mu[, j]
    #   })
    # }
    # )
    
    # # Using The Mixtools approach to calculate responsibilities
    # Calculates responsibilities by taking the weighted liklihood of belonging to class k
    # and deviding it by the sum of the weighted liklihood of belonging to all other classes.
    z = matrix(nrow = N, ncol = k)
    for (i in 1:N) {
      for (j in 1:k) {
        z.denom = c()
        for (m in 1:k) {
          z.denom = c(z.denom,
                      lambda[m] / lambda[j] *
                        (
                          det(margin_cov[[i]][[j]]) /
                          det(margin_cov[[i]][[m]])
                        ) ^ (0.5) *
                        exp(-0.5 * (
                          t(resid_ij[[i]][[m]]) %*% solve(margin_cov[[i]][[m]]) %*% resid_ij[[i]][[m]] -
                          t(resid_ij[[i]][[j]]) %*% solve(margin_cov[[i]][[j]]) %*% resid_ij[[i]][[j]]
                        )))
        }
        # Inverse is used to prevent indeterminant of 0/0 when liklihood exceed floating point
        z[i, j] = 1 / sum(z.denom) 
      }
     }
    # Actually solving for the responsibilities
    z = z / apply(z, 1, sum)

    # Singularity Checking
    sing <- sum(is.na(z))

    # Estimated N in each Class by summing over columns in z
    sum.z = apply(z, 2, sum)
    
    # Maximizing Mixing Proportions
    lambda.new = sum.z / N
    
    # Maximizing Mu
    mu_new <- do.call(cbind, lapply(1:k, function(j) {
      rowSums(do.call(cbind,
                      lapply(1:N, function(i) {
                        (z[i, j] * Bij[[i]][[j]]) / sum.z[j]
                      })))
    }))


    # Maximizing sigma_squared
    
    ## Calculating Numerator
    s.2_num <- lapply(1:k, function(j) {
      sum(sapply(1:N, function(i) {
        (z[i, j] * (sum((y[[i]] - x[[i]] %*% Bij[[i]][[j]]) ^ 2)  + 
                    sum(diag(t(x[[i]]) %*% x[[i]] %*% V[[i]][[j]]))))
      }))
    })
    
    ## Calculating Denominator
    s.2_den <- lapply(1:k, function(j) {
      sum(sapply(1:N, function(i) {
        z[i, j] * nrow(y[[i]])
      }))
    })
    
    s.2_new <- c()
    for (i in 1:k) {
      s.2_new[i] <- s.2_num[[i]] / s.2_den[[i]]
    }
    
    ## Maximizing R
    R.new <- lapply(1:k, function(j) {
      Reduce("+", lapply(1:N, function(i) {
        z[i, j] * (tcrossprod(Bij[[i]][[j]] - mu_new[, j]) +
                     V[[i]][[j]])
      })) / sum.z[j]
    })
    
    
    # Updating Parameters from previous iteration
    lambda <- lambda.new
    mu <- mu_new
    s.2 <- s.2_new
    R <- R.new

    # Estimating Likelihood
    L = matrix(nrow = N, ncol = k)
    L = t(sapply(1:N, function(i) {
      sapply(1:k, function(j) {
        mixtools::dmvnorm(as.vector(y[[i]]),
                as.vector(x[[i]] %*%
                            mu[, j]),
                margin_cov[[i]][[j]])
      })
    }))

    L.n = t(apply(t(L), 2, "*", matrix(lambda, nrow = 1)))
    newobsloglik = sum(log(apply(L.n, 1, sum)))

    # Singularity debugging from mixtools
    if (sing > 0 || is.na(newobsloglik) || abs(newobsloglik) ==
        Inf || newobsloglik < obsloglik) {
      cat("Need new starting values due to singularity...",
          "\n")
      restarts <- restarts + 1
      if (restarts > 15)
        stop("Too many tries!")
      start <-  my_mix_ml_init(
        y = y,
        x = x,
        k = k,
        arb.sigma = TRUE,
        arb.R = TRUE,
        addintercept.random = FALSE
      )

      # Extracting Starting Values
      sigma <- start$sigma
      lambda <- start$lambda
      mu <- start$mu
      R <- start$R
      k <- start$k
      s.2 <- sigma ^ 2
      diff <- 1
      iter <- 0
      obsloglik = -Inf
      ll <- NULL
    }
    else {
      diff <- newobsloglik - obsloglik
      obsloglik <- newobsloglik
      ll[iter] <- c(obsloglik)
      iter <- iter + 1
      cat("iteration=",
          iter,
          "diff=",
          diff,
          "log-likelihood",
          obsloglik,
          "\n")
    }
  }
  
if(!is.null(margin_id)){ 
 vcov_margin<- purrr::map(margin_id, function(i){
   purrr::map(k_seq, function(j){
      x[[i]] %*% R[[j]] %*% t(x[[i]]) + s.2[j] * Omega.k[[i]]
    })
 })
}
  
    return(list(Likelihood_history = ll[1:(iter-1)],
                Within_variance = s.2,
                Mixing_Proportions = lambda,
                Mean_RE = mu,
                VCOV_RE = R,
                posteriors = z,
                VCOV_Marginal = if(!is.null(margin_id))
                  {vcov_margin} else{
                    NULL
                  }))

}


set.seed(123)
my_mix_ml2(y ~ x, data = test, nest = "id", k = 2, margin_id = 1)
