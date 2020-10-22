
set.seed(12345)
# mixed data with 2 clusters and known parameters:
a<-rnorm(n = 100, mean = 0, sd = 9)
b<-rnorm(n = 100, mean = 0, sd = 9)
c<-vector("numeric", length = 100)
c<-(5.43+2.65*b+1.97*b)*rep(c(0,1),50)
c1<-(1.67+5.32*a-3.88*b)*rep(c(1,0),50)
c[c==0]<-c1[c1!=0]

set.seed(12345)
# mixed data with 2 clusters, known parameters, & randomness:
a<-rnorm(n = 100, mean = 0, sd = 9)
b<-rnorm(n = 100, mean = 0, sd = 9)
c<-vector("numeric", length = 100)
c<-(5.43+2.65*b+1.97*b+rnorm(100,0,9))*rep(c(0,1),50)
c1<-(1.67+5.32*a-3.88*b+rnorm(100,0,9))*rep(c(1,0),50)
c[c==0]<-c1[c1!=0]

set.seed(12345)
# data with 3 clusters and known parameters
a<-rnorm(n = 100, mean = 0, sd = 9)
b<-rnorm(n = 100, mean = 0, sd = 9)
c1<-(5.43+2.65*b+1.97*b)*rep(c(0,0,1),length.out = 100)
c2<-(1.67+5.32*a-3.88*b)*rep(c(0,1,0),length.out = 100)
c3<-(3.78-1.32*a-1.22*b)*rep(c(1,0,0),length.out = 100)
c<-c1+c2+c3

set.seed(12345)
# data with 3 clusters, known parameters, and randomness
a<-rnorm(n = 100, mean = 0, sd = 9)
b<-rnorm(n = 100, mean = 0, sd = 9)
c1<-(5.43+2.65*b+1.97*b+rnorm(100,0,9))*rep(c(0,0,1),length.out = 100)
c2<-(1.67+5.32*a-3.88*b+rnorm(100,0,9))*rep(c(0,1,0),length.out = 100)
c3<-(3.78-1.32*a-1.22*b+rnorm(100,0,9))*rep(c(1,0,0),length.out = 100)
c<-c1+c2+c3

dat<-data.frame(c = c, a =a, b = b)


mix.reg<-function(formula=NULL, data, y=NULL, x=NULL, k, lambda = NULL, coef.start = NULL, sigma.start = NULL, random.starts = 10, return_all = FALSE){
  
  if(!is.null(y) & !is.null(x)){
    x<-as.matrix(cbind(1, x))
    y<-as.numeric(y)
  } else if(!is.null(formula) & !is.null(data)){
    x<-model.matrix(formula, data)
    y<-as.vector(model.frame(formula, data)[,!colnames(model.frame(formula, data)) %in% colnames(model.matrix(formula, data))])
  }else{
    stop("Please provide either a formula and data or x and y.")
  }
  
  # initializing values
  k<-k
  j<-length(y)
  iv<-ncol(x)
  ll_it<-vector(mode = "numeric", length = random.starts)
  tries <- list()
  
  # Protects against random starts if user provides intentional starts
  if(is.null(coef.start)&is.null(sigma.start)){
    rs<- random.starts
  }else{
    rs<-1
  }
  
  # Loops over random starts
  for(w in 1:rs){
    # initializing values
    post<-matrix(0, nrow = j, ncol = k)
    xb<-matrix(0, nrow = j, ncol = k)
    res<-matrix(0, nrow = j, ncol = k)
    ll<-vector(mode = "numeric", length = 1000)
    a<-2
    ll[1]<-0
    xw<-list(NULL)
    yw<-list(NULL)
    
    # Allowing for user defined mixture proportions
    if(is.null(lambda)){lambda <- as.vector(matrix(1/k, nrow = k))
    } else { 
      lambda<-lambda
    }
    
    # Sets coefficient starting values (if null adds random noise to coefficients where k = 1)
    if(is.null(coef.start)){
      b<-rbind(
        matrix(runif(n = k, min = 2*min(qr.coef(qr(x), y)), max = 2*max(qr.coef(qr(x), y))),ncol=k), # Intercepts
        matrix(runif(n = iv*k-k, min = 2*min(qr.coef(qr(x), y)[2:iv]), max = 2*max(qr.coef(qr(x), y)[2:iv])),ncol=k) # Slopes
      )
    } else {
      b<-coef.start
    }
    
    # Setting starting values for sigma - if null sigma adds noise to sigma where k = 1 (Updated to use qr.resid)
    if(is.null(sigma.start)){
      s<-runif(n = k, min = .001, max = var(y))
    } else {
      s<-sigma.start
    }
    
    # getting first log likelihood
    xb<-x%*%b #yhat bassed on start values
    post<-t(lambda*t(dnorm(x = y, mean = xb, sd = sqrt(s)))) #applying liklihood formula weigthing by mixture proprotions
    ll[2]<-sum(log(rowSums(post))) # generating liklihood and storing in vector
    
    # main loop
    # Look conditioned on liklihood changes
    while(abs(ll[a]-ll[a-1])>=1e-10){
      # Printing progress
      print(paste("Start", w, "iteration", a, "liklihood", ll[a]))
      
      # getting posterior probabilities
      post<-post/rowSums(post) # posteriors are ratios of liklihoods
      post[is.na(post)]<-1/k # assigns non-informative posterior if missing?
      lambda<-colMeans(post) # mixing proportions are the means of the ratio of liklihoods
      
      # solving for regression coefficients via WLS
      for(i in 1:k){
        xw[[i]]<-(post[,i]^.5)*x
        yw[[i]]<-(post[,i]^.5)*y
        #b[,i]<-QR(x = xw[[i]], y = yw[[i]], least.squares = T)[[3]]
        b[,i]<-qr.coef(qr(xw[[i]]), yw[[i]])
      }
      
      # getting predicted y and residuals
      xb<-x%*%b
      res<-post^.5*(y-xb) # Weighting the residuals based on posterior
      
      # calculating residual variance
      #s<-colSums(post*res^2)/(colSums(post))
      s<-colSums(res^2)/(j-iv)
      s<-s/colMeans(post)
      
      # updating posterior probabilities
      #post<-t(lambda*t(dnorm(x = y, mean = xb, sd = sqrt(s))))
      for(i in 1:k){
        post[,i]<-lambda[i]*dnorm(x = y, mean = xb[,i], sd = sqrt(s[i]))
      }
      
      # getting log likelihood
      ll[a+1]<-sum(log(rowSums(post)))
      a<-a+1
      
    }
    
    # Attempt to estimate the observed information matrix using second derivative
    # See section 2.15.2 in Machlachlan and Peel (2000); 
    # Unsure about terminolgy (i.e., complete data vs incomplete data)
    my_vcov<-list()
    se_mat<-matrix(nrow = ncol(x), ncol = k)
    
    for (i in 1:k){
      my_vcov[[i]]<- solve((t(xw[[i]])%*%xw[[i]])/s[[i]])
      se_mat[, i]<-sqrt(diag(my_vcov[[i]]))
      
    }


    
    # Attempt to estimate empirical observed information matrix using score matrix
    my_vcov2<-list()
    se_mat2<-matrix(nrow = ncol(x), ncol = k)

    for (i in 1:k){
      my_vcov2[[i]]<-solve(((res[,i]*t(xw[[i]]))/s[[i]])%*%t((res[,i]*t(xw[[i]]))/s[[i]]))
      se_mat2[, i]<-sqrt(diag(my_vcov2[[i]]))
      
      if(i == 1){
        full_score<- (res[,i]*t(xw[[i]]))/s[[i]]
      }else{
        full_score<-rbind(full_score, (res[,i]*t(xw[[i]]))/s[[i]])
      }
     
      
    }
    
    full_vcov<-solve(full_score%*%t(full_score))
    
    
    
    
    # source (https://www.stat.washington.edu/sites/default/files/files/reports/2009/tr559.pdf)
    aic<--2*log(exp(max(ll[ll!=0])))+2*(iv*k+2*k-1)
    bic<--2*log(exp(max(ll[ll!=0])))+(iv*k+2*k-1)*log(j)

  ll_it[[w]]<-max(ll[ll!=0])
  
  param<-as.data.frame(rbind(b, sqrt(s)))
  
  colnames(param)<-paste("mixture", 1:k)
  rownames(param)<-c("(Intercept)", colnames(x)[-1], "residual")
  
  colnames(se_mat2)<-paste("mixture", 1:k)
  rownames(se_mat2)<-c("(Intercept)", colnames(x)[-1])
  
  colnames(full_vcov)<-rownames(full_vcov)<-paste0("m", 1:k, "_", rep(c("(Intercept)", colnames(x)[-1]), times = k))
  
  
  tries[[w]]<-list(ll = ll[ll!=0],
                  fit = list(ll = ll[[which.max(ll[ll!=0])]],
                                   AIC = aic,
                                   BIC = bic),
                  param = param,
                  se_mat_o = se_mat,
                  observed_vcov = my_vcov,
                  se_mat_e = se_mat2,
                  empirical_vcov = my_vcov2,
                  full_vcov = full_vcov
                    )
  }
  
  if(return_all==TRUE){
    return(tries)
  }else{
    return(list(Output = tries[[which.max(ll_it)]], `Random Start Likelihood` = ll_it,
                if(sum(round(max(ll_it),2)==round(ll_it,2))<2){
                 Replicate =  "The best log liklihood solution did not replicate. Increase the number of random starts."
                }else{
                 Replicate =  "The best log liklihood replicated."
                }))
  }
}

test<-mix.reg(formula = c~a+b, data = dat, k = 3, random.starts = 200)
test




library(mixtools)
fm_out<-regmixEM(c, cbind(a,b),k = 3)
summary(fm_out)
boot.se(fm_out)
test$Output$param
