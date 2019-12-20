library(tidyverse)

# Set a seed for repeatable plots
set.seed(12345)

# Defining Kernel Function Utilizing the squared exponential kernel
sq_exp_kernel<-function(x1, x2, l = 1){
  Sigma<-matrix(rep(0, length(x1)*length(x2)), nrow = length(x1))

  for (i in seq_along(x1)){
    for (j in seq_along(x2)){

    Sigma[i, j]<-exp(-0.5*(abs(x2[j]-x1[i])/l)^2)

    }
  }
  
  Sigma
}

# 1. Plot some sample functions from the Gaussian process
# as shown in Figure 2.2(a)

# Define the points at which we want to define the functions
x.star <- seq(-5,5,len=50)

# Calculate the covariance matrix
Sigma <- sq_exp_kernel(x.star,x.star)

draw<-function(n_draws, x, mu, cov){
values<-map_dfr(1:n_draws, function(sample){
  
  
  draw<-t(MASS::mvrnorm(1, 
                        mu = mu, 
                        Sigma = cov))
  
  draw<-as.vector(draw)
  
  tmp<-data.frame(sample = sample, 
                  x = x,
                  value = draw)
  
  tmp
})

values
}

prior<-draw(n_draws = 20, x.star, rep(0, length(x.star)), cov = Sigma)

# Plot the result
fig2a <- ggplot(prior,aes(x=x,y=value)) +
  geom_rect(xmin=-Inf, xmax=Inf, ymin=-2, ymax=2, fill="grey80") +
  geom_line(aes(group=sample)) +
  theme_bw() +
  scale_y_continuous(lim=c(-2.5,2.5), name="output, f(x)") +
  xlab("input, x")

fig2a

# 2. Now let's assume that we have some known data points;
# this is the case of Figure 2.2(b). In the book, the notation 'f'
# is used for f$y below.  I've done this to make the ggplot code
# easier later on.
f <- data.frame(x=c(-4,-3,-1,0,2),
                y=c(-2,0,1,2,-1))

# Calculate the covariance matrices
# using the same x.star values as above

post<-function(x, x.star, y, sigma.n=0){
  
  k.xx <- sq_exp_kernel(x,x)
  k.xxs <- sq_exp_kernel(x,x.star)
  k.xsx <- sq_exp_kernel(x.star,x)
  k.xsxs <- sq_exp_kernel(x.star,x.star)
  
  f.star.bar<-k.xsx%*%solve(k.xx+sigma.n^2*diag(1, ncol(k.xx)))%*%y
  
  f.star.cov<- k.xsxs - k.xsx%*%solve(k.xx+sigma.n^2*diag(1, ncol(k.xx)))%*%k.xxs
  
  f.star.bar<-as.vector(f.star.bar)
  
  list(post_mean = f.star.bar,
       post_var = f.star.cov)
}


post_params<-post(f$x, x.star, f$y)



# This time we'll plot more samples.  We could of course
# simply plot a +/- 2 standard deviation confidence interval
# as in the book but I want to show the samples explicitly here.

post<-draw(50, x = x.star, mu = post_params$post_mean, cov = post_params$post_var)

# Plot the results including the mean function
# and constraining data points
fig2b <- ggplot(post,aes(x=x,y=value)) +
  geom_line(aes(group=sample), colour="grey80")+
  geom_line(data = data.frame(x.star = x.star, 
                              y = post_params$post_mean),
            aes(x=x.star,y=y, group = NULL),colour="red", size=1, inherit.aes = FALSE) + 
  geom_point(data=f,aes(x=x,y=y)) +
  theme_bw() +
  scale_y_continuous(lim=c(-3,3), name="output, f(x)") +
  xlab("input, x")

fig2b

# 3. Now assume that each of the observed data points have some
# normally-distributed noise.

# The standard deviation of the noise
sigma.n <- 0.4

post_params<-post(f$x, x.star, f$y, sigma.n = sigma.n)



# This time we'll plot more samples.  We could of course
# simply plot a +/- 2 standard deviation confidence interval
# as in the book but I want to show the samples explicitly here.

post<-draw(50, x = x.star, mu = post_params$post_mean, cov = post_params$post_var)


# Plot the result, including error bars on the observed points
fig3 <- ggplot(post,aes(x=x,y=value)) +
  geom_line(aes(group=sample), colour="grey80")+
  geom_line(data = data.frame(x.star = x.star, 
                              y = post_params$post_mean),
            aes(x=x.star,y=y, group = NULL),colour="red", size=1, inherit.aes = FALSE) + 
  geom_point(data=f,aes(x=x,y=y)) +
  theme_bw() +
  scale_y_continuous(lim=c(-3,3), name="output, f(x)") +
  xlab("input, x")

fig3
