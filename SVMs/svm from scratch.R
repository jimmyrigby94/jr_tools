library(quadprog)
set.seed(12345)
# Generating Linearly Seperable Data
test<-data.frame(class = rep(c(1, -1), each = 10),
           x1 = c(rnorm(10, mean = -5), rnorm(10, mean = 5)),
           x2 = c(rnorm(10, mean = -5), rnorm(10, mean = 5)))

# Plotting Data
test%>%
  ggplot(aes(x = x1, y = x2, color = as.factor(class)))+
  geom_point(shape = 2)


# -------------------------------------------------------------------------

 # Optimization problem is composed of 1/2 aT G a - 1T a
 # min WRT a set of R^n
 # ST yT a = 0, a > 0
 # 
 # G  = [gij] and gij = yiyjx^tjxi

# Helper function to set up G matrix
g_constr<-function(y, X){
  n<- nrow(X)
  mat<-matrix(nrow = n, ncol = n)
  for(i in 1:n){
    yi<-y[i]
    xi<-as.numeric(as.vector(as.matrix(X[i,])))
    for (j in 1:n){
      xj<-as.numeric(as.vector(as.matrix(X[j,])))
      
      mat[i, j]<-yi*y[j]*(t(xj)%*%xi)
    }
  }  
  mat
}

# quadprog sets constraints using A matrix and b matrix
# A^T b >= b_0
# In this program A stores the constraints in rows
# the first is y^t %*% a = 0
# the rows that follow are a >= 0
n<-length(test$class)
eps<-1e-10

X<-cbind(Intercept = rep(1, n), test[c('x1', "x2")])
y<-test$class

lhs_const<-rbind(y, diag(1, nrow = n))
rhs_const<-c(0,rep(0,n))



d<-rep(1, times = n)


solution<-solve.QP(Dmat = g_constr(y, X)+eps*diag(n), 
               dvec = d,
               Amat = t(lhs_const), 
               bvec = rhs_const,
               meq = 1, # Sets the first contraint to equality instead of inequality
               factorized = FALSE)

solution$solution


# Recovering the Primal ---------------------------------------------------

# lagrange multipliers relate back to the primal in that 
# sum_i^n alpha_i*y_i*x_i


recover_primal<-function(alpha, y, X){
  p<-ncol(X)
  n<-nrow(X)
  
  output<-matrix(nrow = n, ncol = p)
  
  for(j in 1:p){
    
    output[,j]<-alpha*y*X[,j]
    
  }
  
  t(colSums(output))
}

# recovering the primals

w<-recover_primal(solution$solution, y, X)

w

# verifying the results

m<-e1071::svm(class~0+x1+x2, data = test, type = "C", scale = FALSE, kernel = "linear")

coef(m)

test%>%
  ggplot(aes(x = x1, y = x2, color = as.factor(class)))+
  geom_point(shape = 2)+
  geom_segment(aes(x = 7, y = -7*w[2]/w[3], xend = -7, yend = 7*w[2]/w[3]))



# Putting into SVM function -----------------------------------------------

my_svm<-function(f = NULL, data = NULL, y, X, slack = FALSE, C=.5){
  
  f<-update(f, .~.-1)
  # Extracting Model Matrices
  if(!is.null(f) & !is.null(data)){
    X<-model.matrix(f, data)
    
    y_name<-as.character(f)[2]
    
    y<-as.vector(
      as.matrix(
        data[y_name]
        )
      )
  }
  
  # extracting necessary parameters
  n<-length(y)
  eps<-1e-10
  
  # Creating matrices and vectors of quadratic programing
  lhs_const<-rbind(y, diag(1, nrow = n)) # matrix multiplied by the lagrange for constraints
  rhs_const<-c(0,rep(0,n)) # the rhs of constraints
  
  
  # If slack parameter is added, constraint is added such that -a>= -C or a <=C
  if(slack == TRUE){
    lhs_const<-rbind(lhs_const, diag(-1, nrow = n))
    rhs_const<-c(rhs_const, rep(-C, n))
  }
  
  
  d<-rep(1, times = n) # Initialized vector for multipliers
  
  solution<-solve.QP(Dmat = Matrix::nearPD(g_constr(y, X))$mat, 
                     dvec = d,
                     Amat = t(lhs_const), 
                     bvec = rhs_const,
                     meq = 1, # Sets the first contraint to equality instead of inequality
                     factorized = FALSE)
  
  
 results <- recover_primal(solution$solution, y, X)
 
 intercept<- -((X[y == -1]%*%results)[which.max(X[y == -1]%*%results)]+(X[y == 1]%*%results)[which.min(X[y == 1]%*%results)])/2
 
 results<-c(intercept, results)
 
 names(results)<-c("Intercept", colnames(X))
 
 results
}

my_svm(class~x1+x2, data = test)
m<-e1071::svm(class~0+x1+x2, data = test, type = "C", scale = FALSE, kernel = "linear")
coef(m)


# Generating Linearly Seperable Data
test<-data.frame(class = rep(c(1, -1), each = 10),
                 x1 = c(rnorm(10, mean = 1), rnorm(10, mean = 1)),
                 x2 = c(rnorm(10, mean = -1), rnorm(10, mean = 1)))

# Plotting Data
test%>%
  ggplot(aes(x = x1, y = x2, color = as.factor(class)))+
  geom_point(shape = 2)

w<-my_svm(class~x1+x2, data = test, slack = TRUE)
w

test%>%
  ggplot(aes(x = x1, y = x2, color = as.factor(class)))+
  geom_point(shape = 2)+
  geom_segment(aes(x = 7, y = -7*w[2]/w[3]-w[1], xend = -7, yend = 7*w[2]/w[3]-w[1]))

m<-e1071::svm(class~0+x1+x2, data = test, type = "C", scale = FALSE, kernel = "linear", cost = .5, shrinkage = FALSE)
w<-coef(m)
w

test%>%
  ggplot(aes(x = x1, y = x2, color = as.factor(class)))+
  geom_point(shape = 2)+
  geom_segment(aes(x = 7, y = -7*w[2]/w[3]-w[1], xend = -7, yend = 7*w[2]/w[3]-w[1]))
