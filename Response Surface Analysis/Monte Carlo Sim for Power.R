library(tidyverse)


resp_surf<-function(dep_var = NULL, fit_var = NULL, control = NULL, data = NULL, robust = FALSE, cluster = NULL){
  if(is.null(dep_var)){
    stop("Please provide a dependent variable in the dep_var argument.", call. = FALSE)
  }
  
  if(!is.character(dep_var)){
    stop("Please place the dependent variable in quotation marks.", call. = FALSE)
  }
  
  if(is.null(fit_var)){
    stop("Please provide two variables for which you are interested in examining congruence.", call. = FALSE)
  }
  
  if(length(fit_var) < 2){
    stop("To assess the effects of congruence, you need to provide fit_var with a vector containing the names of two variables.")
  }
  
  if(!is.character(fit_var)){
    stop("Please place the variables for which you are examining congruence in quotation marks.")
  }
  
  if(is.null(data)){
    stop("Please identify the dataframe containing your variables of intereste using the data argument.")
  }
  
  if(robust == TRUE & is.null(cluster)){
    warning("You have selected to use robust standard errors. If there are clustering factors, please identify them using the cluster argument.", call. = FALSE)
  }
  
  # Creating Formula --------------------------------------------------------
  #Building the equation; Code uses paste to collapse across vector of dvs and ivs to create a function
  dv<-dep_var
  pv<-paste(fit_var, collapse = "+")
  qt<-paste(fit_var, "_sq", sep = "")  
  int<-paste(fit_var,collapse = ":")
  
  #Adds conditionial processing in case no controls are entered into the model
  if(is.null(control)){
    
    rhs<-paste(c(pv,qt,int), collapse = "+")
    
  }else{
    
    cv<- paste(control, collapse = "+")
    
    rhs<-paste(c(pv,qt,int,cv), collapse = "+")
  }
  
  #Combines the right hand side of the equation with the dv
  equ<-paste(dv,rhs, sep = "~")
  
  #Converts to formula
  equation<-as.formula(equ)
  
  #Begins working with the data
  d<-data
  if(min(d[fit_var[1]], na.rm=TRUE)>0|min(d[fit_var[2]], na.rm=TRUE)>0){
    warning("The fit variables you are interested in have a minimum greater than 0. Consider centering your predictors on the scale midpoint.", call. = FALSE)
  }
  dif<-data.frame(id = 1:nrow(d))
  #Creates standardized difference variable
  dif$stdif<-scale(d[fit_var[1]])-scale(d[fit_var[2]])
  dif$stdif_cat<- cut(dif$stdif, breaks = c(-Inf, -.5, .5, Inf), labels = c(paste(fit_var[1], fit_var[2], sep = " < "), paste(fit_var[1], fit_var[2], sep = " congruent to "), paste(fit_var[1], fit_var[2], sep = " > ")))
  
  dif_tab<-table(dif$stdif_cat)/nrow(d)
  
  if(any(dif_tab<.1)){
    warning("Cell Frequencies of congruence or discongruence may be low. Check to make sure that there is enough difference in congruence to examine fit.")
  }
  #Creates quadratic terms based on the values provided in the fit_var argument
  d[qt[1]]<-c(d[fit_var[1]]^2)
  d[qt[2]]<-c(d[fit_var[2]]^2)
  
  #Fits a linear model using the equation created above
  resp_fit<-lm(equation, d)
  
  #Generates the standard error matrix
  if(robust == TRUE){
    se_mat <- sandwich::vcovCL(resp_fit, cluster = d[cluster])
  }else{
    se_mat <- vcov(resp_fit) 
  }
  
  #Converts the results into table format
  results<-data.frame(Estimates = coef(resp_fit), 
                      Standard_Error = sqrt(diag(se_mat)))
  
  #Calculates a t-test for the coefficients
  results$T_Test<-results$Estimates/results$Standard_Error
  
  #calculates the probability of the t-test degress of freedom are n-the coefficients being estimated
  results$P_value<-round(2*pt(q = -abs(results$T_Test), df = resp_fit$df.residual),4)
  
  
  #Calculates parameter estimates and generates the standard errors using the matrix defined above
  loi<-data.frame(Line_of_Interest = rep(c("Line of Congruence", "Line of Incongruence"), each = 2),
                  Parameter = rep(c("Linear", "Quadratic"), times = 2),
                  Estimate = c(coef(resp_fit)[fit_var[1]]+coef(resp_fit)[fit_var[2]],
                               coef(resp_fit)[qt[1]]+coef(resp_fit)[qt[2]]+coef(resp_fit)[[int]],
                               coef(resp_fit)[fit_var[1]]-coef(resp_fit)[fit_var[2]],
                               coef(resp_fit)[qt[1]]+coef(resp_fit)[qt[2]]-coef(resp_fit)[[int]]),
                  Standard_Error= c(sqrt(se_mat[fit_var[1],fit_var[1]]+se_mat[fit_var[2],fit_var[2]]+2*se_mat[fit_var[1],fit_var[2]]),
                                    sqrt(se_mat[qt[1],qt[1]]+se_mat[qt[2],qt[2]]+se_mat[int,int]+2*se_mat[qt[1],qt[2]]+2*se_mat[qt[1],int]+2*se_mat[qt[2],int]),
                                    sqrt(se_mat[fit_var[1],fit_var[1]]+se_mat[fit_var[2],fit_var[2]]-2*se_mat[fit_var[1],fit_var[2]]),
                                    sqrt((se_mat[qt[1],qt[1]]+se_mat[qt[2],qt[2]]+se_mat[int,int])+(2*se_mat[qt[1],qt[2]])-(2*se_mat[qt[1],int])-(2*se_mat[qt[2],int]))))
  
  #Runs Hypothesis testing along the lines of interest
  loi$T_Test<-loi$Estimate/loi$Standard_Error
  loi$P_value<-2*pt(q = -abs(loi$T_Test), df = resp_fit$df.residual)
  
  #Identifies where the stationary point is
  stat_pnt<-data.frame(` `= c("X Coordinates", "Y Coordinates"),
                       Value = c((coef(resp_fit)[fit_var[2]]*coef(resp_fit)[int]-2*coef(resp_fit)[fit_var[1]]*coef(resp_fit)[qt[2]])/(4*coef(resp_fit)[qt[1]]*coef(resp_fit)[qt[2]]-coef(resp_fit)[int]^2),
                                 (coef(resp_fit)[fit_var[1]]*coef(resp_fit)[int]-2*coef(resp_fit)[fit_var[2]]*coef(resp_fit)[qt[1]])/(4*coef(resp_fit)[qt[1]]*coef(resp_fit)[qt[2]]-coef(resp_fit)[int]^2)
                       ))
  #Calculates the principle axes slope and intercept on the x/y plane
  princ_axis<-data.frame(Axis = c("First", "First", "Second", "Second"),
                         Param = c("Intercept", "Slope", "Intercept", "Slope"),
                         Estimate = c(0,
                                      (coef(resp_fit)[qt[2]]-coef(resp_fit)[qt[1]]+sqrt((coef(resp_fit)[qt[1]]-coef(resp_fit)[qt[2]])^2+coef(resp_fit)[int]^2))/coef(resp_fit)[int],
                                      0,
                                      ((coef(resp_fit)[qt[2]]-coef(resp_fit)[qt[1]]-sqrt((coef(resp_fit)[qt[1]]-coef(resp_fit)[qt[2]])^2+coef(resp_fit)[int]^2))/coef(resp_fit)[int])))
  
  princ_axis[1,3]<-stat_pnt[2,2]-stat_pnt[1,2]*princ_axis[2,3]
  princ_axis[3,3]<-stat_pnt[2,2]-stat_pnt[1,2]*princ_axis[4,3]
  
  #Compiles results into list format
  out<-list(
    dif_tab = dif_tab,
    results = results,
    loi = loi,
    stat_pnt=stat_pnt,
    princ_axis=princ_axis,
    model=resp_fit,
    equation = equation)
  
  #Returns the list
  return(out)
}

plot_surf<-function(obj = NULL, max.x = NULL, min.x = NULL, max.y = NULL, min.y = NULL, inc = NULL, phi = 45, theta = 315, llabels =FALSE, xlab=NULL, ylab=NULL, zlab=NULL, ...){
  if((theta>360|theta<270) & !theta == 0){
    warning("Perspective may not align with the line of congruence. Consider retaining a theta between 360 and 270")
  }
  
  #Copies the model generated in the resp_surf function for use here
  eq<-obj[["model"]]
  
  #Generates a 2xn dimension data frame with every combination of x and y
  d<-expand.grid(list(x = seq(min.x, max.x, by = inc),
                      y = seq(min.y, max.y, by = inc)))
  
  #creates quadratic and interaction terms for prediction
  d$x_sq<-d$x^2
  d$y_sq<-d$y^2
  d$int<-d$x*d$y
  
  #names the variables what you have them named in the model
  colnames(d)<-names(coef(eq)[2:6])
  
  #uses the function generated in resp_surf to predict values
  d$y<-predict(eq, d)
  
  #prepares a vector of x values for plotting
  x<-seq(min.x, max.x, by = inc)
  #Prepares a vector of y values for plotting
  y<-seq(min.y, max.y, by = inc)
  #spreads the predictions generated above into a matrix where rows correspond with x and columns correspond with y
  z <- matrix(d$y, nrow = length(x), ncol=length(y))
  
  if(is.null(xlab)){
    xlab<-names(eq$coefficients)[2]
  }
  if(is.null(ylab)){
    ylab<-names(eq$coefficients)[3]
  }
  if(is.null(zlab)){
    zlab<-colnames(eq$model)[1]
  }
  
  #Creates a wire plot
  persp(x = x, y = y, z = z, phi = phi, theta = theta, shade = .5, 
        xlab = xlab, ylab = ylab, zlab = zlab, ...)-> res
  round(res, 3)
  #Plots the line of congruence
  lines(trans3d(x = x, y = x, z = min(z), pmat = res), col = "black", lty = 2)
  
  #grid on (x,y) plane
  for (ix in seq(min.x, max.x, by = inc)) lines (trans3d(x = ix, y = seq(min.y, max.y, by = inc), z = min(z), pmat = res), col = "black", lty ="solid")
  for (iy in seq(min.y, max.y, by = inc)) lines (trans3d(x = seq(min.y, max.y, by = inc), y = iy, z = min(z), pmat = res), col = "black", lty ="solid")
  
  #plots the line of incongruence
  lines(trans3d(x = x, y = -x, z = min(z), pmat = res), col = "black", lty = 4)
  #Labels for the line of congruence/incongruence
  if(llabels ==TRUE){
    text(trans3d(x =min(x)+1.75, y = min(y)+1.5, z = min(z), pmat = res), labels = "Line of Congruence", cex = .75, srt =70)
    text(trans3d(x =min(x)+1.5, y = max(y)-1.75, z = min(z), pmat = res), labels = "Line of Incongruence", cex = .75, srt =350)
  }
  legend(x= "bottomleft", legend = c("Line of Congruence", "Line of Incongruence"), xpd = TRUE,cex = .75,  bty = "n", lty = c(2,4))
  
}


# simulates leader follower dependence where ICC = .09
iter<-10000

# Define cor_mat
cor_mat<-matrix(c(1, 0,
                  0, 1), 
                byrow = TRUE, 2,2)
# Define population slope and intercepts
beta<-c(0, 0, -.025, -.025, .05)

# Calculate line of congruence line of discongruence
congruence_lin <-sum(beta[1:2])
congruence_quad<- sum(beta[3:5])
incongruence_lin<-beta[1]-beta[2]
incongruence_quad<- sum(beta[3:4])-beta[5]

# Janky way to solve for population epsilon -------------------------------
# No analytic solution for covariance of product distribution (interaction) developed
# Variance of quadratic can be solved analytically, covariances cannot
# This means epsilon must be solved for using sims given total variance of y and subtracting variance of y_hat
sample<-1000
eps_list<-c()
rest_eps_list<-c()

# for i in iterations
for(i in 1:iter){
  # Sim data
  suppressMessages(
  sim_help<-MASS::mvrnorm(sample, c(0,0), cor_mat)%>%
    as_tibble(.name_repair = "unique")%>%
    rename(precep_help = ...1,
           nurse_help = ...2)%>%
    mutate(precep_help_sq = precep_help^2,
           nurse_help_sq = nurse_help^2,
           int = precep_help*nurse_help)
  )
  
  sim_help$mastery_hat<-as.matrix(sim_help)%*%beta
  
  rest_eps_list<-1-var(as.matrix(sim_help%>%select(precep_help, nurse_help))%*%beta[1:2])
  eps_list[i] <- 1-var(sim_help$mastery_hat)
  
}

mean(rest_eps_list)-mean(eps_list)
# Defines Error Variance
err_var <- mean(eps_list)

# Main loop to solve for power --------------------------------------------

# Define the range of sample Sizes
sample_range <- seq(100, 500, by = 50)

results_list<-list()

# Iterates over sample sizes
for(sample_size in sample_range){

# Initializes data.frame to store power output
output<-data.frame()

# for i in iterations
for(i in 1:iter){
  
  # Sim data
suppressMessages(
  sim_help<-MASS::mvrnorm(sample_size, c(0,0), cor_mat)%>%
    as_tibble(.name_repair = "unique")%>%
    rename(precep_help = ...1,
           nurse_help = ...2)%>%
    mutate(precep_help_sq = precep_help^2,
           nurse_help_sq = nurse_help^2,
           int = precep_help*nurse_help)
)
  
  sim_help$mastery<-as.matrix(sim_help)%*%beta+rnorm(sample_size, 0, sqrt(err_var))

  m<-resp_surf(dep_var = "mastery", fit_var = c("precep_help", "nurse_help"), data = sim_help, robust = FALSE)
  
  output<-bind_rows(output, m$loi)
  
}

# Estimates effects for the lines of interest
results_list[[as.character(sample_size)]]<-output%>%
                                    group_by(Line_of_Interest, Parameter)%>%
                                    summarise(Estimate = mean(Estimate), 
                                              Power = mean(P_value<.05))

}



incon_power<-map_dfr(sample_range, function(n){
  results_list[[as.character(n)]][4,]%>%
    mutate(sample_size = n)
  
})

# Plotting Hypothesized Models
# Sim data
sample_size<-20000
suppressMessages(
  sim_help<-MASS::mvrnorm(sample_size, c(0,0), cor_mat)%>%
    as_tibble(.name_repair = "unique")%>%
    rename(precep_help = ...1,
           nurse_help = ...2)%>%
    mutate(precep_help_sq = precep_help^2,
           nurse_help_sq = nurse_help^2,
           int = precep_help*nurse_help)
)


sim_help$mastery<-as.matrix(sim_help)%*%beta+rnorm(sample_size, 0, sqrt(err_var))

m<-resp_surf(dep_var = "mastery", fit_var = c("precep_help", "nurse_help"), data = sim_help, robust = FALSE)

plot_ly_surf(m, 2, -2, 2, -2, inc = .25, xlab = "Protege Help", ylab = "Mentor Help", zlab = "Task Mastery" )
