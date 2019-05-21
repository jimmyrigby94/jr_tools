library(dplyr)
library(MASS)
library(sandwich)
library(plot3D)
library(plotly)
set.seed(123)


# Generating Test Data Set ------------------------------------------------


#Means and variances for predictor variables are approximated from Wilson et al (2016)
test<-as.data.frame(mvrnorm(n = 400, mu = c(5.47, 5.41), Sigma = matrix(c(2.7, .03, .03, 2.7), nrow = 2, ncol = 2)))

set.seed(234)

# Effect sizes are generated below in the y line. Change the values to examine different effects. 
test<-test%>%
  mutate(
    V1 = c(scale(test$V1, center = TRUE, scale = FALSE)),
    V2 = c(scale(test$V2, center = TRUE,scale =FALSE)),
    y = 5.20-.38*V1+-.46*V2+.25*V1^2+.23*V2^2+ 1.03*V1*V2+rnorm(n = 400, mean = 0, sd = .13), 
    V1sq= V1^2,
    V2sq = V2^2)

v1<-cut(test$V1, breaks = 10)
v2<-cut(test$V2, breaks = 10)

hist3D(z=table(v1, v2), theta = 300)


# Writing Response Surface Function (with Robust SE option) ---------------

resp_surf<-function(dep_var = NULL, fit_var = NULL, control = NULL, data = NULL, robust = FALSE, cluster = NULL){
  if(min(fit_var[1])>0|min(fit_var[2])>0){
    warning("The fit variables you are interested in have a minimum greater than 0. Consider centering your predictors on the scale midpoint.", call. = FALSE)
  }
  
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
    se_mat <- sandwich::vcovCL(resp_fit, cluster = cluster)
  }else{
    se_mat <- vcov(resp_fit) 
  }
  
  #Converts the results into table format
  results<-data.frame(Estimates = coef(resp_fit), 
             Standard_Error = sqrt(diag(se_mat)))
  
  #Calculates a t-test for the coefficients
  results$T_Test<-results$Estimates/results$Standard_Error
  
  #calculates the probability of the t-test degress of freedom are n-the coefficients being estimated
  results$P_value<-pt(q = -abs(results$T_Test), df = nrow(d)-length(coef(resp_fit)))
  

  #Calculates parameter estimates and generates the standard errors using the matrix defined above
  loi<-data.frame(Line_of_Interest = rep(c("Line of Congruence", "Line of Incongruence"), each = 2),
                 Parameter = rep(c("Linear", "Quadratic"), times = 2),
                 Estimate = c(coef(resp_fit)[fit_var[1]][]+coef(resp_fit)[fit_var[2]],
                              coef(resp_fit)[qt[1]]+coef(resp_fit)[qt[2]]+coef(resp_fit)[[int]],
                              coef(resp_fit)[fit_var[1]]-coef(resp_fit)[fit_var[2]],
                              coef(resp_fit)[qt[1]]+coef(resp_fit)[qt[2]]-coef(resp_fit)[[int]]),
                 Standard_Error= c(sqrt(se_mat[fit_var[1],fit_var[1]]+se_mat[fit_var[2],fit_var[2]]+2*se_mat[fit_var[1],fit_var[2]]),
                                   sqrt(se_mat[qt[1],qt[1]]+se_mat[qt[2],qt[2]]+se_mat[int,int]+2*se_mat[qt[1],qt[2]]+2*se_mat[qt[1],int]+2*se_mat[qt[2],int]),
                                   sqrt(se_mat[fit_var[1],fit_var[1]]+se_mat[fit_var[2],fit_var[2]]-2*se_mat[fit_var[1],fit_var[2]]),
                                   sqrt(se_mat[qt[1],qt[1]]+se_mat[qt[2],qt[2]]+se_mat[int,int]-2*se_mat[qt[1],qt[2]]-2*se_mat[qt[1],int]+2*se_mat[qt[2],int])))
 
  #Runs Hypothesis testing along the lines of interest
  loi$T_Test<-loi$Estimate/loi$Standard_Error
  loi$P_value<-pt(q = -abs(loi$T_Test), df = nrow(d)-length(coef(resp_fit)))
  
  #Identifies where the stationary point is
  stat_pnt<-data.frame(` `= c("X Coordinates", "Y Coordinates"),
                       Value = c((coef(resp_fit)[fit_var[2]]*coef(resp_fit)[int]-2*coef(resp_fit)[fit_var[1]]*coef(resp_fit)[qt[2]])/(4*coef(resp_fit)[qt[1]]*coef(resp_fit)[qt[2]]-coef(resp_fit)[int]^2),
                                 (coef(resp_fit)[fit_var[1]]*coef(resp_fit)[int]-2*coef(resp_fit)[fit_var[2]]*coef(resp_fit)[qt[1]])/(4*coef(resp_fit)[qt[1]]*coef(resp_fit)[qt[2]]-coef(resp_fit)[int]^2)
                                 ))
  #Calculates the principle axes slope and intercept on the x/y plane
  princ_axis<-data.frame(Axis = c("First", "First", "Second", "Second"),
                         Param = c("Intercept", "Slope", "Intercept", "Slope"),
                         Estimate = c(0,
                                      (coef(resp_fit)[qt[2]]-coef(resp_fit)[qt[1]]+sqrt((coef(resp_fit)[qt[1]]-coef(resp_fit)[qt[2]])^2)+coef(resp_fit)[int]^2)/coef(resp_fit)[int],
                                      0,
                                      -((coef(resp_fit)[qt[2]]-coef(resp_fit)[qt[1]]-sqrt((coef(resp_fit)[qt[1]]-coef(resp_fit)[qt[2]])^2)+coef(resp_fit)[int]^2)/coef(resp_fit)[int])))
  
  princ_axis[1,3]<-stat_pnt[2,2]-stat_pnt[1,2]*princ_axis[2,3]
  princ_axis[3,3]<-stat_pnt[2,2]-stat_pnt[1,2]*princ_axis[4,3]
  
  #Compiles results into list format
  out<-list(
       dif_tab = dif_tab,
       dif_plot = dif_plot,
       results = results,
       loi = loi,
       stat_pnt=stat_pnt,
       princ_axis=princ_axis,
       model=resp_fit,
       equation = equation)
  
  #Returns the list
   return(out)
}



# Response Surface Plotting Function --------------------------------------

plot_surf<-function(obj = NULL, max.x = NULL, min.x = NULL, max.y = NULL, min.y = NULL, inc = NULL, phi = 45, theta = 315, llabels =TRUE, ...){
    if((theta>360|theta<270) & !theta == 0){
      warning("Perspective may not align with the line of congruence. Consider retaining a theta between 360 and 270")
    }

   #Copies the model generated in the resp_surf function for use here
  eq<-obj[[5]]
  
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
  
  #Creates a wire plot
  persp(x = x, y = y, z = z, phi = phi, theta = theta, shade = .5, ...)-> res
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


test.out<-resp_surf(dep_var = "y", fit_var = c("V1", "V2"), data = test, robust = TRUE)

plot_surf(obj = test.out, max.x = 3, min.x = -3, max.y = 3, min.y = -3, inc =1, phi= 35, theta = 330, llabels = TRUE ,lphi = 35, ltheta = 330,  xlab = "Pinnacle Agreeableness", ylab = "Other Agreeableness", zlab = "Positive Emotions", cex.lab=.75, border = FALSE, col ="grey")

sandwich::vcovCL(test.out$model)


dataset$precpt_id<-dataset%>%
  group_indices(preceptor_name)