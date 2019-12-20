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
