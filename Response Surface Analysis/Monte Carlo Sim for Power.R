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

plot_ly_surf<-function(obj = NULL, max.x = NULL, min.x = NULL, max.y = NULL, min.y = NULL, inc = NULL,  xlab=NULL, ylab=NULL, zlab=NULL, showscale = FALSE){
  
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
  
  plotly::plot_ly(x = x, y = y, z = ~z, showscale = showscale)%>% 
    plotly::add_surface()%>%
    plotly::add_trace(x = x[x==y], y = y[x==y], z = diag(z), line = list(color = "black"),type = 'scatter3d', mode = 'lines', name = "Line of Congruence")%>%
    plotly::add_trace(x = x, y = y[length(y):1], z = z[row(z) == (ncol(z)-col(z)+1)], line = list(color = "black", dash = 'dash'),type = 'scatter3d', mode = 'lines', name = "Line of Incongruence")%>%
    plotly::layout(scene = list(xaxis = list(title = xlab), 
                                yaxis = list(title = ylab), 
                                zaxis = list(title = zlab)))%>%
    plotly::add_trace(x = x, 
                      y = obj$princ_axis$Estimate[1]+obj$princ_axis$Estimate[2]*x,
                      z = min(z),
                      line = list(color = "red"),type = 'scatter3d', mode = 'lines', name = "First Principal Axis")%>%
    plotly::add_trace(x = x, 
                      y = obj$princ_axis$Estimate[3]+obj$princ_axis$Estimate[4]*x,
                      z = min(z),
                      line = list(color = "red", dash = 'dash'),type = 'scatter3d', mode = 'lines', name = "Second Principal Axis")
}

gen_response_surf_x<-function(n, cor_mat, x_names=NULL){
  sample<-n
  
  suppressMessages(
    sim_help<-MASS::mvrnorm(sample, c(0,0), cor_mat)%>%
      as_tibble(.name_repair = "unique")%>%
      rename(x1 = ...1,
             x2 = ...2)%>%
      mutate(x1_sq = x1^2,
             x2_sq = x2^2,
             int = x1*x2)
  )
  if(!is.null(x_names)){
    colnames(sim_help)<-c(x_names, paste0(x_names, "_sq"), paste0(x_names, collapse = "*"))
  }
  
  sim_help
}

gen_response_surf_y<-function(x_data, beta, sigma=NULL, y_name=NULL){
  
  if(is.null(y_name)){
    y_name<-"y"
  }
  
  if(is.null(sigma)){
    eps<-0
  }else{
    eps<-rnorm(nrow(x_data), 0, sigma)
  }
  
  x_data[y_name]<-as.vector(as.matrix(x_data)%*%beta+eps)
  
  x_data
}

find_sig<-function(n, cor_mat, beta, target_var_y = 1, iter=10000){
  
  sig_vec<-c()
  
  for(i in 1:iter){
    simmed_data<-gen_response_surf_x(n, cor_mat)%>%
      gen_response_surf_y(beta = beta)
    
    sig_vec[[i]]<-target_var_y-var(simmed_data$y)
    
  }
  
  sig_val<-mean(sig_vec)
  
  message("To generate an outcome variable with a variance of ", target_var_y, " given your betas, use a error sd of")
  
  sig_val
}


# Defining Power Assumptions ----------------------------------------------

# Define cor_mat
cor_mat<-matrix(c(1, 0,
                  0, 1), 
                byrow = TRUE, 2,2)


# Defining betas
beta<-list(c(0, 0, -.075, -.075, .15),
           c(0, 0, -.05, -.05, .1),
           c(0, 0, -.025, -.025, .05))

# Increments of sample size ranges
sample_range <- seq(100, 1000, by = 50)

# Initalizes Results
power_results<-data.frame()

# iterates through betas
for(b in 1:length(beta)){
  
  message("Beta: ", b, " of ", length(beta))
  
  # Finds appropriate error for each beta
  error_sig<-find_sig(n = 1000, cor_mat = cor_mat, beta = beta[[b]], target_var_y = 1)
  
  # iterates through sample sizes
  for(s in 1:length(sample_range)){
    
    message("Sample Size: ", s, " of ", length(sample_range))
    # Initializes iteration data frame starting new each time
    output<-data.frame()
    
    # Runs iter iterations Monte Carlos samples for each beta sample size combo
    for(i in 1:iter){
      
      # simulates data for iteration i
      sim_data<- gen_response_surf_x(sample_range[s], cor_mat = cor_mat, x_names = c("precep_help", "nurse_help"))%>%
        gen_response_surf_y(beta = beta[[b]], sigma = error_sig, y_name = "mastery")
      
      # Runs response surface model
      m<-resp_surf(dep_var = "mastery", fit_var = c("precep_help", "nurse_help"), data = sim_data, robust = FALSE)
      
      # Pulls out lines of interest
      output<-bind_rows(output, m$loi)
    }
    

     # Calculates Power Results for model
     power_results<- bind_rows(power_results, output%>%
                                 group_by(Line_of_Interest, Parameter)%>%
                                 summarise(Estimate = mean(Estimate), 
                                           Power = mean(P_value<.05))%>%
                                 mutate(`Sample Size` = sample_range[s],
                                        `Effects` = paste(beta[[b]], collapse = "_"))
     )
     
  }
}

sigs<-c()
  for(b in 1:length(beta)){
    sigs[b]<-find_sig(n = 1000, cor_mat = cor_mat, beta = beta[[b]], target_var_y = 1)
  }

1-sigs


p<-power_results%>%
  filter(Parameter == "Quadratic", Line_of_Interest == "Line of Incongruence")%>%
  mutate(Effect = case_when(Effects == "0_0_-0.025_-0.025_0.05" ~ "Small",
                            Effects == "0_0_-0.05_-0.05_0.1" ~ "Medium",
                            Effects == "0_0_-0.075_-0.075_0.15" ~ "Large"))%>%
  ggplot(aes(x = `Sample Size`, y  = Power, lty = Effect))+
  geom_smooth(se = FALSE, color = "black")+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(),
        axis.line.y = element_line())+
  geom_hline(yintercept = .8, lty = 4)+
  geom_vline(xintercept = 202, lty = 1)+
  geom_vline(xintercept = 407, lty = 2)+
  labs(title = "Estimated Power Curves for Quadratic Effect Along Line of Incongruence",
       subtitle = "Generated using 10,000 Monte Carlo Samples per Condition",
       caption = "Note. Sample size was changed by increments of 50. Curves are generated by a loess smoother.")+
  scale_x_continuous(breaks = seq(100, 1000, by = 50))

p

sim_help<-gen_response_surf_x(1000, cor_mat, x_names = c("precep_help", "nurse_help"))%>%
            gen_response_surf_y(beta = beta[[1]], y_name = "mastery")

m<-resp_surf(dep_var = "mastery", fit_var = c("precep_help", "nurse_help"), data = sim_help, robust = FALSE)

plot_ly_surf(m, 2, -2, 2, -2, inc = .25, xlab = "Protege Help", ylab = "Mentor Help", zlab = "Task Mastery" )
