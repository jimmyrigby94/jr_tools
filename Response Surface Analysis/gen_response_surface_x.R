# Generates the necessary independent variables for response surface analysis. Allos for them to be correlated
# n = number of observations to generate
# cor_mat = correlation structure of key variables
# x_names = names of x variables (vector length 2)
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
    colnames(sim_help)<-c(names, paste0(names, "_sq"), paste0(names, collapse = "*"))
  }
}

