# A semi-hacky way to find the appropriate error variance of y to ensure it aligns with the simulation's goal
# Uses variance algebra and the assumptions of regression to identify the appropriate error standard dev
# Given beta and predictor variance
# Based on monte carlo simulation approach


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
