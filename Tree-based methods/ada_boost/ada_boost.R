ada_boost<-function(y, x, data, iter = 10){
  # Initalize Weights
  n_rows<-nrow(data)
  w<-rep(1/n_rows, times = n_rows)
  
  # Data Storage
  model_out<-data.frame()
  for (i in 1:iter){
    output_split<-ada_split(y = y, x = x, data = data,weights = w)
    
    w<-output_split$weights
    
    model_out[i,"iteration"]<-i
    model_out[i,"alpha"]<-output_split$alpha
    model_out[i, "split_var"]<-output_split$best_split_var
    model_out[i, "split_value"]<-output_split$best_split_point
    model_out[i, "pred_with"]<-output_split$pred_with
    model_out[i, "pred_without"]<-output_split$pred_without
  }
  
  model_out
}
