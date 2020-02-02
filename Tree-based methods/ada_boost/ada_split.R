# Build nodes from scratch for regression
ada_split<-function(y, x, data, weights){
  y_quo<-rlang::sym(y)

  for(j in 1:length(x)){
    
    split_x<-rlang::sym(x[j])
    split_vals<-as.vector(unique(data[,x[j], drop = TRUE]))
    
    
    for(i in 1:length(split_vals)){
      split_dat<- data%>%
        mutate(split = if_else(!!split_x>=split_vals[i], 1, 0))
      
      pred_dat<-suppressMessages(
        split_dat%>%
          group_by(split)%>%
          summarise(pred = mean(!!y_quo, na.rm = TRUE))%>%
          ungroup()%>%
          full_join(split_dat)
      )
      
      cost_dat<-pred_dat%>%
        summarise(cost = adaboost_objective(y = y, x = pred, w = weights, data = data))
      

      if(j == 1 & i == 1){
        best_cost<-cost_dat$cost
        best_split_var<-x[j]
        best_split_point<-split_vals[i]
        misclass<-(pred_dat$pred>.5)!=as.logical(pull(data, y))
      }else{
        if(cost_dat$cost<best_cost){
          best_cost<-cost_dat$cost
          best_split_var<-x[j]
          best_split_point<-split_vals[i]
          misclass<-(pred_dat$pred>.5)!=as.logical(pull(data, y))
          
          pred_with<-pred_dat$pred[pred_dat$split==1][1]
          
          pred_without<-pred_dat$pred[pred_dat$split==0][1]
        }
      }
    }
  }
  
  alpha<-log((1-best_cost)/best_cost)
  weights<-w_update(best_cost, weights, misclass = misclass, alpha)
  
  list(best_cost = best_cost, 
       best_split_var = best_split_var, 
       best_split_point = best_split_point,
       alpha=alpha,
       weights=weights,
       pred_with = pred_with,
       pred_without = pred_without)
}

