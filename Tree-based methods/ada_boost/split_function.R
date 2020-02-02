# Build nodes from scratch for regression
split_rule<-function(y, x, data, cost_fun = ols){
  y_quo<-rlang::sym(y)
  cost_data<-data.frame()
  for(j in 1:length(x)){
    split_x<-rlang::sym(x[j])
    split_vals<-as.vector(unique(data[,x[j], drop = TRUE]))
    for(i in 1:length(split_vals)){
      split_dat<- data%>%
        mutate(split = if_else(!!split_x>=split_vals[i], 1, 0))
      
      cost_dat<-suppressMessages(
        split_dat%>%
          group_by(split)%>%
          summarise(pred = mean(!!y_quo, na.rm =TRUE))%>%
          ungroup()%>%
          full_join(split_dat)%>%
          summarise(cost = cost_fun(!!y_quo, pred))
      )
      
      
      cost_data[nrow(cost_data)+1, "var"]<-x[j]
      cost_data[nrow(cost_data), "split_value"]<-split_vals[i]
      cost_data[nrow(cost_data), "cost"]<-cost_dat$cost
      
      if(j == 1 & i == 1){
        best_cost<-cost_dat$cost
        best_split_var<-x[j]
        best_split_point<-split_vals[i]
      }else{
        if(cost_dat$cost<best_cost){
          best_cost<-cost_dat$cost
          best_split_var<-x[j]
          best_split_point<-split_vals[i]
        }
      }
    }
  }
  list(best_cost = best_cost, 
       best_split_var = best_split_var, 
       best_split_point = best_split_point,
       cost_data = cost_data)
}
