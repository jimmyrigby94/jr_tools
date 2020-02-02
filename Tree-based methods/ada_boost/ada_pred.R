


ada_pred<-function(new_data, models, y){
 y_quo<-sym(y)
 new_data<- new_data%>%
    mutate(row_number = 1:n())

 data_list<-split(new_data, new_data$row_number)
  
  
  
  map_dfr(data_list, function(x){
    suppressMessages(
      x%>%
        gather(key = split_var, value = value, -row_number, -!!y_quo)%>%
        right_join(models)%>%
        mutate(pred = if_else(value>=split_value, pred_with>=.5, pred_without>=.5), 
               pred = pred*alpha)
    )
  })%>%
    group_by(row_number)%>%
    summarise(pred = sum(pred)/sum(alpha), 
              observed = mean(!!y_quo))

}
