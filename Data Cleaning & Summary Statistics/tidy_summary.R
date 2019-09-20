test1<-eth_TCH_final[,c("eth_match", "eth_minority", "Tenure_days", "yrs_nurse", "precep_count", "jobsat", "TOI", "TO")]
test2<-eth_TCH_final%>%
  select(eth_match, eth_minority, Tenure_days, yrs_nurse, precep_count, js1, js2, js3, toi1, toi2, toi3, TO)

auto_sum_tab<-function(x, order, item = FALSE, digits = 2, ...){
out<-list()
if(item == TRUE){

   item_variance<-x%>%
     mutate(id = 1:nrow(x))%>%
     gather(key = key, value = value, -id)%>%
     filter(str_detect(key, "[1-9]$|[1-9]r$"))%>%
     mutate(scale = str_extract(key, "^[A-Za-z[:punct:]]*"),
            item = str_extract(key, "[1-9]$|[1-9]r$"))%>%
     group_by(scale, item)%>%
     summarise(item_var = var(value, na.rm = TRUE))%>%
     ungroup()%>%
     group_by(scale)%>%
     summarise(num_items = n(),
               sum_i_var = sum(item_var, na.rm = TRUE))
    

   sum_score<-x%>%
     mutate(id = 1:nrow(x))%>%
     gather(key = key, value = value, -id)%>%
     filter(str_detect(key, "[1-9]$|[1-9]r$"))%>%
     mutate(scale = str_extract(key, "^[A-Za-z[:punct:]]*"),
            item = str_extract(key, "[1-9]$|[1-9]r$"))%>%
     group_by(id, scale)%>%
     summarise(sum_score = sum(value, na.rm = FALSE))%>%
     spread(key = scale, value = sum_score)
   
   sum_var<-sum_score%>%
     ungroup()%>%
     summarise_all(.funs = list(~var(., na.rm = TRUE)))%>%
     select(-id)%>%
     gather(key = scale, value = sum_score)
   
  alpha_df<-left_join(item_variance, sum_var)%>%
    mutate(alpha = (num_items/(num_items-1))*(1-(sum_i_var/(sum_score))))
   
  scale_score<-x%>%
    mutate(id = 1:nrow(x))%>%
    gather(key = key, value = value, -id)%>%
    filter(str_detect(key, "[1-9]$|[1-9]r$"))%>%
    mutate(scale = str_extract(key, "^[A-Za-z[:punct:]]*"),
           item = str_extract(key, "[1-9]$|[1-9]r$"))%>%
    group_by(id, scale)%>%
    summarise(scale_score = mean(value, na.rm = TRUE))%>%
    spread(key = scale, value = scale_score)
  
  x<-bind_cols(scale_score, x[!str_detect(colnames(x), "[1-9]$|[1-9]r$")])%>%
    ungroup()%>%
    select(-id)
  
  out[["alpha"]]<-alpha_df%>%
    select(-sum_i_var, -sum_score)
  
  out[["data_set"]]<-x

} 
  
  my_summary <- x%>%
    tidyr::gather(key = "var", value = "value")%>%
    dplyr::group_by(var)%>%
    dplyr::summarise(mean = round(mean(value, na.rm = TRUE), digits = digits), sd = round(sd(value, na.rm = TRUE), digits = digits))
  
  
  cor_sum<-psycho::correlation(x[order], ...)
  
  cor_table<- cor_sum$summary
  
  cor_table["var"]<-rownames(cor_table)
  
  out[["summary_stats"]]<-left_join(cor_table, my_summary)%>%select(var, mean, sd, everything())
 
  
  out

}

out_1<-summary_tab(test1, order = c("eth_match",    "eth_minority", "TOI", "Tenure_days",  "yrs_nurse",   "precep_count", "jobsat", "TO"), adjust = "none")
out_2<-summary_tab(test2, order = c("eth_match",    "eth_minority", "toi", "Tenure_days",  "yrs_nurse",   "precep_count", "js", "TO"),  item = TRUE, digits = 2)

