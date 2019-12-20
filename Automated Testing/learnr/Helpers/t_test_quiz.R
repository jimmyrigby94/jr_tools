# Generate quiz given conditional arguments defined in ...
t_test_quiz<-function(args, stem, number_items, ..., random = FALSE, replace = TRUE, parse = TRUE, solution = TRUE){
  
  # Handling tidy eval
  conditions<-enquos(...)
  
  # apply filters to args
  results<-filter(args, !!! conditions)
  
  # Generate Numerical Results
  num_results<-nrow(results)
  
  # Take a randome sample of results OR
  if(random){
    args<-sample_n(results, number_items, replace = replace)
  }else{
    
    # Use index to sequence through them
    rep_t<-number_items%/%num_results
    tail_seq<-number_items%%num_results
    
    index<-c(rep(1:num_results, each = rep_t), 1:tail_seq)
    
    args<-results[index,]%>%
      arrange()
  }
  
  # Uses t_test_question internally to fill question stem and generate solutions
  t_test_question(args = args, stem = stem, parse = TRUE, solution = TRUE)
}
