# Creates Questions given args and a stem
# Has options for generating solutions as well
t_test_question<-function(args, stem, parse = TRUE, solutions = TRUE){
  ############################
  #### Parse Embedded Code ###
  ############################
  if(parse){
    args<- args%>%
      rowwise()%>%
      mutate_at(vars(xbar1:n2), ~eval(parse(text = .)))%>%
      ungroup()
  }
  
  arg_names<-colnames(args)[!colnames(args)%in%c("chapter", "topic", "subtopic")]
  
  
  ##########################################
  ###### Private Helper for parsing ########
  ##########################################
  
  gen_ques<-function(arg_names, stem, args){
    returns<-list()
    for(i in 1:nrow(args)){
      tmp<-stem
      for(j in seq_along(arg_names)){
        tmp<-str_replace_all(tmp, paste0("\\{", arg_names[j], "\\}"), replacement = args[i,]%>%pull(arg_names[j])%>%as.character(.))
      }
      returns[i]<-tmp
    }
    
    unlist(returns)
  }
 
  
  #############################################
  ###### Applying parser to eqch question #####
  #############################################
  
  dist<-stem$dist
  question<-stem$question
  stem<-stem$stem
  
  questions<-args%>%
    transmute(
      chapter = chapter,
      topic = topic,
      subtopic = subtopic,
      parsed_stem = gen_ques(arg_names = arg_names, stem = stem, args = args),
      parsed_question = gen_ques(arg_names = arg_names, stem = question, args = args),
      parsed_dist = gen_ques(arg_names = arg_names, stem = dist, args = args)
    )
  
  
  #################################
  ####### Generating Solutions ####
  #################################
  
  if(solutions & parse){
    
    tmp_args<-args%>%
      select(-c(chapter:sample2, var))
    
    ans<-pmap(tmp_args, t_test_solution)%>%
      map(., ~round(., 2))
  }else if(solutions){
    message("To generate solutions for items, please parse the code.")
    ans<- NULL
  }else{
    ans<- NULL
  }
  
  
  list(questions=questions,
       solutions = ans,
       args = args)
}
