# Generate solutions given args
t_test_solution<-function(xbar1, xbar2, sd1, sd2, n1, n2, equal_var, directional, ask){
  
  ############
  ### prep ###
  ############
  
  equal_var<-str_detect(equal_var, pattern = "Assume")
  
  ###########
  ### DIF ###
  ###########
  dif<-xbar1-xbar2
  
  ###########
  ### SEM ###
  ###########
  
  # Equal variance
  if(equal_var){
    # Calculating pooled SD
    sp<-sqrt(
      ((n1-1)*sd1^2+(n2-1)*sd2^2)/(n1+n2-2)
    )
    
    # Calculating SEM
    sem<-sp/(sqrt(n1+n2))
    
    # Calculating DF
    df<-n1+n2-2
    
    #Unequal Variance
  }else{
    
    # Standard error of the mean
    sem<-sqrt(sd1^2/n1+sd2^2/n2)
    
    # Chunking degrees of freedom calc into numerator and denominator
    df_num<-((sd1^2/n1)+(sd2^2/n2))^2
    df_denom<-(1/(n1-1))*(sd1^2/n1)^2+(1/(n2-1))*(sd2^2/n2)^2
    
    df<-df_num/df_denom
  }
  
  ###########
  ###  T  ###
  ###########
  t<-dif/sem
  
  ###########
  ###  p  ###
  ###########
  
  if(str_detect(directional, "less than")){
    p<- pt(q = t, df = df)
  }else if(str_detect(directional, "different than")){
    p<- (1-pt(q = abs(t), df = df))/2
  }else{
    p<- 1-pt(q = t, df = df)
  }
  
  
  ############
  ###  CI  ###
  ############
  
  if(directional == "less than"){
    lci<- dif+sem*qt(.05, df)
    uci<-Inf
    
    ci<-c(lci, uci)
  }else if(directional == "greater than"){
    lci<--Inf
    uci<- dif+sem*qt(p = .95, df)
    ci<-c(lci, uci)
  }else{
    lci <- dif+sem*qt(.025, df)
    uci <- dif+sem*qt(.975, df)
    ci<-c(lci, uci)
  }
  
  #########
  ## Out ##
  #########
  
  if(str_detect(string = ask, pattern = "probability")){
    out<-p
  }else if(str_detect(string = ask, pattern = "t-value")){
    out<-t
  }else{
    out<- ci
  }
  
  out
}
