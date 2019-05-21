library(tidyverse)
library(rlang)
library(psych)

### Original Implementation that has sped up my work ###
### Computes Cronbach's alpha for a set of scales that share a common stem ###
### Common stems are automatically detected by dropping numeric values at the end of a column name###
### Implements Tidy Eval so that the user facing side of the function is similar to dplyr ###


tidyalpha<-function(data, drop){
  dropquo<-enquo(drop)
  
    scaled<-data%>%
    mutate(id = 1:nrow(data))%>%
    mutate_all(.funs = funs(as.numeric))%>%
    select(-(!!dropquo))%>%
    gather(key = key, value = value, -id)%>%
    filter(str_detect(key, "[1-9]$|[1-9]r$"))%>%
    mutate(scale = str_extract(key, "^[A-Za-z[:punct:]]*"))%>%
    mutate(scale = as.factor(scale))%>%
    group_by(scale)%>%
    mutate(items =  n_distinct(key))%>%
    ungroup()%>%
    filter(items>1)
    
    
   listed<-split(scaled, scaled$scale ,drop = TRUE)
 
 spreadlist<-lapply(X = listed, FUN = spread, key = key, value = value)
 alphaprep<-lapply(X = spreadlist, FUN = select, -id, -scale, -items)
 lapply(X = alphaprep, FUN = alpha)
 
}


personality<-bfi%>%
  mutate(A1_r = 7-A1, 
         E1_r = 7-E1, E2_r=7-E2, 
         O2_r = 7-O2, O5_r=7-O5,
         C4_r = 7-C4, C5_r = 7-C5)

tidyalpha(data = personality, drop=c(A1,E1, E2, O2, O5, C4, C5))

