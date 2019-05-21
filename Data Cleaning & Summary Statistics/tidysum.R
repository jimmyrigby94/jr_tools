# This function creates scale scores for a set of items. It assumes you have a
# consistent naming scheme for scales that ends in either r (denoting reverse
# coded items) or a numeric value. 

### Arguments ###
# data: A dataset 
# drop: Variables you want to not include in scale score. These may include the raw items that are not reverese coded.
# stem: a custom regular expression that identifies the naming convention that are used in the scale. Defaults to alphanumeric characters. 
# tail: a custom regular expression that identifies the naming convetion that is used for the items. Defaults to numeric characters and r (for reverse coded items).


tidysum<-function(data, drop, stem=NULL, tail=NULL){
  dropquo<-enquo(drop)

  if(is.null(tail)){
    use_tail<-"[0-9]$|[0-9]r$|[0-9]_r"
  }else{
    use_tail<-tail
  }

  if(is.null(stem)){
    use_stem<-"^[A-Za-z[:punct:]]*"
  }else{
    use_stem<-stem
  }

  scaled <- data %>%
    dplyr::mutate(unique_row_id = 1:nrow(data))%>%
    dplyr::select_if(function(x){is.numeric(x)|is.integer(x)}) %>%
    dplyr::select(-(!!dropquo))%>%
    tidyr::gather(key = key, value = value, -unique_row_id)%>%
    dplyr::filter(stringr::str_detect(key, use_tail))%>%
    dplyr::mutate(scale = stringr::str_extract(key,  use_stem))%>%
    dplyr::mutate(scale = as.factor(scale))%>%
    dplyr::group_by(unique_row_id, scale) %>%
    dplyr::summarize(scale_score = mean(value))%>%
    tidyr::spread(key = scale, value = scale_score)%>%
    dplyr::arrange(unique_row_id)%>%
    dplyr::bind_cols(data)%>%
    dplyr::ungroup()%>%
    dplyr::select(colnames(data), dplyr::everything(), -unique_row_id)
}

# Example with bfi data in the psych package
library(tidyverse)
bfi<-psych::bfi
personality<-bfi%>%mutate(A1_r = 7-A1,
                          E1_r = 7-E1, E2_r=7-E2,
                          O2_r = 7-O2, O5_r=7-O5,
                          C4_r = 7-C4, C5_r = 7-C5)
cleaned<-tidysum(data = personality, drop=c(A1,E1, E2, O2, O5, C4, C5))

