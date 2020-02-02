set.seed(1234)
library(tidyverse)
library(car)
library(knitr)
library(kableExtra)

n<-100

# Setting populatoin Parameters
# B0 = 15, B1 = .3, B2 = .6, sd = .74
b0<-15; b1<-.3; b2<-.6; b3 <- .15; rmse<-sqrt(1-sum(c(b1, b2, b3)^2))

# Generating Data Frame
my_dat<-data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))%>%
  mutate(x1 = x1-mean(x1),
         x2 = x2-mean(x2),
          y = b0 +b1 * x1+b2 * x2 + b3*x1*x2+ rnorm(n, mean = 0, sd = rmse))

model1<-lm(y~x1, data = my_dat)
model2<-lm(y~x2, data = my_dat)
model3<-lm(y~x1+x2, data = my_dat)
model4<-lm(y~x1+x2+x1*x2, data = my_dat)


tidy_anova<-function(x, type){
  Anova(x, type = type)%>%
    broom::tidy(.)
}

add_ms<-function(x){
  x%>%
    mutate(ms = if_else(term != "Total", sumsq/df, as.numeric(NA)))
}

add_ssreg<-function(x, m){
  
  ss_resid<-m%>%
    Anova(type = 3)%>%
    broom::tidy()%>%
    filter(term == "Residuals")%>%
    select(-c(statistic, p.value))
  
  m_null<-update(m, ~ 1)
  
  ss_tot<-Anova(m_null, type = 3)%>%
    broom::tidy()%>%
    filter(term == "Residuals")%>%
    mutate(term = "Total")%>%
    select(-c(statistic, p.value))
  
  ss_reg<-data.frame(term = "Regression",
                     sumsq = pull(ss_tot, sumsq)[1]-pull(ss_resid, sumsq)[1],
                     df = pull(ss_tot, df)[1]-pull(ss_resid, df)[1])%>%
    mutate(statistic = (sumsq/df)/(pull(ss_resid, sumsq)[1]/pull(ss_resid, df)[1]),
           p.value = 1-pf(statistic, df, pull(ss_resid, df)[1]))
  
  bind_rows(x, ss_reg)
}

add_sstot<-function(x, m){
  
  m<-update(m, ~ 1)
  
  ss_tot<-Anova(m, type = 3)%>%
    broom::tidy()%>%
    filter(term == "Residuals")%>%
    mutate(term = "Total")%>%
    select(-c(statistic, p.value))
  
  bind_rows(x, ss_tot)
}

order_df<-function(x, models){
  col_order<-paste(rep(1:models, 
                       each = 5), 
                   c("sumsq", "df", "ms", "statistic", "p.value"),
                   sep = "_")
  
  bottom<-c("Regression", "Residuals", "Total")
  
  row_order<-c(which(!x$term %in% c("(Intercept)", bottom) & !str_detect(x$term, ":")),
               which(str_detect(x$term, ":")),
               which(x$term=="Regression"),
               which(x$term=="Residuals"),
               which(x$term=="Total"))
  
  x[row_order, c("term", col_order)]
}



anova_comp_table<-function(..., type = 3, caption = "Comparison of ANOVA Tables", model_names = NULL, na_opt = '-'){
  model_list<-list(...)
  
  n_models<-length(model_list)
  
  if(is.null(model_names)){
    model_names<-paste("Model ", 1:n_models)
  }
  
  model_df<-model_list%>%
    # Tidying ANOVA Tables
    map(~tidy_anova(., type = type))%>%
    map2(.x = ., .y = model_list, ~add_ssreg(.x, .y))%>%
    map2(.x = ., .y = model_list, ~add_sstot(.x, .y))%>%
    map(., add_ms)
  
  model_df<-model_df%>%
    # Combining Different models into 1 dataframe
    imap(~mutate(.x, model = .y))%>%
    bind_rows()%>%
    # Formatting column names for different models so they can be blocked
    gather(key = column_name, value = values, -model, -term)%>%
    mutate(column_name = paste(model, column_name, sep = "_"))%>%
    select(-model)%>%
    spread(key = column_name, value = values)%>%
    # Arranging the dataframe appropriately
    order_df(models = n_models)
  
  
  
  
  table_windows<-rep(5, times = n_models)
  names(table_windows)<-model_names
  
  options(knitr.kable.NA = na_opt)
  
  k<- kable(model_df, 
            format = "html", 
            digits = 2, 
            col.names = c("", rep(c("SS", "DF", "MS", "F", "p"), 
                                  times = n_models)), 
            align = paste(c("l", rep("r", times = 5*n_models)), collapse = ""),
            caption = caption)%>%
    add_header_above(c(" " = 1, 
                       table_windows))%>%
    pack_rows("Partial Sums of Squares", 
              1, 
              nrow(model_df)-3)%>%
    pack_rows("Overall Sums of Squares", 
              nrow(model_df)-2, 
              nrow(model_df))%>%
    kable_styling(bootstrap_options = "responsive")
  
  options(knitr.kable.NA = NA)
  
  k
}

anova_comp_table(model1, model2, type = 3)

anova_comp_table(model1,  model2, model3, model4, type = 3)

anova_comp_table(model1,  model2, model3, model4, model_names = c("X1 Only", "X2 Only", "Both", "Interaction"))
