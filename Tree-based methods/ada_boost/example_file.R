library(tidyverse)
library(decisionr)

set.seed(12345)


# Simming Data ------------------------------------------------------------

data<-data.frame(x1 = rnorm(n = 2000), 
                 x2 = rnorm(n = 2000))%>%
  mutate(y = x1*.3 + .2*x2 + rnorm(2000, sd = sqrt(.87)))

people<-sim_people(c(`(Intercept)` = 0, x1 = .3), n_people = 2000, n_blocks = 2000)
design<-tibble(BLOCK = rep(1:2000, each = 2),
               QES = 1, 
               alt = rep(c(0,1), times = 2000),
               `(Intercept)`= rep(1, times = 4000))

for(i in 1:4000){
  if(i%%2==1){
    design$x1[i]<-0
  }else{
    design$x1[i]<-rnorm(1)
  }
}

dec<-sim_dec(design, people, price = "x1")%>%
  filter(x1 != 0)

output_split<-split_rule(y = "y", x = c("x1", "x2"), data = data)

output_split$cost_data%>%
  ggplot(aes(x = split_value, y = cost, color = var))+
  geom_line()+
  theme(panel.grid = element_blank())+
  labs(x = "Split of X", 
       y = "Sum of Squared Error")

output_split<-split_rule(y = "decision", x = c("x1"), data = dec, cost_fun = entropy)

output_split$cost_data%>%
  ggplot(aes(x = split_value, y = cost, color = var))+
  geom_line()+
  theme(panel.grid = element_blank())+
  labs(x = "Split of X", 
       y = "Entropy")

# Running 10 iterations of adaboost
ada_boost_output<-ada_boost(y = "decision", x = c("x1"), data = dec, iter = 10)
 
# Simulating new data set
dec2<-sim_dec(design, people, price = "x1")%>%
  filter(x1 != 0)%>%
  ungroup()%>%
  select(id, x1, decision)


ada_pred<-ada_pred(dec2, ada_boost_output, "decision")

ada_pred%>%
  mutate(cost = observed!=(pred>.5))%>%
  summarise(sum(cost)/n())
