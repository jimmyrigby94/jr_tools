library(tidyverse)

# Consider a situation with four courses of action each relatively close in returning a given reward (i.e., job satisfaction; leader efficacy)
# How would one intelligently choose between those four decisions to maximize the objective?

r1<-function(){rnorm(1, 5)}
r2<-function(){rnorm(1, 4)}
r3<-function(){rnorm(1, 4.3)}
r4<-function(){rnorm(1, 4.7)}

# Start out by initializing the reward object
# reward_hist stores the reward attained after each course of action at each timepoint
# reward_sum stores a summary table of the rewards (effectively all that is necessisary to run the bandit dilema)
# reward_funs stores the reward functions (only applicable in simulation) 
initialize_reward_obj<-function(reward_funs){
  list(
    reward_hist=tibble(time = 0,
                         decision = numeric(1),
                         reward = numeric(1)),
    reward_sum=tibble(decision=1:length(reward_funs),
                       t_selected = numeric(1),
                       reward=numeric(1), 
                       se=numeric(1),
                      upper = reward+qt(.95, t_selected-1)*se),
    reward_funs=reward_funs
    
  )
}

# One could take several approaches
# Select the option that provides the maximum expected reward (greedy strategy)
# Select the option that provides the maximum expected reward, but explore alternatives with a probability of epsilon (greedy-epsilon strategy)
# Select the option that has the highest upper bound (Upper confidence bound selection)

# The function below implements the three strategies above
# It initializes the dilema, however, by randomly sampling the arms until each condition has atleast n_min conditions
# This is not necessisary - optimistic starting values can be selected to encourage exploration
# this strategy just gets a reasonable number of observations to estimate the expected reward
multi_arm_bandit<-function(reward_obj, n_min, upper_bound = FALSE, epsilon = .05){
  
  # identifying the number of arms and the time point for iteration
  n_arms<-length(reward_obj$reward_funs)
  time<-max(reward_obj$reward_hist$time)
  
  # randomly selecting arm if it is the first timepoitn or an arm has been selected less than n_min times
  # this results in an initial exploration phase and gives ballpark estimates of the mean reward and standard error
  if(time==0|any(reward_obj$reward_sum$t_selected<n_min)){
   choice<-sample(1:n_arms, size = 1)

  }else{
    
    # extracting the optimum policy based on upper confidence bound selection or traditional bandit dilemas
    if(upper_bound){
      opt_index<-which.max(reward_obj$reward_sum$upper)
    }else{
      opt_index<-which.max(reward_obj$reward_sum$reward)
    }
    
    max_choice<-reward_obj$reward_sum$decision[opt_index]
    
    # Explores with a probability of epsilon otherwise selects the optimal choice
    if(rbinom(1,1,epsilon)){
      choice<-sample((1:n_arms)[-which.max(reward_obj$reward_sum$reward)], size = 1)
    }else{
      choice<-max_choice
    }
  }

  # Observes the reward function given the current choice and updates the reward history table
    reward_obj$reward_hist <- reward_obj$reward_hist%>%
    add_row(time = time+1,
            decision = choice,
            reward = reward_obj$reward_funs[[choice]]())%>%
    filter(decision !=0)
  
  # Identifies choices that have yet to be selected (to be added to the summary table)
  missing_choice<-(1:n_arms)[! 1:n_arms %in% unique(reward_obj$reward_hist$decision)]
  
  # Summarises the reward_history table and appends choices that have yet to be selected
  # This is inefficient in practice, but is useful for illustrating when rewards get stuck
  reward_obj$reward_sum<-reward_obj$reward_hist%>%
    group_by(decision)%>%
    summarise(t_selected = n(),
              reward_m = mean(reward),
              se = sd(reward)/sqrt(t_selected),
              upper = reward_m+qt(.95, t_selected-1)*se
              )%>%
    rename(reward = reward_m)%>%
    bind_rows(tibble(decision=missing_choice,
                 t_selected = numeric(1),
                 reward=numeric(1), 
                 se=numeric(1),
                 upper = reward+qt(.95, t_selected-1)*se))%>%
    arrange(decision)
  
  reward_obj
    
}

# Lets illustrate the exploration and exploitation dillema using the reward functions defined above
# We will use a mix of the epsilon-greedy and upper confidence bound selection approaches, varying epsilon to increase exploration

n_iter<-100
trials<-100
eps_cond<-rep(c(.01, .05, .10), times = 2)
ub_cond<- rep(c(FALSE, TRUE), each = 3)

# Running the simulation for 100 times for 1000 time points 
m_list<-map2(eps_cond,ub_cond,
     function(x, y){
       
       map(1:n_iter,
           function(iter){
             reward_obj<-initialize_reward_obj(list(r1,r2,r3,r4))
             for(i in 1:trials){
               reward_obj<-multi_arm_bandit(reward_obj, n_min = 5, upper_bound = y, epsilon = x)
             }
             reward_obj
             
           }
           )
     }
     )

# Summarising the results into one table per sim condition
iter_hist<-map(m_list, 
    function(sim_cond){
      map_dfr(1:n_iter, 
          function(iter){
            sim_cond[[iter]]$reward_hist%>%
              mutate(
                i = iter, 
                avg_reward = cumsum(reward)/time,
                p_opt = cumsum(decision ==1)/time
                )
              
          }
      )
    }
    )

# Providing overall summary per sim condition
total_sum<-map_dfr(1:6,
                   function(index){
                     iter_hist[[index]]%>%
                       group_by(time)%>%
                       summarise(avg_reward = mean(avg_reward),
                                 p_opt = mean(p_opt))%>%
                       mutate(eps = eps_cond[index],
                              ub = ub_cond[index])
                   })

# Plotting expected reward per timepoint across sim conditions
# As epsilon increases (exploration increases) the upper confidnec bound methods and pure greedy-epsilon methods converge
total_sum%>%
  ggplot(aes(x = time, y = avg_reward, color = ub))+
  geom_line()+
  facet_wrap(~eps)

# Plotting expected optimal choice per timepoint across sim conditions
# As epsilon increases (exploration increases) the upper confidnec bound methods and pure greedy-epsilon methods converge
total_sum%>%
  ggplot(aes(x = time, y = p_opt, color = ub))+
  geom_line()+
  facet_wrap(~eps)

# Plotting each iteration of the model 
map(1:6, function(index){
  p1<-iter_hist[[index]]%>%
    ggplot(aes(x= time, y= avg_reward))+
    geom_smooth(method = "loess")+
    geom_line(aes(group = i), alpha = .1)
  
  p2<-iter_hist[[index]]%>%
    ggplot(aes(x= time, y= p_opt))+
    geom_smooth(method = "loess")+
    geom_line(aes(group = i), alpha = .1)
  
  list(p1, p2)
}
     )


efficient_bandits<-function(bandit_df, eps, funs, upper = FALSE){
  # Implementing the incremental estimation of the multi-armed bandit
  n_bandits<-nrow(bandit_df)
  
  if (!upper){
    # randomly selecting if this should be an off policy draw with probability = epsilon
    if(rbinom(1, 1, eps)){
      selected<-sample(1:n_bandits, size = 1)
    }else{
      selected<-which.max(bandit_df$reward)
    }
  } else{
    if(rbinom(1, 1, eps)){
      selected<-sample(1:n_bandits, size = 1)
    }else{
      selected<-which.max(bandit_df$upper)
    }
  }
  
  # Drawing from the selected function
  r<-funs[[selected]]()
  
  # updating the expected reward for the bandit
  t<-bandit_df$t[[selected]]+1
  q_tm1<-bandit_df$reward[[selected]]
  q_t<-q_tm1+(r-q_tm1)/t
  bandit_df$reward[[selected]]<-q_t
  
  # implementing online algorithm for sd
  # SSk = SSk-1 + (xk - Mk-1)*(xk - Mk)
  bandit_df$sd[[selected]]<-sqrt((bandit_df$sd[[selected]]^2*(t-2)+(r-q_tm1)*(r-q_t))/(t-1))
  
  # Updating the time times the reward was drawn
  bandit_df$t[[selected]]<-t
  
  bandit_df$upper[[selected]]<-bandit_df$reward[[selected]]+qt(.95, t-1)*bandit_df$sd[[selected]]/sqrt(t)
  
  bandit_df
}


efficient_bandits_loop<-function(bandit_storage, reward_funs, iterations, time){
  
  # Creating a list to store the simulation results
  out_list<-list()
  
  # outer loop for time within simulation
  for (m in 1:iterations){
    
    # initializing the inner loop storage for time point storage
    bdf_list<-list(bandit_storage)
    
    # inner loop for bandit trials within time
    for (i in 2:time){
      
      #implementing efficient bandits
      bdf_list[[i]]<-efficient_bandits(bdf_list[[i-1]], .05, reward_funs)
    }
    out_list[[m]]<-map_dfr(1:length(bdf_list), ~bdf_list[[.x]]%>%mutate(time = .x)) 
  }
  
  map_dfr(1:length(out_list), ~out_list[[.x]]%>%mutate(iter = .x)) 
}

# Initialize with optimistic starting values to encourage early exploration
bdf<-tibble(reward = c(6,6,6,6), 
            sd = c(2, 2, 2, 2),
            bandit = 1:4,
            t = c(5,5,5,5))%>%
  mutate(upper = reward+qt(.95, t-1)*sd/sqrt(t))

efficient_bandits_df<-efficient_bandits_loop(bandit_storage = bdf, 
                                             reward_funs = list(r1,r2,r3,r4),
                                             iterations = 2000, 
                                             time = 50)

efficient_bandits_df%>%
  ggplot(aes(x = t, y = reward, group = iter))+
  geom_line(alpha = .05)

