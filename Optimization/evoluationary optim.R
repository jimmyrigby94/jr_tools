genetic_optim<-function(fun, pop_size, noise, maxit, eps = .001, lb=NULL, ub=NULL, ls=NULL, us=NULL, save_all = FALSE){
  
  # boundaries for uniform distribution sampling (each are length 1 because of NULL)
  init_lb<-c(lb, ls)
  init_ub<-c(ub, us)
  
  # Initializing data storage objects
  out<- data.frame(generation = vector(mode = "numeric", length=1),
                   best_child = vector(mode = "numeric", length=1),
                   objective =  vector(mode = "numeric", length=1))
  
  i<-1
  dif<-eps+1
  # Everything is set up in matrix format with 2 columns n/2 rows
  half<-pop_size/2
  
  # Initializing data storage matrix
  child_mat<-mate_fitness_matrix<-child_fit_matrix<-mate_matrix<-matrix(nrow = half, ncol = 2)
  
  
  # helper function for mean child
  mean_child<-function(x){
    mean(c(x))+rnorm(1, 0, sd = noise)
  }
  


# Initialize Population ---------------------------------------------------

  # sampling the initial population based on boundaries
  pop<-runif(pop_size, init_lb, init_ub)


# Evalutate Fitness -------------------------------------------------------

  # Evaluating sample's fitness
  fitness<- purrr::map_dbl(pop, ~fun(.))
  

  
  if(save_all){
    verbose_out<-data.frame(generation = 0, 
                            value = pop, 
                            fitness = fitness,
                            relationship = "Origin of Life",
                            status = "Alive",
                            step = "first of species")
  }
  
  # Starting loop
 while (i<maxit & (i<40|dif>eps)) {
   
   cat("\r", i)


# Identify Mates ----------------------------------------------------------

   # Evaluating the sample's relative fitness to mate
   # used to conducted weighted sample of the population
   p_mate<-fitness/sum(fitness)
   
   # Selecting observations to mate given fitness
   # index selects both focal value and fitness
   mate_index1<-sample(1:pop_size, half, replace = TRUE, prob = p_mate)
   mate_index2<-sample(1:pop_size, half, replace = TRUE, prob = p_mate)
   
   # populating matrices Rows are associated with mating pairs
   mate_matrix[,1]<-pop[mate_index1]
   mate_matrix[,2]<-pop[mate_index2]
   
   mate_fitness_matrix[,1]<-fitness[mate_index1]
   mate_fitness_matrix[,2]<-fitness[mate_index2]
  

# Reproduce (with genetic variation) --------------------------------------

   # first child is average of mates with some noise
   child_mat[,1]<- apply(mate_matrix, 1, mean_child)
   
   # second child is average weighted by fitness
   mate_fit<-mate_fitness_matrix[,1]/(rowSums(mate_fitness_matrix)/2)
   
   child_mat[,2]<- mate_fit*mate_matrix[,1]+(1-mate_fit)*mate_matrix[,2]+rnorm(half, mean = 0, sd = noise)
   
   if(!is.null(lb)) child_mat[child_mat<lb]<-lb
   if(!is.null(ub)) child_mat[child_mat>ub]<-ub
   

# Evaluate Children -------------------------------------------------------

   child_fit_matrix[,1]<-purrr::map_dbl(child_mat[,1], ~fun(.))
   child_fit_matrix[,2]<-purrr::map_dbl(child_mat[,2], ~fun(.))
   
   child_fit_matrix<-child_fit_matrix
   
   if(save_all){
     verbose_out<-rbind(verbose_out, 
                        data.frame(generation = i, 
                                   value = pop, 
                                   fitness = fitness, 
                                   relationship = "Parent",
                                   status = "Alive",
                                   step = "mating"),
                        data.frame(generation = i, 
                                                  value = as.vector(child_mat), 
                                                  fitness = as.vector(child_fit_matrix),
                                                  relationship = "Child",
                                                  status = "Alive",
                                                  step = "mating")
                        )
   }
   

# Tournament (Only the Strong Survive) ------------------------------------
   
   # Identifying best child out of pair of children
   best_child_index<-child_fit_matrix[,1]==apply(child_fit_matrix, 1, max)
   
   # Storing best child in population (killing the weak child)
   pop<-c(child_mat[best_child_index, 1], child_mat[!best_child_index, 2])
   fitness<-c(child_fit_matrix[best_child_index,1], child_fit_matrix[!best_child_index,2])
   
   # Identifying the best mate in the population
   best_mate_index<-mate_fitness_matrix[,1]==apply(mate_fitness_matrix, 1, max)
   
   # Storing the best parent in the population (killing the weak parent)
   pop<-c(pop, mate_matrix[best_mate_index, 1], mate_matrix[!best_mate_index, 2])
   fitness<-c(fitness, mate_fitness_matrix[best_mate_index,1], mate_fitness_matrix[!best_mate_index,2])
   
   if(save_all){
     verbose_out<-rbind(verbose_out, 
                                     data.frame(generation = i, 
                                                value = pop, 
                                                relationship = rep(c("Parent", "Child"), each = half),
                                                fitness = fitness, 
                                                status = "Alive",
                                                step = "tournament"),
                        data.frame(generation = i,
                                   value = c(child_mat[!best_child_index, 1], child_mat[best_child_index, 2],  mate_matrix[!best_mate_index, 1], mate_matrix[best_mate_index, 2]),
                                   relationship = rep(c("Parent", "Child"), each = half),
                                   fitness = c(child_fit_matrix[!best_child_index,1], child_fit_matrix[best_child_index,2], mate_fitness_matrix[!best_mate_index,1], mate_fitness_matrix[best_mate_index,2]),
                                   status = "Dead", 
                                   step = "tournament")
     )
   }
   
   max_fit<-max(fitness)
   
   # Storing all the output
   out[i, 1]<-i
   out[i, 2]<-pop[fitness==max_fit][[1]]
   out[i, 3]<-max_fit
   
   # Evaluating MAD of the laste 10 generations
   if(i>10) dif<- mad(out[,3][(i-9):i])
   
   i<-i+1
 }
  
  best_index<-which.min(out$objective)
  rs<-list(solution = out$best_child[best_index],
       value = out$objective[best_index],
       iteration_history = out)
  
  if(save_all){
    rs[["verbose_out"]]<-verbose_out
  }
  rs
}

set.seed(12345)
object<-function(x) -x^4+2*x^2+10

out<-genetic_optim(object, 20, .5, 1000, lb = -2, ub = 2, eps = .01, save_all = TRUE)


library(tidyverse)

out$verbose_out$time<-out$verbose_out%>%dplyr::group_by(generation, step)%>%dplyr::group_indices()


anim<-ggplot(out$verbose_out, aes(value, fitness))+
  stat_function(fun=object)+
  geom_jitter(mapping = aes(color = status, shape = relationship), width = .03, alpha = .95)+
  guides(color=guide_legend(title = ""),
         shape = guide_legend(title = ""))+
  labs(x = "Proposed Value",
       y = "Fitness",
       title="Illustration of Evolutionary Optimization",
       subtitle = "Generation: {(frame- 1)%/%3}")+
  scale_color_manual(values = c("green", "red"))+
  theme(panel.grid = element_blank())+
  transition_states(time)+
  enter_fade()+
  exit_fade()

animate(anim, duration = 30, fps = 6)

anim_save(".././Git Repository/jr_tools/Optimization/evolution_optimization.gif")
