library(tidyverse)

######################
###### Helpers #######
######################
source("Helpers/t_test_args.R")
source("Helpers/t_test_question.R")
source("Helpers/t_test_quiz.R")
source("Helpers/t_test_solutions.R")
source("Helpers/write_markdown.R")



####################################
### Item Generation Process ########
####################################

#####################
### 1 Define Stem ###
#####################

# Stems contain ids for chapter, topic, and subtopic
# Stem describes the question context
# dist describes the distrbutions of samples in formula form
# question describes the question of interest
# braces act as tags for args and are parse appropriately below

stem<-tibble(chapter = "sampling_dist",
             topic = "hypothesis_testing", 
             subtopic = "t_test",
             stem = "You are testing for mean differences in {var} between samples of {sample1} and {sample2}. You believe that the mean of {sample1} is {directional} than the mean of {sample2}. {equal_var} that both samples have equal variances.",
             dist = "$${sample1}:  \\bar x = {xbar1}; s = {sd1}; n = {n1}$$ $${sample2}:\\ \\bar x = {xbar2}; s = {sd2}; n = {n2}$$",
             question = "Compute the {ask}."
)

#################
### 2 Args ######
#################

## Define arguments for to populate the stem 
## t-test takes a few arguments 
## Allows for sample specific values to be selected
## Arguments allow for example specific reasonable values (i.e. a reasonable mean and varaince distribution)

# users can iterate over tibble of args defining outcome vars, labels, and ranges of possible values
possible_vals<-tibble(
  var = c("height", "interpersonal skills", "hay fever severity"),
       s1_name = c("men", "psychologists", "dog owners"),
       s2_name = c("women", "engineers", "cat owners"), 
       s1_m_l = c(1.60, 2, 2),
       s1_m_u = c(1.80, 5, 5),
       s2_m_l = c(1.55, 2, 2),
       s2_m_u = c(1.70, 5, 5),
       s1_sd_l = c(.05, .15, .15),
       s1_sd_u = c(.15, .30, .30),
       s2_sd_l = c(.05, .15, .15),
       s2_sd_u = c(.15, .30, .30)
       )


args<- pmap_dfr(possible_vals, t_test_args)


#######################
### 3 Generate Quiz ###
#######################
## based on the item stem and arguments, creates a quiz of arbitrary length
## returns the questions, arguments, and solutions
quiz<-t_test_quiz(args, stem, number_items = 22, directional == "greater than" | directional == "less than", var == "height")

write_rds(quiz, "my_t_test.rds")

########################
### 4 Create Markdown ##
########################
markdown<-write_markdown(quiz = "my_t_test.rds", setup = "library(tidyverse)")

write_file(markdown, path = "Example/my_t_test.rmd")
