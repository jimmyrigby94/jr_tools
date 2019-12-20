setwd("C:\\Users\\Jimmy Rigby\\OneDrive - University Of Houston\\Projects and Reviews\\Automated Grading")

#####Automated Quiz Grading and Feedback#####
###Step 1: Set working Directory: Should Contain all submitted csvs documents
###Step 2: Run Autograde: Takes 1 argument which is a vector containing the answer key
          #Creates two new columns in the student csv's (the answer key and whether they got the item correct)
###Step 3: Run write_grades: Takes 1 argument which is the list of grades that you generated in autograde
          #Saves the newly generated data frames in a new "graded" folder in the parent directory
###Step 4: Run Summary grades: Takes 2 arguments which are the list of grades that you generated in autograde and the number of items on quiz
          #Creates a vector of grades with each person's total grades
###Step 5: Run Email grades: Takes 2 arguments: 1 the object created by summary grades and a data frame of email and file names. 


autograde<-function(key){
  
  file.name<-list.files(pattern = "*.csv")
  
  file.list<-lapply(file.name, FUN = function(x){
    read.csv(x, stringsAsFactors = FALSE)
  })
  
  names(file.list)<-file.name
  
  graded<-lapply(file.list,function(x){
    cbind(x, as.data.frame(key))
  })
  
  str_extract_all(x, pattern = "[:digit:]")
  graded<-lapply(graded, function(x){
    cbind(x, data.frame(correct = x$response==x$key))
  })
}

write_grades<-function(list){
 graded<-list
 wd<-getwd()
   if(!file.exists(paste(getwd(), "graded", sep ="\\"))){
    dir.create(paste(getwd(), "graded", sep ="\\"))
  }
  setwd(paste(getwd(), "graded", sep ="\\"))
  
  lapply(1:length(graded), function(i){
    write.csv(graded[i], file = names(graded)[i], row.names = FALSE)
  })
  setwd(wd)
}

summary_grades<-function(list, items){
  grades<-lapply(1:length(list), function(x){
    100*sum(list[[x]][,4], na.rm = TRUE)/items
  })

  data.frame(file = names(list), grades = unlist(grades))
}


test<-autograde(c("a","b","c","d","e"))

test

grades<-summary_grades(test,5)

grades

email_df<-data.frame(file = c("rigby.csv", "lewis.csv"), email=c("jimmyrigby94@gmail.com", "jimborigby@yahoo.com"), stringsAsFactors = FALSE)

mail_grades<-function(grades_data, email_data){
  
  full_email<-dplyr::left_join(grades_data, email_data)
  
  for(i in 1:nrow(full_email)){
  
send.mail("jimmyrigby94@gmail.com", 
          full_email[i,"email"], 
          subject = "Feedback:Quiz 3",
          body = paste("Hello, This is an automated feedback report for Quiz 3. You received a ", full_email[i,"grades"], "% on Quiz 3.", sep =""), 
          smtp = list(host.name = "smtp.gmail.com", 
                      port = 465,
                      user.name = "jimmyrigby94@gmail.com",
                      passwd = "shamu123",
                      ssl = TRUE),
          authenticate = TRUE,
          attach.files = paste(getwd(), "graded", full_email[i,"file"],sep ="\\"))
    }
}

mail_grades(grades_data = grades, email_data = email_df)



