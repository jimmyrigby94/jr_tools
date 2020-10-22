# plot the regions of rejection for a critical value
plot_crit_region<-function(p, two_tailed = TRUE, tail = "upper"){
  if(two_tailed){
    ggplot(data = NULL, aes(c(-4, 4)))+
      geom_area(stat = "function", fun = dnorm, fill =  "red", xlim = c(-4, qnorm(p/2)), alpha = .75)+
      geom_area(stat = "function", fun = dnorm, fill =  "blue",xlim = c(qnorm(p/2), -qnorm(p/2)), alpha = .75)+
      geom_area(stat = "function", fun = dnorm, fill =  "red", xlim = c(-qnorm(p/2), 4), alpha = .75)+
      labs(x = "Z-Score", y = "Density", title = "Standard Normal Distribution", subtitle = "Red = Critical Region")+
      theme(text = element_text(size = 30))
  } else{
    if (tail == "upper"){
      body<-c(-4, qnorm(p, lower.tail = FALSE))
      tail<-c(qnorm(p, lower.tail = FALSE), 4)
    } else {
      tail<-c(-4, qnorm(p, lower.tail = TRUE))
      body<-c(qnorm(p, lower.tail = TRUE), 4)
    }
    ggplot(data = NULL, aes(c(-4, 4)))+
      geom_area(stat = "function", fun = dnorm, fill =  "red", xlim = tail, alpha = .75)+
      geom_area(stat = "function", fun = dnorm, fill =  "blue",xlim = body, alpha = .75)+
      labs(x = "Z-Score", y = "Density", title = "Standard Normal Distribution", subtitle = "Red = Critical Region")+
      theme(text = element_text(size = 30))
  }
  
}