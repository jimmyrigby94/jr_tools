# plot a normal distribution with arbetrary cut score z
plot_norm<-function(z){
  ggplot(data = NULL, aes(c(-4, 4)))+
    geom_area(stat = "function", fun = dnorm, fill = ifelse(z < 0, "red", "blue"), xlim = c(-4, z), alpha = .75)+
    geom_area(stat = "function", fun = dnorm, fill = ifelse(z < 0, "blue", "red"),, xlim = c(z, 4), alpha = .75)+
    labs(x = "Z-Score", y = "Density", title = "Standard Normal Distribution", subtitle = "Red = Tail; Blue = Body")+
    theme(text = element_text(size = 30))
}