plot_ly_surf<-function(obj = NULL, max.x = NULL, min.x = NULL, max.y = NULL, min.y = NULL, inc = NULL,  xlab=NULL, ylab=NULL, zlab=NULL, showscale = FALSE){
  
  #Copies the model generated in the resp_surf function for use here
  eq<-obj[["model"]]
  
  #Generates a 2xn dimension data frame with every combination of x and y
  d<-expand.grid(list(x = seq(min.x, max.x, by = inc),
                      y = seq(min.y, max.y, by = inc)))
  
  #creates quadratic and interaction terms for prediction
  d$x_sq<-d$x^2
  d$y_sq<-d$y^2
  d$int<-d$x*d$y
  
  #names the variables what you have them named in the model
  colnames(d)<-names(coef(eq)[2:6])
  
  #uses the function generated in resp_surf to predict values
  d$y<-predict(eq, d)
  
  #prepares a vector of x values for plotting
  x<-seq(min.x, max.x, by = inc)
  #Prepares a vector of y values for plotting
  y<-seq(min.y, max.y, by = inc)
  #spreads the predictions generated above into a matrix where rows correspond with x and columns correspond with y
  z <- matrix(d$y, nrow = length(x), ncol=length(y))
  
  if(is.null(xlab)){
    xlab<-names(eq$coefficients)[2]
  }
  if(is.null(ylab)){
    ylab<-names(eq$coefficients)[3]
  }
  if(is.null(zlab)){
    zlab<-colnames(eq$model)[1]
  }
  
  plotly::plot_ly(x = x, y = y, z = ~z, showscale = showscale)%>% 
    plotly::add_surface()%>%
    plotly::add_trace(x = x[x==y], y = y[x==y], z = diag(z), line = list(color = "black"),type = 'scatter3d', mode = 'lines', name = "Line of Congruence")%>%
    plotly::add_trace(x = x, y = y[length(y):1], z = z[row(z) == (ncol(z)-col(z)+1)], line = list(color = "black", dash = 'dash'),type = 'scatter3d', mode = 'lines', name = "Line of Incongruence")%>%
    plotly::layout(scene = list(xaxis = list(title = xlab), 
                                yaxis = list(title = ylab), 
                                zaxis = list(title = zlab)))%>%
    plotly::add_trace(x = x, 
                      y = obj$princ_axis$Estimate[1]+obj$princ_axis$Estimate[2]*x,
                      z = min(z),
                      line = list(color = "red"),type = 'scatter3d', mode = 'lines', name = "First Principal Axis")%>%
    plotly::add_trace(x = x, 
                      y = obj$princ_axis$Estimate[3]+obj$princ_axis$Estimate[4]*x,
                      z = min(z),
                      line = list(color = "red", dash = 'dash'),type = 'scatter3d', mode = 'lines', name = "Second Principal Axis")
}
