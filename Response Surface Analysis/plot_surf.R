plot_surf<-function(obj = NULL, max.x = NULL, min.x = NULL, max.y = NULL, min.y = NULL, inc = NULL, phi = 45, theta = 315, llabels =FALSE, xlab=NULL, ylab=NULL, zlab=NULL, ...){
  if((theta>360|theta<270) & !theta == 0){
    warning("Perspective may not align with the line of congruence. Consider retaining a theta between 360 and 270")
  }
  
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

  # #Creates a wire plot
  # persp(x = x, y = y, z = z, phi = phi, theta = theta, shade = .5, 
  #       xlab = xlab, ylab = ylab, zlab = zlab, ...)-> res
  # round(res, 3)
  # #Plots the line of congruence
  # lines(trans3d(x = x, y = x, z = min(z), pmat = res), col = "black", lty = 2)
  # 
  # #grid on (x,y) plane
  # for (ix in seq(min.x, max.x, by = inc)) lines (trans3d(x = ix, y = seq(min.y, max.y, by = inc), z = min(z), pmat = res), col = "black", lty ="solid")
  # for (iy in seq(min.y, max.y, by = inc)) lines (trans3d(x = seq(min.y, max.y, by = inc), y = iy, z = min(z), pmat = res), col = "black", lty ="solid")
  # 
  # #plots the line of incongruence
  # lines(trans3d(x = x, y = -x, z = min(z), pmat = res), col = "black", lty = 4)
  # #Labels for the line of congruence/incongruence
  # if(llabels ==TRUE){
  #   text(trans3d(x =min(x)+1.75, y = min(y)+1.5, z = min(z), pmat = res), labels = "Line of Congruence", cex = .75, srt =70)
  #   text(trans3d(x =min(x)+1.5, y = max(y)-1.75, z = min(z), pmat = res), labels = "Line of Incongruence", cex = .75, srt =350)
  # }
  # legend(x= "bottomleft", legend = c("Line of Congruence", "Line of Incongruence"), xpd = TRUE,cex = .75,  bty = "n", lty = c(2,4))

  }
