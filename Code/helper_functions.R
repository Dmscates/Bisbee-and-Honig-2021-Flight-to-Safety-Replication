# QQ plot to evaluate balance
qqprep <- function (x, discrete.cutoff, which.subclass = NULL, numdraws = 5000, 
                    interactive = T, which.xs = NULL, ...) {
  X <- x$X
  varnames <- colnames(X)
  for (var in varnames) {
    if (is.factor(X[, var])) {
      tempX <- X[, !colnames(X) %in% c(var)]
      form <- formula(substitute(~dummy - 1, list(dummy = as.name(var))))
      X <- cbind(tempX, model.matrix(form, X))
    }
  }
  covariates <- X
  if (!is.null(which.xs)) {
    if (sum(which.xs %in% dimnames(covariates)[[2]]) != length(which.xs)) {
      stop("which.xs is incorrectly specified")
    }
    covariates <- covariates[, which.xs, drop = F]
  }
  treat <- x$treat
  matched <- x$weights != 0
  ratio <- x$call$ratio
  if (is.null(ratio)) {
    ratio <- 1
  }
  if (identical(x$call$method, "full") | (ratio != 1)) {
    t.plot <- sample(names(treat)[treat == 1], numdraws/2, 
                     replace = TRUE, prob = x$weights[treat == 1])
    c.plot <- sample(names(treat)[treat == 0], numdraws/2, 
                     replace = TRUE, prob = x$weights[treat == 0])
    m.covariates <- x$X[c(t.plot, c.plot), ]
    m.treat <- x$treat[c(t.plot, c.plot)]
  }
  else {
    m.covariates <- covariates[matched, , drop = F]
    m.treat <- treat[matched]
  }
  if (!is.null(which.subclass)) {
    subclass <- x$subclass
    sub.index <- subclass == which.subclass & !is.na(subclass)
    sub.covariates <- covariates[sub.index, , drop = F]
    sub.treat <- treat[sub.index]
    sub.matched <- matched[sub.index]
    m.covariates <- sub.covariates[sub.matched, , drop = F]
    m.treat <- sub.treat[sub.matched]
  }
  nn <- dimnames(covariates)[[2]]
  nc <- length(nn)
  covariates <- data.matrix(covariates)
  toplot <- NULL
  for (i in 1:nc) {
    xi <- covariates[, i]
    m.xi <- m.covariates[, i]
    rr <- range(xi)
    
    
    eqqplot <- function(x,y) {
      sx <- sort(x)
      sy <- sort(y)
      lenx <- length(sx)
      leny <- length(sy)
      if (leny < lenx) 
        sx <- approx(1:lenx, sx, n = leny, method = "constant")$y
      if (leny > lenx) 
        sy <- approx(1:leny, sy, n = lenx, method = "constant")$y
      return(list(x = sx,y = sy))
    }
    toplot <- bind_rows(toplot,
                        bind_rows(data.frame(eqqplot(x = xi[treat == 0],y = xi[treat == 1]),type = "Raw",cov = nn[i],rrlb = rr[1],rrub = rr[2]),
                                  data.frame(eqqplot(x = m.xi[m.treat == 0],y = m.xi[m.treat == 1]),type = "Matched",cov = nn[i],rrlb = rr[1],rrub = rr[2])))
  }
  return(list(toplot = toplot))
}


# Ineration plot
interaction_plot_continuous <- function(model, effect = "", moderator = "", varcov="default", minimum="min", maximum="max",colr = "grey",
                                        incr="default", num_points = 10, conf=.95, mean=FALSE, median=FALSE, alph=80, rugplot=T, 
                                        histogram=F, title="Marginal effects plot", xlabel="Value of moderator", 
                                        ylabel="Estimated marginal coefficient",pointsplot = F,plot = F,show_est = F) {
  # Extract Variance Covariance matrix
  if (varcov == "default"){
    covMat = vcov(model)
  }else{
    covMat = varcov
  }
  
  # Extract the data frame of the model
  mod_frame = model.frame(model)
  
  # Get coefficients of variables
  if(effect == "") {
    int.string <- rownames(summary(model)$coefficients)[grepl(":",rownames(summary(model)$coefficients))]
    effect <- substr(int.string,1,regexpr(":",int.string)[1]-1)
  }
  if(moderator == "") {
    int.string <- rownames(summary(model)$coefficients)[grepl(":",rownames(summary(model)$coefficients))]
    moderator <- substr(int.string,regexpr(":",int.string)[1]+1,nchar(int.string))
  }
  interaction <- paste(effect,":",moderator,sep="")
  beta_1 = summary(model)$coefficients[effect,1]
  beta_3 = summary(model)$coefficients[interaction,1]
  
  # Set range of the moderator variable
  # Minimum
  if (minimum == "min"){
    min_val = min(mod_frame[[moderator]])
  }else{
    min_val = minimum
  }
  # Maximum
  if (maximum == "max"){
    max_val = max(mod_frame[[moderator]])
  }else{
    max_val = maximum
  }
  
  # Check if minimum smaller than maximum
  if (min_val > max_val){
    stop("Error: Minimum moderator value greater than maximum value.")
  }
  
  # Determine intervals between values of the moderator
  if (incr == "default"){
    increment = (max_val - min_val)/(num_points - 1)
  }else{
    increment = incr
  }
  
  # Create list of moderator values at which marginal effect is evaluated
  x_2 <- seq(from=min_val, to=max_val, by=increment)
  
  # Compute marginal effects
  delta_1 = beta_1 + beta_3*x_2
  
  # Compute variances
  var_1 = covMat[effect,effect] + (x_2^2)*covMat[interaction, interaction] + 2*x_2*covMat[effect, interaction]
  
  # Standard errors
  se_1 = sqrt(var_1)
  
  # Upper and lower confidence bounds
  z_score = qnorm(1 - ((1 - conf)/2))
  upper_bound = sapply(1:length(z_score), function(x) delta_1 + z_score[x]*se_1)
  lower_bound = sapply(1:length(z_score), function(x) delta_1 - z_score[x]*se_1)
  
  # Determine the bounds of the graphing area
  max_y = max(upper_bound)
  min_y = min(lower_bound)
  
  # Make the histogram color
  hist_col = colr
  
  stars <- ifelse(abs(summary(model)$coefficients[interaction,3]) >2.6,"***",
                  ifelse(abs(summary(model)$coefficients[interaction,3]) > 1.96,"**",
                         ifelse(abs(summary(model)$coefficients[interaction,3]) > 1.65,"*","")))
  est <- paste("Interaction: ",round(summary(model)$coefficients[interaction,1],3),stars," (",
               round(summary(model)$coefficients[interaction,2],3),")",sep="")
  # Initialize plotting window
  if(plot) {
    plot(x=c(), y=c(), ylim=c(min_y, max_y), xlim=c(min_val, max_val), 
         xlab=xlabel, ylab=ylabel, main=title)
    
    # Plot estimated effects
    if(!pointsplot) {
      lines(y=delta_1, x=x_2,col = colr)
      for(i in ncol(upper_bound):1) {
        polygon(c(x_2,rev(x_2)),c(upper_bound[,i],rev(lower_bound[,i])),border = NA,col = colr)
      }
    }else{
      points(y = delta_1,x = x_2,col = colr,pch = 19)
      for(i in ncol(upper_bound):1) {
        segments(x_2,upper_bound[,i],x_2,lower_bound[,i],col = colr,lwd = i)
      }
    }
    # Add a dashed horizontal line for zero
    abline(h=0, lty=3)
    
    # Add a vertical line at the mean
    if (mean){
      abline(v = mean(mod_frame[[moderator]]), lty=2, col="red")
    }
    
    # Add a vertical line at the median
    if (median){
      abline(v = median(mod_frame[[moderator]]), lty=3, col="blue")
    }
    
    # Add Rug plot
    if (rugplot){
      rug(mod_frame[[moderator]])
    }
    
    if (show_est) {
      text(par('usr')[ 2 ], par('usr')[ 4 ],adj=c(1.05,1.2),
           labels = est)
      
    }
    #Add Histogram (Histogram only plots when minimum and maximum are the min/max of the moderator)
    if (histogram & minimum=="min" & maximum=="max"){
      par(new=T)
      hist(mod_frame[[moderator]], axes=F, xlab="", ylab="",main="", border=hist_col, col=hist_col)
    } 
  }
  return(list(delta_1 = delta_1,x_2 = x_2,ub = upper_bound,lb = lower_bound,inc = increment,est = est))
}
