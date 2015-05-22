to.pdf <- function(expr, filename, ..., verbose=TRUE) {
  if(!file.exists(dirname(filename)))
    dir.create(dirname(filename), recursive=TRUE)
  if ( verbose )
    cat(sprintf("Creating %s\n", filename))
  pdf(filename, ...)
  on.exit(dev.off())
  eval.parent(substitute(expr))
}


# Bootstrap A-Ci curve!
bootaci <- function(d, nboot=500){
  
  
  p <- list()
  for(i in 1:nboot){
    
    ii <- sample(1:nrow(d), replace=TRUE)
    dat <- d[ii,]
    
    p[[i]] <- try(fitaci(dat))
    message(i)
  }
  
  return(p)
}

getci <- function(b){
  
  cis <- seq(100, 500, length=100)
  
  phot <- matrix(ncol=100,nrow=length(b))
  for(i in 1:length(b)){
    phot[i,] <- b[[i]]$Photosyn(Ci=cis)$ALEAF
  }
  
  lcl <- apply(phot,2,quantile, 0.025)
  ucl <- apply(phot,2,quantile, 0.975)
  
  return(data.frame(Ci=cis, lcl=lcl, ucl=ucl))
}

alpha <- function (colour, alpha = NA) {
  col <- col2rgb(colour, TRUE)/255
  if (length(colour) != length(alpha)) {
    if (length(colour) > 1 && length(alpha) > 1) {
      stop("Only one of colour and alpha can be vectorised")
    }
    if (length(colour) > 1) {
      alpha <- rep(alpha, length.out = length(colour))
    }
    else if (length(alpha) > 1) {
      col <- col[, rep(1, length(alpha)), drop = FALSE]
    }
  }
  alpha[is.na(alpha)] <- col[4, ][is.na(alpha)]
  new_col <- rgb(col[1, ], col[2, ], col[3, ], alpha)
  new_col[is.na(colour)] <- NA
  new_col
}


addaciline <- function(fit, ...){
  
  p <- coef(fit)
  cis <- seq(100, 500, length=101)
  asim <- fit$Photosyn(Ci=cis)
  with(asim, lines(Ci, ALEAF, ...))
  
}

addpoly <- function(x,y1,y2,col=alpha("lightgrey",0.8),...){
  ii <- order(x)
  y1 <- y1[ii]
  y2 <- y2[ii]
  x <- x[ii]
  polygon(c(x,rev(x)), c(y1, rev(y2)), col=col, border=NA,...)
}

se <- function(x)sd(x)/sqrt(length(x))


#' Adds error bars to a plot
#' 
#' @description Yet another function that adds error bars. The user must specify the length of the error bars.
#' @param x The x coordinates of start of the error bar
#' @param y The y coordinates of start of the error bar
#' @param SE The length of the error bar
#' @param direction One of 'up', 'down', 'right', 'left', 'updown' or 'rightleft'.
#' @param barlen The length of the cross-bar at the end of the error bar.
#' @param \ldots Additional parameters passed to \code{\link{arrows}}, such as the colour (\code{col}).
#' #' @details Simple wrapper for \code{\link{arrows}}, where \code{angle=90} and \code{code=3}. The \code{barlen} argument corresponds to \code{length} in \code{arrows}.
#' @examples
#' # A simple example. Also note that we can specify the colour of the error bars, or other parameters
#' # that arrows() recognizes.
#' x <- rnorm(20)
#' y <- x + rnorm(20)
#' se <- runif(20, 0.2,0.4)
#' plot(x,y,pch=21,bg="white",panel.first=adderrorbars(x,y,se,direction="updown", col="darkgrey"))
#' @export
adderrorbars <- function(x,y,SE,direction,barlen=0.04,...){
  
  if("up" %in% direction)
    arrows(x0=x, x1=x, y0=y, y1=y+SE, code=3, angle=90, length=barlen,...)
  if("down" %in% direction) 
    arrows(x0=x, x1=x, y0=y, y1=y-SE, code=3, angle=90, length=barlen,...)
  if("left" %in% direction) 
    arrows(x0=x, x1=x-SE, y0=y, y1=y, code=3, angle=90, length=barlen,...)
  if("right" %in% direction)
    arrows(x0=x, x1=x+SE, y0=y, y1=y, code=3, angle=90, length=barlen,...)  
  
}

predline <- function(fit, from=NULL, to=NULL, poly=TRUE, ...){
  
  if(is.null(from))from <- min(fit$model[,2], na.rm=TRUE)
  if(is.null(to))to <- max(fit$model[,2], na.rm=TRUE)
  
  newdat <- data.frame(X = seq(from,to, length=101))
  
  nm <- names(coef(fit))
  names(newdat)[1] <- nm[length(nm)]
  
  if(poly){
    pred <- as.data.frame(predict(fit, newdat, se.fit=TRUE, interval="confidence")$fit)
    addpoly(newdat[[1]], pred$lwr, pred$upr)
  }
  ablinepiece(fit, from=from, to=to, ...)
  
}


#'@title Add a line to a plot
#'@description As \code{abline}, but with \code{from} and \code{to} arguments. 
#'If a fitted linear regression model is used as asn argument, it uses the min and max values of the data used to fit the model.
#'@param a Intercept (optional)
#'@param b Slope (optional)
#'@param reg A fitted linear regression model (output of \code{\link{lm}}).
#'@param from Draw from this X value
#'@param to Draw to this x value
#'@param \dots Further parameters passed to \code{\link{segments}}
#'@export
ablinepiece <- function(a=NULL,b=NULL,reg=NULL,from=NULL,to=NULL,...){
  
  # Borrowed from abline
  if (!is.null(reg)) a <- reg
  
  if (!is.null(a) && is.list(a)) {
    temp <- as.vector(coefficients(a))
    from <- min(a$model[,2], na.rm=TRUE)
    to <- max(a$model[,2], na.rm=TRUE)
    
    if (length(temp) == 1) {
      a <- 0
      b <- temp
    }
    else {
      a <- temp[1]
      b <- temp[2]
    }
  }
  
  segments(x0=from,x1=to,
           y0=a+from*b,y1=a+to*b,...)
  
}


alpha <- function (colour, alpha = NA) {
  col <- col2rgb(colour, TRUE)/255
  if (length(colour) != length(alpha)) {
    if (length(colour) > 1 && length(alpha) > 1) {
      stop("Only one of colour and alpha can be vectorised")
    }
    if (length(colour) > 1) {
      alpha <- rep(alpha, length.out = length(colour))
    }
    else if (length(alpha) > 1) {
      col <- col[, rep(1, length(alpha)), drop = FALSE]
    }
  }
  alpha[is.na(alpha)] <- col[4, ][is.na(alpha)]
  new_col <- rgb(col[1, ], col[2, ], col[3, ], alpha)
  new_col[is.na(colour)] <- NA
  new_col
}


