#' Groundwater drought index
#' 
#' @description
#' `sgi`: generalized normal distribution (pelgno) used here. 
#' 
#' @examples man/examples/ex-sgi.R
#' @importFrom lmomco pargno
#' @export
sgi <- function(data, scale, kernel=list(type='rectangular',shift=0),
                distribution='GNorm', fit='ub-pwm', na.rm=FALSE,
                ref.start=NULL, ref.end=NULL, x=FALSE, params=NULL, ...){
  sol <- spei2(data, scale, kernel, distribution, fit, na.rm,
               ref.start, ref.end, x, params)
  sol$call <- match.call(expand.dots=FALSE)
  
  values <- sol$fitted %<>% as.numeric()
  values[is.na(data)] <- NA
  
  sol$fitted <- values
  sol$data <- data
  return(sol)
}

#' @importFrom zoo zoo
sgi_site <- function(df, sitename, scale=3, na.rm = FALSE){
  x <- zoo(df[[sitename]], df$date)
  r <- sgi(x,  scale=scale, na.rm = na.rm)
  r
}

#' @importFrom lubridate date
plot_sgi <- function(r){
  x     <- r$data
  t     <- date(x)
  value <- r$fitted %>% as.numeric()
  
  plot(t, x, type = "b", xlab = NULL, ylab = NULL); grid()
  par(new = TRUE)
  
  # browser()
  plot(t, value, type = "l", col = "red", xlab =NULL, ylab = NULL, axes = FALSE)
  abline(h = 0   , col = "black")
  abline(h = -0.8, col = "red", lty = 2)
  # value
}
