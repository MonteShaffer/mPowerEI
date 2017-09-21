#' Numerical Integration (x,y) \code{integrateMe}
#'
#' @param x a numeric vector representing x-axis
#' @param y a numeric vector representing y-axis, must be of same length as x
#' @param method a string representing the numerical integration approach 'trapezoid'
#'
#' @return list with "area", "dydx" cumulative integral from x[1] to current x[i].
#' @export
#'
#' @examples
#' # Calculate the area under the sine curve from 0 to 2*pi:
#' n <- 202
#' x <- seq(0, 2*pi, len = n)
#' y <- sin(x)
#' ans <- integrateMe(x, y); #=> 0
#' str(ans); 
#' plot(x,y,type="l",col="black",xlab="",ylab="",xlim=c(0,2*pi),ylim=c(-1,2));
#' abline(h=0)
#' par(new=T);
#' plot(x,ans$dydx, col="green",xlab="",ylab="",xlim=c(0,2*pi),ylim=c(-1,2));



integrateMe <- function(x, y, method = "trapezoid")
{
    area = pracma::trapz(x,y);
    cum = pracma::cumtrapz(x,y);
    
    list(area=area,dydx=as.vector(cum));
}

