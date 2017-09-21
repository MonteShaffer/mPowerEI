#' Convert Units (from gravity to m/s^2)
#'
#' Constants must be loaded with:  \code{setup = loadSetup();}
#'
#' @param x numeric vector
#' @param from units to change from
#' @param to units to change to
#'
#' @return numeric vector of converted result
#' @export
#'
#' @examples
#' 
#' convertUnits(1); # 9.8
#' convertUnits(seq(-2,2,by=0.25)); 
#' 
#' 
convertUnits <- function(x,from="g",to="m/s^2")
{
  
  if(!is.vector(x)) { stop("'invalid value of 'x' - must be numeric (vector)"); }
  fromTo = paste(from,to,sep='');
  
  # switch doesn't work like "c syntax" so using less efficient if/then
  if(fromTo == "gm/s^2") { setup$g * x; }
  else if(fromTo == "m/s^2g") { x / setup$g; }
  #else if() {}
  else { stop("'missing conversion method'");}
}
