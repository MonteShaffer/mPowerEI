#' Compute gravity at a certain altitude on earth.
#' 
#' 
#' Constants must be loaded with:  \code{setup = loadSetup();}
#' 
#' @param h height in meters
#'
#' @return gravity on earth adjusted for altitude \code{h}
#' @export
#'
#' @examples
#' g10 = g_h(10);
#' g100 = g_h(100);
#' g1000 = g_h(1000);
g_h = function(h)
{
    setup$g * (( setup$r_e/( setup$r_e + h))^2);
}