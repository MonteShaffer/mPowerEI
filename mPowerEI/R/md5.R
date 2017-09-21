#' MD5 Encryption
#' 
#' One-way encyption using md5 methodology from library digest::digest
#'
#' @param txt a text string or a vector of text strings
#'
#' @return a text string or a vector of text strings
#' @export
#'
#' @examples
#' 
#' md5('hello-again/#&0!_ugly-characters-break-R');
#' 
#' md5(c('hello','again'));
#' 
md5 <- function(txt)
{
  result = lapply(txt, FUN = function(x) { digest::digest(as.character(x), algo="md5", serialize=F) } );
  unlist(result);
}