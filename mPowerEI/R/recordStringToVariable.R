#' Convert recordId to a variable that can be used in R
#'
#' @param r a record id, string in form '224f8562-c923-40fe-b29e-a6af1cc7ca73'
#' @param prepend a string to 'prepend' to the updated string
#'
#' @return a string record variable of form 'RECORD224f8562c92340feb29ea6af1cc7ca73'
#' @export
#'
#' @examples
#' r='224f8562-c923-40fe-b29e-a6af1cc7ca73'
#' rv = recordStringToVariable(r)
#' rvr = recordVariableToString(rv)
#' r==rvr
#' 
#' h='7457113a-34bd-4324-935c-f66bd33f1e4b';
#' hv = recordStringToVariable(h,prepend="HEALTH");
#' hvh = recordVariableToString(hv,prepend="HEALTH");
#' 
#' 
#' 
recordStringToVariable = function(r,prepend="RECORD")
{
  paste(prepend,gsub("-","",r),sep='');
}

#' Convert recordId to a variable that can be used in R
#' 
#' Uses implode from library(tractor.base)
#'
#' @param r_ a record variable, string in form 'RECORD224f8562c92340feb29ea6af1cc7ca73'
#' @param prepend a string to 'prepend' to the updated string
#'
#' @return a string record string in form '224f8562-c923-40fe-b29e-a6af1cc7ca73'
#' @export
#'
#' @examples
#' r='224f8562-c923-40fe-b29e-a6af1cc7ca73'
#' rv = recordStringToVariable(r)
#' rvr = recordVariableToString(rv)
#' r==rvr
#' 
#' h='7457113a-34bd-4324-935c-f66bd33f1e4b';
#' hv = recordStringToVariable(h,prepend="HEALTH");
#' hvh = recordVariableToString(hv,prepend="HEALTH"); 
#' 
#' 
recordVariableToString = function(r_,prepend="RECORD")
{
  len = nchar(r_);
  plen = nchar(prepend);
  lens = c(8,4,4,4,12);
  
  tractor.base::implode( c( substr(r_,1+plen,plen+sum(lens[1])), substr(r_,1+plen+sum(lens[1]),plen+sum(lens[1:2])), substr(r_,1+plen+sum(lens[1:2]),plen+sum(lens[1:3])), substr(r_,1+plen+sum(lens[1:3]),plen+sum(lens[1:4])), substr(r_,1+plen+sum(lens[1:4]),plen+sum(lens[1:5])) ) ,sep='-');	
}