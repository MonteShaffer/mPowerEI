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





getRecordPath = function (rv)
{
  # requires audit
  r = recordVariableToString(rv);
  rinfo=audit$rclist[[rv]];
  hv = rinfo$info[1];
  h = recordVariableToString(hv,prepend="HEALTH");
  
  userFolder = paste(localCache,"userObjects",hv,sep="/");
  recordFolder = paste(userFolder,rv,sep="/");
  
  recordFolder;
}





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



#' Access attributes of data frame or list 
#' 
#' The string is evaluated to overcome issues with \code{assign} and \code{get} methods.
#' https://stackoverflow.com/questions/35275842/using-r-how-to-reference-variable-variables-or-variables-variable-a-la-php-r/35374712#35374712
#'
#' @param str string that needs to be evaluated
#'
#' @return evaluated value from the string
#' @export
#'
#' @examples
#' model = list("four" = "score", "seven"="years");
#' str = 'model$four';
#' result = access(str);
#' print(result);
#' 
#' m = matrix(rnorm(1000), ncol=25);
#' str='m[1:5,8:10]';
#' result = access(str);
#' print(result);
#' 
#' 
#' 
#' ## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
#' ## Page 9: Plant Weight Data.
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14);
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69);
#' group <- gl(2, 10, 20, labels = c("Ctl","Trt"));
#' weight <- c(ctl, trt);
#' lm.D9 <- lm(weight ~ group);
#' lm.D90 <- lm(weight ~ group - 1); # omitting intercept
#' 
#' myA = anova(lm.D9); myA; str(myA);
#' 
#' str = 'myA@heading';
#' result = access(str);
#' print(result);
#' 
#' 
#' myS = summary(lm.D90); myS; str(myS);
#' 
#' 
#' str = 'myS$terms@factors';
#' result = access(str);
#' print(result);
#' 
#' str = 'myS$terms@factors@dimnames';
#' result = access(str);
#' print(result);
#' 
#' str = 'myS$terms@dataClasses@names';
#' result = access(str);
#' print(result);
#' 
access <- function(str)
{
  
  E = unlist( strsplit(as.character(str),"[@]") );
  k = length(E);
  if(k==1)
  {
    eval(parse(text=str));
  } else {
    # k = 2
    nstr = paste("attributes(",E[1],")",sep="");
    nstr = paste(nstr,'$',E[2],sep="");
    
    if(k>2) {
      for(i in 3:k)
      {
        nstr = paste("attributes(",nstr,")",sep="");
        nstr = paste(nstr,'$',E[i],sep="");
      }
    }
    access(nstr);
  }
}


#https://stackoverflow.com/questions/4948361/how-do-i-save-warnings-and-errors-as-output-from-a-function
