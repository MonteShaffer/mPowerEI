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
