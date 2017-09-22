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









# 
#' Compute Angle between two vectors in n-dim space
#'
#' https://stackoverflow.com/questions/1897704/angle-between-two-vectors-in-r
#' 
#' @param v1 numeric vector
#' @param v2 numeric vector
#' @param out degree output format (default is 'radians', also 'degrees')
#'
#' @return numeric angle in 'out' format
#' @export
#'
#' @examples
#' v1 = c(2,1,0); v2 = c(0,1,2);
#' computeAngle(v1,v2);
#' computeAngle(v1,v2,out="degrees");
#' 
computeAngle = function(v1,v2, out="radians")
{
  v1_ = vectorMagnitude(v1);
  v2_ = vectorMagnitude(v2);
  
  cosTheta = pracma::dot(v1,v2) / (v1_ * v2_);
  
  theta = acos(cosTheta);	
  if(out == "degrees") { theta = theta * 180 / pi; }
  theta;
}



# vgrotated = c(0,0,-1);
# vgraw = as.numeric(tlist$outbound$deviceMotion[1,12:14]);
# gRotatedMatrix = getRotationMatrix(vgraw,vgrotated);

# vraw = as.numeric(tlist$outbound$accel[1,-1]);
# vrotated = ???
# vrotated = vraw%*%gRotatedMatrix;


#' Determine length or magnitude of n-dim vector
#'
#' @param v numeric vector of coordinates
#'
#' @return numeric magnitude
#' @export
#'
#' @examples
#' vectorMagnitude(v1);
#' vectorMagnitude(v1-v2);
#' 
vectorMagnitude = function(v)
{
  
  sqrt(sum(v^2));
}
#' Normalize a vector so the magnitude is 1 (unit radius)
#'
#' @param v numeric vector of coordinates
#'
#' @return numeric vector of coordinates (norm 1)
#' @export
#'
#' @examples
#' unitVector(v1)
#' unitVector(v2)
unitVector = function(v)
{

  vlen = vectorMagnitude(v);  # can this be nonzero?
  v/vlen;
}

#' Create rotation matrix (3-D space)
#' 
#' Represent the transformations required to get from v1 to v2
#'
#' @param v1 numeric vector of length 3
#' @param v2 numeric vector of length 3
#'
#' @return numeric matrix (3x3)
#' @export
#'
#' @examples
#' getRotationMatrix(v1,v2);
getRotationMatrix = function(v1,v2)
{
  #https://gamedev.stackexchange.com/questions/20097/how-to-calculate-a-3x3-rotation-matrix-from-2-direction-vectors
  
  v1_ = unitVector(v1);
  v2_ = unitVector(v2);
  
  # if(v1_ == v2_) { stop("vectors are equal"); }
  
  matr = matrix(0,nrow=3,ncol=3);
  
  matr[1,] = v1_;
  matr[3,] = unitVector(pracma::cross(v1,v2));
  matr[2,] = unitVector(pracma::cross(matr[3,],v1));
  
  
  matr;
}


#' Create quaternion (3-D space)
#' 
#' Represent the transformations required to get from v1 to v2
#'
#' @param v1 numeric vector of length 3
#' @param v2 numeric vector of length 3
#'
#' @return numeric vector of length 4 [w  x,y,z] with *norm 1*
#' @export
#'
#' @examples
#' getQuaternion(v1,v2);
#' 
getQuaternion = function(v1,v2)
{
  # quat is of form (w,x,y,z)
  # http://lolengine.net/blog/2013/09/18/beautiful-maths-quaternion-from-vectors
  # http://lolengine.net/blog/2014/02/24/quaternion-from-two-vectors-final
  # https://stackoverflow.com/questions/1171849/finding-quaternion-representing-the-rotation-from-one-vector-to-another	
  #https://math.stackexchange.com/questions/2251214/calculate-quaternions-from-two-directional-vectors
  # http://tutis.ca/Rotate/7quaternions.htm
  
  v1_ = unitVector(v1);
  v2_ = unitVector(v2);
  
  
  
  quat = c( pracma::dot(v1_,v2_), pracma::cross(v1_,v2_) );
  quat[0] = quat[0] + vectorMagnitude(quat);
  
  
  unitVector(quat);
}














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

