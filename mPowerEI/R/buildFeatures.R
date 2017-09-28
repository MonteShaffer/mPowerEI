
#' Compute Range differences based on normalizing to min or max
#' 
#' How far is "max" from the "min" in normalized units?
#' How far is "min" from the "max" in normalized units?
#'
#' @param x 
#'
#' @return list with details from computation
#' @export
#'
#' @examples
computeNormValue = function(x)
{
  
  d = range(x);  # we don't want a zero here ... it may be freeze of gait, but math stops working well.
    if(d[1] < .000001) { d[1] = 0.001; }
    dmin = d / min(d);
    dmax = d / max(d);
    
  ddeviationmax = abs(dmin[2] - dmin[1]);
  ddeviationmin = abs(dmax[2] - dmax[1]);
  
list(d=d,dmin=dmin,dmax=dmax,ddeviationmin=ddeviationmin,ddeviationmax=ddeviationmax);
}


#' Load Motion Object
#'
#' @param rv [requires audit]
#'
#' @return
#' @export
#'
#' @examples
getMotionObject = function (rv)
{
  
 rvObj = parseSingleRecordVariable(rv);
 rvObj;
  
}


#' Title
#'
#' @param rvObj 
#'
#' @return
#' @export
#'
#' @examples
getPedometerFeaturesFromRecord = function(rvObj)
{
  
  # rvObj = getMotionObject(rv);
  plist = pinfo = list();
  
  
  for(act in setup$acts)
  {
    pinfo[[act]] = rvObj$raw$tlist[[act]]$pedometer;
    plen = length(pinfo[[act]]);
    if(plen > 2)
      {
      # let's not record if less than 2 data points, I don't care if rest [unlikely] or outbound/return ... I will merge into a final list ...
      
      # plist = rbind(plist,pinfo[[act]][,5:16]);
      plist = rbind(plist,pinfo[[act]][,5:13]);
      }
    
  }
  
  
  plen = dim(plist)[1];
  

        medianDDPS = medianDSPS = medianDD = medianDS = medianDPS = medianSPS = NA;
        iqrDDPS = iqrDSPS = iqrDD = iqrDS = iqrDPS = iqrSPS = NA;  
  
    mindeviationDPS = maxdeviationDPS = mindeviationDDPS = maxdeviationDDPS = NA;
    mindeviationSPS = maxdeviationSPS = mindeviationDSPS = maxdeviationDSPS = NA;
  
  if(!is.null(plen))
    {
    # medians ... biased based on height
    med = plyr::colwise(median)(plist);
        medianDDPS = med$deltaDistancePerSecond;
        medianDSPS = med$deltaStepsPerSecond;
        medianDD = med$deltaDistance;
        medianDS = med$deltaSteps;
        medianDPS = med$distancePerSecond;
        medianSPS = med$stepsPerSecond;
        
    # IQR ... biased based on height
        iqrDDPS = IQR(plist$deltaDistancePerSecond);
        iqrDSPS = IQR(plist$deltaStepsPerSecond);
        iqrDD = IQR(plist$deltaDistance);
        iqrDS = IQR(plist$deltaSteps);
        iqrDPS = IQR(plist$distancePerSecond);
        iqrSPS = IQR(plist$stepsPerSecond);
    
    # normMin / normMax   ... standardize to min/max values ... independent of height
        
        cinfo = computeNormValue(plist$deltaDistancePerSecond);
          mindeviationDDPS = cinfo$ddeviationmin;
          maxdeviationDDPS = cinfo$ddeviationmax;
          
        cinfo = computeNormValue(plist$distancePerSecond);
          mindeviationDPS = cinfo$ddeviationmin;
          maxdeviationDPS = cinfo$ddeviationmax;
          
        cinfo = computeNormValue(plist$deltaStepsPerSecond);
          mindeviationDSPS = cinfo$ddeviationmin;
          maxdeviationDSPS = cinfo$ddeviationmax;
          
        cinfo = computeNormValue(plist$stepsPerSecond);
          mindeviationSPS = cinfo$ddeviationmin;
          maxdeviationSPS = cinfo$ddeviationmax;
          
    
    }
    

  list(medianDDPS=medianDDPS, medianDSPS= medianDSPS, medianDD= medianDD, medianDS= medianDS, medianDPS= medianDPS, medianSPS= medianSPS, iqrDDPS =iqrDDPS, iqrDSPS= iqrDSPS, iqrDD= iqrDD,iqrDS = iqrDS, iqrDPS= iqrDPS, iqrSPS= iqrSPS, mindeviationDPS = mindeviationDPS, maxdeviationDPS = maxdeviationDPS, mindeviationDDPS = mindeviationDDPS, maxdeviationDDPS = maxdeviationDDPS, mindeviationSPS =  mindeviationSPS, maxdeviationSPS = maxdeviationSPS, mindeviationDSPS = mindeviationDSPS, maxdeviationDSPS = maxdeviationDSPS);
  
  
  
}










#' Build Features From Data Objects ... happens at "record-id" level (rv)
#'
#' @return nothing
#' @export
#'

buildFeatures = function() {}
