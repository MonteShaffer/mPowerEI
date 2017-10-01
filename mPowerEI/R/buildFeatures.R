
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
#' @return rvObj object
#' @export
#'

getMotionObject = function (rv)
{
  
 rvObj = parseSingleRecordVariable(rv);
 rvObj;
  
}



#' Adjust position based on lowess
#' 
#' Assume walking in straight line so x,y,z should all be straight
#' Assume mode is zero
#'
#' @param xt 
#' @param yi 
#'
#' @return updated yi
#' @export
#'
adjustDriftLowess = function(xt,yi)
{
  
  myL = lowess(xt,yi);  # lowess adjustment for drift...
  med = median(yi, na.RM = T);
  deltaML = myL$y - med;	
  yn = yi - deltaML;	
    #ymod = Mode(yn);  # is mode always zero acceleration?
    ymean = mean(yn);
  
  #yn = yn - ymod; # normed to mode = 0 acceleration
  
  yn = yn - ymean;    
    
  yn;
}

#' Determine if the accelerometer is active, drop noise before full on walking
#'
#' @param dframe 
#'
#' @return list of first and last index, with updated yn zero-centered acceleration with drift ... 
#' @export
#'
determineMotionInterval = function(dframe,mot="pos",act="outbound")
{
  
  xt = dframe$time;
  yi = dframe[[mot]];
  
  yn = adjustDriftLowess(xt,yi);  # leveled, mean-center = 0
    ymin = min(yn);
    ymax = max(yn);
    yrange = ymax - ymin;
    
    xmin = min(xt);
    xmax = max(xt);
    xrange = xmax - xmin;
  ymed = median(yn);
  ymean = mean(yn);
  yiqr = IQR(yn);
  ysd = sd(yn);
  
  if(act != "rest")
  {
  yminsearch = ymed - 2*yiqr;
  ymaxsearch = ymed + 2*yiqr;
  } else {
    #yminsearch = ymean - 0.25* ysd;
    #ymaxsearch = ymean + 0.25* ysd;
    
    yminsearch = ymed - 0.5*yiqr;
    ymaxsearch = ymed + 0.5*yiqr;
  }
  
  #yminsearch = ymed - ysd;
  #ymaxsearch = ymed + ysd;
  
  # make certain it is in the window before we start to search if it is out
  # prevents edge spikes
  infirst = which(yn > yminsearch & yn < ymaxsearch)[1];
    firsts = which((yn < yminsearch | yn > ymaxsearch));
  first = firsts[ which(firsts > infirst)[1] ];
  
  
  # reverse the vectors and search again ... 
  inlast = length(yn) - which(rev(yn) > yminsearch & rev(yn) < ymaxsearch)[1];
    lasts = length(yn) - which(rev(yn) < yminsearch | rev(yn) > ymaxsearch );
  last = lasts[ which(lasts < inlast)[1] ];
  
  ynn = adjustDriftLowess(xt[first:last],yi[first:last]);
  
  
  list(yn=yn,first=first,last=last,ynn=ynn);
 
}




#' MAD (median absolute deviation)
#'
#' Plus median, IQR
#' 
#' @param x 
#'
#' @return list of results
#' @export
#'

getMAD = function(x)
{
  
  # median absolute deviation
  # https://en.wikipedia.org/wiki/Median_absolute_deviation
  len = length(x);
  med = median(x);
  iqr = IQR(x);
  dx = x - med;
  dx_ = abs(x-med);
  MAD = median(dx_);  # MAD can be zero ... iqr can be zero, so sigma can be zero
  
  k = 1.4826;  
  sigma = k * MAD;
  
  # https://www.ibm.com/support/knowledgecenter/en/SSWLVY_1.0.0/com.ibm.spss.analyticcatalyst.help/analytic_catalyst/modified_z.html
  
  if(is.na(MAD) | MAD == 0)
  {
    mea = mean(x);
    dxm = x-mea;
    dxm_ = abs(x-mea);
    meanAD = sum(dxm_)/length(x);
    
    
    k = 1.253314;  
    MAD = k * meanAD;
    sigma = k * meanAD;
    
  }
  
  
  
  
  MADs = dx / MAD;  # normed
  sx = dx / sigma;
  
  # +/- 3 sigma is good data?
  # filter values with NA
  fx = ( ifelse((sx < 3 & sx > -3), sx,NA) );
  
  list("median"=med,"IQR"=iqr,"MAD"=MAD,"k"=k,"sigma"=sigma,"MADs"=MADs,"sx"=sx,"fx"=fx,"x"=x,"dx"=dx);
  
  #list("median"=med,"MAD"=MAD,"x"=x,"dx"=dx,"MADs"=MADs);
  
  
}


#' Compute mode
#'
#' https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
#' @param x 
#'
#' @return mode
#' @export
#'
 
Mode <- function(x) {
  
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


#' Truncate data based on significant motion
#'
#' @param olist list from orientToGravity
#' @param act  should be "outbound" or "return"
#'
#' @return nlist truncated list with aligned timestamps based on motion
#' @export
#'

computeMotionWindow = function(olist,act)
{
  intervals = list(); 
  integrals = list();
  firsts = list();
  lasts = list();
  
  for(d in setup$dims)
  {	
    #print(d);
    oriented = as.data.frame(cbind(olist[[act]]$timestamp / 1000,olist[[act]][[d]]));
      datapoints = dim(oriented)[1];
      if(datapoints < 25) { return(NULL); } # don't need to do other dims
      
      colnames(oriented) = c("t","acc");
      
      integral = doIntegration(oriented$t,oriented$acc);
      integrals[[d]] = integral;
      
    interval =  determineMotionInterval(integral,"pos",act); 
      
    intervals[[d]] = interval;
      firsts[[d]] = interval$first;
      lasts[[d]] = interval$last;
      olist[[act]][[d]] = interval$yn; # updated normed acceleration
      
  }
  
  minidx=max(unlist(firsts));  if(minidx < 5) {minidx = 5;}  # truncate "bad angles"
  maxidx=min(unlist(lasts));
  
  onlist = olist;
    for(d in setup$dims)
    {
    # update to second-pass lowess (lowess, find motion, then update lowess)
    #  ynn = adjustDriftLowess(xt[first:last],yi[first:last]);
    first = intervals[[d]]$first;
    last = intervals[[d]]$last;
    
    onlist[[act]][[d]][first:last] = intervals[[d]]$ynn;
  }
  
  
  nlist = onlist[[act]][minidx:maxidx,];
    if(dim(nlist)[1] < 25) { return(NULL); }
  
  
  nlist$Angles = NA;
  for(i in 1:dim(nlist)[1]-1)
    {
    nlist$Angles[i+1] = computeAngle(nlist[i,2:4],nlist[i+1,2:4],"degrees");
    }
  nlist$Angles[1] = 0;
  
  
  
  nlist; 		
}


getGlobalExtremesFromLocals = function(tpobj,dir="min",cuts=3)
{
  first = 1;
  last = first + cuts - 1;
  globalidx = 0;
  
  len = length(tpobj$yi); 
  results = NULL;
  
  while(last <= len)
    {
    if(dir=="min")
    {
    local = which.min(tpobj$yi[first:last]);
    } else { local = which.max(tpobj$yi[first:last]); }
      result = globalidx + local;
    results = c(results, result);
    globalidx = result;

    first = result + 1;
    last = first + cuts - 1;
    
    remaining = len - last;
    if(remaining < 1) { break; } # skip last
  }
  
  results;
  
}





computeMotionDetails = function(rv,info,plotme=TRUE)
{
  recordFolder = paste( getRecordPath(rv), "motion-details", "",sep="/");
  
  loop = c("outbound","returnwalk", "resting");
  # each could be null which means NA;  # outbound, returnwalk, resting
  
  result = NULL;
  
  for(loo in loop)
  {
    dframe = info[[loo]];  
    
    
    
    if(!is.null(dframe))
    {
      
      
      
      
      
      
      for(d in setup$dims)
      {
        
        # set initial NA values
        usefulTime = subExtremes = subDomain = minorCycles = minorHz = overallMin = overallMax = overallAmplitude = majorCycles = subLength = majorAmplitudeMedian = majorAmplitudeIQR = majorCycleTimeMAD = majorCycleOutOfBounds = majorHz = majorCycleTimeMedian = majorCycleTimeIQR = majorCycleTimeMAD = majorCycleOutOfBounds = NA;
        
        majorAmplitudes = majorDetails = NA;
        
        usefulTime = diff(range(dframe$timestamp))/1000;
        # outbound, returnwalk [pick 1, combine, etc.]
        
        dfxy = as.data.frame(cbind(dframe$timestamp,dframe[[d]])); 
        colnames(dfxy) = c("xt","yi");
        
        tp = pastecs::turnpoints(dfxy$yi);
        
        subMin = dfxy[tp$pits,];  dim(subMin);
        subMax = dfxy[tp$peaks,];	dim(subMax);
        
        subExtremes = dim(subMin)[1] + dim(subMax)[1];	subExtremes;
        subDomain = diff(range(subMin$xt)) / 1000; subDomain; # seconds	
        
        minorCycles = subExtremes/2;      minorCycles;
        minorHz = microCycles/subDomain;  minorHz;
        
        overallMin = min(subMin$yi); overallMin;
        overallMax = max(subMax$yi); overallMax;
        
        overallAmplitude = overallMax - overallMin;
        
        ## not useful ##
        #diffMin = diff(subMin$xt); diffMin;
        #diffMinMad = getMAD(diffMin); str(diffMinMad);
        #diffMax = diff(subMax$xt); diffMax;
        #diffMaxMad = getMAD(diffMax); str(diffMaxMad);
        
        subMinE = getGlobalExtremesFromLocals(subMin,"min",3);  subMinE;  # this matters
        subMaxE = getGlobalExtremesFromLocals(subMax,"max",3);  subMaxE;
        
        majorCycles = (length(subMinE) + length(subMaxE))/2;    majorCycles;
        
        timePointsMin = subMin$xt[subMinE]; timePointsMin; diff(timePointsMin);
        timePointsMax = subMax$xt[subMaxE]; timePointsMax; diff(timePointsMax);
        
        subLength = min(length(subMinE),length(subMaxE));  # full pairs
        yValuesMin = subMin$yi[subMinE];  yValuesMin;
        yValuesMax = subMax$yi[subMaxE];  yValuesMax;
        majorAmplitudes = yValuesMax[1:subLength] - yValuesMin[1:subLength];  
        majorAmplitudes;   
        aMAD = getMAD(majorAmplitudes); 
          # str(aMAD);
        
        majorAmplitudeMedian  = aMAD$median;   majorAmplitudeMedian;
        majorAmplitudeIQR     = aMAD$IQR;      majorAmplitudeIQR;
        majorAmplitudeMAD     = aMAD$MAD;      majorAmplitudeMAD;
        
        # proportion outside IQR  
        majorAmplitudeOutOfBounds = sum( abs(aMAD$dx) > aMAD$IQR) / subLength; 
        majorAmplitudeOutOfBounds;
        
        diff(range(timePointsMin))/1000;  diff(range(timePointsMax))/1000;
        
        majorDetails = c(diff(timePointsMin),diff(timePointsMax)) / 1000; 
        majorDetails; sum(majorDetails)/2;
        
        majorHz = majorCycles / ( sum(majorDetails)/2 ); majorHz;
        
        mDMAD = getMAD(1/majorDetails);  
          #str(mDMAD);
        
        majorCycleTimeMedian  = mDMAD$median;   majorCycleTimeMedian;
        majorCycleTimeIQR     = mDMAD$IQR;      majorCycleTimeIQR;
        majorCycleTimeMAD     = mDMAD$MAD;      majorCycleTimeMAD;
        
        # how many outside IQR?  proportion of majorCycles
        majorCycleOutOfBounds = sum( abs(mDMAD$dx) > mDMAD$IQR) / majorCycles;  majorCycleOutOfBounds;
        
        
        
        if(plotme)
        {
          
          pname = paste(loo,d,sep="-"); # save as file in appropriate folder?
          pfile = paste(recordFolder,pname,".pdf",sep="");
          pdf(pfile,7,7); # in inches
          
          # http://www.endmemo.com/program/R/pchsymbols.php
          plot(dfxy,type="l", main=pname, xaxt='n', yaxt='n', xlab="", ylab="");
          par(new=T);
          plot(subMin$xt,subMin$yi,main="",xlim=c(min(dfxy$xt),max(dfxy$xt)),ylim=c(min(dfxy$yi),max(dfxy$yi)),xlab="",ylab="",col="red", xaxt='n',  yaxt='n');
          par(new=T);
          plot(subMax$xt,subMax$yi,main="",xlim=c(min(dfxy$xt),max(dfxy$xt)),ylim=c(min(dfxy$yi),max(dfxy$yi)),xlab="",ylab="",col="green", xaxt='n',  yaxt='n');
          par(new=T);
          plot(subMin$xt[subMinE],subMin$yi[subMinE],main="",xlim=c(min(dfxy$xt),max(dfxy$xt)),ylim=c(min(dfxy$yi),max(dfxy$yi)),xlab="",ylab="",bg="red",col="red", xaxt='n',  yaxt='n',pch=25);
          par(new=T);
          plot(subMax$xt[subMaxE],subMax$yi[subMaxE],main="",xlim=c(min(dfxy$xt),max(dfxy$xt)),ylim=c(min(dfxy$yi),max(dfxy$yi)),xlab="",ylab="",bg="green",col="green", xaxt='n',  yaxt='n',pch=24);
          dev.off();
        }
        
        result[[loo]][[d]] = list(usefulTime = usefulTime, subExtremes = subExtremes, subDomain = subDomain, minorCycles = minorCycles, minorHz = minorHz, overallMin = overallMin, overallMax = overallMax, overallAmplitude = overallAmplitude, majorCycles = majorCycles, subLength = subLength, majorAmplitudeMedian = majorAmplitudeMedian, majorAmplitudeIQR = majorAmplitudeIQR, majorCycleTimeMAD = majorCycleTimeMAD, majorCycleOutOfBounds = majorCycleOutOfBounds, majorHz = majorHz, majorCycleTimeMedian = majorCycleTimeMedian, majorCycleTimeIQR = majorCycleTimeIQR, majorCycleTimeMAD = majorCycleTimeMAD, majorCycleOutOfBounds = majorCycleOutOfBounds, majorAmplitudes = majorAmplitudes, majorDetails =majorDetails);
        
      } # end of d
      
      
      
    }  # !null
    
    
    
  }  # for loo
  
  
  
  result;
}


#' Motion Features
#'
#' @param rvObj 
#'
#' @return list of features
#' @export
#'

getMotionFeaturesFromRecord = function(rv,plotme=TRUE)
{
  rvObj = getMotionObject(rv);
  
  recordObj = list();
  
  recordFolder = paste( getRecordPath(rv), "motion-details", "",sep="/");
    if(!dir.exists(recordFolder)) { dir.create(recordFolder,recursive=T); }
  
    rawF = paste(setup$designpoint,"motionDetails.Rda",sep="-");
  recordFile = paste(recordFolder,rawF,sep="");
  
  if(file.exists(recordFile)) 
  {
  load(recordFile);
    return(recordObj);
  }
  
  outbound = returnwalk = resting = NULL;
    outbound = computeMotionWindow(rvObj$motion$olist,"outbound");
    returnwalk = computeMotionWindow(rvObj$motion$olist,"return");
    resting = computeMotionWindow(rvObj$motion$olist,"rest");
    
  recordObj$motionWindow = list("outbound"=outbound,"returnwalk"=returnwalk,"resting"=resting);
  
    
   # $angle was from "acc"
   # $Angles is from "pos" after normed with lowess / motion trimmed 
    
  walkingACCMED = walkingACCMAD = walkingACCIQR = NA;  
  restingACCMED = restingACCMAD = restingACCIQR = NA;

  angles.walking = NULL;
    if(!is.null(outbound)) { angles.walking = c(angles.walking,outbound$angle); }  
    if(!is.null(returnwalk)) { angles.walking = c(angles.walking,returnwalk$angle); } 
  angles.resting = NULL; 
    if(!is.null(resting)) { angles.resting = c(angles.resting,resting$angle); } 
  
  angles.walking.MAD = getMAD(angles.walking);
    walkingACCMED = angles.walking.MAD$median; 
    walkingACCMAD = angles.walking.MAD$MAD; 
    walkingACCIQR = angles.walking.MAD$IQR; 
  angles.resting.MAD = getMAD(angles.resting);
    restingACCMED = angles.resting.MAD$median; 
    restingACCMAD = angles.resting.MAD$MAD; 
    restingACCIQR = angles.resting.MAD$IQR; 
  
  
  walkingPOSMED = walkingPOSMAD = walkingPOSIQR = NA;  
  restingPOSMED = restingPOSMAD = restingPOSIQR = NA;
  
  angles.walking = NULL;
    if(!is.null(outbound)) { angles.walking = c(angles.walking,outbound$Angles); }  
    if(!is.null(returnwalk)) { angles.walking = c(angles.walking,returnwalk$Angles); } 
  angles.resting = NULL; 
    if(!is.null(resting)) { angles.resting = c(angles.resting,resting$Angles); } 
  
  if(!is.null(angles.walking)) 
    {
    angles.walking.MAD = getMAD(angles.walking);
    walkingPOSMED = angles.walking.MAD$median; 
    walkingPOSMAD = angles.walking.MAD$MAD; 
    walkingPOSIQR = angles.walking.MAD$IQR; 
    }
  if(!is.null(angles.resting)) 
    {
  angles.resting.MAD = getMAD(angles.resting);
    restingPOSMED = angles.resting.MAD$median; 
    restingPOSMAD = angles.resting.MAD$MAD; 
    restingPOSIQR = angles.resting.MAD$IQR; 
  }
  
  
  recordObj$angles = list(walkingACCMED = walkingACCMED, walkingACCMAD = walkingACCMAD, walkingACCIQR = walkingACCIQR, restingACCMED = restingACCMED, restingACCMAD = restingACCMAD, restingACCIQR = restingACCIQR, walkingPOSMED = walkingPOSMED, walkingPOSMAD = walkingPOSMAD, walkingPOSIQR = walkingPOSIQR, restingPOSMED = restingPOSMED, restingPOSMAD = restingPOSMAD, restingPOSIQR = restingPOSIQR);
  
  
 more = computeMotionDetails(rv,recordObj$motionWindow,plotme);
  # POS (outbound,returnwalk); resting ... x,y,z => ranked 1,2,3
  
  recordObj$more = more;
 
save(recordObj, file=recordFile);
  
  return(recordObj);
  
}





#' Pedometer Features
#'
#' @param rvObj 
#'
#' @return list of features
#' @export
#'

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
