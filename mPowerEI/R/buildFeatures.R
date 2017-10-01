
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
#' Assume mean is zero-ish
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
determineMotionInterval = function(integral,mot="pos",act="outbound")
{
  
  xt = integral$time;
  yi = integral[[mot]];
    ymin = min(yi);
    ymax = max(yi);
              #plot(xt,yi,type="l",xlim=c(xmin,xmax),ylim=c(ymin,ymax));
  
  yn = adjustDriftLowess(xt,yi);  # leveled, mean-center = 0
    ymin = min(yn);
    ymax = max(yn);
    yrange = ymax - ymin;
              #plot(xt,yn,type="l",xlim=c(xmin,xmax),ylim=c(ymin,ymax));
    
    xmin = min(xt);
    xmax = max(xt);
    xrange = xmax - xmin;
  ymed = median(yn);
  ymean = mean(yn);
  yiqr = IQR(yn);
  ysd = sd(yn);
              #abline(h=ymed,col="gray");
  
    
  # if data is pretty normal, IQR doesn't work well ... 
  # rv="RECORDa3e54d84360e4e8a9534de188d5fa9e1";
  
  # find a way to skip good data ...
  
  if(act != "rest")
  {
  yminsearch = ymed - 2*yiqr;
  ymaxsearch = ymed + 2*yiqr;
  
  yminsearchIn = ymed - 1*yiqr;
  ymaxsearchIn = ymed + 1*yiqr;
  } else {
    #yminsearch = ymean - 0.25* ysd;
    #ymaxsearch = ymean + 0.25* ysd;
    
    yminsearch = ymed - 1*yiqr;
    ymaxsearch = ymed + 1*yiqr;
    
    yminsearchIn = ymed - 0.5*yiqr;
    ymaxsearchIn = ymed + 0.5*yiqr;
    
   
  }
  
            #abline(h=yminsearch,col="red");
            #abline(h=ymaxsearch,col="red");
            
            #abline(h=yminsearchIn,col="pink");
            #abline(h=ymaxsearchIn,col="pink");
  #yminsearch = ymed - ysd;
  #ymaxsearch = ymed + ysd;
  
  # make certain it is in the window before we start to search if it is out
  # prevents edge spikes
  infirst = which(yn > yminsearchIn & yn < ymaxsearchIn)[1];
    firsts = which((yn < yminsearchIn | yn > ymaxsearchIn));
  first = firsts[ which(firsts > infirst)[1] ];
  
  
  
  # reverse the vectors and search again ... 
  inlast = length(yn) - which(rev(yn) > yminsearchIn & rev(yn) < ymaxsearchIn)[1];
    lasts = length(yn) - which(rev(yn) < yminsearchIn | rev(yn) > ymaxsearchIn );
  last = lasts[ which(lasts < inlast)[1] ];
            
  # find a way to skip good data ...          
  if(is.na(first)) { first = 1;}   
  if(is.na(last)) { last = length(xt);} 
            
            #abline(v=xt[first],col="blue");
            #abline(v=xt[last],col="blue");
            
            
  
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
  med = iqr = dx = dx_ = MAD = k = sigma = MADs = sx = fx = NA;
  

  # median absolute deviation
  # https://en.wikipedia.org/wiki/Median_absolute_deviation
  len = length(x);
  print("MAD:");print(len);
  if(len > 3)  # we need enough records
  {
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
  }
  
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
  
  dframe = olist[[act]];
  
  for(d in setup$dims)
  {	
    #print(d);
    oriented = as.data.frame(cbind(dframe$timestamp / 1000,dframe[[d]]));
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
  
  minidx=max(unlist(firsts));  if(minidx < 3) {minidx = 3;}  # truncate "bad angles"
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
  
  
    # if(dim(nlist)[1] < 25) { return(NULL); }
  
  
  nlist$Angles = NA;
  for(i in 1:dim(nlist)[1]-1)
    {
    nlist$Angles[i+1] = computeAngle(nlist[i,2:4],nlist[i+1,2:4],"degrees");
    }
  nlist$Angles[1] = 0;
  
  
  
  nlist; 		
}


#' Global Extremes from Local Extremes
#'
#' @param tpobj turningpoints obj
#' @param dir "min" or "max"
#' @param cuts number of points to consider in a local scan (for hip motion 3 seems to work ok)
#'
#' @return list of indexes related to global extreme
#' @export
#'
getGlobalExtremesFromLocals = function(tpobj,dir="min",cuts=3)
{
  # need to keep track of previous max location ... prevent 1,1,1
  # P:/_synapseCacheMonte/userObjects/HEALTH0049a4192bad47689e6493757c5e74b1/RECORDc12e0b3de7e24fbf9296e036270a6b80
  
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


#' Merge global extremes based on one-two constraint
#'
#' @param subMin 
#' @param subMax 
#' @param subMinE 
#' @param subMaxE 
#' @param animate if true, we plot and animate the results (e.g., debugging)
#'
#' @return list with updated values
#' @export
#'

mergeExtremes = function(subMin,subMax,subMinE,subMaxE,animate=FALSE)
{
  
  subMin;subMax; subMinE;subMaxE;
  
   
  
subFinal = list("subMinE"=c(),"subMaxE"=c(),"cValue"=c(), "cTime"=c());
  minTimes = subMin$xt[subMinE];  minIdx = 1;
  maxTimes = subMax$xt[subMaxE];  maxIdx = 1;
  
  if(animate)
  {
  xmin=min(subMin$xt[subMinE],subMax$xt[subMaxE]);
  xmax=max(subMin$xt[subMinE],subMax$xt[subMaxE]);
  
  ymin=min(subMin$yi[subMinE],subMax$yi[subMaxE]);
  ymax=max(subMin$yi[subMinE],subMax$yi[subMaxE]);
  
  plot(subMin$xt[subMinE],subMin$yi[subMinE],type="b",col="green",xlim=c(xmin,xmax),ylim=c(ymin,ymax))
  par(new=T)
  plot(subMax$xt[subMaxE],subMax$yi[subMaxE],type="b",col="red",xlim=c(xmin,xmax),ylim=c(ymin,ymax))
  }
  
  # start
  current = "min";
  if(min(maxTimes) < min(minTimes)) { current = "max";}
  
  # look for next one that is has changed from globalMin to globalMax (and reverse)
  while(minIdx <= length(minTimes))
  {
  #print(current);
  if(current=="min")
  {
    # update based on current
    subFinal$subMinE = c(subFinal$subMinE,subMinE[minIdx]);
    # loop, update current value
    cTime = minTimes[minIdx]; if(is.na(cTime)) { break; }
    #print(subMinE[minIdx]); print(cTime);
      subFinal$cTime = c(subFinal$cTime,cTime);
      subFinal$cValue = c(subFinal$cValue,subMin$yi[ subMinE[minIdx] ]);
    if(animate)
    {
    points(cTime,subMin$yi[ subMinE[minIdx] ], pch=24, col="green",bg="green");
    Sys.sleep(1);
    }
    
    current="max";
    maxIdx = which(maxTimes > cTime)[1];
    
    if(is.na(maxIdx)) { break; }
    
    #print(maxIdx);
    
    
  } else {
    # update based on current
    subFinal$subMaxE = c(subFinal$subMaxE,subMaxE[maxIdx]);
    # loop, update current value
    cTime = maxTimes[maxIdx]; if(is.na(cTime)) { break; }
    #print(subMaxE[maxIdx]); print(cTime);
      subFinal$cTime = c(subFinal$cTime,cTime);
      subFinal$cValue = c(subFinal$cValue,subMax$yi[ subMaxE[maxIdx] ]);
    if(animate)
    {
    points(cTime,subMax$yi[ subMaxE[maxIdx] ], pch=25, col="red",bg="red");
    Sys.sleep(1);
    }
      
    current = "min";
    minIdx = which(minTimes > cTime)[1];
    
    if(is.na(minIdx)) { break; }
    
    #print(minIdx);
  }
    # end while loop
    
  }
  
  
subFinal;


}


#' Compute Motion Details
#'
#' @param rv record identifer, record variable (rv)
#' @param info contains motion-detected compressed forms of "outbound", "resting", "returnwalk"
#' @param plotme if true, we plot to PDF files in RECORDS folder
#'
#' @return list of details
#' @export
#'

computeMotionDetails = function(rv,info,plotme=TRUE)
{ 
  
  recordFolder = paste( getRecordPath(rv), "motion-details", "",sep="/");
  
  loop = c("outbound","returnwalk", "resting");
  # each could be null which means NA;  # outbound, returnwalk, resting
  
  result = NULL;
  
  for(loo in loop)
  {
    dframe = info[[loo]];  
    
    print(dim(dframe));
    
    if(is.null(dframe)){ next; }
    
    #if(!is.null(dframe))
    {
      
      
      
      
      
      
      for(d in setup$dims)
      {
        
        # set initial NA values
        usefulTime = subExtremes = subDomain = minorCycles = minorHz = overallMin = overallMax = overallAmplitude = majorCycles = subLength = majorAmplitudeMedian = majorAmplitudeIQR = majorCycleTimeMAD = majorCycleOutOfBounds = majorHz = majorCycleTimeMedian = majorCycleTimeIQR = majorCycleTimeMAD = majorCycleOutOfBounds = nAmplitudesExpansion = nAmplitudesContraction = NA;
        
        majorAmplitudes = majorDetails = NA;
        
        usefulTime = diff(range(dframe$timestamp))/1000;
        # outbound, returnwalk [pick 1, combine, etc.]
        
        dfxy = as.data.frame(cbind(dframe$timestamp,dframe[[d]])); 
        colnames(dfxy) = c("xt","yi");
        
        if(length(dfxy$yi) > 4)
        {
        tp = pastecs::turnpoints(dfxy$yi);
        
        subMin = dfxy[tp$pits,];  dim(subMin);
        subMax = dfxy[tp$peaks,];	dim(subMax);
        
        subExtremes = dim(subMin)[1] + dim(subMax)[1];	subExtremes;
        subDomain = diff(range(subMin$xt)) / 1000; subDomain; # seconds	
        
        minorCycles = subExtremes/2;      minorCycles;
        minorHz = minorCycles/subDomain;  minorHz;
        
        overallMin = min(subMin$yi); overallMin;
        overallMax = max(subMax$yi); overallMax;
        
        overallAmplitude = overallMax - overallMin;
        
        ## not useful ##
        #diffMin = diff(subMin$xt); diffMin;
        #diffMinMad = getMAD(diffMin); str(diffMinMad);
        #diffMax = diff(subMax$xt); diffMax;
        #diffMaxMad = getMAD(diffMax); str(diffMaxMad);
        
        subMinE_ = getGlobalExtremesFromLocals(subMin,"min",3);  subMinE_;  # cuts matter
        subMaxE_ = getGlobalExtremesFromLocals(subMax,"max",3);  subMaxE_;
        
        # let's merge and truncate ... may have overstated a single max
        # P:\_synapseCacheMonte\userObjects\HEALTH0049a4192bad47689e6493757c5e74b1\RECORDc12e0b3de7e24fbf9296e036270a6b80\motion-details
        
        # subMin;subMax;subMinE;subMaxE;
        
        subFinal = mergeExtremes(subMin,subMax,subMinE_,subMaxE_);
        
          subMinE = subFinal$subMinE; subMinE;
          subMaxE = subFinal$subMaxE; subMaxE;
        
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
        
        # let's do normed scores to capture compression
        nAmplitudes = (majorAmplitudes)/min(majorAmplitudes);
          nAmplitudesExpansion = max(nAmplitudes)-1;
          
        nAmplitudes = (majorAmplitudes)/max(majorAmplitudes);
          nAmplitudesContraction = 1-min(nAmplitudes);
        

        
        
        
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
        
        result[[loo]][[d]] = list(usefulTime = usefulTime, subExtremes = subExtremes, subDomain = subDomain, minorCycles = minorCycles, minorHz = minorHz, overallMin = overallMin, overallMax = overallMax, overallAmplitude = overallAmplitude, majorCycles = majorCycles, subLength = subLength, majorAmplitudeMedian = majorAmplitudeMedian, majorAmplitudeIQR = majorAmplitudeIQR, majorCycleTimeMAD = majorCycleTimeMAD, majorCycleOutOfBounds = majorCycleOutOfBounds, majorHz = majorHz, majorCycleTimeMedian = majorCycleTimeMedian, majorCycleTimeIQR = majorCycleTimeIQR, majorCycleTimeMAD = majorCycleTimeMAD, majorCycleOutOfBounds = majorCycleOutOfBounds, majorAmplitudes = majorAmplitudes, majorDetails =majorDetails, nAmplitudesExpansion=nAmplitudesExpansion, nAmplitudesContraction=nAmplitudesContraction);
        
        } # end of turnpoints length        
        
      } # end of d
      
      
      
    }  # !null
    
    
    
  }  # for loo
  
  
  
  result;
}


#' Motion Features
#'
#' @param rv record identifier in variable form [rv]
#' @param plotme if true, we plot to a PDF file in the folder "motion-details" of the RECORD
#'
#' @return list of features [recordObj]
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
  #load(recordFile);
  #  return(recordObj);
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
