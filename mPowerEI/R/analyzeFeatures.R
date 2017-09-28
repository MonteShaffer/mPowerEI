
# do system-wide loops on different analyses


getImputationMedians(dframe,good,myC)
{
  # good records may be bad if badJSON existed ...
  
  iframe = dframe[dframe$rv == good,myC];
  # sum(is.na(df$col))
  
  
}

imputateDataFrame = function(dframe,myC)
{
  # dframe = pfeats;
  # let's cache all unique healthcode median values ...
  medianCache = list();
    myHVs = unique(dframe$hv);  
    hlen = length(myHVs);
  for(i in 1:hlen)
    {
    print(paste(i," of ",hlen)); flush.console();
    hvi = myHVs[i];
    s = subset(dframe, hv == as.character(hvi))[myC];  # 1:20
    sn = na.omit(s);
    hmeds = plyr::colwise(median)(sn);
    medianCache[[hvi]] = hmeds;
    }
  
    
  # let's replace Inf -Inf with NA
    # https://stackoverflow.com/questions/12188509/cleaning-inf-values-from-an-r-dataframe
    dframe <- do.call(data.frame,lapply(dframe, function(x) replace(x, is.infinite(x),NA)));
    dframe <- do.call(data.frame,lapply(dframe, function(x) replace(x, is.nan(x),NA)));
    
  # loop once, and replace NA with median values from goodRecords within a health code id
    dlen = dim(dframe)[1];
    for(i in 1:dlen)
    {
      print(paste(i," of ",dlen)); flush.console();
      drow = dframe[i,];
      hv = drow$hv;
      dcount = sum(is.na(drow));
      if(dcount != 0)
      {
        # we have at least one NA
          for(j in myC)
          {
            dv = drow[j];
            dvn = names(drow)[j];
            
            if(is.na(dv))
            {
              replacev = medianCache[[hv]][[dvn]];
            dframe[i,j] = replacev;
            }
          #stop();
          }
      }
    }
    
    
    # loop again, and replace NA with median values from all good records plus noise 
    # 5799 records still left ...
    dframen = na.omit(dframe)[myC];
    gmeds = plyr::colwise(median)(dframen);
    
    dlen = dim(dframe)[1];
    for(i in 1:dlen)
    {
      print(paste(i," of ",dlen)); flush.console();
      drow = dframe[i,];
      hv = drow$hv;
      dcount = sum(is.na(drow));
      if(dcount != 0)
      {
        # we have at least one NA
        for(j in myC)
        {
          dv = drow[j];
          dvn = names(drow)[j];
          
          if(is.na(dv))
          {
            noise = runif(1, -0.001, 0.001);
            replacev = gmeds[[dvn]] + noise;
            dframe[i,j] = replacev;
          }
          #stop();
        }
      }
    }
    
    
   
    
  dframe;
}


#' Appends health record information
#'
#' @param dframe 
#'
#' @return update dframe
#' @export
#'

appendRecordData = function(pframe)  # 6.6 minutes if not cached...
{
  # 3 minutes
  myO = paste(localCache,"summaryObjects","",sep="/");
  pedF = paste(myO, paste(synapseProject,"PEDOMETER-HEALTH",sep='-'), ".Rda", sep='');
  tstart = Sys.time();
  
  
  if(!file.exists(pedF))
  {
    
    
  records = rownames(pframe);
  pframe$rv = records;
  pframe$hv = NA;
  pframe$healthState = pframe$isPD = pframe$gender = pframe$age = NA;
    
    
    
    #trainme$healthState[mPower$walking.training$medTimepoint=="I don't take Parkinson medications"] = 5;
    
    
    #trainme$isPD = NA; 
    
  
  
  #pframe$demographics = NA;
  
  # codeHealthState
  
  
  # row.names(audit$aframe)  ... unique health codes
  # row.names(audit$alist)   ... list of good/bad records to figure out subset of health-records...
  # audit$alist[[healthCode]]$details$goodRecords;
  
  rlen = length(records);
  for(i in 1:rlen)
    {
    print(paste(i," of ",rlen)); flush.console();
    rv = records[i];
      myH = audit$rclist[[rv]]$info[1];
    pframe$hv[i] = myH;
      demographics = audit$alist[[myH]]$demographics;
      if(length(demographics) > 1)
      {
        pframe$age[i] = scoreFactor(demographics,"age","age"); 
        pframe$healthState[i] = scoreFactor(demographics,"medTimepoint","healthState");
        pframe$isPD[i] = scoreFactor(demographics,"medTimepoint","isPD");
        pframe$gender[i] = scoreFactor(demographics,"gender","gender");
      }
    #pframe$demographics[i] = audit$alist[[myH]]$demographics;
    }
  
  save(pframe,file=pedF);
  
  }  else {
  
  load(pedF);
}
  
  tend = Sys.time();
  timer = tend - tstart; 
  print(timer);
  
  
  pframe;
}

getPedometerFeatures = function()
{
  # we are building for testing, so will use all 79,000 records  ## 2.2 hours to build ...
  tstart = Sys.time();
  
  
  myO = paste(localCache,"summaryObjects","",sep="/");
  pedF = paste(myO, paste(synapseProject,"PEDOMETER",sep='-'), ".Rda", sep='');
  
  if(!file.exists(pedF))
  {
  pframe = data.frame();
  
  records = names(audit$rclist);
    rlen = length(records);
  for(i in 1:rlen)
  {
    print(paste(i," of ",rlen)); flush.console();
    rv = records[i];
    rvObj = getMotionObject(rv);
    
    pfeat = getPedometerFeaturesFromRecord(rvObj);
    pframe = rbind(pframe,pfeat);
    
  }
    
    rownames(pframe) = records;
    
    tend = Sys.time();
    
    
    save(pframe,file=pedF);
  } else {
    
    load(pedF);
    tend = Sys.time();
  }
    
  timer = tend - tstart; 
    print(timer);
  
    
  
  pframe;
  
}






#' Analyze Features
#'
#' Graphs, Plots, and Play
#' @return nothing
#' @export
#'

analyzeFeatures = function() {}
