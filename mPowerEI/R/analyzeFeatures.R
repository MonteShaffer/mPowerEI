#' Compare Records
#'
#' @param submitr list from submit template, r as string
#' @param auditr list from harvest audit, rv as variable
#'
#' @return list (rv) of missing values (we don't have in audit)
#' @export
#'

compareRecords = function(submitr, auditr)
{
  
  submitr_ = c();
  rlen = length(submitr);
  
  for(i in 1:rlen)
  {
   r =  submitr[i];
   rv = recordStringToVariable(r);
   
   submitr_[i] = rv;
  }
  
  
  diffr = setdiff(submitr_,auditr);
  
 diffr; 
}




# do system-wide loops on different analyses

#' Imputate
#' 
#' We first cache all unique health code missing values.
#' We replace NAN/INF with NA
#' We loop once to replace health code groups with median values
#' We loop again to replace global median values
#' 
#' [TODO: address extreme outliers]
#' [TODO: add $imputed variable to dataframe 0 or 1]
#' 
#'
#' @param dframe dataframe with NA, NAN, INF, -INF values
#' @param myC columns to imputate
#'
#' @return updated dataframe
#' @export
#'

imputateDataFrame = function(dframe,myC)
{
  tstart = Sys.time();
  
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
              noise = runif(1, -0.001, 0.001);
              replacev = medianCache[[hv]][[dvn]]  + noise;
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
    
    tend = Sys.time(); timer = tend - tstart; print(timer);
    
   
    
  dframe;
}


#' Appends health record information
#'
#' @param dframe 
#'
#' @return update dframe 
#' @export
#'

appendRecordData = function(pframe,missingrecords)  # 6.6 minutes if not cached...
{
  # 3 minutes
  myO = paste(localCache,"summaryObjects","",sep="/");
  pedF = paste(myO, paste(synapseProject,"PEDOMETER-HEALTH",sep='-'), ".Rda", sep='');
  tstart = Sys.time();
  
  
  if(!file.exists(pedF))
  {
    
    
  records = rownames(pframe);
  pframe$rv = records;
  
  # let's add rows based on missingrecords (in rv format)
ro = pframe[1,];
ro[!is.na(ro)]=NA;

  for(mr in missingrecords)
  {
  tro = ro;
  tro$rv = mr;
    row.names(tro) = mr;
  
    pframe = rbind(pframe,tro);
    
  }
  

records = rownames(pframe);
  
  pframe$r = NA;
  pframe$hv = NA;
  pframe$h = NA;
  pframe$source = NA;
  pframe$healthState = pframe$isPD = pframe$gender = pframe$age = NA;
    
    
    
    #trainme$healthState[mPower$walking.training$medTimepoint=="I don't take Parkinson medications"] = 5;
    
    
    #trainme$isPD = NA; 
    
  
  
  #pframe$demographics = NA;
  
  # codeHealthState
  
  
  # row.names(audit$aframe)  ... unique health codes
  # row.names(audit$alist)   ... list of good/bad records to figure out subset of health-records...
  # audit$alist[[healthCode]]$details$goodRecords;
  
  #rlen = length(records);
  rlen = dim(pframe)[1];
  for(i in 1:rlen)
    {
    print(paste(i," of ",rlen)); flush.console();
    rv = records[i];
r = recordVariableToString(rv);
      myH = hv = audit$rclist[[rv]]$info[1];  # missing records not in my audit
h = recordVariableToString(hv,"HEALTH");

if(is.null(myH))
{
  h="xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx";  
  hv = myH = recordStringToVariable(h,"HEALTH");
}

 
    pframe$hv[i] = myH;
    
    pframe$r[i] = r;
    pframe$h[i] = h;
    
      demographics = audit$alist[[myH]]$demographics;
      if(length(demographics) > 1)
      {
        pframe$age[i] = scoreFactor(demographics,"age","age");         pframe$gender[i] = scoreFactor(demographics,"gender","gender");
      }
    
    instance = audit$rclist[[rv]]$instance;
    
    if(length(instance) > 1)
    {
      
      pframe$source = as.character(instance$dframe);
    
        
        
        pframe$healthState[i] = scoreFactor(instance$info,"medTimepoint","healthState");
        pframe$isPD[i] = scoreFactor(instance$info,"medTimepoint","isPD");
        
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

#' Get Features from Pedometer Data
#'
#' @param records list of character strings of records.  From submission template, doesn't match audit 79168 - 79137
#'
#' @return dataframe of pedometer options
#' @export
#'

getPedometerFeatures = function(records,method="string")
{
  
  # we are building for testing, so will use all 79,000 records  ## 2.2 hours to build ...
  tstart = Sys.time();
  
  
  myO = paste(localCache,"summaryObjects","",sep="/");
  pedF = paste(myO, paste(synapseProject,"PEDOMETER",sep='-'), ".Rda", sep='');
  
  if(!file.exists(pedF))
  {
  pframe = data.frame();
  
  #records = names(audit$rclist);
    rlen = length(records);
  for(i in 1:rlen)
  {
    print(paste(i," of ",rlen)); flush.console();
    #rv = records[i];
    if(method == "string")
      {
      r = records[i];
      rv = recordStringToVariable(r);
    } else {
      rv = records[i];
      r = recordVariableToString(rv);
     }
    
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
 


#' Get Features from Motion Data
#'
#' @param records list of character strings of records.  From submission template, doesn't match audit 79168 - 79137
#' @param method is the records list "string" or "variable(s)"
#' @param finalize if true, we will merge into a large object; if false, we can sample(records) and run multiple instances to speed up process
#' @param plotme if true, we build PDF images in motion-details subfolder of RECORD
#'
#' @return dataframe of motion options [we can later further manipulate]
#' @export
#'

getMotionFeatures = function(records,method="string",finalize=FALSE,plotme=TRUE)
{
  
  
  # we are building for testing, so will use all 79,000 records  ## 2.2 hours to build ...
  # we can parallelize by caching the local objects
  tstart = Sys.time();
  
  #records = sample(records);
  
  myO = paste(localCache,"summaryObjects","",sep="/");
  pedF = paste(myO, paste(synapseProject,"MOTION",sep='-'), ".Rda", sep='');
  
  if(!file.exists(pedF))
  {
    pframe = data.frame();
    
    #records = names(audit$rclist);
    rlen = length(records);
    for(i in 1:rlen)
    {
      
      #rv = records[i];
      if(method == "string")
      {
        r = records[i];
        rv = recordStringToVariable(r);
      } else {
        rv = records[i];
        r = recordVariableToString(rv);
      }
      
      print(paste(i," of ",rlen)); 
      print(rv);
      print("######################################");
      flush.console();
      
      #rvObj = getMotionObject(rv);
      
      #pfeat = getMotionFeaturesFromRecord(rv,rvObj,plotme);
      pfeat = getMotionFeaturesFromRecord(rv,plotme);
      #stop();
      if(finalize)
        {
        pframe = rbind(pframe,pfeat);
      }
      
      #Sys.sleep(5);
      
    }
    
    rownames(pframe) = records;
    
    tend = Sys.time();
    
    if(finalize)
    {
    save(pframe,file=pedF);
    }
  } else {
    
    load(pedF);
    tend = Sys.time();
  }
  
  timer = tend - tstart; 
  print(timer);
  
  
  
  pframe;
  
}





