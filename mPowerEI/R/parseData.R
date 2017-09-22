parseAllRecordData = function(verbose=TRUE,force=FALSE) 
{
  # assumes audit has run ... 
  # audit = harvestAudit(); # builds/caches list objects of health/record 
  rclist = names(audit$rclist);
  rclist = sample(rclist); # randomize
    rclen = length(rclist);
  for(i in 1:rclen)
      {
      rv = rclist[i];
      status = ("###      CCCC  of  TTTT         ###");
          status = gsub("CCCC",i,statusi);
          status = gsub("TTTT",rclen,statusi);
          
      if(verbose==T)
      {
        print("##################################");
        print(status)
        print("##################################");
        print(rv); 
        print("##################################");
        flush.console();
      }
          parseSingleRecordVariable(rv);  # return nothing, just build...
      
      
      
       
      }
}



parseSingleRecordVariable = function(rv,force=FALSE) 
{
  # timer?
  # rv = "RECORD28832018b9c849e79d67f201d99eb8b6";
  r = recordVariableToString(rv);
    rinfo=audit$rclist[[rv]];
      hv = rinfo$info[1];
      h = recordVariableToString(rv,prepend="HEALTH");
      
    userFolder = paste(localCache,"userObjects",hv,sep="/");
    recordFolder = paste(userFolder,rv,sep="/");
    
      rawF = "rawData.Rda";
        rawFile = paste(recordFolder,rawF,sep="/");
     
        
    if(!file.exists(rawFile) | force==T)
        {
          rlist = initializeRecordRawList(); 
          tlist = initializeRecordTimeList();
          ilist = list(); # internal list of function calls/stacks
        jsons = rinfo$files;
        
        # raw iteration
        for(j in jsons)
          {
            js = unlist(strsplit(j,".js"));
            if(!is.na(js[2]))
            {
              if(js[2] == "on")
              {
                s = unlist(strsplit(j,"_"));
                key = s[1];
                act = gsub(".json","",s[3]);
                f = paste(recordFolder,j,sep="/");
                jsonstring <- readr::read_file(f);
                  # jsonstring = gsub("\\]\\[",",",jsonstring);
                
                temp = jsonlite::fromJSON(jsonstring);
                
                rlist[[act]][[key]] = temp;  # original form
                
                
                # let's figure out tlist [timestamped]
                
                if(key=="pedometer")
                {
                  ptemp = getSecondsFromPedometerTime(temp);
                  stop("pedometer");
                  
                  #print(temp);
                  
                  if(!is.null(ptemp))
                  {
                    
                    ptemp = ptemp[order(ptemp$endU),];
                    
                    tlist[[act]][[key]] = ptemp;
                  }
                  
                }
                if(key=="deviceMotion")
                {
                  
                  dtemp = collapseTimeStampDeviceMotion(temp);
                  
                  #stop("deviceMotion");
                  
                  if(!is.null(dtemp))
                  {
                    
                    dtemp = dtemp[order(dtemp$timestamp),];
                    
                    tlist[[act]][[key]] = dtemp;
                  }
                }
                if(key=="accel")
                {
                  # 
                  
                  temp = temp[c("timestamp", "x", "y", "z")];
                  
                  #stop("accel");
                  
                  if(!is.null(temp))
                  {
                    temp = temp[order(temp$timestamp),];
                    
                    tlist[[act]][[key]] = temp;
                  }
                }
                
                
              }
            }
          }
        
        # end raw iteration
        rawObj = list(r=r,h=h,rv=rv,hv=hv,files=jsons,
                        rlist=rlist,tlist=tlist,ilist=ilist);
        
        save(rawObj,file=rawFile);
      }
      load(rawFile);
      raw = rawObj;
        
      motionF = paste(setup$designpoint,"motionData.Rda",sep="-");
      motionFile = paste(recordFolder,motionF,sep="/");
        
      
      if(!file.exists(motionFile) | force==T)
      {
        
        # design points
          
        slist = scaleToTimeIncrement(tlist,setup$designpoint);
        mlist = mergeListsAccelDeviceMotion(slist);
        olist = orientToGravity(mlist);
        ilist = list(); # internal list of function calls/stacks
        
        morder = determineOrder(mlist);  
        
        
        # end motion iteration
        motionObj = list(slist=slist,mlist=mlist,
                         olist=olist,morder=morder,ilist=olist);
        
        save(motionObj,file=motionFile);
      }
      load(motionFile);
      motion = motionObj;
        
        
        
      
     list(raw=raw,motion=motion); 
}




determineOrder = function(mlist)
{
  morder = data.frame();
  
  acts = c("outbound","rest","return");
  values = list();
  cvec = c(); # c("outbound","rest","return");
  
  
  for(act in acts)
  {
    if(length( mlist[[act]]$timestamp ) > 0)
    {
      val = range(mlist[[act]]$timestamp);
      morder = rbind(morder, c(val[1],val[2],val[2]-val[1]) );  cvec = c(act,cvec);			
    }
  }
  colnames(morder)=c("min","max","time");
  rownames(morder)=cvec;
  
  morder = morder[order(morder$min),];
  
  morder;
  
}





orientToGravity = function(mlist)
{
  # return timestamp, x, y, z
  gravityDown = c(0,0,-1);
  
  olist = list();
  for(n in names(mlist))
  {
    # use this to compute initial angle in iteration ... can't be "null" (0,0,0)
    ovector = as.numeric(mlist[[n]][1,2:4]);
    olist[[n]] = data.frame();
    
    for(i in 1:dim(mlist[[n]])[1])
    {
      gravity = as.numeric(mlist[[n]][i,16:18]);
      rotationMatrix = getRotationMatrix(gravity,gravityDown);
      
      vraw = as.numeric(mlist[[n]][i,2:4]);
      vrotated = as.vector(vraw%*%rotationMatrix);
      
      oangle = computeAngle(ovector,vrotated,"degrees");
      
      
      
      olist[[n]] = rbind(olist[[n]], c(as.numeric(mlist[[n]][i,1]), vrotated, oangle) );
      
      ovector = vrotated; # next iteration
      
    }
    
    colnames(olist[[n]]) = c("timestamp","x","y","z","angle");
  }
  olist;
  
}





## https://www.macrumors.com/2016/06/23/apple-researchkit-hire-stephen-friend/
mergeListsAccelDeviceMotion = function(slist)
{
  mlist = list();
  for(act in names(slist))
  {
    accel = slist[[act]]$accel;
    
    if(dim(accel)[1] < 1) { next; }
    
    deviceMotion = slist[[act]]$deviceMotion;
    
    if(dim(deviceMotion)[1] < 1) { next; }
    
    
    merged = merge(accel,deviceMotion,by="timestamp");
    
    amax = max(accel$points);
    dmax = max(deviceMotion$points);
    cut = floor( setup$pareto * min(amax,dmax) ); # greater than this number
    
    # assume this just happens at ends, not in middle, that the units are unchanged (deltat is constant)
    merged = subset(merged,points.x > cut);
    merged = subset(merged,points.y > cut);
    
    mlist[[act]] = merged;
  }
  mlist;
}





rebuildTimeIncrement = function(temp,increment)
{
  # increment in milliseconds
  nrows = dim(temp)[1];
  ncols = dim(temp)[2];
  
  ntemp = data.frame();
  if(length(nrows) < 1) { return(ntemp); }
  cnames = c(colnames(temp),"points");
  temp$points = NA;
  # first element floor
  i=1;
  row = temp[i, ];
  t = as.numeric(row[1]);
  # multiply timestamp by 1000 to get milliseconds
  ms = floor(t * 1000);
  step = 1000 / increment;
  mod = ms %% increment;
  nms = ms - mod;
  
  nrow = row;
  nrow$points = 1;
  nrow[1] = nms; # set to this initial marker
  ntemp = rbind(ntemp,nrow);
  
  nstop = nms + increment;
  # hopefully it hits next stop
  
  stack = numeric();  # keep stack of indexes
  k=0;
  
  for(i in 2:nrows)
  {
    row=temp[i,];
    t = as.numeric(row[1]);
    ms = t * 1000;
    
    if(ms < nstop)
    {
      k=1+k;
      stack[k] = i;
    }
    else
    {
      nrow = row;			
      nrow[1] = nstop;
      sub = temp[stack,2:ncols];
      med = plyr::colwise(median)(sub);
      nrow[2:ncols] = med;
      
      nrow$points = length(stack);
      
      ntemp = rbind(ntemp,nrow);
      
      stack = numeric();  # keep stack of indexes
      k=0;
      nstop = nstop + increment;
    }
    
    #if(i>25){stop();}
  }
  
  ntemp;
  
}





scaleToTimeIncrement = function(tlist,increment)
{
  slist = list();
  
  for(act in names(tlist))
  {
    for(key in names(tlist[[act]]))
    {
      if(key=="pedometer")
      {
        #stop();
      }
      if(key=="deviceMotion")
      {
        temp = tlist[[act]][[key]];
        ntemp = rebuildTimeIncrement(temp,increment);
        
        slist[[act]][[key]] = ntemp;
        #stop();
      }
      if(key=="accel")
      {
        temp = tlist[[act]][[key]];
        ntemp = rebuildTimeIncrement(temp,increment);
        
        slist[[act]][[key]] = ntemp;
        #stop();
      }
    }
    
    
  }
  
  slist;
}









initializeRecordRawList = function()
{
  rlist = list();
  rlist$outbound = list();
  rlist$outbound$accel = list();
  rlist$outbound$deviceMotion = list();
  rlist$outbound$pedometer = list();
  rlist$return = list();
  rlist$return$accel = list();
  rlist$return$deviceMotion = list();
  rlist$return$pedometer = list();
  rlist$rest = list();
  rlist$rest$accel = list();
  rlist$rest$deviceMotion = list();
  rlist$rest$pedometer = list();
  
  rlist;
}

initializeRecordTimeList = function()
{
  tlist = list();
  tlist$outbound = list();
  tlist$outbound$accel = list();
  tlist$outbound$deviceMotion = list();
  tlist$outbound$pedometer = list();
  tlist$return = list();
  tlist$return$accel = list();
  tlist$return$deviceMotion = list();
  tlist$return$pedometer = list();
  tlist$rest = list();
  tlist$rest$accel = list();
  tlist$rest$deviceMotion = list();
  tlist$rest$pedometer = list();
  
  tlist;
}



getSecondsFromPedometerTime = function(precords)
{
  precords$startU = precords$endU = precords$diffU = precords$stepsPerSecond = precords$distancePerSecond = precords$deltaTime = precords$deltaSteps = precords$deltaDistance =  precords$deltaStepsPerSecond = precords$deltaDistancePerSecond =NA;
  
  if(is.null(precords))
  {
    return(NULL);
  }
  
  print(paste("DIM:",dim(precords)[1]));
  
  # https://stackoverflow.com/questions/1962278/dealing-with-timestamps-in-r
  csteps = cdist = ctime = 0;
  
  if(is.null(dim(precords)[1]))
  {
    return(NULL);
  }
  
  for(p in 1:dim(precords)[1])
  {
    precord = precords[p,];
      start = as.numeric(as.POSIXct(gsub("T"," ",precord$startDate)));
    precords$startU[p] = start;
      end = as.numeric(as.POSIXct(gsub("T"," ",precord$endDate)));
    precords$endU[p] = end;
      time = end - start;
    precords$diffU[p] = time;
      steps = precord$numberOfSteps;		
      dist = precord$distance;
    dsteps = steps - csteps;
    ddist = dist - cdist;
    precords$deltaSteps[p] = dsteps;
    precords$deltaDistance[p] = ddist;
      dtime = time - ctime;
    precords$deltaTime[p] = dtime;
    precords$distancePerSecond[p] = dist/time;
    precords$stepsPerSecond[p] = steps/time;
    precords$deltaDistancePerSecond[p] = ddist/dtime;
    precords$deltaStepsPerSecond[p] = dsteps/dtime;
      csteps = steps;
      cdist = dist;
      ctime = time;
  }
  precords;
}



collapseTimeStampDeviceMotion = function(drecords)
{
  dframe <- data.frame();
  if(is.null(drecords))
  {
    return(NULL);
  }
  if(is.null(dim(drecords)[1]))
  {
    return(NULL);
  }
  for(d in 1:dim(drecords)[1])
  {
    drecord = drecords[d,];
    
    row = c(drecord$timestamp, drecord$attitude$x, drecord$attitude$y, drecord$attitude$z, drecord$attitude$w,  drecord$rotationRate$x, drecord$rotationRate$y, drecord$rotationRate$z,  drecord$userAcceleration$x, drecord$userAcceleration$y, drecord$userAcceleration$z, drecord$gravity$x, drecord$gravity$y, drecord$gravity$z);
    
    dframe = rbind(dframe, row);
  }
  
  colnames(dframe) = c("timestamp","dax","day","daz","daw","drx","dry","drz","dux","duy","duz","dgx","dgy","dgz");
  
  dframe;
}



