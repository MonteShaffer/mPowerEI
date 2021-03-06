

#' Convert Factor to Number
#'
#' 
#' @param dframerow 
#' @param myK 
#' @param myS
#'
#' @return single numeric value
#' @export
#'

scoreFactor = function(dframerow,myK,myS)
{
  myV = dframerow[[myK]];
  
  #print(myK);print(myS);print(myV);
  
  if(myS == "healthState")
  {
    if(length(myV) < 1) { return(3.333); }
    if(is.na(myV)) { return(3.333); }
    if(myV =="Immediately before Parkinson medication") { return(0); }
    if(myV =="Another time") { return(1); }
    if(myV =="Just after Parkinson medication (at your best)") { return(2); }
    if(myV =="I don't take Parkinson medications") { return(5); }
    
    return(3);
  } else if(myS == "isPD")
  {
    if(length(myV) < 1) { return(0.666); }
    if(is.na(myV)) { return(0.666); }
    if(myV =="Immediately before Parkinson medication") { return(1); }
    if(myV =="Another time") { return(1); }
    if(myV =="Just after Parkinson medication (at your best)") { return(1); }
    if(myV =="I don't take Parkinson medications") { return(0); }
    
    return(0.5);
  }  else if(myS == "age")
  {
    if(length(myV) < 1) { return(33.333); }
    #if(is.na(myV)) { return(33.333); }
    
    myV = as.numeric(myV);
    if(is.na(myV)) { return(33.222); }
   
    return(myV);
  }  
  else if(myS == "gender")
  {
    if(length(myV) < 1) { return(0.5005); }
    if(is.na(myV)) { return(0.5115); }
    if(myV =="Male") { return(1); }
    if(myV =="Female") { return(0); }
    
    
    return(0.5);
  }   else { } 
  
}

#' Build variable $healthState = 0,1,2, 3,  5 and $isPD = 0, 0.5, 1  where 0.5 and 3 are NA.
#'
#' @param dframe 
#'
#' @return updated dframe with $healthState and $isPD
#' @export
#'
#' @examples trainme = codeHealthState(mPower$walking.training);
codeHealthState = function(dframe)
{
  trainme = dframe;
  trainme$healthState = NA; 
  trainme$healthState[mPower$walking.training$medTimepoint=="Immediately before Parkinson medication"] = 0;
  
  trainme$healthState[mPower$walking.training$medTimepoint=="Another time"] = 1;
  
  
  trainme$healthState[mPower$walking.training$medTimepoint=="Just after Parkinson medication (at your best)"] = 2;
  
  trainme$healthState[mPower$walking.training$medTimepoint==""] = 3;
  trainme$healthState[is.na(mPower$walking.training$medTimepoint)] = 3;
  
  trainme$healthState[mPower$walking.training$medTimepoint=="I don't take Parkinson medications"] = 5;
  
  
  trainme$isPD = NA; 
  trainme$isPD[mPower$walking.training$medTimepoint=="Immediately before Parkinson medication"] = 1;
  
  trainme$isPD[mPower$walking.training$medTimepoint=="Another time"] = 1;
  
  
  trainme$isPD[mPower$walking.training$medTimepoint=="Just after Parkinson medication (at your best)"] = 1;
  
  trainme$isPD[mPower$walking.training$medTimepoint==""] = 0.5;
  trainme$isPD[is.na(mPower$walking.training$medTimepoint)] = 0.5;
  
  trainme$isPD[mPower$walking.training$medTimepoint=="I don't take Parkinson medications"] = 0;
  
  trainme;
}







#' Select Best Features [Step-wise] to maximize ROC
#'
#' Hierarchy:  pick single feature with highest ROC, include in model; include second-best feature (with first feature) with highest ROC, include in model; and so on.
#' 
#' We stop when the next feature to be added lowers the overall ROC.
#' 
#' setup multi-cores before call, if possible
#' library(doMC);
#' registerDoMC(cores = 16);
#' 
#' nested loop with stop
#' find first feature with highest roc, place that in the system, loop by adding all remaining features, add one at a time until ROC isn't improving...
#' default iCut
#' 
#' dframe = pfeats; xfeats=1:20; rnum=22;
#' 
#' @param dframe dataframe
#' @param xfeats columns with features
#' @param rnum number, column index or $r
#' @param fastignore if true, we will drop any negative ROC deltas
#' @param ignoreme  if fastignore=true, these are the values to drop default 0,0 ... the first is for the first pass, the latter is for the second pass.  by default negative ROC deltas
#' @param startfeats a numeric vector to start... startfeats = c(151,13,76) ... this would predefine the first set of features, enables restarts ... [we have also updated the Rerror.txt to cache individual 'unique' ensemble calls]  
#'
#' @return list of results, roc.nest is the xfeats in order that matter
#' @export 
#' 
#' 
stepwiseFeatureSelection = function(dframe,xfeats,rnum,fastignore=TRUE,ignoreme=c(0,0),startfeats=NULL)
{
  
  
  
  
  tstart = Sys.time();
  tpfeats = dampenOutliers(dframe,xfeats); # default iCut
  
  # benchmark
  nobs = 275 + 178
  tobs = 275;
  bench = tobs / nobs;  # 0.607064
  timers = list();
  
  
  roc.gold = 0.91734137545722882;
  roc.rndm = bench;
  roc.previousloop = 0;
  roc.continue = TRUE;
  roc.current = roc.previous = 0;
  roc.nest = NULL;
  roc.list = xfeats;
  roc.names = NULL;
    i=0;
    for(xfeat in xfeats)
      {
      i=i+1;
      roc.names[xfeat] = names(tpfeats)[roc.list[i]]
      }
    
  roc.maxs = NULL;
  rocsInner = list();
  roc.ignore = NULL;
  
  if(is.null(startfeats))
  {
  rocs = NULL;
  
  roc.loop = 1;  roc.index = paste("X",roc.loop,sep='');
    tstartOuter = Sys.time();
  for(xfeat in roc.list)
  {
    tstartInner = Sys.time();
    print(paste("######################",xfeat,"######################"));
print(paste("######################",names(tpfeats)[xfeat],"######################"));
    # outer loop sets baseline	
    tpfeat = tpfeats[,c(rnum,xfeat)]; 
    resultme = NULL;
    #resultme = synapse.PD_score_challenge1(tpfeat); # namespace conflict
    resultme = PD_score_challenge1(tpfeat); # namespace conflict
    rocs[xfeat] = resultme$error$ROC;
    status = paste(rocs[xfeat]," :: ", rocs[xfeat] - roc.rndm);
    print(paste("######################",status,"######################"));
    tendInner = Sys.time(); timerInner = tendInner - tstartInner; print(timerInner);
      timers[[roc.index]][[xfeat]] = list(timer = timerInner, units = attr(timerInner,"units") )
  }
    tendOuter = Sys.time(); timerOuter = tendOuter - tstartOuter; print(timerOuter);
      timers[[roc.index]]$outer = list(timer = timerOuter, units = attr(timerOuter,"units") )
  rocs;
  # determine roc.nest first value;
  names(rocs) = roc.names;

  
  print("##  ############   first pass    ##################");	  
  
  rocsInner[[roc.index]] = rocs;
  
  roc.sort = order(-rocs);	
  roc.nest = c(roc.nest,roc.sort[1]);	
  roc.maxs = c(roc.maxs, as.numeric(rocs[roc.sort[1]]) );
  roc.current = roc.previous = as.numeric(rocs[roc.sort[1]]);	# max is first element
  
  
  
  # tfeat = # truncate features for only positive ROCS
  # maybe do this at every stage ... this will make hierarchy run faster ...
  compareto = roc.rndm;
  roc.diff = rocs - compareto;
  
  roc.ignore.1 = roc.ignore = c(as.numeric(roc.nest),as.numeric(which(roc.diff < ignoreme[1]))); # if zero, we may lose one e.g., #20 from pedometer ... interaction effects.
    # was -0.005
  
  if(fastignore==T){
    roc.remaining = setdiff(roc.list,roc.ignore);
  }else{
    roc.remaining = setdiff(roc.list,as.numeric(roc.nest));
  }
  
  #
  } else {
    # we have starting features, so let's get to it... # allows us to restart at a certain point ... 
    roc.nest = as.numeric(startfeats);
    roc.loop = length(startfeats)-1; # we will immediately iterate to the next one
    roc.index = paste("X",roc.loop,sep='');
    roc.remaining = setdiff(roc.list,roc.nest);
    
    
    
    
    
    # we don't have a roc.current/previous so the first inner loop will not truncate well
    #stop();
    tpfeat = tpfeats[,c(rnum,roc.nest)]; 
    resultme = NULL;
    resultme = PD_score_challenge1(tpfeat); # namespace conflict
    
    roc.current = roc.previous = roc.previousloop = resultme$error$ROC;
    
    
  }
  
  
  print(roc.remaining);
  
 # myList = list(gold=roc.gold,rndm=roc.rndm,current=roc.current,previous=roc.previous,nest=roc.nest,list=roc.list,names=roc.names,maxs=roc.maxs,deltas=c(roc.maxs[1],diff(roc.maxs)),details=rocsInner,timers=timers);
  
 # data.table::fwrite(myList,file="bigrun.txt");
  
  print(paste("######################",roc.loop,"######################"));
  
  
  
  # next loop
  roc.loop = roc.loop + 1; roc.index = paste("X",roc.loop,sep='');	
  
  # 19                4               15               20 
  
  while(roc.continue==T)
  {
    print(paste("######################",roc.loop,"######################"));
    rocs = NULL;
    xfeattemp = roc.remaining;
    xlentemp = length(xfeattemp);
    tstartOuter = Sys.time();
    for(i in 1:xlentemp)
    { 
      tstartInner = Sys.time();
      xfeat = xfeattemp[i]; if(is.na(xfeat)){ break; }
      xfeattemplist = c(roc.nest,xfeat);
      print(paste("######################",xfeat,"######################"));
print(paste("######################",names(tpfeats)[xfeat],"######################"));
      print(paste(i," of ",xlentemp)); flush.console();  
      print( c(rnum,xfeattemplist) );
      print(paste("######################",xfeat,"######################"));
     
      tpfeat = tpfeats[,c(rnum,xfeattemplist)];
      
      resultme = NULL;
      #resultme = synapse.PD_score_challenge1(tpfeat);
      resultme = PD_score_challenge1(tpfeat);
      
      rocs[xfeat] = resultme$error$ROC;
      status = paste(rocs[xfeat]," :: ", rocs[xfeat] - roc.rndm);
      print(paste("######################",status,"######################"));
      status = paste(rocs[xfeat]," :: ", rocs[xfeat] - roc.previousloop);
      print(paste("######################",status,"######################"));
      
      
      tendInner = Sys.time(); timerInner = tendInner - tstartInner; print(timerInner);
      timers[[roc.index]][[xfeat]] = list(timer = timerInner, units = attr(timerInner,"units") )
    }  # end of for loop
    
    tendOuter = Sys.time(); timerOuter = tendOuter - tstartOuter; print(timerOuter);
    timers[[roc.index]]$outer = list(timer = timerOuter, units = attr(timerOuter,"units") )
    
    
    #stop();
    
    # names(rocs) = roc.names[roc.remaining];
    
    if(!is.null(rocs))
    {
    rocsInner[[roc.index]] = rocs;
    
    roc.sort = order(-rocs);  # NAs?	properly sort
    
    roc.current = as.numeric(rocs[roc.sort[1]]);
    }  else { break; }
    
    
    
    
    
            if(roc.previous > roc.current) 
            { 
              roc.continue = F;
              print(roc.nest);
              print(roc.names[roc.nest]);
            } else {
              
              
              # tfeat = # truncate features for only positive ROCS
              # maybe do this at every stage ... this will make hierarchy run faster ... 
        
              compareto = roc.current;
              roc.diff = rocs - compareto;
              
              
              
              # 19                4               15               20 
              
              roc.maxs = c(roc.maxs, as.numeric(rocs[roc.sort[1]]) );
              roc.nest = c(roc.nest,roc.sort[1]);	# this was max ... 
              #roc.remaining = setdiff(roc.list,as.numeric(roc.nest));
              
              roc.ignore = unique(c(as.numeric(roc.nest),roc.ignore,as.numeric(which(roc.diff < ignoreme[2])))); # if zero, we may lose one e.g., #20 from pedometer # as we get deeper, let's make the cut more relevant ... interaction effects.
              # was -0.05
              
              if(fastignore==T){
                roc.remaining = setdiff(roc.list,roc.ignore);
              }else{
                roc.remaining = setdiff(roc.list,as.numeric(roc.nest));
              }
              
              
              print(roc.remaining);
              
              # next loop
              roc.loop = roc.loop + 1;	roc.index = paste("X",roc.loop,sep='');
              roc.previous = roc.current;
            } # end continue
    
    
    
   # roc.previousloop = which(roc.maxs[tail(roc.maxs,n=1)]); 
    roc.previousloop = tail(roc.maxs,n=1); 
    
    
    
    
    
    
  } # end while
  
  
  
  
  
  
  
  
  
  
  
  
  tend = Sys.time();
  timerTotal = tend - tstart;  print(timerTotal);
    timers$total = list(timer = timerTotal, units = attr(timerTotal,"units") )
  
  
  
  names(roc.nest) = roc.names[as.numeric(roc.nest)];
  
  
  
  
  
  myList = list(gold=roc.gold,rndm=roc.rndm,current=roc.current,previous=roc.previous,nest=roc.nest,list=roc.list,names=roc.names,maxs=roc.maxs,deltas=c(roc.maxs[1],diff(roc.maxs)),details=rocsInner,timers=timers);
  
  
  
  
  
  
  myList;
  
} 




#' Dampen outliers
#'
#' @param dframe 
#' @param xfeats numeric vector, list of columns to be evaluated
#' @param iCut number, number of IQRs to use to truncate min/max
#'
#' @return updated dframe
#' @export
#'

dampenOutliers = function(dframe,xfeats,iCut=6)
{
  # iCut is number of IQR to truncate to
  pmed = plyr::colwise(median)(dframe[,xfeats]);  
  pmean = plyr::colwise(mean)(dframe[,xfeats]);
  psd = plyr::colwise(sd)(dframe[,xfeats]); 
  pmin = plyr::colwise(min)(dframe[,xfeats]); 
  pmax = plyr::colwise(max)(dframe[,xfeats]); 
  piqr = plyr::colwise(IQR)(dframe[,xfeats]);  
  
  pcutMin = pmed - iCut*piqr;
  pcutMax = pmed + iCut*piqr;
 
  rframe = dframe;
  xlen = length(xfeats);
  xnames = names(dframe[,xfeats]);
  for(i in 1:xlen)
    {
    xname = xnames[i];
      xval = as.numeric(pcutMin[xname]);
    xidx = xfeats[i];
    
    rframe[[xname]][ rframe[[xname]] < xval  ] = xval; 
      xval = as.numeric(pcutMax[xname]);
    rframe[[xname]][ rframe[[xname]] > xval  ] = xval; 
    
    
    #rframe[,xidx] = ifelse(x < xval, xval, x);
    #rframe[,xidx] = ifelse(x > xval, xval, x);
    }
  
  
  rmed = plyr::colwise(median)(rframe[,xfeats]);  
  rmin = plyr::colwise(min)(rframe[,xfeats]); 
  rmax = plyr::colwise(max)(rframe[,xfeats]); 
  
rframe;  
}


#' Internal Test of Greedy Ensemble with Predictions and Accuracy
#'
#' @param submitme the dataframe with submission, not entire synapseSubmission, $r and $h are required.
#' @param split number, 1-99, what percent of the data will be sampled into train vs test set (e.g., 80 means 80 into train, 20 into 'out of sample' test)
#' @param dv string, the name of the column that will be the dv
#' @param dvv numeric vector, the values of the DV that will be classified, first = 0, second = 5
#' @param dvf string vector, the factors of the DV that will be classified, first = 'PD.off', second = 'nonPD'
#' @param xfeats numeric vector, the column numbers that define the features
#' @param dreplace string, how do we replace duplicates in analysis set, "first" is just the first value, "median" is the median of all values, "none" skips this step...
#'
#' @return list of items from caret, model = , ensemble = , predict = , accuracy = , control = 
#' @export
#'

mPower_test<-function(submitme, dv="healthState", dvv = c(0,5), dvf=c("PD.off","nonPD"), xfeats=1:20, split=80, dreplace="none")
  {
  timerx = NULL; timeru = NULL; timeri = NULL;
  
  tstart = Sys.time();
  # submitme has some noise applied to imputate, so if we re-imputate, we will get slightly different values ...
  # this function samples based on 'split' so a given run will have slightly different values
  # $r and $h are for recordId and healthCode
  
  play = submitme;
  
  # subset based on dvv
  psets = NULL;
  for(dva in dvv)
    {
    print(dva);
    pset = subset(play, play[[dv]] == dva);
    psets = rbind(psets,pset);
    }
  
  play = psets;
  #set.seed(9876); # if you want repeatable
  play = play[sample(nrow(play)),];  # sample by row ... randomize
  
  myC = c();
    myC = c(myC, which(colnames(play)=="h"));
    myC = c(myC, which(colnames(play)=="r"));
    myC = c(myC, which(colnames(play)==dv));
    myC = c(myC, xfeats);
  
  xlen = length(xfeats);
  xpos = 4:(3+xlen);
  
  playN = dim(play)[1];
  playS = floor(split/100 * playN);
  
  ptrain = play[1:playS,myC];           # 14340
  ptest  = play[(1+playS):playN,myC];   #  3585  [0.25]
  
  tend = Sys.time(); timer = tend - tstart; print(timer); flush.console();
    timerx = c(timerx,as.character(timer));
    timeru = c(timeru, attr(timer,"units") );
    timeri = c(timeri, "init");
  
  
  
  tstart = Sys.time();
  if(dreplace != "none")
  {
  # replace duplicates with "median" or merely "first value"
    ptrain = replaceDuplicates(ptrain,xpos,dreplace); # 2354
    ptest = replaceDuplicates(ptest,xpos,dreplace);   # 1218   [0.52]
  }
  tend = Sys.time(); timer = tend - tstart; print(timer); flush.console();
 
    timerx = c(timerx,as.character(timer));
    timeru = c(timeru, attr(timer,"units") );
    timeri = c(timeri, "duplicates");
  
  
  tstart = Sys.time();
  ## fit model
  model.dv = ptrain[,3];
    model.dvf = factor(model.dv,levels=dvv,labels=dvf);
    model.features = ptrain[,xpos];
    model.features.names = names(model.features);
  
  
  print("Run Control"); flush.console(); 
  set.seed(1234)
  model.control <- trainControl(  method="boot",
                              number = 50,
                              savePredictions="final",
                              classProbs=TRUE,
                              index=createResample(model.dvf, 50),
                              summaryFunction=twoClassSummary );
  
  tmpM = model.matrix( ~ ., model.features);  # useful if we have factors - factors to dummies
                                              # we are not allowed to include factors, so?
                                              # [,-1] removes the intercept
  
  print(dim(model.features));
  print(length(model.dv));
  print(table(model.dvf));
  
  tend = Sys.time(); timer = tend - tstart; print(timer);
  
  timerx = c(timerx,as.character(timer));
  timeru = c(timeru, attr(timer,"units") );
  timeri = c(timeri, "control");
  
  
  
  
  tstart = Sys.time();
  print("Caret Call"); flush.console();
  model.list <- caretList(
    y=model.dvf , x=model.features,
    trControl=model.control,
    metric = "ROC",
    tuneList=list(glmnet = caretModelSpec(method = "glmnet"), 
                  rf = caretModelSpec(method="rf"), # random forest
                  svmLinear = caretModelSpec(method="svmLinear"), 
                  knn = caretModelSpec(method="knn"), 
                  nnet = caretModelSpec(method = "nnet", trace=F)),
    continue_on_fail = FALSE, preProc = c("center", "scale")
  )
  
  
  tend = Sys.time(); timer = tend - tstart; print(timer);
  
  timerx = c(timerx,as.character(timer));
  timeru = c(timeru, attr(timer,"units") );
  timeri = c(timeri, "caret");
  
  
  
  tstart = Sys.time();
  
  print("Ensemble run"); flush.console();
  greedy_ensemble <- caretEnsemble(
    model.list, 
    metric="ROC",
    trControl=trainControl(
      number=length(model.list),
      summaryFunction=twoClassSummary,
      classProbs=TRUE
    ))
  
  
  
  
  tend = Sys.time(); timer = tend - tstart; print(timer);
  
  timerx = c(timerx,as.character(timer));
  timeru = c(timeru, attr(timer,"units") );
  timeri = c(timeri, "ensemble");
  
  
  
  
  
  }




#' Replace 'healthCode' duplicates in dataframe
#'
#' @param dframe dataframe
#' @param xpos positions of xfeats that need to be analyzed
#' @param dreplace string, how to replace with "first" value or "median"
#'
#' @return updated dataframe without replicates
#' @export
#'

replaceDuplicates = function(dframe, xpos, dreplace="first")
{
  
  rframe = NULL;
  myHs = NULL;
  # stack approach, we are not going to adjust order .. not efficient
  dlen = dim(dframe)[1];
  
  for(i in 1:dlen)
  {
  #print(paste(i," of ",dlen)); flush.console();  
    drow = dframe[i,];
    dh = as.character(drow$h);
  #print(dh);
    # index 3 is the dv
    dv = as.numeric(drow[3]);
    
    if( length( which(myHs == dh) ) < 1 )
    {
      # not found
      if(dreplace=="first") {
        dreplacerow = drow;
      } else if(dreplace=="median") {
        dsub = subset(dframe,dframe$h==dh);
          dmed = plyr::colwise(median)(dsub[,c(3,xpos)]);  # median of state?
        dreplacerow = drow;
        dreplacerow[xpos] = dmed[-1];
      }
      
    
    rframe = rbind(rframe,dreplacerow);    
    myHs = c(myHs,dh); # added to stack
    }
      
    
  }
  
  
  
  rframe;
  
}



