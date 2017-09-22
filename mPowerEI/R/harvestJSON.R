
#' Harvest JSON data-motion objects from Synapse
#'
#' @param dframe data.frame with a list of \code{healthCode} s.
#' @param syntable string, tablename to do query and download
#' @param random boolean (true or false); if true, we randomize the list (allows for multiple instances)
#' @param verbose boolean (true or false); if true, we print messages to console
#'
#' @return number of total records
#' @export
#'
#' @examples
#' 
#' harvestJSON(mPower$walking.training,     'syn10146553');
#' harvestJSON(mPower$walking.testing,      'syn10733842');
#' harvestJSON(mPower$walking.supplemental, 'syn10733835');
#' harvestJSON(mPower$walking.training,     'syn10733842',random=F,verbose=F);
#' 
#' 
harvestJSON <- function(dframe,syntable,random=T,verbose=T)
{
  uniqueHealthCodes = unique(dframe$healthCode);
  
  c = 0;
  records = 0;
  total = length(uniqueHealthCodes);
  # organize by user (healthCode), then by record (recordId)
  # folder/HEALTH(healthCode)/RECORD(recordId)/
  
  if(random == T)
  {
    uniqueHealthCodes = sample(uniqueHealthCodes);
  }
  
  for(h in (uniqueHealthCodes))
  {
    if(verbose == T)
    {
    print(paste("health: ",h));   flush.console();
    c = 1+c;
    #print(h);
    
    
    print("##################################");
    status = ("###      CCCC  of  TTTT        ###   RRRR records     ###");
    status = gsub("CCCC",c,status,fixed=T);
    status = gsub("TTTT",total,status,fixed=T);
    
    status = gsub("RRRR",records,status,fixed=T);
    print(status);
    print("##################################");
    flush.console();
    }
    
    hv = recordStringToVariable(h,prepend="HEALTH");
    
    userFolder = paste(localCache,"userObjects",hv,sep="/");
    if(!dir.exists(userFolder)) { dir.create(userFolder,recursive=T); }

    
      cache = paste(userFolder,paste(syntable,".cache",sep=''),sep="/");
    
    print(cache);
    if(!file.exists(cache))
    {
      hrecords = harvestSingle(h,syntable);
      write(hrecords,cache);
    }
    else
    {				
      hrecords = as.numeric(scan(cache,quiet=T));
    }
    
    print(hrecords);
    records = records + hrecords;
    
    #prepareData(h,force=T);
    
   # if(prepare == T)
    {
      #prepareData(h,force=F);
    }
    
    
    
    #stop();
  }
  #print(jsonlist);
  records;
}






#' Harvest JSON data-motion objects from Synapse (single healthCode)
#'
#' @param h a string, the healthCode
#' @param syntable a string, the relevant table for SQL
#'
#' @return integer, the number of records
#' @export
#'
#' @examples
#' 
#' 
#' 
#' 
harvestSingle <- function(h,syntable='syn10146553')
{
  hv = recordStringToVariable(h,prepend="HEALTH");
  
  userFolder = paste(localCache,"userObjects",hv,sep="/");
  if(!dir.exists(userFolder)) { dir.create(userFolder,recursive=T); }

  records = 0;

  template = getSingleHealthCodeQuery(h,syntable);	
  data = attr(template,"values");

  if(dim(data)[1] > 0)
  {
    for(json in setup$jsonlist)
    {
      # hack workaround for synapse bug ... ## https://sagebionetworks.jira.com/browse/SYNR-1043
      if(length(data[[json]]) < 1 ) { next; }
      rNA = !is.na(data[[json]]);
      
      templa = data[rNA,];
      
      if(dim(templa)[1] < 1) { next; }
      templat = template;
        attr(templat,"values") = templa;
      
      if(h=="b80b54cc-7323-4a1e-88cf-112051223fc5")
          {
          # hack for another bug in synapse code  ## Error in cumulativeDownloadResults[[j]] : subscript out of bounds 
        listRecords = sample(unique(data$recordId))
        for(r in listRecords)
        {
          rtemplate = getSingleRecordIdQuery(h,r,syntable);	
          rdata = attr(rtemplate,"values");
          
          if(dim(rdata)[1] > 0)
          {
            for(json in setup$jsonlist)
            {
              
              if(length(rdata[[json]]) < 1 ) { next; }
              rrNA = !is.na(rdata[[json]]);
              
              rtempla = rdata[rrNA,];
              
              if(dim(rtempla)[1] < 1) { next; }
              rtemplat = rtemplate;
                attr(rtemplat,"values") = rtempla;
              
              json_files <- synDownloadTableColumns(rtemplat, json);
              
                print(rdata);
                print(json);
                print(userFolder);
                print(json_files);
              localrecords = doFileCopy(rdata,json,userFolder,json_files);
                print(paste("records:",records));
                print(paste("localrecords:",localrecords));
              
              print("alsex")
              records = localrecords + records;
              
            }
          }
          
        }
        # getSingleRecordCodeQuery
            #print(templat)
        #json_files <- synDownloadTableColumns(templat, json);
           # print(json);
            #print(names(json_files));
        #stop("monte")
        
          next;
          }
      
      json_files <- synDownloadTableColumns(templat, json);
      localrecords = doFileCopy(data,json,userFolder,json_files);
      print("monte")
      records = localrecords + records;
    }
  }
  records;
}


#' Copy Synapse Cache to Local Cache (internal private function)
#'
#' @param data 
#' @param json 
#' @param userFolder 
#' @param json_files 
#'
#' @return integer 'localrecords'
#' @export
#'

doFileCopy = function(data,json,userFolder,json_files)
  {
  
  localrecords = 0;
  k=0;
  for(n in names(json_files))
  {
    k=1+k;
    f = as.character(json_files[k]);
    s = subset(data,data[[json]]==n);
    slen = dim(s)[1];
    
    if(dim(s)[1] > 0)
    {
      r = s$recordId;
      print(r);  print(paste(n,slen,json,sep="   ----  ")); 
      print(f);
      print("##########################");
        rv = recordStringToVariable(r);
        localrecords = 1 + localrecords;
        recordFolder = paste(userFolder,rv,sep="/");
        if(!dir.exists(recordFolder)) { dir.create(recordFolder, recursive=T); }
        recordFile = paste(recordFolder,gsub(".items","",json),sep="/");
        print(recordFile);
        
        if(file.exists(f))
        {
          if(!file.exists(recordFile))
          {
            file.copy(f,recordFile);
          }
        }
    }
  }
  localrecords;
}
        
      

#' SQL query to Synapse to get Single Record
#'
#' @param h 
#' @param r 
#' @param syntable 
#'
#' @return Synapse custom 'table' template
#' @export
#'

getSingleRecordIdQuery <- function(h,r,syntable='syn10146553')
{
  
  
  hv = recordStringToVariable(h,prepend="HEALTH");
  rv = recordStringToVariable(r,prepend="RECORD");
  
  userFolder = paste(localCache,"userObjects",hv,sep="/");
  if(!dir.exists(userFolder)) { dir.create(userFolder,recursive=T); }
  
  
  
  query = paste(userFolder,paste(rv,"-query.Rda",sep=''),sep="/"); 
  if(!file.exists(query))
  {		
    sql = "SELECT * FROM TTTTT where recordId='XXX'";
    sql = gsub("XXX",r,sql);
    sql = gsub("TTTTT",syntable,sql);
    print(paste(hv," --- ",sql));
    flush.console();
    
    
    
    
    
    template <- synTableQuery(sql);
    save(template,file=query);
  }
  if(!file.exists(query))
  {
    stop("template didn't save");
  }
  # stored as template
  load(query);
  
  #print(template);
  #print(str(template));
  template;
}



#' SQL query to Synapse to get all records for Single Health Code
#'
#' @param h 
#' @param syntable 
#'
#' @return Synapse custom 'table' template
#' @export
#'


getSingleHealthCodeQuery <- function(h,syntable='syn10146553')
{
  
  hv = recordStringToVariable(h,prepend="HEALTH");
  
  userFolder = paste(localCache,"userObjects",hv,sep="/");
  if(!dir.exists(userFolder)) { dir.create(userFolder,recursive=T); }
  
  
  
  query = paste(userFolder,paste(syntable,"-query.Rda",sep=''),sep="/"); 
  if(!file.exists(query))
  {		
    sql = "SELECT * FROM TTTTT where healthCode='XXX'";
    sql = gsub("XXX",h,sql);
    sql = gsub("TTTTT",syntable,sql);
    print(paste(hv," --- ",sql));
    flush.console();
    
    
    
    
    
    template <- synTableQuery(sql);
    save(template,file=query);
  }
  if(!file.exists(query))
  {
    stop("template didn't save");
  }
  # stored as template
  load(query);
  
  #print(template);
  #print(str(template));
  template;
}


#' Perform an Audit on localCache of objects 
#'
#' Good/Bad based on completeness of records
#' Lists organize the data on different keying features for quick lookups
#' 
#' @param path string that can be relative to 'localCache' or absolute
#' @param key string used for caching a unique audit
#'
#' @return list named 'audit'
#' @export
#'
#' @examples
#' audit = harvestAudit();
harvestAudit <- function(path="userObjects",key="AUDIT")
  {
  
  tstart = Sys.time();
  myO = paste(localCache,"summaryObjects","",sep="/");
    auditF = paste(myO, paste(synapseProject,key,sep='-'), ".Rda", sep='');
    
  if(file.exists(auditF))
  {
    load(auditF);
      tc = audit$tc;
    
    tend = Sys.time();
    timer = (tend - tstart);
    
    print(paste("Total Records:", tc));
    #print(paste("Time:", timer,"minutes"))
    print(timer);
    
  return(audit);  
  }
    
  p1 = paste(localCache,path,sep="/"); # local
  p2 = paste(path); # full path
  
  if(dir.exists(p1)) { myP = p1; } else { if(dir.exists(p2)) { myP = p2; } }
     if(!exists("myP")) { stop(paste("Bad path:", path))}
  print(paste("Scanning path:",myP));
  
  aframe = data.frame(); rclist = list();
  alist = list();
    
  
    myDs = list.dirs(myP, recursive=F);
    myHs = list.dirs(myP, full.names=F, recursive=F);
  nh = length(myHs);
  tc = 0;
  for(i in 1:nh)
  {
    goodR = totalR = 0;
    cGood = cBad = c();
    # loop over healthCode in variable form [HEALTH000240d111104dd2a2d0e344c37efd68]
    myH = myHs[i];
      print(myH);  print(paste(i," OF ",nh)); flush.console();    
    myD = myDs[i];
      myRs = list.dirs(myD, full.names=F, recursive=F);
      nr = length(myRs);
      
    alist[[myH]] = list("goodRecords"=0,"totalRecords"=0,"details" = list());
    
      for(j in 1:nr)
      {
        tc = 1+tc;
        totalR = 1+totalR;
        myR = myRs[j];
        # loop over recordId in variable form [RECORDa3e54d84360e4e8a9534de188d5fa9e1]
          myDR = paste(myD,myR,"",sep="/");
        myFs = list.files(myDR, pattern = "\\.json$");
          countF = length(myFs);
        if(countF ==8)
          {
          goodR = 1+goodR;
          cGood = c(cGood,myR);
        } else {cBad = c(cBad,myR)}
            
            rc=c(myH,myR,countF);
          rclist[[myR]] = list(info=(rc),files=myFs); 
          
      }
    
    alist[[myH]]$goodRecords = goodR;
    alist[[myH]]$totalRecords = totalR;
    alist[[myH]]$details = list('goodRecords'=cGood,'badRecords'=cBad);
    
      hc = c(goodR,totalR);
      
      aframe = rbind(aframe, hc);
      
      
  }
  
  rownames(aframe) = myHs;
  colnames(aframe) = c("goodRecords","totalRecords");
  
  tend = Sys.time();
  timer = (tend - tstart);
  
  print(paste("Total Records:", tc));
  #print(paste("Time:", timer,"minutes"))
  print(timer);
  
    audit=list(aframe=aframe,rclist=rclist,alist=alist,time=list(tstart,tend,timer),tc=tc);
  # build object audit...save(template,file=query);
  save(audit,file=auditF);
  
  audit;
  }

