
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
      

      json_files <- synDownloadTableColumns(templat, json);

      k=0;
      for(n in names(json_files))
      {
        k=1+k;
        f = as.character(json_files[k]);
        s = subset(data,data[[json]]==n);
        if(dim(s)[1] > 0)
          {
              r = s$recordId;
                print(r);
              rv = recordStringToVariable(r);
              records = 1 + records;
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
    }
  }
  records;
}





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




