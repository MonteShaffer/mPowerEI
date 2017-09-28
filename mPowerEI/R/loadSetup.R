

#' Load Setup 
#'
#' Loads constant variables used in package
#' 
#' @return list 'setup' is returned.
#' @export
#'
#' @examples
#' setup = loadSetup();  	# builds constants
#' str(setup);
#' 
loadSetup <- function()
  {
  
  
 setup = list();
 
 setup$dims=c("x","y","z");
 setup$keys = c("accel", "deviceMotion","pedometer");
 setup$acts = c("outbound", "return","rest");
 
 setup$designpoint = 100; # number of milliseconds per design point
 setup$pareto = 0.8 # number of elements in design-point analysis to be considered a good point
 
 
 # https://en.wikipedia.org/wiki/Earth_radius
 setup$g = setup$g_n = 9.80665; # standard gravity m/s^2
 setup$g_0 = 9.78033 # normal equatorial value for gravity m/s^2
 setup$r_e = 6371008.8 # radius of earth in meters
 
 setup$jsonlist = synapseListJSON;
 
 setup;
 
}



#' Map table data to Records (rv)
#'
#' @param mPower list of data objects
#'
#' @return lists 'recs' of data objects mapped on recordStringToVariable(RecordId)
#' @export
#'
#' @examples
#' mPower = loadSynapseData();	# loads summary info of data tables
#' recs = recordMap(mPower); # organizes data based on rv

recordMap = function(mPower)
{
  tstart = Sys.time();
  myO = paste(localCache,"summaryObjects","",sep="/");
  recsF = paste(myO, paste(synapseProject,"RECORDS",sep='-'), ".Rda", sep='');
  
  if(file.exists(recsF))
  {
    load(recsF);
    tc = length(names(recs));
    
    tend = Sys.time();
    timer = (tend - tstart);
    
    print(paste("Total Records:", tc));
    #print(paste("Time:", timer,"minutes"))
    print(timer);
    
    return(recs);  
  }
  
  
recs = list();
  for(n in names(mPower))
  {
  flen = dim(mPower[[n]])[1]; # frame length  
  print(n); flush.console(); Sys.sleep(3);
  for(f in 1:flen)
    {
    row_ = mPower[[n]][f,];
      rid = row_$recordId;
      rv = recordStringToVariable(rid);
      hid = row_$healthCode
      hv = recordStringToVariable(hid,prepend="HEALTH");
        userFolder = paste(localCache,"userObjects",hv,sep="/");
        recordFolder = paste(userFolder,rv,sep="/");
      p = recordFolder;
      
      recs[[rv]]$dframes[[n]] = list(rid=rid,rv=rv,hid=hid,hv=hv,p=p,row=row_);
      
    if(!is.null(recs[[rv]]$ref))
    {
    alen =  length(recs[[rv]]$ref); 
    recs[[rv]]$ref[1+alen] = n; 
    } else { 
                recs[[rv]]$rid = rid;
                recs[[rv]]$rv = rv;
                recs[[rv]]$hid = hid;
                recs[[rv]]$hv = hv;
                recs[[rv]]$p = p;
            recs[[rv]]$ref[1] = n; 
            }

     
      
      
      status = ("###      CCCC  of  TTTT     [ DFDFDF ]    ###");
        status = gsub("CCCC",f,status);
        status = gsub("TTTT",flen,status);
        status = gsub("DFDFDF",n,status);
      print("##################################");
      print(status)
      print("##################################");
      print(rv); 
      flush.console();
      
    }
  }
tc = length(names(recs));

tend = Sys.time();
timer = (tend - tstart);

print(paste("Total Records:", tc));
#print(paste("Time:", timer,"minutes"))
print(timer);


save(recs,file=recsF);
return(recs);

}




#' Get Complete Records
#'
#' A complete record has 8 JSON objects.  We verifiy the JSON objects are valid as well.
#' 
#' Also stores an object of the badrecords
#'
#' @param dfstring string representing a dataframe element in mPower list
#'
#' @return list of (rvs) - parseSingleRecordVariable(rv) will be evaluated to determine the JSON objects are valid
#' @export
#'
#' @examples
#' mPower = loadSynapseData();	# loads summary info of data tables
#' setup = loadSetup();  	# builds constants
#' audit = harvestAudit();
#' recs = recordMap(mPower);
getCompleteRecords = function (dfstring)
{
  myO = paste(localCache,"summaryObjects","",sep="/");
  recsB = paste(myO, paste(synapseProject,"BADJSON",dfstring,sep='-'), ".Rda", sep='');
  recsCL = paste(myO, paste(synapseProject,"COMPLETERECORDS",dfstring,sep='-'), ".Rda", sep='');
  
 clist = list();
 blist = list(); # store why bad ...
 
 
 clist;
  
}