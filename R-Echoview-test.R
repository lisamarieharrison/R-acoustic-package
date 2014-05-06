library(RDCOMClient)

#create an Echoview object and open Echoview
EVAppObj=COMCreate('EchoviewCom.EvApplication')


#read in an Echoview file
EVFile= EVOpenFile(EVAppObj, fileName = "C:/Users/Lisa/Documents/phd/southern ocean/BROKE-West raw data/Echoview/test/Transect01-07_final.ev")$EVFile

#create fileset name
EVCreateFileset(EVFile, filesetName = "EVtest")

#function to add calibration file to fileset
EVAddCalibrationFile <- function(EVFile, filesetName, calibrationFile){
  
  destination.fileset = EVFindFilesetByName(EVFile, filesetName)$fileset
  destination.fileset$SetCalibrationFile(calibrationFile)  
  
  msg=paste(Sys.time(),' : Adding ', calibrationFile,' to fileset name ',filesetName,sep='')
  message(msg)
}

EVAddCalibrationFile(EVFile, "38H-120H-200H", "C:/Users/Lisa/Documents/phd/southern ocean/BROKE-West raw data/Echoview/test/SimradEK60DSRII2010.ecs")


#function to clear all files from fileset

EVClearRawData=function(EVFile,filesetName)
{
  destination.fileset=EVFindFilesetByName(EVFile,filesetName)$filesetObj
  
  nbr.of.raw.in.fileset=destination.fileset[["DataFiles"]]$Count()
     
  #remove files
  msgV=paste(Sys.time(),' : Removing data files from EV file ',sep='')
  message(msgV)
  
  while(nbr.of.raw.in.fileset > 0){
    dataFiles <- destination.fileset[["DataFiles"]]$Item(0)$FileName()
    
    rmfile <- destination.fileset[["DataFiles"]]$Item(0)
    destination.fileset[["DataFiles"]]$Remove(rmfile) 
    nbr.of.raw.in.fileset=destination.fileset[["DataFiles"]]$Count()
    
    msg=paste(Sys.time(),' : Removing ', basename(dataFiles),' from fileset name ',filesetName,sep='')
    message(msg)
    msgV=c(msgV,msg)
  }
}

#clear raw data from fileset
EVClearRawData(EVFile, "38H-120H-200H")

#read names of .raw files in a Fileset
EVFilesInFileset = function(EVFile, filesetName){
  
  fileset.loc = EVFindFilesetByName(EVFile, filesetName)$filesetObj
  nbr.of.raw.in.fileset.pre = fileset.loc[["DataFiles"]]$Count()
  
  raw.names <- 0
  for(i in 0:(nbr.of.raw.in.fileset.pre - 1)){
    raw.names[i + 1] <- basename(fileset.loc[["DataFiles"]]$Item(i)$FileName())
  }
  
  msg = paste(Sys.time(),' : Returned names for ', nbr.of.raw.in.fileset.pre, ' data files in fileset ', filesetName ,sep = '')
  message(msg)
  return(raw.names)
  
}


#add multiple raw data file to EV test

raw.files <- list.files(path = "C:/Users/Lisa/Documents/phd/southern ocean/BROKE-West raw data/Echoview/test/", pattern= "38H_120H_200H-D20060121.*\\.raw", full.names = T) 
EVAddRawData(EVFile = EVFile, filesetName = "38H-120H-200H", dataFiles = raw.files)


#function to check the time range for the fileset

EVFindFilesetTime <- function(EVFile, filesetName){
  
  fileset.loc = EVFindFilesetByName(EVFile, filesetName)$filesetObj
  
  start.date <- as.Date(trunc(fileset.loc$StartTime()), origin = "1899-12-30")
  percent.day.elapsed <- fileset.loc$StartTime() - trunc(fileset.loc$StartTime())
  seconds.elapsed <- 86400*percent.day.elapsed
  start.time <- as.POSIXct(seconds.elapsed, origin = start.date, tz = "GMT")
  
  end.date <- as.Date(trunc(fileset.loc$EndTime()), origin = "1899-12-30")
  percent.day.elapsed <- fileset.loc$EndTime() - trunc(fileset.loc$EndTime())
  seconds.elapsed <- 86400*percent.day.elapsed
  end.time <- as.POSIXct(seconds.elapsed, origin = end.date, tz = "GMT")
  
  return(list(start.time = start.time, end.time = end.time))
  
}

fileset.times <- EVFindTime(EVFile, "38H-120H-200H")

#function to add a new class (or multiple classes)

EVAddNewClass <- function(EvFile, name){
  
  
  for(i in 1:length(name)){
    
    add.class <- EVFile[["RegionClasses"]]$Add(name[i])
    
    if(add.class == FALSE){
      msg=paste(Sys.time(),' : Error: could not add region class', name[i],'to EvFile' ,sep=' ')
      message(msg)
    }
    
    if(add.class == TRUE){
      add.class
      msg=paste(Sys.time(),' : Added region class', name[i],'to EvFile' ,sep=' ')
      message(msg)
    }  
  }
}
  
#add a new region class called CTD
EVAddNewClass(EVFile, "CTD")  

#find date and time of start of CTD drop
ctd.info <- read.csv(file = "C:/Users/Lisa/Documents/phd/southern ocean/BROKE-West raw data/Echoview/test/ctd_information.csv")

#add zeros at the start of single digit dates and times and make into a single string
ctd.date <- 0
for(i in 1:nrow(ctd.info)){
  if(nchar(ctd.info$month[i]) == 1){
    ctd.info$month[i] <- paste(0, ctd.info$month[i], sep = "")
  }
  if(nchar(ctd.info$day[i]) == 1){
    ctd.info$day[i] <- paste(0, ctd.info$day[i], sep = "")
  }
  while(nchar(ctd.info$start_time[i]) < 6){
    ctd.info$start_time[i] <- paste(0, ctd.info$start_time[i], sep = "")
  }
  while(nchar(ctd.info$bottom_time[i]) < 6){
    ctd.info$bottom_time[i] <- paste(0, ctd.info$bottom_time[i], sep = "")
  }
  while(nchar(ctd.info$end_time[i]) < 6){
    ctd.info$end_time[i] <- paste(0, ctd.info$end_time[i], sep = "")
  }

  #pad times with 0's to make correct length for .evr file
  ctd.info$start_time[i]  <- paste(ctd.info$start_time[i], 0, 0, 0, 0, sep = "")
  ctd.info$bottom_time[i] <- paste(ctd.info$bottom_time[i], 0, 0, 0, 0, sep = "")
  ctd.info$end_time[i]    <- paste(ctd.info$end_time[i], 0, 0, 0, 0, sep = "")
  
  #make a single date string
  ctd.date[i] <- paste(ctd.info$year[i], ctd.info$month[i], ctd.info$day[i], sep = "")
}


#write echoview region definitions file for the CTD region
line_1 <- paste("EVRG 7 5.4.96.24494", sep = "")
line_2 <- "1"
line_3 <- ""
line_4 <- paste("13 4 1 0 3 -1 1", ctd.date[32], ctd.info$start_time[32],  "0", ctd.date[32], ctd.info$end_time[32], "250", sep = " ")
line_5 <- "0"
line_6 <- "0"
line_7 <- "CTD class"
line_8 <- paste(ctd.date[32], ctd.info$start_time[32],  "0", ctd.date[32], ctd.info$start_time[32],  "250", ctd.date[32], ctd.info$end_time[32], "250", ctd.date[32], ctd.info$end_time[32], "0", "1", sep = " ")
line_9 <- "CTD_32"
file_lines <- c(line_1, line_2, line_3, line_4, line_5, line_6, line_7, line_8, line_9)

dummy.def <- file("C:/Users/Lisa/Documents/phd/southern ocean/BROKE-West raw data/Echoview/test/dummy_file.evr", 'w')
for (i in 1:9){
  writeLines(file_lines[i], con = dummy.def)
}
close(dummy.def)

#funtion to import region definitions file in to Echoview
#evr = echoview region definitions file name and path
#region_name = name of region to be imported 

EVImportRegionDef <- function(EvFile, evr, region_name){
  
  #check whether a region of that name already exists
  CheckName <- EvFile[["Regions"]]$FindByName(region_name)
  if (is.null(CheckName) == TRUE){
    
    #import region definitions file
    EvFile$Import(evr)
    
    #check whether new region has been added
    CheckName <- EvFile[["Regions"]]$FindByName(region_name)
    
    if(is.null(CheckName) == FALSE){ msg <- paste(Sys.time(),' : Imported region definitions: Region ',region_name,' added',sep='')
          message(msg)
    } else { msg <- paste(Sys.time(),' : Failed to import region definitions' ,sep='')
             warning(msg)}
    
  } else { msg <- paste(Sys.time(),' : Failed to import region definitions: A region of that name already exists' ,sep='')
          warning(msg) 
  }
}

EVImportRegionDef(EVFile, "C:/Users/Lisa/Documents/phd/southern ocean/BROKE-West raw data/Echoview/test/dummy_file.evr", line_9)


#function to export data for acoustic variable by region

EVExportRegionSv <- function(EVFile, VariableName, RegionName, FilePath){
  
  acoustic.var <- EVFile[["Variables"]]$FindByName(VariableName)
  ev.region <- EVFile[["Regions"]]$FindByName(RegionName)
  export.data <- acoustic.var$ExportDataForRegion(FilePath, ev.region)
  
  if(export.data == TRUE){
    msg <- paste(Sys.time(),' : Exported data for Region ', RegionName, ' in Variable ', VariableName, sep='')
    message(msg)
  } else { msg <- paste(Sys.time(),' : Failed to export data' ,sep='')
           warning(msg)
  }
           
}

EVExportRegionSv(EVFile, '120H Sv hrp raw', 'CTD_32', "C:/Users/Lisa/Documents/phd/southern ocean/BROKE-West raw data/Echoview/test/test_script.csv")













