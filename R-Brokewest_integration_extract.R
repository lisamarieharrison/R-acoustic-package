#For each EV transect file of the BROKE-West survey export integration by cells in Echoview 
#Exports 38kHz and 120kHz on a 10m x 50ping grid for high resolution data
#restarts if Echoview crashes
#Author: Lisa-Marie Harrison
#Date: 28/01/2015
library(RDCOMClient)
library(EchoviewR)

#get list of EV files
file.dates <- list.files(path = "C:/Users/43439535/Documents/Lisa/BROKE-West/EV", full.names = T, pattern = paste(".*\\.ev$", sep = ""))
transect <- substr(list.files(path = "C:/Users/43439535/Documents/Lisa/BROKE-West/EV", pattern = paste(".*\\.ev$", sep = "")), 1, 13)


startEchoView <- function(i){
  #restarts Echoview if it crashes
  
  result <- NULL
  
  while(is.null(result)){
    result = tryCatch({
      EVAppObj=COMCreate('EchoviewCom.EvApplication')
      EVAppObj$EvFiles() # try to access the app to see if it works
      EVAppObj
    }, warning = function(w) {
      msg <- paste("WARNING: Got some warning while creating EV, but continuing", w, sep = " ")
      print(msg)
    }, error = function(e) {
      msg <- paste("Looks like we failed to create EV", e, sep = " ")
      print(msg)
      return(NULL)
    })
  }
  return(result)
}


#loop over each date and export integration by cells separately
for (i in 36:length(file.dates)) {
  tryCatch({
    
    EVAppObj <- startEchoView()
    
    #read in an Echoview file
    EVFile= EVOpenFile(EVAppObj, fileName = file.dates[i])$EVFile
    
    #add calibration file
    EVAddCalibrationFile(EVFile, "38H-120H-200H", "C:/Users/43439535/Documents/Lisa/BROKE-West/SimradEK60DSRII2010.ecs")
    
    #read raw data names in fileset
    file.names <- EVFilesInFileset(EVFile = EVFile, filesetName = '38H-120H-200H')
    
    #clear raw data files
    EVClearRawData(EVFile = EVFile, filesetName = '38H-120H-200H')
    
    #add multiple raw data file to EV test
    raw.files <- paste("C:/Users/43439535/Documents/Lisa/BROKE-West/RAW/", file.names, sep = "")
    EVAddRawData(EVFile = EVFile, filesetName = "38H-120H-200H", dataFiles = raw.files)
    
    
    #change grid of 120kHz and 38kHz variables to 10m x 50pings
    varObj = EVAcoVarNameFinder(EVFile, acoVarName = "120 H hrp 0 to 250 m")$EVVar
    EVChangeVariableGrid(EVFile = EVFile, acousticVar = varObj, verticalType = 4, verticalDistance = 50, horizontalType = 1, horizontalDistance = 10)
    
    varObj = EVAcoVarNameFinder(EVFile, acoVarName = "38 H hrp 0 to 250 m")$EVVar
    EVChangeVariableGrid(EVFile = EVFile, acousticVar = varObj, verticalType = 4, verticalDistance = 50, horizontalType = 1, horizontalDistance = 10)
    
    #export integration by cells for 38kHz and 120kHz
    return.file.38 <- paste("C:/Users/43439535/Documents/Lisa/BROKE-West/Extracted data/10x50 integration/", transect[i], "_10m_50ping_38kHz.csv", sep = "")
    return.file.120 <- paste("C:/Users/43439535/Documents/Lisa/BROKE-West/Extracted data/10x50 integration/", transect[i], "_10m_50ping_120kHz.csv", sep = "")
    EVExportIntegrationByCells(EVFile = EVFile, variableName = "38 H hrp 0 to 250 m", filePath = return.file.38)
    EVExportIntegrationByCells(EVFile = EVFile, variableName = "120 H hrp 0 to 250 m", filePath = return.file.120)
    
    #save the open .EV file
    EVSaveFile(EVFile = EVFile)
    
    #close the current file
    EVCloseFile(EVFile = EVFile)
    
    message("Finished processing", transect[i])
    
  }, warning = function(w) {
    msg <- paste("WARNING: Got some warning, but continuing", w, sep = " ")
    print(msg)
  }, error = function(e) {
    msg <- paste("ERROR: Looks like we got an error, rerunning station number", e, i, sep = " ")
    print(msg)
    i <- i - 1
  })
}














