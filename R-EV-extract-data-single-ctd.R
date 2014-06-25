# Extracts data around a single CTD station from Echoview using the R acoustic package
# Functions must be loaded in first

#read in the .csv file containing the start and end times for each CTD drop
ctd.info <- read.csv(file = "C:/Users/Lisa/Documents/phd/southern ocean/BROKE-West raw data/Echoview/test/ctd_information.csv")

#add zeros at the start of single digit dates and times and make into a single string
ctd.date <- EVctdTimes(ctd.info)$ctd.date
ctd.info <- EVctdTimes(ctd.info)$ctd.info.R

#create an Echoview object and open Echoview
EVAppObj=COMCreate('EchoviewCom.EvApplication')

#open new echoview object
EVFile= EVOpenFile(EVAppObj, fileName = "C:/Users/Lisa/Documents/phd/southern ocean/BROKE-West raw data/Echoview/test/Transect01-07_final.ev")$EVFile

#add a calibration file to the fileset
EVAddCalibrationFile(EVFile, "38H-120H-200H", "C:/Users/Lisa/Documents/phd/southern ocean/BROKE-West raw data/Echoview/test/SimradEK60DSRII2010.ecs")

#clear all current raw data from fileset
EVClearRawData(EVFile, "38H-120H-200H")

#read in raw data files around CTD station
raw.file.pattern <- paste("38H_120H_200H-D", ctd.date[32], ".*\\.raw$", sep = "")
raw.files <- list.files(path = "C:/Users/Lisa/Documents/phd/southern ocean/BROKE-West raw data/Echoview/test/", pattern = raw.file.pattern, full.names = T) 
EVAddRawData(EVFile = EVFile, filesetName = "38H-120H-200H", dataFiles = raw.files)

#check the start and end time of the fileset
fileset.times <- EVFindFilesetTime(EVFile, "38H-120H-200H")

#create a new region class called CTD
EVAddNewClass(EVFile, "CTD")  

#change start time to 1 hour before CTD drop
ctd.start.time.new <- ChangeStartTime(ctd.info, h = 1, m = 0)$ctd.time
ctd.date.new       <- ChangeStartTime(ctd.info, h = 1, m = 0)$ctd.date

#change end time to 1 hour after CTD drop
ctd.end.time.new <- ChangeEndTime(ctd.info, ctd.date.new, h = 1, m = 0, ctd.start.time.new)$ctd.time
ctd.date.end     <- ChangeEndTime(ctd.info, ctd.date.new, h = 1, m = 0, ctd.start.time.new)$ctd.date.end


#write echoview region definitions file for the CTD region
line_1 <- paste("EVRG 7 5.4.96.24494", sep = "")
line_2 <- "1"
line_3 <- ""
line_4 <- paste("13 4 1 0 3 -1 1", ctd.date.new[32], ctd.start.time.new[32],  "0", ctd.date.end[32], ctd.end.time.new[32], "250", sep = " ")
line_5 <- "0"
line_6 <- "0"
line_7 <- "CTD class"
line_8 <- paste(ctd.date.new[32], ctd.start.time.new[32],  "0", ctd.date.new[32], ctd.start.time.new[32],  "250", ctd.date.end[32], ctd.end.time.new[32], "250", ctd.date.end[32], ctd.end.time.new[32], "0", "1", sep = " ")
line_9 <- paste("CTD_", 32, sep = "")
file_lines <- c(line_1, line_2, line_3, line_4, line_5, line_6, line_7, line_8, line_9)

dummy.def <- file("C:/Users/Lisa/Documents/phd/southern ocean/BROKE-West raw data/Echoview/test/dummy_file.evr", 'w')
for (i in 1:9){
  writeLines(file_lines[i], con = dummy.def)
}
close(dummy.def)

#import the region definitions file into Echoview
EVImportRegionDef(EVFile, "C:/Users/Lisa/Documents/phd/southern ocean/BROKE-West raw data/Echoview/test/dummy_file.evr", line_9)

#change the data range bitmap to remove the noise class that has been given to data around the CTD
acoustic.var <- EVFile[["Variables"]]$FindByName('38H b hrp aspikes')
EVadjustDataRngBitmap(varObj = acoustic.var, -999, 0)

#export the Sv values for the region as a csv file
EVExportRegionSv(EVFile, '120H Sv hrp raw', 'CTD_32', "C:/Users/Lisa/Documents/phd/southern ocean/BROKE-West raw data/Echoview/test/test_script.csv")

















