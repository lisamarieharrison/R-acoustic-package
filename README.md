R-acoustic-package
==================

Additions to the R acoustic package before upload to acoustic repository

This script contains test functions to be loaded into the R acoustic package eventually. The script automatically
exports the Sv data around a CTD station in Echoview from R as a .csv file. The script only runs this for a single
CTD station currently. The following functions to add to the R acoustic package are included:

EVAddCalibrationFile – Function to add a calibration file to a fileset
EVFilesInFileset – Function to return names of all raw files in a fileset
EVClearRawData – Function to remove all raw files in a fileset
EVFindFilesetTime – Function to find the start and end date and time of a fileset
EVAddNewClass – Function to create a new region class
EVImportRegionDef – Function to import a region definitions file into Echoview
EVExportRegionSv – Function to export Sv values for a region in a variable as a .csv file
