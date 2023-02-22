#----- required external packages
library(stringr)
library(xml2)
library(dplyr)
library(data.table)
library(readxl)
library(devtools)

#----- xmlGenerator
devtools::install_github("https://github.com/cw-data/xmlGenerator")
library(xmlGenerator)


#--------------------------------
# A minimal reproducible workflow that's included with library(xmlGenerator)
# Purpose: include in the package everything a user needs to "see what's going on" without providing their own data
#--------------------------------
# step 1: load and validate example data
# this step assigns objects to the user's global environment for inspection.
xmlGenerator::load_example_data() # ?load_example_data for more info
# step 2: build xml from the example data
xmlGenerator::build_xml(record_list) # ?build_xml for more info

#--------------------------------
# An example "real" workflow
# Purpose: mock up the exact workflow a user needs to follow to use library(xmlGenerator) to make xml
#--------------------------------
# step 1: assign the filepath to your forms data as a character string
FORMS_SPREADSHEET <- "my_directory/my_data.xlsx" # replace with your filepath
# step 2: run load_data()
# this function validates your input data for filetype, colnames, reference types
# then it parses your data into a list (an instance of class record_list)
xmlGenerator::load_data(spreadsheet_path = FORMS_SPREADSHEET)
# step 3: run build_xml()
# this function builds xml out of your `record_list` object
# "write = TRUE" will bring up a dialog for the user to choose where to save the xml output
xmlGenerator::build_xml(record_list, write = FALSE)
