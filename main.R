#----- required external packages
library(stringr)
library(xml2)
library(dplyr)
library(data.table)

#----- my package
library(xmlGenerator)


#--------------------------------
# A minimal reproducible workflow that's included with library(xmlGenerator)
# Purpose: include in the package everything a user needs to "see what's going on" without providing their own data
#--------------------------------

# package includes example data so a user can see what the required data format is
xmlGenerator::load_example_data()
xmlGenerator::build_xml(record_list)

#--------------------------------
# An example "real" workflow
# Purpose: mock up the exact workflow a user needs to follow to use library(xmlGenerator) to make xml
#--------------------------------
# step 1: assign the filepath to your forms data as a character string
FORMS_SPREADSHEET <- "data-raw/testdata.xlsx"
# step 2: run loadData()
# this function validates your input data for filetype, colnames, reference types
# then it parses your data into a list (an instance of class record_list)
xmlGenerator::load_data(spreadsheet_path = FORMS_SPREADSHEET)
# step 3: run buildXML()
# this function builds xml out of your `record_list` object
# "write = TRUE" will bring up a dialog for the user to choose where to save the xml output
xmlGenerator::build_xml(record_list, write = FALSE)
