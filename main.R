#----- required external packages
library(stringr)
library(xml2)
library(dplyr)
library(data.table)

#----- my package
library(xmlGenerator)


#--------------------------------
# A minimal reproducible workflow
#--------------------------------

# package includes example data so a user can see what the required data format is
# xmlGenerator::load_example_data()

# step 1: assign the filepath to your forms data as a character string in variable `forms_spreadsheet`
forms_spreadsheet <- "data-raw/testdata.xlsx"
# step 2: run loadData()
# this function validates your input data for filetype, colnames, reference types
# then it parses your data into a list (an instance of class record_list)
xmlGenerator::loadData(forms_spreadsheet = forms_spreadsheet)
# step 3: run buildXML()
# this function builds xml out of your `record_list` object
# "write = TRUE" will bring up a dialog for the user to choose where to save the xml output
xmlGenerator::buildXML(record_list, write = FALSE)
