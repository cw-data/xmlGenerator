# a minimal workflow
#----- external packages
library(xml2)
library(dplyr)
library(data.table)
library(stringr)

#----- my package
library(xmlGenerator)

#----- load package functions
# source("R/loadData.R")

# xmlGenerator::load_example_data()
forms_spreadsheet <- "data-raw/testdata.xlsx"
# xmlGenerator::testfunction(forms_spreadsheet)
xmlGenerator::loadData(forms_spreadsheet = forms_spreadsheet)








# xmlGenerator::loadData(forms_spreadsheet)
# xmlGenerator::buildXML(record_list, write = FALSE)
