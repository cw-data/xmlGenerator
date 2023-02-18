#----- load project functions
# source("R/validate_spreadsheet.R")
# source("R/validate_ncol.R")
# source("R/validate_colnames.R")
# source("R/validate_reftypes.R")
# source("R/validateAuthors.R")

loadData <- function(forms_spreadsheet){
    tryCatch(
        expr = {
            #----- load static asset
            # example_forms_spreadsheet <- example_data # read example (new_spec)
            load("data/example_data.rda")

            #----- validate parts of `forms_spreadsheet`
            forms_spreadsheet <- validate_spreadsheet(forms_spreadsheet)
            validate_ncol(forms_spreadsheet, example_data)
            validate_colnames(forms_spreadsheet, example_data)
            ref_type_lookup <- xmlGenerator::make_ref_type_lookup()
            forms_spreadsheet <- validate_reftypes(forms_spreadsheet, ref_type_lookup)
            record_list <- validateAuthors(forms_spreadsheet, ref_type_lookup, example_data)

            #----- if validations passed:
            assign("forms_spreadsheet", forms_spreadsheet, envir = globalenv())
            assign("record_list", record_list, envir = globalenv())
            message("`forms_spreadsheet` parsed to `record_list`...\nOutput available as `record_list` in global envrionment...\n")
            message("Ready to build xml!\n\n") # message indicating the function job completed
        }
    )
}

