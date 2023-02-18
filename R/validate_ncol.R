# check that ncol is legit
validate_ncol <- function(forms_spreadsheet, example_data) {
    tryCatch(
        expr = {
            if(ncol(forms_spreadsheet) == ncol(example_data)) { # if the number of columns is correct
                message("`forms_spreadsheet` ncol() is acceptable...") # print success message
                assign("forms_spreadsheet", forms_spreadsheet, envir = globalenv())
                return(forms_spreadsheet)
            } else if(ncol(forms_spreadsheet) != ncol(example_data)) { # if number of columns is incorrect
                message("The number of columns in the `forms_spreadsheet` you provided does not match the required format. Re-format your input and try `loadData(forms_spreadsheet)` again.")
                # take out trash
                rm(example_data)
                break # end the program
            }
        }
    )
}


