validate_colnames <- function(forms_spreadsheet, example_data) {
    tryCatch(
        expr = {
            # check that colnames are legit
            if(length(setdiff(colnames(example_data), colnames(forms_spreadsheet)))==0){ # if there aren't any mismatched columns
                message("`forms_spreadsheet` colnames are acceptable...") # print success message
                return(forms_spreadsheet)
            } else { # if there are mismatched columns
                message("The column names in the `forms_spreadsheet` that you provided do not match the required format. Re-format your input and try `loadData(forms_spreadsheet)` again.")
                # take out trash
                break # end the program
            }
        }
    )
}
