# check that ncol is legit
validate_blanks <- function(forms_spreadsheet) {
    tryCatch(
        expr = {
            # check for blank $`Reference Type`
            blanks <- forms_spreadsheet %>%
                subset(is.na(`Reference Type`))
            if(nrow(blanks)>0) {
                for (i in 1:nrow(blanks)) {
                    message(paste("Row", i, "in your `forms_spreadsheet` contains NA in the $`Reference Type` column. $`Reference Type` cannot be NA for a valid input. Correct your input and try `loadData(forms_spreadsheet)` again."))
                }
                break # end the program
            }
        }
    )
}
