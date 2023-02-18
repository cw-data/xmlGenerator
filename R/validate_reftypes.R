# check that ncol is legit
validate_reftypes <- function(forms_spreadsheet, ref_type_lookup) {
    tryCatch(
        expr = {
            forms_spreadsheet <- dplyr::left_join(forms_spreadsheet, # add the $value column to `forms_spreadsheet`
                                                  ref_type_lookup %>% select("Reference type (from Form)", "value"),
                                                  by = c("Reference Type" = "Reference type (from Form)"))
            weird_refs <- subset(forms_spreadsheet, !(`Reference Type` %in% ref_type_lookup$`Reference type (from Form)`)) # find ref-types that aren't in the lookup table
            if(nrow(weird_refs)==0){ # if all of the unique values in forms_spreadsheet$`Reference Type` are in `ref_type_lookup$`Reference type (from Form)``
                message("`forms_spreadsheet` reference types are acceptable...") # print success message
            } else if(nrow(weird_refs)>0){ # if there are any ref-types that aren't in the lookup table
                for (i in 1:nrow(weird_refs)){ # print the weird refs so the user can do something about them
                    message(paste("Row", i, "in your `forms_spreadsheet` contains an unacceptable $`Reference Type`. Correct $`Reference Type` at row", i, "or add the reference type to `resources/endnote_ref-type_dictionary.xlsx` and add a module to the `R` directory of this project to accomodate for this new reference type."))
                }
                break # end the program
            }
            return(forms_spreadsheet)
            # success message
            message("`forms_spreadsheet` validated...loaded to global environment as `forms_spreadsheet`...\n\nParsing and preparing `record_list`...")
        }
    )
}
