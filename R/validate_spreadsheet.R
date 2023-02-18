# validate the user-provided `forms_spreadsheet` is a spreadsheet
validate_spreadsheet <- function(forms_spreadsheet){
    tryCatch(
        expr = {
            if(tools::file_ext(forms_spreadsheet) == "xlsx"){
                forms_spreadsheet <- readxl::read_excel(forms_spreadsheet)
                message("`forms_spreadsheet` is xlsx...")
                # assign("forms_spreadsheet", forms_spreadsheet, envir = globalenv())
                return(forms_spreadsheet)
            } else if(tools::file_ext(forms_spreadsheet) == "xls"){
                forms_spreadsheet <- readxl::read_excel(forms_spreadsheet)
                message("`forms_spreadsheet` is xls...")
                return(forms_spreadsheet)
            } else if(tools::file_ext(forms_spreadsheet) == "csv"){
                forms_spreadsheet <- data.table::fread(forms_spreadsheet)
                message("`forms_spreadsheet` is csv...")
                return(forms_spreadsheet)
            } else {
                message("`loadData()` requires argument `forms_spreadsheet` to be one of these filetypes: 'xlsx', 'xls', 'csv'. The `forms_spreadsheet` that you provided is not one of the acceptable filetypes. Input a `forms_spreadsheet`with an acceptable filetype and try `loadData(forms_spreadsheet)` again.")
                break
            }
        }
    )
}


