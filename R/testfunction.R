testfunction <- function(forms_spreadsheet){
    # y <- x*x
    # ret_df <- data.frame(x=x,
    #                      y=y)
    # assign("ret_df", ret_df, envir = globalenv())
    # return(ret_df)
    # mydf <- readxl::read_excel("data-raw/testdata.xlsx")
    # assign("mydf", mydf, envir = globalenv())
    # return(mydf)

    if(tools::file_ext(forms_spreadsheet) == "xlsx"){
        forms_spreadsheet <- readxl::read_excel(forms_spreadsheet)
        message("`forms_spreadsheet` is xlsx...")

        assign("forms_spreadsheet", forms_spreadsheet, envir = globalenv())
        # return(forms_spreadsheet)
    }
}
