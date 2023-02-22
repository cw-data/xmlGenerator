#' Load example data
#'
#' Load example data in \code{\link{xmlGenerator}}
#'
#' @param none
#'
#' @return \code{example_data}: a data frame containing the column structure that user-provided data must match. A user's MS Forms spreadsheet must have exactly the same column order and names as \code{example_data} to generate valid xml.
#' @return \code{forms_spreadsheet}: a data frame containing user-provided MS Forms data plus one column that \code{load_example_data()} left-joined to \code{example_data}: \code{forms_spreadsheet$value} that contains key values for reference-type lookups called in \code{xmlGenerator::build_xml()}.
#' @return \code{record_list}: a list of \code{forms_spreadsheet} data parsed into fields for xml.
#' @return \code{EXAMPLE_DATA}: a character string filepath that points to the source (e.g., xlsx, xls, csv) of \code{forms_spreadsheet}.
#'
#' @examples
#' xmlGenerator::load_example_data()
#'
#' @export

load_example_data <- function(example_data = xmlGenerator::example_data) {
    tryCatch(
        expr = {
            assign("EXAMPLE_DATA", "my_directory/my_data.xlsx", envir = globalenv()) # assign an example filepath
            assign("example_data", example_data, envir = globalenv()) # assign example data
            message("`example_data` loaded to global environment and converted to `forms_spreadsheet`") # print a success message
            forms_spreadsheet <- example_data # in the example scenario, `forms_spreadsheet` == `example_data`. IRL `forms_spreadsheet` != `example_data`

            validate_ncol(forms_spreadsheet, example_data) # validate number of columns
            validate_colnames(forms_spreadsheet, example_data) # validate column names
            ref_type_lookup <- make_ref_type_lookup() # make the ref-type lookup table
            forms_spreadsheet <- validate_reftypes(forms_spreadsheet, ref_type_lookup) # validate `forms_spreadsheet` ref types
            record_list <- validateAuthors(forms_spreadsheet, ref_type_lookup, example_data) # parse `forms_spreadsheet`

            assign("forms_spreadsheet", forms_spreadsheet, envir = globalenv())
            assign("record_list", record_list, envir = globalenv())
            message("`forms_spreadsheet` parsed to `record_list`...\nOutput available as `record_list` in global envrionment...\n")
            message("Run xmlGenerator::build_xml(record_list) to build xml!\n\n") # message indicating the function job completed
        }
    )
}
