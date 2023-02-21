#' Make a lookup table for reference types
#'
#' Make a reference type lookup table in \code{\link{xmlGenerator}}
#'
#' @param none
#'
#' @return \code{"example_data"}: a data frame containing reference type (e.g., book, newspaper) key-value pairs and column-index information
#'
#' @examples
#' # note: xmlGenerator::make_ref_type_lookup() is a sub-function of `load_example_data()` and `load_data()` and isn't meant to be called on its own.
#' xmlGenerator::make_ref_type_lookup()
#'
#' @export

make_ref_type_lookup <- function() {
    #----- a module for loadData.R
    # colname_lookup <- data.table::fread("data-raw/colname_tagname_dictionary_20230130.csv")
    # ref_type_lookup <- readxl::read_excel("data-raw/endnote_ref-type_dictionary_20230130.xlsx") # a lookup table of reference types (new_spec)
    # save(ref_type_lookup, file='data/ref_type_lookup.rda')
    # rm(ref_type_lookup)
    # load(file="data/ref_type_lookup.rda")
    ref_type_lookup <- xmlGenerator::ref_type_lookup
    ref_type_lookup$value <- stringr::str_extract(ref_type_lookup$XML, "([0-9]+)") # extract the value from inside the xml tags
    return(ref_type_lookup)
}
