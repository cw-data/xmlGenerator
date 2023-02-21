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
