#----- a module for loadData.R

make_ref_type_lookup <- function() {
    ref_type_lookup <- readxl::read_excel("data-raw/endnote_ref-type_dictionary_20230130.xlsx") # a lookup table of reference types (new_spec)
    ref_type_lookup$value <- stringr::str_extract(ref_type_lookup$XML, "([0-9]+)") # extract the value from inside the xml tags
    return(ref_type_lookup)
}
