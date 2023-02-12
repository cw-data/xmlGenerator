
load_example_data <- function() {
    tryCatch(
        expr = {
            assign("example_data", example_data, envir = globalenv())
            message("Example data loaded to global environment")
        }
    )
}
