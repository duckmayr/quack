## Helper function to determine if an R object is of Date class
is.Date = function(object) "Date" %in% class(object)

#' Format a date according to American convention
#'
#' Puts a date in Month Date, Year format and removes any leading zeroes
#'
#' @param date A [Date] class vector, or an object coercible to one
#'
#' @return A character vector giving the American formatted version of the
#'     dates in the object given as the `dates` argument
#'
#' @export
american_date_format = function(date) {
    ## Ensure we're working with a Date object
    if ( !is.Date(date) ) {
        date = try(as.Date(date), silent = TRUE)
        if ( inherits(date, "try-error") ) {
            stop("Cannot coerce date provided to Date class", call. = FALSE)
        }
    }
    ## Reformat the date
    result = format(date, "%B %d, %Y")
    ## Remove leading zeroes
    result = gsub(pattern = " 0", replacement = " ", x = result)
    ## Return formatted date
    return(result)
}
