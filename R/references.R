#' Determine what types of sources and which journals are cited in a bib file
#'
#' This function reads in a bib file and outputs to the console which types of
#' sources are contained in it (@Article, @Book, etc), and how many of each are
#' in the bib file. It also outputs which journals the @Article references come
#' from, and how many times each journal is listed in an @Article reference in
#' the bib file.
#'
#' @param filename A character vector of length one giving the path of the bib
#'     file you want examined
#'
#' @return Invisibly returns the contents of the bib file
#'
#' @export
categorize_references = function(filename) {
    contents = readLines(filename)
    types = stats::na.omit(stringr::str_extract(contents, "@[A-Za-z]+"))
    types = types[types != "@Comment" & types != "@comment"]
    types = table(types)
    cat("Reference types:\n")
    for ( i in 1:length(types) ) {
        cat("    ", names(types)[i], ": ", types[i], "\n", sep = "")
    }
    cat("\n")
    pattern  = "[Jj]ournaltitle *=.*\\}|[Jj]ournal *=.*\\}"
    journals = stats::na.omit(stringr::str_extract(contents, pattern))
    journals = stringr::str_extract(journals, "(?<=\\{).+(?=\\})")
    journals = table(journals)
    cat("Journals cited:\n")
    for ( i in 1:length(journals) ) {
        cat("    ", names(journals)[i], ": ", journals[i], "\n", sep = "")
    }
    cat("\n")
    return(invisible(paste(contents, collapse = "\n")))
}
