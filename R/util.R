extract_matches <- function(pattern, text) {
    return(unlist(regmatches(text, regexec(pattern = pattern, text = text))))
}
