check_packages <- function(path = getwd()) {
    packages <- find_packages(path)
    expected_entries <- paste0(
        "+ ", packages$Package, ", version ", packages$Version
    )
    README <- readLines(con = find_README(path))
    pkg_pattern <- "\\+ [^,]+, version [^\n]+"
    actual_list <- extract_matches(pattern = pkg_pattern, text = README)
    missing_entries <- which(sapply(expected_entries, function(e) {
        !any(grepl(pattern = e, x = actual_list))
    }))
    msg_ending <- "listed correctly in README"
    if ( length(missing_entries) > 0 ) {
        cli::cli_alert_danger("Packages detected but not {msg_ending}:")
        for ( i in missing_entries ) {
            cat("    ", expected_entries[i], "\n", sep = "")
        }
    } else {
        cli::cli_alert_success("All detected packages {msg_ending}")
    }
    return(invisible(expected_entries[missing_entries]))
}

check_files <- function(path = getwd()) {
    files  <- find_files(path)
    README <- readLines(con = find_README(path))
    not_listed <- which(sapply(files, function(f) {
        !any(grepl(pattern = f, x = README))
    }))
    msg_ending <- "listed correctly in README"
    if ( length(not_listed) > 0 ) {
        cli::cli_alert_danger("Files detected but not {msg_ending}:")
        for ( i in not_listed ) {
            cat("    - ", files[i], "\n", sep = "")
        }
    } else {
        cli::cli_alert_success("All detected files {msg_ending}")
    }
    return(invisible(files[not_listed]))
}
