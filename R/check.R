check_packages <- function(path = getwd()) {
    detected_packages <- renv::dependencies(
        path = path, progress = FALSE, errors = "ignored"
    )
    detected_packages <- unique(detected_packages$Package)
    installed_packages <- as.data.frame(utils::installed.packages())
    indices <- which(installed_packages$Package %in% detected_packages)
    detected_versions <- installed_packages$Version[indices]
    detected_packages <- installed_packages$Package[indices]
    expected_entries <- paste0(
        "+ ", detected_packages, ", version ", detected_versions
    )
    README <- normalizePath(path.expand(file.path(path, "README.md")))
    files_in_path <- normalizePath(list.files(path = path, full.names = TRUE))
    if ( !(README %in% files_in_path) ) {
        stop("Could not find README.md in ", path)
    }
    README <- readLines(con = README)
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
