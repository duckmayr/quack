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

check_file_list <- function(path = getwd()) {
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

check_for_other_software <- function(path = getwd()) {
    files  <- find_files(path)
    README <- readLines(con = find_README(path))
    bad_files <- character()
    python_files <- which(grepl(pattern = "\\.py$", x = files))
    stata_files <- which(grepl(pattern = "\\.do$", x = files))
    if ( length(python_files) > 0 ) {
        if ( !any(grepl(pattern = "python", x = README, ignore.case = TRUE)) ) {
            msg <- "Python scripts found but Python isn't discussed in README"
            cli::cli_alert_danger(msg)
        }
        bad_files <- c(bad_files, files[python_files])
    }
    if ( length(stata_files) > 0 ) {
        if ( !any(grepl(pattern = "stata", x = README, ignore.case = TRUE)) ) {
            msg <- "Stata do files found but Stata isn't discussed in README"
            cli::cli_alert_danger(msg)
        }
        bad_files <- c(bad_files, files[stata_files])
    }
    return(invisible(bad_files))
}
