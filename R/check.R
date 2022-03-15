check_packages <- function(path = getwd()) {
    README <- readLines(con = find_README(path))
    pkgs   <- find_packages(path)
    listed <- sapply(1:nrow(pkgs), function(i) {
        pattern <- sprintf("%s\\W[^.\n]*%s", pkgs$Package[i], pkgs$Version[i])
        any(grepl(pattern = pattern, x = README))
    })
    msg_ending <- "listed correctly in README"
    if ( all(listed) ) {
        cli::cli_alert_success("All detected packages {msg_ending}")
    } else {
        cli::cli_alert_danger("Packages detected but not {msg_ending}:")
        for ( i in which(!listed) ) {
            cat(sprintf("    - %s v %s\n", pkgs$Package[i], pkgs$Version[i]))
        }
    }
    return(invisible(pkgs[!listed, ]))
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

check_for_R_version <- function(path = getwd()) {
    README <- readLines(con = find_README(path))
    major  <- R.version$major
    minor  <- R.version$minor
    this_version_pattern <- paste0("R\\W\\D*", major, "\\.", minor)
    any_version_pattern  <- "R\\W[^.\n]*[1-4]\\.[0-9]+\\.[0-9]+"
    has_this_version <- any(grepl(pattern = this_version_pattern, x = README))
    has_any_version  <- any(grepl(pattern = any_version_pattern,  x = README))
    if ( has_this_version ) {
        cli::cli_alert_success("Correct R version listed in README")
    } else if ( has_any_version ) {
        cli::cli_alert_warning("The wrong R version may be listed in README.")
        cat(sprintf("  Your current R version is %s.%s.\n", major, minor))
        cat("  Please make sure the right R version is listed.\n")
    } else {
        cli::cli_alert_danger("Could not find any R version listed in README")
    }
    return(invisible(R.version.string))
}
