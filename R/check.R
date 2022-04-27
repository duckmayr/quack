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

#' Check research project for best practices
#'
#'
#'
#' @param path The path to the project's root directory; the default is the
#'     current working directory
#'
#' @return Invisibly returns a list with the following elements:
#' \describe{
#'     \item{unlisted_files}{
#'         Files discovered in your project directory that are not described
#'         in your README file
#'     }
#'     \item{current_R_version}{The value \code{\link[base]{R.version.string}}}
#'     \item{unlisted_packages}{
#'         A dataframe with the names and version numbers of
#'         R packages detected in your research project's code that are either
#'         not listed in your README file, or which are listed in your README
#'         file in a way that does not reference the version of the package
#'         currently installed on your machine.
#'     }
#'     \item{files_for_other_software}{
#'         A character vector that includes
#'         \itemize{
#'             \item Paths to Python scripts if you do not mention in the
#'                 README file that Python was used in analysis, and
#'             \item Paths to Stata do files if you do not mention in the
#'                 README file that Stata was used in analysis.
#'         }
#'     }
#' }
#' @export
check_research_project <- function(path = getwd()) {
    cli::cli_h1("Checking README")
    unlisted_files <- check_file_list(path)
    current_R_version <- check_for_R_version(path)
    unlisted_packages <- check_packages(path)
    files_for_other_software <- check_for_other_software(path)
    return(invisible(list(
        unlisted_files = unlisted_files,
        current_R_version = current_R_version,
        unlisted_packages = unlisted_packages,
        files_for_other_software = files_for_other_software
    )))
}
