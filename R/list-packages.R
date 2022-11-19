#' List packages required by R scripts in a directory
#'
#' This function reports the names and currently installed versions of R
#' packages used by all R scripts and R Markdown files in a given directory.
#'
#' This function looks at all R scripts and R Markdown files contained in a
#' directory (and its subdirectories) and tries to identify all packages used
#' in those R scripts and R Markdown files. It looks for names of packages in
#' [library()] and [require()] calls, as well as packages used in the form
#' `package::function()` or `package:::function()`. Detection of packages used
#' may be imperfect, but this should work fairly well.
#'
#' The function also records the versions of those packages currently
#' installed, as well as the R version being used, and if RStudio is installed,
#' the RStudio version installed, and reports that information as well.
#'
#' @param path A character vector of length one giving the directory you'd like
#'     searched for R scripts and R Markdown files
#'
#' @return An object of class `RequiredPackages`, which is a list of length
#'     three, with elements
#'     \describe{
#'         \item{R_version}{
#'             A character vector of length one giving the R version
#'         }
#'         \item{RStudio_version}{
#'             A character vector of length one giving the RStudio version,
#'             if applicable, or NA if not.
#'         }
#'         \item{packages}{
#'             A character matrix with two columns, "Package", giving the names
#'             of the packages detected, and "Version", giving the version
#'             currently installed.
#'         }
#'     }
#'
#' @seealso [print.RequiredPackages()]
#'
#' @export
list_required_packages = function(path = ".") {
    installed_packages = installed.packages()
    packages_used = character()
    files_to_check = list.files(
        path = path,
        pattern = ".*\\.Rmd|.*\\.R",
        full.names = TRUE,
        recursive = TRUE
    )
    package_pattern = "(?<=library\\()[A-Za-z0-9.]+"
    package_pattern = paste0(package_pattern, "|(?<=require\\()[A-Za-z0-9.]+")
    package_pattern = paste0(package_pattern, "|(?<=\\b)[A-Za-z0-9.]+?(?=::)")
    package_pattern = paste0(package_pattern, "|(?<=\\b)[A-Za-z0-9.]+?(?=:::)")
    for ( filename in files_to_check ) {
        contents = readLines(filename)
        packages = unlist(stringr::str_extract_all(contents, package_pattern))
        packages_used = unique(c(packages_used, packages))
    }
    available = packages_used %in% rownames(installed_packages)
    problems = packages_used[!available]
    packages_to_report = packages_used[available]
    packages = installed_packages[packages_to_report, c("Package", "Version")]
    rownames(packages) = NULL
    if ( length(problems) > 0 ) {
        pre = "These packages may have been used but aren't installed:\n"
        problems = paste(problems, collapse = ", ")
        post = "\nCheck to be sure"
        warning(pre, problems, post, call. = FALSE)
    }
    has_RStudio = require(rstudioapi, quietly = TRUE)
    RStudio_version = "if"(has_RStudio, rstudioapi::versionInfo()$version, NA)
    return(structure(
        .Data = list(
            R_version = R.version.string,
            RStudio_version = as.character(RStudio_version),
            packages = packages
        ),
        class = "RequiredPackages"
    ))
}

#' Print a RequiredPackages object
#'
#' Prints the information from the RequiredPackages object in a format that may
#' be used as a Markdown unordered list.
#'
#' @param x An object of class [RequiredPackages][list_required_packages()]
#' @param ... Arguments passed to or from other methods (currently unused)
#'
#' @return Invisibly returns x
#'
#' @export
print.RequiredPackages = function(x, ...) {
    cat("Software used:\n\n")
    cat("-", x$R_version, "\n")
    if ( !is.na(x$RStudio_version) ) {
        cat("- RStudio version", x$RStudio_version, "\n")
    }
    cat("- R add-on packages\n")
    packages = x$packages[ , "Package"]
    versions = x$packages[ , "Version"]
    for ( i in seq_along(packages) ) {
        cat("  +", packages[i], "version", versions[i], "\n")
    }
    cat("\n")
    return(invisible(x))
}
