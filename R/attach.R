#' Attach packages commonly used in data cleaning and analysis
#'
#' By default this function will attach the packages
#' `dplyr`, `tidyr`, `readr`, and `ggplot2`.
#' You can avoid attaching any of these packages via the `packages_to_omit`
#' parameter.
#' If any additional packages are requested via the `additional_packages`
#' parameter, those packages will be attached as well.
#' Information will be printed to the console listing which packages were
#' attached, which could not be attached, and what functions from the attached
#' packages mask other functions that were on the search path before calling
#' `attach_common_packages()`.
#'
#' @param additional_packages A character vector giving the names of additional
#'     packages to attach, or `NULL` (the default) if no additional packages
#'     are needed.
#' @param packages_to_omit A character vector giving the names of packages
#'     *not* to attach, or `NULL` (the default) if no packages need be omitted.
#'
#' @return Invisibly returns a character vector giving the names of the
#'     packages successfully attached
#'
#' @examples
#' \dontrun{
#' attach_common_packages()
#' }
#'
#' @export
attach_common_packages = function(
        additional_packages = NULL,
        packages_to_omit = NULL
) {
    pkgs = setdiff(c("dplyr", "tidyr", "readr", "ggplot2"), packages_to_omit)
    pkgs = c(pkgs, additional_packages)
    attached = NULL
    for ( pkg in pkgs ) {
        present = suppressPackageStartupMessages(
            require(pkg, character.only = TRUE, quietly = TRUE)
        )
        if ( present ) {
            attached = c(attached, pkg)
        }
    }
    cli::cat_line(cli::rule(center = "Packages attached"))
    for ( pkg in attached ) {
        cli::cli_alert_success("Attached package {pkg}")
    }
    for ( pkg in setdiff(pkgs, attached) ) {
        cli::cli_alert_danger("Could not attach package {pkg}")
    }
    cli::cat_line(cli::rule(center = "Function conflicts"))
    ## Get a data frame of all objects we can find
    conflicts = sapply(search(), ls)
    conflicts = data.frame(
        pkg = rep(names(conflicts), lengths(conflicts)),
        obj = unlist(conflicts)
    )
    ## Subset that to the objects with duplicate names on the search path
    condition1 = duplicated(conflicts$obj)
    condition2 = duplicated(conflicts$obj, fromLast = TRUE)
    conflicts  = conflicts[condition1 | condition2, ]
    conflicts  = conflicts[order(conflicts$obj), ]
    ## Eliminate any objects that are *actually* identical
    for ( obj in unique(conflicts$obj) ) {
        subs = conflicts[conflicts$obj == obj, ]
        tmp  = sapply(1:nrow(subs), function(i) {
            res = get(x = subs$obj[i], pos = subs$pkg[i])
            if ( !is.function(res) ) return(NULL) ## Eliminate non-functions
            return(res)
        })
        tmp = tmp[lengths(tmp) != 0] ## Eliminate NULL entries
        if ( sum(!duplicated(tmp)) < 2 ) {
            conflicts = conflicts[conflicts$obj != obj, ]
        }
    }
    conflicts$pkg = gsub("package:", "", conflicts$pkg)
    maskers = conflicts[conflicts$pkg %in% pkgs, ]
    condition1 = !(conflicts$pkg %in% pkgs)
    condition2 = conflicts$obj %in% maskers$obj
    maskees = conflicts[condition1 & condition2, ]
    masking_calls = format(paste0(maskers$pkg, "::", maskers$obj))
    masked_calls = sapply(maskers$obj, function(x) {
        paste(paste0(maskees$pkg[maskees$obj == x], "::", x), collapse = ", ")
    })
    masks = paste(masking_calls, "masks", masked_calls)
    sapply(masks, cli::cli_alert_danger)
    return(invisible(attached))
}
