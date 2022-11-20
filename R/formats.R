## Helper function to locate resources for a template in this package
resource_path = function(template, ...) {
    return(
        system.file(
            "rmarkdown", "templates", template, "resources", ...,
            package = "quack"
        )
    )
}

#' Custom formatted PDF document
#'
#' @param ... Arguments to be passed to [bookdown::pdf_document2()]
#'
#' @return An R Markdown output format object
#'
#' @export
article = function(...) {
    template = resource_path("article", "quack-article-template.tex")
    preamble = resource_path("article", "quack-article-preamble.tex")
    dots = list(...)
    if ( "template" %in% names(dots ) ) {
        stop("Do not use `template` YAML option when using {quack} formats")
    }
    if ( "includes" %in% names(dots) ) {
        includes = dots$includes
        includes$in_header = c(preamble, includes$in_header)
        dots$includes = includes
    } else {
        dots$includes = rmarkdown::includes(in_header = preamble)
    }
    args = c(dots, toc = FALSE, template = template)
    return(do.call(bookdown::pdf_document2, args))
}

#' Custom formatted Beamer presentation
#'
#' @param ... Arguments to be passed to [bookdown::beamer_presentation2()]
#'
#' @return An R Markdown output format object
#'
#' @export
presentation = function(...) {
    xsty  = function(x) gsub("\\.sty", "", x)
    theme = xsty(resource_path("presentation", "beamerthemeAustin.sty"))
    color = xsty(resource_path("presentation", "beamercolorthemelonghorn.sty"))
    pream = tempfile()
    calls = c(
        paste0("\\usepackage{", theme, "}"),
        paste0("\\usepackage{", color, "}")
    )
    writeLines(calls, pream)
    return(
        bookdown::beamer_presentation2(
            includes = rmarkdown::includes(in_header = pream),
            ...
        )
    )
}

#' PDF document formatted to meet APSR, AJPS, and JOP submission standards
#'
#' @param ... Arguments to be passed to [bookdown::pdf_document2()]
#'
#' @return An R Markdown output format object
#'
#' @export
submission = function(...) {
    template = resource_path("submission", "quack-submission-template.tex")
    dots = list(...)
    if ( "template" %in% names(dots ) ) {
        stop("Do not use `template` YAML option when using {quack} formats")
    }
    if ( !("toc" %in% names(dots)) ) {
        dots$toc = FALSE
    }
    args = c(dots, template = template)
    return(do.call(bookdown::pdf_document2, args))
}

#' PDF document format for syllabi
#'
#' @param ... Arguments to be passed to [bookdown::pdf_document2()]
#'
#' @return An R Markdown output format object
#'
#' @export
syllabus = function(...) {
    template = resource_path("syllabus", "quack-syllabus-template.tex")
    return(bookdown::pdf_document2(template = template, ...))
}
