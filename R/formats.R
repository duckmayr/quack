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
    custom_template = resource_path("article", "quack-article-template.tex")
    custom_preamble = resource_path("article", "quack-article-preamble.tex")
    return(
        bookdown::pdf_document2(
            toc = FALSE,
            template = custom_template,
            includes = rmarkdown::includes(in_header = custom_preamble),
            ...
        )
    )
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
