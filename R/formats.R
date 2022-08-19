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
            includes = rmarkdown::includes(in_header = custom_preamble)
        )
    )
}
