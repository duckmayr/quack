replace_file_text <- function(filepath, pattern, replacement) {
    txt <- readLines(normalizePath(filepath))
    txt <- gsub(pattern = pattern, replacement = replacement, x = txt)
    writeLines(txt, con = normalizePath(filepath))
}

research_project_skeleton <- function(
    path,
    author,
    title,
    git,
    ignore_pdf,
    ignore_binary_data,
    separate_analysis,
    bib,
    type,
    ...
) {
    dir.create(path)
    resources_path <- system.file(
        "rstudio", "templates", "project", "resources",
        package = "quack"
    )
    file.copy(
        from = file.path(resources_path, "README.md"),
        to   = file.path(normalizePath(path), "README.md")
    )
    if ( git ) {
        gert::git_init(path)
    }
    gitignore <- file.path(path, ".gitignore")
    if ( ignore_pdf ) {
        cat("## Ignore PDF output\n*.pdf\n\n", file = gitignore, append = TRUE)
    }
    if ( ignore_binary_data ) {
        cat(
            "## Ignore binary data\n*.rds\n*.RData\n\n",
            file = gitignore, append = TRUE
        )
    }
    cat(
        "## Ignore user-specific files\n.Rhistory\n.RData\n.Ruserdata\n",
        file = gitignore, append = TRUE
    )
    empirical <- type %in% c("Empirical", "Both")
    if ( empirical ) {
        dir.create(file.path(path, "data"))
        file.copy(
            from = file.path(resources_path, "data"),
            to   = file.path(normalizePath(path)),
            recursive = TRUE
        )
    }
    if ( separate_analysis ) {
        dir.create(file.path(path, "analysis"))
        file.copy(
            from = file.path(resources_path, "analysis"),
            to   = file.path(normalizePath(path)),
            recursive = TRUE
        )
    }
    if ( author != "" ) {
        if ( empirical ) {
            replace_file_text(
                filepath    = file.path(path, "data", "codebook.Rmd"),
                pattern     = "Your Names\\(s\\) Here",
                replacement = author
            )
        }
        if ( separate_analysis ) {
            replace_file_text(
                filepath    = file.path(path, "analysis", "analysis.Rmd"),
                pattern     = "Your Names\\(s\\) Here",
                replacement = author
            )
        }
    }
    if ( title != "" ) {
        replace_file_text(
            filepath    = file.path(path, "README.md"),
            pattern     = "^# Title Here",
            replacement = paste("#", title)
        )
        if ( empirical ) {
            replace_file_text(
                filepath    = file.path(path, "data", "codebook.Rmd"),
                pattern     = "Your Paper Title Here",
                replacement = title
            )
        }
        if ( separate_analysis ) {
            replace_file_text(
                filepath    = file.path(path, "analysis", "analysis.Rmd"),
                pattern     = "Your Paper Title Here",
                replacement = title
            )
        }
    }
    if ( bib != "" ) {
        file.copy(
            from = bib,
            to   = file.path(normalizePath(path), basename(bib))
        )
    }
    return(TRUE)
}
