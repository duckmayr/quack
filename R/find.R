find_README <- function(path = getwd()) {
    README <- normalizePath(path.expand(file.path(path, "README.md")))
    files_in_path <- normalizePath(list.files(path = path, full.names = TRUE))
    if ( !(README %in% files_in_path) ) {
        stop("Could not find README.md in ", path)
    }
    return(README)
}

find_files <- function(path = getwd()) {
    path <- normalizePath(path.expand(path))
    is_git_repo <- path == try(gert::git_find(path), silent = TRUE)
    if ( is_git_repo ) {
        files <- gert::git_ls(path)$path
        files <- setdiff(files, ".gitignore")
    } else {
        files <- list.files(path = path, recursive = TRUE, all.files = TRUE)
    }
    return(files)
}

find_packages <- function(path = getwd()) {
    detected_packages <- renv::dependencies(
        path = path, progress = FALSE, errors = "ignored"
    )
    detected_packages <- unique(detected_packages$Package)
    installed_packages <- as.data.frame(utils::installed.packages())
    indices <- which(installed_packages$Package %in% detected_packages)
    return(installed_packages[indices, c("Package", "Version")])
}
