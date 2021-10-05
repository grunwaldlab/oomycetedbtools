#' Find config file in a parent directory
#'
#' Search all parent directories of a given directory and return the path to the
#' first file found with a given name.
#'
#' @param file_name The name of the file.
#' @param dir_path The directory to start the search from. Defaults to current
#'   working directory.
#' @param regex TRUE/FALSE. Is the `file_name` option a regular expression?
#'
#' @export
find_config <- function(file_name, dir_path = getwd(), regex = FALSE) {
  matching_files <- character(0)

  for (path in parent_dir_paths(dir_path)) {

    # Get all files in this directory
    file_paths <- list.files(path, recursive = FALSE, full.names = TRUE, all.files = TRUE)
    file_paths[!file.info(file_paths)$isdir]

    # Look for target file
    matching_files <- file_paths[grepl(basename(file_paths), pattern = file_name, fixed = !regex)]
    if (length(matching_files) > 0) {
      break()
    }

  }

  # Check that a single file was found
  if (length(matching_files) == 0) {
    stop('Did not find any files matching "', file_name, '"', call. = FALSE)
  } else if (length(matching_files) == 0) {
    warning('Found multiple files matching "', file_name,
            '". Only using the first file, called: "', matching_files[1], '"', call. = FALSE)
    matching_files = matching_files[1]
  }

  return(matching_files)
}



#' Get paths to all parent directories
#'
#' Returns a vector with the full paths to all parent directories of a directory.
#'
#' @param dir_path The directory to start the search from. Defaults to current
#'   working directory.
#'
#' @keywords internal
parent_dir_paths <- function(dir_path = getwd()) {
  parent_path <- dirname(dir_path)
  if (parent_path == dir_path) { # If the root is reached
    return(parent_path)
  } else {
    return(c(dir_path, parent_dir_paths(parent_path)))
  }
}


#' Apply defaults arguments from YAML
#'
#' Load a YAML file and rerun the parent function using the arguments present in the YAML file.
#'
#' @keywords internal
apply_config_file <- function(config_path, ...) {
  # Get arguments specified by user
  arguments <- as.list(match.call(call = sys.call(-1), definition = sys.function(-1)))[-1]

  # Load YAML file
  config_args <- yaml::read_yaml(config_path)

  # Assign values from config file for arguments not specified in function
  args_to_seq <- config_args[! names(config_args) %in% names(arguments)]
  for (i in seq_len(length(args_to_seq))) {
    assign(names(args_to_seq)[[i]], args_to_seq[[i]], pos = parent.frame())
  }

  # Return modified argument list
}

#' Find the folder with the release files
#'
#' Find the folder with the release files
#'
#' @param path A path to a folder with a oomycetedbtools config file.
#'
#' @export
find_release_dir <- function(path = getwd()) {
  apply_config_file(find_config('oomycetedbtools_config.yml', path))

  return(release_path)
}
