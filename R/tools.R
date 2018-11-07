get_blast_databases <- function(db_dir) {
  # Get possible blast database names
  db_file_names <- tools::file_path_sans_ext(list.files(db_dir))
  output <- unique(db_file_names)

  # Remove any that do not appear the correct number of times
  is_database <- vapply(output, function(x) as.list(table(db_file_names))[[x]] == 6, logical(1))
  output <- output[is_database]

  return(rev(output))
}


get_latest_release_fa <- function() {
  local_release_names <- list.files(local_release_dir, pattern = paste0(release_name_prefix, "[0-9]+\\.fa"))
  local_release_nums <- stringr::str_match(local_release_names, pattern = paste0(release_name_prefix, "([0-9]+)\\.fa"))[, 2]
  local_release_nums <- as.numeric(local_release_nums)
  file.path(local_release_dir, local_release_names[which.max(local_release_nums)])
}



#' Taken from https://stackoverflow.com/questions/5076593/how-to-determine-if-you-have-an-internet-connection-in-r
has_internet <- function(){
  !is.null(curl::nslookup("r-project.org", error = FALSE))
}
