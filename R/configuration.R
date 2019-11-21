#' Default parameters
#'
#' Used as defaults for most functions in this package, to simplify their use. These might need to
#' be changed if the location of the Google Drive assets change or the directory structure of the
#' website on the server is changed.
#'
#' @export
config <- list(
  website_root = "/data/www/OomyceteDB/dev",
  googledrive_root_url = "https://drive.google.com/drive/u/1/folders/1XJLFW_S9EzVOGgHCqg42H88dNtD91gRq",
  release_spreadsheet_name = "releases",
  release_spreadsheet_path = "data/releases.csv", # relative path from website root
  release_name_prefix = "release_",
  local_release_dir = "data/releases",  # relative path from website root
  blast_database_dir = "data/blast_databases",  # relative path from website root
  blast_path = "shiny_apps/blast/blast/bin"  # relative path from website root
)
