#' Release a version of the database to the OomyceteDB
#'
#' This is an interactive function that walks the user through the steps needed
#' to add a release of OomyceteDB to the `oomycetedbdata` package. The
#' `oomycetedbdata` package is the source of the data for the OomyceteDB
#' website, so this will effectively add a release to the website. This function
#' will do the following things: * Validate that the database is formatted
#' correctly, as described in [the database format guide](database_format.html).
#' * Show the version of the database to be released and any changes to the user
#' for a manual check. * Either upload the release to Github or store/modify a
#' version of the `oomycetedbdata` package on the local computer for manual
#' upload.
#'
#' @param overwrite (`TRUE`/`FALSE` of length 1) Whether or not to replace the
#'   previous release with this release rather than creating a new one. Default:
#'   `FALSE`.
#' @param description (`character` of length 1) A description of this release.
#'   This will be shown publicly with the release. Default: prompt user.
#' @param validate (`TRUE`/`FALSE` of length 1) Whether or not to validate the
#'   database source before attempting to create a release. It is highly
#'   recommended to validate before a release. Default: TRUE.
#' @inheritParams validate
#'
#' @export
release <- function(
  database_path = NULL,
  release_path = NULL,
  interactive = TRUE,
  validate = TRUE,
  auto_update = FALSE,
  overwrite = FALSE,
  description = NULL,
  google_user = NULL,
  google_pass = NULL,
  github_user = NULL,
  github_pass = NULL,
  config_path = find_config('oomycetedbtools_config.yml')
) {

  # Look for and apply configuration file by overwriting defaults of unspecified arguments
  #  NOTE: This function modifies variables in this environment instead of returning values
  apply_config_file(config_path)

  # Validate parameter values

  # Validate input database format
  format_is_valid <- validate(
    database_path = database_path,
    interactive = interactive,
    auto_update = auto_update,
    google_user = google_user,
    google_pass = google_pass,
    github_user = github_user,
    github_pass = github_pass
  )
  if (! format_is_valid) {
    #exit
  }

  # Show differences between this and the last version to the user
  this_release <- format_release(release_path)
  last_release <- get_last_release(release_path)
  if (is.null(last_release)) {
    # "This is the first release"
  } else {
    compare_releases(this_release, last_release)
    # Ask to proceed
  }


  # Ask user for description and spell check description
  if (is.null(description)) {
    # Prompt user
  }


  # Update database with new release
  create_release(release_path)
}



