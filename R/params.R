#' Title: biblio_ds
#'
#' @param db_params A list of parameters for the database
#' @param db_params$raw_data_path The path to the raw data
#' @param db_params$out_path The path to the output folder
#' @param db_params$origin The origin of the data
#' @param db_params$format The format of the data
#' @param db_params$delim The delimiter/separator of the data
#' @param db_params$years The years to consider
#' @param db_params$n_max The maximum number of elements to consider
#'
#' @description
#' It reads the data from the raw data path, converts it to a data frame, and
#' filters only the articles in English. It also removes duplicates from the data
#' frame. It modifies globally the db_params argument
#'
#'
#' @return A data frame with the data
#' @export
#'
biblio_ds <- function(db_params) {
## Set the path to the folder where the data will be stored
## This folder will be created if it does not exist


## Set the path to the folder where the output will be stored
## This folder will be created if it does not exist
db_params$out_path_figs <<- file.path(db_params$out_path, "figures")
db_params$out_path_figs_web <<- file.path(db_params$out_path, "html")
db_params$out_path_summaries <<- file.path(db_params$out_path, "summaries")
db_params$out_path_figs <- file.path(db_params$out_path, "figures")
db_params$out_path_figs_web <- file.path(db_params$out_path, "html")
db_params$out_path_summaries <- file.path(db_params$out_path, "summaries")

paths = c(db_params$out_path, db_params$out_path_figs,
          db_params$out_path_summaries, db_params$out_path_figs_web)
for (p in paths) {
  if (!dir.exists(p)) {
    dir.create(p, recursive = TRUE, showWarnings = FALSE)
  }
}


if (file.exists(file.path(db_params$out_path, "M.rda"))) {
  load(file.path(db_params$out_path, "M.rda"))
  return (M)
} else {
  ## Glob the files in the path
  datasets <- file.path(db_params$raw_data_path, list.files(db_params$raw_data_path))

  ## Convert the files to a data frame
  M <- convert2df(
    datasets,
    dbsource = db_params$origin,
    format = db_params$format,
    remove.duplicates = TRUE
  )
  M <- M[M$LA == "ENGLISH", ]

  ## Filter only articles
  M <- dplyr::filter(M, grepl("ARTICLE", M$DT))

  M <- duplicatedMatching(M,
                          Field = "SR",
                          exact = FALSE,
                          tol = 0.95)
  M <- duplicatedMatching(M,
                          Field = "SR_FULL",
                          exact = FALSE,
                          tol = 0.95)
  save(M, file = file.path(db_params$out_path, "M.rda"))
  return (M)
}
}
