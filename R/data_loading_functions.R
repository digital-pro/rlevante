#' Get redivis datasets
#'
#' Retrieve LEVANTE project datasets from Redivis and prepare them for further analysis.
#'
#' @return A list of one or more datasets from a specific organization in the Redivis repository. In our case that is normally "levante"
#' @export
#' @examples

# remember to specify at least one table in addition to the dataset name(s)
get_datasets <- function(dataset_names, org_name = "levante", tables = NULL) {
  org <- redivis::organization(org_name)

  # what we want to have happen
  datasets <- dataset_names |> rlang::set_names() |> purrr::map(\(dn) fetch_dataset(dn, org))
  #datasets <- dataset_names |> rlang::set_names() |> purrr::map(\(dn) org$dataset(dn))

  get_table_names <- \(ds) if (!is.null(tables)) tables else ds$list_tables() |> purrr::map(\(t) t$name)
  get_dataset_tables <- \(ds) ds |> get_table_names() |> rlang::set_names() |> purrr::map(\(tn) ds$table(tn)$to_tibble())

  return_value <- purrr::map(datasets, get_dataset_tables)

  # But first we want to cache all the datasets. Some might have been cached previously,
  # however on a local disk it is not expensive (until we get smarter:))
  purrr::map(datasets, cache_dataset)
  return(return_value)
}

# get dataset from cache or Redivis
# this should probably be in the data_caching_functions?
fetch_dataset <- function(dn, org) {
  cache_ok <- check_dataset(dn)
  if (cache_ok)
    #print('CACHE OKAY')
    return(retrieve_dataset(dn))
  else
    # WE don't seem to have all the info we need to get a full cache
    # so try delaying until after tables are loaded

    # first, get it from redivis
    our_dataset <- org$dataset(dn)
    # we also need properties to get last update time
    # NOTE: this call has parameters "backwards"!
    #our_dataset_properties <- get_dataset_properties('levante', dn)
    #cache_dataset(our_dataset, our_dataset_properties)
    return(our_dataset)
}

# Get full datasets, not just tables
get_datasets_full <- function(dataset_names, org_name = "levante") {
  org <- redivis::organization(org_name)
  datasets <- dataset_names |> rlang::set_names() |> purrr::map(\(dn) org$dataset(dn))
}

# Get a single full dataset, not just tables
get_dataset_full <- function(dataset_name, org_name = "levante") {
  org <- redivis::organization(org_name)
  dataset <- org$dataset(dataset_name)
}

# This should be simpler, but when we retrieve a single dataset
# we don't seem to get its properties
get_dataset_properties <- function(dataset_name, org_name = "levante") {
  org <- redivis::organization(org_name)
  dataset <- org$dataset(dataset_name)
  properties <- dataset$properties
}


#' Fix some stuff in tables
#'
#' There can be some anomalies in LEVANTE data stored in Redivis. This function
#' will clean up some of the most common
#'
#' @return A Levante specific function to clean up some known potential data issues.
#' @export
#' @examples

fix_table_types <- function(table_data) {
  table_data |>
    dplyr::mutate(dplyr::across(dplyr::where(rlang::is_character),
                                \(x) x |> dplyr::na_if("null") |> dplyr::na_if("None"))) |>
    dplyr::mutate(dplyr::across(dplyr::matches("birth_"), as.integer),
                  dplyr::across(dplyr::matches("difficulty"), as.double),
                  dplyr::across(dplyr::matches("rt"), as.character),
                  dplyr::across(dplyr::matches("email_verified|is_reliable|is_bestrun"), as.logical))
}

#' Combine tables into a single megatable
#'
#' longer description
#'
#' @return For the case where the data you're using is in multiple tables, this
#'  function will combine them into one single large table if needed.
#'
#' @export
#' @examples

combine_datasets <- function(dataset_tables) {
  all_table_names <- purrr::map(dataset_tables, names) |> unlist() |> unique()
  dataset_tables |>
    purrr::map(\(ds) ds |> purrr::map(\(t) fix_table_types(t))) |>
    purrr::list_transpose(template = all_table_names, simplify = FALSE) |>
    purrr::map(list_rbind)
  # map(\(dt) list_rbind(dt, names_to = "dataset_name"))
}

#' Collect user data
#'
#' longer description
#'
#' @return Assemble & join user data with groups
#'
#' @export
#' @examples

collect_users <- function(dataset_data) {
  distinct(dataset_data$users) |>
    left_join(distinct(dataset_data$user_groups),
              by = "user_id", relationship = "many-to-many") |>
    left_join(distinct(dataset_data$groups),
              by = "group_id", suffix = c("_user", "_group"))
}
