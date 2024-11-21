#' General utilities for working with Redivis datasets
#'
#'

#' Set the desired theme for our output
setup_ui <- function() {
  .font <- "Source Sans Pro"
  theme_set(theme_bw(base_size = 14, base_family = .font))
  theme_update(panel.grid = element_blank(),
             strip.background = element_blank(),
             legend.key = element_blank(),
             panel.border = element_blank(),
             axis.line = element_line(),
             strip.text = element_text(face = "bold"))
}

#' @param our_dataset Already loaded dataset
#' @return Properties of the dataset
get_dataset_properties <- function(our_dataset) {
  return(our_dataset$properties)
}

# List all the datasets to which you have access in a particular organization.
# This is a way to get the update time and/or version of a dataset
# to compare with a previously downloaded copy


#' @param org_name Return datasets belonging to this Organization
#' @return List of datasets belonging to the organization
list_organization_datasets <- function(org_name = 'levante') {
  org <- redivis::organization(org_name)
  datasets <- org$list_datasets()

  # For debugging
  #for (dataset in datasets){
  #  print(dataset$properties$"name")
  #}
  return(datasets)
}

# List all the datasets you have created.
# This is a way to get the update time and/or version of a dataset
# to compare with a previously downloaded copy
# (Can we get the username for the current user ourselves?)
#' @param user_name List all datasets owned by this user
#' @return list of datasets owned by the user
list_user_datasets <- function(user_name) {

  # Datasets per user
  user <- redivis::user(user_name)
  datasets <- user$list_datasets()

  # For debugging
  #for (dataset in datasets){
  #  print(dataset$properties$"name")
  #}
  return(datasets)
}

# pick a specific dataset out of the list of datasets provided
#' @param dataset_list List of datasets to search.
#' @param dataset_name Name of dataset we are looking for.
#' @return The dataset we found.
find_dataset <- function(dataset_list, dataset_name) {
  found_dataset <- dataset_list[sapply(dataset_list, +
                                         function(x) x$name == dataset_name)]
  return(found_dataset)
}

