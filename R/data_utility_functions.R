#' General utilities for working with Redivis datasets
#'
#'

get_dataset_properties <- function(our_dataset) {
  return(our_dataset$properties)
}

# List all the datasets to which you have access in a particular organization.
# This is a way to get the update time and/or version of a dataset
# to compare with a previously downloaded copy
list_organization_datasets <- function(org_name = 'levante') {
  org <- redivis::organization(org_name)
  datasets <- org$list_datasets()

  # For debugging
  #for (dataset in datasets){
  #  print(dataset$properties$"name")
  #}
  return(datasets)
}

# pick a specific dataset out of the list of datasets provided
find_dataset <- function(dataset_list, dataset_name) {
  found_dataset <- dataset_list[sapply(dataset_list, +
                                         function(x) x$name == dataset_name)]
  return(found_dataset)
}

