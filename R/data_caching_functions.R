#' Functions to cache and retrieve Redivis Datasets
#'
#'

# Not sure if cache should be per project or per user
# For now set the default to be under the current project
dataset_cache_dir <- "./dataset_cache/"

# Save an in memory dataset to a cache location
# We also want to save at least the last_updated time for re-loading
# so we save all the properties in case we need others
# Annoyingly, the dataset doesn't include its name or its
# properties (like last_updated). Fixing that might require
# some modification of get_datasets so we know what we got.
cache_dataset <- function(our_dataset, cache_dir = dataset_cache_dir) {
  save(our_dataset, paste(cache_dir, our_dataset$name))
  #return(success_flag)
}

# Load a cached dataset into memory
retrieve_dataset <- function(dataset_name, cache_dir = dataset_cache_dir) {
  loaded_dataset <- load(paste(cache_dir, dataset_name))
  return(loaded_dataset)
}



