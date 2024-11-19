#' Functions to cache and retrieve Redivis Datasets
#'
#'

dataset_cache_dir <- "./dataset_cache/"

# Save an in memory dataset to a cache location
cache_dataset <- function(our_dataset, cache_dir = dataset_cache_dir) {
  save(our_dataset, paste(cache_dir, our_dataset$name))
  #return(success_flag)
}

# Load a cached dataset into memory
retrieve_dataset <- function(dataset_name, cache_dir = dataset_cache_dir) {
  loaded_dataset <- load(paste(cache_dir, dataset_name))
  return(loaded_dataset)
}



