#' Functions to cache and retrieve Redivis Datasets
#'
#'

# Not sure if cache should be per project or per user
# For now set the default to be under the current project
# file sep on Windows returns "/" so we need to normalize
canonical_home = normalizePath(Sys.getenv("Home"), winslash = '/')
dataset_cache_dir <- file.path(canonical_home,".levante", fsep = .Platform$file.sep)

# Create the cache dir if it doesn't already exist
if (dir.exists(dataset_cache_dir) == FALSE)
  dir.create(dataset_cache_dir)

# Save an in memory dataset to a cache location
# We also want to save at least the last_updated time for re-loading
# so we save all the properties in case we need others
# Annoyingly, the dataset doesn't include its name or its
# properties (like last_updated). Fixing that might require
# some modification of get_datasets so we know what we got.
cache_dataset <- function(our_dataset, our_dataset_properties, cache_dir = dataset_cache_dir) {
  cache_file <- file.path(cache_dir, our_dataset$name, fsep = .Platform$file.sep)
  save(our_dataset, file = cache_file)

  # Get dataset last modified time
  # Set new last modified time to the dataset last-modified time
  new_time <- as.POSIXct((our_dataset_properties$updatedAt/1000),
        format="%Y-%m-%dT%H:%M")

  # Update the file's last modified time
  Sys.setFileTime(cache_file, new_time)

  # Return value is mostly for debugging
  return(paste(cache_file, new_time))
}

# See if cached dataset is up to date
check_dataset <- function(dataset_name, cache_dir = dataset_cache_dir) {
  # first get the last update time for the Redivis dataset
  # and convert from milliseconds to seconds for use in R
  dataset_last_update_raw <- get_dataset_properties(dataset_name, org = 'levante')$updatedAt
  dataset_last_update <- as.POSIXct((dataset_last_update_raw/1000),
                            format="%Y-%m-%dT%H:%M")

  # now get the equivalent date from the cache
  cache_file <- file.path(cache_dir, dataset_name, fsep = .Platform$file.sep)
  file_info <- file.info(cache_file)
  cache_modified_time <- file_info$mtime

  # We're not trying for sub-second precision, but if the dataset's
  # update time is meaningfully later than the file modified time,
  # then we have a cache miss. We get the results in seconds
  cache_diff <- difftime(dataset_last_update, cache_modified_time, units = "secs")
  if (cache_diff > 120) # leave 2 minutes in case of possible glitches
    # cache miss
    return(FALSE)
  else
    # cache hit
    return(TRUE)

}

# Load a cached dataset into memory when it has been
# determined that is newer
retrieve_dataset <- function(dataset_name, cache_dir = dataset_cache_dir) {
  loaded_dataset_name <- load(file.path(cache_dir, dataset_name, fsep = .Platform$file.sep))
  # now how do we return the contents of loaded_dataset_name?
  return(get(loaded_dataset_name))
}



