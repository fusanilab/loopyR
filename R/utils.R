# Helper functions

#' Check if cache exists
#'
#' @description Checks if cache and cache environment exist.
#'  Creates .loopy_temp_cache if it doesn't exist.
#'
#' @param type string One of either "uploaded_video", "results_video" or "results"
#' @return Boolean
#' @noRd
.check_cache <- function(type = c("uploaded_video", "results_video", "results")) {
  ## Creates a hidden environment to store the downloaded data
  if (!exists(".loopy_temp_cache")) {
    .loopy_temp_cache <<- new.env(parent = emptyenv())
  }
  if (type %in% ls(.loopy_temp_cache)) {
    out <- TRUE
  } else {
    out <- FALSE
  }
  return(out)
}

#' Cache content from loopy
#'
#' @description stores the large return loopy calls in the environment .loopy_temp_cache
#' @param content Loopy content to be cached
#' @param type string One of either "uploaded_video", "results_video" or "results"
#' @param override_cache Boolean If existing cache should be overridden
#' @noRd
.cache_content <-
  function(content,
           type = c("uploaded_video", "results_video", "results"),
           override_cache = FALSE) {
    cache_exists <- .check_cache(type = type)

    if (!cache_exists | override_cache == TRUE) {
      if (type == "uploaded_video") {
        .loopy_temp_cache$uploaded_video <<- content
      } else if (type == "results_video") {
        .loopy_temp_cache$results_video <<- content
      } else {
        .loopy_temp_cache$results <<- content
      }
    }
  }

#' Get cached content
#' @description Retrieves cached content.
#' @param type string One of either "uploaded_video", "results_video" or "results"
#'
#' @return Cached loopy content
#'
#' @noRd
.get_cache <- function(type = c("uploaded_video", "results_video", "results")) {
  cache_exists <- .check_cache(type = type)
  if (cache_exists) {
    message("fetching cached content")
    return(get(type, envir = .loopy_temp_cache))
  } else {
    msg <- sprintf("cache for %s doesn't exist; download content from loopy first", type)
    warning(msg)
  }
}
