#' List Videos
#'
#' @description Lists videos hosted on Loopy, list can either be of videos uploaded to Loopy or those that were produced via a Loopy video processing operation.
#'
#' @param video_type string Videos that were either uploaded to Loopy ("uploaded") or produced by Loopy's video processing ("results").
#' @param verbose Boolean A URL parameter which permits user access to all the files they have permission to access (TRUE) or to their files (FALSE); default is TRUE.
#' @param cache Boolean TRUE/FALSE whether return object should be cached for easier access
#' @param override_cache Boolean TRUE/FALSE if existing cache should be overridden by current call
#'
#' @return list of Loopy videos with metadata.

list_videos <-
  function(video_type = c("uploaded", "results"),
           verbose = TRUE,
           cache = TRUE,
           override_cache = FALSE) {

    # Check that user enter video_type matches with possible values.
    video_type <- tolower(video_type)
    video_type <- match.arg(video_type)

    # Check if cache already exists
    type <- paste0(video_type, "_video")
    cache_exists <- .check_cache(type)

    if(cache_exists & override_cache == FALSE){
      out <- .get_cache(type)
    }else{
      url <- sprintf("/api/1.0/videos/%s", video_type)
      response <- loopy_api(url = url,verbose = verbose)
      out <- response$content
      if(cache == TRUE){
        .cache_content(out, type = type, override_cache = override_cache)
      }
    }

    return(out)
  }

#' Get Video Info
#'
#' @description Gets metadata for a user-specified video.
#'
#' @param video_id string A URL parameter which limits return to only those with the video id.
#' @param verbose boolean A URL parameter which permits user access to all the files they have permission to access (TRUE) or to their files (FALSE); default is TRUE.
#'
#' @return list List of Loopy metadata for user-specified videos.

get_video_info <-
  function(video_id, verbose = TRUE) {
    url <- sprintf(
      "/api/1.0/video/%s/info", video_id
    )

    response <- loopy_api(url = url, verbose = verbose)

    return(response$content)
  }

#' List Results
#'
#' @description Produces a list of results from Loopy's image processing operations.
#'
#' @param verbose boolean A URL parameter which permits user access to all the files they have permission to access (TRUE) or to their files (FALSE); default is TRUE.
#' @param video_id string A URL parameter which limits return to only those with the video id.
#' @param collection_id string A URL parameter which limits the return to only those from the specified collection.
#'
#' @return list A list of metadata for the results of Loopy's image processing operations.

list_results <-
  function(video_id = NULL, collection_id = NULL, verbose = TRUE) {
    url <- "/api/1.0/results"

    response <- loopy_api(
      url = url,
      verbose = verbose,
      video_id = video_id,
      collection_id = collection_id
    )

    return(response$content)
  }
