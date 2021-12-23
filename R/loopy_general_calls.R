#' List Videos
#'
#' @description Lists videos hosted on Loopy, list can either be of videos uploaded to Loopy or those that were produced via a Loopy video processing operation.
#'
#' @param video_type string Videos that were either uploaded to Loopy ("uploaded") or produced by Loopy's video processing ("results").
#' @param verbose string A URL parameter which permits user access to all the files they have permission to access ('1') or to their files ('0'); default is '0'.
#'
#' @return list of Loopy videos with metadata.
#' @export
list_videos <-
  function(video_type = c("uploaded", "results"),
           verbose = "1") {

    # Check that user enter video_type matches with possible values.
    video_type <- tolower(video_type)
    video_type <- match.arg(video_type)

    url <- sprintf(
      "/api/1.0/videos/%s",
      video_type
    )

    response <-
      loopy_api(
        url = url,
        verbose = verbose
      )

    return(response$content)
  }

#' Get Video Info
#'
#' @description Gets metadata for a user-specified video.
#'
#' @param video_id string A URL parameter which limits return to only those with the video id.
#' @param verbose string A URL parameter which permits user access to all the files they have permission to access ('1') or to their files ('0'); default is '0'.
#'
#' @return list List of Loopy metadata for user-specified videos.
#' @export
get_video_info <-
  function(video_id, verbose = "1") {
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
#' @param verbose string A URL parameter which permits user access to all the files they have permission to access ('1') or to their files ('0'); default is '0'.
#' @param video_id string A URL parameter which limits return to only those with the video id.
#' @param collection_id string A URL parameter which limits the return to only those from the specified collection.
#'
#' @return list A list of metadata for the results of Loopy's image processing operations.
#' @export
list_results <-
  function(video_id = NULL, collection_id = NULL, verbose = "1") {
    url <- "/api/1.0/results"

    response <- loopy_api(
      url = url,
      verbose = verbose,
      video_id = video_id,
      collection_id = collection_id
    )

    return(response$content)
  }
