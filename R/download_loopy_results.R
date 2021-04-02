#' Download Tracking Data
#'
#' @description Downloads and returns tracking data as a data frame.
#'
#' @param asset_id string The asset_id that Loopy creates for each result.
#' @param group_key string The group_key that Loopy creates for each result.
#'
#' @return data.frame Tracking data.

download_tracking_data <-
  function(asset_id, group_key) {
    # This function uses the main loopy_api function, but the call might be different enough to warrant a whole new function.
    url <- sprintf("/api/1.0/result/download/asset/%s", asset_id)

    response <- loopy_api(
      url = url,
      asset_id = asset_id,
      group_key = group_key,
      format = "csv",
      verbose = NULL
    )

    if (response$content$status == "in-progress" | response$content$status == "generate-asset") {
      # Checks the job status of the request. If status turns to 200 it downloads the data.
      try <- 0
      time <- 1
      # Makes a maximum of 5 attempts
      n_tries <- 5
      # Wait 1 sec so that the first call isn't immediate.
      Sys.sleep(time)
      message("Data needs to be prepared:", "\n", "Function will make 5 attempts to download the data.")
      while (try < n_tries) {
        job_status <- check_job_status(response$content$job_id, logging = TRUE)
        if (job_status == 200) {
          break
        } else {
          Sys.sleep(time = time)
          # exponential backoff
          time <- time * 2
          try <- try + 1
        }
      }

      if (try == 5) {
        warning("Request timed out: Try call again later")
      } else {
        # Make the call again
        response <- loopy_api(
          url = url,
          asset_id = asset_id,
          group_key = group_key,
          format = "csv"
        )
      }
    }

    response$content
  }



#' Check job status
#'
#' @description Checks if a Loopy API request is in progress, finished, or failed.
#' Function needs quite a bit of checking.
#'
#' @param job_id string Loopy generated id for preparing or caching data requests.
#' @param logging boolean If set to TRUE will write output to logfile
#' @return numeric HTTP status code.
#'

check_job_status <-
  function(job_id, logging = TRUE) {
    # This function needs a bit more testing
    url <- sprintf("/api/1.0/job/%s/status", job_id)

    response <- loopy_api(url, verbose = "1")

    if(logging == TRUE){
      # Create a directory for storing job status checks
      if(!dir.exists("log")){
        dir.create("log")
      }
      if(!file.exists("log\\job_status.txt")){
        file.create("log\\job_status.txt")
      }

      cat("=======================", "\n", file="log\\job_status.txt", append = TRUE)
      cat(as.character(Sys.time()), "\n", file = "log\\job_status.txt", append = TRUE)
      capture.output( response, file = "log\\job_status.txt", append=TRUE)
      cat("=======================", "\n", file = "log\\job_status.txt", append = TRUE)

    }

    if (response$content$status == "finished") {
      message("Job is finished: Data has been prepared/cached")
      status_code <- response$status_code
    } else if (response$content$status == "in-progress") {
      message("Data is being prepared")
      status_code <- response$status_code
    } else {
      ## I have to do more testing to see what the range of responses are.
      status_code <- response$status_code
      warning("Data preparation may have failed.", "\n", "Status_code is: ", status_code)
    }
    return(status_code)
  }
