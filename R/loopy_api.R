#' Call Loopy API
#'
#' @description Wrapper for httr::GET that handles Loopy API requests.
#' @param url string Loopy URL file path (origin should be set with set_loopy_user)
#' @param verbose string A URL parameter which permits user access to all the files they have permission to access ('1') or to their files ('0'); default is '0'.
#' @param video_id string A URL parameter which limits return to only those with the video id.
#' @param collection_id string A URL parameter which limits the return to only those from the specified collection.
#'
#' @return list A list including parse JSON (or HTML) data, the URL used in the API request, and the status code of the request.


loopy_api <- function(url,
                      verbose = "0",
                      video_id = NULL,
                      collection_id = NULL) {
  if (!is.null(verbose)) {
    if (!as.character(verbose) %in% c("0", "1")) {
      warning(
        "verbose= must be either '0','1', or NULL; \n Defaulting to '0' (only user's files)"
      )
      verbose <- "0"
    }
  }
  # Creates a list and then filters out those that are set to NULL.
  # Some of the values in Loopy ids are "NULL" so there has to be a way to distinguish between those where the id=NULL and when the user doesn't know or care what the id is.
  params <- list(
    "video_id" = video_id,
    "collection_id" = collection_id,
    "verbose" = verbose
  )

  # Removes NULLs so that values won't be included in the parameters
  params[sapply(params, is.null)] <- NULL

  # Get the values defined with set_loopy_user
  # ? If a user wants to create a local .Renviron file will this still find it?
  loopy_origin <- Sys.getenv("LOOPY_ORIGIN")
  api <- Sys.getenv("LOOPY_API_KEY")

  # Make sure that the user properly set loopy origin and api key
  if (nchar(loopy_origin) == 0 | nchar(api) == 0) {
    stop(
      "Loopy URL and/or Loopy API key not set.\n",
      "Check that .Renviron file exists.\n",
      "You can set Loopy URL and API key by using set_loopy_user function"
    )
  }

  response <- httr::GET(
    url = sprintf("%s%s", loopy_origin, url),
    httr::add_headers(
      "Content-Type" = "application/json",
      "X-Api-Key" = api
    ),
    query = params
  )

  # Check for json or xml.
  # The xml check is probably not necessary.
  # Most of the relevant info is json format
  http_type <- httr::http_type(response)
  if (http_type == "application/json") {
    parsed <-
      jsonlite::fromJSON(httr::content(
        response,
        "text",
        encoding = "UTF-8"
      ),
      simplifyVector = FALSE
      )
  } else if (http_type == "text/html") {
    parsed <-
      xml2::read_html(httr::content(response, "text"))
    message("API returned html, not json")
  } else if (http_type == "application/x-hdf5") {
    parsed <- httr::content(response)
    message("LoopyR currently does not support returning the data in h5 format. \n Data has been returned in raw format.")
  } else if (http_type == "text/csv") {
    parsed <- httr::content(response)
  } else {
    stop("API did not return json, html, or csv.", call. = FALSE)
  }

  if (httr::http_error(response)) {
    stop(
      sprintf(
        "Loopy API request failed [%s]\n%s\n<%s>",
        httr::status_code(response),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }


  # Organizes the httr response
  # Eventually this should be made into a cleaner response
  structure(
    list(
      content = parsed,
      path = response$url,
      response = response$status_code
    ),
    class = "loopy_api"
  )
}

print.loopy_api <- function(x, ...) {
  # Defines print structure for loopy_api objects
  cat("<Loopy ", x$path, ">\n", sep = "")
  cat("Status code: ", x$response, "\n")
  str(x$content)
  invisible(x)
}
