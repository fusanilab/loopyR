#' Set Loopy user
#'
#' Stores Loopy API key and URL in .Renviron, creates .Renviron if it doesn't exist.
#' @param api_key string Users API key.
#' @param loopy_origin string Loopy URL origin (protocol, subdomain, domain name and port, where applicable); Paths, querys, and parameters are specified in other functions.
#' @param renviron_path string Path where .Renviron is stored. Default is the user's home directory.
#' @return Creates (if necessary) and appends user's Loopy API key and URL to user's .Renviron file.

set_loopy_user <-
  function(api_key,
           loopy_origin,
           renviron_path = Sys.getenv("HOME")) {

    # Format api key and loopy url for storage in .Renviron file
    api_key <- paste0("LOOPY_API_KEY=", api_key)
    loopy_origin <- paste0("LOOPY_ORIGIN=", loopy_origin)

    # Find where .Reviron is or will be stored
    renviron <- sprintf("%s/.Renviron", renviron_path)

    # Check if .Renviron file exists.
    if (!file.exists(renviron)) {
      # Create .Renvrion and give message to use where file is being created
      file.create(renviron)
      message(sprintf(".Renviron file created at %s", renviron_path))

      # Write the two key=value pairs to the new .Renviron file
      cat(
        c(api_key, loopy_origin),
        file = renviron,
        sep = "\n",
        append = T
      )
    } else {
      # If .Renviron file already exists at specified path
      message(sprintf("Writing to .Renviron file at %s", renviron_path))

      # Store lines so nothing is overwritten
      tx <- readLines(renviron)

      # Checks if Loopy_API_KEY already exists
      if (any(grepl(pattern = "LOOPY_API_KEY=[A-z0-9]", tx))) {
        # Updates LOOPY_API_KEY if it already exists
        tx <- gsub(
          pattern = "LOOPY_API_KEY=[A-z0-9]*",
          replacement = api_key,
          x = tx
        )
      } else {
        tx <- c(tx, api_key)
      }

      # Checks if LOOPY_ORIGIN already exists
      if (any(grepl(pattern = "LOOPY_ORIGIN=.*", tx))) {
        # Updates LOOPY_ORIGIN if it already exists
        tx <- gsub(
          pattern = "LOOPY_ORIGIN=.*",
          replacement = loopy_origin,
          x = tx
        )
      } else {
        tx <- c(tx, loopy_origin)
      }

      # Writes all of the key-value pairs
      writeLines(tx, con = renviron)
    }

    message(
      "API key and Loopy URL origin written to .Renviron.",
      "\n",
      "Please restart your R session to use the Loopy API key and URL."
    )
  }


check_loopy_user <- function(renviron_path = Sys.getenv("HOME")){

  cred_check <- check_reviron(renviron_path = renviron_path)

  if(sum(cred_check) != 3){
    cred_msg <- "Set using set_loopy_user function."
    if(cred_check['renviron'] == FALSE){
      message(paste(".Renviron not found; create manually or use set_loopy_user."))
    }
    if(cred_check['key'] == FALSE){
      message(paste("Loopy API key not found;", cred_msg))
    }
    if(cred_check['origin'] == FALSE){
      message(paste("Loopy origin not found;", cred_msg))
    }
  }else{
    tryCatch(
      {
        call <- loopy_api(url = "/api/1.0/results")
        call_msg <- sprintf("Loopy call returned status of %d;", call$status_code)
        if(call$status_code == 200){
          message(paste(call_msg, "Attempt to access Loopy was successful! :)"))
        }else if(call$status_code >= 300){
          ### change to be more informative.
          message(
            paste(
              call_msg,
              "Attempt to access Loopy was unsuccesful. :( \n"
            )
          )
        }
      },
      error = function(e){
        if(grepl("url", e)){
          message("There is a problem with the Loopy origin URL:\n")
        }else if(grepl("API", e)){
          message("There is a problem with the Loopy API key:\n")
        }
        message(paste(e, "\n"))
      }
    )
  }
}

#' Check .Renviron
#'
#' @description Function that checks if an .Renviron file exists and checks if
#'     Loopy credentials are present.
#'
#' @param renviron_path
#'
#' @return
#' @noRd
check_reviron <- function(renviron_path = Sys.getenv("HOME")){

  data_status <- c(renviron = FALSE, key = FALSE, origin = FALSE)

  # Check that .Renviron, API key, and Loopy origin exist
  renviron <- sprintf("%s/.Renviron", renviron_path)
  key <- Sys.getenv("LOOPY_API_KEY")
  origin <- Sys.getenv("LOOPY_ORIGIN")


  if (file.exists(renviron)) {
    data_status['renviron'] <- TRUE
  }
  if(nchar(key) != 0){
    data_status['key'] <- TRUE

  }
  if(nchar(origin) != 0){
    data_status['origin'] <- TRUE
  }

  return(data_status)
}


