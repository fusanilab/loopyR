#' Set Loopy user
#'
#' Stores Loopy API key and URL in .Renviron, creates .Renviron if it doesn't exist.
#' @param api_key string Users API key.
#' @param loopy_url string Loopy URL.
#' @param renviron_path string Path where .Renviron is stored. Default is the user's home directory.
#' @example
#' set_loopy_user("u53r100pyAPIk3y", "http://aVPN-pathfor.somelab.university.ac.at:5000/")
#' @return Creates (if necessary) and appends user's Loopy API key and URL to user's .Renviron file.

set_loopy_user <- function(api_key,
                           loopy_url,
                           renviron_path = Sys.getenv("HOME")) {

  # Format api key and loopy url for storage in .Renviron file
  api_key <- paste0("LOOPY_API_KEY=", api_key)
  loopy_url <- paste0("LOOPY_URL=", loopy_url)

  # Find where .Reviron is or will be stored
  renviron <- sprintf("%s\\.Renviron", renviron_path)

  # Check if .Renviron file exists.
  if (!file.exists(renviron)) {
    # Create .Renvrion and give message to use where file is being created
    file.create(renviron)
    message(sprintf(".Renviron file created at %s", renviron_path))

    # Write the two key=value pairs to the new .Renviron file
    cat(
      c(api_key, loopy_url),
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
        replace = api_key,
        x = tx
      )
    } else {
      tx <- c(tx, api_key)
    }

    # Checks if LOOPY_URL already exists
    if (any(grepl(pattern = "LOOPY_URL=.*", tx))) {
      # Updates Loopy_URL if it already exists
      tx <- gsub(
        pattern = "LOOPY_URL=.*",
        replace = loopy_url,
        x = tx
      )
    } else {
      tx <- c(tx, loopy_url)
    }

    # Writes all of the key-value pairs
    writeLines(tx, con = renviron)
  }

  message(
    "API key and Loopy URL written to .Renviron.",
    "\n",
    "Please restart your R session to use the Loopy API key and URL."
  )
}
