context("User side")
library(loopyR)

# Test goes though two passes.
#  The first test the condition where the user does not have a .Renviron file already.
#  In the second test, the .Renviron file already exists and has loopy user info that it overrides.

# TEST 1

# Test that user gets a message that a .Renviron file is created
# This is the expected message if a .Renviron file does not exist already.
testthat::expect_message(
  set_loopy_user(api_key = '3xamp134P1k3y',
                 loopy_origin = 'http://example-vpn:0000',
                 renviron_path = "./tests/"),
  ".Renviron file created at ./tests/"
)

# TEST 2

# If a .Renviron file already exists then the user should get a different message.
api_2 <- '53c0nd3xamp134P1k3y'
origin_2 <- 'http://second-example-vpn:0000'

testthat::expect_message(
  set_loopy_user(api_key = api_2,
                 loopy_origin = origin_2,
                 renviron_path = "./tests/"),
  "API key and Loopy URL origin written to .Renviron.\nPlease restart your R session to use the Loopy API key and URL."
)

# Check that changes are made
renv <- unlist(strsplit(readLines('./tests/.Renviron'), '='))
testthat::expect_match(renv[2], api_2)
testthat::expect_match(renv[4], origin_2)

# Remove the useless .Renviron file.
file.remove("./tests/.Renviron")


