context("User side")
library(loopyR)

# Test goes though two passes.
#  The first test the condition where the user does not have a .Renviron file already.
#  In the second test, the .Renviron file already exists and has loopy user info that it overrides.

# TEST 1

# Test that an .Renviron file is created and user details are added.

api_1 <- '3xamp134P1k3y'
origin_1 <- 'http://example-vpn:0000'

# Tests the expected message if a .Renviron file does not exist already.
testthat::expect_message(
  set_loopy_user(api_key = api_1,
                 loopy_origin = origin_1,
                 renviron_path = "./tests/"),
  ".Renviron file created at ./tests/"
)

# Check that the loopy user details are added to .Renviron file.
renv1 <- unlist(strsplit(readLines('./tests/.Renviron'), '='))
testthat::expect_match(renv1[2], api_1)
testthat::expect_match(renv1[4], origin_1)


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
renv2 <- unlist(strsplit(readLines('./tests/.Renviron'), '='))
testthat::expect_match(renv2[2], api_2)
testthat::expect_match(renv2[4], origin_2)

# Remove the useless .Renviron file.
file.remove("./tests/.Renviron")


