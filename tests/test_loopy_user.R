library(loopyR)

# Test that user gets a message that a .Renviron file is created
# This is the expected message if a .Renviron file does not exist already.
testthat::expect_message(
  set_loopy_user(api_key = '3xamp134P1k3y',
                 loopy_origin = 'http://example-vpn:0000',
                 renviron_path = "./tests/"),
  ".Renviron file created at ./tests/"
)

# If a .Renviron file already exists then the user should get a different message.
testthat::expect_message(
  set_loopy_user(api_key = '53c0nd3xamp134P1k3y',
                 loopy_origin = 'http://second-example-vpn:0000',
                 renviron_path = "./tests/"),
  "API key and Loopy URL origin written to .Renviron.\nPlease restart your R session to use the Loopy API key and URL."
)

# Remove the useless .Renviron file.
file.remove("./tests/.Renviron")


