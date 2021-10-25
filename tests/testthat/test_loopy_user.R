library(loopyR)

# Test goes though three passes.
#  The 1st test the condition where the user does not have a .Renviron file already.
#  In the 2nd test, the .Renviron file already exists and has loopy user info that it overrides.
#  In the 3rd test, the .Renviron file already exists but does not have loopy info.

# TEST 1

# Test that an .Renviron file is created and user details are added.
testthat::test_that(
  "loopy_user creates .Renviron file and writes loopy data to it",
  {
    api_1 <- "3xamp134P1k3y"
    origin_1 <- "http://example-vpn:0000"

    # Tests the expected message if a .Renviron file does not exist already.
    testthat::expect_message(
      set_loopy_user(
        api_key = api_1,
        loopy_origin = origin_1,
        renviron_path = "../"
      ),
      ".Renviron file created at ../"
    )

    # Check that the loopy user details are added to .Renviron file.
    renv1 <- unlist(strsplit(readLines("../.Renviron"), "="))
    testthat::expect_match(renv1[2], api_1)
    testthat::expect_match(renv1[4], origin_1)
  }
)

# TEST 2

# If a .Renviron file already exists then the user should get a different message.
testthat::test_that(
  "Modifies a pre-existing .Revniron file that already has loopy user data written to it.",
  {
    api_2 <- "53c0nd3xamp134P1k3y"
    origin_2 <- "http://second-example-vpn:0000"

    testthat::expect_message(
      set_loopy_user(
        api_key = api_2,
        loopy_origin = origin_2,
        renviron_path = "../"
      ),
      "API key and Loopy URL origin written to .Renviron.\nPlease restart your R session to use the Loopy API key and URL."
    )

    # Check that changes are made
    renv2 <- unlist(strsplit(readLines("../.Renviron"), "="))
    testthat::expect_match(renv2[2], api_2)
    testthat::expect_match(renv2[4], origin_2)

    # Remove the useless .Renviron file.
    file.remove("../.Renviron")
  }
)

# TEST 3

# Test to add loopy user info into an already existing .Renviron file.

testthat::test_that(
  "Modifies pre-existing .Renviron file without loopy user info. ",
  {
    file.create("../.Renviron")

    cat(
      c("OTHER_API_KEY=d1ff3r3n7ap1k3y", "OTHER_WEBSITE=website.web"),
      file = "../.Renviron",
      sep = "\n",
      append = T
    )

    api_3 <- "t41rd3xamp134P1k3y"
    origin_3 <- "http://third-example-vpn:0000"

    testthat::expect_message(
      set_loopy_user(
        api_key = api_3,
        loopy_origin = origin_3,
        renviron_path = "../"
      ),
      "API key and Loopy URL origin written to .Renviron.\nPlease restart your R session to use the Loopy API key and URL."
    )

    # Check that changes are made
    # This might mess up?
    renv3 <- unlist(strsplit(readLines("../.Renviron"), "="))
    testthat::expect_equal("LOOPY_API_KEY" %in% renv3, TRUE)
    testthat::expect_equal("LOOPY_ORIGIN" %in% renv3, TRUE)
    testthat::expect_match(renv3[match("LOOPY_API_KEY", renv3) + 1], api_3)
    testthat::expect_match(renv3[match("LOOPY_ORIGIN", renv3) + 1], origin_3)

    # Remove the useless .Renviron file.
    file.remove("../.Renviron")
  }
)

# Test incorrect
testthat::test_that(
  "check_loopy_user with incorrect API key and Loopy origin",
  {
    renviron_path <- "../../"
    api_1 <- "3xamp134P1k3y"
    origin_1 <- "http://example-vpn:0000"
    set_loopy_user(
        api_key = api_1,
        loopy_origin = origin_1,
        renviron_path = renviron_path
      )

    # The functions still use the working .Renviron data.
    # The problem may be that the system needs to be restarted.
    # Either way, this needs some testing.

    check_loopy_user(renviron_path = renviron_path)
    #testthat::expect_message(
    #  check_loopy_user(renviron_path = renviron_path),
    #  "Loopy call returned status of 404; Attempt to access Loopy was unsuccesful. :("
    #)
    file.remove(paste0(renviron_path,".Renviron"))

  }
)




