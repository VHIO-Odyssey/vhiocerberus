env_ok <- c(
  VHIO_SQL_SERVER_USER = "env_user",
  VHIO_SQL_SERVER_PASSWORD = "env_password",
  VHIO_SQL_SERVER_IP = "env.server"
)

test_that("reads every setting from the environment variables", {
  withr::local_envvar(env_ok)

  expect_equal(
    vhio_credentials(),
    list(user = "env_user", password = "env_password", server = "env.server")
  )
})

test_that("an explicit argument wins over the environment variable", {
  withr::local_envvar(env_ok)

  expect_equal(vhio_credentials(user = "explicit")$user, "explicit")
  expect_equal(vhio_credentials(user = "explicit")$server, "env.server")
})

test_that("a missing setting aborts naming the variable", {
  withr::local_envvar(c(env_ok, VHIO_SQL_SERVER_PASSWORD = NA))

  expect_error(vhio_credentials(), "VHIO_SQL_SERVER_PASSWORD")
})

test_that("an empty environment variable counts as missing", {
  withr::local_envvar(c(env_ok, VHIO_SQL_SERVER_IP = ""))

  expect_error(vhio_credentials(), "VHIO_SQL_SERVER_IP")
})

test_that("verify_oncoclinical_data() fails before connecting if settings are missing", {
  withr::local_envvar(c(
    VHIO_SQL_SERVER_USER = NA,
    VHIO_SQL_SERVER_PASSWORD = NA,
    VHIO_SQL_SERVER_IP = NA
  ))

  testthat::with_mocked_bindings(
    expect_error(verify_oncoclinical_data("breast"), "VHIO_SQL_SERVER"),
    connect_to_vhio_database = function(...) stop("should not connect")
  )
})
