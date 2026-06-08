local_test_project <- function(env = parent.frame()) {
  tmp <- withr::local_tempdir(.local_envir = env)
  usethis::local_project(tmp, force = TRUE, .local_envir = env)
  tmp
}
