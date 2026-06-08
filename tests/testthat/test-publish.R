test_that("load_ids returns empty list when file does not exist", {
  expect_equal(load_ids(tempfile()), list())
})

test_that("load_ids returns empty list for empty yaml", {
  f <- withr::local_tempfile(fileext = ".yml")
  writeLines("", f)
  expect_equal(load_ids(f), list())
})

test_that("load_ids round-trips yaml", {
  f <- withr::local_tempfile(fileext = ".yml")
  ids <- list(gdrive = list("paper.qmd" = list(id = "abc123", url = "https://example.com")))
  yaml::write_yaml(ids, f)
  expect_equal(load_ids(f), ids)
})

test_that("gdrive_url builds correct URL", {
  expect_equal(gdrive_url("abc123"), "https://docs.google.com/document/d/abc123")
})

test_that("now_utc returns ISO 8601 UTC string", {
  expect_match(now_utc(), "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$")
})

test_that("open_published errors when no ids file exists", {
  tmp <- withr::local_tempdir()
  file.create(file.path(tmp, "paper.qmd"))
  expect_error(open_published(file.path(tmp, "paper.qmd")), "No published URL found")
})

test_that("open_published errors when gdrive entry is missing", {
  tmp <- withr::local_tempdir()
  file.create(file.path(tmp, "paper.qmd"))
  yaml::write_yaml(
    list(gdrive = list("other.qmd" = list(url = "https://example.com"))),
    file.path(tmp, "_publish_ids.yml")
  )
  expect_error(open_published(file.path(tmp, "paper.qmd")), "No published URL found")
})

test_that("docx_publish_args errors when publish files are missing", {
  withr::local_dir(withr::local_tempdir())
  expect_error(docx_publish_args(), "Missing DOCX publish support file")
})

test_that("check_package errors when package is not installed", {
  expect_error(check_package("_not_a_real_package_"), "is required but not installed")
})
