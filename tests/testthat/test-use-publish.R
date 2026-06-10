test_that("use_publish_workflow copies all files by default", {
  tmp <- local_test_project()
  use_publish_workflow()
  expect_true(file.exists(file.path(tmp, "justfile")))
  expect_true(file.exists(file.path(tmp, "publish", "reference.docx")))
  expect_true(file.exists(file.path(tmp, "publish", "docx-format.lua")))
})

test_that("copied justfile is byte-identical to the template", {
  # Regression: usethis::use_template() whisker-rendered the justfile and
  # stripped just's {{file}}/{{args}} placeholders, so the installed recipe
  # ran pubthis::publish('') for every file.
  tmp <- local_test_project()
  use_publish_workflow()
  template <- system.file("templates", "justfile", package = "pubthis", mustWork = TRUE)
  expect_equal(readLines(file.path(tmp, "justfile")), readLines(template))
})

test_that("copied justfile recipes still interpolate the file argument", {
  tmp <- local_test_project()
  use_publish_workflow()
  justfile <- paste(readLines(file.path(tmp, "justfile")), collapse = "\n")
  expect_match(justfile, "pubthis::publish('{{file}}')", fixed = TRUE)
  expect_match(justfile, "pubthis::open_published('{{file}}')", fixed = TRUE)
})

test_that("use_publish_workflow with justfile = FALSE omits justfile", {
  tmp <- local_test_project()
  use_publish_workflow(justfile = FALSE)
  expect_false(file.exists(file.path(tmp, "justfile")))
  expect_true(file.exists(file.path(tmp, "publish", "reference.docx")))
  expect_true(file.exists(file.path(tmp, "publish", "docx-format.lua")))
})

test_that("use_publish_workflow can be run twice without error", {
  local_test_project()
  use_publish_workflow()
  expect_no_error(use_publish_workflow())
})

test_that("re-running reports existing files that match the current template", {
  local_test_project()
  use_publish_workflow()
  expect_message(use_publish_workflow(), "matches the current template")
})

test_that("re-running flags an existing file that differs from the template", {
  # A stale or customised justfile (e.g. one mangled by the old
  # use_template() bug) is kept, but the user is told it is out of date.
  tmp <- local_test_project()
  use_publish_workflow()
  writeLines("# customised", file.path(tmp, "justfile"))
  expect_message(use_publish_workflow(), "differs from the current template")
  expect_equal(readLines(file.path(tmp, "justfile")), "# customised")
})
