test_that("use_publish_workflow copies support files by default", {
  tmp <- local_test_project()
  use_publish_workflow()
  expect_false(file.exists(file.path(tmp, "justfile")))
  expect_true(file.exists(file.path(tmp, "publish", "reference.docx")))
  expect_true(file.exists(file.path(tmp, "publish", "docx-format.lua")))
})

test_that("use_publish_workflow copies the justfile when asked", {
  tmp <- local_test_project()
  use_publish_workflow(justfile = TRUE)
  expect_true(file.exists(file.path(tmp, "justfile")))
})

test_that("use_publish_workflow checks an existing justfile", {
  tmp <- local_test_project()
  justfile <- file.path(tmp, "justfile")
  writeLines("# Custom file", justfile)
  expect_message(use_publish_workflow(), "differs from the current template")
  expect_equal(readLines(justfile), "# Custom file")
})

test_that("copied justfile is byte-identical to the template", {
  tmp <- local_test_project()
  use_publish_workflow(justfile = TRUE)
  template <- system.file(
    "templates",
    "justfile",
    package = "pubthis",
    mustWork = TRUE
  )
  expect_equal(readLines(file.path(tmp, "justfile")), readLines(template))
})

test_that("copied justfile recipes still interpolate the file argument", {
  tmp <- local_test_project()
  use_publish_workflow(justfile = TRUE)
  justfile <- paste(readLines(file.path(tmp, "justfile")), collapse = "\n")
  expect_match(justfile, "pubthis::publish('{{file}}')", fixed = TRUE)
  expect_match(justfile, "pubthis::open_published('{{file}}')", fixed = TRUE)
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
  tmp <- local_test_project()
  use_publish_workflow(justfile = TRUE)
  writeLines("# customised", file.path(tmp, "justfile"))
  expect_message(
    use_publish_workflow(justfile = TRUE),
    "differs from the current template"
  )
  expect_equal(readLines(file.path(tmp, "justfile")), "# customised")
})
