test_that("use_publish_workflow copies all files by default", {
  tmp <- local_test_project()
  use_publish_workflow()
  expect_true(file.exists(file.path(tmp, "justfile")))
  expect_true(file.exists(file.path(tmp, "publish", "reference.docx")))
  expect_true(file.exists(file.path(tmp, "publish", "docx-format.lua")))
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

test_that("use_just_instructions runs without error", {
  expect_no_error(use_just_instructions())
})
