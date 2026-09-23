test_that("publish rejects an empty path before touching anything", {
  expect_error(publish(""), "must be a single non-empty")
})

test_that("check_qmd_file rejects non-scalar and missing input", {
  expect_error(check_qmd_file(""), "must be a single non-empty")
  expect_error(check_qmd_file(NULL), "must be a single non-empty")
  expect_error(check_qmd_file(NA_character_), "must be a single non-empty")
  expect_error(
    check_qmd_file(c("a.qmd", "b.qmd")),
    "must be a single non-empty"
  )
  expect_error(check_qmd_file(tempfile(fileext = ".qmd")), "File not found")
})

test_that("check_qmd_file rejects directories", {
  tmp <- withr::local_tempdir()
  expect_error(check_qmd_file(tmp), "File not found")
})

test_that("check_qmd_file rejects non-qmd files", {
  f <- withr::local_tempfile(fileext = ".docx")
  file.create(f)
  expect_error(check_qmd_file(f), "must be a")
})

test_that("check_qmd_file returns the absolute path for a valid qmd", {
  tmp <- withr::local_tempdir()
  qmd <- file.path(tmp, "paper.qmd")
  file.create(qmd)
  withr::local_dir(tmp)
  result <- check_qmd_file("paper.qmd")
  expect_true(fs::is_absolute_path(result))
  expect_equal(fs::path_file(result), "paper.qmd")
})

test_that("load_deployments returns empty list when file does not exist", {
  expect_equal(load_deployments(tempfile()), list())
})

test_that("load_deployments returns empty list for empty yaml", {
  f <- withr::local_tempfile(fileext = ".yml")
  writeLines("", f)
  expect_equal(load_deployments(f), list())
})

test_that("load_deployments round-trips yaml", {
  f <- withr::local_tempfile(fileext = ".yml")
  deployments <- list(list(
    source = "paper.qmd",
    gdrive = list(list(id = "abc123"))
  ))
  yaml::write_yaml(deployments, f)
  expect_equal(load_deployments(f), deployments)
})

test_that("load_deployments errors on the old flat _publish_ids.yml shape", {
  f <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(list(gdrive = list("paper.qmd" = list(id = "abc123"))), f)
  expect_error(load_deployments(f), "not in the expected format")
})

test_that("set_gdrive_deployment preserves other providers on the same source", {
  deployments <- list(list(
    source = "paper.qmd",
    `posit-connect-cloud` = list(list(
      id = "xyz",
      url = "https://connect.example/xyz"
    ))
  ))
  updated <- set_gdrive_deployment(deployments, "paper.qmd", "abc123")
  expect_equal(updated[[1]][["posit-connect-cloud"]][[1]][["id"]], "xyz")
  expect_equal(updated[[1]][["gdrive"]][[1]][["id"]], "abc123")
})

test_that("set_gdrive_deployment appends a new source entry when none exists", {
  deployments <- list(list(
    source = "other.qmd",
    gdrive = list(list(id = "old"))
  ))
  updated <- set_gdrive_deployment(deployments, "paper.qmd", "abc123")
  expect_length(updated, 2)
  expect_equal(gdrive_entry(updated, "paper.qmd")[["id"]], "abc123")
  expect_equal(gdrive_entry(updated, "other.qmd")[["id"]], "old")
})

test_that("gdrive_url builds correct URL", {
  expect_equal(
    gdrive_url("abc123"),
    "https://docs.google.com/document/d/abc123"
  )
})

test_that("open_published errors when no publish file exists", {
  tmp <- withr::local_tempdir()
  file.create(file.path(tmp, "paper.qmd"))
  expect_error(
    open_published(file.path(tmp, "paper.qmd")),
    "No published doc found"
  )
})

test_that("open_published errors when gdrive entry is missing", {
  tmp <- withr::local_tempdir()
  file.create(file.path(tmp, "paper.qmd"))
  yaml::write_yaml(
    list(list(source = "other.qmd", gdrive = list(list(id = "abc123")))),
    file.path(tmp, "_publish.yml")
  )
  expect_error(
    open_published(file.path(tmp, "paper.qmd")),
    "No published doc found"
  )
})

test_that("open_published derives the URL from a hand-created id-only yml", {
  tmp <- withr::local_tempdir()
  file.create(file.path(tmp, "paper.qmd"))
  yaml::write_yaml(
    list(list(source = "paper.qmd", gdrive = list(list(id = "abc123")))),
    file.path(tmp, "_publish.yml")
  )
  opened <- NULL
  local_mocked_bindings(browse_url = function(url) opened <<- url)
  open_published(file.path(tmp, "paper.qmd"))
  expect_equal(opened, "https://docs.google.com/document/d/abc123")
})

test_that("open_published works alongside an existing posit-connect-cloud entry", {
  tmp <- withr::local_tempdir()
  file.create(file.path(tmp, "paper.qmd"))
  yaml::write_yaml(
    list(list(
      source = "paper.qmd",
      `posit-connect-cloud` = list(list(
        id = "xyz",
        url = "https://connect.example/xyz"
      )),
      gdrive = list(list(id = "abc123"))
    )),
    file.path(tmp, "_publish.yml")
  )
  opened <- NULL
  local_mocked_bindings(browse_url = function(url) opened <<- url)
  open_published(file.path(tmp, "paper.qmd"))
  expect_equal(opened, "https://docs.google.com/document/d/abc123")
})

test_that("find_publish_dir finds publish/ next to the qmd", {
  tmp <- withr::local_tempdir()
  fs::dir_create(fs::path(tmp, "publish"))
  expect_equal(find_publish_dir(fs::path(tmp)), fs::path(tmp, "publish"))
})

test_that("find_publish_dir walks up from a manuscripts/ subdirectory", {
  tmp <- withr::local_tempdir()
  fs::dir_create(fs::path(tmp, "publish"))
  fs::dir_create(fs::path(tmp, "manuscripts"))
  expect_equal(
    find_publish_dir(fs::path(tmp, "manuscripts")),
    fs::path(tmp, "publish")
  )
})

test_that("find_publish_dir errors when no publish/ exists in any parent", {
  tmp <- withr::local_tempdir()
  expect_error(
    find_publish_dir(fs::path(tmp)),
    "No .*publish.* directory found"
  )
})

test_that("docx_publish_args errors when support files are missing", {
  tmp <- withr::local_tempdir()
  fs::dir_create(fs::path(tmp, "publish"))
  expect_error(
    docx_publish_args(fs::path(tmp, "paper.qmd")),
    "Missing DOCX publish support file"
  )
})

test_that("docx_publish_args resolves files relative to the qmd, not getwd()", {
  tmp <- withr::local_tempdir()
  fs::dir_create(fs::path(tmp, "publish"))
  fs::dir_create(fs::path(tmp, "manuscripts"))
  file.create(fs::path(tmp, "publish", "reference.docx"))
  file.create(fs::path(tmp, "publish", "docx-format.lua"))
  withr::local_dir(withr::local_tempdir())
  args <- docx_publish_args(fs::path(tmp, "manuscripts", "paper.qmd"))
  expect_equal(
    args,
    c(
      paste0("--reference-doc=", fs::path(tmp, "publish", "reference.docx")),
      paste0("--lua-filter=", fs::path(tmp, "publish", "docx-format.lua"))
    )
  )
})

test_that("rendered_output takes the path from quarto's Output created line", {
  result <- list(
    stdout = "",
    stderr = "pandoc ...\nOutput created: _output/paper.docx\n"
  )
  expect_equal(
    rendered_output(
      result,
      wd = "/proj/manuscripts",
      default = "/proj/manuscripts/paper.docx"
    ),
    fs::path("/proj/manuscripts/_output/paper.docx")
  )
})

test_that("rendered_output falls back to the sibling path", {
  result <- list(stdout = "", stderr = "no marker here")
  expect_equal(
    rendered_output(result, wd = "/proj", default = "/proj/paper.docx"),
    "/proj/paper.docx"
  )
})

test_that("figure bookmarks start at the caption", {
  skip_on_cran()
  skip_if(Sys.which("quarto") == "", "Quarto is not installed")
  skip_if_not_installed("knitr")

  tmp <- withr::local_tempdir()
  publish_dir <- fs::path(tmp, "publish")
  fs::dir_create(publish_dir)
  fs::file_copy(
    system.file(
      "templates",
      c("reference.docx", "docx-format.lua"),
      package = "pubthis"
    ),
    publish_dir
  )

  qmd <- fs::path(tmp, "paper.qmd")
  writeLines(
    c(
      "---",
      "format: docx",
      "---",
      "",
      "See @fig-test.",
      "",
      "```{r}",
      "#| label: fig-test",
      '#| fig-cap: "Caption bookmark target."',
      "#| echo: false",
      "",
      "plot(1)",
      "```"
    ),
    qmd
  )

  docx <- render_docx(qmd)
  xml <- paste(
    readLines(unz(docx, "word/document.xml"), warn = FALSE),
    collapse = ""
  )
  image <- regexpr("<w:drawing>", xml, fixed = TRUE)[[1]]
  bookmark <- regexpr('w:name="fig-test"', xml, fixed = TRUE)[[1]]
  caption <- regexpr("Caption bookmark target.", xml, fixed = TRUE)[[1]]
  bookmark_end <- regexpr(
    "<w:bookmarkEnd",
    substring(xml, bookmark),
    fixed = TRUE
  )[[1]]

  expect_gt(image, 0)
  expect_gt(bookmark, image)
  expect_gt(caption, bookmark)
  expect_gt(bookmark_end, caption - bookmark)
})

test_that("upload_to_gdrive errors clearly when the DOCX is missing", {
  expect_error(
    upload_to_gdrive(tempfile(fileext = ".docx"), "paper"),
    "Rendered DOCX not found"
  )
})

test_that("confirm_unresolved_comments stays quiet when there are none", {
  local_mocked_bindings(count_unresolved_comments = function(doc_id) 0)
  expect_no_message(confirm_unresolved_comments("abc123"))
})

test_that("confirm_unresolved_comments warns with the count", {
  local_mocked_bindings(count_unresolved_comments = function(doc_id) 2)
  expect_message(confirm_unresolved_comments("abc123"), "2 unresolved comments")
})

test_that("confirm_unresolved_comments reports a failed lookup instead of staying silent", {
  local_mocked_bindings(count_unresolved_comments = function(doc_id) {
    cli::cli_abort("boom")
  })
  expect_no_error(confirm_unresolved_comments("abc123"))
  expect_message(
    confirm_unresolved_comments("abc123"),
    "Could not check for unresolved comments"
  )
})

test_that("check_package errors when package is not installed", {
  expect_error(
    check_package("_not_a_real_package_"),
    "is required but not installed"
  )
})
