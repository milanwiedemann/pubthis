
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pubthis

Render and publish Quarto manuscripts to Google Drive with consistent
formatting. Fixes known issues with figures when converting Quarto files
to Google Docs. Modelled on [usethis](https://usethis.r-lib.org/),
`use_publish_workflow()` drops the required files into any project
across projects.

One suggested workflow: write in Quarto, publish to Google Drive, get
comments from collaborators, update the `.qmd`, and repeat.

## Installation

``` r
# install.packages("pak")
pak::pak("milanwiedemann/pubthis")
```

## Usage

### Set up a new project

Run once to add the workflow to your project:

``` r
pubthis::use_publish_workflow()
```

This copies into your project:

- `publish/reference.docx`: Word template for consistent styles
- `publish/docx-format.lua`: Lua filter for DOCX output needed for
  figures in Google Docs
- `justfile`: task runner with `publish`, `open`, and `auth-gdrive`
  commands

Then add the following to your `.qmd` YAML. For example, if your
manuscript is at `manuscripts/paper.qmd`, the path from there to
`publish/` at the project root is `../publish/`:

``` yaml
format:
  docx:
    reference-doc: ../publish/reference.docx
filters:
  - ../publish/docx-format.lua
```

### Publish a manuscript

From the terminal using `just`:

``` sh
# Render and publish to Google Drive
just publish manuscripts/paper.qmd

# Open the published doc in the browser
just open manuscripts/paper.qmd
```

Or directly from R:

``` r
pubthis::publish("manuscripts/paper.qmd")
pubthis::open_published("manuscripts/paper.qmd")
```

### Authentication

Run `just auth-gdrive` to see instructions for caching Google Drive
credentials.

### How published IDs are tracked

On first publish, a `_publish_ids.yml` file is created next to the
`.qmd` file. Commit this file so collaborators always open the same
shared document.

## Related packages

- [trackdown](https://claudiozandonella.github.io/trackdown/)
- [officedown](https://davidgohel.github.io/officedown/)
- [officer](https://davidgohel.github.io/officer/)
- [redoc](https://github.com/noamross/redoc)
