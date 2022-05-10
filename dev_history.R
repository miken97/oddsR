
# Step 0: Resources -------------------------------------------------------

# pkg development resources

# <https://www.pipinghotdata.com/posts/2020-10-25-your-first-r-package-in-1-hour>
# <https://rtask.thinkr.fr/when-development-starts-with-documentation>
# <https://github.com/ThinkR-open/attachment/blob/master/devstuff_history.R>
# <https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html>


# Step 1: Create Pkg ------------------------------------------------------

# create pkg
usethis::create_package("C:/mike_nanos/r_files/oddsR")


# Step 2: Development History ---------------------------------------------

# create dev_history.R and hide from pkg build
# > save dev_history.R in project root
usethis::use_build_ignore("dev_history.R")


# Step 3: Git Connection --------------------------------------------------

# connect local pkg to git repo
usethis::use_git()
usethis::use_github()


# Step 4: Description & License -------------------------------------------

# add mit license and update description file
usethis::use_mit_license("Mike Nanos")
# > update description file manually

# add pipe
usethis::use_pipe()

# document and run first check
devtools::document()
devtools::check()


# Step 5: Functions & Dependencies ----------------------------------------

# create global variables in utils.R
usethis::use_r("utils")

# create data description file data.R
# usethis::use_r("data")

# create datasets
# usethis::use_data_raw("")


## misc functions ----
usethis::use_r("between2")
usethis::use_r("cite_by_doi")
usethis::use_r("copy_df")
usethis::use_r("cran_pkgs")
usethis::use_r("lookup")
usethis::use_r("not_in")
usethis::use_r("r_data")
usethis::use_r("r_resources")
usethis::use_r("seq_range")
usethis::use_r("use_browser")

## probability functions ----
usethis::use_r("beta_a_b")
usethis::use_r("gamma_a_b")

## spatial functions ----
usethis::use_r("st_drop")
usethis::use_r("st_erase")
usethis::use_r("tibble_to_sf")

## formatting and viz functions ----
usethis::use_r("as_href")
usethis::use_r("colours")
usethis::use_r("get_bin_width")
usethis::use_r("plot_theme")
usethis::use_r("scrape_pal")
usethis::use_r("theme_mn")
usethis::use_r("today")

# document functions
# > insert roxygen skeleton
devtools::document()

# specify package dependencies
usethis::use_package("broom")
usethis::use_package("dplyr")
usethis::use_package("reactablefmtr")
usethis::use_package("here")
usethis::use_package("httr")
usethis::use_package("rlang")

# check build
devtools::check()


# Step 6: Package Site ----------------------------------------------------

# build site
pkgdown::build_site()

# add pkgdown, yml and docs to .buildignore
usethis::use_build_ignore("_pkgdown.yml")
usethis::use_build_ignore("^pkgdown$")

# generate readme
usethis::use_readme_rmd()
devtools::build_readme()


# library(tidyverse)
#
# res <- r_resources()
#
# types <- sort(unique(res$type))
#
# topics <- res |>
#   select(topics) |>
#   separate_rows(topics, sep = ", ") |>
#   count(topics, sort = T) |>
#   arrange(topics)
#
# types
# topics |> view()
#
#
# topics_na <- res |>
#   transmute(
#     row_id = row_number(),
#     topics
#   ) |>
#   separate_rows(topics, sep = ", ") |>
#   filter(is.na(topics)) |>
#   pull(row_id)
#
# res |>
#   # filter(row_number() == 302)
#   filter(row_number() %in% topics_na)
#
#
# res |>
#   transmute(
#     row_id = row_number(),
#     topics
#   ) |>
#   separate_rows(topics, sep = ", ") |>
#   filter(topics == "NA")
