
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


## pinnacle ----
usethis::use_r("pinnacle")

# document functions
# > insert roxygen skeleton
devtools::document()

# specify package dependencies
usethis::use_package("dplyr")
usethis::use_package("glue")
usethis::use_package("httr2")
usethis::use_package("implied")
usethis::use_package("lubridate")
usethis::use_package("nhldata")
usethis::use_package("purrr")
usethis::use_package("tibble")
usethis::use_package("tidyr")

# check build
devtools::check()


# Step 6: Package Site ----------------------------------------------------

# generate readme
usethis::use_readme_rmd()
devtools::build_readme()

# build site
pkgdown::build_site()

# add pkgdown, yml and docs to .buildignore
usethis::use_build_ignore("_pkgdown.yml")
usethis::use_build_ignore("^pkgdown$")
