oomycetedbtools
===============

This is an R package for some of the code used to keep `oomycetedb.org`
up to date. \*\* This is not a general-use package, \*\* but you are
free to use it and adapt it.

Updating the website
--------------------

The `update_website` function is run each night at 1am using the
following [cron tab](https://en.wikipedia.org/wiki/Cron):

    0 1 * * * Rscript -e 'oomycetedbtools::update_website()'

For this to work this package must be installed in R on the system using
the following command:

    devtools::install_github("grunwaldlab/oomycetedbtools")

    ## Downloading GitHub repo grunwaldlab/oomycetedbtools@master
    ## from URL https://api.github.com/repos/grunwaldlab/oomycetedbtools/zipball/master

    ## Installation failed: Not Found (404)

Since this package is only desinged to update this website in it
original form, the default parameters for updating the website are
stored as constants in `R/configuration.R`. Install the package and type
`?update_website` for more information.

License
-------

GPL version 3 or later
