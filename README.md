
COVIDminerAPI <img src="man/figures/logo.png" align="right" alt="" width="120" />
=========================================================================

The COVIDminerAPI package
-----------------

COVIDminerAPI is a package to enable easy access from R to the COVIDminer API.

COVIDminer
------------

The [COVIDminer](https://rupertoverall.net/covidminer/) project is a text mining resource covering the rapidly-expanding literature surrounding the SARS-CoV-2 virus and the COVID-19 pandemic. For more information on this project please visit the COVIDminer website at [https://rupertoverall.net/covidminer/](https://rupertoverall.net/covidminer/) and particularly the [tutorial](https://rupertoverall.net/covidminer/tutorial) if you are new to the project.


Installation
------------

The package can be installed in R using [devtools](https://www.rstudio.com/products/rpackages/devtools/):

``` r
devtools::install_github("rupertoverall/COVIDminerAPI", build_vignettes = TRUE)
```


Usage
------------

The COVIDminerAPI package is used to construct API queries to the [COVIDminer](https://rupertoverall.net/covidminer/) text mining service.
Because the functions accept character vectors and return data frames and lists, this package makes calling the web service and parsing the response fit seamlessly into your R code.

A example session is shown below:

``` r
# The api_search function is used to retrieve COVIDminer identifiers for two entities.
search.response = COVIDminerAPI::api_search(query = c("bdnf", "autophagy"))

# The most useful field in the results is 'best-match', which returns a single identifier for each query term.
best.matches = search.response$results$`best-match`

# The best matching identifiers can then be passed to the api_get function to get the interaction network.
get.response = COVIDminerAPI::api_get(query = best.matches, filters = "default")

# If you want to use network analysis tools, then a simple edge list is a good starting point.
edge.list = get.response$results[, c("source", "relation", "target")]
```

