#' Construct a COVIDminer URL.
#'
#' Generates a URL link to a network for browsing in COVIDminer.
#'
#' @param query A character vector of text descriptors of database IDs. See
#'   Details.
#' @param filters A character vector of filters. Default is all, which will not
#'   apply any filters. A good option for general use is default, which will
#'   only retrieve SARS-COV-2 primary literature.
#' @param level An integer describing the network level that is to be returned
#'   by \code{get}. Either 0 (only the entities specified in \code{query}) or 1
#'   (the query entites and their first neighbours).
#' @param unmapped Logical. Should unmapped entities be used?
#'
#' @return A URL as text.
#'
#' @details The identifiers used in this function must be those used in the
#'   database. The function \code{\link[COVIDminerAPI]{api_search}} can be used
#'   to retrieve these from text descriptors (e. g. gene/protein identifiers or
#'   gene symbols). The function constructs a URL that points to a
#'   dynamically-generated web page which will show the entities in the query
#'   and all the links between them that are currently in the COVIDminer
#'   database. As the COVIDminer database is continuously updated, the results
#'   on that web page will change with time.
#'
#' @examples
#'
#' url <- COVIDminerAPI::show_url(
#'   query = c("GeneID:627", "GO:GO:0006915", "GO:GO:0006914", "MeSH:D019636"),
#'   filters = "default",
#'   level = 0
#' )
#' utils::browseURL(url)
#'
#' @export
show_url = function(
	query,
	filters = "all",
	level = 0,
	unmapped = TRUE
){
	return(paste0(
		"https://rupertoverall.net/covidminer/",
		"?ids=", I(paste(query, collapse = ",")), 
		"&filters=", paste(filters, collapse = ","),
		"&level=", as.numeric(level),
		"&unmapped=", as.numeric(unmapped)
	))
}
