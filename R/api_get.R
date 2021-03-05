#' Get interactions from the COVIDminer database.
#'
#' This function takes a vector of COVIDminer database IDs and allows networks
#' containing these entities to be retrieved.
#'
#' @param query A character vector of text descriptors of database IDs. See
#'   Details.
#' @param filters A character vector of filters. Default is \code{all}, which
#'   will not apply any filters. A good option for general use is
#'   \code{default}, which will only retrieve SARS-COV-2 primary literature.
#' @param level An integer describing the network level that is to be returned. 
#' Either 0 (only the entities specified in \code{query}) or 1 (the query entities and their first neighbours; the default).
#' @param unmapped Logical. Should unmapped entities be used?
#' @param meta Logical. If \code{TRUE}, duplicate interactions will be merged
#'   into a single directed edge. The results column \code{edge_weight} contains
#'   the number of edges that were merged. Default is to show each interaction
#'   separately (so that literature source information is retained).
#' @param sort Logical. Should the result be sorted. Currently not implemented.
#' @param timeout An integer specifying the timeout in seconds for the http
#'   query.
#' @param show.url Logical. Should the URL be displayed in the console (useful
#'   for debugging).
#'
#' @return A nested \code{list} object with top level elements; query (details
#'   of the query as interpreted by the server), results (a \code{data.frame}
#'   with results for each query element).
#'
#' @examples
#'
#' search.response <- COVIDminerAPI::api_search(query = c("bdnf", "autophagy"))
#' best.matches <- search.response$results$`best-match`
#'
#' get.response <- COVIDminerAPI::api_get(query = best.matches, filters = "default")
#' edge.list <- get.response$results[, c("source", "relation", "target")]
#'
#' @importFrom httr modify_url
#' @importFrom jsonlite fromJSON
#' @importFrom RCurl getURL
#' @importFrom stats setNames
#' @importFrom utils URLencode setTxtProgressBar txtProgressBar
#' @export
api_get = function(
	query,
	filters = "all",
	level = 1,
	unmapped = TRUE,
	meta = FALSE,
	sort = FALSE,
	timeout = 100,
	show.url = FALSE
){
	# These options cannot be user set (or they would break the code).
	api.url = "https://rupertoverall.net/covidminer/api/get"
	return = "json"
	
	# Collapse query terms into a comma-separated string for the URL.
	queries = sapply(query, utils::URLencode, reserved = TRUE)
	
	# Check for clean input.
	clean = TRUE
	if(query[1] == ""){
		clean = FALSE
		warning("The 'query' parameter cannot be empty.")
	}
	if(!as.numeric(level) %in% c(0, 1)){
		clean = FALSE
		warning("The 'level' parameter must be either 1, 0.")
	}
	if(!as.numeric(meta) %in% c(0, 1)){
		clean = FALSE
		warning("The 'meta' parameter must be either 1, 0, TRUE or FALSE.")
	}
	if(!as.numeric(unmapped) %in% c(0, 1)){
		clean = FALSE
		warning("The 'unmapped' parameter must be either 1, 0, TRUE or FALSE.")
	}
	if(!as.numeric(sort) %in% c(0)){
		#clean = FALSE
		#warning("The 'sort' parameter must be either 1, 0, TRUE or FALSE.")
		warning("The 'sort' parameter is not yet implemented")
	}
	# Abort if input not clean
	if(!clean){
		# 
		return(NULL)
	}else{
		# Construct the API call and retrieve results.
		json = tryCatch({
			url = httr::modify_url(
				paste0(api.url), 
				query = list(
					ids = I(paste(queries, collapse = ",")), 
					filters = paste(filters, collapse = ","),
					level = as.numeric(level),
					unmapped = as.numeric(unmapped),
					meta = meta,
					return = return,
					sort = as.numeric(sort)
				)
			)
			if(show.url) message(url)
			chars = unlist(strsplit(RCurl::getURL(url), ""))
			cleantxt = paste(chars[!grepl("[\u01-\u08\u7F-\u9F]", chars)], collapse = "") # Remove control characters if they slip through.
			jsonlite::fromJSON(cleantxt)
		},
		error = function(e){
			warning("Could not connect to the COVIDminer API.")
			return(NULL)
		})
		if(length(json$results) == 1){
			message(paste("  Retrieved", length(json$results), "statement."))
		}else{
			message(paste("  Retrieved", length(json$results), "statements."))
		}
		
		json$results = as.data.frame(do.call("rbind", lapply(json$results, unlist)))
		json$results$edge_weight = as.numeric(json$results$edge_weight)
		
		return(json)
	}
}
