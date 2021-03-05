#' Search the COVIDminer database for matching identifiers.
#'
#' This function takes free text values and searches the COVIDminer database for
#' entry IDs, names or aliases with partial matches.
#'
#' @param query A character vector of text descriptors of database IDs. See
#'   Details.
#' @param sort Logical. Should the result be sorted. Currently not implemented.
#' @param timeout An integer specifying the timeout in seconds for the http
#'   query.
#'
#' @return A nested \code{list} object with top level elements; query (details
#'   of the query as interpreted by the server), results (a \code{data.frame}
#'   with results for each query element), matches; all matching database
#'   entities for each query element) and hits; the COVIDminer IDs where a
#'   perfect match was found).
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
api_search = function(
	query,
	sort = FALSE,
	timeout = 100
){
	# These options cannot be user set (or they would break the code).
	api.url = "https://rupertoverall.net/covidminer/api/search"
	return = "json"
	
	# Collapse query terms into a comma-separated string for the URL.
	queries = sapply(query, utils::URLencode, reserved = TRUE)
	
	# Check for clean input.
	clean = TRUE
	if(query[1] == ""){
		clean = FALSE
		warning("The 'query' parameter cannot be empty.")
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
		# For large query lists, we hit a server timeout...
		# ...so we will perform individual queries and collate post factum.
		pb = utils::txtProgressBar(min = 0, max = length(queries), style = 3)
		progress = 0
		json.list = lapply(queries, function(q){
			url = paste0(api.url, "?terms=", q, "&return=", return)
			progress <<- progress + 1
			utils::setTxtProgressBar(pb, progress - 1)
			query.results = tryCatch({
				jsonlite::fromJSON(txt = RCurl::getURL(url))
			},
			error = function(e){
				warning(e)
				warning("Could not connect to the COVIDminer API.")
				return(NULL)
			})
		})
		utils::setTxtProgressBar(pb, length(queries))
		close(pb)
		
		json = list() # Collate.
		json$query = json.list[[length(json.list)]]$query # Timestamp of last query.
		json$query$`hit-count` = sum(sapply(lapply(json.list, "[[", "query"), "[[", "hit-count"))
		json$query$`search-terms` = as.character(sapply(lapply(json.list, "[[", "query"), "[[", "search-terms"))
		json$query$url = paste0(api.url, "?terms=", I(paste(queries, collapse = ",")), "&return=", return)
		if(json$query$`hit-count` == 1){
			if(length(json$query$`search-terms`) == 1){
				message(paste0("  Retrieved ", json$query$`hit-count`, " hit from ", length(json$query$`search-terms`), " search term."))
			}else{
				message(paste0("  Retrieved ", json$query$`hit-count`, " hit from ", length(json$query$`search-terms`), " search terms."))
			}
		}else{
			if(length(json$query$`search-terms`) == 1){
				message(paste0("  Retrieved ", json$query$`hit-count`, " hits from ", length(json$query$`search-terms`), " search term."))
			}else{
				message(paste0("  Retrieved ", json$query$`hit-count`, " hits from ", length(json$query$`search-terms`), " search terms."))
			}
		}
		json$results = as.data.frame(do.call("rbind", lapply(json.list, function(j){
			result = j$results[[1]]
			result$matches = NULL
			unlist(result)
		})))
		
		json$matches  = lapply(json.list, function(j){ # Returned as a list separately from the results data.frame.
			return(as.data.frame(j$results[[1]]$matches))
		})
		
		hits = json$results$`perfect-match` != ""
		json$hits = stats::setNames(as.character(json$results$`perfect-match`)[hits], json$results$`search-term`[hits])
		
		return(json)
	}
}
