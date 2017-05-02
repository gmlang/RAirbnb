## originally written by noahwz: https://github.com/NoahZinsmeister/Rbnb
## modified by gmlang

#' @title Constructs URL of a full API call.
#' 
#' @description
#' Used internally by searchLocation().
#' 
#' @param base_url  string, base url of API.
#' @param params    list of parameter values.
#' @return url of full API call.
#' @seealso \code{\link{searchLocation}}.
#' @keywords internal

constructGET = function(base_url, params) {
        # replace all spaces with %20
        params = lapply(params, function(x) gsub(" ", "%20", x))
        
        # append param names and values into a string like: name=value&...
        args = paste(names(params), params, sep = "=", collapse = "&")
        
        # return the appropriate url call
        paste0(base_url, "?", args)
}
