## originally written by noahwz: https://github.com/NoahZinsmeister/Rbnb
## modified by gmlang

#' @title Check if API request returns an error or success.
#' 
#' @description
#' Stop and print an error message if API call doesn't return 200 status code.
#' 
#' @param request Object returned by GET()
#' @keywords internal
checkRequest = function(request) {
        if (request$status_code != 200) { # if request is not successful
                parsed_content = httr::content(request, as = "parsed")
                stop(paste0("Airbnb's API returned an error.\n\n",
                            paste(names(parsed_content), parsed_content,
                                  sep = ": ", collapse = "\n")))
        }
}
