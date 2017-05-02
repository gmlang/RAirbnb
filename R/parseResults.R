## originally written by Kroeger: https://github.com/NoahZinsmeister/Rbnb
## modified by gmlang

#' @title Extracts values from parsed data.
#' 
#' @description
#' Used internally by searchLocation().
#' 
#' @param results list of queried and parsed data.
#' 
#' @return a data frame.
#' @seealso \code{\link{searchLocation}}.
#' @keywords internal

rmPrefix = function(name, prefix) gsub(paste("^", prefix, sep=""), "", name)

parseResults = function(results) {
        results = dplyr::bind_rows(lapply(results, function(x) 
                dplyr::bind_rows(as.list(unlist(x)))))
        
        filterResults = function(i) {
                if (grepl("image|url|photo|png|scrim|listing.user.id", i)) 
                        results[i] <<- NULL
        }
        lapply(names(results), filterResults)
        
        # remove listing prefix
        names(results) = rmPrefix(names(results), "listing.")
        
        # replace underscores with periods
        names(results) = gsub("_", ".", names(results))
                
        # list numeric vars
        numericList = c("bathrooms", "beds", "bedrooms", "lat", "lng",
                        "picture.count", "person.capacity", "reviews.count",
                        "star.rating", "pricing.quote.guests",
                        "pricing.quote.guest.details.number.of.adults",
                        "pricing.quote.localized.nightly.price",
                        "pricing.quote.localized.service.fee",
                        "pricing.quote.localized.total.price",
                        "pricing.quote.long.term.discount.amount.as.guest",
                        "pricing.quote.nightly.price",
                        "pricing.quote.service.fee",
                        "pricing.quote.total.price")
        
        # make sure the vars are in the dataset
        numericList = numericList[numericList %in% names(results)]
        
        # change class of certain vars to numeric
        dplyr::mutate_at(.tbl=results, .cols=numericList, 
                         dplyr::funs("as.numeric"))
}

