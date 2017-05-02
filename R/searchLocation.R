## originally written by noahwz: https://github.com/NoahZinsmeister/Rbnb
## modified by gmlang

#' @title Searches for Airbnb listings by location via its API.
#' 
#' @param location string, the desired search place, usually a city name. Doesn't 
#'                 matter if capitalized or not. For example, Osaka or osaka.    
#' @param api_key  string, your own Airbnb API key. Ours has been provided for 
#'                 convenience.
#' @param checkin  string, check in date of format "yyyy-mm-dd".
#' @param checkout string, check out date of format "yyyy-mm-dd".
#' @param guests   integer, number of guests, default = 1.
#' @param min_bathrooms numeric, min number of bathrooms, default = 1.
#' @param min_bedrooms integer, min number of bedrooms, default = 1.
#' @param price_min integer, min price (USD) per night, default = 0.
#' @param price_max integer, max price (USD) per night, default = 25.
#' @param min_num_pics integer, min number of pictures, default = 5.
#' @param verbose boolean, T/F, indicating whether to print status updates.
#' 
#' @return A named list containing various search outputs.
#' @export

searchLocation = function(location, 
                          api_key = "d306zoyjsyarp7ifhu67rjxn52tv0t20",
                          checkin = Sys.Date(),
                          checkout = Sys.Date() + 1,
                          guests = 1, 
                          min_bathrooms = 1, 
                          min_bedrooms = 1, 
                          min_beds = 1, 
                          price_min = 0, 
                          price_max = 25, 
                          min_num_pics = 5, 
                          verbose = F) {
        # default search parameters
        params = list(location = location, 
                      client_id = api_key, 
                      checkin = checkin,
                      checkout = checkout,
                      guests = guests, 
                      min_bathrooms = min_bathrooms, 
                      min_bedrooms = min_bedrooms, 
                      min_beds = min_beds, 
                      price_min = price_min, 
                      price_max = price_max, 
                      min_num_pic_urls = min_num_pics, 
                      suppress_facets = TRUE,
                      ib = FALSE, # also show non-instant-bookable listings 
                      locale = "en-US", 
                      currency = "USD", 
                      sort = 1,
                      # "amenities[]" = 1,
                      "_format" = "for_search_results_with_minimal_pricing", # "for_search_results"
                      "_limit" = 1, 
                      "_offset" = 0)
        api_url = "https://api.airbnb.com/v2/search_results"
        
        # make an exploratory call, only return 1 listing
        request = httr::RETRY("GET", url = constructGET(api_url, params))
        checkRequest(request)
        primary_results = httr::content(request, as = "parsed")

        # check number of listings returned for the location
        num_listings = primary_results$metadata$listings_count

        # if 0 listings, stop
        if (num_listings == 0)
                stop("No results found. Try a different search term.")

        # else, figure out how many max prices cutoffs we need to get x or fewer 
        # listings between cutoffs.
        # NOTE: this is because if num_listings is over 1000, it means that 
        # there are more results than we're able to get by manipulating limit 
        # and offset, hence using price cutoffs
        getNumListings = function(p_low, p_high) {
                # gets number of listings in [p_low, p_high-1]
                params$price_min = p_low
                params$price_max = p_high-1
                request = httr::RETRY("GET", url = constructGET(api_url, params))
                checkRequest(request)
                parsed_results = httr::content(request, as = "parsed")
                
                # return number of listings returned
                parsed_results$metadata$listings_count
        }

        # starting cutoffs 
        price_cutoffs = c(price_min, price_max)

        # make cutoffs more granular until we have bins of <300 listings
        while (any(num_listings > 300)) {
                # between which cutoffs are there most listings?
                max_index = which.max(num_listings)
                
                # split that range in half to get new cutoff
                new_cutoff = as.integer(mean(c(price_cutoffs[max_index], 
                                               price_cutoffs[max_index+1])))
                if (new_cutoff %in% price_cutoffs)
                        stop("Couldn't find sufficiently granular price cutoffs 
                             to return all results.")
            
                # insert new cutoff
                price_cutoffs = append(price_cutoffs, new_cutoff, 
                                       after = max_index)
                
                # add in number of listings for new cutoffs
                num_listings = append(num_listings, 
                                      c(getNumListings(price_cutoffs[max_index], 
                                                       price_cutoffs[max_index+1]),
                                        getNumListings(price_cutoffs[max_index+1], 
                                                       price_cutoffs[max_index+2])),
                                      after = max_index)
                
                # and remove the old cutoff
                num_listings = num_listings[-max_index]
        }

        # if >2000 listings, warn
        if (sum(num_listings) > 2000) {
                warning(paste0(sum(num_listings), 
                               " listings found. Performance may be impaired."), 
                        immediate. = TRUE)
        } else {
                if (verbose) print(paste0(sum(num_listings), 
                                          " listings found! Retrieving data..."))
        }

        # for every pair of price cutoffs, load the data in, 
        # given max chunk size of 50
        all_results = list()
        params$`_limit` = 50
        for (i in 1:length(num_listings)) {
                if (num_listings[i] == 0) next
                if (verbose) print(paste0("Batch ", i, " of ", 
                                          length(num_listings)))
                params$price_min = price_cutoffs[i]
                params$price_max = price_cutoffs[i+1]-1

                # make the necessary calls, saving results only at each step
                repeat {
                        request = httr::RETRY("GET", 
                                              url = constructGET(api_url, params))
                        checkRequest(request)
                        parsed_results = httr::content(request, as = "parsed")
                        all_results = c(all_results, parsed_results$search_results)
                        params$`_offset` = 
                                parsed_results$metadata$pagination$next_offset
                        if (parsed_results$metadata$pagination$result_count < 50) 
                                break
                }
                params$`_offset` = 0
        }

        # return the list of all results, metadata from last call, and passed location
        list(location = location,
             results = list(num_listings = length(all_results),
                            data = parseResults(all_results)))
}

