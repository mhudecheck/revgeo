# This code enables reverse geocoding with Google Maps API and Photon API  
# revgeo() requires RCurl and RJSONIO to function properly
# You must provide a valid Google Maps API to reverse geocode with Google Maps
# You can specify whether you want the output to be a string, a hashed string, or a dataframe with option="string" | "hash" | "frame"
# If you specify an option that isn't included in the list, the function will return the results as a dataframe
# Specifying a hashed string or dataframe allows you to specify whether you want to return the entire results, or a single item with item=""
# Valid options for 'items=' include 'housenumber', 'street', 'city', 'county', 'state', and 'country'

# This package is licensed under GPL 3.0
# Please contact Mike Hudecheck at michael.hudecheck@gess.ethz.ch if you have any questions

#' Reverse Geocoding with the Photon Geocoder for OpenStreetMap and Google Maps.
#' @description Enables the use of the Photon geocoder for OpenStreetMap and Google Maps to reverse geocode coordinate pairs. Photon allows for unlimited geocode queries, while Google Maps provides a little more information for 'out of the way' locations. Google Maps requires an API key and limits users to 2,500 free queries a day.  
#' @author Michael Hudecheck, \email{michael.hudecheck@gess.ethz.ch}
#' @param longitude Required. You must enter a valid longitude coordinate;  e.g., -77.0229529
#' @param latitude Required. You must enter a valid latitude coordinate; e.g., 38.89283435
#' @param google Defaults to NULL, which automatically selects the Photon API. Enter TRUE to use the Google Maps API.
#' @param API Defaults to NULL. Enter a valid Google Maps API key to use the Google Maps API. 
#' @param output Defaults to NULL, which returns a reverse geocoded address as a string.  Other valid options include 'hash', which returns a hashed string, and 'frame', which returns a dataframe.
#' @param item Defaults to NULL. You can use 'item' in conjunction with 'hash' or 'frame' to return portion of the address; e.g., 'zip' for postal code. Options include 'housenumber', 'street', 'city', 'county', 'state', and 'country'.
#' @examples 
#' revgeo(longitude=-77.0229529, latitude=38.89283435)
#' revgeo(longitude=-77.0229529, latitude=38.89283435, output='frame')
#' revgeo(longitude=-77.0229529, latitude=38.89283435, output='hash', item='zip')
#' @source https://github.com/mhudecheck/revgeo/
#' @keywords reverse 
#' @keywords geocode
#' @import RCurl 
#' @import RJSONIO
#' @export

revgeo <- function(longitude, latitude, google = NULL, API = NULL, output = NULL, item = NULL) {
  if(missing(google)) {
    google <- NULL
  }
  if(missing(API)) {
    API <- NULL
  }
  if(missing(output)) {
    output <- NULL
  }
  if(missing(item)) {
    item <- NULL
  }
  geocode_data <- list()
  geocode_frame <- data.frame()
  if(is.null(google)) {
    url <- paste0("http://photon.komoot.de/reverse?lon=", longitude,"&lat=", latitude)
    for(i in url) {
      print(paste0("Getting geocode data from Photon:", i))
      data <- getURLAsynchronous(i)
      returned_data <- fromJSON(data)
      housenumber <- tryCatch(returned_data$features[[1]]$properties$housenumber, error=function(e) "House Number Not Found") 
      street <- tryCatch(returned_data$features[[1]]$properties$street, error=function(e) "Street Not Found")
      city <- tryCatch(returned_data$features[[1]]$properties$city, error=function(e) "City Not Found") 
      zip <- tryCatch(returned_data$features[[1]]$properties$postcode, error=function(e) "Postcode Not Found") 
      state <- tryCatch(returned_data$features[[1]]$properties$state, error=function(e) "State Not Found")
      country <- tryCatch(returned_data$features[[1]]$properties$country, error=function(e) "Country Not Found") 
      if(is.null(housenumber)) {
        housenumber <- "House Number Not Found,"
      }
      if(is.null(street)) {
        street <- "Street Not Found"
      }
      if(is.null(city)) {
        city <- "City Not Found"
      }
      if(is.null(zip)) {
        zip <- "Postcode Not Found"
      }
      if(is.null(state)) {
        state <- "State Not Found"
      }
      if(is.null(country)) {
        country <- "Country Not Found"
      }
      if(is.null(output)) {
        geocode_data <- append(geocode_data, paste(paste0(housenumber, " ", street), city, state, zip, country, sep=", "))
      } else if (output == "string") {
        geocode_data <- append(geocode_data, paste(paste0(housenumber, " ", street), city, state, zip, country, sep=", "))
      } else if (output == "hash") {
        geocode_data[["housenumber"]] <- c(geocode_data[["housenumber"]], housenumber)
        geocode_data[["street"]] <- c(geocode_data[["street"]], street)
        geocode_data[["city"]] <- c(geocode_data[["city"]], city)
        geocode_data[["state"]] <- c(geocode_data[["state"]], state)
        geocode_data[["zip"]] <- c(geocode_data[["zip"]], zip)
        geocode_data[["country"]] <- c(geocode_data[["country"]], country)
      } else {
        geocode_data[["housenumber"]] <- c(geocode_data[["housenumber"]], housenumber)
        geocode_data[["street"]] <- c(geocode_data[["street"]], street)
        geocode_data[["city"]] <- c(geocode_data[["city"]], city)
        geocode_data[["state"]] <- c(geocode_data[["state"]], state)
        geocode_data[["zip"]] <- c(geocode_data[["zip"]], zip)
        geocode_data[["country"]] <- c(geocode_data[["country"]], country)
        geocode_frame <- rbind(geocode_frame, as.data.frame(geocode_data))
      }
    }
  ### Start Google ###
  } else if (is.null(API) && !(is.null(google))) {
      print("Please enter your Google api")
      return()
  } else {
    url <- paste0("https://maps.googleapis.com/maps/api/geocode/json?latlng=",latitude,",",longitude,"&key=",API)
    postcode <- list()
    for(i in url) {
      print(paste0("Getting geocode data from Google:", i))
      data <- getURLAsynchronous(i)
      returned_data <- fromJSON(data)
      address <- returned_data$results[[1]]$formatted_address
      k <- 1
      while(k<8) {
        j <- returned_data$results[[1]]$address_components[[k]]
        if(j$types[1]=="street_number") {
          housenumber <-  tryCatch(j$short_name, error=function(e) "House Number Not Found")
        } else if(j$types[1]=="route") {
          street <- tryCatch(j$long_name, error=function(e) "Street Not Found")
        } else if(j$types[1]=="locality") {
          city <- tryCatch(j$long_name, error=function(e) "City Not Found")
        } else if(j$types[1]=="administrative_area_level_2") {
          county <- tryCatch(j$long_name, error=function(e) "County Not Found")
        } else if(j$types[1]=="postal_code") {
          zip <- tryCatch(j$long_name, error=function(e) "Postcode Not Found") 
        } else if(j$types[1]=="administrative_area_level_1") {
          state <- tryCatch(j$long_name, error=function(e) "State Not Found")
        } else if(j$types[1]=="country") {
          country <- tryCatch(j$long_name, error=function(e) "State Not Found")
        }
        k <- k+1
      }
      if(is.null(housenumber)) {
        housenumber <- "House Number Not Found"
      }
      if(is.null(street)) {
        street <- "Street Not Found"
      }
      if(is.null(city)) {
        city <- "City Not Found"
      }
      if(is.null(zip)) {
        zip <- "Postcode Not Found"
      }
      if(is.null(state)) {
        state <- "State Not Found"
      }
      if(is.null(country)) {
        country <- "Country Not Found"
      }
      if(is.null(output)) {
        geocode_data <- append(geocode_data, address)
      } else if (output == "string") {
        geocode_data <- append(geocode_data, address)
      } else if (output == "hash") {
        geocode_data[["housenumber"]] <- c(geocode_data[["housenumber"]], housenumber)
        geocode_data[["street"]] <- c(geocode_data[["street"]], street)
        geocode_data[["city"]] <- c(geocode_data[["city"]], city)
        geocode_data[["county"]] <- c(geocode_data[["country"]], country)
        geocode_data[["state"]] <- c(geocode_data[["state"]], state)
        geocode_data[["zip"]] <- c(geocode_data[["zip"]], zip)
        geocode_data[["country"]] <- c(geocode_data[["country"]], country)
      } else {
        geocode_data[["housenumber"]] <- c(geocode_data[["housenumber"]], housenumber)
        geocode_data[["street"]] <- c(geocode_data[["street"]], street)
        geocode_data[["city"]] <- c(geocode_data[["city"]], city)
        geocode_data[["county"]] <- c(geocode_data[["country"]], country)
        geocode_data[["state"]] <- c(geocode_data[["state"]], state)
        geocode_data[["zip"]] <- c(geocode_data[["zip"]], zip)
        geocode_data[["country"]] <- c(geocode_data[["country"]], country)
        geocode_frame <- rbind(geocode_frame, as.data.frame(geocode_data))
      }
    }
  }
  if(!nrow(geocode_frame)) {
    if(!(is.null(output %in% 'hash'))) {
      if(is.null(item)) {
        return(geocode_data)
      } else {
        return(geocode_data[item])
      }
    } else {
      return(geocode_data)
    }
  } else {
    return(as.data.frame(geocode_data))
  }
}