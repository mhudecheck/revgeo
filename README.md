# Reverse Geocoding in R with Google Maps and Photon 

This code lets you reverse geocode coordinate pairs with Google Maps and Photon.  As far as I'm aware, it's the first package in R which specifically allows you to use Photon and OpenStreetMaps to reverse geocode (as opposed to geocode).  This is pretty important, since the Google Maps API is limited to 2,500 free queries a day.

You can install revgeo() with the devtools package:
```
library(devtools)
install_github("mhudecheck/revgeo")
```
The syntax for revgeo() is pretty simple:
```
library('revgeo')

# Usage: revgeo(longitude, latitude, google=NULL, API=NULL, output=NULL, item=NULL)
# Example: revgeo(longitude=-86.46222, latitude=33.94954, google=NULL, API=NULL, output='hash', item='zip')
```
You'll need to create a Google Maps API, set google=TRUE, and include your API key with API='your API key' if you want to use revgeo() with Google Maps:
```
library('revgeo')

# Example: revgeo(longitude=-86.46222, latitude=33.94954, google=TRUE, API='your API key')
```
There are a couple of neat things you can do with revgeo().  The default return is an address string:
```
library('revgeo')

# Example: revgeo(-77.016472, 38.785026)
# Returns: "146 National Plaza, Fort Washington, Maryland, 20745, United States of America"
```
You can use output='hash' to return a hashed string and output='frame' to return a dataframe with seperate columns for house numbers, streets, cities, counties, states, countries, and postal codes. 

You can use output='hash' or output='frame' with item='housenumber', 'street', 'city', 'county', 'state', and 'country' to return anyone of those values. 

```
library('revgeo')

# Example: revgeo(-77.016472, 38.785026, output='hash', item='zip')
# Returns: "20745"
```
The Revgeo package is still very much a work in progress, so please send any comments or issues to the 'issues' section above. 

You can find documentation on the Google Maps API at: https://developers.google.com/maps/documentation/geocoding/start.

You can find documentation on the Photon API at: http://photon.komoot.de/

This package started as an extension to RPhoton/geocode to enable reverse geocoding with Photon, but it's since turned into a complete rewrite, with the added benefit of being able to use the Google Maps API without having to load multiple libraries.  If you'd like to geocode an address with Photon (and not reverse geocode latitude/longitude coordinates), you easily use their library at https://github.com/rCarto/photon/blob/master/R/geocode.R. 

I'll try to get around to including standard geocoding in this package, but it won't happen in the next couple of weeks.
