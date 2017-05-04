# Reverse Geocoding in R with Google Maps, Photon, and Bing

This code lets you reverse geocode coordinate pairs with Google Maps, Photon, and Bing.  As far as I'm aware, it's the first package in R which specifically allows you to the Photon geocoder for OpenStreetMaps or Bing to reverse geocode (as opposed to geocode).  This is pretty important, since the Google Maps API is limited to 2,500 free queries a day.

You can install revgeo() through CRAN:
```
install.packages('revgeo')
```
You also install revgeo() with the devtools package:
```
library(devtools)
install_github('mhudecheck/revgeo')
```
The syntax for revgeo() is pretty simple:
```
library('revgeo')

# Usage: revgeo(longitude, latitude, provider=NULL, API=NULL, output=NULL, item=NULL)
# Example: revgeo(longitude=-86.46222, latitude=33.94954, provider=NULL, API=NULL, output='hash', item='zip')
```
You'll need to create a Google Maps API, set provider='google' (or google=TRUE for v.11), and include your API key with API='your API key' if you want to use revgeo() with Google Maps:
```
library('revgeo')

# Example: revgeo(longitude=-86.46222, latitude=33.94954, provider='google', API='your API key')
```
The same is true for Bing if you're using v.12 (available from GitHub):
```
library('revgeo')

# Example: revgeo(longitude=-86.46222, latitude=33.94954, provider='bing', API='your API key')
```
There are a couple of neat things you can do with revgeo().  The default return is an address string:
```
library('revgeo')

# Example: revgeo(-77.016472, 38.785026)
# Returns: "146 National Plaza, Fort Washington, Maryland, 20745, United States of America"
```
You can use output='hash' to return a hashed string and output='frame' to return a dataframe with seperate columns for house numbers, streets, cities, counties, states, countries, and postal codes. 
```
library('revgeo')

# Example: result <- revgeo(-77.016472, 38.785026, output="frame")
# Returns: data frame (result) with names(result) equal to 'housenumber', 'street', 'city', 'county', 'state', and 'country'.
```
You can also use output='hash' or output='frame' with item='housenumber', 'street', 'city', 'county', 'state', and 'country' to return any one of those values. 
```
library('revgeo')

# Example: revgeo(-77.016472, 38.785026, output='hash', item='zip')
# Returns: "20745"
```
The Revgeo package is still very much a work in progress, so please send any comments or issues to the 'issues' section above. 

You can find documentation on the Google Maps API at: https://developers.google.com/maps/documentation/geocoding/start.

You can find documentation on the Photon API at: http://photon.komoot.de/.

You can find documentation on the Bing API at: https://www.bingmapsportal.com/.

This package started as an extension to RPhoton/geocode to enable reverse geocoding with Photon, but it's since turned into a complete rewrite, with the added benefit of being able to use the Google Maps API and the Bing API without having to load multiple libraries.  If you'd like to geocode an address with Photon (and not reverse geocode latitude/longitude coordinates), you can easily use their library at https://github.com/rCarto/photon/blob/master/R/geocode.R. 

I'll try to get around to including standard geocoding in this package, but it isn't a promise.
