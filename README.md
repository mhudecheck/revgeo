# Reverse Geocoding in R with Google Maps and Photon 

This code lets you reverse geocode coordinate pairs with Google Maps and Photon.  As far as I'm aware, it's the first package in R which specifically allows you to reverse geocode (as opposed to geocode) with Photon and OpenStreetMaps.  This is pretty important, since the Google Maps API is limited to 2,500 (free) queries a day.

You can install revgeo() with the devtools package:
```
library(devtools)
install_github("mhudecheck/revgeo")
```
You can use revgeo() by:
```
library('revgeo')
revgeo(latitude, longitude, google=NULL, API=NULL, output=NULL, item=NULL)
```

You can specify whether you want the output to be a string, a hashed string, or a dataframe with option="string" | "hash" | "frame".

If you specify an option that isn't included in the list, the function will return the results as a dataframe.

Specifying a hashed string or dataframe allows you to specify whether you want to return the entire results, or a single item with item="".

Valid options for 'items=' include 'housenumber', 'street', 'city', 'county', 'state', and 'country'.

You can find documentation on the Google Maps API at: https://developers.google.com/maps/documentation/geocoding/start.

You can find documentation on the Photon API at: http://photon.komoot.de/

This started as an extension to RPhoton/geocode to enable reverse geocoding with Photon, but it's turned into a complete rewrite.  However, if you need to geocode an address (and not reverse geocode latitude/longitude coordinates), you can find their library at https://github.com/rCarto/photon/blob/master/R/geocode.R. 
