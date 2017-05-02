# Reverse Geocoding in R with Google Maps API and Photon API

This code lets you reverse geocode coordinate pairs with Google Maps and Photon.
You can specify whether you want the output to be a string, a hashed string, or a dataframe with option="string" | "hash" | "frame"
If you specify an option that isn't included in the list, the function will return the results as a dataframe
Specifying a hashed string or dataframe allows you to specify whether you want to return the entire results, or a single item with item=""
Valid options for 'items=' include 'housenumber', 'street', 'city', 'county', 'state', and 'country'

This project started out as an extension of https://github.com/rCarto/photon/blob/master/R/geocode.R to enable reverse geocoding with Photon API
It's since turned into a complete rewrite, with the added benefit of allowing reverse geoocoding with Google Maps API
As far as I'm aware, there is no shared code between the two projects, but kudos to them for introducing the Photon API to the community

Please leave any issues or break/fix actions under 'Issues'
