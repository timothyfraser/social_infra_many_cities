# README

## “`social_infra_many_cities`: a repository for analyzing many US cities’ social infrastructure”

### 0. Setup

To proceed, you’ll need to make a `keys.R` file, located within this
main repository directory. This file will contain your Google Places API
key and your Census API key, which you can register for online in a
matter of minutes. Use `keys_template.R` as a template, edit it, then
rename it to `keys.R`. (Be sure not to share these keys, as they contain
your sensitive user data.)

    #' `keys.R`
    #'
    #' File of API keys. DO NOT SHARE ON GITHUB.
    #' Fill this in with your values and then change file name to keys.R

    # Let's load in your API keys here
    Sys.setenv(
      # Google Places API Key
      "PLACES_API_KEY" = "a_very_long_key",
      # Census API Key
      "CENSUS_API_KEY" = "another_very_long_key"
    )

### 1. API Searches

Below, we’ll walk through the replication process for gathering our
social infrastructure sites using our 2 km (most efficient) grid, and
tally the results up using our 1 km grid.

This demonstration will use D. Cooley’s helpful *googleway* vignette
from November 2020, available
[here](https://cran.r-project.org/web/packages/googleway/vignettes/googleway-vignette.html#google-places-api).

First, we have to enable the Google Maps Places API. Second, we need to
get an API Key. I made it restricted to work only from RStudio Cloud and
only for the Places API. Let’s set it below.

## 1.1 Select Terms

This project will use the Google Maps Places API to collect geocoded
point and polygon data of social infrastructure sites in the United
States. For $0.05 per longitude-latitude coordinates queries, the API
provides a near-complete record of the locations and meta-data of up to
20 social infrastructure sites within a 1 square-kilometer area, the
average size of an American’s neighborhood (Donaldson 2013).

We’ve validated the measures using a case study of Boston, and they
worked pretty well! Let’s repeat our measures for some more cities.

We can use the following 10 keywords, which actually produce results in
Boston: libraries, community centers, city hall facilities, places of
worship, cafes, bookstores, parks, squares, fountains, and gardens.

We should exclude the following 9 keywords, which didn’t produce any
team results in Boston: coffeeshop, barbershops, beauty salons,
recreation centers, sports fields, museums, aquariums, art galleries,
and zoos.

This will cost us 0.05 dollars per search, for every 2 square kilometer
grid cells, which are more cost effective but do not lose us data
accuracy (see map from our validation study). (After obtaining results,
we can analyze it on whatever size grid we want - these two stages are
not linked.) So, we are going to pay at a rate of $0.25 per km2 to
receive all 10 indicators. This is an actual, accurate price estimate,
based on Boston.

1.  Where shall we map? Let’s then for the 3 biggest cities in the US,
    this month!!

-   New York (pop = 8,622,000, 778 km2, $194)
-   Los Angeles (pop = 4,085,000, 1215 km2, $303)
-   Chicago (pop = 2,670,000, 606 km2, $151),
-   Houston (pop = 2,378,000, 1732 km2, $432)
-   Phoenix (pop = 1,608,139, 1339 km2, $334)

Cities 1-5 subtotal 1,414 dollars out of 3,000.

Let’s try this out (just see if each city’s queries work, map it), and
then repeat for the next 5 biggest cities:

-   Philadelphia, PA (pop: 1,590,402, 367 km2, $92)
-   San Antonio, TX (pop: 1,579,504, 1307 km2, $327)
-   San Diego, CA (pop: 1,469,490, 964 km2, $241)
-   Dallas, TX (pop: 1,400,337, 620 km2, $155)
-   San Jose, CA (pop: 1,036,242, 469 km2, $117)

Cities 5-10 subtotal: 932 dollars (cumulative: 2346 out of 3000)

## 1.2 Functions

To map sites most efficiently, we want to get the smallest geography
possible (blocks), zoom into just those within the city limits, filter
out blocks that are entirely water, and then make a grid out of those
remaining polygons. In the files below, we write a code to do this using
the census’s handy-dandy R package `tigris`.

Check out these function files for more details:

-   `R/get_bg.R`
-   `R/get_bounds.R`
-   `R/get_data.R`
-   `R/get_grid.R`
-   `R/get_grid_1km.R`
-   `R/get_search.R`
-   `R/get_api.R`
-   `R/get_testapi.R`
-   `R/get_results.R`
-   `R/get_map.R`
-   `R/get_diagnostics.R`
