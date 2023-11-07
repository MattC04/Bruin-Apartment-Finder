# Apartments Practice

library(tidyverse)
library(ggplot2)
library(tidygeocoder)
library(sf)

apt2 <- read.csv("C:/Users/stell/Desktop/UCLA/Data Science Related Stuffs/NSDC Practice/apt_updated.csv")
str(apt2)
colnames(apt2) <- gsub("name_", "", colnames(apt2))
colnames(apt2)[1] <- "place"
colnames(apt2) <- gsub("room_", "", colnames(apt2))
colnames(apt2) <- gsub("_name", "", colnames(apt2))
colnames(apt2) <- vapply(colnames(apt2), str_to_lower, "price")
colnames(apt2)[3] <- "url"
colnames(apt2)  

apt2 <- apt2 %>% 
  filter(name != "")

  # for those that have other features listed, features3 read in the community ameneties (not apartment features as the others)
apt2 %>% 
  select(place, features3, features, features2, floor, kitchen) %>% 
  filter(features == "" & features2 == "" & floor == "" & kitchen == "") 
sum(apt2$features3 != "")

incorrect <- ((apt2$features != "") | (apt2$features2 != "") | (apt2$highlights != "") | (apt2$kitchen != "") | (apt2$floor != "")) & (apt2$features3 != "")
apt2$features3[incorrect] <- ""


apt2$master_list <- paste(apt2$features, apt2$features2, apt2$features3, apt2$highlights, apt2$kitchen, apt2$floor, sep = "\n")
apt2 <- apt2 %>% 
  select(place, address, backup, rating, name, price, bed, bath, sqft, distance, blurb, built, property, furnished, walkscore, transitscore, master_list)

sum(apt2$master_list == "\n\n\n\n\n")

apt2 %>% 
  filter(master_list == "\n\n\n\n\n") %>% 
  select(place) %>% 
  unique()
  # checked and all are missing features

apt2 <- apt2 %>% 
  filter(master_list != "\n\n\n\n\n")




# Rating -------------------------------------------------------------------------------------------
subset(apt2, !complete.cases(apt2)) # incomplete cases from rating

apt2 %>% 
  select(place, rating) %>% 
  filter(is.na(rating)) %>% 
  unique()
  # properties all "not yet rated"
  # left as NAs


# Fix incorrect furnished values ------------------------------------------------
apt2 %>% 
  select(furnished) %>%
  unique()

apt2 <- apt2 %>% 
  mutate(furnished = str_to_lower(furnished))

apt2$furnished <- vapply(apt2$furnished, function(x){
  if (x != "furnished") {
    return("FALSE")
  } else {
    return("TRUE")
  }
}, "furnished")

unique(apt2$furnished)
apt2$furnished <- as.logical(apt2$furnished) | grepl("Furnished", apt2$master_list)

# Convert sqft into int --------------------------------------------------------

unique(apt2$sqft)
sum(!grepl("sq ft", apt2$sqft))

apt2 %>% 
  filter(!grepl("sq ft", sqft)) %>% 
  select(sqft) %>% 
  unique() # missing values

apt2$sqft <- vapply(apt2$sqft, function(x){
  y <- (strsplit(x, split = " ")[[1]][1])
  y <- gsub(",", "", y)
  return(as.integer(y))
}, integer(1))
head(apt2$sqft)
sum(is.na(apt2$sqft))

apt2 <- apt2 %>% 
  filter(!is.na(sqft))
  #checked website and was also missing sq ft so deleted the NAs


# Convert built year -----------------------------------------------------------

unique(apt2$built)
apt2$property[grepl("units", apt2$built)] <- apt2$built[grepl("units", apt2$built)]
apt2$built[!grepl("Built in", apt2$built)] <- ""

apt2$built <- vapply(apt2$built, function(x){
  y <- gsub("Built in ", "", x)
  return(as.integer(y))
}, integer(1))

# Convert property ------------------------------------------------------------------
unique(apt2$property)

apt2 %>% 
  filter(!grepl("/", property)) %>% 
  select(place, property) %>% 
  unique() # no missing obs but missing stories

no_stories <- !grepl("/", apt2$property)
apt2$property[no_stories] <- paste(apt2$property[no_stories], "/ ", sep ="")

split_prop <- unlist(strsplit(apt2$property, "/"))
apt2$property_units <- split_prop[seq(1, length(split_prop), 2)]
apt2$property_units <- gsub(" units", "", apt2$property_units)
apt2$property_units <- as.integer(apt2$property_units)

apt2$property_stories <- split_prop[seq(2, length(split_prop), 2)]
apt2$property_stories <- as.integer(gsub(" stories", "", apt2$property_stories))
  # NAs from missing stories values

apt2 %>% 
  select(place, name, property, property_units, property_stories) %>% 
  filter(place == "2434 Purdue Ave")


# Convert price-----------------------------------------------------------------
unique(apt2$price)

apt2 <- apt2 %>% 
  filter(price != "Call for Rent" & price != "Call for Rent / Person") 

og_price <- apt2$price
per_person <- grepl(" / Person", apt2$price)

apt2$bed_count <- gsub(" beds", "", apt2$bed)
apt2$bed_count <- gsub(" bed", "", apt2$bed_count)
apt2$bed_count <- gsub("Studio", "1", apt2$bed_count)
apt2$bed_count <- as.integer(apt2$bed_count)

unique(apt2$bed_count)

avg_price <- function(x, y = 1) {
  x <- gsub(",", "", x)
  x <- gsub("[$]", "", x)
  per <- FALSE
  if (grepl(" / Person", x)) {
    x <- gsub(" / Person", "", x)
    per <- TRUE
  }
  if (grepl(" – ", x)) {
    x <- unlist(str_split(x, " – "))
  }
  x <- as.integer(x)
  
  if (length(x) > 1) {
    x <- round(mean(x))
  } 
  
  if (per) {
    return(c(x, x * y))
  } else {
    return (c(round(x / y), x))
  }
}

temp <- data.frame(price = apt2$price,
                   bed_count = apt2$bed_count)  

for(i in seq_along(temp$price)) {
  temp$price_pax[i] <- avg_price(temp$price[i], temp$bed_count[i])[1]
  temp$total_price[i] <- avg_price(temp$price[i], temp$bed_count[i])[2]
}

apt2$price_pax <- temp$price_pax
apt2$total_price <- temp$total_price


# sq ft per person ------------------------------------------------------------

apt2$total_sqft <- apt2$sqft
apt2$total_sqft[per_person] <- apt2$sqft[per_person] * apt2$bed_count[per_person]

apt2$sqft_pax <- apt2$sqft
apt2$sqft_pax[!per_person] <- round(apt2$sqft[!per_person] / apt2$bed_count[!per_person])

# Fix master_list ----------------------------------------------------------------
head(apt2$master_list)
apt2$master_list <- gsub("\n", ", ", apt2$master_list)

all <- NULL
for (i in apt2$master_list) {
  all <- c(all, unlist(strsplit(i, ", ")))
}
unique(all)

# Because there's too many features, I'm just going to pick a few
# Washer/Dryer
# High Speed Internet Access"
# Air Conditioning
# Stove

apt2$laundry <- grepl("Washer/Dryer", apt2$master_list)
apt2$internet <- grepl("High Speed Internet Access", apt2$master_list) | grepl("Wi-Fi", apt2$master_list)
apt2$ac <- grepl("Air Conditioning", apt2$master_list)
apt2$balcony <- grepl("Balcony", apt2$master_list)
apt2$dishwasher <- grepl("Dishwasher", apt2$master_list)
apt2$patio <- grepl("Patio", apt2$master_list)
apt2$hardwood <- grepl("Hardwood Floors", apt2$master_list)
apt2$tile <- grepl("Tile Floors", apt2$master_list)
apt2$heating <- grepl("Heating", apt2$master_list)
apt2$intercom <- grepl("Intercom", apt2$master_list)
apt2$granite <- grepl("Granite Countertops", apt2$master_list)
apt2$accessible <- grepl("Wheelchair Accessible", apt2$master_list)
apt2$ice <- grepl("Ice Maker", apt2$master_list)

# Check distance ---------------------------------------------------------------------

correct <- grepl(" miles from UCLA.", apt2$blurb) | grepl(" mile from UCLA.", apt2$blurb)
sum(!correct)

head(apt2$distance)
apt2$distance <- vapply(apt2$distance, function(x){
  x <- gsub(" mi", "", x)
  return(as.numeric(x))
}, 0.7)


apt2 %>%
  select(place, address, distance, blurb) %>% 
  filter(!(grepl(" miles from UCLA.", blurb) | grepl(" mile from UCLA.", blurb))) %>% 
  select(place) %>% 
  unique()
  # 188 places w/ incorrect distance
  # temporarily set as NA

apt2$distance[!correct] <- NA

# Geocode address ---------------------------------------------------------------
unique(apt2$address) # has incomplete addresses
unique(apt2$backup) # has all addresses

apt2$address <- vapply(apt2$backup, function(x){
  x <- gsub("Property Address: ", "", x)
  x <- gsub("\n", " ", x)
  if (grepl("-", x)) {
    x <- unlist(strsplit(x, "-"))
    return(x[length(x)])
  }
  return(x)
}, "template")

apt2 <- apt2 %>% 
  geocode(address = address)

apt2$long
apt2$lat
apt2 %>% 
  select(place, address, long, lat) %>% 
  filter(is.na(long) | is.na(lat))

na_add <- is.na(apt2$long)
apt2$lat[na_add] <- c(34.028550, 34.028550, 34.053220, 34.023170, 34.023170, 34.023170, 34.023170, 34.061910)
apt2$long[na_add] <- c(-118.443672, -118.443672, -118.429810, -118.437780, -118.437780, -118.437780, -118.437780, -118.448420)
  # fixed NAs manually

my_sf <- st_as_sf(apt2, coords = c('long', 'lat'))
st_crs(my_sf) = 4326

ucla <- data.frame("long" = -118.439789907,
                   "lat" = 34.06999972)
ucla <- st_as_sf(ucla, coords = c("long", "lat"), crs = "epsg:4326")

dist <- as.numeric(st_distance(my_sf, ucla)) * 0.000621371
apt2$distance[correct] - dist[correct]
max((apt2$distance[correct] - dist[correct] ) / sd(apt2$distance[correct]  - dist[correct]))

apt2$dist <- dist


ggplot(my_sf) +
  geom_sf()


# Finalize --------------------------------------------------------------------

colnames(apt2)
final <- apt2 %>% 
  select(place, room = name, price_pax, total_price, bed, bath, sqft_pax, total_sqft, rating, built, property_units, 
         property_stories, walkscore, transitscore, distance, dist, features = master_list,
         laundry, furnished, internet, ac, balcony, dishwasher, patio, hardwood,
         tile, intercom, granite, accessible, ice,
         long, lat, bed_count, address)

glimpse(final)
  # place: name of apartment (may have multiple rooms)
  # room: name of room that is being rented out
  # price_pax: room price per person
    # calculated by total price / # of beds 
      # if price was not already in " / Person" format
      # see avg_price function
    # note: for prices given in ranges, I took the average price of the range
  # total_price: total price for entire room based on bedroom count
  # bed: # of bedrooms
  # bath: # of bathrooms
  # sqft_pax: sqft per person
  # total_sqft: total sqft of room
  # rating: property rating; see apartments.com to see how it's calculated
  # built: year apartment was built
  # property_units: # of total units in apartment
  # property_stories: # of total floors in apartment
  # walkscore: measures walkability; see apartments.com to see how it's calculated
  # transitscore: measures public transportation accessibility; see apartments.com to see how it's calculated
  # distance: distance recorded on apartments.com
    # NAs mean the distance recorded was not for UCLA but another school
  # dist:straight line distance calculated with the sf package
    # IDK how well this worked so plz let me know if its wrong
    # or feel free to try and calculate using another method
      # I don't have a Google API key so wasn't able to use the other, probably more accurate methods
  # features: master list of features listed for the apartment
  # internet, ac, balcony, dishwasher, patio, hardwood, tile, intercom, granite, accessible, ice (maker): features
  # long: longitude
  # lat: latitude
  # bed_count: number of bedrooms in numeric format
  # address: address of apt
  


# Export Dataset as TSV -------------------------------------------------------
# csv doesn't work bc a lot of vals have commas & it messes up the table


# write.table(final, "C:/Users/stell/Desktop/UCLA/Data Science Related Stuffs/NSDC Practice/updated.tsv", sep = "\t", row.names = FALSE)
  # uncomment to export


