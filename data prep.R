#prepare packages
install.packages("rgdal")
install.packages("rgeos")
install.packages("plyr")
install.packages("mapview")
install.packages("leaflet")
install.packages("spgwr")
install.packages("rms")
install.packages("data.table")
#load packages
library(sp)
library(rgdal)
library(rgeos)
library(plyr)
library(mapview)
library(leaflet)
library(spgwr)
library(rms)
library(data.table)
setwd("D:\\MMG\\New idea")
#read layers
business = readOGR("business.shp")
crime = readOGR("Crime_points.shp")
Business_crime = readOGR("Business_crime.shp")
business_DEFGHI = readOGR("Businesses_DEFGHI.shp")
#CRS transform
business = spTransform(business, CRS("+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
crime = spTransform(crime, CRS("+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
Business_crime = spTransform(Business_crime, CRS("+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
business_DEFGHI = spTransform(business_DEFGHI, CRS("+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
data = as.data.frame(business_DEFGHI)


#changing crime data from factors to numeric.
# Columns
cols = colnames(data)
cols = cols[grepl("^CRM", cols)]

#changeing the factors to numeric characters and multiplying the crime rates by the relative polygon area.
for(i in cols) {
  data[[i]] = as.numeric(as.character(data[[i]]))
}

cols = colnames(data)
cols = cols[grepl("^LcESC", cols)]

for(i in cols) {
  data[[i]] = as.numeric(as.character(data[[i]]))
}
#creating the distance matrix
distance = gDistance(business_DEFGHI[], crime[], byid = TRUE)
distance_matrix = as.data.table(distance)
subset = as.data.table(distance)
# If data.table
subset[, FID_2 := rownames(subset)]



total = merge(data,subset,by="FID_2")
coordinates(total) = ~coords.x1 + coords.x2
writeOGR(total, ".", driver = "ESRI Shapefile", overwrite_layer = TRUE)
st_write(total, "total.shp", delete_dsn = TRUE)
#join(Business_crime, subset, by = FID_2, type = "left", match = "all")

# If data.frame
ids <- rownames(distance)
distance$FID_2 <- ids

#write.csv(subset, "distance.csv")



#ids = strsplit(rownames(distance), " ")
#distance$FID_1 = sapply(ids, "[", 1)
#dat$id2 = sapply(ids, "[", 2)


#values need to be part of the same dataframe or CSV.
#plain formula - Y = LcESC_values as a function of X = CRMCYTOTC

model1 = lm(data$LcESC ~ data$CRMCYTOTC, data = data)
summary(model1)
bw = gwr.sel(data$LcESC ~ data$CRMCYTOTC, data = data)
testrun1 = gwr(data$LcESC ~ data$CRMCYTOTC, data = data, bandwidth = bw, gwr.Gauss(distance_matrix, bw))
testrun1



#coords = cbind(data$coords.x1, data$coords.x2
# Columns
#cols = colnames(Withcrime)
#cols = cols[grepl("^CRM", cols)]

#changeing the factors to numeric characters and multiplying the crime rates by the relative polygon area.
#for(i in cols) {
#  Withcrime[[i]] = as.numeric(as.character(Withcrime[[i]]))
#  Withcrime[[i]] = Withcrime[[i]] * Withcrime[["percentage of total polygon area"]]
#}

#write.csv(distance_matrix,"distance_1.csv")

#dat = read.csv("distance_1.csv")

#proj = rgdal::make_EPSG()
#crs1 = sp::CRS("+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
#map = SpatialPointsDataFrame(data = data, coords = cbind(data$coordinates_x, data$coordinates_y), proj4string = crs1)
#plain formula - map$CRMCYMURD ~ colleges$
#colleges coordinates as X, polygon centroids as Y.


