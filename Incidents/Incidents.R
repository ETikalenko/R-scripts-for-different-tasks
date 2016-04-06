library(RCurl)
library(xlsx)
library(RJSONIO)
library(plyr)

startDate <- "2014-01-01"
endDate <- "2015-01-01"
userName <- "CTDOT_Statewide"
state <- "CT"

commandLine <- paste("java -jar got.jar username=", userName, " since=", startDate, " until=", endDate, sep = "")
system(commandLine)

# Need to format address before using Google Maps. Google Maps formats all addresses following way: <street><city><state><postal code><country>. 
# If in location street is in the end of input string google geocoder can not find the street.
# Examples of formatted addresses from google geocoder: 
# S8N at Exit 44 (US 202 & CHRISTOPHER RD) TORRINGTON CT ---> Christopher Rd, Torrington, CT 06790, USA
# I84W at Exit 50 (US 44 WB) HARTFORD CT   ---> Hartford, CT, USA
# I91S 0.50 miles before Exit 38A (RTE 75 DAY HILL RD (X38A,B)) WINDSOR CT ---> Windsor, CT, USA
#
# So need to find corresponding street (which can be found in parenthesys in most cases).
# Input parameter: location.
# Output: street if it was found or empty string.
getStreet <- function(str) {
# Evaluating the expression between parenthesys and remove all unnecessary from it
# Examples of input ----> output: 
# (US 202 & CHRISTOPHER RD)  ----> CHRISTOPHER RD, (RTE 75 DAY HILL RD (X38A,B)) ----> DAY HILL RD, (US 44 WB) ----> WB
  pos <- gregexpr(pattern = "[\\(\\)]", str)
  checkStreet <- substr(str, min(pos[[1]])+1, max(pos[[1]]-1))
  checkStreet <- gsub("\\(.*?\\)", "", checkStreet)
  street <- gsub("^[^_]*([0-9]|\\&)", "", checkStreet)
  
  return(street)
}

# Function for getting geo coordinates for given address. Using Google Maps API.
# It's possible to get json-file with coordinates. Function constructs url for getting files and extract latitude and longitude.
# Input parameter: address.
# Output: vector with coordinates or vector with two empty strings
getLatLong <- function(str) {
   rootURL <- "https://maps.google.com/maps/api/geocode/"
#   targetURL <- URLencode(paste(rootURL, "json?address=", str, "&sensor=false", sep = ""))
   targetURL <- URLencode(paste(rootURL, "json?address=", str, "&key=YOUR_SECRET_API_KEY", sep = ""))
   
# Getting information from google map
   json <- getURL(targetURL)
   x <- fromJSON(json)
   
# if google maps found an address then return vector with coordinate, else - vector with empty strings
   if (x$status == "OK") {
     lat <- x$results[[1]]$geometry$location[1]
     lng <- x$results[[1]]$geometry$location[2]
     result <- c(lat, lng)
   } else {
     print(paste("Can not find geo coords for: ", str))
     result <- c("", "")
   }
  
  return(result)
}

# Function for tweet parsing. 
# Input parameter: raw string
# Output: row for output data frame with required format
parseString <- function(str) {
# Split string on separate words
  token <- unlist(strsplit(str, split = " "))
  stopifnot(length(token) > 0)

# Detecting rows with "Cleared:". Output data frame has additional column for marking cleared records
# If first token is  "Cleared:" then mark row as cleared (set field to 1) and delete this token
  if (token[1] == "Cleared:") {
    cleared <- 1
    token <- token[-1]
  } else { cleared <- 0}
  
# First token is number of incident  
  incidentNum <- gsub("[\\(\\)]", "", token[1])

  incidentType <- token[2]
  token <- token[-c(1, 2)]
  while (token[1] != '-') {
    incidentType <- paste(incidentType, token[1])
    token <- token[-1]
  }
  
# If last token not AM, PM or not have ":" then delete it - processing of strings with hashtags and strings without AM or PM
  time <- ""
  while (unlist(regexec(pattern = "\\d\\/", token[length(token)])) == -1) {
    if (unlist(regexec(pattern = "AM|PM|\\:", token[length(token)])) != -1) {
      time <- paste(token[length(token)], time)
      token <- token[-length(token)]
    } else token <- token[-length(token)]
  }
  date <- token[length(token)]
  token <- token[-c((length(token)))]
  
# Deleting last preposition "at" from location
  if (token[length(token)] == "at") token <- token[-length(token)]
  
# Deleting sign "-" after incident type
  token <- token[-1]
  
# Find city. City is all before roadway which is string containing 0-9 or word OTHER
  city <- ""
  while ((unlist(regexec(pattern = "[0-9]|OTHER", token[1])) == -1)&(length(token) != 0)) {
       city <- paste(city, token[1])
       token <- token[-1]
     }
  
  if (length(token) != 0) {
    roadway <- token[1]
    location <- paste(token[-1], collapse = " ")
  } else { 
    roadway <- ""
    location <- ""
#    print(paste("Check the row with incident number (incomplete information): ", incidentNum))
  }

# Defining street  
#  street <- getStreet(location)
  
# Try to define geocoords with street. If result is not ok then try to detect geocoords for city
#  geoCoord <- getLatLong(paste(street, city, state, "USA"))
#  Sys.sleep(1)
 # if (geoCoord[1] == "") {
  #  print(paste("Trying again to get geo for incidentNum: ", incidentNum))
   # geoCoord <- getLatLong(paste(city, state, "USA"))
    #Sys.sleep(1)
#  }
#  latitude <- geoCoord[1]
#  longitude <- geoCoord[2]
  
# Exclude rows with empty city from output data frame
  latitude = ""
  longitude = ""
  if (city != "") {
    result <- c(cleared, date, time, incidentNum, incidentType, city, state, roadway, location, clearTime, 
                clearDate, latitude, longitude)
  } else { 
  result <- c()  
  }
  
  return(result)
}

#====================================================================================================================================
# Main part of program

# Reading the file with data
dat <- read.csv(file = "output_got.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)
dat <- dat$text

clearTime <- ""
clearDate <- ""

# Creating empty data frames. 
# df - for output.
# log - for not parsed tweets.
df <- matrix(ncol = 13, nrow = 0)
log <- matrix(ncol =1, nrow = 0)

# Parse file
i <- 1
for (i in 1:length(dat)) {
  row <- try(parseString(dat[i]))
  if("try-error" %in% class(row)) {
    print(row)
    log <- rbind(log, dat[i])
    next
  }
  if (length(row) != 0) {
    df <- rbind(df, row)
  } else {
    log <- rbind(log, dat[i])
  }
} 

# Set column names for output data frame and convert factors to characters
df <- setNames(as.data.frame(df), c("cleared", "Date", "Time", "IncidentNum", "IncidentType", "City", "State", "Roadway", "Location", 
                                    "ClearTime", "ClearDate", "Latitude", "Longitude"))
df <- data.frame(lapply(df, as.character), stringsAsFactors = FALSE)

log <- setNames(as.data.frame(log), c("Nonparsed"))
log <- data.frame(lapply(log, as.character), stringsAsFactors = FALSE)

# Processing records with mark "Cleared:"
clear <- df[df$cleared == 1,]

for (i in 1:nrow(clear)) {
  clearInc <- clear[i,]
  replaceString <- df[df$IncidentNum == clearInc$IncidentNum & df$cleared == 0,]
  if (nrow(replaceString) == 0) next
  df[df$IncidentNum == clearInc$IncidentNum & df$cleared == 0,]$ClearTime <- clear[i,]$Time
  df[df$IncidentNum == clearInc$IncidentNum & df$cleared == 0,]$ClearDate <- clear[i,]$Date
  df <- df[!(df$IncidentNum == clearInc$IncidentNum & df$cleared == 1),]
}

df <- df[!(df$IncidentNum == clearInc$IncidentNum & df$cleared == 1),]

df$cleared <- NULL

# Create output files
write.xlsx(df, file = "incidents.xlsx", row.names = FALSE)
if (nrow(log) != 0) { 
  write.xlsx(log, file = "log.xlsx", row.names = FALSE)
}
