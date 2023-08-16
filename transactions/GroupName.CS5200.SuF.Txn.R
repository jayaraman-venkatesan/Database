# R Script Title : Transactions
#
# Description: This script performs transaction queries on database 
#
# Date: 2023-07-18






library("RMySQL")

library("DBI")



#' Connect to remote database
#' 
#' Connects to the remote db with preset credentials
connectToRemoteDB <- function() {
  db_user <- "thisispracticum"
  db_password <- "thisispracticum"
  db_name <- "demo_test"
  db_host <- "practicum1.c0lvnnjxlfni.us-east-1.rds.amazonaws.com"
  db_port <- 3306
  
  # Connect to the MySQL server
  mydb <- dbConnect(RMySQL::MySQL(), user = db_user, password = db_password,
                    dbname = db_name, host = db_host, port = db_port)
  
  
  return(mydb)
}


#' Find flight
#' 
#' Finds flight id with the data provided in dataframe
#' 
#' @param db database connection variables
#' @param dataFrame contains data to be searched on
#'
#'
#' @examples
#' findFlights(mydb,csvData)

findFlights <- function(db,dataFrame){
  
  dataFrame$flight_date <- as.Date(dataFrame$flight_date, format = "%m/%d/%Y")
  
  flights_data <- dbGetQuery(db, "SELECT fid, date, airline, aircraft, altitude,
                             heavy FROM flights")
  

  #flights_data$date <- as.Date(flights_data$date, format = "%m/%d/%Y")
  
  matching_flight <- which(
    flights_data$airline == dataFrame$airline &
      flights_data$aircraft == dataFrame$aircraft & 
      flights_data$altitude == dataFrame$altitude_ft &
      flights_data$date == dataFrame$flight_date
  )
  
  
  
  
  flightID <- NULL
  
  if (length(matching_flight) > 0) {
    matching_flight <- min(matching_flight)
  } 
  
  if (length(matching_flight) > 0) {
    # match found 
    matched_fid <- flights_data$fid[matching_flight]
    flightID <- matched_fid
  } 
  
  return(flightID)
  
}

#' Find conditions
#' 
#' Finds condition id with the data provided in dataframe
#' 
#' @param db database connection variables
#' @param dataFrame contains data to be searched on
#'
#'
#' @examples
#' findConditions(mydb,csvData)

findConditions <- function(db,dataFrame){
  conditions_data <- dbGetQuery(db, "SELECT cid, sky_condition, 
                                explanation FROM conditions")
  

  
  
  matching_condition <- which(
    conditions_data$sky_condition == dataFrame$sky_conditions 
  )

  if (length(matching_condition) > 0) {
    matching_condition <- min(matching_condition)
  } 
  

  conditionID <- NULL
  
  if (length(matching_condition) > 0) {
    # match found     
    matched_cid <- conditions_data$cid[matching_condition]
    conditionID <- matched_cid
  } 
  
  return(conditionID)
  
}

#' Find Airport
#' 
#' Finds airport id with the data provided in dataframe
#' 
#' @param db database connection variables
#' @param dataFrame contains data to be searched on
#'
#'
#' @examples
#' findAirport(mydb,csvData)

findAirport <- function(db,dataFrame){
  airport_data <- dbGetQuery(db, "SELECT airportState, airportCode, aid 
                             FROM airports")
  
  # store the results  
  airportTableState <- airport_data$airportState
  airportTableAid <- airport_data$aid
  
  
  
  # Find matching aid but comparing values 
  matching_index <- which(airport_data$airportState == dataFrame$origin)
  
  
  if (length(matching_index) > 0) {
    matching_index <- min(matching_index)
  } 
  
  returnAid <- NULL
  
  if (length(matching_index) > 0) {
    # there is a match 
    matched_aid <- airport_data$aid[matching_index]
    returnAid <- matched_aid
  } 
  
  return(returnAid)
}


#' Populating airport table
#' 
#' Populate airports table with the data provided in dataframe
#' @param db database connection variables
#' @param dataFrame contains data to be populated
#' @param index contains the index of the dataFrame to be populated
#'
#'
#' @examples
#' populateAirportsTableFromCsv(mydb,csvData,1)

populateAirportsTableFromCsv <- function(db, dataFrame, index) {
  table_name <- "airports"
  
  airportDataFrame <- dataFrame[index,]
  
  data_selected <- data.frame(
    airportState = airportDataFrame$origin,
    airportCode = airportDataFrame$airport
  )
  
  
  # Check if the record already exists in the table
  query <- sprintf(
    "SELECT COUNT(*) FROM %s WHERE  airportState = '%s'",
    table_name,
    data_selected$airportState
  )
  
  result <- dbGetQuery(db, query)
  
  # If the record already exists return 
  if (result[[1]] > 0) {
    cat("Record already exists in the table.")
    return()
  }
  
  
  dbWriteTable(db, table_name, data_selected, append = TRUE, row.names = FALSE)
}


#' Populating conditions table
#' 
#' Populate condition table with the data provided in dataframe
#' @param db database connection variables
#' @param dataFrame contains data to be populated
#' @param index contains the index of the dataFrame to be populated
#'
#'
#' @examples
#' populateConditionsTableFromCsv(mydb,csvData,1)

populateConditionsTableFromCsv <- function(db, conditionsDataFrame, index) {
  
  
  table_name <- "conditions"
  
  dataFrame <- conditionsDataFrame[index,]
  
  
  # creating data frame using ifelse to error check data
  data_selected <- data.frame(
    sky_condition = dataFrame$sky_conditions,
    explanation = ""
  )
  
  # Check if the record already exists in the table
  query <- sprintf(
    "SELECT COUNT(*) FROM %s WHERE sky_condition = '%s' AND explanation = '%s'",
    table_name,
    data_selected$sky_condition,
    ""
  )
  
  result <- dbGetQuery(db, query)
  
  # If the record already exists return 
  if (result[[1]] > 0) {
    cat("Record already exists in the table.")
    return()
  }
  
  
  # append data into target table 
  dbWriteTable(db, table_name, data_selected, append = TRUE, row.names = FALSE)
  
}


#' Populating Flights table
#' 
#' Populate flights table with the data provided in dataframe
#' @param db database connection variables
#' @param dataFrame contains data to be populated
#' @param index contains the index of the dataFrame to be populated
#'
#'
#' @examples
#' populateFlightsTableFromCsv(mydb,csvData,1)

populateFlightsTableFromCsv <- function(db, flightsDataFrame, index) {
  table_name <- "flights" 
  
  dataFrame <- flightsDataFrame[index,]
  
  
  data_selected <- data.frame(
    date = as.Date(dataFrame$flight_date, format = "%m/%d/%Y"),
    airline = ifelse(is.na(dataFrame$airline), "UNKNOWN", dataFrame$airline),
    aircraft = ifelse(is.na(dataFrame$aircraft), "UNKNOWN", dataFrame$aircraft),
    altitude = ifelse(is.na(dataFrame$altitude_ft), 0, dataFrame$altitude_ft),
    heavy = ifelse(tolower(dataFrame$heavy_flag) == "yes", 1, 0)
  )
  
  
  data_selected$origin <- findAirport(db,dataFrame)
  
  # Check if the record already exists in the table
  query <- sprintf(
    "SELECT COUNT(*) FROM %s WHERE airline = '%s' AND aircraft = '%s' AND
    altitude = '%s' AND date = '%s'" ,
    table_name,
    data_selected$airline,
    data_selected$aircraft,
    data_selected$altitude,
    data_selected$date
  )
  
  result <- dbGetQuery(db, query)
  
  # If the record already exists return 
  if (result[[1]] > 0) {
    cat("Flight Record already exists in the table.")
    return()
  }
  
  
  
  # append data into target table 
  dbWriteTable(db, table_name, data_selected, append = TRUE, row.names = FALSE)
}




#' Populating Strikes table
#' 
#' Populate strikes table with the data provided in dataframe
#' @param db database connection variables
#' @param dataFrame contains data to be populated
#' @param index contains the index of the dataFrame to be populated
#'
#'
#' @examples
#' populateStrikesTableFromCsv(mydb,csvData,1)

populateStrikesTableFromCsv <- function(db, dataFrame, index) {
  table_name <- "strikes" 
  
  dataFrame <- dataFrame[index,]
  

  
  # create data frame
  data_selected <- data.frame(
    numbirds = ifelse(is.na(dataFrame$wildlife_struck), 0,
                      dataFrame$wildlife_struck),
    impact = ifelse(is.na(dataFrame$impact), "UNKNOWN",dataFrame$impact),
    damage = ifelse(tolower(dataFrame$damage) == "no damage", 0, 1),
    altitude = ifelse(is.na(dataFrame$altitude_ft), 0, dataFrame$altitude_ft)
  )
  
  
  
  data_selected$fid <- findFlights(db,dataFrame)
  data_selected$conditions <- findConditions(db,dataFrame)
  
  
  dbWriteTable(db, table_name, data_selected, append = TRUE, row.names = FALSE)
}



#' Load data as transactions
#' Creates transactions and loads the data into database tables
#' 
#' @param db database connection variables
#' @param csvData contains data to be populated
#'
#'
#' @examples
#' loadBirdStrikesData(mydb,csvData)

loadBirdStrikesData <- function(db, csvData) {
  
  # using loop and each row has own transactions 
  tryCatch({
    for (i in seq_len(nrow(csvData))) {
      dbBegin(db)
      populateAirportsTableFromCsv(db, csvData, i)
      populateConditionsTableFromCsv(db, csvData, i)
      populateFlightsTableFromCsv(db, csvData, i)
      populateStrikesTableFromCsv(db, csvData, i)
      dbCommit(db)
      
    }
  }, error = function(e) {
    cat("Error occurred:", conditionMessage(e), "\n")
    dbRollback(db)
  })
}




main <- function() {
  
  # connect to remote db
  mydb <- connectToRemoteDB()
  
  #load transaction csv
  csvData <- read.csv("myoutput.csv", header = TRUE)
  
  #do transactions
  loadBirdStrikesData(mydb, csvData)
  
  #disconnect from db
  dbDisconnect(mydb)
}


#' Part 6 answers : 
#' 1. how would you back out inserted data? 
#' We have a rollback scheduled for all the errors caused during a transaction
#' 
#' 2. How do you deal with concurrency?
#' Have then under the transaction begina and commit as either they will work
#' as a whole or rollback. Also MySQL by default uses REPETABLE READ isolation 
#' level which by it self allows concurrency with the cost of having duplicates
#' which is fine in this case
#' 
#' 3. How do you assign synthetic keys in a concurrent environment?
#' Though there are multiple ways to address this, we use auto increment on
#' keys which mean the engine WILL have lock on auto increment column hence 
#' there will be no concurrency issues.

main()

