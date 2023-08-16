library(DBI)

library(RSQLite)
library(dplyr)


# Establish a connection to your SQLite database
connectToDB <- function(){
  con <<- dbConnect(RSQLite::SQLite(), "my_database.db")
  
  db_user     <- 'admin'
  db_password <- 'Dhanoo30$'
  db_name     <- 'database1'
  db_host     <- 'database1.ct2zux9jbsp4.us-east-2.rds.amazonaws.com'
  db_port     <- 3306
  
  ## Connect to remote server database
  dbcon <<-  dbConnect(RMySQL::MySQL(),
                       user = db_user,
                       password = db_password,
                       dbname = db_name,
                       host = db_host,
                       port = db_port)
  
  if (dbIsValid(dbcon)) {
    cat("Connection to MySQL database successful!\n")
  } else {
    cat("Failed to connect to the MySQL database.\n")
  }
}


getInitialData <- function(){
  
  query_select <- "SELECT * FROM products"
  productdf <<- dbGetQuery(con, query_select)
  
  
  query_select <- "SELECT * FROM reps"
  repdf <<- dbGetQuery(con, query_select)
  
  query_select <- "SELECT * FROM customers"
  cusdf <<- dbGetQuery(con, query_select)
  cusdf<<- cusdf%>% select(cID, country) 
  
}



# Cleans warehouse byb dropping table if present
performWareHouseCleanup <- function(){
  
  dbExecute(dbcon,"DROP TABLE IF EXISTS product_facts")
  dbExecute(dbcon,"DROP TABLE IF EXISTS rep_facts")
  dbExecute(dbcon,"DROP TABLE IF EXISTS RegionDim")
  dbExecute(dbcon,"DROP TABLE IF EXISTS DateTimeDim")
  
  
}


# Creates star schema by using facts and dimention tabels

# Used date as diention tabels as we need date base analysis
createStarSchema <- function(){
  
  query3<-"CREATE TABLE DateTimeDim (
  datetimedim_key VARCHAR(255) PRIMARY KEY,
  quarter_of_year INT,
  year_num INT,
  month_of_year INT
  );"
  dbExecute(dbcon, query3)
  
  
  queryRegDim<-"CREATE TABLE RegionDim (
  territory VARCHAR(255),
  country VARCHAR(255),
  Regkey VARCHAR(255),
  CONSTRAINT PRIMARY KEY (Regkey)
  );"
  dbExecute(dbcon, queryRegDim)
  
  
  queryprod<-"CREATE TABLE product_facts (
  productname VARCHAR(255),
  Regkey VARCHAR(255),
  datetimedim_key VARCHAR(255),
  total INT,
  CONSTRAINT PRIMARY KEY (productname,Regkey,datetimedim_key),
  FOREIGN KEY (datetimedim_key) REFERENCES DateTimeDim(datetimedim_key),
  FOREIGN KEY (Regkey) REFERENCES RegionDim(Regkey)
  );"
  dbExecute(dbcon, queryprod)
  
  
  
  
  queryRep<-"CREATE TABLE rep_facts (
  RepName VARCHAR(255),
  datetimedim_key VARCHAR(255),
  total INT,
  CONSTRAINT PRIMARY KEY (RepName,datetimedim_key),
  FOREIGN KEY (datetimedim_key) REFERENCES DateTimeDim(datetimedim_key)
  );"
  dbExecute(dbcon, queryRep)
  
  
  
}

# Populates warehouse by reading sqlite db and loading in appropriate mysql tables
populateWareHouse <- function(){
  
  # populating date dim table
  
  query_select <- "SELECT DISTINCT date FROM salestxn"
  result <- dbGetQuery(con, query_select)
  result$when_happened <- as.Date(result$date, format = "%m-%d-%Y")
  # Extract components from the date column
  result <- result %>%
    mutate(
      quarter_of_year = lubridate::quarter(when_happened),
      year_num = lubridate::year(when_happened),
      month_of_year = lubridate::month(when_happened),
      month_of_year = sprintf("%02d", month_of_year))
  result$datetimedim_key <- paste(result$year_num, result$month_of_year, result$quarter_of_year, sep = "_")
  datedf<-result
  # Create the dataframe
  datedf1 <- result %>% 
    select(datetimedim_key, quarter_of_year, year_num, month_of_year)
  dbWriteTable(dbcon, "DateTimeDim", datedf1, append = TRUE, header = TRUE, 
               row.names = FALSE)
  
  
  
  datedfMap <- datedf %>%
    select(datetimedim_key, when_happened) %>%
    distinct(when_happened, .keep_all = TRUE)
  datedfMap
  
  
  query_select <- "SELECT pID,repID,cID,date, amount, qty FROM salestxn"
  salesdf <- dbGetQuery(con, query_select)
  
  
  datedfMap$when_happened <- format(as.Date(datedfMap$when_happened), "%m-%d-%Y")
  datedfMap <- datedfMap %>% 
    rename( "date"="when_happened")
  datedfMap
  combined_data <- merge(salesdf, datedfMap, by="date")
  
  # Create the pid, customerdim_key, and total columns based on your requirements
  
  repdf$RepName <- paste(repdf$firstName,repdf$lastName,sep=" ")
  repdf <- repdf %>% rename( "repID"="rID")
  repdf <- repdf %>% select(repID, RepName, territory)
  combined_dataWithRep <- merge(combined_data, repdf, by="repID")
  combined_dataWithRep <- merge(combined_dataWithRep, cusdf, by="cID") 
  combined_dataWithRep <- merge(combined_dataWithRep, productdf, by="pID")
  
  
  regiondf <- combined_dataWithRep %>% select(territory, country) %>%  distinct()
  regiondf$RegKey <- paste(regiondf$territory, regiondf$country, sep = "_")
  
  dbWriteTable(dbcon, "RegionDim", regiondf, append = TRUE, header = TRUE, 
               row.names = FALSE)
  combined_dataWithRep$Regkey  <- paste(combined_dataWithRep$territory, combined_dataWithRep$country, sep = "_")
  combined_dataWithRep <- combined_dataWithRep %>%  mutate(pid = pID,
                                                           total = amount * qty)
  productdf <- combined_dataWithRep %>% 
    select(prod, Regkey, datetimedim_key, total)
  repfact_df<- combined_dataWithRep %>% 
    select(RepName, datetimedim_key, total)
  grouped_productdf <- productdf %>%
    group_by(prod, Regkey, datetimedim_key) %>%
    summarise(total = sum(total))
  grouped_repdf <- repfact_df %>%
    group_by(RepName, datetimedim_key) %>%
    summarise(total = sum(total))
  
  grouped_productdf <- grouped_productdf  %>% rename("productname"="prod")
  grouped_productdf
  dbWriteTable(dbcon, "product_facts", grouped_productdf, append = TRUE, header = TRUE, 
               row.names = FALSE)
  grouped_repdf 
  dbWriteTable(dbcon, "rep_facts", grouped_repdf, append = TRUE, header = TRUE, 
               row.names = FALSE)
  
}


# Create approriate index to reduce the quering and loading time

createIndexes <- function(){
  
  index_query <- "CREATE INDEX idx_quarter_of_year ON DateTimeDim (quarter_of_year);"
  dbExecute(dbcon, index_query)
  
  index_query <- "CREATE INDEX idx_date ON DateTimeDim (datetimedim_key);"
  dbExecute(dbcon, index_query)
  
  index_query <- "CREATE INDEX idx_productname ON product_facts (productname);"
  dbExecute(dbcon, index_query)
  
  index_query <- "CREATE INDEX idx_repname ON rep_facts (RepName);"
  dbExecute(dbcon, index_query)
  
}





performSampleQueries <- function(){
  
  query <- "
   SELECT  d.quarter_of_year,   SUM(p.total) AS total_sold
    FROM  product_facts p JOIN
    DateTimeDim d ON p.datetimedim_key = d.datetimedim_key
    WHERE   d.year_num = 2020
    GROUP BY  d.quarter_of_year;
"
  # Execute the query and fetch the results
  result <<- dbGetQuery(dbcon, query)
  print(result)
  
  
  query <- "
    SELECT    d.quarter_of_year,
    SUM(p.total) AS total_sold
FROM   product_facts p
JOIN  DateTimeDim d ON p.datetimedim_key = d.datetimedim_key
WHERE  d.year_num = 2020   AND p.productname = 'Alaraphosol'
GROUP BY   d.quarter_of_year;
"
  # Execute the query and fetch the results
  result <- dbGetQuery(dbcon, query)
  print(result)
  
  
  query <- "
  SELECT p.productname,
    SUM(p.total) AS total_sold FROM product_facts p
JOIN  DateTimeDim d ON p.datetimedim_key = d.datetimedim_key
WHERE  d.year_num = 2020
GROUP BY   p.productname
ORDER BY  total_sold DESC
LIMIT 1;
"
  # Execute the query and fetch the results
  result <- dbGetQuery(dbcon, query)
  print(result)
  
  query <- "
 SELECT   r.RepName,
    SUM(r.total) AS total_sold
FROM   rep_facts r JOIN
DateTimeDim d ON r.datetimedim_key = d.datetimedim_key
WHERE d.year_num = 2020
GROUP BY  r.RepName;
"
  # Execute the query and fetch the results
  result <- dbGetQuery(dbcon, query)
  print(result)
  
  
}



# Driver function
main <- function(){
  connectToDB()
  getInitialData()
  performWareHouseCleanup()
  
  
  createStarSchema()
  populateWareHouse()
  createIndexes()
  
  performSampleQueries()
  
}

main()

dbDisconnect(dbcon)



