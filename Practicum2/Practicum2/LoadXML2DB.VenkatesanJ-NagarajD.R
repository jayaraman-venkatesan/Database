# Load the required package
library(RSQLite)
library(XML)
library(dplyr)


con <- NULL;
salesrep_file <- "pharmaReps.xml"


products_df <- NULL
customers_df <- NULL
combined_df <- NULL

attributes_df <- NULL

createDB <- function(){
  
  
  # Set the file path for the SQLite database (you can change the filename and path)
  db_file <- "my_database.db"
  
  # Connect to the database or create one if it doesn't exist
  con <<- dbConnect(RSQLite::SQLite(), dbname = db_file)
  
}

createTable <- function(query){
  if(is.null(con)){
    cat("connection to db doesnt exisits ! ")
    return()
  }
  
  dbExecute(con , statement = query)
  
  
}

createPracticumTables <- function(){
  
  
  createTable("CREATE TABLE IF NOT EXISTS products (
      pID INTEGER PRIMARY KEY,
      prod TEXT
    )"
  )
  
  
  createTable(
    "CREATE TABLE IF NOT EXISTS reps (
      rID INTEGER PRIMARY KEY,
      firstName TEXT,
      lastName TEXT,
      territory TEXT
    )
    "
  )
  
  createTable(
    "CREATE TABLE IF NOT EXISTS customers (
      cID INTEGER PRIMARY KEY,
      cust TEXT,
      country TEXT
    )
    "
  )
  
  createTable(
    "CREATE TABLE IF NOT EXISTS salestxn (
      txnID INTEGER PRIMARY KEY,
      date DATE,
      cID INTEGER,
      pID INTEGER,
      amount NUMBER,
      qty INTEGER,
      repID INTEGER,
      FOREIGN KEY(cID) REFERENCES customers(cID), 
      FOREIGN KEY(pID) REFERENCES products(pID),   
      FOREIGN KEY(repID) REFERENCES reps(rID)
    )
    "
  )
  
}


populateSalesRep <- function(){
  filename <- "txn-xml/pharmaReps.xml"
  xmlDoc <- xmlParse(filename,validate = TRUE)
  df <- xmlToDataFrame(xmlDoc, stringsAsFactors = FALSE)
  
  xml_elements <- xmlChildren(xmlRoot(xmlDoc))
  
  # Extract the attributes and convert to a data frame
  attributes_df <<- sapply(xml_elements, xmlAttrs)
  
  
  
  reps_df <<- cbind(df, attributes_df)
  
  colnames(reps_df)[colnames(reps_df) == "attributes_df"] <<- "rID"
  
  
  reps_df$rID <<- gsub("^r", "", reps_df$rID)
  
  dbWriteTable(con,"reps",reps_df,append=T,row.names=FALSE)
  
  
  
  
  
}


populateProducts <- function(){
  
  if(! is.null(combined_df)){
    products_df <<- unique(data.frame(prod = combined_df$prod))
    
    products_df$pID <<- 1:nrow(products_df)
    
    dbWriteTable(con,"products",products_df,append=T,row.names=FALSE)
    
  }
  
  
}

populateCustomers <- function(){
  
  if(!is.null(combined_df)){
    
    customers_df <<- unique(data.frame(cust = combined_df$cust,country=combined_df$country))
    
    customers_df$cID <<- 1:nrow(customers_df)
    
    dbWriteTable(con,"customers",customers_df,append=T,row.names=FALSE)
    
    
    
  }
  
  
}


populateSalesTxn <- function(){
  
  combined_df$date <<- as.Date(combined_df$date, format = "%m/%d/%Y")
  
  combined_df$date <<- format(combined_df$date, format = "%m-%d-%Y")
  
  merged_df <- merge(combined_df, products_df, by = "prod", all.x = TRUE)
  
  
  
  merged_df <- merge(merged_df, customers_df, by = "cust", all.x = TRUE)
  
  sales_txn_df <<- data.frame(pID=merged_df$pID , 
                              cID = merged_df$cID, 
                              qty = merged_df$qty,
                              amount = merged_df$amount,
                              date = merged_df$date,
                              repID = merged_df$repID
  )
  
  
  
  dbWriteTable(con,"salestxn",sales_txn_df,append=T,row.names=FALSE)
  
  
}


doCombineDF <- function (files){
  
  df_list <- list()
  
  # Loop through the files
  for (f in files) {
    xml_doc <- xmlParse(paste0("txn-xml/", f),validate = TRUE)
    df <- xmlToDataFrame(xml_doc)
    
    # Append each data frame to the list
    df_list[[length(df_list) + 1]] <- df
  }
  
  # Combine all the data frames in the list into a single data frame
  if (length(df_list) > 0) {
    combined_df <<- do.call(rbind, df_list)
  } else {
    # If the list is empty, handle it as needed (e.g., print a message or set combined_df to NULL)
    combined_df <<- NULL
  }
  
  
}



load <- function(){
  files <- list.files(path="txn-xml")
  
  # load sales rep info if exists
  if(!salesrep_file %in% files){
    cat("cannot find sales representatives file. Exiting")
    return()
  } else {
    populateSalesRep();
  }
  
  
  #iterate through files
  files <- files[!files %in% salesrep_file]
  
  doCombineDF(files)
  
  populateProducts()
  populateCustomers()
  populateSalesTxn()
  
}


main <- function(){
  createDB();
  createPracticumTables();
  load();
  
}

main()

products_df 
customers_df 
combined_df 
nrow(sales_txn_df)
attributes_df 
# Close the database connection
dbDisconnect(con)
