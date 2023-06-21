# R Script Title : Document database
#
# Description: This script demonstrates the ability of sqldf to perform data 
# operations
#
# Author: Jayaraman Venkatesan
# Date: 2023-06-14
# Term : Summer full 2023

library(sqldf)
library(dplyr)
library(knitr)

# Reading CSV1
df1 <- read.csv('https://s3.us-east-2.amazonaws.com/artificium.us/assignments/80.xml/a-80-305/gen-xml/synthsalestxns-Jan2Mar.csv')

# Reading CSV2
df2 <- read.csv('https://s3.us-east-2.amazonaws.com/artificium.us/assignments/80.xml/a-80-305/gen-xml/synthsalestxns-Sep2Oct.csv')

# Reading CSV3
df3 <- read.csv('https://s3.us-east-2.amazonaws.com/artificium.us/assignments/80.xml/a-80-305/gen-xml/synthsalestxns-Nov2Dec.csv')

# Merging CSVs 
merged_df <- rbind(df1, df2, df3)

# Pre-processing merged CSV to remove $ symbol from amount column
merged_df$amount <- gsub("\\$", "", merged_df$amount)



result <- sqldf("Select restaurant , SUM(amount) , COUNT(*) from merged_df group by restaurant")


# Presenting the result with Kable 
kable(result, align = c("l", "c", "r"), caption = "Restaurant Visits")
