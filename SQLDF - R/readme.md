
## CS5200 - Database Management Systems

### Assignment : Demonstrating the Power of SQLDF in R


This assignment focuses on utilizing SQLDF to extract information from dataframes.

### Implementations

1. Input: The data is available in three CSV files containing information on visits to restaurants. The CSV file URLs are as follows:
    1. [January to March](https://s3.us-east-2.amazonaws.com/artificium.us/assignments/80.xml/a-80-305/gen-xml/synthsalestxns-Jan2Mar.csv)

    2.  [September to October](https://s3.us-east-2.amazonaws.com/artificium.us/assignments/80.xml/a-80-305/gen-xml/synthsalestxns-Sep2Oct.csv)

    3. [November to December](https://s3.us-east-2.amazonaws.com/artificium.us/assignments/80.xml/a-80-305/gen-xml/synthsalestxns-Nov2Dec.csv)


2. Merging: The CSV files are merged using the rbind function to create a consolidated dataframe.Merged them using rbind

3. Pre-processing: Perform any necessary pre-processing steps on the merged dataframe, such as cleaning, formatting, or transforming the data as required.

4. Query: Write a SQL query to answer the following question:
"What is the total number of visits and aggregate revenue for each restaurant?"

