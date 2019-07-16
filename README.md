# data.table-benchmarking
A benchmarking test between data.table functions and equivalent dplyr functions. 

## 001_mk_data.r - 
  Captures data from the American Community Survey (https://www.census.gov/programs-surveys/acs/).  This data is not being    analyzed.  It is used as dummy data to test the efficiency of data.table functions versus dplyr functions.  This process creates a new file called 001_acs.rds.  This file is too large for github.  Thus it is not saved. 

## 002_an_bmarkDT.r, 003_an_bmarkDP.r - 
  Executes a series of common data processing, manipulation, and aggegration commands.  The following commands are executed: 

1. Write operations

2. Read operations

3. Data Conversion (using lubridate, as.POSIXct)

4. String Regex manipulation

5. Joins

6. Aggregation


Each series of commands is performed 30 times each on the ACS data that has been expanded to be approximately 6 GB in storage space.   The expanded ACS data had approximately 25 Million rows of data.  The Write operations create a new file called tmp.csv (which is read in during the Read operations).  The tmp.csv file is not saved, as it is too large for github. 

All benchmarking was done in sequence starting with data.table first and then dplyr second.  Sessions were run on a server running Microsoft R (MRAN), with 8 cores and 64 GB of RAM.  Sessions were run during minimal server load in which only the benchmarking commands were being performed. 

## 004_an_summary.r - 
  Summarizes the comparison between data.table and dplyr for each of the command types executed. 

## 005_run_all.r - 
  source() executes each r script (001, 002, 003, and 004)
