library(Quandl)

Quandl.api_key("TsHzstzW28XTsmBjJZug")

# Peridical Price Data
price <- Quandl('NSE/OIL', collapse = "daily",
	type = "zoo",
	limit = 1 # number of values not in date format
	)
print(price)

# CALL PRICES: Filtered Dates
dated_data <- Quandl("FRED/GDP",
	start_date="2019-01-01", end_date="2019-01-02")

print(dated_data)

# RETREIVING SPECIFIC COLUMNS
col_spes <- Quandl(c("FRED/GDP.1", "WIKI/AAPL.4"))

print(col_spes)

# PREPROCESSING DATA (returns)
#preproc <- Quandl("FRED/GDP", transform="rdiff")
#print(preproc)

# MULTIPLE TIME-SERIES CALLS
#Quandl(c('EXMPL/DATA1.1', 'EXMPL/DATA2.2'))

# DOWNLOAD TIME-SERIES
#Quandl.database.bulk_download_to_file("ZEA", "./ZEA.zip")

# MF1: Data Retreival
mf1 <- Quandl.datatable('MER/F1', compnumber=c("39102" , "2438"))
print(mf1)

# MF1: Specific Columns
mf1_1 <- Quandl.datatable('MER/F1',qopts.columns=c("compnumber", "ticker"))
print(mf1_1)

# MF1: Report Dates, Indicator and Amount of Columns
mf1_2 <- Quandl.datatable('MER/F1',compnumber="39102",
	qopts.columns=c("reportdate", "indicator", "amount"))
print(mf1_2)

#Retrieve Entire Table Data
mf1_ <- Quandl.datatable('MER/F1', paginate=TRUE)
print(mf1_)

#TRANSFORMATIONS:
# none        no effect   z[t] = y[t]
# diff        row-on-row change   z[t] = y[t] – y[t-1]
# rdiff       row-on-row % change     z[t] = (y[t] – y[t-1]) / y[t-1]
# rdiff_from  latest value as % increment     z[t] = (y[latest] – y[t]) / y[t]
# cumul       cumulative sum  z[t] = y[0] + y[1] + … + y[t]
# normalize   scale series to start at 100    z[t] = y[t] ÷ y[0] * 100


# TIME-SERIES PARAMETERS:
# database_code:  Code identifying the database to which the dataset belongs.

# dataset_code:   Code identifying the dataset.

# limit:          Use limit=n to get the first n rows of the dataset. Use limit=1 to get just the latest row.

# column_index:   Request a specific column. Column 0 is the date column and is always returned. Data begins at column 1.

# start_date:     yyyy-mm-dd  Retrieve data rows on and after the specified start date.

# end_date:		yyyy-mm-dd  Retrieve data rows up to and including the specified end date.

# order:  		ascending/descending  Return data in ascending or descending order of date. Default is desc.

# collapse: 	# daily
		  		# weekly
          		# monthly
         		# quarterly
          		# annual
      			# Change the sampling frequency of the returned data. Default is none, data is returned in its original peeriodicity.

# transform:		Perform pre-calculations on the data prior to downloading. Default is none.



