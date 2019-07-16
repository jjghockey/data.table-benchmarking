#####################################################################################################
#Engagement		-	DataTable Benchmarking															#
#FileName		-	002_an_bmarkDT.r						  										#
#By				- 	Jeremy Guinta 																	#
#																	  								#
#Last Update Date:	6/30/2019									  									#
#																	  								#
#Purpose:		-	Benchmark functions between dplyr and data.table								#
#				-	Take the ~3.5 million ACS data and expand to ~25 million rows					#
#					to simulate a "massive" dataset 												#
#																									#
#				- 	Run on a Windows 64 bit machine with 64 GB of RAM, 8 cores						#
#				-	Running MRAN R 3.4.3															#
#																									#
#####################################################################################################

#I. Setup -------------------------------------------------------------------------------------------

	#Remove Objects
	rm(list=ls())

	#Clear Memory
	gc(reset=TRUE)
	
	#Set Working Directory
	setwd("C:/users/Jguinta/desktop/DataTableBenchMarking/")
	
	#Package Install
	require(lubridate)		#Date/Time functions
	require(reshape2)		#Data Reshape
	require(tidyverse)		#Tidyverse of packages
	require(data.table)		#Data.table functions
	require(dtplyr)			#Data.table plus dplyr
	require(bit64)			#Numeric precision data.table()
		
	#Options
	options(scipen=30)
	
	#Custom Functions  
	
	#benchmark() - Takes a function input and runs it <iter> number of times
	#input: 	iter - number of times to execute
	#			cate - freeform category of the benchmark (e.g., "String Parsing")
	#			type - freeform category of the function (e.g., data.table) - These should be standardized to do comparisons across multiple FUN types
	#			FUN - properly formed R statement
	#output: 	an object with <iter> number of observations that shows
	#			the start and stop time of each iteration of the function
	benchmark<-function(iter=30, cate=c(""), type=c(""), FUN=c("") ) {
		j<-1
		mn<-as.numeric(j)
		mx<-as.numeric(iter)
		pb = txtProgressBar(min = 0, max = as.numeric(mx-mn), initial = 0, style=3) 
		rm(base)
		gc(reset=TRUE)
		while (j <= iter) {	
						srt<-Sys.time()
						eval(parse(text=c(FUN)))
						stp<-Sys.time()
						tm<-data.frame(type=type, cate=cate, iter=j, start=srt, stop=stp, func=FUN)
					if(j==1) {
						base<-tm
					}
					else {
						base<-rbind(base,tm)
					}
				setTxtProgressBar(pb, j)
				j<-j+1
				}
		rm(tm)
		gc(reset=TRUE)
		return(base)
		}

	#sum_func() - Takes the output from benchmark() and creates a univariate summary of the data
	#input:			a dataframe with the fields "type", "start", "stop"
	#output: 		a data.table object with mean, median, min, max, and standard deviation of the difference between start and stop
	sum_func<-function(x) {
		x<-as.data.table(x)
		x[, dif:=stop-start]
		out<-x[, list(cnt=.N, avg=mean(dif), med=median(dif), mn=min(dif), mx=max(dif), stddev=sd(dif)), by=list(type, cate)]		
		return(out)
	}
	
#II. Data Loading -------------------------------------------------------------------------------------------
#A. Load data (See 001 for data creation process)
load("../data/001_acs.rda")

acs<-as.data.table(acs)
acs[, dte:=str_trim(substr(dtetm, 1,regexpr(" ", dtetm)-1)) ]
acs[, tm:=str_trim(substr(dtetm, regexpr(" ", dtetm)+1,255)) ]

acs<-sample_n(acs, 25000000, replace=TRUE)  #Make the data "huge."

acs<-as.data.table(acs)  #We are going to do benchmarks on data.table first
	
#Parameters - Benchmarks are run a minimum of 30 times with the average time of the function processing 

#III. Data Analysis -------------------------------------------------------------------------------------------

#### data.table() benchmarking ####

#The following are the types of commands that my team and I use on a regular basis.
#The purpose of this benchmark is to establish a clean line on when 
#data.table() vs. dplyr should be used when handling excessively "large" data
#The benchmark covers reading and writing processes, string parsing, variable creation, merging and aggregation

#A. Write
	val_write<-benchmark(iter=30, cate=c("Write"), type="data.table", FUN=c('fwrite(file="./tmp.csv", acs)'))
	val_write_tbl<-sum_func(val_write)			
	
#B. Read
	fwrite(file="./tmp.csv", acs) #Create the file
	val_read<-benchmark(iter=30, cate=c("Read"), type="data.table", FUN=c('tmp<-fread("./tmp.csv")'))
	val_read_tbl<-sum_func(val_read)		

#C. Process text dates into R Dates
	#1. Lubridate
	val_date<-benchmark(iter=30, cate=c("Lubridate"), type="data.table", FUN=c('acs[, dtetm2:=mdy_hms(dtetm)]'))
	val_date_tbl<-sum_func(val_date)		
	
	#2. Base R functions, but cast as data.table
	val_date2<-benchmark(iter=30, cate=c("as.POSIXct"), type="data.table", FUN=c('acs[, dtetm2:=as.POSIXct(dtetm, "%m/%d/%Y %H:%M:%S", tz="GMT")]'))
	val_date2_tbl<-sum_func(val_date2)			

	#3. Paste and Combined then convert using lubridate
	val_date3<-benchmark(iter=30, cate=c("Paste + Lubridate"), type="data.table", FUN=c('acs[, dtetm2:=mdy_hms(paste(dte, tm, sep=" "))]'))
	val_date3_tbl<-sum_func(val_date3)		

	#3. Paste and Combined then convert using Base R functions, but cast as data.table
	val_date4<-benchmark(iter=30, cate=c("Paste + as.POSIXct"), type="data.table", FUN=c('acs[, dtetm2:=as.POSIXct(paste(dte, tm, sep=" "),"%m/%d/%Y %H:%M:%S", tz="GMT")]'))
	val_date4_tbl<-sum_func(val_date4)		
		
#D. Process Strings (regular expressions)
	val_string<-benchmark(iter=30, cate=c("String 1"), type="data.table", FUN=c('acs[, first_six:=str_sub(dtetm,1,10)]'))
	val_string_tbl<-sum_func(val_string)		
	

	val_string2<-benchmark(iter=30, cate=c("String 2"), type="data.table"
				, FUN=c('acs[,dte:=str_trim(str_sub(dtetm,1,regexpr(" ", dtetm)-1))] 
						 acs[,tm:=str_trim(str_sub(dtetm,regexpr(" ", dtetm),255))] 
				')
				)
	val_string2_tbl<-sum_func(val_string2)		

	
#E. Merging / Joining
	ST<-fread("./PUMSState.csv")
	ST[, stdescr:=substr(State, 1,regexpr("/", State)-1)]
	ST[, stcd:=substr(State, regexpr("/", State)+1,255)]
	ST[, State:=NULL]
	setnames(ST, c("st", "State", "stab"))
	
	#1. Text Key
	val_join<-benchmark(iter=30, cate=c("Join - Text Key"), type="data.table"
				, FUN=c('
						setkey(acs, stab)
						setkey(ST, stab)
						combo<-acs[ST,nomatch=0]
				'))
	val_join_tbl<-sum_func(val_join)					
	
	#2. Numeric Key
	val_join2<-benchmark(iter=30, cate=c("Join - Numeric Key"), type="data.table"
				, FUN=c('
						setkey(acs, st)
						setkey(ST, st)
						combo<-acs[ST,nomatch=0]
				'))
	val_join2_tbl<-sum_func(val_join2)				
		
	#3. Merge but object is a data.table() object
	val_join3<-benchmark(iter=30, cate=c("Join - Merge - Numeric Key"), type="data.table"
				, FUN=c('
						combo<-merge(acs,ST,by.x=c("st"), by.y=c("st"), all=FALSE)
				'))
	val_join3_tbl<-sum_func(val_join3)	

	#3. Merge but object is a data.table() object
	val_join4<-benchmark(iter=30, cate=c("Join - Merge - Text Key"), type="data.table"
				, FUN=c('
						combo<-merge(acs,ST,by.x=c("stab"), by.y=c("stab"), all=FALSE)
				'))
	val_join4_tbl<-sum_func(val_join4)			

#F. Simple Aggregation
	val_agg<-benchmark(iter=30, cate=c("Simple Aggregation"), type="data.table"
				, FUN=c('
						tbl<-acs[, mean(wagp_infl, na.rm=TRUE), educ]
				'))
	val_agg_tbl<-sum_func(val_agg)
	
	val_agg2<-benchmark(iter=30, cate=c("Complex Aggregation"), type="data.table"
				, FUN=c('
						tbl<-acs[occp_ind!="none", list(val=mean(wagp_infl, na.rm=TRUE)), occp_descr][val<20000,]
				'))
	val_agg2_tbl<-sum_func(val_agg2)
	
	val_agg3<-benchmark(iter=30, cate=c("summarize_at() Aggregation"), type="data.table"
				, FUN=c('
						tbl<-acs[, list(mon=month(mdy(dte)), wagp_infl, wkhp, schl, ind, occp_descr, dte)][order(mon)][,ord:=paste("record_", as.character(mon), sep="" )]
						setkey(tbl, ord)
						one<-tbl[, lapply(.SD, mean, na.rm=TRUE), by=ord, .SDcols=c("wagp_infl", "schl", "wkhp")]
						two<-tbl[, lapply(.SD, sd, na.rm=TRUE), by=ord, .SDcols=c("wagp_infl", "schl", "wkhp")]
						three<-tbl[, lapply(.SD, min, na.rm=TRUE), by=ord, .SDcols=c("wagp_infl", "schl", "wkhp")]
						four<-tbl[, lapply(.SD, max, na.rm=TRUE), by=ord, .SDcols=c("wagp_infl", "schl", "wkhp")]
						setnames(one, c("ord", "wagp_infl_mean", "schl_mean", "wkhp_mean"))
						setnames(two, c("ord", "wagp_infl_sd", "schl_sd", "wkhp_sd"))
						setnames(three, c("ord", "wagp_infl_min", "schl_min", "wkhp_min"))
						setnames(four, c("ord", "wagp_infl_max", "schl_max", "wkhp_max"))
						tbl<-cbind(one,two,three,four)
				'))
	val_agg3_tbl<-sum_func(val_agg3)
	
#IV. Output Summaries
	DT_out<-rbind(val_read, val_write, val_date, val_date2, val_date3, val_date4, val_string, val_string2, val_join, val_join2, val_join3, val_join4, val_agg, val_agg2, val_agg3)

	saveRDS(file="./002_DT.rds", DT_out)
