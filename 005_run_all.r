	#Set Working Directory
	setwd("C:/users/Jguinta/desktop/DataTableBenchMarking/")
	
	source("./002_an_bmarkDT.r")
	gc(reset=TRUE)
	rm(list=ls())
	source("./003_an_bmarkDP.r")
	gc(reset=TRUE)
	rm(list=ls())
	source("004_an_summary.r")
	
	