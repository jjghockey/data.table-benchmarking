#####################################################################################################
#Engagement		-	DataTable Benchmarking															#
#FileName		-	004_an_summary.r						  										#
#By				- 	Jeremy Guinta 																	#
#																	  								#
#Last Update Date:	6/30/2019									  									#
#																	  								#
#Purpose:		-	Summarize benchmarking from each source (dplyr vs. data.table)					#
#																									#
#####################################################################################################

#I. Setup

	#Remove Objects
	rm(list=ls())

	#Clear Memory
	gc(reset=TRUE)
	
	#Set Working Directory
	setwd("C:/users/Jguinta/desktop/DataTableBenchMarking/")
	
	#Package Install
	require(reshape2)		#Data Reshape
	require(tidyverse)		#Tidyverse of packages
	require(data.table)		#Data.table functions
	require(dtplyr)			#Data.table plus dplyr
	require(bit64)			#Numeric precision data.table()
	
	#Options
	options(scipen=30)
	
	#Custom Functions  

	sum_func<-function(x) {
		x<-as.data.table(x)
		x[, dif:=stop-start]
		
		return(out)
	}	
	
	#Graphic Options
	scheme <- c("#6495ED", "#001933", "#08519c", "#778899", "#B0C4DE", "#999999", "#000000","#C90E17", "#800000", "#B23232","#691b14")                     
	
#II. Data Loading 
	DT<-readRDS("./002_DT.rds")
	DT<-as.data.table(DT)
	DT[,type:="data.table"] #Mistake 

	DP<-readRDS("./003_DP.rds")
	DP<-as.data.table(DP)
	DP[,type:="dplyr"] #Mistake 
	
#III. Data Processing 
	
	#A. Combine the data
	tblDT<-DT[, .(iter, type, cate,dif=stop-start)]
	tblDP<-DP[, .(iter, type, cate,dif=stop-start)]

	tbl<-rbind(tblDT, tblDP)
	
	#B. Classify the operations
	tbl[cate %in% c("Read", "Write"), cls:="Read/Write"]
	tbl[cate %in% c("Lubridate", "as.POSIXct", "Paste + Lubridate", "Paste + as.POSIXct"), cls:="Date Processing"]	
	tbl[cate %in% c("String 1", "String 2"), cls:="String Processing"]
	tbl[cate %in% c("Join - Text Key", "Join - Numeric Key", "Join - Merge - Numeric Key", "Join - Merge - Text Key"), cls:="Joining"]
	tbl[cate %in% c("Simple Aggregation", "Complex Aggregation", "summarize_at() Aggregation"), cls:="Aggregation"]
	
	#C. Make summary table 
	tbl_sum<-tbl[, list(cnt=.N, avg=mean(dif), med=median(dif), mn=min(dif), mx=max(dif), stddev=sd(dif)), by=list(type, cate,cls)]			
	tbl1<-tbl_sum[type=="dplyr", .(avg_DP=avg, med_DP=med, mn_DP=mn, mx_DP=mx, stddev_DP=stddev, cate, cls, type)]
	tbl2<-tbl_sum[type=="data.table", .(avg_DT=avg, med_DT=med, mn_DT=mn, mx_DT=mx, stddev_DT=stddev, cate, cls, type)]

	tbl_sum<-merge(tbl1[,type:=NULL], tbl2[, type:=NULL], by=c("cate", "cls"))
	tbl_sum2<-tbl[, list(cnt=.N, avg=mean(dif), med=median(dif), mn=min(dif), mx=max(dif), stddev=sd(dif)), by=list(type, cate,cls)]			
	
#IV. Analysis
	#A. Average Difference Plot
		p<-ggplot(tbl_sum2[is.na(cate)==FALSE & cls!="Read/Write",], aes(x=cate, y=avg, group=type, color=type))+geom_point(position=position_dodge(width=0.5), size=4)
		p<-p+geom_errorbar(aes(ymin=avg-(1.96*(stddev/sqrt(cnt))), ymax=avg+(1.96*(stddev/sqrt(cnt)))), colour="grey", width=.1,  position=position_dodge(width=0.5))
		p<-p+coord_flip()
		p<-p+theme_bw()
		p<-p+labs(title="Average Difference between data.table() Operations and dplyr() Operations", subtitle="Excluding Read/Write Operations", y="Average Runtime (in Seconds)", x="Operation")
		p<-p+scale_color_manual(values=c(scheme))
		p<-p+theme(panel.grid.major=element_line(color="white"), text=element_text(family="ArialMT"))	
		p<-p+theme(legend.title=element_blank())
		p<-p+theme(legend.position = "bottom")
		p<-p+theme(plot.title = element_text(size = rel(1.5)))
		p<-p+theme(axis.text.y = element_text(size= rel(1.5)))
		p<-p+theme(axis.text.x = element_text(size= rel(1.5)))
		plt1<-p
		
		p<-ggplot(tbl_sum2[is.na(cate)==FALSE & cls=="Read/Write",], aes(x=cate, y=avg, group=type, color=type))+geom_point(size=4)
		p<-p+geom_errorbar(aes(ymin=avg-(1.96*(stddev/sqrt(cnt))), ymax=avg+(1.96*(stddev/sqrt(cnt)))), colour="grey", width=.1)
		p<-p+coord_flip()
		p<-p+theme_bw()
		p<-p+labs(title="Average Difference between data.table() Operations and dplyr() Operations", subtitle="Read/Write Operations", y="Average Runtime (in Seconds)", x="Operation")
		p<-p+scale_color_manual(values=c(scheme))
		p<-p+theme(panel.grid.major=element_line(color="white"), text=element_text(family="ArialMT"))	
		p<-p+theme(legend.title=element_blank())
		p<-p+theme(legend.position = "bottom")
		p<-p+theme(plot.title = element_text(size = rel(1.5)))
		p<-p+theme(axis.text.y = element_text(size= rel(1.5)))
		p<-p+theme(axis.text.x = element_text(size= rel(1.5)))		
		plt2<-p
		
	#B. Dodge Bar Difference
		reord<-as.vector(as.matrix((tbl_sum2[type=="data.table", .(cate, avg)][order(-avg)][, avg:=NULL])))
		tbl_sum2[, cate:=factor(cate, levels=c(reord))]
		p<-ggplot(tbl_sum2[is.na(cate)==FALSE & cls!="Read/Write",], aes(as.factor(cate), avg, group=type, fill=type))+geom_bar(position="dodge", stat="identity")
		p<-p+coord_flip()
		p<-p+theme_bw()
		p<-p+labs(title="Average Difference between data.table() Operations and dplyr() Operations", subtitle="Excluding Read/Write Operations", y="Average Runtime (in Seconds)", x="Operation")
		p<-p+scale_fill_manual(values=c(scheme))
		p<-p+theme(panel.grid.major=element_line(color="white"), text=element_text(family="ArialMT"))	
		p<-p+theme(legend.title=element_blank())
		p<-p+theme(legend.position = "bottom")
		p<-p+theme(plot.title = element_text(size = rel(1.5)))
		p<-p+theme(axis.text.y = element_text(size= rel(1.5)))
		p<-p+theme(axis.text.x = element_text(size= rel(1.5)))
		plt3<-p
		
		reord<-as.vector(as.matrix((tbl_sum2[type=="data.table", .(cate, avg)][order(-avg)][, avg:=NULL])))
		tbl_sum2[, cate:=factor(cate, levels=c(reord))]
		p<-ggplot(tbl_sum2[is.na(cate)==FALSE & cls=="Read/Write",], aes(as.factor(cate), avg, group=type, fill=type))+geom_bar(position="dodge", stat="identity")
		p<-p+coord_flip()
		p<-p+theme_bw()
		p<-p+labs(title="Aveage Difference between data.table() Operations and dplyr() Operations", subtitle="Read/Write Operations", y="Average Runtime (in Seconds)", x="Operation")
		p<-p+scale_fill_manual(values=c(scheme))
		p<-p+theme(panel.grid.major=element_line(color="white"), text=element_text(family="ArialMT"))	
		p<-p+theme(legend.title=element_blank())
		p<-p+theme(legend.position = "bottom")
		p<-p+theme(plot.title = element_text(size = rel(1.5)))
		p<-p+theme(axis.text.y = element_text(size= rel(1.5)))
		p<-p+theme(axis.text.x = element_text(size= rel(1.5)))		
		plt4<-p
		
	#C. Percent Change
		tbl_sum[, pct:=(as.numeric(avg_DP)-as.numeric(avg_DT))/as.numeric(avg_DT)]
		reord<-as.vector(as.matrix((tbl_sum[, .(cate, pct)][order(-pct)][, pct:=NULL])))
		tbl_sum[, cate:=factor(cate, levels=c(reord))]
		
		p<-ggplot(tbl_sum[cls!="Read/Write",], aes(as.factor(cate), pct))+geom_bar(stat="identity", fill=scheme[[1]])
		p<-p+coord_flip()
		p<-p+theme_bw()
		p<-p+labs(title="Percent Difference between data.table() Operations and dplyr() Operations", subtitle="Excluding Read/Write Operations", y="Percent Difference", x="Operation")
		p<-p+scale_y_continuous(labels = scales::percent_format())
		p<-p+theme(panel.grid.major=element_line(color="white"), text=element_text(family="ArialMT"))	
		p<-p+theme(legend.title=element_blank())
		p<-p+theme(legend.position = "none")
		p<-p+theme(plot.title = element_text(size = rel(1.5)))
		p<-p+theme(axis.text.y = element_text(size= rel(1.5)))
		p<-p+theme(axis.text.x = element_text(size= rel(1.5)))
		p<-p+annotate("text", label="data.table() is faster", x=8, y=0.90)
		p<-p+geom_segment(arrow=arrow(), x=8, xend=8, y=1.70, yend=2.10)
		p<-p+geom_segment(x=8.5, xend=8.5, y=0, yend=10.00, color="red")
		plt5<-p
	
#V. Output
	ggsave(file="./004_plt1.png", plt1, height=8, width=11)
	ggsave(file="./004_plt2.png", plt1, height=8, width=11)
	ggsave(file="./004_plt3.png", plt1, height=8, width=11)
	ggsave(file="./004_plt4.png", plt1, height=8, width=11)
	ggsave(file="./004_plt5.png", plt1, height=8, width=11)
	
	fwrite(file="./004_tbl.csv",tbl_sum)
	
