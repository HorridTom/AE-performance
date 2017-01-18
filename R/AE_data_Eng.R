# Version 2

getAEdata_Eng <- function() {
	
	#This function retrieves all the A&E attendance data from NHS England's website
	require(gdata)	

	#Get a list of file urls
	AEurls <- getAEdata_urls()

	#Read the data and return a vector of the outputs from readAEdata
	AElist <- lapply(AEurls, function(x) readAEdata(x))
	AElist <- lapply(AElist, fix2015formats)

	data <- do.call(rbind, AElist)
	
	#Remove spurious rows
	data <- data[which(!data$Name==""),]

	#Remove spurious factor levels
	data <- as.data.frame(lapply(data, function(x) if(is.factor(x)) factor(x) else x))
	
	#Rename columns
	data <- renameAEcols(data)
	
	#Convert data types
	data <- AE.convert.types(data)
	
	data
}

saveAEdata_CW <- function() {
		
	data <- getAEdata_Eng()
	data <- filterAEdata(data,"Chelsea And Westminster")
	write.csv(data,"CWAEdata.csv")

	return()
}

saveAEdata_Eng <- function() {
		
	data <- getAEdata_Eng()
	
	write.csv(data,"AEdata.csv")

	return()
}

filterAEdata <- function (d, search.str = "Chelsea And Westminster") {
	
	#This function filters a data frame with column "Name" by the search string passed
	d[grep(search.str, d$Name), ]
	
}

getAEdata.hosp <- function(search.str = "Chelsea And Westminster") {

	#This function loads all the data and then restricts to
	#those where Name matches the search string passed
	AE.data <- getAEdata_Eng()
	AE.data <- filterAEdata(AE.data,search.str)
	AE.data <- as.data.frame(lapply(AE.data, function(x) if(is.factor(x)) factor(x) else x))
}

AE.convert.types <- function(d) {
	
	#This function converts the data types from factor to more appropriate types
	d$Type.1.attend <- as.numeric(gsub(",","",as.character(d$Type.1.attend)))
	d$Type.2.attend <- as.numeric(gsub(",","",as.character(d$Type.2.attend)))
	d$Type.3.attend <- as.numeric(gsub(",","",as.character(d$Type.3.attend)))
	d$Total.attendances <- as.numeric(gsub(",","",as.character(d$Total.attendances)))
	d$Breach.type.1 <- as.numeric(gsub(",","",as.character(d$Breach.type.1)))
	d$Breach.type.2 <- as.numeric(gsub(",","",as.character(d$Breach.type.2)))
	d$Breach.type.3 <- as.numeric(gsub(",","",as.character(d$Breach.type.3)))
	d$Em.Adm.type.1 <- as.numeric(gsub(",","",as.character(d$Em.Adm.type.1)))
	d$Em.Adm.type.2 <- as.numeric(gsub(",","",as.character(d$Em.Adm.type.2)))
	d$Em.Adm.type.3.4 <- as.numeric(gsub(",","",as.character(d$Em.Adm.type.3.4)))
	d$Em.Adm.non.AE <- as.numeric(gsub(",","",as.character(d$Em.Adm.non.AE)))
	d$Dec.to.adm.4.to.12 <- as.numeric(gsub(",","",as.character(d$Dec.to.adm.4.to.12)))
	d$Dec.to.adm.12 <- as.numeric(gsub(",","",as.character(d$Dec.to.adm.12)))
	
	d$Prop.4h.type.1 <- as.numeric(gsub("%","",as.character(d$Prop.4h.type.1)))/100
	d$Prop.4h.all <- as.numeric(gsub("%","",as.character(d$Prop.4h.all)))/100
	
	d
}

renameAEcols <- function (data) {

	#This function renames the columns from the text labels in the original files to
	#shorter more manageable names.

	colnames(data)[match("Type.1.Departments...Major.A.E",colnames(data))] <- "Type.1.attend"
	colnames(data)[match("Type.2.Departments...Single.Specialty",colnames(data))] <- "Type.2.attend"
	colnames(data)[match("Type.3.Departments...Other.A.E.Minor.Injury.Unit",colnames(data))] <- "Type.3.attend"
	colnames(data)[match("Type.1.Departments...Major.A.E.1",colnames(data))] <- "Breach.type.1"
	colnames(data)[match("Type.2.Departments...Single.Specialty.1",colnames(data))] <- "Breach.type.2"
	colnames(data)[match("Type.3.Departments...Other.A.E.Minor.Injury.Unit.1",colnames(data))] <- "Breach.type.3"
	colnames(data)[match("Percentage.in.4.hours.or.less..type.1.",colnames(data))] <- "Prop.4h.type.1"
	colnames(data)[match("Percentage.in.4.hours.or.less..all.",colnames(data))] <- "Prop.4h.all"
	colnames(data)[match("Emergency.Admissions.via.Type.1.A.E",colnames(data))] <- "Em.Adm.type.1"
	colnames(data)[match("Emergency.Admissions.via.Type.2.A.E",colnames(data))] <- "Em.Adm.type.2"
	colnames(data)[match("Emergency.Admissions.via.Type.3.and.4.A.E",colnames(data))] <- "Em.Adm.type.3.4"
	colnames(data)[match("Other.Emergency.admissions..i.e.not.via.A.E.",colnames(data))] <- "Em.Adm.non.AE"
	colnames(data)[match("Number.of.patients.spending..4.hours.but..12.hours.from.decision.to.admit.to.admission",colnames(data))] <- "Dec.to.adm.4.to.12"
	colnames(data)[match("Number.of.patients.spending..12.hours.from.decision.to.admit.to.admission",colnames(data))] <- "Dec.to.adm.12"
	
	return(data)

}


readAEdata <- function (file_url) {

	#This function reads a file of A&E data from the NHS England website and outputs it as a dataframe

	#First load the gdata package that enables reading of Excel files
	#require(gdata)

	#file_url is the url of the Excel file in question
	#file_url <- "http://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/04/2014.03.30-AE-66Eue.xls"

	#Download the file
	Y <- read.xls(file_url)

	#If the first column is redundant, remove it
	firstcol <- Y[,1]
	if(all(is.na(firstcol))) {
		Y[,1] <- NULL	
	}
	firstcol <- Y[,1]
	if(all(is.na(firstcol))) {
		Y[,1] <- NULL	
	}



	#Find the cell specifying the period and extract the text
	Period_row <- Y[ which(Y$Title.=="Period:"), ]
	Period_cell <- Period_row[[2]]
	Period_char <- as.character(Period_cell)

	#Convert the period text to a week ending Date
	#First extract a list with one element, a vector. The vector has three elements.
	Period_char_split <- strsplit(Period_char," ")

	#TODO: test the first two elements of the vector that is the only element of this list
	#	to ensure they are "Week" and "Ending" respectively, throw a warning if not.

	#The third element of this vector is the date of the week ending, as a character string
	Week_ending_str <- Period_char_split[[1]][3]
	Week_ending_Date <- as.Date(Week_ending_str, "%d/%m/%Y")
	
	#If the last column is redundant, remove it
	lastcol <- Y[,ncol(Y)]
	if(all(is.na(lastcol))) {
		Y[,ncol(Y)] <- NULL	
	}

	#Lift the row containing column descriptions and use as dataframe labels
	rowofnames <- Y[apply(Y,1,function(x){"Code" %in% x}),]
	if (nrow(rowofnames)==1) {
			rn <- attr(rowofnames,"row.names")
		}
		else {
			rn <- as.integer(11)
		}
	#This is the offending line...
	#sheetcolnames <- as.character(as.vector(unlist(Y[rn,])))
	#I think this should work?
	sheetcolnames <- sapply(Y[rn,],as.character)

	#Check for empty column name
	emptycol <- match("",sheetcolnames)
	if (!is.na(emptycol)) {sheetcolnames[emptycol] <- "BLANK"}

		
	#Rename columns
	colnames(Y) <- sheetcolnames
	
	#Remove empty column if present
	if (!is.na(match("BLANK",colnames(Y)))) {Y$BLANK <-NULL}

	#Remove header and footer rows
	lastrow <- Y[apply(Y,1,function(x){"Notes:" %in% x}),]
	if (nrow(lastrow)==1) {
			lrn <- attr(lastrow,"row.names") - 1
		}
		else {
			lrn <- nrow(Y)
		}
	frn <- rn + 1

	Y <- Y[frn:lrn,]

	#Add a column every entry of which is the week end Date for this data
	Y$wk.ending <- Week_ending_Date

	groupnum <- match("SHA",colnames(Y))
	if(!is.na(groupnum)) {
			Y$sha.flag <- TRUE
			colnames(Y)[groupnum] <- "Area Team"
	}
	else {
			Y$sha.flag <- FALSE
	}
	
	return(Y)
}

getAEdata_urls <- function () {
	
	#This function returns the urls for NHS England A&E data *.xls files from five pages
	#yielding addresses for weekly data from November 2010 to (in principle) March 2015

	baseurl <- "http://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/weekly-ae-sitreps-"
	start_year <- 2010:2015
	end_year <- 11:16
	join <- rep("-",6)
	end <- rep("/",6)
	index_urls <- paste0(baseurl,start_year,join,end_year,end)
	index_urls[[6]] <- "http://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/statistical-work-areasae-waiting-times-and-activityweekly-ae-sitreps-2015-16/"

	#return(index_urls)
	unlist(lapply(index_urls,function(x) getAEdata_page_urls(x)))
	
}


getAEdata_page_urls <- function (index_url) {

	#This function returns the urls for NHS England A&E data *.xls files from one index page
	#E.g. 2014 index_url = "http://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/weekly-ae-sitreps-2014-15/"	

	#Get the html from the index website
	con <- url(index_url, "r")
	html_lines <- readLines(con)
	
	#Close connection
	close(con)

	#Look for lines that contain the signature part of the url and the signature text
	data_url_lines <- grep(".xls",html_lines)
	xlsdata_url_lines <- grep("E Week Ending",html_lines[data_url_lines])
	NHSE_xlsdata_lines <- html_lines[data_url_lines][xlsdata_url_lines]
	
	#Extract urls from html lines
	starts <- regexpr("http",NHSE_xlsdata_lines)
	ends <- regexpr(".xls",NHSE_xlsdata_lines) + 3
	urls <- substr(NHSE_xlsdata_lines, starts, ends)

	#Return urls
	return(urls)

}

fix2015formats <- function(df) {

	colnames(df)[colnames(df)=="Total Attendances"] <- "Total attendances"
	
	#if (ncol(df) == 23) {

		
		drops <- c("Total Attendances > 4 hours","Total Emergency Admissions via A&E","Total Emergency Admissions")
		df <- df[,!(colnames(df) %in% drops)]
	
		colnames(df)[colnames(df)=="Type 1 Departments - Major A&E.1"] <- "Type 1 Departments - Major A&E"
		colnames(df)[colnames(df)=="Type 2 Departments - Single Specialty.1"] <- "Type 2 Departments - Single Specialty"
		colnames(df)[colnames(df)=="Type 3 Departments - Other A&E/Minor Injury Unit.1"] <- "Type 3 Departments - Other A&E/Minor Injury Unit"
		colnames(df)[colnames(df)=="Other Emergency Admissions (i.e not via A&E)"] <- "Other Emergency admissions (i.e not via A&E)"
		colnames(df)[colnames(df)=="Number of patients spending >4 hours from decision to admit to admission"] <- "Number of patients spending >4 hours but <12 hours from decision to admit to admission"

	#}

	if(!("Area Team" %in% colnames(df))) { df[,"Area Team"] <- "-" }

	df <- df[,!sapply(df,function(x){all(is.na(x))})]

	df

}

lag.column <- function(df,c1,nm = "lag.col",backlag = FALSE,sc = NULL) {

	if (backlag == FALSE) {
		df[nm] <- df[c1]
		df[[nm]][2:nrow(df)] <- df[[c1]][1:nrow(df)-1]
		is.na(df[nm])[1] <- TRUE
	} else {
		df[nm] <- df[c1]
		df[[nm]][1:nrow(df)-1] <- df[[c1]][2:nrow(df)]
		is.na(df[nm])[nrow(df)] <- TRUE
	}
	if (!is.null(sc)){
		if(backlag==FALSE){
			df[df[[sc]]==TRUE,][[nm]] <- NA
		} else {
			sc2 <- df[sc]
			sc2[1:nrow(df)-1,] <- df[[sc]][2:nrow(df)]
			sc2[nrow(df),] <- TRUE
			df[sc2==TRUE,][[nm]] <- NA
		}
	
	}
	df

}

test.structure <- function(df1, df2) {
	#This function takes two dataframes as arguments and
	#tests to see whether the column names and data types
	#are identical

	name.id <- identical(names(df1),names(df2))
	type.id <- identical(sapply(df1,typeof),sapply(df2,typeof))
	
	name.id & type.id

}



