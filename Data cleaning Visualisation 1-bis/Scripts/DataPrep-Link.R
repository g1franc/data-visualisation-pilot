
library(plyr)
library(gtools)
library(data.table)
# ---------------------------------------------------------------------------------------------------------------------------------------------
# I. GLOBAL VARIABLE #################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

otherCountry <- "Others"
frameworkContract <- c("FP6", "FP7", "H2020")
# ---------------------------------------------------------------------------------------------------------------------------------------------
# II. LOAD DATASETS #################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

#set current directory
setwd("C:/Users/bruled/Documents/Pwc Project/2 - Project/DataVisualisation/Visualisation/1 - Cordis/3/data-visualisation-pilot/Data cleaning Visualisation 1-bis")

### Load datasets
# FP6: https://data.europa.eu/euodp/data/dataset/cordisfp6projects
# FP7: https://data.europa.eu/euodp/data/dataset/cordisfp7projects
# H2020: https://data.europa.eu/euodp/data/dataset/cordisH2020projects
# Population: http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=demo_pjan&lang=en
# GDP: http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=naida_10_gdp&lang=en
Dataset_FP6Organizations = read.csv("Input/cordis-fp6organizations.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")
Dataset_FP7Organizations = read.csv("Input/cordis-fp7organizations.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")
Dataset_H2020Organizations = read.csv("Input/cordis-h2020organizations.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")


Dataset_FP6Projects = read.csv("Input/cordis-fp6Projects.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")
Dataset_FP7Projects = read.csv("Input/cordis-fp7Projects.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")
Dataset_H2020Projects = read.csv("Input/cordis-h2020Projects.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")

Dataset_Projects <- rbind(rbind(Dataset_FP6Projects, Dataset_FP7Projects), Dataset_H2020Projects)



Dataset_Countries = read.csv("Input/Countries.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")
Dataset_Countries <- Dataset_Countries[Dataset_Countries$EU28 == TRUE,]

Dataset_population  = read.csv("Input/demo_pjan_1_Data.csv", header=TRUE, sep=",", stringsAsFactors=FALSE, comment.char="")
Dataset_GDP  = read.csv("Input/naida_10_gdp_1_Data.csv", header=TRUE, sep=",", stringsAsFactors=FALSE, comment.char="")



# ---------------------------------------------------------------------------------------------------------------------------------------------
# III. FUNCTIONS ADDITIONAL INFORMATION #################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------


#filter dataset Project 
Dataset_Projects <- merge(Dataset_Projects, Dataset_Countries, by.x=c("coordinatorCountry"), by.y=c("euCode"), all.x=TRUE)
Dataset_Projects <- Dataset_Projects[Dataset_Projects$EU28 == TRUE,]

Dataset_Projects <- Dataset_Projects[!is.na(Dataset_Projects$coordinatorCountry),]

Dataset_Projects <- subset(Dataset_Projects, select=-c(reference,acronym,status,programme,topics,frameworkProgramme,title,endDate,projectUrl,objective,fundingScheme,coordinator,participants,subjects,
                                                       FP6participantCountries,FP7participantCountries,FP8participantCountries, isoCode,EU28,Schengen))

Dataset_Projects$totalCost <- sub(',', '.',Dataset_Projects$totalCost)


Dataset_Projects$totalCost <- as.numeric(Dataset_Projects$totalCost)


#Dataset_Projects$totalCost <- as.numeric(Dataset_Projects$totalCost)
Dataset_Projects$startDate <- as.Date(as.POSIXlt(as.character(Dataset_Projects$startDate), format="%Y-%m-%d"))

Dataset_Projects$startDate <- as.numeric(format(Dataset_Projects$startDate, "%Y"))
Dataset_Projects <- Dataset_Projects[!is.na(Dataset_Projects$startDate),]


##### compute numberOfProjectCoordinators per country
output <-aggregate(Dataset_Projects$coordinatorCountry,
                    by=list(Dataset_Projects$coordinatorCountry, Dataset_Projects$startDate),
                    FUN=length)
output <- plyr::rename(output,c("Group.1"="Country",
                                "Group.2" = "Year",
                                "x" = "numberOfProjectCoordinators"))

##### compute totalBudgetManagedByCoordinators per country
tmp <- Dataset_Projects[!is.na(Dataset_Projects$totalCost),]
tmp <-aggregate(tmp$totalCost,
                   by=list(tmp$coordinatorCountry, tmp$startDate),
                   FUN=sum)
tmp <- plyr::rename(tmp,c("Group.1"="Country",
                          "Group.2" = "Year",
                          "x" = "totalBudgetManagedAsCoordinators"))
output <- merge(output, tmp, by.x=c("Country", "Year"), by.y=c("Country", "Year"), all.x=TRUE)


##### total number of project 
tmp <- merge(Dataset_FP6Organizations, Dataset_FP6Projects, by.x=c("projectReference"), by.y=c("reference"), all.x=TRUE)
tmp <- merge(tmp, Dataset_Countries, by.x=c("country"), by.y=c("euCode"), all.x=TRUE)
tmp <- tmp[tmp$EU28 == TRUE,]
tmp <- tmp[!is.na(tmp$country),]
tmp$startDate <- as.Date(as.POSIXlt(as.character(tmp$startDate), format="%Y-%m-%d"))
tmp$startDate <- as.numeric(format(tmp$startDate, "%Y"))
tmp <- tmp[!is.na(tmp$startDate),]
tmp1 <-aggregate(tmp$country, by=list(tmp$country, tmp$startDate), FUN=length)

tmp <- merge(Dataset_FP7Organizations, Dataset_FP7Projects, by.x=c("projectReference"), by.y=c("reference"), all.x=TRUE)
tmp <- merge(tmp, Dataset_Countries, by.x=c("country"), by.y=c("euCode"), all.x=TRUE)
tmp <- tmp[tmp$EU28 == TRUE,]
tmp <- tmp[!is.na(tmp$country),]
tmp$startDate <- as.Date(as.POSIXlt(as.character(tmp$startDate), format="%Y-%m-%d"))
tmp$startDate <- as.numeric(format(tmp$startDate, "%Y"))
tmp <- tmp[!is.na(tmp$startDate),]
tmp2 <-aggregate(tmp$country, by=list(tmp$country, tmp$startDate), FUN=length)

tmp <- merge(Dataset_H2020Organizations, Dataset_H2020Projects, by.x=c("projectReference"), by.y=c("reference"), all.x=TRUE)
tmp <- merge(tmp, Dataset_Countries, by.x=c("country"), by.y=c("euCode"), all.x=TRUE)
tmp <- tmp[tmp$EU28 == TRUE,]
tmp <- tmp[!is.na(tmp$country),]
tmp$startDate <- as.Date(as.POSIXlt(as.character(tmp$startDate), format="%Y-%m-%d"))
tmp$startDate <- as.numeric(format(tmp$startDate, "%Y"))
tmp <- tmp[!is.na(tmp$startDate),]
tmp3 <-aggregate(tmp$country, by=list(tmp$country, tmp$startDate), FUN=length)

tmp<-rbind(rbind(tmp1, tmp2), tmp3)
remove(tmp1)
remove(tmp2)
remove(tmp3)

tmp <-aggregate(tmp$x, by=list(tmp$Group.1, tmp$Group.2), FUN=sum)
tmp <- plyr::rename(tmp,c("Group.1"="Country",
                          "Group.2" = "Year",
                          "x" = "TotalOfProject"))
output <- merge(output, tmp, by.x=c("Country", "Year"), by.y=c("Country", "Year"), all.x=TRUE)

##### compute numberOfProjectParticipation per country
output$numberOfProjectParticipants <- output$TotalOfProject - output$numberOfProjectCoordinators


##### compute GDP per country
output <- merge(output, Dataset_Countries, by.x=c("Country"), by.y=c("euCode"), all.x=TRUE)
output <- subset(output, select=-c(FP6participantCountries,FP7participantCountries,FP8participantCountries, isoCode,EU28,Schengen))
Dataset_GDP <- Dataset_GDP[Dataset_GDP$NA_ITEM == "Gross domestic product at market prices",]
output <- merge(output, Dataset_GDP, by.x=c("name", "Year"), by.y=c("GEO", "TIME"), all.x=TRUE)

output <- subset(output, select=-c(UNIT, NA_ITEM, Flag.and.Footnotes))
                   
##### compute population per country
output <- merge(output, Dataset_population, by.x=c("name", "Year"), by.y=c("GEO", "TIME"), all.x=TRUE)
output <- subset(output, select=-c(AGE, SEX,UNIT, Flag.and.Footnotes))

##### compute per capita
output <- plyr::rename(output,c("Value.x"="GDP", "Value.y" = "Population"))

output$GDP <- sub(',', '',output$GDP)
output$GDP <- sub(',', '',output$GDP)
output$GDP <- sub(',', '',output$GDP)
output$GDP <- as.numeric(output$GDP)
output$Population <- sub(' ', '',output$Population)
output$Population <- sub(' ', '',output$Population)
output$Population <-as.numeric(output$Population)
output$GDPPerCapita <- output$GDP / output$Population * 1000000

output$totalBudgetManagedAsCoordinatorsPerCapita <- output$totalBudgetManagedAsCoordinators / output$Population

##### save file 
output$Year <- paste(output$Year,"/01/01", sep='')

options(scipen = 4)
write.table(output, "output/CountryInformation.csv", sep = "\t", quote = FALSE)
options(scipen = 0)

