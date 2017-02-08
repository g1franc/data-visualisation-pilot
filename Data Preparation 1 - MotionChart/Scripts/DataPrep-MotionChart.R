

# ---------------------------------------------------------------------------------------------------------------------------------------------
# II. LOAD DATASETS #################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

#set current directory
setwd("C:/Users/bruled/Documents/Pwc Project/2 - Project/DataVisualisation/Visualisation/1 - Cordis/data-visualisation-pilot/Data Preparation 1 - MotionChart")

### Load datasets
# FP6: https://data.europa.eu/euodp/data/dataset/cordisfp6projects
# FP7: https://data.europa.eu/euodp/data/dataset/cordisfp7projects
# H2020: https://data.europa.eu/euodp/data/dataset/cordisH2020projects
# Population: http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=demo_pjan&lang=en : demo_pjan_1_Data.csv
# GDP: http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=naida_10_gdp&lang=en : naida_10_gdp_1_Data.csv
# Population by educational attainment level (%) - main indicators (edat_lfse_03): http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=edat_lfse_03&lang=en - edat_lfse_03_1_Data.csv 
# Total intramural R&D expenditure (GERD) by sectors of performance: http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=rd_e_gerdtot&lang=en : rd_e_gerdtot_1_Data.csv

Dataset_FP6Organizations = read.csv("../Input/cordis-fp6organizations.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")
Dataset_FP7Organizations = read.csv("../Input/cordis-fp7organizations.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")
Dataset_H2020Organizations = read.csv("../Input/cordis-h2020organizations.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")

Dataset_FP6Projects = read.csv("../Input/cordis-fp6Projects.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")
Dataset_FP7Projects = read.csv("../Input/cordis-fp7Projects.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")
Dataset_H2020Projects = read.csv("../Input/cordis-h2020Projects.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")
#combine the 3 datasets
Dataset_Projects <- rbind(rbind(Dataset_FP6Projects, Dataset_FP7Projects), Dataset_H2020Projects)


Dataset_population  = read.csv("../Input/demo_pjan_1_Data.csv", header=TRUE, sep=",", stringsAsFactors=FALSE, comment.char="")
Dataset_GDP  = read.csv("../Input/naida_10_gdp_1_Data.csv", header=TRUE, sep=",", stringsAsFactors=FALSE, comment.char="")

Dataset_PopByEducAttain = read.csv("../Input/edat_lfse_03_1_Data.csv", header=TRUE, sep=",", stringsAsFactors=FALSE, comment.char="")
Dataset_RDExpend = read.csv("../Input/rd_e_gerdtot_1_Data.csv", header=TRUE, sep=",", stringsAsFactors=FALSE, comment.char="")



Dataset_Countries = read.csv("../Input/Countries.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")
#select only EU-28 countries 
Dataset_Countries <- Dataset_Countries[Dataset_Countries$EU28 == TRUE,]


# ---------------------------------------------------------------------------------------------------------------------------------------------
# II. data preparation ########################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------


### filter dataset Project 
Dataset_Projects <- merge(Dataset_Projects, Dataset_Countries, by.x=c("coordinatorCountry"), by.y=c("euCode"), all.x=TRUE)
#select only EU-28 countries
Dataset_Projects <- Dataset_Projects[Dataset_Projects$EU28 == TRUE,]
Dataset_Projects <- Dataset_Projects[!is.na(Dataset_Projects$coordinatorCountry),]

#remove useless column 
Dataset_Projects <- subset(Dataset_Projects, select=-c(reference,acronym,status,programme,topics,frameworkProgramme,title,endDate,projectUrl,objective,fundingScheme,coordinator,participants,subjects,
                                                       FP6participantCountries,FP7participantCountries,FP8participantCountries, isoCode,EU28,Schengen))

#clean data + set correct type for columns 
Dataset_Projects$totalCost <- sub(',', '.',Dataset_Projects$totalCost)
Dataset_Projects$totalCost <- as.numeric(Dataset_Projects$totalCost)

Dataset_Projects$startDate <- as.Date(as.POSIXlt(as.character(Dataset_Projects$startDate), format="%Y-%m-%d"))
Dataset_Projects$startDate <- as.numeric(format(Dataset_Projects$startDate, "%Y"))

# remove projects with no start date => ~200 project
Dataset_Projects <- Dataset_Projects[!is.na(Dataset_Projects$startDate),]

# ---------------------------------------------------------------------------------------------------------------------------------------------
# III. compute number Of Project coordinated per country/year #################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
output <-aggregate(Dataset_Projects$coordinatorCountry,
                    by=list(Dataset_Projects$coordinatorCountry, Dataset_Projects$startDate),
                    FUN=length)
# output dataset will store all the results 
output <- plyr::rename(output,c("Group.1"="Country",
                                "Group.2" = "Year",
                                "x" = "numberOfProjectCoordinators"))

# ---------------------------------------------------------------------------------------------------------------------------------------------
# IV. compute total budget managed by coordinator per country/year ############################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

##### compute totalBudgetManagedByCoordinators per country
tmp <- Dataset_Projects[!is.na(Dataset_Projects$totalCost),]
tmp <-aggregate(tmp$totalCost,
                   by=list(tmp$coordinatorCountry, tmp$startDate),
                   FUN=sum)
tmp <- plyr::rename(tmp,c("Group.1"="Country",
                          "Group.2" = "Year",
                          "x" = "totalBudgetManagedAsCoordinators"))

#save the result in output dataset
output <- merge(output, tmp, by.x=c("Country", "Year"), by.y=c("Country", "Year"), all.x=TRUE)
remove(tmp)

remove(Dataset_Projects)
# ---------------------------------------------------------------------------------------------------------------------------------------------
# V. compute the number of project participation (i.e. coordinator or participant role) per country/year ######################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

##### total number of project per country, year, framework contract
func_TotalNbOfProject <- function (datasetOrg, datasetProject) {
  tmp <- merge(datasetOrg, datasetProject, by.x=c("projectReference"), by.y=c("reference"), all.x=TRUE)
  tmp <- merge(tmp, Dataset_Countries, by.x=c("country"), by.y=c("euCode"), all.x=TRUE)
  tmp <- tmp[tmp$EU28 == TRUE,]
  tmp <- tmp[!is.na(tmp$country),]
  tmp$startDate <- as.Date(as.POSIXlt(as.character(tmp$startDate), format="%Y-%m-%d"))
  tmp$startDate <- as.numeric(format(tmp$startDate, "%Y"))
  tmp <- tmp[!is.na(tmp$startDate),]
  tmp <-aggregate(tmp$country, by=list(tmp$country, tmp$startDate), FUN=length)
  return(tmp)
}

tmp1 <- func_TotalNbOfProject(Dataset_FP6Organizations, Dataset_FP6Projects)
tmp2 <- func_TotalNbOfProject(Dataset_FP7Organizations, Dataset_FP7Projects)
tmp3 <- func_TotalNbOfProject(Dataset_H2020Organizations, Dataset_H2020Projects)

tmp<-rbind(rbind(tmp1, tmp2), tmp3)
remove(tmp1)
remove(tmp2)
remove(tmp3)

#group per country, year
tmp <-aggregate(tmp$x, by=list(tmp$Group.1, tmp$Group.2), FUN=sum)
tmp <- plyr::rename(tmp,c("Group.1"="Country",
                          "Group.2" = "Year",
                          "x" = "TotalOfProject"))

#save the result in output dataset
output <- merge(output, tmp, by.x=c("Country", "Year"), by.y=c("Country", "Year"), all.x=TRUE)
remove(tmp)
remove(func_TotalNbOfProject)
# ---------------------------------------------------------------------------------------------------------------------------------------------
# VI. compute the number of project participation (as participant) per country/year ###########################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

output$numberOfProjectParticipants <- output$TotalOfProject - output$numberOfProjectCoordinators


# ---------------------------------------------------------------------------------------------------------------------------------------------
# VI. add the GDP per country/year ############################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

output <- merge(output, Dataset_Countries, by.x=c("Country"), by.y=c("euCode"), all.x=TRUE)
output <- subset(output, select=-c(FP6participantCountries,FP7participantCountries,FP8participantCountries, isoCode,EU28,Schengen))
Dataset_GDP <- Dataset_GDP[Dataset_GDP$NA_ITEM == "Gross domestic product at market prices",]
output <- merge(output, Dataset_GDP, by.x=c("name", "Year"), by.y=c("GEO", "TIME"), all.x=TRUE)

#save the result in output dataset
output <- subset(output, select=-c(UNIT, NA_ITEM, Flag.and.Footnotes))

# ---------------------------------------------------------------------------------------------------------------------------------------------
# VIII. add the population per country/year ###################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
output <- merge(output, Dataset_population, by.x=c("name", "Year"), by.y=c("GEO", "TIME"), all.x=TRUE)
output <- subset(output, select=-c(AGE, SEX,UNIT, Flag.and.Footnotes))

# ---------------------------------------------------------------------------------------------------------------------------------------------
# IX. Population by educational attainment level ##############################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
Dataset_PopByEducAttain <- subset(Dataset_PopByEducAttain, select=-c(SEX, AGE, UNIT))

Dataset_PopByEducAttainED02 <- Dataset_PopByEducAttain[Dataset_PopByEducAttain$ISCED11 == "ED0-2", ]
Dataset_PopByEducAttainED02 <- subset(Dataset_PopByEducAttainED02, select=-c(ISCED11))
Dataset_PopByEducAttainED02 <- plyr::rename(Dataset_PopByEducAttainED02,c("Value" = "% pop. that study up to secondary education"))
                                
Dataset_PopByEducAttainED38 <- Dataset_PopByEducAttain[Dataset_PopByEducAttain$ISCED11 == "ED3-8", ]
Dataset_PopByEducAttainED38 <- subset(Dataset_PopByEducAttainED38, select=-c(ISCED11))
Dataset_PopByEducAttainED38 <- plyr::rename(Dataset_PopByEducAttainED38,c("Value" = "% pop. with a secondary or tertiary education"))

Dataset_PopByEducAttainED34 <- Dataset_PopByEducAttain[Dataset_PopByEducAttain$ISCED11 == "ED3_4", ]
Dataset_PopByEducAttainED34 <- subset(Dataset_PopByEducAttainED34, select=-c(ISCED11))
Dataset_PopByEducAttainED34 <- plyr::rename(Dataset_PopByEducAttainED34,c("Value" = "% pop. with have a secondary education"))

Dataset_PopByEducAttainED58 <- Dataset_PopByEducAttain[Dataset_PopByEducAttain$ISCED11 == "ED5-8", ]
Dataset_PopByEducAttainED58 <- subset(Dataset_PopByEducAttainED58, select=-c(ISCED11))
Dataset_PopByEducAttainED58 <- plyr::rename(Dataset_PopByEducAttainED58,c("Value" = "% pop. with a tertiary education"))


output <- merge(output, Dataset_PopByEducAttainED02, by.x=c("Country", "Year"), by.y=c("GEO", "TIME"), all.x=TRUE)
output <- merge(output, Dataset_PopByEducAttainED38, by.x=c("Country", "Year"), by.y=c("GEO", "TIME"), all.x=TRUE)
output <- merge(output, Dataset_PopByEducAttainED34, by.x=c("Country", "Year"), by.y=c("GEO", "TIME"), all.x=TRUE)
output <- merge(output, Dataset_PopByEducAttainED58, by.x=c("Country", "Year"), by.y=c("GEO", "TIME"), all.x=TRUE)

remove(Dataset_PopByEducAttainED02)
remove(Dataset_PopByEducAttainED38)
remove(Dataset_PopByEducAttainED34)
remove(Dataset_PopByEducAttainED58)
# ---------------------------------------------------------------------------------------------------------------------------------------------
# X. Total intramural R&D expenditure #########################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

Dataset_RDExpend <- subset(Dataset_RDExpend, select=-c(UNIT))
Dataset_RDExpend$Value <- sub(' ', '',Dataset_RDExpend$Value)
Dataset_RDExpend$Value <- as.numeric(Dataset_RDExpend$Value)


Dataset_RDExpendAllSect <- Dataset_RDExpend[Dataset_RDExpend$SECTPERF == "All sectors", ]
Dataset_RDExpendAllSect <- subset(Dataset_RDExpendAllSect, select=-c(SECTPERF))
Dataset_RDExpendAllSect$Value <- Dataset_RDExpendAllSect$Value * 1000000
Dataset_RDExpendAllSect <- plyr::rename(Dataset_RDExpendAllSect,c("Value" = "R&D Budget"))


Dataset_RDExpendBusiness <- Dataset_RDExpend[Dataset_RDExpend$SECTPERF == "Business enterprise sector", ]
Dataset_RDExpendBusiness <- subset(Dataset_RDExpendBusiness, select=-c(SECTPERF))
Dataset_RDExpendBusiness$Value <- Dataset_RDExpendBusiness$Value * 1000000
Dataset_RDExpendBusiness <- plyr::rename(Dataset_RDExpendBusiness,c("Value" = "R&D Budget of enterprise"))

Dataset_RDExpendGov <- Dataset_RDExpend[Dataset_RDExpend$SECTPERF == "Government sector", ]
Dataset_RDExpendGov <- subset(Dataset_RDExpendGov, select=-c(SECTPERF))
Dataset_RDExpendGov$Value <- Dataset_RDExpendGov$Value * 1000000
Dataset_RDExpendGov <- plyr::rename(Dataset_RDExpendGov,c("Value" = "R&D Budget of government"))

Dataset_RDExpendEduc <- Dataset_RDExpend[Dataset_RDExpend$SECTPERF == "Higher education sector", ]
Dataset_RDExpendEduc <- subset(Dataset_RDExpendEduc, select=-c(SECTPERF))
Dataset_RDExpendEduc$Value <- Dataset_RDExpendEduc$Value * 1000000
Dataset_RDExpendEduc <- plyr::rename(Dataset_RDExpendEduc,c("Value" = "R&D Budget of high education"))

Dataset_RDExpendNonProf <- Dataset_RDExpend[Dataset_RDExpend$SECTPERF == "Private non-profit sector", ]
Dataset_RDExpendNonProf <- subset(Dataset_RDExpendNonProf, select=-c(SECTPERF))
Dataset_RDExpendNonProf$Value <- Dataset_RDExpendNonProf$Value * 1000000
Dataset_RDExpendNonProf <- plyr::rename(Dataset_RDExpendNonProf,c("Value" = "R&D Budget of non-profit organisation"))


output <- merge(output, Dataset_RDExpendAllSect, by.x=c("name", "Year"), by.y=c("GEO", "TIME"), all.x=TRUE)
output <- merge(output, Dataset_RDExpendBusiness, by.x=c("name", "Year"), by.y=c("GEO", "TIME"), all.x=TRUE)
output <- merge(output, Dataset_RDExpendGov, by.x=c("name", "Year"), by.y=c("GEO", "TIME"), all.x=TRUE)
output <- merge(output, Dataset_RDExpendEduc, by.x=c("name", "Year"), by.y=c("GEO", "TIME"), all.x=TRUE)
output <- merge(output, Dataset_RDExpendNonProf, by.x=c("name", "Year"), by.y=c("GEO", "TIME"), all.x=TRUE)

remove(Dataset_RDExpendAllSect)
remove(Dataset_RDExpendBusiness)
remove(Dataset_RDExpendGov)
remove(Dataset_RDExpendEduc)
remove(Dataset_RDExpendNonProf)

# ---------------------------------------------------------------------------------------------------------------------------------------------
# XI. compute GDP per capita per country/year #################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
output <- plyr::rename(output,c("Value.x"="GDP", "Value.y" = "Population"))
output$GDP <- sub(',', '',output$GDP)
output$GDP <- sub(',', '',output$GDP)
output$GDP <- sub(',', '',output$GDP)
output$GDP <- as.numeric(output$GDP)
output$GDP <- output$GDP * 1000000

output$Population <- sub(' ', '',output$Population)
output$Population <- sub(' ', '',output$Population)
output$Population <-as.numeric(output$Population)
output$GDPPerCapita <- output$GDP / output$Population

# ---------------------------------------------------------------------------------------------------------------------------------------------
# XII. compute budget managed as coordinators per capita per country/year #####################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
output$totalBudgetManagedAsCoordinatorsPerCapita <- output$totalBudgetManagedAsCoordinators / output$Population


# ---------------------------------------------------------------------------------------------------------------------------------------------
# XIII. compute cumulative information per country ############################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
output$numberOfProjectCoordinatorsCumulative <- ave(output$numberOfProjectCoordinators, output$Country, FUN = cumsum)
output$totalBudgetManagedAsCoordinatorsCumulative <- ave(output$totalBudgetManagedAsCoordinators, output$Country, FUN = cumsum)
output$TotalOfProjectCumulative <- ave(output$TotalOfProject, output$Country, FUN = cumsum)
output$numberOfProjectParticipantsCumulative <- ave(output$numberOfProjectParticipants, output$Country, FUN = cumsum)
output$totalBudgetManagedAsCoordinatorsPerCapitaCumulative <- ave(output$totalBudgetManagedAsCoordinatorsPerCapita, output$Country, FUN = cumsum)


# ---------------------------------------------------------------------------------------------------------------------------------------------
# XIV. save file ##############################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
output <- plyr::rename(output,c("Country" = "CountryShort",
                                "name"="Country", 
                                "numberOfProjectCoordinators" = "Nr. of coordinated projects",
                                "totalBudgetManagedAsCoordinators" = "Budget of coordinated projects",
                                "TotalOfProject" = "Total nr. of projects",
                                "numberOfProjectParticipants" = "Nr. of projects as participant",
                                "GDPPerCapita" = "GDP / capita",
                                "totalBudgetManagedAsCoordinatorsPerCapita" = "Budget of coordinated projects / capita",
                                "numberOfProjectCoordinatorsCumulative" = "Nr. of coordinated projects - Cumulative",
                                "totalBudgetManagedAsCoordinatorsCumulative" = "Budget of coordinated projects - Cumulative",
                                "TotalOfProjectCumulative" = "Total nr. of projects - Cumulative",
                                "numberOfProjectParticipantsCumulative" = "Nr. of projects as participant - Cumulative",
                                "totalBudgetManagedAsCoordinatorsPerCapitaCumulative" = "Budget of coordinated projects / capita - Cumulative"
                                ))

output <- output[output$Year != 2017, ]
output <- output[output$Year != 2018, ]
output$GDP <- format(output$GDP, scientific = FALSE)

options(scipen = 999)
write.table(output, "output/CountryInformation.csv", sep = "\t", quote = FALSE)
options(scipen = 0)

