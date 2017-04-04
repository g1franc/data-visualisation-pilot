# ---------------------------------------------------------------------------------------------------------------------------------------------
# I. data preparation ########################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
cat("Data preparation \n")

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
# II. compute number Of Project coordinated per country/year #################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
cat("compute number Of Project coordinated per country/year \n")

output <-aggregate(Dataset_Projects$coordinatorCountry,
                    by=list(Dataset_Projects$coordinatorCountry, Dataset_Projects$startDate),
                    FUN=length)
# output dataset will store all the results 
output <- plyr::rename(output,c("Group.1"="Country",
                                "Group.2" = "Year",
                                "x" = "numberOfProjectCoordinators"))

# ---------------------------------------------------------------------------------------------------------------------------------------------
# III. compute total budget managed by coordinator per country/year ############################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
cat("compute total budget managed by coordinator per country/year \n")

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
# IV. compute the number of project participation (i.e. coordinator or participant role) per country/year ######################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
cat("compute the number of project participation (i.e. coordinator or participant role) per country/year \n")

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

#group per country, year
tmp <-aggregate(tmp$x, by=list(tmp$Group.1, tmp$Group.2), FUN=sum)
tmp <- plyr::rename(tmp,c("Group.1"="Country",
                          "Group.2" = "Year",
                          "x" = "TotalOfProject"))

#save the result in output dataset
output <- merge(output, tmp, by.x=c("Country", "Year"), by.y=c("Country", "Year"), all.x=TRUE)

remove(tmp1)
remove(tmp2)
remove(tmp3)
remove(tmp)
remove(func_TotalNbOfProject)

# ---------------------------------------------------------------------------------------------------------------------------------------------
# V. compute the number of project participation (as participant) per country/year ###########################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
cat("compute the number of project participation (as participant) per country/year \n")

output$numberOfProjectParticipants <- output$TotalOfProject - output$numberOfProjectCoordinators

# ---------------------------------------------------------------------------------------------------------------------------------------------
# VI. add the GDP per country/year ############################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
cat("add the GDP per country/year \n")

output <- merge(output, Dataset_Countries, by.x=c("Country"), by.y=c("euCode"), all.x=TRUE)
output <- subset(output, select=-c(FP6participantCountries,FP7participantCountries,FP8participantCountries, isoCode,EU28,Schengen))
Dataset_GDP <- Dataset_GDP[Dataset_GDP$na_item == "B1GQ",]
Dataset_GDP <- Dataset_GDP[Dataset_GDP$unit == "CP_MUSD",]
Dataset_GDP <- subset(Dataset_GDP, select=-c(unit, na_item))
Dataset_GDP <- plyr::rename(Dataset_GDP,c("values"="GDP"))

#save the result in output dataset
output <- merge(output, Dataset_GDP, by.x=c("Country", "Year"), by.y=c("geo", "time"), all.x=TRUE)
remove(Dataset_GDP)

# ---------------------------------------------------------------------------------------------------------------------------------------------
# VII. add the population per country/year ###################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
cat("add the population per country/year \n")

Dataset_population <- Dataset_population[Dataset_population$age == "TOTAL",]
Dataset_population <- Dataset_population[Dataset_population$sex == "T",]
Dataset_population <- subset(Dataset_population, select=-c(age, sex, unit))
Dataset_population <- plyr::rename(Dataset_population,c("values"="Population"))

#save the result in output dataset
output <- merge(output, Dataset_population, by.x=c("Country", "Year"), by.y=c("geo", "time"), all.x=TRUE)
remove(Dataset_population)

# ---------------------------------------------------------------------------------------------------------------------------------------------
# VIII. add the population by educational attainment level ##############################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
cat("add the population by educational attainment level \n")

Dataset_PopByEducAttain <- Dataset_PopByEducAttain[Dataset_PopByEducAttain$age =="Y15-64",]
Dataset_PopByEducAttain <- Dataset_PopByEducAttain[Dataset_PopByEducAttain$sex =="T",]
Dataset_PopByEducAttain <- subset(Dataset_PopByEducAttain, select=-c(sex, age, unit))

#Dataset_PopByEducAttainED02 <- Dataset_PopByEducAttain[Dataset_PopByEducAttain$ISCED11 == "ED0-2", ]
#Dataset_PopByEducAttainED02 <- subset(Dataset_PopByEducAttainED02, select=-c(ISCED11))
#Dataset_PopByEducAttainED02 <- plyr::rename(Dataset_PopByEducAttainED02,c("Value" = "% pop. that study up to secondary education"))

#Dataset_PopByEducAttainED38 <- Dataset_PopByEducAttain[Dataset_PopByEducAttain$ISCED11 == "ED3-8", ]
#Dataset_PopByEducAttainED38 <- subset(Dataset_PopByEducAttainED38, select=-c(ISCED11))
#Dataset_PopByEducAttainED38 <- plyr::rename(Dataset_PopByEducAttainED38,c("Value" = "% pop. with a secondary or tertiary education"))

Dataset_PopByEducAttainED34 <- Dataset_PopByEducAttain[Dataset_PopByEducAttain$isced11 == "ED3_4", ]
Dataset_PopByEducAttainED34 <- subset(Dataset_PopByEducAttainED34, select=-c(isced11))
Dataset_PopByEducAttainED34 <- plyr::rename(Dataset_PopByEducAttainED34,c("values" = "% pop. with a secondary education"))

Dataset_PopByEducAttainED58 <- Dataset_PopByEducAttain[Dataset_PopByEducAttain$isced11 == "ED5-8", ]
Dataset_PopByEducAttainED58 <- subset(Dataset_PopByEducAttainED58, select=-c(isced11))
Dataset_PopByEducAttainED58 <- plyr::rename(Dataset_PopByEducAttainED58,c("values" = "% pop. with a tertiary education"))

#output <- merge(output, Dataset_PopByEducAttainED02, by.x=c("Country", "Year"), by.y=c("geo", "time"), all.x=TRUE)
#output <- merge(output, Dataset_PopByEducAttainED38, by.x=c("Country", "Year"), by.y=c("geo", "time"), all.x=TRUE)
output <- merge(output, Dataset_PopByEducAttainED34, by.x=c("Country", "Year"), by.y=c("geo", "time"), all.x=TRUE)
output <- merge(output, Dataset_PopByEducAttainED58, by.x=c("Country", "Year"), by.y=c("geo", "time"), all.x=TRUE)

#remove(Dataset_PopByEducAttainED02)
#remove(Dataset_PopByEducAttainED38)
remove(Dataset_PopByEducAttainED34)
remove(Dataset_PopByEducAttainED58)
remove(Dataset_PopByEducAttain)

# ---------------------------------------------------------------------------------------------------------------------------------------------
# IX. add the total intramural R&D expenditure #########################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
cat("add the total intramural R&D expenditure \n")

Dataset_RDExpend <- Dataset_RDExpend[Dataset_RDExpend$unit =="MIO_EUR",]
Dataset_RDExpend <- subset(Dataset_RDExpend, select=-c(unit))
Dataset_RDExpend$values <- sub(' ', '',Dataset_RDExpend$values)
Dataset_RDExpend$values <- as.numeric(Dataset_RDExpend$values)

Dataset_RDExpendAllSect <- Dataset_RDExpend[Dataset_RDExpend$sectperf == "TOTAL", ]
Dataset_RDExpendAllSect <- subset(Dataset_RDExpendAllSect, select=-c(sectperf))
Dataset_RDExpendAllSect$values <- Dataset_RDExpendAllSect$values * 1000000
Dataset_RDExpendAllSect <- plyr::rename(Dataset_RDExpendAllSect,c("values" = "National R&D Budget"))

#Dataset_RDExpendBusiness <- Dataset_RDExpend[Dataset_RDExpend$SECTPERF == "Business enterprise sector", ]
#Dataset_RDExpendBusiness <- subset(Dataset_RDExpendBusiness, select=-c(SECTPERF))
#Dataset_RDExpendBusiness$values <- Dataset_RDExpendBusiness$values * 1000000
#Dataset_RDExpendBusiness <- plyr::rename(Dataset_RDExpendBusiness,c("values" = "R&D Budget of enterprise"))

#Dataset_RDExpendGov <- Dataset_RDExpend[Dataset_RDExpend$SECTPERF == "Government sector", ]
#Dataset_RDExpendGov <- subset(Dataset_RDExpendGov, select=-c(SECTPERF))
#Dataset_RDExpendGov$values <- Dataset_RDExpendGov$values * 1000000
#Dataset_RDExpendGov <- plyr::rename(Dataset_RDExpendGov,c("values" = "R&D Budget of government"))

#Dataset_RDExpendEduc <- Dataset_RDExpend[Dataset_RDExpend$SECTPERF == "Higher education sector", ]
#Dataset_RDExpendEduc <- subset(Dataset_RDExpendEduc, select=-c(SECTPERF))
#Dataset_RDExpendEduc$values <- Dataset_RDExpendEduc$values * 1000000
#Dataset_RDExpendEduc <- plyr::rename(Dataset_RDExpendEduc,c("values" = "R&D Budget of high education"))

#Dataset_RDExpendNonProf <- Dataset_RDExpend[Dataset_RDExpend$SECTPERF == "Private non-profit sector", ]
#Dataset_RDExpendNonProf <- subset(Dataset_RDExpendNonProf, select=-c(SECTPERF))
#Dataset_RDExpendNonProf$values <- Dataset_RDExpendNonProf$values * 1000000
#Dataset_RDExpendNonProf <- plyr::rename(Dataset_RDExpendNonProf,c("values" = "R&D Budget of non-profit organisation"))


output <- merge(output, Dataset_RDExpendAllSect, by.x=c("Country", "Year"), by.y=c("geo", "time"), all.x=TRUE)
#output <- merge(output, Dataset_RDExpendBusiness, by.x=c("Country", "Year"), by.y=c("GEO", "TIME"), all.x=TRUE)
#output <- merge(output, Dataset_RDExpendGov, by.x=c("Country", "Year"), by.y=c("GEO", "TIME"), all.x=TRUE)
#output <- merge(output, Dataset_RDExpendEduc, by.x=c("Country", "Year"), by.y=c("GEO", "TIME"), all.x=TRUE)
#output <- merge(output, Dataset_RDExpendNonProf, by.x=c("Country", "Year"), by.y=c("GEO", "TIME"), all.x=TRUE)

remove(Dataset_RDExpendAllSect)
#remove(Dataset_RDExpendBusiness)
#remove(Dataset_RDExpendGov)
#remove(Dataset_RDExpendEduc)
#remove(Dataset_RDExpendNonProf)
remove(Dataset_RDExpend)

# ---------------------------------------------------------------------------------------------------------------------------------------------
# X. compute GDP per capita per country/year #################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
cat("compute GDP per capita per country/year \n")

output$GDP <- sub(',', '',output$GDP)
output$GDP <- as.numeric(output$GDP)
output$GDP <- output$GDP * 1000000

output$Population <- sub(' ', '',output$Population)
output$Population <-as.numeric(output$Population)
output$GDPPerCapita <- output$GDP / output$Population

# ---------------------------------------------------------------------------------------------------------------------------------------------
# XI. compute budget managed as coordinators per capita per country/year #####################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
cat("compute budget managed as coordinators per capita per country/year \n")

output$totalBudgetManagedAsCoordinatorsPerCapita <- output$totalBudgetManagedAsCoordinators / output$Population

# ---------------------------------------------------------------------------------------------------------------------------------------------
# XII. compute cumulative information per country ############################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
cat("compute cumulative information per country \n")

output$numberOfProjectCoordinatorsCumulative <- ave(output$numberOfProjectCoordinators, output$Country, FUN = cumsum)
output$totalBudgetManagedAsCoordinatorsCumulative <- ave(output$totalBudgetManagedAsCoordinators, output$Country, FUN = cumsum)
output$TotalOfProjectCumulative <- ave(output$TotalOfProject, output$Country, FUN = cumsum)
output$numberOfProjectParticipantsCumulative <- ave(output$numberOfProjectParticipants, output$Country, FUN = cumsum)
output$totalBudgetManagedAsCoordinatorsPerCapitaCumulative <- ave(output$totalBudgetManagedAsCoordinatorsPerCapita, output$Country, FUN = cumsum)

# ---------------------------------------------------------------------------------------------------------------------------------------------
# XIII. save file ##############################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
cat("exporting the updated data to /Datasets/Motionchart.csv \n")

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

output <- output[output$Year <= arg_4, ]
output$GDP <- format(output$GDP, scientific = FALSE)

options(scipen = 999)
write.table(output, "../Datasets/Motionchart.csv", sep = ",", quote = FALSE, row.names = FALSE)
options(scipen = 0)