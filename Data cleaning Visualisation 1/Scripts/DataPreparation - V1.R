# ---------------------------------------------------------------------------------------------------------------------------------------------
# I. GLOBAL VARIABLE #################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

otherCountry <- "Others"
frameworkContract <- c("FP6", "FP7", "H2020")
# ---------------------------------------------------------------------------------------------------------------------------------------------
# II. LOAD DATASETS #################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

#set current directory
setwd("C:/Users/vandeloc/Documents/Documents/08 Cordis/Data cleaning Visualisation 1")


### Load datasets
# FP6: https://data.europa.eu/euodp/data/dataset/cordisfp6projects
# FP7: https://data.europa.eu/euodp/data/dataset/cordisfp7projects
# H2020: https://data.europa.eu/euodp/data/dataset/cordisH2020projects
# Population: http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=demo_pjan&lang=en
# GDP: http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=naida_10_gdp&lang=en
Dataset_FP6Organizations = read.csv("Input/cordis-fp6organizations.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")

Dataset_FP7Organizations = read.csv("Input/cordis-fp7organizations.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")

Dataset_H2020Organizations = read.csv("Input/cordis-h2020organizations.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")

Dataset_Population = read.csv("Input/demo_pjan_Data.csv", header=TRUE, sep=",", stringsAsFactors=FALSE, comment.char="", na.strings = ":")

Dataset_GDP = read.csv("Input/naida_10_gdp_Data.csv", header=TRUE, sep=",", stringsAsFactors=FALSE, comment.char="", na.strings = ":")

Dataset_Countries = read.csv("Input/Countries.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")

listCountries <- Dataset_Countries[Dataset_Countries$Selected == TRUE, ]
listCountries <-rbind(listCountries, "otherCountry")
#Dataset_FP6Organizations <- head(Dataset_FP6Organizations, n=100)
#Dataset_FP7Organizations <- head(Dataset_FP7Organizations, n=100)
#Dataset_H2020Organizations <- head(Dataset_H2020Organizations, n=100)
# ---------------------------------------------------------------------------------------------------------------------------------------------
# III. FUNCTIONS #################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

#rename function for columns when selecting the column using a variable containing its name
func_rename <- function(dat, oldnames, newnames) {
  datnames <- colnames(dat)
  datnames[which(datnames %in% oldnames)] <- newnames
  colnames(dat) <- datnames
  dat
}


#check if a country is part of "europe", if true return euCode; if not return otherCountry
func_ComputeCountryName <- function(countryName, codeName){
  if(!is.element(countryName, codeName)) {
    countryName <- otherCountry
  }
  else {
    euCode <- listCountries[(codeName == countryName), ]$euCode
    countryName <- euCode
  }
  return(countryName)
}


#set country attribute of dataset to euCode using the ComputeCountryName
func_setCountry <- function(frameworkContract, dataset) {
  for(i in 1:nrow(dataset)) {
    if (frameworkContract == "FP6") {
      countryShort <- func_ComputeCountryName(dataset[i,]$country, listCountries$FP6participantCountries)
    } else if (frameworkContract == "FP7") {
      countryShort <- func_ComputeCountryName(dataset[i,]$country, listCountries$FP7participantCountries)
    } else if (frameworkContract == "H2020") {
      countryShort <- func_ComputeCountryName(dataset[i,]$country, listCountries$FP8participantCountries)
    } else if (frameworkContract == "GDP") {
      countryShort <- func_ComputeCountryName(dataset[i,]$country, listCountries$GDP)
    }else if (frameworkContract == "population") {
      countryShort <- func_ComputeCountryName(dataset[i,]$country, listCountries$population)
    } else {}
    dataset[i,]$country <- countryShort
  }
  return (dataset$country)
}

#Aggregate all other countries after setting the country attribute to the euCode and calculate
func_aggregateOtherCountry <- function(dataset, attributeName) {
  dataset <- rbind(dataset, list("otherCountry", 0))
  for (i in 1:nrow(dataset)) {
    if (dataset[i,]$country == otherCountry) {
      dataset[dataset$country == "otherCountry", attributeName] <- dataset[dataset$country == "otherCountry", attributeName] +
                                                                    dataset[i, attributeName]
    }
  }
  dataset <- dataset[!(dataset$country == otherCountry),]
}

#merge aggregated dataset for selected attribute with overal dataset
func_mergeDatasets <- function (outputDataset, selectedSet, attributeName) {
  if (any(colnames(outputDataset) == attributeName)) {
    outputDataset <- merge(outputDataset, selectedSet, by.x=c("countryShort", "frameworkContract"), by.y=c("country", "frameworkContract"), all.x=TRUE)
    attributeName.x <- gsub(" ","",paste(attributeName,".x"))
    attributeName.y <- gsub(" ","",paste(attributeName, ".y"))
    outputDataset[,colnames(outputDataset) == attributeName.x][is.na(outputDataset[,colnames(outputDataset) == attributeName.x])] <- 
      outputDataset[,colnames(outputDataset) == attributeName.y][is.na(outputDataset[,colnames(outputDataset) == attributeName.x])]
    outputDataset <- outputDataset[,!colnames(outputDataset) == attributeName.y]
    outputDataset <- func_rename(outputDataset, attributeName.x, attributeName)
  } else {
    outputDataset <- merge(outputDataset, selectedSet, by.x=c("countryShort", "frameworkContract"), by.y=c("country", "frameworkContract"), all.x=TRUE)
  }
  return (outputDataset)
}


#calculate number of projects in database for the selected value
func_calculateNbProjectSelectedValue <- function(frameworkContract, dataset, aggregateAttribute, selectionName, attributeName, outputDataset) {
  aggregatedSet <- aggregate(dataset$country,
                             by=list(dataset$country,
                                     aggregateAttribute),
                             FUN=length)
  aggregatedSet <- plyr::rename(aggregatedSet, c("Group.1"="country", "Group.2"="aggregate", "x"="nbProject" ))
  
  for (j in 1:length(selectionName)) {
    aggregatedSet <- aggregatedSet[(aggregatedSet$aggregate == selectionName[j]), ]
  }
  
  aggregatedSet <- aggregatedSet[ ,!colnames(aggregatedSet)=="aggregate"]
  
  if (length(aggregatedSet$country) != 0) {
    aggregatedSet$country <- func_setCountry(frameworkContract, aggregatedSet)
    aggregatedSet <- func_aggregateOtherCountry(aggregatedSet, "nbProject")
    aggregatedSet <- plyr::rename(aggregatedSet, c("country"="country", "nbProject"=attributeName))
    aggregatedSet$frameworkContract <- frameworkContract
    outputDataset <- func_mergeDatasets(outputDataset, aggregatedSet, attributeName)
  }
    
  return (outputDataset)
}

#Calculate the number of projects with role participant or coordinator
func_calculateNbProjectParticipantCoordinator <- function (outputDataset) {
  nbProjectParticipant <- outputDataset$nbProjectParticipant
  nbProjectCoordinator <- outputDataset$nbProjectCoordinator
  nbProjectParticipant[is.na(nbProjectParticipant)] <- 0
  nbProjectCoordinator[is.na(nbProjectCoordinator)] <- 0
  outputDataset$nbProjectParticipantCoordinator <- nbProjectParticipant + nbProjectCoordinator
  
  return (outputDataset$nbProjectParticipantCoordinator)
}


#calculate the number of institutions per country for a given dataset
func_calculateNbInstitution <- function(frameworkContract, dataset, outputDataset) {
  aggregatedSet <- aggregate(dataset$country,
                             by=list(dataset$country,
                                     dataset$name),
                             FUN=length)
  aggregatedSet <- plyr::rename(aggregatedSet, c("Group.1"="country", "Group.2"="institution", "x"="freq" ))
  aggregatedSet <- sort(table(aggregatedSet$country), decreasing = TRUE)
  aggregatedSet <- data.frame(aggregatedSet)
  aggregatedSet <- plyr::rename(aggregatedSet, c("Var1" = "country", "Freq"="nbInstitution"))
  
  if (length(aggregatedSet$country) != 0) {
    aggregatedSet$country <- as.character(aggregatedSet$country)
    aggregatedSet$country <- func_setCountry(frameworkContract, aggregatedSet)
    aggregatedSet <- func_aggregateOtherCountry(aggregatedSet, "nbInstitution")
    aggregatedSet$frameworkContract <- frameworkContract
    outputDataset <- func_mergeDatasets(outputDataset, aggregatedSet, "nbInstitution")
  }
  
  return (outputDataset)
}

#Calculate GDP or population per country
func_calculateGDPorPopulation <- function(frameworkContract, dataset, attributeName, outputDataset) {
  dataset[is.na(dataset)] <- "0"
  dataset$Value <- gsub(" ","",paste(dataset$Value))
  dataset$Value <- as.numeric(dataset$Value)
  dataset <- dataset[,colnames(dataset) %in% c("GEO", "Value"), drop=FALSE]
  dataset <- plyr::rename(dataset, c("GEO"="country", "Value"=attributeName))
  dataset$country <- func_setCountry(attributeName, dataset)
  dataset <- func_aggregateOtherCountry(dataset, attributeName)
  dataset$frameworkContract <- frameworkContract
  outputDataset <- func_mergeDatasets(outputDataset, dataset, attributeName)
}


#Calculate GDP per capita
func_calculateGDPperCapita <- function (outputDataset) {
  GDP <- outputDataset$GDP
  population <- outputDataset$population
  GDP[is.na(GDP)] <- 0
  population[is.na(population)] <- 0
  
  outputDataset$GDPperCapita <- GDP/population
  
  return (outputDataset$GDPperCapita)
}


#build a dataset that contains all following combinations 
#framework contract |country short |country long
func_BuildDataset_NbProjPerCountry <- function() {
  sizeLinkDataset <-length(listCountries$euCode) * length(frameworkContract)
  Dataset_NbProjPerCountry <- data.frame(frameworkContract= rep("",sizeLinkDataset),
                                         countryShort =  rep("",sizeLinkDataset),
                                         countryLong =  rep("",sizeLinkDataset)
  )
  Dataset_NbProjPerCountry$frameworkContract <- as.character(Dataset_NbProjPerCountry$frameworkContract)
  Dataset_NbProjPerCountry$countryShort <- as.character(Dataset_NbProjPerCountry$countryShort)
  Dataset_NbProjPerCountry$countryLong <- as.character(Dataset_NbProjPerCountry$countryLong)
  
  currentRow <- 1
  for (i in frameworkContract)
  {
    frameworkContract <- i
    for(j in 1:length(listCountries$euCode) ) {
      countryShort <- listCountries$euCode[j]
      countryLong <- listCountries$name[j]
      Dataset_NbProjPerCountry[currentRow,]$frameworkContract <- frameworkContract
      Dataset_NbProjPerCountry[currentRow,]$countryShort <- countryShort
      Dataset_NbProjPerCountry[currentRow,]$countryLong <- countryLong
      currentRow <- currentRow + 1
    }
  }
  return(Dataset_NbProjPerCountry)
}



#Calculate all variables
func_calculateVariables <- function(frameworkContract, dataset, outputDataset) {
  
  outputDataset <- func_calculateNbProjectSelectedValue(frameworkContract, dataset, dataset$role, "coordinator", "nbProjectCoordinator", outputDataset)
  outputDataset <- func_calculateNbProjectSelectedValue(frameworkContract, dataset, dataset$role, "participant", "nbProjectParticipant", outputDataset)
  outputDataset$nbProjectParticipantCoordinator <- func_calculateNbProjectParticipantCoordinator(outputDataset)
  outputDataset <- func_calculateNbProjectSelectedValue(frameworkContract, dataset, dataset$activityType, "HES", "nbProjectHES", outputDataset)
  outputDataset <- func_calculateNbProjectSelectedValue(frameworkContract, dataset, dataset$activityType, "REC", "nbProjectREC", outputDataset)
  outputDataset <- func_calculateNbProjectSelectedValue(frameworkContract, dataset, dataset$activityType, "PRC", "nbProjectPRC", outputDataset)
  outputDataset <- func_calculateNbProjectSelectedValue(frameworkContract, dataset, dataset$activityType, "PUB", "nbProjectPUB", outputDataset)
  outputDataset <- func_calculateNbProjectSelectedValue(frameworkContract, dataset, dataset$activityType, "OTH", "nbProjectOTH", outputDataset)
  
  outputDataset <- func_calculateNbInstitution(frameworkContract, dataset, outputDataset)
  
  outputDataset <- func_calculateGDPorPopulation(frameworkContract, Dataset_GDP, "GDP", outputDataset)
  outputDataset <- func_calculateGDPorPopulation(frameworkContract, Dataset_Population, "population", outputDataset)
  outputDataset$GDPperCapita <- func_calculateGDPperCapita(outputDataset)
  
  return (outputDataset)
}

# ---------------------------------------------------------------------------------------------------------------------------------------------
# IV. Build Dataset #################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
Dataset_NbProjPerCountry <- func_BuildDataset_NbProjPerCountry()
Dataset_NbProjPerCountry <- func_calculateVariables("FP6", Dataset_FP6Organizations, Dataset_NbProjPerCountry)
Dataset_NbProjPerCountry <- func_calculateVariables("FP7", Dataset_FP7Organizations, Dataset_NbProjPerCountry)
Dataset_NbProjPerCountry <- func_calculateVariables("H2020", Dataset_H2020Organizations, Dataset_NbProjPerCountry)
Dataset_NbProjPerCountry[is.na(Dataset_NbProjPerCountry)] <- 0

write.table(Dataset_NbProjPerCountry, "Output/NbProjPerCountry.txt")
