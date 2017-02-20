# ---------------------------------------------------------------------------------------------------------------------------------------------
# I. LOAD DATASETS #################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

#set current directory
setwd("C:/Users/bruled/Documents/Pwc Project/2 - Project/DataVisualisation/Visualisation/1 - Cordis/data-visualisation-pilot/CORDIS/organisationsNetwork")


#
### Load datasets
# H2020: https://data.europa.eu/euodp/data/dataset/cordisH2020projects

#Dataset_H2020Organizations = read.csv("../Datasets/test.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")

Dataset_H2020Organizations = read.csv("../Datasets/cordis-h2020organizations.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")





# ---------------------------------------------------------------------------------------------------------------------------------------------
# II. FUNCTIONS LINK #################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------




func_BuildLinkForOneOrg <- function(dataset, orgName){
  #dataset <- Dataset_H2020Organizations
  #orgName <- "C"
  
  listProject <- dataset[dataset$name == orgName, ]$projectReference
  listProject <- unique(listProject)
  
  listOrg  <- dataset[dataset$projectReference %in% listProject,]$name
  listOrg <- unique(listOrg)
  
  datasetTmp <- dataset[dataset$name %in% listOrg,]
  datasetTmp <- dataset[dataset$projectReference %in% listProject,]
  
  datasetTmp <- subset(datasetTmp, select=c(projectReference, name))
  datasetTmp <- datasetTmp[datasetTmp$name != orgName, ]
  
  
  
  if (nrow(datasetTmp) == 0)
  {
    output <- data.frame(orgName, "",0, orgName)
    output <- plyr::rename(output,c("orgName" = "Org1",
                                    "X.." = "Org2",
                                    "X0" = "nbLink",
                                    "orgName.1"= "Org"))
  }
  else 
  {
    datasetTmp <-aggregate(datasetTmp$name, 
                           by=list(datasetTmp$projectReference),
                           FUN=paste, simplify = FALSE)
    datasetTmp <- plyr::rename(datasetTmp,c("Group.1"="Project",
                                            "x" = "Organisations"))
    
    data <- list()
    for (i in 1:nrow(datasetTmp)) {
      tmp <- data.frame(orgName, datasetTmp[i,]$Organisations)
      colnames(tmp) <- c("Org1", "Org2")
      data[[i]] = tmp
    }
    
    tmp <- do.call(rbind, data)
    output <-data.frame(tmp)
    output<-aggregate(output$Org1,
                      by=list(output$Org1, output$Org2),
                      FUN=length)
    output <-data.frame(output)
    colnames(output) <- c("Org1", "Org2", "nbLink")
    output$Org <- orgName
  }
  
  return (output) 
}

# build a dataset containing all the interactions between the different countries  
func_BuildLink <- function(dataset) {
  #dataset <- Dataset_H2020Organizations
  #dataset <- head(dataset, n=5000)
  
  
  listOrg <- unique(dataset$name)
  data <- list()
  
  for (i in 1:length(listOrg)) {
    data[[i]] =func_BuildLinkForOneOrg(dataset, listOrg[i])
    print(i)
  }
  tmp <- do.call(rbind, data)
  
  output <-data.frame(tmp)
  return(output)
  
}

func_AddAdditionalInformation <- function(DatasetLink, DatasetOrg) {
#  DatasetLink <- Dataset_link
#  DatasetOrg <- Dataset_H2020Organizations
  
  DatasetOrg <- subset(DatasetOrg, select=c(name, country, activityType))
  # nb proj per org
  DatasetNbProjPerOrg <-aggregate(DatasetOrg$name,
                                  by=list(DatasetOrg$name),
                                  FUN=length)
  DatasetNbProjPerOrg<- plyr::rename(DatasetNbProjPerOrg,c("Group.1"="name",
                                                           "x" = "NbProject"))
  DatasetLink <- merge(DatasetLink, DatasetNbProjPerOrg, by.x=c("Org1"), by.y=c("name"), all.x=TRUE)
  DatasetLink <- merge(DatasetLink, DatasetNbProjPerOrg, by.x=c("Org2"), by.y=c("name"), all.x=TRUE)
  DatasetLink <- DatasetLink[order(DatasetLink$Org1, DatasetLink$Org2),]
  nrow(DatasetLink)
  
 
  #country + type of org
  DatasetOrg <- unique(DatasetOrg)
  DatasetLink <- merge(DatasetLink, DatasetOrg, by.x=c("Org1"), by.y=c("name"), all.x=TRUE, all.y = FALSE)
  DatasetLink <- merge(DatasetLink, DatasetOrg, by.x=c("Org2"), by.y=c("name"), all.x=TRUE, all.y = FALSE)
  DatasetLink <-unique(DatasetLink) 
  
  nrow(DatasetLink)  
 
  DatasetLink <- plyr::rename(DatasetLink,c("country.x" = "Country1",
                                            "country.y" = "Country2",
                                            "NbProject.x" = "NbProject1",
                                            "NbProject.y" = "NbProject2",
                                            "activityType.x" = "activityType1",
                                            "activityType.y" = "activityType2"))
  
  nrow(DatasetLink)
  DatasetLink <- DatasetLink[c("Org", "Org1", "Org2", "Country1", "Country2", "activityType1", "activityType2", "NbProject1", "NbProject2", "nbLink")]
  nrow(DatasetLink)
  DatasetLink <- DatasetLink[order(DatasetLink$Org,DatasetLink$Org1,DatasetLink$Org2),]
  nrow(DatasetLink)

  return(DatasetLink)
}


# ---------------------------------------------------------------------------------------------------------------------------------------------
# V. Build Dataset #################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------


Sys.time()
Dataset_link <- func_BuildLink(Dataset_H2020Organizations)
nrow(Dataset_link)
Sys.time()

# Dataset_link = read.csv("VisualisedData/NbLinkOrgLevel1.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")
# Dataset_link = read.csv("VisualisedData/tmp.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")

Dataset_link2 <- func_AddAdditionalInformation(Dataset_link, Dataset_H2020Organizations)
nrow(Dataset_link2)
Sys.time()


options(scipen = 10)
write.table(Dataset_link2, "VisualisedData/NbLinkOrgLevel1.csv", sep = ";", quote = FALSE,row.names = FALSE)
options(scipen = 0)

#Dataset_ListOrg <- func_ListOrg()
#write.table(Dataset_ListOrg, "VisualisedData/OrgInformation.csv", sep = ";", quote = FALSE)



