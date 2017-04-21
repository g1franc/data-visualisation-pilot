require(data.table)

# ---------------------------------------------------------------------------------------------------------------------------------------------
# I. LOAD DATASETS #################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

#set current directory
setwd("C:/Users/bruled/Documents/Pwc Project/2 - Project/DataVisualisation/Visualisation/data-visualisation-pilot/CORDIS/DataPrep")

#
### Load datasets
# H2020: https://data.europa.eu/euodp/data/dataset/cordisH2020projects

Dataset_H2020Organizations = read.csv("../Datasets/inputData/cordis-h2020organizations.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")

Dataset_H2020Organizations <- subset(Dataset_H2020Organizations, select=c(projectID, name, activityType, country))

#selectCountry <- list("DE")
#Dataset_H2020Organizations  <- Dataset_H2020Organizations[Dataset_H2020Organizations$country %in% selectCountry,]

# ---------------------------------------------------------------------------------------------------------------------------------------------
# II. FUNCTIONS LINK #################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

func_BuildLinkForOneOrg <- function(dataset, orgName){
  #dataset <- Dataset_H2020Organizations
  #orgName <- "- 18 DEGREES"
  
  #list all the projects link to the current organisation 
  listProject <- dataset[dataset$name == orgName, ]$projectID
  listProject <- unique(listProject)
  
  #list all the org that have participated to these projects 
  listOrg  <- dataset[dataset$projectID %in% listProject,]$name
  listOrg <- unique(listOrg)
  
  datasetTmp <- dataset[dataset$name %in% listOrg,]
  datasetTmp <- dataset[dataset$projectID %in% listProject,]
  datasetTmp <- subset(datasetTmp, select=c(projectID, name))
  datasetTmp <- datasetTmp[datasetTmp$name != orgName, ]
  
  if (nrow(datasetTmp) == 0)
  {
    output <- data.frame(Source = as.character(), Target = as.character(), Type = as.character())
  }
  else 
  {
    #compute the number of project per organisation
    datasetTmp <-aggregate(datasetTmp$name, 
                           by=list(datasetTmp$projectID),
                           FUN=paste, simplify = FALSE)
    datasetTmp <- plyr::rename(datasetTmp,c("Group.1"="Project",
                                            "x" = "Organisations"))
    #create all the pair org1, org2 
    data <- list()
    for (i in 1:nrow(datasetTmp)) {
      tmp <- data.frame(orgName, datasetTmp[i,]$Organisations)
      colnames(tmp) <- c("Source", "Target")
      data[[i]] = tmp
    }
    
    tmp <- do.call(rbind, data)
    output <-data.frame(tmp)
    output<-aggregate(output$Source,
                      by=list(output$Source, output$Target),
                      FUN=length)
    output <-data.frame(output)
    output$Type <- "Undirected"
    colnames(output) <- c("Source", "Target", "Weight", "Type")
    output$Org <- orgName
  }
  
  return (output) 
}

func_BuildLink <- function(dataset) {
  #dataset <- Dataset_H2020Organizations
  #dataset <- head(dataset, n=5000)
  
  #list all organisation 
  #listOrg <- c("- 18 DEGREES", "INSTITUT D'AERONOMIE SPATIALE DE BELGIQUE")
  #i<-1
  listOrg <- unique(dataset$name)

  # For each of the organisations list with who it is working 
  for (i in 1:length(listOrg)) {
    dataOrg =func_BuildLinkForOneOrg(dataset, listOrg[i])
    
    fileName <- listOrg[i]
    fileName = gsub(' ','_', fileName)
    fileName = gsub('\\*','', fileName)
    fileName = gsub('/','', fileName)
    fileName = gsub('\'','', fileName)
    fileName = gsub('\"','', fileName)
    fileName = gsub(',','', fileName)
    fileName = gsub('&','', fileName)
    fileName = gsub(':','', fileName)
    
    name <- paste("c:/input/", fileName, "_edge.csv", sep = "")
    options(scipen = 10)
    write.table(dataOrg, name, sep = ";", quote = FALSE, row.names = FALSE)
    options(scipen = 0)
    

    ListOrgWithLink <- c(as.character(dataOrg$Source), as.character(dataOrg$Target))
    ListOrgWithLink <- unique(ListOrgWithLink)
    if (length(ListOrgWithLink) == 0){
      ListOrgWithLink <- c(listOrg[i])
    }
      
    
    listNode <- subset(dataset, select=c(name, activityType))
    listNode  <- listNode[listNode$name %in% ListOrgWithLink,]
    data_t = data.table(listNode)
    
    setkeyv(data_t, c('name', 'activityType'))
    listNode = data_t[,Weight := .N, by = c("name", "activityType")]
    listNode <- unique(data.frame(listNode))    
    
    listNode$Id <- as.character(listNode$name)
    listNode$ActivityType <- as.character(listNode$activityType)
    listNode$NbProject <- as.character(listNode$Weight)
    listNode <- subset(listNode, select=c(Id, ActivityType, NbProject))
    

    name <- paste("c:/input/", fileName, "_node.csv", sep = "")
    write.table(listNode, name, sep = ";", quote = FALSE, row.names = FALSE)
    remove(listNode)
    
    if (i%%1000 == 0){
      print(paste(i, "on", length(listOrg), ":", Sys.time()))
    }
  }
  
  return ()
}



# ---------------------------------------------------------------------------------------------------------------------------------------------
# V. Build Dataset #################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
Sys.time()
func_BuildLink(Dataset_H2020Organizations)
beep()
Sys.time()
