
# ---------------------------------------------------------------------------------------------------------------------------------------------
# I. FUNCTIONS LINK #################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------
cat("loading functions \n")

# For one organisation list all the organisation it is organised with 
# The output is a dataset with the following format org1, org2, nbLink, Org 
# in the output Org1 = Org =  orgName => this is due to historical reason (i.e. it is the format expected by the python script)
# If an organisation has no relation with any other organisation the format is orgName, "", 0, OrgName
func_BuildLinkForOneOrg <- function(dataset, orgName){
  #dataset <- Dataset_H2020Organizations
  #orgName <- "C"

  #list all the projects link to the current organisation 
  listProject <- dataset[dataset$name == orgName, ]$projectReference
  listProject <- unique(listProject)

  #list all the org that have participated to these projects 
  listOrg  <- dataset[dataset$projectReference %in% listProject,]$name
  listOrg <- unique(listOrg)
 
  datasetTmp <- dataset[dataset$name %in% listOrg,]
  datasetTmp <- dataset[dataset$projectReference %in% listProject,]
  datasetTmp <- subset(datasetTmp, select=c(projectReference, name))
  datasetTmp <- datasetTmp[datasetTmp$name != orgName, ]

  if (nrow(datasetTmp) == 0)
  {
    #the organisation has no relation with any other organisation
    output <- data.frame(orgName, "",0, orgName)
    output <- plyr::rename(output,c("orgName" = "Org1",
                                    "X.." = "Org2",
                                    "X0" = "nbLink",
                                    "orgName.1"= "Org"))
  }
  else 
  {
    #compute the number of project per organisation
    datasetTmp <-aggregate(datasetTmp$name, 
                           by=list(datasetTmp$projectReference),
                           FUN=paste, simplify = FALSE)
    datasetTmp <- plyr::rename(datasetTmp,c("Group.1"="Project",
                                            "x" = "Organisations"))
    #create all the pair org1, org2 
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

# build a dataset containing all the interactions for all organisations 
func_BuildLink <- function(dataset) {
  #dataset <- Dataset_H2020Organizations
  #dataset <- head(dataset, n=5000)

  #list all organisation 
  listOrg <- unique(dataset$name)
  data <- list()
  
  # For each of the organisations list with who it is working 
  for (i in 1:length(listOrg)) {
    data[[i]] =func_BuildLinkForOneOrg(dataset, listOrg[i])
  }
  
  #aggregate all 
  tmp <- do.call(rbind, data)
  output <-data.frame(tmp)
  return(output)
}

#for each of the lines of the datasets containing all links between the organisations add additional information (country, activity and number of project)
func_AddAdditionalInformation <- function(DatasetLink, DatasetOrg) {
#  DatasetLink <- Dataset_link
#  DatasetOrg <- Dataset_H2020Organizations

  # select only interesting column 
  DatasetOrg <- subset(DatasetOrg, select=c(name, country, activityType))
  
  # compute nb proj per org
  DatasetNbProjPerOrg <-aggregate(DatasetOrg$name,
                                  by=list(DatasetOrg$name),
                                  FUN=length)
  DatasetNbProjPerOrg<- plyr::rename(DatasetNbProjPerOrg,c("Group.1"="name",
                                                           "x" = "NbProject"))
  DatasetLink <- merge(DatasetLink, DatasetNbProjPerOrg, by.x=c("Org1"), by.y=c("name"), all.x=TRUE)
  DatasetLink <- merge(DatasetLink, DatasetNbProjPerOrg, by.x=c("Org2"), by.y=c("name"), all.x=TRUE)
  DatasetLink <- DatasetLink[order(DatasetLink$Org1, DatasetLink$Org2),]
  
 
  #compute country and type of org
  DatasetOrg <- unique(DatasetOrg)
  DatasetLink <- merge(DatasetLink, DatasetOrg, by.x=c("Org1"), by.y=c("name"), all.x=TRUE, all.y = FALSE)
  DatasetLink <- merge(DatasetLink, DatasetOrg, by.x=c("Org2"), by.y=c("name"), all.x=TRUE, all.y = FALSE)
  DatasetLink <-unique(DatasetLink) 
  
  #rename and reorder columns   
  DatasetLink <- plyr::rename(DatasetLink,c("country.x" = "Country1",
                                            "country.y" = "Country2",
                                            "NbProject.x" = "NbProject1",
                                            "NbProject.y" = "NbProject2",
                                            "activityType.x" = "activityType1",
                                            "activityType.y" = "activityType2"))
  
  DatasetLink <- DatasetLink[c("Org", "Org1", "Org2", "Country1", "Country2", "activityType1", "activityType2", "NbProject1", "NbProject2", "nbLink")]
  DatasetLink <- DatasetLink[order(DatasetLink$Org,DatasetLink$Org1,DatasetLink$Org2),]

  return(DatasetLink)
}


# ---------------------------------------------------------------------------------------------------------------------------------------------
# II. Build Dataset #################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

cat("producing the data \n")
cat("this will take some time \n")
Dataset_link <- func_BuildLink(Dataset_H2020Organizations)
cat("producing additional information \n")
Dataset_link2 <- func_AddAdditionalInformation(Dataset_link, Dataset_H2020Organizations)

cat("exporting the updated data to /Datasets/organisationsNetwork.csv \n")
options(scipen = 10)
write.table(Dataset_link2, "../Datasets/organisationsNetwork.csv", sep = ",", quote = FALSE,row.names = FALSE)
options(scipen = 0)
