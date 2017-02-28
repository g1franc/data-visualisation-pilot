#install.packages("tm")
#install.packages("RJSONIO")
#install.packages("plyr")
require(XML)
require(tm)
require(RJSONIO)
require(plyr)

# ---------------------------------------------------------------------------------------------------------------------------------------------
# I. LOAD DATA ################################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

getwd()
setwd("C:/Users/vandeloc/Documents/Documents/Cordis/GitHub/TransparencyRegister/Data Preparation")
setwd("C:/Users/bruled/Documents/Pwc Project/2 - Project/DataVisualisation/Visualisation/1 - Cordis/data-visualisation-pilot/TransparencyRegister/Map")

xmlfile <-  xmlTreeParse("../Datasets/full_export_new.xml", useInternalNodes = TRUE)


# ---------------------------------------------------------------------------------------------------------------------------------------------
# II. TRANSFORM DATA FROM XML TO DATA.FRAME ###################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

func_parseXMLNodeInterestRepresentative <- function(xmlNode){
  # xmlNode <- rootNode[["resultList"]][i][[1]]
  country <- xmlValue(xmlNode[["contactDetails"]][["country"]])
  cat <- xmlValue(xmlNode[["category"]][["mainCategory"]])
  subCat <- xmlValue(xmlNode[["category"]][["subCategory"]])
  goal <-   xmlValue(xmlNode[["goals"]])
  activity <- xmlValue(xmlNode[["activities"]][["activityEuLegislative"]])
  listInterest <-xmlSApply(xmlNode[["interests"]], function(x) xmlSApply(x, xmlValue))
  inter <- paste(listInterest, collapse=";")
  output <- data.frame(countries = character(),
                       categories = character(),
                       subCategories = character(), 
                       goals = character(),
                       activities = character(),
                       interest = character())
  output<-rbind(output, data.frame(countries = country,
                                   categories = cat, 
                                   subCategories = subCat, 
                                   goals = goal, 
                                   activities = activity,
                                   interest = inter)) 
  return(output)
}

rootNode = xmlRoot(xmlfile)
data <- list()
for (i in 1:xmlSize(rootNode[["resultList"]])){
  data[[i]] =func_parseXMLNodeInterestRepresentative(rootNode[["resultList"]][i][[1]])
}

tmp <- do.call(rbind, data)
dataset <- data.frame(tmp)

#remove useless variables 
remove(i)
remove(data)
remove(rootNode)
remove(xmlfile)
remove(tmp)

# ---------------------------------------------------------------------------------------------------------------------------------------------
# III. BUILD DATA.FRAME FOR MAP ###############################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

#list the possible list of value for interest
listInterest <- paste(dataset$interest, collapse = ';')
listInterest <- strsplit(listInterest, ";")[[1]]
listInterest <- unique(listInterest)

func_test <- function(data, filterValue){
  data <- data[grep(filterValue, data[, "interest"]), ]
  output <- count(data, c('countries', 'categories', 'subCategories'))
  output <- cbind(output, filterValue)
  
  return(output)
}

#Compute the number of entity per combination of country, category, subcategory and interest
data <- list()
i<-1
for (j in 1:length(listInterest)){
  data = rbind(data,func_test(dataset, listInterest[j]))
  i<- i+1
}

#reorder columns
data <- data[c(1,2,3,5,4)]

#save dataset
write.table(data, "../Datasets/datasetMap.csv", sep = ";", quote = FALSE, row.names = FALSE)

#remove useless variables 
remove(data)
remove(i)
remove(j)
remove(listInterest)
remove(func_test)


# ---------------------------------------------------------------------------------------------------------------------------------------------
# III. BUILD DATA.FRAME FOR SANKEY ############################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

func_BuildDatasetForSankeyBasedOnFilterInterest <- function(data, filterValue){
  data <- data[grep(filterValue, data$interest), ]
  data$interest <- filterValue
  output <-aggregate(data$subCategories, 
                     by=list(data$subCategories, data$interest),
                     FUN=length, simplify = FALSE)
  output$Group.2 <- filterValue
  return (output)
}

#compute the number of pair categories / subcategorie 
datasetSankey1 <-aggregate(dataset$categories, 
                       by=list(dataset$categories, dataset$subCategories),
                       FUN=length, simplify = FALSE)

#compute the list of possible interests 
listInterst <- paste(dataset$interest, collapse = ';')
listInterst <- strsplit(listInterst, ";")[[1]]
listInterst <- unique(listInterst)

#compute the number of pair subcategorie / Interest
data <- list()
for (i in 1:length(listInterst)){
  data[[i]] = func_BuildDatasetForSankeyBasedOnFilterInterest(dataset, listInterst[i])
}
tmp <- do.call(rbind, data)
datasetSankey2 <- data.frame(tmp)

# aggregate the two datasets 
datasetSankey <- rbind(datasetSankey1, datasetSankey2)
datasetSankey <- plyr::rename(datasetSankey,c("Group.1"="From",
                                              "Group.2" = "To"))
datasetSankey <- data.frame(lapply(datasetSankey, as.character), stringsAsFactors=FALSE)

#save dataset
write.table(datasetSankey, "../Datasets/datasetSankey.csv", sep = ";", quote = FALSE, row.names = FALSE)

#remove useless variables 
remove(data)
remove(listInterst)
remove(datasetSankey1)
remove(datasetSankey2)
remove(tmp)
remove(i)


# ---------------------------------------------------------------------------------------------------------------------------------------------
# III. BUILD DATA.FRAME FOR WORDCLOUD #########################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------

func_CleanString <- function(data){
  docs <- Corpus(VectorSource(data) )
  docs <- tm_map(docs, removePunctuation)   
  docs <- tm_map(docs, removeNumbers)  
  docs <- tm_map(docs, tolower) 
  docs <- tm_map(docs, removeWords, c(stopwords("english"), 
                                      stopwords("danish"),
                                      stopwords("dutch"), 
                                      stopwords("finnish"), 
                                      stopwords("french"), 
                                      stopwords("german"), 
                                      stopwords("hungarian"), 
                                      stopwords("italian"),
                                      stopwords("norwegian"), 
                                      stopwords("portuguese"), 
                                      stopwords("spanish"), 
                                      stopwords("norwegian"), 
                                      stopwords("swedish")))
  docs <- tm_map(docs, stripWhitespace)
}

# aggregate all goals and activities 
listGoal <- paste(dataset$goals, collapse = ' ')
listActivity <- paste(dataset$activities, collapse = ' ')

# remove punctuation, number, put everything to lower, remove stop words and white space
listGoal <- func_CleanString(listGoal)
listActivity <- func_CleanString(listActivity)

# create document-term matrix (standard with frequency)
dtm_goal <- DocumentTermMatrix(listGoal, control=list(wordLengths=c(3,Inf)))
dtm_activity <- DocumentTermMatrix(listActivity, control=list(wordLengths=c(3,Inf)))

# Generate list of 100 most frequent words
freq_goal <- head(sort(colSums(as.matrix(dtm_goal)), decreasing=TRUE),100)
freq_activity <- head(sort(colSums(as.matrix(dtm_activity)), decreasing=TRUE),100)

# Build JSON
JSON_goal <- toJSON(freq_goal)
JSON_activity <- toJSON(freq_list_activity)

#save dataset
write.table(JSON_goal, "../Datasets/wordCountMapping_goal_100.js", sep = ";", quote = FALSE, row.names = FALSE)
write.table(JSON_activity, "../Datasets/wordCountMapping_activity_100.js", sep = ";", quote = FALSE, row.names = FALSE)
