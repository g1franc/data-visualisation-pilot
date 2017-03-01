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
countries = read.csv("../Datasets/Countries.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, comment.char="")


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

countries$name <- toupper(countries$name)
countries <- subset (countries, select =c(name, EU28))
datasetMap <- merge(dataset, countries, by.x=c("countries"), by.y=c("name"), all.x=TRUE)
datasetMap <- datasetMap[datasetMap$EU28 == TRUE,]
datasetMap <- datasetMap[!is.na(datasetMap$interest),]


#list the possible list of value for interest
listInterest <- paste(datasetMap$interest, collapse = ';')
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
  data = rbind(data,func_test(datasetMap, listInterest[j]))
  i<- i+1
}

#reorder columns
data <- data[c(1,2,3,5,4)]

data <- plyr::rename(data,c("filterValue" = "Interest",
                            "freq" = "Count"))
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
  #data <- dataset
  #filterValue <- "Sport"
  data <- data[grep(filterValue, data$interest), ]
  data$interest <- filterValue
  output <-aggregate(data$subCategories, 
                     by=list(data$categories, data$subCategories, data$interest),
                     FUN=length, simplify = FALSE)
  output$Group.3 <- filterValue
  return (output)
}
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
datasetSankey <- data.frame(tmp)

# aggregate the two datasets 
datasetSankey <- plyr::rename(datasetSankey,c("Group.1" = "category",
                                              "Group.2" = "subcategory",
                                              "Group.3" = "interest"))
datasetSankey <- data.frame(lapply(datasetSankey, as.character), stringsAsFactors=FALSE)
datasetSankey$x <- as.numeric(datasetSankey$x)

# compute number of entity per category
tmp <-aggregate(datasetSankey$x, 
                   by=list(datasetSankey$category),
                   FUN=sum, simplify = FALSE)
datasetSankey <- merge(datasetSankey, tmp, by.x=c("category"), by.y=c("Group.1"), all.x=TRUE)
datasetSankey <- plyr::rename(datasetSankey,c("x.x" = "interest.x",
                                              "x.y" = "category.x"))
                                              
# compute number of entity per subcategory
tmp <-aggregate(datasetSankey$interest.x, 
                by=list(datasetSankey$category, datasetSankey$subcategory),
                FUN=sum, simplify = FALSE)
datasetSankey <- merge(datasetSankey, tmp, by.x=c("category","subcategory"), by.y=c("Group.1","Group.2"), all.x=TRUE)
datasetSankey <- plyr::rename(datasetSankey,c("x" = "subCategory.x"))

# add colours 
colours <- c('#2C1320', '#432818','#52050a', '#9e0142', '#d53e4f', '#E3879E', '#f46d43', '#fdae61', '#fee08b', '#ffffbf', '#e6f598', '#abdda4', '#66c2a5', '#3288bd', '#5e4fa2', '#03254e', '#4a5759', '#FECEE9', '#5F4B66',
             '#2C1320', '#432818','#52050a', '#9e0142', '#d53e4f', '#E3879E', '#f46d43', '#fdae61', '#fee08b', '#ffffbf', '#e6f598', '#abdda4', '#66c2a5', '#3288bd', '#5e4fa2', '#03254e', '#4a5759', '#FECEE9')
interest <- unique(datasetSankey$interest)
colours = data.frame(interest, colours)
datasetSankey <- merge(datasetSankey, colours, by.x=c("interest"), by.y=c("interest"), all.x=TRUE)

datasetSankey <- datasetSankey[c("category", "subcategory", "interest", "interest.x", "category.x", "subCategory.x", "colours")]
datasetSankey <- datasetSankey[order(datasetSankey$category,datasetSankey$subcategory,datasetSankey$interest),]

#datasetSankey <- head(datasetSankey, n=2)
#convert to json 
makeList<-function(x, parentNode){
  # x <- datasetSankey
  if(ncol(x)>5){
    listSplit <- split(x[-1],x[1],drop=T)
    nodeWeight <- as.numeric(x[[1,5]])
    lapply(names(listSplit),
           function(y){
             list(name=y,
                  weight = nodeWeight,
                  parent=parentNode,
                  children=makeList(listSplit[[y]], y))
             }
           )
  }else{
    lapply(seq(nrow(x[1])),
           function(y){
             list(name=x[,1][y],
                  parent=parentNode,
                  colours=x[,5][y],
                  weight=x[,2][y])
             }
           )
  }
}

jsonOut<-toJSON(list(name="Root", parent="null", children=makeList(datasetSankey, "Root")))
#save dataset
write.table(jsonOut, "../Datasets/datasetSankey.js", sep = ";", quote = FALSE, row.names = FALSE)





#remove useless variables 
remove(data)
remove(colours)
remove(listInterst)
remove(datasetSankey)
remove(tmp)
remove(i)
remove(jsonOut)
remove(interest)
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
