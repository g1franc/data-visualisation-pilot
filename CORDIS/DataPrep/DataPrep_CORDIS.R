workdir <- getwd()
setwd(workdir)

arg_1 = "true"
arg_2 = "false"
arg_3 = "false"
arg_4 = "2016"

#### Load arguments ####
args <- commandArgs(trailingOnly = TRUE)
arg_1 = args[1] #Chord boolean
arg_2 = args[2] #Motionchart boolean
arg_3 = args[3] #Organisations Network boolean
arg_4 = args[4] #update with data until year x
cat(paste("arguments loaded succesfully:", 
        "\n Chord: ", arg_1, 
        "\n Motionchart: ", arg_2,
        "\n Organisations Network: ", arg_3,
        "\n Update with data until year: ", arg_4,
        "\n"))


#### Call scripts depending on variables ####
source("LoadDatasets.R")
cat("The required datasets are loaded \n")

if ( arg_1 == "true" ) {
  source("DataPrep_Chord.R")
  cat("The data for the chord diagram is now updated \n")
}

if ( arg_2 == "true" ) {
  source("DataPrep_Motionchart.R")
  cat("The data for the motionchart is now updated \n")
}

if ( arg_3 == "true" ) {
  source("DataPrep_OrganisationsNetwork.R")
  cat("The data for the organisations network is now updated \n")
}