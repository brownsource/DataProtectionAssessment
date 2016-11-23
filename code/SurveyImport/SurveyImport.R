################################################################################
# IMPORTS A KOBO FORM RESULTS, PROCESSES AND SPLITS RESULTS INTO SEPERATE FILES

# INPUTS: 
# ../../DataProtectionAssessmentNotPublic/data/SurveyImport/[Country]_DataProtectionSurveyResults.csv
# ../../DataProtectionAssessmentNotPublic/data/SurveyImport/[Country]_PartnerList.csv

# CLEAR WORKSPACE
rm(list = ls()) # Remove all the objects we created so far.

# INSTALL PACKAGES IF REQUIRED
if(!require(dplyr)){
        install.packages("dplyr")
}

# INSTALL LIBRARIES IF REQUIRED
#!! NEED TO CHANGE THIS BLOCK OF CODE !!#
library(dplyr)

### SELECT WHICH COUNTRY
country <- "jordan"
# country <- "lebanon"

# LOAD THE DATA
# Import survey data
survey.import <- read.csv(paste("../DataProtectionAssessmentNotPublic/data/SurveyImport/", country, "_DataProtectionSurveyResults.csv", sep = ""),
                          header = TRUE,
                          stringsAsFactors = FALSE)
survey.import[survey.import == "n/a"] <- ""
survey.import[survey.import == "N/A"] <- ""

# Import partner list to retrieve partner names
partner.list <- read.csv(paste("../DataProtectionAssessmentNotPublic/data/SurveyImport/", country, "_PartnerList.csv", sep = ""),
                         header = TRUE, 
                         stringsAsFactors = FALSE)



