################################################################################
OutputCSV <- function(country.Data, country.Name){
        
        # Saves a CSV of Partner information for that country of the iGraph Object
        # 
        # Args: 
        #   country.Data: the iGraph Object for the country
        # 
        # Returns:
        #   -

        export.Data <- data.frame(
                name        = V(country.Data)$name,
                acronym     = V(country.Data)$acronym,
                type        = V(country.Data)$type,
                egypt       = V(country.Data)$egypt,
                iraq        = V(country.Data)$iraq,
                jordan      = V(country.Data)$jordan,
                lebanon     = V(country.Data)$lebanon,
                turkey      = V(country.Data)$turkey,
                DSA_jordan  = V(country.Data)$DSA_jordan,
                DSA_lebanon = V(country.Data)$DSA_lebanon
        )

        # Set the output directory
        output.Path <- "../DataProtectionAssessmentNotPublic/output/PartnerMapping/"
        filename.Path <- paste(output.Path, country.Name, ".csv", sep = "")
        dir.create(dirname(filename.Path), showWarnings = FALSE)
        
        write.csv(export.Data, file = filename.Path, row.names=FALSE)       
}