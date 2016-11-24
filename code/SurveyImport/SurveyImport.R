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

################################################################################
### PREPARE THE COMMENTS DATA ##################################################
################################################################################

# Create a data.frame
comments <- data.frame(matrix(ncol = 10, nrow = nrow(survey.import)))
comments.Labels <- c("id",
                     "label",
                     "organisation.Name",
                     "participant.Name",
                     "email.Address",
                     "summary.Request",
                     "own.Case.Management",
                     "reasons.For.Sharing",
                     "implications.If.Not.Shared",
                     "comment")
colnames(comments) <- comments.Labels
rm(comments.Labels)

# import data (matching some with the partner_list table)
comments$id                         <- survey.import$GROUP_organization_details.organizational_connections_source_organisation
comments$label                      <- partner.list$Acronym[match(comments$id,partner.list$ID)]
comments$organisation.Name          <- partner.list$Name[match(comments$id,partner.list$ID)]
comments$participant.Name           <- survey.import$GROUP_participation_agreement.participation_name
comments$email.Address              <- survey.import$GROUP_participation_agreement.participation_email
comments$summary.Request            <- survey.import$GROUP_participation_agreement.summary_request_yn
comments$own.Case.Management        <- survey.import$GROUP_organization_details.diagnostics_own_case_management_name
comments$reasons.For.Sharing        <- survey.import$GROUP_comments.reasons_for_sharing
comments$implications.If.Not.Shared <- survey.import$GROUP_comments.implications_not_sharing
comments$comment                    <- survey.import$GROUP_comments.comments

# output to CSV

filename.Path <- paste("../DataProtectionAssessmentNotPublic/output/SurveyResults/", country, "/tables/ParticipantComments.csv", sep = "")
dir.create(dirname(filename.Path), showWarnings = FALSE)
write.csv(comments, file = filename.Path, row.names=FALSE)

################################################################################
### PREPARE THE EDGES DATA #####################################################
################################################################################

# Create a data.frame to save the cleaned EDGE values to
edges <- data.frame(matrix(ncol = 5))
column.Names.Edges <- c(
        "from", 
        "to",
        "label",
        "type",
        "weight"
)
colnames(edges) <- column.Names.Edges
edges <- edges[-c(1), ]
# Remove the column name vector
rm(column.Names.Edges)

# IDENTIFY DOWNSTREAM ORGANISATION CONNECTIONS
# Repeat for each row in kobo_import
for (i in 1:nrow(survey.import)){
        
        # If the value of the cell is TRUE
        TargetOrgs <- c(ifelse (survey.import[i,57:147] == "True",
                                # Extract the Organisation ID from the header
                                as.numeric(substr(c(colnames(survey.import[57:147])), 76, 78)), #would like to use nchar to get the string length
                                # Or write NA
                                NA))
        # Remove the NA
        TargetOrgs <- TargetOrgs[!is.na(TargetOrgs)]
        # If thelength of TargetOrgs is NOT 0 then Transpose the Organisatonis to the Edges table with the Source ID as the current Row's Organisation
        if(length(TargetOrgs) != 0) { 
                # Create a vector with the source organisation
                SourceOrgs <- rep(as.character(survey.import[i,4]), length(TargetOrgs))
                # Create a vector with target organisations and some other fixed variables
                EdgeFrame <-data.frame(from = SourceOrgs, to = TargetOrgs, label = NA, type = "Directed", weight = 1)
                # Append the values to the edges table
                edges <- rbind(edges, EdgeFrame)
        }
}
rm(EdgeFrame)

# IDENTIFY UPSTREAM ORGANISATION CONNECTIONS

# Repeat for each row in kobo_import
for (i in 1:nrow(survey.import)){
        
        # If the value of the cell is TRUE
        SourceOrgs <- c(ifelse (survey.import[i,151:241] == "True",
                                # Extract the Organisation ID from the header
                                as.numeric(substr(c(colnames(survey.import[57:147])), 76, 78)),
                                # Or write NA
                                NA))
        # Remove the NA
        SourceOrgs <- SourceOrgs[!is.na(SourceOrgs)]
        # If thelength of TargetOrgs is NOT 0 then Transpose the Organisatonis to the Edges table with the Source ID as the current Row's Organisation
        if(length(SourceOrgs) != 0) { 
                # Create a vector with the source organisation
                TargetOrgs <- rep(as.character(survey.import[i,4]), length(SourceOrgs))
                # Create a vector with target organisations and some other fixed variables
                EdgeFrame <-data.frame(from = as.character(SourceOrgs), to = TargetOrgs, label = NA, type = "Directed", weight = 1)
                # Append the values to the edges table
                edges <- rbind(edges, EdgeFrame)
        }
}
rm(EdgeFrame)

# output to CSV

filename.Path <- paste("../DataProtectionAssessmentNotPublic/output/SurveyResults/", country, "/tables/edges.csv", sep = "")
dir.create(dirname(filename.Path), showWarnings = FALSE)
write.csv(edges, file = filename.Path, row.names=FALSE)


################################################################################
### PREPARE THE NODES DATA #####################################################
################################################################################

nodes <- data.frame(matrix(ncol = 33, nrow = nrow(survey.import)))
nodes.Labels <- c("org.ID",
                  "org.Label",
                  "org.Name",
                  "org.Type",
                  "replied.Or.Nominated",
                  "sector.Basic_needs",
                  "sector.Education",
                  "sector.Food_security",
                  "sector.Health",
                  "sector.Livelihoods",
                  "sector.Protection",
                  "sector.Shelter",
                  "sector.WASH",
                  "location.camp_based",
                  "location.non_camp_based",
                  "data_protection.Organisational_measures.Policy",
                  "data_protection.Organisational_measures.Adherence",
                  "data_protection.Organisational_measures.Total",
                  "data_protection.Physical_measures.Policy",
                  "data_protection.Physical_measures.Adherence",
                  "data_protection.Physical_measures.Total",
                  "data_protection.Technical_measures.Policy",
                  "data_protection.Technical_measures.Adherence",
                  "data_protection.Technical_measures.Total",
                  "data_protection.Rights_of_data_subject_measures.Policy",
                  "data_protection.Rights_of_data_subject_measures.Adherence",
                  "data_protection.Rights_of_data_subject_measures.Total",
                  "data_protection.Onward_sharing_measures.Policy",
                  "data_protection.Onward_sharing_measures.Adherence",
                  "data_protection.Onward_sharing_measures.Total",
                  "data_protection.Data_protection.Policy",
                  "data_protection.Data_protection.Adherence",
                  "data_protection.Data_protection.Total")
colnames(nodes) <- nodes.Labels
rm(nodes.Labels)

# import data (matching some with the partner.list table)
nodes$org.ID    <- survey.import$GROUP_organization_details.organizational_connections_source_organisation
nodes$org.Label <- partner.list$Acronym[match(nodes$org.ID,partner.list$ID)]
nodes$org.Name  <- partner.list$Name[match(nodes$org.ID,partner.list$ID)]
nodes$org.Type  <- partner.list$Type[match(nodes$org.ID,partner.list$ID)]

nodes$replied.Or.Nominated <- rep(c("Replied"), nrow(survey.import))

nodes$sector.Basic_needs   <- survey.import$GROUP_organization_details.organizational_connections_sectors.basic_needs
nodes$sector.Education     <- survey.import$GROUP_organization_details.organizational_connections_sectors.education
nodes$sector.Food_security <- survey.import$GROUP_organization_details.organizational_connections_sectors.food_security
nodes$sector.Health        <- survey.import$GROUP_organization_details.organizational_connections_sectors.health
nodes$sector.Livelihoods   <- survey.import$GROUP_organization_details.organizational_connections_sectors.livelihoods
nodes$sector.Protection    <- survey.import$GROUP_organization_details.organizational_connections_sectors.protection
nodes$sector.Shelter       <- survey.import$GROUP_organization_details.organizational_connections_sectors.shelter
nodes$sector.WASH          <- survey.import$GROUP_organization_details.organizational_connections_sectors.wash

nodes$location.camp_based     <- survey.import$GROUP_organization_details.organizational_connections_location.camp_based
nodes$location.non_camp_based <- survey.import$GROUP_organization_details.organizational_connections_location.non_camp_based

# Calculate ORGANISATIONAL MEASURES values
nodes$data_protection.Organisational_measures.Policy <- ((
        survey.import$GROUP_data_protection.SUBGROUP_organizational_measures.dp_om_focal_point +
                survey.import$GROUP_data_protection.SUBGROUP_organizational_measures.dp_om_have_policy +
                survey.import$GROUP_data_protection.SUBGROUP_organizational_measures.dp_om_have_code_of_conduct +
                survey.import$GROUP_data_protection.SUBGROUP_organizational_measures.dp_om_data_breach_process)
) / 4

nodes$data_protection.Organisational_measures.Adherence <- survey.import$GROUP_data_protection.SUBGROUP_organizational_measures.dp_om_staff_rating / 5

nodes$data_protection.Organisational_measures.Total <- ((
        survey.import$GROUP_data_protection.SUBGROUP_organizational_measures.dp_om_focal_point +
                survey.import$GROUP_data_protection.SUBGROUP_organizational_measures.dp_om_have_policy +
                survey.import$GROUP_data_protection.SUBGROUP_organizational_measures.dp_om_have_code_of_conduct +
                survey.import$GROUP_data_protection.SUBGROUP_organizational_measures.dp_om_data_breach_process) *
                survey.import$GROUP_data_protection.SUBGROUP_organizational_measures.dp_om_staff_rating
) / 20

# Calculate PHYSICAL MEASURES values
nodes$data_protection.Physical_measures.Policy <- ((
        survey.import$GROUP_data_protection.SUBGROUP_physical_measures.dp_pm_stored_safe + 
                survey.import$GROUP_data_protection.SUBGROUP_physical_measures.dp_pm_offices_locked + 
                survey.import$GROUP_data_protection.SUBGROUP_physical_measures.dp_pm_staff_reminder + 
                survey.import$GROUP_data_protection.SUBGROUP_physical_measures.dp_pm_safe_paper_waste + 
                survey.import$GROUP_data_protection.SUBGROUP_physical_measures.dp_pm_visitors + 
                survey.import$GROUP_data_protection.SUBGROUP_physical_measures.dp_pm_usb + 
                survey.import$GROUP_data_protection.SUBGROUP_physical_measures.dp_pm_filing_cabinets)
) / 7

nodes$data_protection.Physical_measures.Adherence <- survey.import$GROUP_data_protection.SUBGROUP_physical_measures.dp_pm_staff_rating / 5

nodes$data_protection.Physical_measures.Total <- ((
        survey.import$GROUP_data_protection.SUBGROUP_physical_measures.dp_pm_stored_safe + 
                survey.import$GROUP_data_protection.SUBGROUP_physical_measures.dp_pm_offices_locked + 
                survey.import$GROUP_data_protection.SUBGROUP_physical_measures.dp_pm_staff_reminder + 
                survey.import$GROUP_data_protection.SUBGROUP_physical_measures.dp_pm_safe_paper_waste + 
                survey.import$GROUP_data_protection.SUBGROUP_physical_measures.dp_pm_visitors + 
                survey.import$GROUP_data_protection.SUBGROUP_physical_measures.dp_pm_usb + 
                survey.import$GROUP_data_protection.SUBGROUP_physical_measures.dp_pm_filing_cabinets) * 
                survey.import$GROUP_data_protection.SUBGROUP_physical_measures.dp_pm_staff_rating
) / 35

# Calculate TECHNICAL MEASURES values
nodes$data_protection.Technical_measures.Policy <- ((
        survey.import$GROUP_data_protection.SUBGROUP_technical_measures.dp_tm_passwords +
                survey.import$GROUP_data_protection.SUBGROUP_technical_measures.dp_tm_tiered_access +
                survey.import$GROUP_data_protection.SUBGROUP_technical_measures.dp_tm_backups +
                survey.import$GROUP_data_protection.SUBGROUP_technical_measures.dp_tm_personal_account_awareness)
) / 4

nodes$data_protection.Technical_measures.Adherence <- survey.import$GROUP_data_protection.SUBGROUP_technical_measures.dp_tm_staff_rating / 5

nodes$data_protection.Technical_measures.Total <- ((
        survey.import$GROUP_data_protection.SUBGROUP_technical_measures.dp_tm_passwords +
                survey.import$GROUP_data_protection.SUBGROUP_technical_measures.dp_tm_tiered_access +
                survey.import$GROUP_data_protection.SUBGROUP_technical_measures.dp_tm_backups +
                survey.import$GROUP_data_protection.SUBGROUP_technical_measures.dp_tm_personal_account_awareness) *
                survey.import$GROUP_data_protection.SUBGROUP_technical_measures.dp_tm_staff_rating
) / 20

# Calculate RIGHTS OF DATA SUBJECT MEASURES values
nodes$data_protection.Rights_of_data_subject_measures.Policy <- 
        ifelse (survey.import$GROUP_data_protection.SUBGROUP_rights_of_data_subjects.dp_rds_data_collection_yn == 0, NA,
                ((as.numeric(survey.import$GROUP_data_protection.SUBGROUP_rights_of_data_subjects.dp_rds_informed) +
                          as.numeric(survey.import$GROUP_data_protection.SUBGROUP_rights_of_data_subjects.dp_rds_consent) +
                          as.numeric(survey.import$GROUP_data_protection.SUBGROUP_rights_of_data_subjects.dp_rds_opportunity_to_object) +
                          as.numeric(survey.import$GROUP_data_protection.SUBGROUP_rights_of_data_subjects.dp_rds_aware_of_rights))
                ) / 4
        )

nodes$data_protection.Rights_of_data_subject_measures.Adherence <- 
        ifelse (survey.import$GROUP_data_protection.SUBGROUP_rights_of_data_subjects.dp_rds_data_collection_yn == 0, NA,
                as.numeric(survey.import$GROUP_data_protection.SUBGROUP_rights_of_data_subjects.dp_rds_staff_rating) / 5
        )

nodes$data_protection.Rights_of_data_subject_measures.Total <- 
        ifelse (survey.import$GROUP_data_protection.SUBGROUP_rights_of_data_subjects.dp_rds_data_collection_yn == 0, NA,
                ((as.numeric(survey.import$GROUP_data_protection.SUBGROUP_rights_of_data_subjects.dp_rds_informed) +
                          as.numeric(survey.import$GROUP_data_protection.SUBGROUP_rights_of_data_subjects.dp_rds_consent) +
                          as.numeric(survey.import$GROUP_data_protection.SUBGROUP_rights_of_data_subjects.dp_rds_opportunity_to_object) +
                          as.numeric(survey.import$GROUP_data_protection.SUBGROUP_rights_of_data_subjects.dp_rds_aware_of_rights)) *
                         as.numeric(survey.import$GROUP_data_protection.SUBGROUP_rights_of_data_subjects.dp_rds_staff_rating)
                ) /20
        )

# Calculate ONWARD DATA SHARING MEASURES values

temp <- data.frame(
        "tempController1" = survey.import$GROUP_data_protection.SUBGROUP_onward_data_sharing.dp_ods_yn, 
        "tempA" = survey.import$GROUP_data_protection.SUBGROUP_onward_data_sharing.dp_ods_outside_of_country, 
        "tempController2" = survey.import$GROUP_data_protection.SUBGROUP_onward_data_sharing.dp_ods_national_legislation_yn,
        "tempB"= survey.import$GROUP_data_protection.SUBGROUP_onward_data_sharing.dp_ods_national_legislation_risk_mitigation, 
        "tempC" = survey.import$GROUP_data_protection.SUBGROUP_onward_data_sharing.dp_ods_law_enforcement,
        "tempController3" = survey.import$GROUP_data_protection.SUBGROUP_onward_data_sharing.dp_ods_third_party_yn,
        "tempD" = survey.import$GROUP_data_protection.SUBGROUP_onward_data_sharing.dp_ods_third_party_provision_for_deletion,
        "tempE" = survey.import$GROUP_data_protection.SUBGROUP_onward_data_sharing.dp_ods_third_party_archived_securely, 
        "tempMultiplier" = survey.import$GROUP_data_protection.SUBGROUP_onward_data_sharing.dp_ods_staff_rating
)

result <- ifelse(temp$tempController1 == "", NA, {
        rowSums(cbind(
                c(as.numeric(as.character(temp$tempA))), 
                c(ifelse(temp$tempController2 == 0, NA, {
                        as.numeric(as.character(temp$tempB))
                })),
                c(as.numeric(as.character(temp$tempC))),
                c(ifelse(temp$tempController3 == 0, NA, {
                        as.numeric(as.character(temp$tempD)) +
                                as.numeric(as.character(temp$tempE))
                }))),
                na.rm = TRUE)
})
result.Policy <- result
result.Total <- result * as.numeric(as.character(temp$tempMultiplier))

maxmultiplier <-  ifelse(temp$tempController1 == "", NA, {
        2+
                ifelse(temp$tempController2 == 1,1,0)+
                ifelse(temp$tempController3 == 1,2,0)
})

multiplier.Policy <- maxmultiplier
multiplier.Total <- maxmultiplier * 5

nodes$data_protection.Onward_sharing_measures.Policy <- result.Policy/multiplier.Policy
nodes$data_protection.Onward_sharing_measures.Adherence <- as.numeric(as.character(temp$tempMultiplier)) / 5
nodes$data_protection.Onward_sharing_measures.Total <- result.Total/multiplier.Total              

# Calculate final DATA PROTECTION RATING value
nodes$data_protection.Data_protection.Policy <- rowMeans(nodes[c("data_protection.Organisational_measures.Policy",
                                                                 "data_protection.Physical_measures.Policy",
                                                                 "data_protection.Technical_measures.Policy",
                                                                 "data_protection.Rights_of_data_subject_measures.Policy",
                                                                 "data_protection.Onward_sharing_measures.Policy")], 
                                                         na.rm=TRUE)

nodes$data_protection.Data_protection.Adherence <- rowMeans(nodes[c("data_protection.Organisational_measures.Adherence",
                                                                    "data_protection.Physical_measures.Adherence",
                                                                    "data_protection.Technical_measures.Adherence",
                                                                    "data_protection.Rights_of_data_subject_measures.Adherence",
                                                                    "data_protection.Onward_sharing_measures.Adherence")], 
                                                            na.rm=TRUE)

nodes$data_protection.Data_protection.Total <- rowMeans(nodes[c("data_protection.Organisational_measures.Total",
                                                                "data_protection.Physical_measures.Total",
                                                                "data_protection.Technical_measures.Total",
                                                                "data_protection.Rights_of_data_subject_measures.Total",
                                                                "data_protection.Onward_sharing_measures.Total")], 
                                                        na.rm=TRUE)

# ADD orgainsitions to the node table that were listed in the edge extraction

# Create a list of all of the UPSTREAM nominated nodes
nominated.Nodes <- data.frame(org.ID = unique(edges$from))
nominated.Nodes <- cbind(nominated.Nodes, 
                         org.Label = partner.list$Acronym[match(nominated.Nodes$org.ID,partner.list$ID)], 
                         org.Name  = partner.list$Name[match(nominated.Nodes$org.ID,partner.list$ID)],
                         org.Type  = partner.list$Type[match(nominated.Nodes$org.ID,partner.list$ID)],
                         replied.Or.Nominated = "Nominated")

# Remove the nodes that already exist in the nodes table
nominated.Nodes <- nominated.Nodes[!nominated.Nodes$org.ID %in% nodes$org.ID,]

# Append remaining nominated nodes to the nodes table
nodes$org.ID <- as.character(nodes$org.ID)
nominated.Nodes$org.ID <- as.character(nominated.Nodes$org.ID)
nodes <- bind_rows(nodes, nominated.Nodes)

# Create a list of all of the DOWNSTREAM nominated nodes
nominated.Nodes <- data.frame(org.ID = unique(edges$to))
nominated.Nodes <- cbind(nominated.Nodes, 
                         org.Label = partner.list$Acronym[match(nominated.Nodes$org.ID,partner.list$ID)], 
                         org.Name  = partner.list$Name[match(nominated.Nodes$org.ID,partner.list$ID)],
                         org.Type  = partner.list$Type[match(nominated.Nodes$org.ID,partner.list$ID)],
                         replied.Or.Nominated = "Nominated")

# Remove the nodes that already exist in the nodes table
nominated.Nodes <- nominated.Nodes[!nominated.Nodes$org.ID %in% nodes$org.ID,]

# Append remaining nominated nodes to the nodes table
nodes$org.ID <- as.character(nodes$org.ID)
nominated.Nodes$org.ID <- as.character(nominated.Nodes$org.ID)
nodes <- bind_rows(nodes, nominated.Nodes) 

# output to CSV

filename.Path <- paste("../DataProtectionAssessmentNotPublic/output/SurveyResults/", country, "/tables/nodes.csv", sep = "")
dir.create(dirname(filename.Path), showWarnings = FALSE)
write.csv(nodes, file = filename.Path, row.names=FALSE)

################################################################################
### PREPARE THE FEEDBACK DATA ##################################################
################################################################################

feedback <- data.frame(matrix(ncol = 32, nrow = nrow(survey.import)))
feedback.Labels <- c("org.ID",
                     "org.Label",
                     "org.Name",
                     "org.Type",
                     "Dont_follow_policy.Lack_of_awareness_of_policy_existance",
                     "Dont_follow_policy.Lack_of_awareness_of_policy_details",
                     "Dont_follow_policy.Not_enough_time_to_follow_protocol",
                     "Dont_follow_policy.Not_enough_resource_to_follow_protocol",
                     "Dont_follow_policy.Valid_and_exceptional_circumstances_but_without_approval",
                     "Dont_follow_policy.NA",
                     "Do_follow_policy.Awareness_of_policy_existance",
                     "Do_follow_policy.Awareness_of_policy_details",
                     "Do_follow_policy.Enough_time_to_follow_protocol",
                     "Do_follow_policy.Enough_resource_to_follow_protocol",
                     "Do_follow_policy.Valid_and_exceptional_circumstances_but_with_approval",
                     "Do_follow_policy.Normative_behaviour",
                     "Do_follow_policy.NA",
                     "Satisfaction",
                     "Frequency",
                     "Transfer_method.Printed_material",
                     "Transfer_method.Unencrypted_via_email_USB",
                     "Transfer_method.Encrypted_via_email_USB",
                     "Transfer_method.FTP",
                     "Transfer_method.Secure_FTP",
                     "Transfer_method.Direct_access_through_web_interface",
                     "Transfer_method.API",
                     "Transfer_method.Other",
                     "Transfer_method.Other.List",
                     "Agree_with_statement.Happy_with_format",
                     "Agree_with_statement.Happy_with_turnaround",
                     "Agree_with_statement.Happy_with_workflow",
                     "Agree_with_statement.Happy_with_accuracy")

colnames(feedback) <- feedback.Labels
rm(feedback.Labels)

feedback$org.ID    <- survey.import$GROUP_organization_details.organizational_connections_source_organisation
feedback$org.Label <- partner.list$Acronym[match(feedback$org.ID,partner.list$ID)]
feedback$org.Name  <- partner.list$Name[match(feedback$org.ID,partner.list$ID)]
feedback$org.Type  <- partner.list$Type[match(nodes$org.ID,partner.list$ID)]

feedback[,5:28] <- survey.import[,256:279]
feedback[,29:32] <- survey.import[,281:284] 

# output to CSV

filename.Path <- paste("../DataProtectionAssessmentNotPublic/output/SurveyResults/", country, "/tables/Feedback.csv", sep = "")
dir.create(dirname(filename.Path), showWarnings = FALSE)
write.csv(feedback, file = filename.Path, row.names=FALSE)

################################################################################
### PREPARE THE FEEDBACK DATA ##################################################
################################################################################

DPAllIndicators <- data.frame(matrix(ncol = 37, nrow = nrow(survey.import)))
DPAllIndicators.Labels <- c("org.ID",
                            "org.Label",
                            "org.Name",
                            "org.Type",
                            "DPOM.FocalPoint",
                            "DPOM.HavePolicy",
                            "DPOM.CodeOfConduct",
                            "DPOM.DataBreachProcess",
                            "DPOM.StaffAdherence",
                            "DPPM.StoredSafe",
                            "DPPM.OfficesLocked",
                            "DPPM.StaffReminder",
                            "DPPM.SafePaperWaste",
                            "DPPM.Visitors",
                            "DPPM.USB",
                            "DPPM.FilingCabinets",
                            "DPPM.StaffAdherence",
                            "DPTM.Passwords",
                            "DPTM.TieredAccess",
                            "DPTM.Backups",
                            "DPTM.PersonalAccountAwareness",
                            "DPTM.StaffAdherence",
                            "DPRDS.DataCollection_YN",
                            "DPRDS.Informed",
                            "DPRDS.Consent",
                            "DPRDS.OpportunityToObject",
                            "DPRDS.AwareOfRights",
                            "DPRDS.StaffAdherence",
                            "DPODS.ODS_YN",
                            "DPODS.OutsideOfCountry",
                            "DPODS.NationalLegislation_YN",
                            "DPODS.NationalLegislationRiskMitigation",
                            "DPODS.LawEnforcement",
                            "DPODS.ThirdParty_YN",
                            "DPODS.ThirdPartyProvisionForDeletion",
                            "DPODS.ThirdPartyArchivedSecurely",
                            "DPODS.StaffAdherence")
colnames(DPAllIndicators) <- DPAllIndicators.Labels
rm(DPAllIndicators.Labels)

DPAllIndicators$org.ID    <- survey.import$GROUP_organization_details.organizational_connections_source_organisation
DPAllIndicators$org.Label <- partner.list$Acronym[match(DPAllIndicators$org.ID,partner.list$ID)]
DPAllIndicators$org.Name  <- partner.list$Name[match(DPAllIndicators$org.ID,partner.list$ID)]
DPAllIndicators$org.Type  <- partner.list$Type[match(DPAllIndicators$org.ID,partner.list$ID)]

DPAllIndicators[,5:9] <- survey.import[,19:23]
DPAllIndicators[,10:17] <- survey.import[,25:32]
DPAllIndicators[,18:22] <- survey.import[,34:38]
DPAllIndicators[,23:29] <- survey.import[,40:46]
DPAllIndicators[,30:37] <- survey.import[,48:55]


# output to CSV

filename.Path <- paste("../DataProtectionAssessmentNotPublic/output/SurveyResults/", country, "/tables/DataProtectionAllIndicators.csv", sep = "")
dir.create(dirname(filename.Path), showWarnings = FALSE)
write.csv(DPAllIndicators, file = filename.Path, row.names=FALSE)
