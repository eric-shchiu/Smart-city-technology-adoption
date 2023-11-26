library(dplyr)
library(readxl)
library(writexl)
library(tidyr)

setwd("~/Box Sync/Index blueprint2/Water and Transit Project Notes/Organizational Outreach/Firmographs/Firmographs2022/Survey")

#read in qualtrics file (raw)
surv <- read_excel("2.24TransportationSurvey_October 30, 2023_12.53.xlsx")
surv <- surv[-1, ]
surv$Q67=ifelse(surv$Q67=="Other", surv$Q68, surv$Q67)
#update stanislaus RTA name because they are on our list, just did not find themselves :)
surv$Q67 <- ifelse(surv$Q67 == "Stanislaus Regional Transit Authority", "Stanislaus Regional Transit",surv$Q67)
surv <- subset(surv, !(is.na(surv$Q67)))
surv$indicator <- apply(surv[, 18:51], 1, function(row) any(!is.na(row))) #indicator if they replied to anything at all
# Convert TRUE/FALSE to 1/0
surv$indicator <- as.integer(surv$indicator)
length(unique(surv$Q67[surv$indicator==1]))
surv <- subset(surv, surv$indicator==1)

#read in original sampling frame to ID agency characteristics
samp_frame <- read_excel("transit_survey.xlsx")
samp_frame$survey_response <- ifelse(samp_frame$AgencyName %in% surv$Q67, 1, 0)
write_xlsx(samp_frame, "transit_survey.xlsx")


#transit signal priority respondents

#to analyze survey, need to merge responses!!! need to revisit strategy for merging

#Q8_1 is tsp adoption

#Q18 is for piloting, future adoption

#Q33 is rationale for adoption


