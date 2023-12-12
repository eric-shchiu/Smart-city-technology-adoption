library(dplyr)
library(readxl)
library(writexl)
library(tidyr)
library(ggplot2)

setwd("~/Box Sync/Index blueprint2/Water and Transit Project Notes/Organizational Outreach/Firmographs/Firmographs2022/Survey")

#Clean survey-----
#read in qualtrics file (raw)
surv <- read_excel("2.24TransportationSurvey_October 30, 2023_12.53.xlsx")
surv <- surv[-1, ]
surv$Q67=ifelse(surv$Q67=="Other", surv$Q68, surv$Q67)
#update stanislaus RTA name because they are on our list, just did not find themselves :)
surv$Q67 <- ifelse(surv$Q67 == "Stanislaus Regional Transit Authority", "Stanislaus Regional Transit",surv$Q67)
surv <- subset(surv, !(is.na(surv$Q67)))
surv$indicator <- apply(surv[, 18:51], 1, function(row) any(!is.na(row))) #indicator if they replied to anything at all
# Convert TRUE/FALSE to 1/0
surv <- subset(surv, surv$Q67 !="DOT" & surv$Q67 !="DOT2")
surv$indicator <- as.integer(surv$indicator)
surv <- subset(surv, surv$indicator==1)

#read in original sampling frame to ID agency characteristics
samp_frame <- read_excel("transit_survey.xlsx")
samp_frame$survey_response <- ifelse(samp_frame$AgencyName %in% surv$Q67, 1, 0)
#write_xlsx(samp_frame, "transit_survey.xlsx")

####Multiple responses-----
# Identify the most complete response for each agency

#this definitely creates issues. Fresno COG responses are the exact same length , I went with the longer one in duration the first time
surv$`Duration (in seconds)` <- as.numeric(surv$`Duration (in seconds)`)

# Assuming 'Q67' is the agency name and Q7_1 to Q8_17 are the categorical question variables
completeness_duration_ranking <- surv %>%
  group_by(ResponseId) %>%
  summarize(
    completeness = sum(!is.na(across(starts_with('Q')))),
    max_duration = max(`Duration (in seconds)`, na.rm = TRUE)
  )

# Initialize completeness values in the original data frame
surv <- left_join(surv, completeness_duration_ranking, by = "ResponseId")

# Sort by completeness and duration descending
surv <- surv %>% arrange(Q67, desc(completeness), desc(max_duration))

# Initialize a new column to store agency completeness rank
surv$agency_rank <- NA

# Assign completeness ranks to each agency
unique_agencies <- unique(surv$Q67)
for (agency in unique_agencies) {
  rank <- seq_len(sum(surv$Q67 == agency))
  surv$agency_rank[surv$Q67 == agency] <- rank
}

surv_filled <- surv %>%
  group_by(Q67) %>%
  arrange(agency_rank) %>%
  fill(starts_with('Q'), .direction = 'up') %>%
  ungroup()

# Select only the columns you need
result_df_filled <- surv_filled %>%
  filter(agency_rank == 1) %>%
  select(Q67, starts_with('Q')) %>%
  distinct()

#let's join orgtype to the survey reults
surv2 <- samp_frame %>% select(AgencyName, OrgType)
result_df_filled = merge(result_df_filled, surv2, by.x="Q67", by.y="AgencyName", all.x=FALSE, all.y=FALSE)

#Transit Signal priority-----
#Q8_1 is tsp adoption
table(result_df_filled$Q7_1) #one adopter from MPA
table(result_df_filled$Q8_1)
result_df_filled$TSP <- ifelse(result_df_filled$Q8_1=="Procuring"|result_df_filled$Q8_1=="Piloting"|result_df_filled$Q8_1=="Using Systemwide"|
                                 result_df_filled$Q7_1=="Yes" , 1,0)
sum(na.omit(result_df_filled$TSP))

####Plots-----
tspadopt <- result_df_filled %>% group_by(Q8_1, OrgType) %>% summarize(Count=n())
ggplot(na.omit(subset(tspadopt, tspadopt$Q8_1 !="Not at all")), aes(x=Q8_1, y=Count, fill=OrgType))+geom_bar(stat="identity", position="dodge")+
  xlab("Transit Signal Priority Adoption Stage") +ylab("Count of Agencies")+
  theme_minimal()

#Q18 is for piloting, future adoption

#Q33 is rationale for adoption, need to ID all moderately and very important responses, then tabulate
#by agency type
# Assuming result_df_filled is your DataFrame
selected_columns <- c('Q67', 'OrgType', 'Q33_1', 'Q33_2', 'Q33_3', 'Q33_4', 'Q33_5', 'Q33_6', 'Q33_7', 'Q33_8', 'Q33_9', 'Q33_10', 'Q33_10_TEXT')

# Subset the DataFrame
subset_df <- result_df_filled[selected_columns]
# Assuming your DataFrame is named result_df_filled

# Define a mapping between column names and reasons
reason_mapping <- c(
  "Q67"="AgencyName",
  "OrgType"="OrgType",
  "Q33_1" = "Obsolescence of old technology",
  "Q33_2" = "Workforce expectations",
  "Q33_3" = "Regulatory compliance, state mandates",
  "Q33_4" = "Transparency and public relations",
  "Q33_5" = "Public pressure",
  "Q33_6" = "Operating cost reductions",
  "Q33_7" = "Improve user experience and safety",
  "Q33_8" = "Environmental concerns",
  "Q33_9" = "Expand access to underserved communities",
  "Q33_10" = "Other",
  "Q33_10_TEXT" = "Other - Text"
)

# Rename the columns using the mapping
colnames(subset_df) <- reason_mapping[colnames(subset_df)]

#keep only moderately and very important
subset_df_binary <- subset_df %>%
  mutate_at(
    vars(-c("OrgType", "AgencyName")),
    ~as.numeric(. %in% c("Very important", "Extremely important","Moderately important"))
  )

# Define non-numeric columns to exclude from aggregation
non_numeric_columns <- c("OrgType", "AgencyName")

# Aggregate by OrgType and calculate the sum of numeric columns
aggregated_df <- subset_df_binary %>%
  group_by(OrgType) %>%
  summarise_at(vars(-one_of(non_numeric_columns)), sum)

# Reshape the data for plotting
aggregated_df_long <- tidyr::gather(aggregated_df, key = "Rationale", value = "Count", -OrgType)
aggregated_df_long <- subset(aggregated_df_long, aggregated_df_long$Rationale !="Other" & 
                               aggregated_df_long$Rationale !="Other - Text")


# Plot using ggplot2
ggplot(aggregated_df_long, aes(x = Rationale, y = Count, fill = OrgType)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Count of Agencies by Reason and Organization Type",
       x=NULL,
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels by 45 degrees

#MINUTES----
mins <- read_excel("~/Downloads/1061 Minutes by Year.xlsx")
setdiff(mins$`NTD ID`, samp_frame$NTDID)

#Real time transit data-----

table(result_df_filled$Q7_12) #one adopter from MPA
table(result_df_filled$Q8_12)
result_df_filled$RTPD <- ifelse(result_df_filled$Q8_12=="Procuring"|result_df_filled$Q8_12=="Piloting"|result_df_filled$Q8_12=="Using Systemwide"|
                                 result_df_filled$Q7_12=="Yes" , 1,0)
sum(na.omit(result_df_filled$RTPD))

##rationale-----
selected_columns <- c('Q67', 'OrgType', 'Q38_1', 'Q38_2', 'Q38_3', 'Q38_4', 'Q38_5', 'Q38_6', 'Q38_7', 'Q38_8', 'Q38_9', 'Q38_10', 'Q38_10_TEXT')

# Subset the DataFrame
subset_df <- result_df_filled[selected_columns]
# Assuming your DataFrame is named result_df_filled

# Define a mapping between column names and reasons
reason_mapping <- c(
  "Q67"="AgencyName",
  "OrgType"="OrgType",
  "Q38_1" = "Obsolescence of old technology",
  "Q38_2" = "Workforce expectations",
  "Q38_3" = "Regulatory compliance, state mandates",
  "Q38_4" = "Transparency and public relations",
  "Q38_5" = "Public pressure",
  "Q38_6" = "Operating cost reductions",
  "Q38_7" = "Improve user experience and safety",
  "Q38_8" = "Environmental concerns",
  "Q38_9" = "Expand access to underserved communities",
  "Q38_10" = "Other",
  "Q38_10_TEXT" = "Other - Text"
)

# Rename the columns using the mapping

colnames(subset_df) <- reason_mapping[colnames(subset_df)]

#keep only moderately and very important
subset_df_binary <- subset_df %>%
  mutate_at(
    vars(-c("OrgType", "AgencyName")),
    ~as.numeric(. %in% c("Very important", "Extremely important","Moderately important"))
  )

#add in size indicator 
sf <- samp_frame %>% select(AgencyName, VOMS)
sf$size <- ifelse(sf$VOMS > 99, 1,sf$VOMS ) #this keeps NAs so we exclude
sf$size <- ifelse(sf$VOMS < 99, 0,sf$size)
subset_df_binary <- merge(subset_df_binary, sf, by.x="AgencyName", by.y="AgencyName", all.x=TRUE, all.y=FALSE)
result_df_filled <- merge(result_df_filled, sf, by.x="Q67", by.y="AgencyName", all.x=TRUE, all.y=FALSE)

# Define non-numeric columns to exclude from aggregation
non_numeric_columns <- c("OrgType", "AgencyName", "size")

#totals for rationale
column_sums <- as.data.frame(colSums(subset_df_binary[, !names(subset_df_binary) %in% c("AgencyName", "OrgType")]))
print(column_sums)

#calculate for small and large respectively
cs <- subset(subset_df_binary, subset_df_binary$size == 1)
column_sums2 <- as.data.frame(colSums(cs[, !names(cs) %in% c("AgencyName", "OrgType")]))
print(column_sums2)


#33 adopters
result_df_filled$Q8_12 <- ifelse(is.na(result_df_filled$Q8_12), 0, result_df_filled$Q8_12)
length(result_df_filled$Q67[result_df_filled$Q8_12=="Using Systemwide"|result_df_filled$Q8_12=="Procuring"])
#adopters by size
table(result_df_filled$size[result_df_filled$Q8_12=="Using Systemwide"|result_df_filled$Q8_12=="Procuring"])

