library(dplyr)
library(readxl)
library(writexl)
library(tidyr)
library(ggplot2)
library(sjPlot)

setwd("~/Box Sync/Index blueprint2/Water and Transit Project Notes/Organizational Outreach/Firmographs/Firmographs2022/Survey")

#Update agency information with recent numbers (2022, since our data collection begins here, instead of 2018)
transit_survey <- read_xlsx("~/Downloads/transit_survey.xlsx")
info <- read_xlsx("2021 Agency Information.xlsx")
revenue <- read_xlsx("2021 Funding Sources_static.xlsx", sheet=3)
info <- subset(info, info$State=="CA")
info$`NTD ID` <- gsub("0RO2-", "", info$`NTD ID`)
info$`NTD ID` <- gsub("9R02-", "", info$`NTD ID`)
info$`NTD ID` <- gsub("6R02-", "", info$`NTD ID`)
transit_survey$NTDID <- gsub("9R02-", "", transit_survey$NTDID)
transit_survey$NTDID <- gsub("6R02-", "", transit_survey$NTDID)
setdiff(transit_survey$NTDID, info$`NTD ID`)
info <- subset(info, info$`NTD ID` %in% transit_survey$NTDID)
info$NTDID=info$`NTD ID`
info$VOMS=info$`Total VOMS`
info$pop= info$`Service Area Pop`
info$mile = info$`Service Area Sq Miles`
info <- info %>% select(NTDID, pop, mile, VOMS)
revenue$NTDID = revenue$`NTD ID`
revenue$revenue =revenue$Total
revenue <- revenue %>% select(revenue, NTDID)
transit_survey <- merge(transit_survey, revenue, by.x="NTDID", by.y="NTDID", all.x=TRUE, all.y=FALSE)
transit_survey <- merge(transit_survey, info, by.x="NTDID", by.y="NTDID", all.x=TRUE, all.y=FALSE)

#update agency characteristics based on new NTDIDs (in whichever year we go with)
write_xlsx(transit_survey, "transit_survey.csv")

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
  summarise_at(vars(-one_of(non_numeric_columns)), sum)

# Reshape the data for plotting
aggregated_df_long <- tidyr::gather(aggregated_df, key = "Rationale", value = "Count")
aggregated_df_long <- subset(aggregated_df_long, aggregated_df_long$Rationale !="Other" & 
                               aggregated_df_long$Rationale !="Other - Text")
adf <- subset(aggregated_df_long, aggregated_df_long$Count > 7)

# Plot using ggplot2 with a muted blue color palette
ggplot(adf, aes(x = reorder(Rationale, Count), y = Count, fill = Count)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#b3cde3", high = "#3182bd") +  # Muted blue color palette
  labs(title = "Count of Agencies by Adoption Rationale",
       x = NULL,
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels




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


#NTD ANALYSIS----
#sampling frame
samp_frame <- read_excel("transit_survey.xlsx")
#read in documents
ntd <- read_excel("~/Downloads/Qlik Sense - Summary Transit Organizations - February 6, 2024.xlsx")

#double check what is NOT matching with NTDIDs
intersect(samp_frame$NTDID, ntd$`ID NTD`)
x <- setdiff(ntd$`ID NTD`, samp_frame$NTDID)
nx <- subset(ntd, ntd$`ID NTD` %in% x)

#Some firmographs agencies don't have NTD IDs (that do)
#others don't have NTDIDs at all, and match with cities in our database, but names are different (e.g. Clovis)

###name fixing-----

#NTDID matches
#anywhere match accounts for the 9RO2 prefix in samp_frame
ntd$`ID NTD` <- ifelse(ntd$`ID NTD` =="-", "XX",ntd$`ID NTD` ) #get rid of dash so it doesn't match!
ntd$matched <- ifelse(sapply(ntd$`ID NTD`, function(x) any(grepl(x, samp_frame$NTDID))), 1, 0)

#let's pull over columns from samp_frame so we can do a final merge on name
sf <- samp_frame %>% select(NTDID, AgencyName)
sf$NTDID <- gsub("^9R02-", "", sf$NTDID)
ntd <- merge(ntd, sf, by.x="ID NTD", by.y="NTDID", all.x=TRUE, all.y=FALSE)

#drop , California - perfect name matches
ntd$match_name <- ntd$`Name Parent`
ntd$match_name <- gsub(", California", "", ntd$match_name)
ntd$match_name <- gsub("City of ", "", ntd$match_name)
ntd$matched <- ifelse(ntd$match_name %in% intersect(ntd$match_name, samp_frame$AgencyName), 1, ntd$matched)

#port over the ntd matched names
ntd$AgencyName <- ifelse(ntd$match_name  %in% intersect(ntd$match_name, samp_frame$AgencyName), ntd$match_name, ntd$AgencyName)

#what else is left?
ntd$match_name[ntd$matched==0]
#correct names manually
ntd$AgencyName[ntd$match_name == "Dinuba"] <- "CIty of Dinuba"
ntd$AgencyName[ntd$match_name  == "Clovis"] <- "Clovis - Transit"
ntd$AgencyName[ntd$match_name == "Colusa County"] <- "Colusa County Transit Agency"
ntd$AgencyName[ntd$match_name  ==  "Stanislaus Regional Transit Authority"] <- "Stanislaus Regional Transit"
ntd$AgencyName[ntd$match_name  == "Tulare County Association of Governments"] <- "Tulare County Association of Governments (TCAG)"
length(ntd$AgencyName[!is.na(ntd$AgencyName)])

#automated check to generate where the match_name is anywhere in samp_frame agency name JUST TO KNOW what "probable misses" could be

unmatched <- ntd$match_name[is.na(ntd$AgencyName)]

matched_unmatched <- c()

# Iterate over each value in unmatched
for (value in unmatched) {
  # Check if the value appears anywhere in samp_frame$AgencyName
  if (any(grepl(value, samp_frame$AgencyName, fixed = TRUE))) {
    # If it does, append it to matched_unmatched
    matched_unmatched <- c(matched_unmatched, value)
  }
}

# Print the matched values
print(matched_unmatched)

#the remainder are cities with Public Works boards that may not actually deal with transit
#since I selected boards that could be related to transit and discuss transit, this may include cities that DO NOT ACTUALLY manage transit


##pair w/ mins------

mins <- read_excel("~/Desktop/Qlik Sense - Year Minutes Summary - February 26, 2024.xlsx")
mins <- subset(mins, mins$`2023` > 5)
mins <- mins %>% select(`Name Parent`, `2023`)

ntd2 <- merge(ntd, mins, by.x="Name Parent", by.y="Name Parent", all.x=TRUE, all.y=FALSE)
ntd2 <- subset(ntd2, !(is.na(ntd2$AgencyName)) & !is.na(ntd2$`2023`)) #subset to agencies in our sampling frame and with non NA minutes 
#select only relevant columns, this deletes duplicates too (which arises when one agency appears multiple times in the master list, even though the 
#count minutes are the same (e.g. Fresno,Sacramento Regional Transit District,  Los Angeles County Metropolitan Transportation Authority)
ntd2$minutes_sum <- ntd2$`2023` #for ease of name, the quotes are annoying
ntd3 <- unique(ntd2 %>% select(AgencyName, minutes_sum))

sf <- merge(samp_frame, ntd3, by.x="AgencyName", by.y="AgencyName", all.x=TRUE, all.y=TRUE)
sf$minutes <- ifelse(sf$minutes_sum > 0, 1, 0) #assign binary indicator for minute publication
sf$minutes <- ifelse(is.na(sf$minutes), 0 , sf$minutes)

write.csv(sf, "~/Desktop/firmographs.csv")
##analysis----

#descriptive stats

#composition of minutes total
type1 = subset(sf, sf$minutes==1) %>% group_by(OrgType) %>% summarize(pc = length(unique(AgencyName))/length(sf$AgencyName[sf$minutes==1]))
#percent of each type
type2 = sf %>% group_by(OrgType) %>% summarize(pc = length(unique(AgencyName[minutes ==1]))/length(AgencyName), n= length(AgencyName), mins=length(unique(AgencyName[minutes ==1])))
#size
size = subset(sf, sf$minutes==1)  %>% summarize(VOMS = mean(na.omit(VOMS)), pop = mean(na.omit(pop)), rev=mean(na.omit(revenue)))
#regression
sf$OrgType <- ifelse(sf$OrgType == "Independent Public Agency" , "Ind. Public Agency", sf$OrgType)
sf$OrgType <- relevel(as.factor(sf$OrgType), ref="Local Government")
sf$VOMS <- ifelse(sf$VOMS==0, NA,sf$VOMS)
m1 <-lm(minutes~log(VOMS+1), data = sf)

m2 <-lm(minutes~log(pop), data = sf)

m3 <-lm(minutes~log(revenue), data = sf)

m4<-lm(minutes~log(mile), data = sf)


models <- list(
  "Model 1"     = m1,
  "Model 2"     = m2,
  "Model 3" = m3,
  "Model 4"     = m4
)


model_summary <- modelsummary(models,stars = TRUE, coef_rename = TRUE,vcov="HC2", estimate = c("estimate"),
            fmt = fmt_sprintf("%.3f"),
             output=  "~/Desktop/minutes.html")

model_summary <- modelsummary(models,stars = TRUE, coef_rename = TRUE,vcov="HC2", 
                              fmt = fmt_sprintf("%.3f"),
                              output=  "~/Desktop/minutes.html")


#plot
show_sjplot_pals()

plot_models(m1, m2, m3, show.p = TRUE, show.values=TRUE, p.threshold = c(0.05, 0.01, 0.001) , title="Minutes Publication (Log)",
            vline.color = "red",
            legend.title = "Model")+ scale_color_sjplot(palette = "eight")
plot_models(m4, m5, m6, show.p = TRUE, show.values=TRUE, p.threshold = c(0.05, 0.01, 0.001) , title="Minutes Publication",
            vline.color = "red",
            legend.title = "Model")+ scale_color_sjplot(palette = "eight")
                                                   

