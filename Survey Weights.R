# Load necessary libraries
library(dplyr)
library(survey)
library(ggplot2)

### Read Data ###
# Read the first CSV file
transit_survey_video <- read.csv('Data/transit_survey_with-minute-and-video-counts.csv')

# Create a new column 'post_video' and assign 0
transit_survey_video$post_video <- 0

# Update 'post_video' where 'count - video (past 12 months)' is greater than or equal to 6
transit_survey_video$post_video[transit_survey_video$`count - video (past 12 months)` >= 6] <- 1

# View the first few rows
head(transit_survey_video)

# Create subsets based on survey response
responded <- transit_survey_video %>% filter(survey_response == 1)
no_response <- transit_survey_video %>% filter(survey_response == 0)

# Create subsets based on minute posting behavior
posted <- transit_survey_video %>% filter(minute_posted_6more == 1)
no_posted <- transit_survey_video %>% filter(minute_posted_6more == 0)

# Read the second CSV file
survey_tsp <- read.csv('Data/transit_survey_tsp.csv')

# Create a subset where 'tsp' equals 1
tsp <- survey_tsp %>% filter(tsp == 1)


### Prepare the sample and targeted population ###
# Pre-sample from 'responded' DataFrame (replace 'NTDID', 'pop', 'revenue', 'mile', 'VOMS' with column names from your dataset)
pre_sample <- responded %>% 
  dplyr::select(NTDID, pop, revenue, mile, VOMS) %>% 
  na.omit()

# Apply logarithmic transformation
pre_sample <- pre_sample %>% 
  mutate(across(c(pop, revenue, mile, VOMS), log))

# Pre-target from 'transit_survey_video' DataFrame
pre_target <- transit_survey_video %>% 
  dplyr::select(NTDID, pop, revenue, mile, VOMS, minute_posted_6more) %>% 
  na.omit()

# Apply logarithmic transformation
pre_target <- pre_target %>% 
  mutate(across(c(pop, revenue, mile, VOMS), log))

### Fit the Inverse Probability Weighting (IPW) model ###
# Calculate propensity scores using logistic regression (for IPW)
ps_model <- glm(minute_posted_6more ~ pop + revenue + mile + VOMS, family = binomial(link = "logit"), data = pre_target)

# Extract predicted probabilities (propensity scores)
propensity_scores <- predict(ps_model, type = "response")

# Calculate IPW weights: the inverse of the propensity scores
pre_target$ipw_weights <- ifelse(pre_target$minute_posted_6more == 0, 
                                 1 / propensity_scores, 
                                 1 / (1 - propensity_scores))

# Create a survey design object with IPW weights
ipw_design <- svydesign(id = ~1, weights = ~ipw_weights, data = pre_target)

# Perform a left join to match the ipw_weights from pre_target to pre_sample by NTDID
pre_sample <- merge(pre_sample, 
                    pre_target[, c("NTDID", "ipw_weights")], 
                    by = "NTDID", 
                    all.x = TRUE)

# Check the sum of the IPW weights
sum_ipw_weights <- sum(pre_sample$ipw_weights)
print(sum_ipw_weights)

# If you want to adjust the weights to match the target population size
total_target_population <- nrow(pre_target)
scaling_factor <- total_target_population / sum_ipw_weights

# Adjust the IPW weights
pre_sample$adjusted_ipw_weights <- pre_sample$ipw_weights * scaling_factor

# Check the adjusted weights
sum_adjusted_weights <- sum(pre_sample$adjusted_ipw_weights)
print(sum_adjusted_weights)  # This should equal total_target_population

# Plot covariate balance before and after adjustment (example: pop vs revenue)
ggplot(pre_sample, aes(x = pop, y = revenue, color = as.factor(NTDID))) +
  geom_point(aes(size = adjusted_ipw_weights)) +
  labs(title = "Covariate Balance After IPW Adjustment", 
       x = "Log of Population", 
       y = "Log of Revenue",
       color = "Treatment Group")

### Fit the Raking Model ###
# Step 1: Create consistent quartile breaks based on pre_target
# Create quartiles for covariates in pre_target
quartile_breaks_pop <- quantile(pre_target$pop, probs = seq(0, 1, by = 0.25), na.rm = TRUE)
pre_target$pop_quartile <- cut(pre_target$pop, breaks = quartile_breaks_pop, include.lowest = TRUE)

quartile_breaks_mile <- quantile(pre_target$mile, probs = seq(0, 1, by = 0.25), na.rm = TRUE)
pre_target$mile_quartile <- cut(pre_target$mile, breaks = quartile_breaks_mile, include.lowest = TRUE)

quartile_breaks_VOMS <- quantile(pre_target$VOMS, probs = seq(0, 1, by = 0.25), na.rm = TRUE)
pre_target$VOMS_quartile <- cut(pre_target$VOMS, breaks = quartile_breaks_VOMS, include.lowest = TRUE)

quartile_breaks_revenue <- quantile(pre_target$revenue, probs = seq(0, 1, by = 0.25), na.rm = TRUE)
pre_target$revenue_quartile <- cut(pre_target$revenue, breaks = quartile_breaks_revenue, include.lowest = TRUE)

# Ensure that pre_sample uses the same quartile breaks and convert to character
pre_sample$pop_quartile <- cut(pre_sample$pop, breaks = quartile_breaks_pop, include.lowest = TRUE)
pre_sample$mile_quartile <- cut(pre_sample$mile, breaks = quartile_breaks_mile, include.lowest = TRUE)
pre_sample$VOMS_quartile <- cut(pre_sample$VOMS, breaks = quartile_breaks_VOMS, include.lowest = TRUE)
pre_sample$revenue_quartile <- cut(pre_sample$revenue, breaks = quartile_breaks_revenue, include.lowest = TRUE)

# Drop quartiles from population margins if they don't exist in the sample
pop_margin <- data.frame(table(pre_target$pop_quartile))
mile_margin <- data.frame(table(pre_target$mile_quartile))
VOMS_margin <- data.frame(table(pre_target$VOMS_quartile))
revenue_margin <- data.frame(table(pre_target$revenue_quartile))

# Rename the columns
colnames(pop_margin) <- c("pop_quartile", "Freq")
colnames(mile_margin) <- c("mile_quartile", "Freq")
colnames(VOMS_margin) <- c("VOMS_quartile", "Freq")
colnames(revenue_margin) <- c("revenue_quartile", "Freq")

# Step 2: Create a survey design object for raking
pre_sample$initial_weight <- 1  # Assume initial weights are 1 if not provided
raking_design <- svydesign(id = ~1, weights = ~initial_weight, data = pre_sample)

# Step 3: Perform raking using the population margins
raking_design <- rake(raking_design, 
                      sample.margins = list(~pop_quartile, ~mile_quartile, ~VOMS_quartile, ~revenue_quartile), 
                      population.margins = list(pop_margin, mile_margin, VOMS_margin, revenue_margin))

# Step 4: Print a summary of the raked design
# Extract the raked weights
pre_sample$raked_weight <- weights(raking_design)

# Plot covariate balance before and after adjustment (example: pop vs revenue)
ggplot(pre_sample, aes(x = pop, y = revenue, color = as.factor(NTDID))) +
  geom_point(aes(size = raked_weight)) +
  labs(title = "Covariate Balance After IPW Adjustment", 
       x = "Log of Population", 
       y = "Log of Revenue",
       color = "Treatment Group")

### Fit the CBPS model ###
# minute_posted_6more is the treatment indicator and you're balancing based on the covariates pop, revenue, mile, VOMS
cbps_model <- CBPS(minute_posted_6more ~ pop + revenue + mile + VOMS, 
                   data = pre_target, 
                   method = "exact")

# Print a summary of the adjusted model
summary(cbps_model)

# Extract the adjusted weights from the CBPS model
pre_target$cbps_weights <- cbps_model$weights

pre_sample <- merge(pre_sample, 
                    pre_target[, c("NTDID", "cbps_weights")], 
                    by = "NTDID", 
                    all.x = TRUE)

# Save the pre_sample DataFrame to a CSV file
write.csv(pre_sample, file = "Data/pre_sample_weight.csv", row.names = FALSE)

