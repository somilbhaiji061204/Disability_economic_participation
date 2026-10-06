library(readxl)
raw_data <- read_excel(file.choose(), col_names = FALSE)
install.packages("readxl")
library(readxl)
raw_data <- read_excel(file.choose(), col_names = FALSE)
dim(raw_data)
head(raw_data, 10)
# Remove Census header rows
data <- raw_data[-(1:7), ]
dim(data)
data <- raw_data[-(1:7), ]
dim(data)
head(data, 5)
names(data) <- c(
  "Table_Code",
  "State_Code",
  "District_Code",
  "Area_Name",
  "Area_Type",
  "Disability_Type",
  "Age_Group",
  "Disabled_Persons",
  "Disabled_Males",
  "Disabled_Females",
  "Main_Worker_Persons",
  "Main_Worker_Males",
  "Main_Worker_Females",
  "Marginal_Worker_Less_3_Months_Persons",
  "Marginal_Worker_Less_3_Months_Males",
  "Marginal_Worker_Less_3_Months_Females",
  "Marginal_Worker_3_6_Months_Persons",
  "Marginal_Worker_3_6_Months_Males",
  "Marginal_Worker_3_6_Months_Females",
  "Non_Worker_Persons",
  "Non_Worker_Males",
  "Non_Worker_Females"
)
names(data)
sapply(data, class)
summary(data)
unique(data$Area_Type)
unique(data$Disability_Type)
unique(data$Age_Group)
unique(data$State_Code)
numeric_cols <- 8:22

data[numeric_cols] <- lapply(data[numeric_cols], as.numeric)
sapply(data[8:22], class)
numeric_cols <- 8:22
numeric_cols <- 8:22

data[numeric_cols] <- lapply(data[numeric_cols], as.numeric)
sapply(data[8:22], class)
sum(is.na(data[8:22]))
sum(duplicated(data))
sum(data$Disabled_Persons != data$Disabled_Males + data$Disabled_Females)
c(
  Main_Workers = sum(data$Main_Worker_Persons != 
                       data$Main_Worker_Males + data$Main_Worker_Females),
  
  Marginal_Less_3_Months = sum(data$Marginal_Worker_Less_3_Months_Persons != 
                                 data$Marginal_Worker_Less_3_Months_Males + 
                                 data$Marginal_Worker_Less_3_Months_Females),
  
  Marginal_3_6_Months = sum(data$Marginal_Worker_3_6_Months_Persons != 
                              data$Marginal_Worker_3_6_Months_Males + 
                              data$Marginal_Worker_3_6_Months_Females),
  
  Non_Workers = sum(data$Non_Worker_Persons != 
                      data$Non_Worker_Males + data$Non_Worker_Females)
)
table(data$Area_Type)
table(data$Disability_Type)
table(data$Age_Group)
head(data[, 1:7], 30)
age_analysis <- data[
  data$Area_Type == "Total" &
    data$Disability_Type == "Total" &
    data$Age_Group %in% c("0-14", "15-59", "60+"),
]
age_analysis <- data[
  data$Area_Type == "Total" &
    data$Disability_Type == "Total" &
    data$Age_Group %in% c("0-14", "15-59", "60+"),
]
age_analysis[, 1:7]
age_analysis[, c(
  "Area_Name",
  "Age_Group",
  "Disabled_Persons",
  "Main_Worker_Persons",
  "Marginal_Worker_Less_3_Months_Persons",
  "Marginal_Worker_3_6_Months_Persons",
  "Non_Worker_Persons"
)]
age_analysis$Marginal_Workers <- 
  age_analysis$Marginal_Worker_Less_3_Months_Persons +
  age_analysis$Marginal_Worker_3_6_Months_Persons
head(age_analysis[, c(
  "Area_Name",
  "Age_Group",
  "Main_Worker_Persons",
  "Marginal_Workers"
)])
age_analysis$Economic_Participation_Rate <-
  (age_analysis$Main_Worker_Persons +
     age_analysis$Marginal_Workers) /
  age_analysis$Disabled_Persons * 100
head(age_analysis[, c(
  "Area_Name",
  "Age_Group",
  "Disabled_Persons",
  "Main_Worker_Persons",
  "Marginal_Workers",
  "Economic_Participation_Rate"
)])
id="2j7w9q"
age_analysis[
  age_analysis$Area_Name == "INDIA",
  c("Age_Group", "Disabled_Persons", "Main_Worker_Persons",
    "Marginal_Workers", "Economic_Participation_Rate")
]
age_analysis[
  age_analysis$Area_Name == "INDIA",
  c("Age_Group", "Economic_Participation_Rate")
]
# Calculate the average economic participation rate across States/UTs
# for each age group to examine variation beyond the India-level total.

aggregate(
  Economic_Participation_Rate ~ Age_Group,
  data = age_analysis,
  FUN = mean
)
# Create a total economic participation count by combining
# main workers and both categories of marginal workers.

age_analysis$Total_Workers <-
  age_analysis$Main_Worker_Persons +
  age_analysis$Marginal_Workers
# Check the calculated total worker counts for India.

age_analysis[
  age_analysis$Area_Name == "INDIA",
  c("Age_Group", "Main_Worker_Persons",
    "Marginal_Workers", "Total_Workers")
]
# Review economic participation counts and rates by age group
# for India as a basic validation of the calculated indicators.

age_analysis[
  age_analysis$Area_Name == "INDIA",
  c(
    "Age_Group",
    "Disabled_Persons",
    "Total_Workers",
    "Economic_Participation_Rate"
  )
]
# Calculate the minimum and maximum economic participation rates
# across States/UTs for each age group to assess geographic variation.

aggregate(
  Economic_Participation_Rate ~ Age_Group,
  data = age_analysis,
  FUN = function(x) c(
    Minimum = min(x),
    Maximum = max(x)
  )
)
# Create a simple India-level dataset for visualising
# economic participation by age group.

india_age_rates <- age_analysis[
  age_analysis$Area_Name == "INDIA",
  c("Age_Group", "Economic_Participation_Rate")
]

# Display the newly created dataset
print(india_age_rates)
# Visualise economic participation rates among persons with disabilities
# across the three age groups at the India level.

barplot(
  india_age_rates$Economic_Participation_Rate,
  names.arg = india_age_rates$Age_Group,
  ylab = "Economic Participation Rate (%)",
  xlab = "Age Group",
  main = "Economic Participation by Age Group in India"
)
# Create the India-level age-group graph with percentage labels
# to make the participation rates directly readable.

bar_positions <- barplot(
  india_age_rates$Economic_Participation_Rate,
  names.arg = india_age_rates$Age_Group,
  ylab = "Economic Participation Rate (%)",
  xlab = "Age Group",
  main = "Economic Participation by Age Group in India"
)

text(
  x = bar_positions,
  y = india_age_rates$Economic_Participation_Rate,
  labels = paste0(
    round(india_age_rates$Economic_Participation_Rate, 2),
    "%"
  ),
  pos = 3
)
# Create the India-level age-group graph with percentage labels
# and extra space above the bars so all labels are visible.

bar_positions <- barplot(
  india_age_rates$Economic_Participation_Rate,
  names.arg = india_age_rates$Age_Group,
  ylab = "Economic Participation Rate (%)",
  xlab = "Age Group",
  main = "Economic Participation by Age Group in India",
  ylim = c(0, 55)
)

text(
  x = bar_positions,
  y = india_age_rates$Economic_Participation_Rate,
  labels = paste0(
    round(india_age_rates$Economic_Participation_Rate, 2),
    "%"
  ),
  pos = 3
)
# Calculate the average economic participation rate across States/UTs
# for each age group to examine geographic variation in participation.

state_age_mean <- aggregate(
  Economic_Participation_Rate ~ Age_Group,
  data = age_analysis,
  FUN = mean
)

state_age_mean
# Summarise variation in economic participation rates across States/UTs
# for each age group using the mean, median, and standard deviation.

state_age_summary <- aggregate(
  Economic_Participation_Rate ~ Age_Group,
  data = age_analysis,
  FUN = function(x) c(
    Mean = mean(x),
    Median = median(x),
    SD = sd(x)
  )
)

state_age_summary
# Check the number of State/UT observations available
# for each age group before conducting comparative analysis.

table(age_analysis$Age_Group)
# Reshape the State/UT-level data so that each State/UT has
# one economic participation rate for each age group.
# This allows us to compare age groups within the same States/UTs.

state_age_wide <- reshape(
  age_analysis[, c("Area_Name", "Age_Group", "Economic_Participation_Rate")],
  idvar = "Area_Name",
  timevar = "Age_Group",
  direction = "wide"
)

state_age_wide
# Reshape the data so each State/UT has a separate participation-rate
# column for each age group: 0-14, 15-59, and 60+.

state_age_wide <- reshape(
  age_analysis[
    age_analysis$Area_Name != "INDIA",
    c("Area_Name", "Age_Group", "Economic_Participation_Rate")
  ],
  idvar = "Area_Name",
  timevar = "Age_Group",
  direction = "wide"
)

state_age_wide
# Create a State/UT-level table with one economic participation rate
# column for each age group.

state_age_wide <- data.frame(
  Area_Name = unique(age_analysis$Area_Name[age_analysis$Area_Name != "INDIA"])
)

state_age_wide$Rate_0_14 <- age_analysis$Economic_Participation_Rate[
  age_analysis$Age_Group == "0-14" &
    age_analysis$Area_Name != "INDIA"
]

state_age_wide$Rate_15_59 <- age_analysis$Economic_Participation_Rate[
  age_analysis$Age_Group == "15-59" &
    age_analysis$Area_Name != "INDIA"
]

state_age_wide$Rate_60_plus <- age_analysis$Economic_Participation_Rate[
  age_analysis$Age_Group == "60+" &
    age_analysis$Area_Name != "INDIA"
]

state_age_wide
# Calculate the change in economic participation between age groups
# for each Census State/UT.

state_age_wide$Change_15_59_vs_0_14 <-
  state_age_wide$Rate_15_59 - state_age_wide$Rate_0_14

state_age_wide$Change_60_plus_vs_15_59 <-
  state_age_wide$Rate_60_plus - state_age_wide$Rate_15_59

state_age_wide
# Summarize how much economic participation changes from age 15–59
# to age 60+ across the 35 Census State/UTs.

summary(state_age_wide$Change_60_plus_vs_15_59)
# Identify the State/UTs with the largest and smallest decline
# in economic participation between ages 15–59 and 60+.

state_age_wide[
  which.min(state_age_wide$Change_60_plus_vs_15_59),
  c("Area_Name", "Change_60_plus_vs_15_59")
]

state_age_wide[
  which.max(state_age_wide$Change_60_plus_vs_15_59),
  c("Area_Name", "Change_60_plus_vs_15_59")
]
# Prepare the State/UT-by-age dataset for count-based statistical analysis.
# We exclude the India aggregate and retain the worker and population counts.

age_model_data <- age_analysis[
  age_analysis$Area_Name != "INDIA",
  c(
    "Area_Name",
    "Age_Group",
    "Disabled_Persons",
    "Main_Worker_Persons",
    "Marginal_Workers",
    "Total_Workers"
  )
]

age_model_data
# Check whether total workers plus non-workers equals the total
# disabled population for every State/UT and age group.

age_model_data$Non_Worker_Persons <-
  age_analysis$Non_Worker_Persons[
    age_analysis$Area_Name != "INDIA"
  ]

sum(
  age_model_data$Total_Workers +
    age_model_data$Non_Worker_Persons !=
    age_model_data$Disabled_Persons
)
# Create the worker and non-worker counts that will be used as the
# outcome in the count-based economic participation model.

age_model_data$Non_Workers <-
  age_model_data$Disabled_Persons -
  age_model_data$Total_Workers

head(age_model_data)
# Model economic participation by age group while controlling
# for differences between State/UTs.

age_model <- glm(
  cbind(Total_Workers, Non_Workers) ~ Age_Group + Area_Name,
  family = quasibinomial(),
  data = age_model_data
)

summary(age_model)
# Convert the age-group coefficients from log-odds into
# odds ratios, which are easier to interpret.

exp(coef(age_model)[c("Age_Group15-59", "Age_Group60+")])
# Calculate the overall economic participation rate for each
# age group across the 35 State/UTs.

age_rate_summary <- aggregate(
  cbind(Total_Workers, Non_Workers) ~ Age_Group,
  data = age_model_data,
  FUN = sum
)

age_rate_summary$Economic_Participation_Rate <-
  age_rate_summary$Total_Workers /
  (age_rate_summary$Total_Workers +
     age_rate_summary$Non_Workers) * 100

age_rate_summary
# Calculate the odds ratio for economic participation among
# older adults (60+) compared with working-age adults (15–59).

exp(
  coef(age_model)["Age_Group60+"] -
    coef(age_model)["Age_Group15-59"]
)
# Check how much overdispersion is present in the model.

dispersion_ratio <- deviance(age_model) / df.residual(age_model)

dispersion_ratio
# Look at the distribution of economic participation rates
# across State/UTs for each age group.

aggregate(
  Economic_Participation_Rate ~ Age_Group,
  data = age_analysis[age_analysis$Area_Name != "INDIA", ],
  FUN = function(x) c(
    Minimum = min(x),
    Median = median(x),
    Mean = mean(x),
    Maximum = max(x),
    SD = sd(x)
  )
)
# Compare the economic participation rates across age groups
# for each State/UT.

head(
  state_age_wide[
    order(state_age_wide$Rate_15_59, decreasing = TRUE),
    c("Area_Name", "Rate_0_14", "Rate_15_59", "Rate_60_plus")
  ],
  10
)
# Examine the relationship between economic participation
# in the 15–59 and 60+ age groups across State/UTs.

cor.test(
  state_age_wide$Rate_15_59,
  state_age_wide$Rate_60_plus,
  method = "spearman"
)
# Install the package needed for beta-binomial regression.
# Run this only once on your computer.

install.packages("glmmTMB")
# Load the package so we can use the beta-binomial model.

library(glmmTMB)
# Fit a beta-binomial regression with age group and State/UT.
# This allows more variability than the ordinary binomial model.

age_model_betabinom <- glmmTMB(
  cbind(Total_Workers, Non_Workers) ~ Age_Group + Area_Name,
  family = betabinomial(link = "logit"),
  data = age_model_data
)

summary(age_model_betabinom)
# Convert the age-group coefficients into odds ratios.
# The reference category is 0–14 years.

exp(
  fixef(age_model_betabinom)$cond[
    c("Age_Group15-59", "Age_Group60+")
  ]
)
# Calculate the odds ratio for economic participation
# among persons aged 60+ compared with those aged 15–59.

exp(
  fixef(age_model_betabinom)$cond["Age_Group60+"] -
    fixef(age_model_betabinom)$cond["Age_Group15-59"]
)
# Extract the 60+ versus 15–59 comparison
# with its 95% confidence interval and p-value.

model_coef <- summary(age_model_betabinom)$coefficients$cond

beta_difference <- model_coef["Age_Group60+", "Estimate"] -
  model_coef["Age_Group15-59", "Estimate"]

se_difference <- sqrt(
  model_coef["Age_Group60+", "Std. Error"]^2 +
    model_coef["Age_Group15-59", "Std. Error"]^2
)

z_value <- beta_difference / se_difference

p_value <- 2 * pnorm(-abs(z_value))

OR <- exp(beta_difference)

CI_lower <- exp(beta_difference - 1.96 * se_difference)
CI_upper <- exp(beta_difference + 1.96 * se_difference)

c(
  Odds_Ratio = OR,
  CI_Lower = CI_lower,
  CI_Upper = CI_upper,
  P_Value = p_value
)
# Inspect the columns needed for the sex-based analysis
# This helps us verify the Census variables before calculating rates.

data[, c(
  "Area_Name",
  "Area_Type",
  "Disability_Type",
  "Age_Group",
  "Disabled_Males",
  "Disabled_Females",
  "Main_Worker_Males",
  "Main_Worker_Females",
  "Marginal_Worker_Less_3_Months_Males",
  "Marginal_Worker_Less_3_Months_Females",
  "Marginal_Worker_3_6_Months_Males",
  "Marginal_Worker_3_6_Months_Females",
  "Non_Worker_Males",
  "Non_Worker_Females"
)]
# Create the dataset for comparing economic participation by sex.
# We use Total area, Total disability type, and Total age.

sex_analysis <- data[
  data$Area_Type == "Total" &
    data$Disability_Type == "Total" &
    data$Age_Group == "Total",
  c(
    "Area_Name",
    "Disabled_Males",
    "Disabled_Females",
    "Main_Worker_Males",
    "Main_Worker_Females",
    "Marginal_Worker_Less_3_Months_Males",
    "Marginal_Worker_Less_3_Months_Females",
    "Marginal_Worker_3_6_Months_Males",
    "Marginal_Worker_3_6_Months_Females",
    "Non_Worker_Males",
    "Non_Worker_Females"
  )
]

dim(sex_analysis)
# Calculate economic participation rates separately for males and females.
# This tells us what percentage of disabled males and females are economically active.

sex_analysis$Participation_Rate_Males <-
  sex_analysis$Total_Workers_Males /
  sex_analysis$Disabled_Males * 100

sex_analysis$Participation_Rate_Females <-
  sex_analysis$Total_Workers_Females /
  sex_analysis$Disabled_Females * 100 
# Check the economic participation rate of disabled males and females in India.
# This gives us the basic descriptive comparison before statistical testing.

sex_analysis[
  sex_analysis$Area_Name == "INDIA",
  c(
    "Area_Name",
    "Participation_Rate_Males",
    "Participation_Rate_Females"
  )
]
# Check the underlying India counts used to calculate participation rates.
# This helps us identify why the rates are not being displayed.

sex_analysis[
  sex_analysis$Area_Name == "INDIA",
  c(
    "Disabled_Males",
    "Disabled_Females",
    "Total_Workers_Males",
    "Total_Workers_Females",
    "Participation_Rate_Males",
    "Participation_Rate_Females"
  )
]
# Display all India-level values clearly.
# This lets us see the male and female participation rates.

print(
  sex_analysis[
    sex_analysis$Area_Name == "INDIA",
    c(
      "Disabled_Males",
      "Disabled_Females",
      "Total_Workers_Males",
      "Total_Workers_Females",
      "Participation_Rate_Males",
      "Participation_Rate_Females"
    )
  ],
  width = Inf
)
# Calculate the number of non-workers separately for males and females.
# These counts will be used as the second outcome in the beta-binomial model.

sex_analysis$Non_Workers_Males <-
  sex_analysis$Disabled_Males -
  sex_analysis$Total_Workers_Males

sex_analysis$Non_Workers_Females <-
  sex_analysis$Disabled_Females -
  sex_analysis$Total_Workers_Females
# Check that workers + non-workers equals the total disabled population.
# A result of 0 for both checks means the counts are internally consistent.

sum(
  sex_analysis$Total_Workers_Males +
    sex_analysis$Non_Workers_Males !=
    sex_analysis$Disabled_Males
)

sum(
  sex_analysis$Total_Workers_Females +
    sex_analysis$Non_Workers_Females !=
    sex_analysis$Disabled_Females
)
# Create a long-format dataset with one row for each sex within each State/UT.
# This allows the beta-binomial model to directly compare males and females.

sex_model_data <- rbind(
  data.frame(
    Area_Name = sex_analysis$Area_Name,
    Sex = "Male",
    Total_Workers = sex_analysis$Total_Workers_Males,
    Non_Workers = sex_analysis$Non_Workers_Males
  ),
  
  data.frame(
    Area_Name = sex_analysis$Area_Name,
    Sex = "Female",
    Total_Workers = sex_analysis$Total_Workers_Females,
    Non_Workers = sex_analysis$Non_Workers_Females
  )
)

sex_model_data$Sex <- factor(sex_model_data$Sex)
# Check the size and structure of the sex-analysis dataset.
# We expect 72 rows: 36 locations × 2 sexes.

dim(sex_model_data)

table(sex_model_data$Sex)
# Fit a beta-binomial regression for economic participation by sex.
# State/UT is included to account for geographic differences in participation.

sex_model_betabinom <- glmmTMB(
  cbind(Total_Workers, Non_Workers) ~ Sex + Area_Name,
  family = betabinomial(link = "logit"),
  data = sex_model_data
)

summary(sex_model_betabinom)
# Convert the male coefficient from log-odds to an odds ratio.
# Female is the reference group, so this compares males with females.

exp(
  fixef(sex_model_betabinom)$cond["SexMale"]
)
# Remove INDIA from the sex model so that the analysis uses only the 35 State/UTs.
# INDIA is an aggregate of these units and should not be treated as an additional independent area.

sex_model_data_state <- sex_model_data[
  sex_model_data$Area_Name != "INDIA",
]

dim(sex_model_data_state)
# Refit the beta-binomial model using only the 35 State/UTs.
# This estimates the association between sex and economic participation
# while accounting for geographic differences across the State/UTs.

sex_model_betabinom_state <- glmmTMB(
  cbind(Total_Workers, Non_Workers) ~ Sex + Area_Name,
  family = betabinomial(link = "logit"),
  data = sex_model_data_state
)

summary(sex_model_betabinom_state)
# Convert the male coefficient from log-odds to an odds ratio.
# Female is the reference group, so this gives the odds of
# economic participation for males relative to females.

exp(
  fixef(sex_model_betabinom_state)$cond["SexMale"]
)
# Calculate the 95% confidence interval for the sex odds ratio.
# We calculate the interval on the log-odds scale and then exponentiate it.

sex_coef <- fixef(sex_model_betabinom_state)$cond["SexMale"]
sex_se <- sqrt(
  vcov(sex_model_betabinom_state)$cond["SexMale", "SexMale"]
)

sex_CI <- exp(
  sex_coef + c(-1, 1) * 1.96 * sex_se
)

sex_CI
# Create the rural–urban analysis dataset.
# We keep the overall disability and age categories,
# but retain only Rural and Urban areas.

area_analysis <- data[
  data$Area_Type %in% c("Rural", "Urban") &
    data$Disability_Type == "Total" &
    data$Age_Group == "Total",
  c(
    "Area_Name",
    "Area_Type",
    "Disabled_Persons",
    "Main_Worker_Persons",
    "Marginal_Worker_Less_3_Months_Persons",
    "Marginal_Worker_3_6_Months_Persons",
    "Non_Worker_Persons"
  )
]

dim(area_analysis)
# Calculate total workers by adding main and both categories of marginal workers.
# Then calculate the economic participation rate among persons with disabilities.

area_analysis$Total_Workers <-
  area_analysis$Main_Worker_Persons +
  area_analysis$Marginal_Worker_Less_3_Months_Persons +
  area_analysis$Marginal_Worker_3_6_Months_Persons

area_analysis$Economic_Participation_Rate <-
  area_analysis$Total_Workers /
  area_analysis$Disabled_Persons * 100

head(area_analysis)
# Check that total workers plus non-workers equals the total disabled population.
# This confirms that our worker/non-worker counts are internally consistent.

sum(
  area_analysis$Total_Workers +
    area_analysis$Non_Worker_Persons !=
    area_analysis$Disabled_Persons
)
# Extract the India-level Rural and Urban participation rates.
# This gives us the national descriptive comparison before analysing State/UTs.

india_area_rates <- area_analysis[
  area_analysis$Area_Name == "INDIA",
  c("Area_Type", "Economic_Participation_Rate")
]

india_area_rates
# Compare Rural and Urban participation rates across the 35 State/UTs.
# INDIA is excluded because it is a national aggregate, not a separate State/UT.

state_area_rates <- area_analysis[
  area_analysis$Area_Name != "INDIA",
  c("Area_Name", "Area_Type", "Economic_Participation_Rate")
]

head(state_area_rates)
# Calculate the difference between Rural and Urban participation rates
# Positive values = Rural participation is higher
# Negative values = Urban participation is higher

state_area_wide <- reshape(
  state_area_rates,
  idvar = "Area_Name",
  timevar = "Area_Type",
  direction = "wide"
)

state_area_wide$Rural_Urban_Difference <-
  state_area_wide$Economic_Participation_Rate.Rural -
  state_area_wide$Economic_Participation_Rate.Urban

head(state_area_wide)
# Put Rural and Urban participation rates into separate columns
# so that each State/UT has one row for direct comparison.

library(tidyr)

state_area_wide <- pivot_wider(
  state_area_rates,
  names_from = Area_Type,
  values_from = Economic_Participation_Rate
)

head(state_area_wide)
# Calculate the Rural–Urban participation gap for each State/UT
# Positive = Rural participation is higher
# Negative = Urban participation is higher

state_area_wide$Rural_Urban_Difference <-
  state_area_wide$Rural - state_area_wide$Urban

head(state_area_wide)
# Summarize the Rural–Urban participation gap across all 35 State/UTs
# This shows the smallest, typical, and largest rural–urban differences.

summary(state_area_wide$Rural_Urban_Difference)
# Sort State/UTs by the Rural–Urban participation gap
# This lets us identify where the rural advantage is largest.

state_area_wide[
  order(-state_area_wide$Rural_Urban_Difference),
  c("Area_Name", "Rural", "Urban", "Rural_Urban_Difference")
]
# Create a dataset containing Rural and Urban worker/non-worker counts
# for each State/UT. India is excluded because it is an aggregate
# of the State/UTs and should not be treated as an independent observation.

area_model_data <- area_analysis[
  area_analysis$Area_Name != "INDIA",
  c(
    "Area_Name",
    "Area_Type",
    "Total_Workers",
    "Non_Worker_Persons"
  )
]

head(area_model_data)
# Check that workers + non-workers equals the total disabled population
# for every Rural/Urban State/UT observation.

sum(
  area_model_data$Total_Workers +
    area_model_data$Non_Worker_Persons !=
    area_analysis$Disabled_Persons[
      area_analysis$Area_Name != "INDIA"
    ]
)
# Model economic participation by Rural/Urban residence
# while accounting for differences between State/UTs.
# The beta-binomial model allows for extra variation in these
# aggregated Census counts.

area_model_betabinom <- glmmTMB(
  cbind(Total_Workers, Non_Worker_Persons) ~ Area_Type + Area_Name,
  family = betabinomial(link = "logit"),
  data = area_model_data
)

summary(area_model_betabinom)
# Convert the Urban regression coefficient from log-odds
# into an odds ratio that is easier to interpret.

area_coef <- fixef(area_model_betabinom)$cond["Area_TypeUrban"]

area_OR <- exp(area_coef)

area_OR
# Calculate the 95% confidence interval for the Rural–Urban odds ratio
# using the standard error of the Urban coefficient.

area_se <- sqrt(
  vcov(area_model_betabinom)$cond[
    "Area_TypeUrban",
    "Area_TypeUrban"
  ]
)

area_CI <- exp(
  area_coef + c(-1, 1) * 1.96 * area_se
)

area_CI
# Create the disability-type analysis dataset
# We keep only specific disability categories and exclude the overall "Total"
# so that the categories are not mixed with their aggregate.

disability_analysis <- data[
  data$Area_Type == "Total" &
    data$Age_Group == "Total" &
    data$Disability_Type != "Total",
  c(
    "Area_Name",
    "Disability_Type",
    "Disabled_Persons",
    "Main_Worker_Persons",
    "Marginal_Worker_Less_3_Months_Persons",
    "Marginal_Worker_3_6_Months_Persons",
    "Non_Worker_Persons"
  )
]

dim(disability_analysis)
# Calculate total workers by adding main workers and both marginal-worker categories.
# This gives us the total number of economically participating persons with disabilities.

disability_analysis$Total_Workers <-
  disability_analysis$Main_Worker_Persons +
  disability_analysis$Marginal_Worker_Less_3_Months_Persons +
  disability_analysis$Marginal_Worker_3_6_Months_Persons


# Calculate the economic participation rate.
# The denominator is the total disabled population for that disability type.

disability_analysis$Economic_Participation_Rate <-
  disability_analysis$Total_Workers /
  disability_analysis$Disabled_Persons * 100
# Check the disability type, total disabled population,
# total workers, and calculated economic participation rate.

head(
  disability_analysis[
    ,
    c(
      "Area_Name",
      "Disability_Type",
      "Disabled_Persons",
      "Total_Workers",
      "Economic_Participation_Rate"
    )
  ],
  10
)
# Select India's data only.
# This lets us compare economic participation across disability types
# at the national level without mixing in State/UT observations.

india_disability_rates <- disability_analysis[
  disability_analysis$Area_Name == "INDIA",
  c(
    "Disability_Type",
    "Disabled_Persons",
    "Total_Workers",
    "Economic_Participation_Rate"
  )
]

india_disability_rates
# Display India's economic participation rate for each disability type.
# This gives us the main descriptive comparison for the national analysis.

india_disability_rates[
  ,
  c("Disability_Type", "Economic_Participation_Rate")
]
# Remove the India aggregate so that only the 35 State/UTs
# are used for the State-level descriptive analysis.

state_disability_rates <- disability_analysis[
  disability_analysis$Area_Name != "INDIA",
  c(
    "Area_Name",
    "Disability_Type",
    "Economic_Participation_Rate"
  )
]

dim(state_disability_rates)
# Summarise economic participation across the 35 State/UTs
# for each disability type.
# This shows the typical level and the geographic variation.

disability_type_summary <- aggregate(
  Economic_Participation_Rate ~ Disability_Type,
  data = state_disability_rates,
  FUN = function(x) c(
    Mean = mean(x),
    Median = median(x),
    SD = sd(x),
    Minimum = min(x),
    Maximum = max(x)
  )
)

disability_type_summary
# Create a clean ranking using the existing mean column.
# We sort the disability types from the highest to the lowest
# mean economic participation rate across State/UTs.

disability_type_summary[
  order(-disability_type_summary$Economic_Participation_Rate.Mean),
  c(
    "Disability_Type",
    "Economic_Participation_Rate.Mean"
  )
]
# Check the exact column names currently stored in the summary table.
# This helps us identify why R is not recognising the column selection.

names(disability_type_summary)
# Create a simple table containing the mean participation rate
# for each disability type across the 35 State/UTs.

disability_mean_rates <- aggregate(
  Economic_Participation_Rate ~ Disability_Type,
  data = state_disability_rates,
  FUN = mean
)

disability_mean_rates
# Rank disability types from the highest to the lowest
# mean economic participation rate across the 35 State/UTs.

disability_mean_rates[
  order(-disability_mean_rates$Economic_Participation_Rate),
]
# Prepare the data for the beta-binomial model.
# We exclude INDIA because it is an aggregate of the 35 State/UTs,
# not an independent geographic observation.

disability_model_data <- disability_analysis[
  disability_analysis$Area_Name != "INDIA",
  c(
    "Area_Name",
    "Disability_Type",
    "Total_Workers",
    "Non_Worker_Persons"
  )
]

dim(disability_model_data)
# Fit a beta-binomial model to examine whether economic participation
# differs by disability type after accounting for State/UT.
#
# Workers and non-workers are entered as grouped counts rather than
# treating each Census row as an individual observation.

library(glmmTMB)

disability_model_betabinom <- glmmTMB(
  cbind(Total_Workers, Non_Worker_Persons) ~
    Disability_Type + Area_Name,
  family = betabinomial(link = "logit"),
  data = disability_model_data
)

summary(disability_model_betabinom)
# Check whether the beta-binomial model has any convergence
# or fitting problems before reporting its results.

diagnose(disability_model_betabinom)
# Check the convergence status of the fitted beta-binomial model.
# We want to confirm whether glmmTMB considers the optimization successful.

disability_model_betabinom$fit$convergence
# Check the optimizer message returned when fitting the model.

disability_model_betabinom$fit$message
# Check whether the age beta-binomial model converged successfully.
age_model_betabinom$fit$convergence
age_model_betabinom$fit$message
# Check whether the sex beta-binomial model converged successfully.
sex_model_betabinom_state$fit$convergence
sex_model_betabinom_state$fit$message
# Check whether the rural–urban beta-binomial model converged successfully.
area_model_betabinom$fit$convergence
area_model_betabinom$fit$message
# Fit a standard binomial model to the grouped Census counts.
# This provides a baseline model that we can diagnose for overdispersion
# before deciding whether a more complex model is necessary.

age_model_binomial <- glm(
  cbind(Total_Workers, Non_Workers) ~ Age_Group + Area_Name,
  family = binomial(link = "logit"),
  data = age_model_data
)
# Check the standard binomial model for overdispersion.
# A value substantially greater than 1 indicates extra variation
# beyond what the ordinary binomial model expects.

summary(age_model_binomial)$dispersion
# Check for overdispersion in the ordinary binomial model.
# A ratio close to 1 suggests the binomial variance assumption is reasonable;
# a substantially larger value suggests overdispersion.

age_overdispersion <- 
  deviance(age_model_binomial) /
  df.residual(age_model_binomial)

age_overdispersion
# Fit a sim04_model_diagnostics.Rpler binomial model using age group only.
# This helps us see how much of the extra variation is associated
# with differences between State/UTs.

age_model_binomial_simple <- glm(
  cbind(Total_Workers, Non_Workers) ~ Age_Group,
  family = binomial(link = "logit"),
  data = age_model_data
)
# Calculate the overdispersion ratio for the simpler model.

deviance(age_model_binomial_simple) /
  df.residual(age_model_binomial_simple)
# ---------------------------------------------------------
# Chi-square test: Sex and economic participation
# ---------------------------------------------------------
# We test whether worker status differs between males and females
# using the national Census counts.

india <- sex_analysis$Area_Name == "INDIA"

sex_chi_table <- matrix(
  c(
    sum(
      sex_analysis$Main_Worker_Males[india],
      sex_analysis$Marginal_Worker_Less_3_Months_Males[india],
      sex_analysis$Marginal_Worker_3_6_Months_Males[india]
    ),
    sex_analysis$Non_Worker_Males[india],
    
    sum(
      sex_analysis$Main_Worker_Females[india],
      sex_analysis$Marginal_Worker_Less_3_Months_Females[india],
      sex_analysis$Marginal_Worker_3_6_Months_Females[india]
    ),
    sex_analysis$Non_Worker_Females[india]
  ),
  nrow = 2,
  byrow = TRUE,
  dimnames = list(
    c("Male", "Female"),
    c("Workers", "Non_Workers")
  )
)

sex_chi_table

# Perform the chi-square test of association.
sex_chi_test <- chisq.test(sex_chi_table)

sex_chi_test
# Calculate Cramér's V to measure the strength of the association.
# This is important because the chi-square p-value is strongly affected
# by the very large Census population size.

library(effectsize)

sex_cramers_v <- cramers_v(sex_chi_table)

sex_cramers_v
# Install the package needed to calculate Cramér's V.
install.packages("effectsize")
library(effectsize)
# Calculate the strength of the association between sex and worker status.
sex_cramers_v <- cramers_v(sex_chi_table)

sex_cramers_v
# ---------------------------------------------------------
# Chi-square test: Age group and economic participation
# ---------------------------------------------------------
# We test whether economic participation differs across the
# three main age groups: 0–14, 15–59 and 60+.

age_india <- age_analysis$Area_Name == "INDIA"

age_chi_table <- matrix(
  c(
    age_analysis$Total_Workers[
      age_india & age_analysis$Age_Group == "0-14"
    ],
    age_analysis$Non_Workers[
      age_india & age_analysis$Age_Group == "0-14"
    ],
    
    age_analysis$Total_Workers[
      age_india & age_analysis$Age_Group == "15-59"
    ],
    age_analysis$Non_Workers[
      age_india & age_analysis$Age_Group == "15-59"
    ],
    
    age_analysis$Total_Workers[
      age_india & age_analysis$Age_Group == "60+"
    ],
    age_analysis$Non_Workers[
      age_india & age_analysis$Age_Group == "60+"
    ]
  ),
  nrow = 3,
  byrow = TRUE,
  dimnames = list(
    c("0-14", "15-59", "60+"),
    c("Workers", "Non_Workers")
  )
)

age_chi_table

# Perform the chi-square test of association.
age_chi_test <- chisq.test(age_chi_table)

age_chi_test
# ---------------------------------------------------------
# Chi-square test: Age group and economic participation
# ---------------------------------------------------------
# We test whether economic participation differs across
# the three main age groups.

age_india <- age_analysis$Area_Name == "INDIA"

age_chi_table <- matrix(
  c(
    # 0–14
    age_analysis$Total_Workers[
      age_india & age_analysis$Age_Group == "0-14"
    ],
    age_analysis$Disabled_Persons[
      age_india & age_analysis$Age_Group == "0-14"
    ] -
    age_analysis$Total_Workers[
      age_india & age_analysis$Age_Group == "0-14"
    ],

    # 15–59
    age_analysis$Total_Workers[
      age_india & age_analysis$Age_Group == "15-59"
    ],
    age_analysis$Disabled_Persons[
      age_india & age_analysis$Age_Group == "15-59"
    ] -
    age_analysis$Total_Workers[
      age_india & age_analysis$Age_Group == "15-59"
    ],

    # 60+
    age_analysis$Total_Workers[
      age_india & age_analysis$Age_Group == "60+"
    ],
    age_analysis$Disabled_Persons[
      age_india & age_analysis$Age_Group == "60+"
    ] -
    age_analysis$Total_Workers[
      age_india & age_analysis$Age_Group == "60+"
    ]
  ),
  nrow = 3,
  byrow = TRUE,
  dimnames = list(
    c("0-14", "15-59", "60+"),
    c("Workers", "Non_Workers")
  )
)

age_chi_table

# Perform the chi-square test of association.
age_chi_test <- chisq.test(age_chi_table)

age_chi_test
# Calculate the strength of the association between age group
# and economic participation.
age_cramers_v <- cramers_v(age_chi_table)

age_cramers_v
# ---------------------------------------------------------
# Chi-square test: Rural-urban residence and economic participation
# ---------------------------------------------------------
# We test whether economic participation differs between
# rural and urban areas using the national Census counts.

india_area <- area_analysis$Area_Name == "INDIA"

area_chi_table <- matrix(
  c(
    area_analysis$Total_Workers[
      india_area & area_analysis$Area_Type == "Rural"
    ],
    area_analysis$Non_Worker_Persons[
      india_area & area_analysis$Area_Type == "Rural"
    ],
    
    area_analysis$Total_Workers[
      india_area & area_analysis$Area_Type == "Urban"
    ],
    area_analysis$Non_Worker_Persons[
      india_area & area_analysis$Area_Type == "Urban"
    ]
  ),
  nrow = 2,
  byrow = TRUE,
  dimnames = list(
    c("Rural", "Urban"),
    c("Workers", "Non_Workers")
  )
)

area_chi_table

# Perform the chi-square test of association.
area_chi_test <- chisq.test(area_chi_table)

area_chi_test
# Calculate the strength of the association between
# rural-urban residence and economic participation.

area_cramers_v <- cramers_v(area_chi_table)

area_cramers_v
# ---------------------------------------------------------
# Chi-square test: Disability type and economic participation
# ---------------------------------------------------------
# We test whether economic participation differs across
# the eight specific disability types.

disability_india <- disability_analysis$Area_Name == "INDIA"

disability_chi_table <- matrix(
  c(
    disability_analysis$Total_Workers[
      disability_india &
        disability_analysis$Disability_Type == "In-Seeing"
    ],
    disability_analysis$Non_Worker_Persons[
      disability_india &
        disability_analysis$Disability_Type == "In-Seeing"
    ],
    
    disability_analysis$Total_Workers[
      disability_india &
        disability_analysis$Disability_Type == "In-Hearing"
    ],
    disability_analysis$Non_Worker_Persons[
      disability_india &
        disability_analysis$Disability_Type == "In-Hearing"
    ],
    
    disability_analysis$Total_Workers[
      disability_india &
        disability_analysis$Disability_Type == "In-Speech"
    ],
    disability_analysis$Non_Worker_Persons[
      disability_india &
        disability_analysis$Disability_Type == "In-Speech"
    ],
    
    disability_analysis$Total_Workers[
      disability_india &
        disability_analysis$Disability_Type == "In-Movement"
    ],
    disability_analysis$Non_Worker_Persons[
      disability_india &
        disability_analysis$Disability_Type == "In-Movement"
    ],
    
    disability_analysis$Total_Workers[
      disability_india &
        disability_analysis$Disability_Type == "Mental-Retardation"
    ],
    disability_analysis$Non_Worker_Persons[
      disability_india &
        disability_analysis$Disability_Type == "Mental-Retardation"
    ],
    
    disability_analysis$Total_Workers[
      disability_india &
        disability_analysis$Disability_Type == "Mental-Illness"
    ],
    disability_analysis$Non_Worker_Persons[
      disability_india &
        disability_analysis$Disability_Type == "Mental-Illness"
    ],
    
    disability_analysis$Total_Workers[
      disability_india &
        disability_analysis$Disability_Type == "Any-Other"
    ],
    disability_analysis$Non_Worker_Persons[
      disability_india &
        disability_analysis$Disability_Type == "Any-Other"
    ],
    
    disability_analysis$Total_Workers[
      disability_india &
        disability_analysis$Disability_Type == "Multiple-Disability"
    ],
    disability_analysis$Non_Worker_Persons[
      disability_india &
        disability_analysis$Disability_Type == "Multiple-Disability"
    ]
  ),
  nrow = 8,
  byrow = TRUE,
  dimnames = list(
    c(
      "In-Seeing",
      "In-Hearing",
      "In-Speech",
      "In-Movement",
      "Mental-Retardation",
      "Mental-Illness",
      "Any-Other",
      "Multiple-Disability"
    ),
    c("Workers", "Non_Workers")
  )
)

disability_chi_table

# Perform the chi-square test of association.
disability_chi_test <- chisq.test(disability_chi_table)

disability_chi_test
# Calculate the strength of the association between disability type
# and economic participation.

disability_cramers_v <- cramers_v(disability_chi_table)

disability_cramers_v
# FINAL RESULTS TABLES
# This section creates clean, reproducible tables for the main results
# of the study after all data cleaning and statistical analysis are complete.
# TABLE 1: NATIONAL DESCRIPTIVE PROFILE
# This table summarizes the national disabled population by sex,
# age group, and disability type using the India-level Census totals.

india_total <- data[
  data$Area_Name == "INDIA" &
    data$Area_Type == "Total" &
    data$Disability_Type == "Total" &
    data$Age_Group == "Total",
]

table1_sex <- data.frame(
  Category = c("Male", "Female"),
  Disabled_Population = c(
    india_total$Disabled_Males,
    india_total$Disabled_Females
  )
)

table1_sex$Percentage <- round(
  table1_sex$Disabled_Population /
    sum(table1_sex$Disabled_Population) * 100,
  1
)

table1_sex
# TABLE 1: AGE GROUP
# This adds the national distribution of persons with disabilities
# across the three substantive age groups used in the analysis.

table1_age <- data[
  data$Area_Name == "INDIA" &
    data$Area_Type == "Total" &
    data$Disability_Type == "Total" &
    data$Age_Group %in% c("0-14", "15-59", "60+"),
  c("Age_Group", "Disabled_Persons")
]

table1_age$Percentage <- round(
  table1_age$Disabled_Persons /
    sum(table1_age$Disabled_Persons) * 100,
  1
)

table1_age
# TABLE 1: DISABILITY TYPE
# This adds the national distribution of persons with disabilities
# across the eight specific disability categories.

table1_disability <- data[
  data$Area_Name == "INDIA" &
    data$Area_Type == "Total" &
    data$Age_Group == "Total" &
    data$Disability_Type != "Total",
  c("Disability_Type", "Disabled_Persons")
]

table1_disability$Percentage <- round(
  table1_disability$Disabled_Persons /
    sum(table1_disability$Disabled_Persons) * 100,
  1
)

table1_disability
# TABLE 1: COMBINE ALL NATIONAL PROFILE SECTIONS
# This combines the separate sex, age, and disability summaries
# into one publication-ready descriptive table.

table1_sex_final <- data.frame(
  Characteristic = "Sex",
  Category = table1_sex$Category,
  Population = table1_sex$Disabled_Population,
  Percentage = table1_sex$Percentage
)

table1_age_final <- data.frame(
  Characteristic = "Age group",
  Category = table1_age$Age_Group,
  Population = table1_age$Disabled_Persons,
  Percentage = table1_age$Percentage
)

table1_disability_final <- data.frame(
  Characteristic = "Disability type",
  Category = table1_disability$Disability_Type,
  Population = table1_disability$Disabled_Persons,
  Percentage = table1_disability$Percentage
)

table1_final <- rbind(
  table1_sex_final,
  table1_age_final,
  table1_disability_final
)

table1_final
# TABLE 2: ECONOMIC PARTICIPATION BY AGE GROUP
# This table summarizes main, marginal, total worker, and non-worker
# counts and economic participation rates for the three analytical age groups.

table2_age <- age_analysis[
  age_analysis$Area_Name == "INDIA",
  c(
    "Age_Group",
    "Disabled_Persons",
    "Main_Worker_Persons",
    "Marginal_Workers",
    "Total_Workers"
  )
]

table2_age$Non_Workers <- (
  table2_age$Disabled_Persons -
    table2_age$Total_Workers
)

table2_age$Economic_Participation_Rate <- round(
  table2_age$Total_Workers /
    table2_age$Disabled_Persons * 100,
  1
)

table2_age$Non_Worker_Rate <- round(
  table2_age$Non_Workers /
    table2_age$Disabled_Persons * 100,
  1
)

table2_age
# TABLE 2: DISPLAY ALL COLUMNS
# This prints the complete economic participation table so that
# all calculated counts and percentages can be checked before formatting.

print(table2_age, width = Inf)
# TABLE 3: ECONOMIC PARTICIPATION BY SEX
# This table summarizes worker counts and economic participation
# rates separately for males and females at the national level.

table3_sex <- data.frame(
  Sex = c("Male", "Female"),
  Disabled_Population = c(
    sex_analysis$Disabled_Males[sex_analysis$Area_Name == "INDIA"],
    sex_analysis$Disabled_Females[sex_analysis$Area_Name == "INDIA"]
  ),
  Main_Workers = c(
    sex_analysis$Main_Worker_Males[sex_analysis$Area_Name == "INDIA"],
    sex_analysis$Main_Worker_Females[sex_analysis$Area_Name == "INDIA"]
  ),
  Marginal_Workers = c(
    sex_analysis$Marginal_Worker_Less_3_Months_Males[sex_analysis$Area_Name == "INDIA"] +
      sex_analysis$Marginal_Worker_3_6_Months_Males[sex_analysis$Area_Name == "INDIA"],
    sex_analysis$Marginal_Worker_Less_3_Months_Females[sex_analysis$Area_Name == "INDIA"] +
      sex_analysis$Marginal_Worker_3_6_Months_Females[sex_analysis$Area_Name == "INDIA"]
  )
)

table3_sex$Total_Workers <- (
  table3_sex$Main_Workers +
    table3_sex$Marginal_Workers
)

table3_sex$Non_Workers <- (
  table3_sex$Disabled_Population -
    table3_sex$Total_Workers
)

table3_sex$Economic_Participation_Rate <- round(
  table3_sex$Total_Workers /
    table3_sex$Disabled_Population * 100,
  1
)

table3_sex$Non_Worker_Rate <- round(
  table3_sex$Non_Workers /
    table3_sex$Disabled_Population * 100,
  1
)

table3_sex
# TABLE 4: ECONOMIC PARTICIPATION BY RURAL–URBAN RESIDENCE
# This table compares disabled population, worker counts, and
# economic participation between rural and urban areas.

table4_area <- area_analysis[
  area_analysis$Area_Name == "INDIA",
  c(
    "Area_Type",
    "Disabled_Persons",
    "Main_Worker_Persons",
    "Marginal_Worker_Less_3_Months_Persons",
    "Marginal_Worker_3_6_Months_Persons",
    "Non_Worker_Persons"
  )
]

table4_area$Marginal_Workers <- (
  table4_area$Marginal_Worker_Less_3_Months_Persons +
    table4_area$Marginal_Worker_3_6_Months_Persons
)

table4_area$Total_Workers <- (
  table4_area$Main_Worker_Persons +
    table4_area$Marginal_Workers
)

table4_area$Economic_Participation_Rate <- round(
  table4_area$Total_Workers /
    table4_area$Disabled_Persons * 100,
  1
)

table4_area$Non_Worker_Rate <- round(
  table4_area$Non_Worker_Persons /
    table4_area$Disabled_Persons * 100,
  1
)

table4_area <- table4_area[
  ,
  c(
    "Area_Type",
    "Disabled_Persons",
    "Main_Worker_Persons",
    "Marginal_Workers",
    "Total_Workers",
    "Non_Worker_Persons",
    "Economic_Participation_Rate",
    "Non_Worker_Rate"
  )
]

table4_area
# TABLE 4: DISPLAY ALL COLUMNS
# This prints the complete rural–urban table so that all values
# can be checked before we finalize the table.

print(table4_area, width = Inf)
# TABLE 5: ECONOMIC PARTICIPATION BY DISABILITY TYPE
# This table compares worker counts and economic participation
# across the eight specific disability types at the national level.

table5_disability <- disability_analysis[
  disability_analysis$Area_Name == "INDIA",
  c(
    "Disability_Type",
    "Disabled_Persons",
    "Main_Worker_Persons",
    "Marginal_Worker_Less_3_Months_Persons",
    "Marginal_Worker_3_6_Months_Persons",
    "Non_Worker_Persons"
  )
]

table5_disability$Marginal_Workers <- (
  table5_disability$Marginal_Worker_Less_3_Months_Persons +
    table5_disability$Marginal_Worker_3_6_Months_Persons
)

table5_disability$Total_Workers <- (
  table5_disability$Main_Worker_Persons +
    table5_disability$Marginal_Workers
)

table5_disability$Economic_Participation_Rate <- round(
  table5_disability$Total_Workers /
    table5_disability$Disabled_Persons * 100,
  1
)

table5_disability$Non_Worker_Rate <- round(
  table5_disability$Non_Worker_Persons /
    table5_disability$Disabled_Persons * 100,
  1
)

table5_disability <- table5_disability[
  ,
  c(
    "Disability_Type",
    "Disabled_Persons",
    "Main_Worker_Persons",
    "Marginal_Workers",
    "Total_Workers",
    "Non_Worker_Persons",
    "Economic_Participation_Rate",
    "Non_Worker_Rate"
  )
]

table5_disability
# TABLE 5: DISPLAY ALL COLUMNS
# This prints the complete disability-type table so we can
# verify every count and participation rate before finalizing it.

print(table5_disability, width = Inf)
# TABLE 6: CHI-SQUARE AND CRAMER'S V RESULTS
# This table summarizes the statistical tests examining the association
# between the four main explanatory factors and economic participation.

table6_stats <- data.frame(
  Factor = c(
    "Sex",
    "Age group",
    "Rural–urban residence",
    "Disability type"
  ),
  
  Chi_Square = c(
    1729003,
    4018764,
    40859,
    637172
  ),
  
  Degrees_of_Freedom = c(
    1,
    2,
    1,
    7
  ),
  
  P_Value = c(
    "<0.001",
    "<0.001",
    "<0.001",
    "<0.001"
  ),
  
  Cramers_V = c(
    0.25,
    0.39,
    0.04,
    0.15
  ),
  
  Interpretation = c(
    "Moderate association",
    "Moderate-to-strong association",
    "Very small association",
    "Small association"
  )
)

table6_stats
# EXPORT TABLE 1
# This saves the national descriptive profile as a CSV file
# so the final result can be reused outside R.

write.csv(
  table1_final,
  "results/tables/Table_1_National_Profile.csv",
  row.names = FALSE
)
# EXPORT TABLE 2
# This saves the age-group economic participation results
# as a CSV file for later use in the paper.

write.csv(
  table2_age,
  "results/tables/Table_2_Economic_Participation_by_Age.csv",
  row.names = FALSE
)
# EXPORT TABLE 3
# This saves the national economic participation results
# for males and females as a CSV file.

write.csv(
  table3_sex,
  "results/tables/Table_3_Economic_Participation_by_Sex.csv",
  row.names = FALSE
)
# EXPORT TABLE 4
# This saves the rural–urban comparison as a CSV file
# for use in the paper and for reproducible project outputs.

write.csv(
  table4_area,
  "results/tables/Table_4_Economic_Participation_Rural_Urban.csv",
  row.names = FALSE
)
# EXPORT TABLE 5
# This saves the economic participation results for the
# eight specific disability types as a CSV file.

write.csv(
  table5_disability,
  "results/tables/Table_5_Economic_Participation_by_Disability_Type.csv",
  row.names = FALSE
)
# EXPORT TABLE 6
# This saves the final statistical test results
# as a CSV file for reporting and reproducibility.

write.csv(
  table6_stats,
  "results/tables/Table_6_Chi_Square_Cramers_V.csv",
  row.names = FALSE
)
# CREATE FIGURES FOLDER
# This creates a dedicated folder for the final publication-ready
# graphs, keeping figures separate from the exported tables.

dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)
# FIGURE 1: ECONOMIC PARTICIPATION BY AGE GROUP
# This creates a clean bar chart showing the national economic
# participation rate of persons with disabilities across age groups.

library(ggplot2)

figure1_age <- ggplot(
  table2_age,
  aes(x = Age_Group, y = Economic_Participation_Rate)
) +
  geom_col(
    width = 0.55
  ) +
  geom_text(
    aes(label = paste0(Economic_Participation_Rate, "%")),
    vjust = -0.4,
    size = 4
  ) +
  scale_y_continuous(
    limits = c(0, 60),
    breaks = seq(0, 60, 10),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Economic Participation of Persons with Disabilities by Age Group",
    x = "Age Group",
    y = "Economic Participation Rate (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 13,
      face = "bold",
      hjust = 0.5
    ),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

figure1_age
# EXPORT FIGURE 1
# This saves the approved age-group participation graph as a
# high-resolution PNG suitable for the dissertation and paper.

ggsave(
  "results/figures/Figure_1_Economic_Participation_by_Age.png",
  plot = figure1_age,
  width = 8,
  height = 6,
  dpi = 300
)
# FIGURE 2: ECONOMIC PARTICIPATION BY SEX
# This creates a clean bar chart comparing the economic
# participation rate of male and female persons with disabilities.

figure2_sex <- ggplot(
  table3_sex,
  aes(x = Sex, y = Economic_Participation_Rate)
) +
  geom_col(
    width = 0.55
  ) +
  geom_text(
    aes(label = paste0(Economic_Participation_Rate, "%")),
    vjust = -0.4,
    size = 4
  ) +
  scale_y_continuous(
    limits = c(0, 55),
    breaks = seq(0, 50, 10),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Economic Participation by Sex",
    x = "Sex",
    y = "Economic Participation Rate (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 13,
      face = "bold",
      hjust = 0.5
    ),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

figure2_sex
# EXPORT FIGURE 2
# This saves the approved sex-based economic participation graph
# as a high-resolution PNG for the dissertation and paper.

ggsave(
  "results/figures/Figure_2_Economic_Participation_by_Sex.png",
  plot = figure2_sex,
  width = 8,
  height = 6,
  dpi = 300
)
# FIGURE 3: ECONOMIC PARTICIPATION BY RURAL–URBAN RESIDENCE
# This creates a clean bar chart comparing economic participation
# between rural and urban areas at the national level.

figure3_area <- ggplot(
  table4_area,
  aes(x = Area_Type, y = Economic_Participation_Rate)
) +
  geom_col(
    width = 0.55
  ) +
  geom_text(
    aes(label = paste0(Economic_Participation_Rate, "%")),
    vjust = -0.4,
    size = 4
  ) +
  scale_y_continuous(
    limits = c(0, 45),
    breaks = seq(0, 40, 10),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Economic Participation by Rural–Urban Residence",
    x = "Area Type",
    y = "Economic Participation Rate (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 13,
      face = "bold",
      hjust = 0.5
    ),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

figure3_area
# EXPORT FIGURE 3
# This saves the approved rural–urban participation graph as a
# high-resolution PNG for the dissertation and paper.

ggsave(
  "results/figures/Figure_3_Economic_Participation_Rural_Urban.png",
  plot = figure3_area,
  width = 8,
  height = 6,
  dpi = 300
)
# FIGURE 4: ECONOMIC PARTICIPATION BY DISABILITY TYPE
# This creates a clean bar chart comparing economic participation
# across the eight specific disability types at the national level.

figure4_disability <- ggplot(
  table5_disability,
  aes(x = Disability_Type, y = Economic_Participation_Rate)
) +
  geom_col(
    width = 0.65
  ) +
  geom_text(
    aes(label = paste0(Economic_Participation_Rate, "%")),
    vjust = -0.4,
    size = 3.5
  ) +
  scale_y_continuous(
    limits = c(0, 48),
    breaks = seq(0, 40, 10),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Economic Participation by Disability Type",
    x = "Disability Type",
    y = "Economic Participation Rate (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 13,
      face = "bold",
      hjust = 0.5
    ),
    axis.title = element_text(size = 11),
    axis.text.x = element_text(
      size = 9,
      angle = 45,
      hjust = 1
    ),
    axis.text.y = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

figure4_disability
# EXPORT FIGURE 4
# This saves the approved disability-type participation graph as a
# high-resolution PNG for the dissertation and paper.

ggsave(
  "results/figures/Figure_4_Economic_Participation_by_Disability_Type.png",
  plot = figure4_disability,
  width = 8,
  height = 6,
  dpi = 300
)
# PREPARE STATE/UT PARTICIPATION DATA
# This creates one national-level economic participation rate for
# each State/UT, excluding the INDIA aggregate from the comparison.

state_participation <- data[
  data$Area_Type == "Total" &
    data$Disability_Type == "Total" &
    data$Age_Group == "Total" &
    data$Area_Name != "INDIA",
  c(
    "Area_Name",
    "Disabled_Persons",
    "Main_Worker_Persons",
    "Marginal_Worker_Less_3_Months_Persons",
    "Marginal_Worker_3_6_Months_Persons"
  )
]

state_participation$Marginal_Workers <- (
  state_participation$Marginal_Worker_Less_3_Months_Persons +
    state_participation$Marginal_Worker_3_6_Months_Persons
)

state_participation$Total_Workers <- (
  state_participation$Main_Worker_Persons +
    state_participation$Marginal_Workers
)

state_participation$Economic_Participation_Rate <- round(
  state_participation$Total_Workers /
    state_participation$Disabled_Persons * 100,
  1
)

state_participation <- state_participation[
  order(state_participation$Economic_Participation_Rate),
  c(
    "Area_Name",
    "Disabled_Persons",
    "Total_Workers",
    "Economic_Participation_Rate"
  )
]

state_participation
# FIGURE 5: STATE/UT VARIATION IN ECONOMIC PARTICIPATION
# This creates a horizontal bar chart showing economic participation
# across all 35 State/UTs, ordered from lowest to highest rate.

figure5_state <- ggplot(
  state_participation,
  aes(
    x = Economic_Participation_Rate,
    y = reorder(Area_Name, Economic_Participation_Rate)
  )
) +
  geom_col(
    width = 0.65
  ) +
  geom_text(
    aes(
      label = paste0(Economic_Participation_Rate, "%")
    ),
    hjust = -0.15,
    size = 3
  ) +
  scale_x_continuous(
    limits = c(0, 70),
    breaks = seq(0, 70, 10),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Economic Participation Across States and Union Territories",
    x = "Economic Participation Rate (%)",
    y = "State/Union Territory"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 13,
      face = "bold",
      hjust = 0.5
    ),
    axis.title = element_text(size = 11),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

figure5_state
# EXPORT FIGURE 5
# This saves the State/UT participation figure as a high-resolution PNG
# for use in the dissertation, paper, or presentation.

ggsave(
  "results/figures/Figure_5_State_UT_Economic_Participation.png",
  plot = figure5_state,
  width = 9,
  height = 12,
  dpi = 300
)