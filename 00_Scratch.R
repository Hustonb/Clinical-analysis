#Load necessary packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(gtsummary)
library(car)

#Import data as Tibble
Viewraw_data <-readr::read_csv("Data/survey.csv")
raw_data
#initial inspection of data
options(scipen = 999)
glimpse(raw_data)
summary(raw_data)
#The summary reveals that age is taking on values we know to be incorrect, lets look at these now
erroneous_ages<- raw_data |> 
  filter(!(Age<=110 & Age>=18)) |>
  select(Age)
glimpse(erroneous_ages)

clean_data <- raw_data |>
  mutate(Age=if_else(
    Age<=110 & Age>=18,
    Age,
    NA
  ))
clean_data

clean_data |>
  ggplot(aes(Age))+
  geom_boxplot()

#Transform outcome var into binary 1/0 and create a factor for visualizations.
clean_data <- clean_data |>
  mutate(
    treatment = factor(
      treatment,
      levels = c("No", "Yes"),
      labels = c("No Treatment", "Treatment Pursued")
    )
  )


#Categorical vars Gender, Country and have many unique values and can be simplified for analysis
unique(clean_data$Gender)
#transforming Gender to take on values Male, Female, and Other would both clean some of the entries here and be more
#useful for our analysis.
clean_data <- clean_data |>
  mutate(Gender=case_when(
    Gender %in% c("M","Male","male","m","Male-ish","maile","Cis Male","Mal","Male (CIS)", "Man","msle","mail","cis male","Cis Man","Malr","") ~ "Male",
    Gender %in% c("Female","female","Trans-female","Cis Female","F","Woman","f","Femake","woman","cis-female/femme","Trans woman","Female (trans","Female (cis)","femail") ~ "Female",
    .default = "Other"
  ))
clean_data

#We see that the majority of participants were within the US, create a more useful var for analysis from this. 
clean_data <- clean_data |>
  mutate(
    US_Respondent = factor(
      if_else(Country == "United States", 1, 0),
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  )

#remove columns we don't need for analysis towards answering the research question

clean_data <- clean_data |>
  select(-"Timestamp",-"no_employees")

#Looking at missing values and how to handle them
#Leveraged View(clean_data), we see that no values that are supposed to be NA were entered incorrectly in the data
clean_data |>
  mutate(across(everything(), as.character)) |>
  pivot_longer(
    cols = everything(),
    names_to = "Col_name",
    values_to = "Value"
  ) |>
  group_by(Col_name) |>
  summarise(N_Missing=sum(is.na(Value)),
            Prop_missing=N_Missing/n()) |>
  filter(N_Missing!=0)|>
  arrange(desc(N_Missing))

#We exclude comments, state, and work_interfere as above 20% of the values are missing from this column which we
#set as an arbitrary cutoff for col exclusion. REWRITE THIS
clean_data <- clean_data |>
  select(-"comments",-"state",-"work_interfere")

#Descriptive statistics
#The raw dataset contained 1259 rows. We'll be including all of these rows except the 8 with missing ages 
#in our final analytic sample.
clean_data <- clean_data |>
  filter(!is.na(Age))

clean_data |>
  group_by(treatment) |>
  summarise(
    Mean_Age=mean(Age,na.rm=TRUE),
    SD_Age=sd(Age, na.rm=TRUE),
    Min_age=min(Age, na.rm=TRUE),
    Max_age=max(Age, na.rm=TRUE)
  )

ggplot(
  clean_data,
  aes(x = Age)
) +
  geom_histogram()

#The final step in preparing our data for analysis, we exclude columns that aren't relevant in predicting the research question
#Cols that don't relate to demographics of survey participants or workplace characteristics will be removed.
#care_options just describes whether the employee knows what mental health options their workplace offers for healthcare.
#Cols such as mental_health_consequence will be removed as they detail someones perception of workplace attitude
#towards mental health, not a characteristic of the workplace they exist in.
analysis_data <- clean_data |>
  select(
    -Country,
    -care_options,
    -leave,
    -mental_health_consequence,
    -phys_health_consequence,
    -coworkers,
    -supervisor,
    -mental_health_interview,
    -phys_health_interview,
    -mental_vs_physical,
    -obs_consequence
  )

table1 <- analysis_data |>
  select(
    treatment,
    Age,
    Gender,
    family_history,
    US_Respondent,
    self_employed,
    remote_work,
    tech_company,
    benefits,
    wellness_program,
    seek_help,
    anonymity
  ) |>
  tbl_summary(
    by = treatment,
    label = list(
      Age ~ "Age",
      Gender ~ "Gender",
      family_history ~ "Family History",
      US_Respondent ~ "US Respondent",
      self_employed ~ "Self-Employed",
      remote_work ~ "Remote Work",
      tech_company ~ "Tech Company",
      benefits ~ "Mental Health Benefits",
      wellness_program ~ "Wellness Program",
      seek_help ~ "Resources for Seeking Help",
      anonymity ~ "Anonymity Protections"
    ),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "ifany"
  ) |>
  modify_header(
    label ~ "**Characteristic**"
    
  ) |>
  bold_labels()

table1

#TABLE 1 NEARLY DONE

#Exploratory visualization of data towards research question

#dist of Age
ggplot(
  analysis_data, aes(Age)
)+
  geom_histogram()

ggplot(
  analysis_data, aes(Age,treatment)
)+
  geom_boxplot()

ggplot(
  data = analysis_data,
  mapping = aes(x = Gender, fill = treatment)
) +
geom_bar(position = "dodge")

ggplot(
  data = analysis_data,
  mapping = aes(x = family_history, fill = treatment)
) +
  geom_bar(position = "dodge")

#Can make contingency tables to investigate multicollinearity between categorical independent variables
#Some potential variables to investigate based on our domain knowledge: benefits, wellness_program, seek_help, anonymity

contingencytable <- function(var1, var2) {
  analysis_data |>
    select({{ var1 }}, {{ var2 }}) |>
    mutate(
      across(
        everything(),
        ~ factor(.x, levels = c("Yes", "No", "Don't know"))
      )
    ) |>
    table()
}

prop.table(contingencytable(benefits,wellness_program),margin=1)
prop.table(contingencytable(benefits,seek_help),margin=1)
prop.table(contingencytable(benefits,anonymity),margin=1)
prop.table(contingencytable(wellness_program,seek_help),margin=1)
prop.table(contingencytable(wellness_program,anonymity),margin=1)
prop.table(contingencytable(seek_help,anonymity),margin=1)
#No need for a correlation matrix since we only have one numeric predictor

#The independence of observations assumption should be met as each person only has one
#corresponding response. The one thing to watch here would be whether multiple people 
#from the same workspace were taking this survey.
#Vif will be used below to further confirm that the lack of multicollinearity condition
#is met
#Will also verify linear logit condition when fitting first model below

#fit first version of log model
model1 <- glm(treatment ~ .,
          data = analysis_data,
          family = "binomial"
)

# print results
summary(model1)


#Note that gtsummary is supposedly useful for reporting model results too when the time comes

#change survey questions to factors and set reference level for modelin then refit
analysis_data <- analysis_data |>
  mutate(
    benefits = factor(
      benefits,
      levels = c("No", "Yes", "Don't know")
    ),
     wellness_program= factor(
       wellness_program,
      levels = c("No", "Yes", "Don't know")
    ),
    seek_help = factor(
      seek_help,
      levels = c("No", "Yes", "Don't know")
    ),
    anonymity = factor(
      anonymity,
      levels = c("No", "Yes", "Don't know")
    )
  )
#refit first v of model with proper reference levels
model1 <- glm(treatment ~ .,
              data = analysis_data,
              family = "binomial"
)

# print results
summary(model1)

#verify model conditions
vif_val <- as.data.frame(vif(model1))
vif_val
#We see that the adjusted GVIF values are well below the widely accepted threshold of 5, so we satisfy the assumption
#of no multicollinearity for the sake of fitting log model. REWORD AS NEEDED.

#linear logit condition
logit_data <- analysis_data |>
  mutate(
    predicted_prob = predict(
      model1,
      newdata = analysis_data,
      type = "response"
    ),
    log_odds = qlogis(predicted_prob)
  )

ggplot(logit_data, aes(x = Age, y = log_odds)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Assessment of Linearity Between Age and Log-Odds",
    x = "Age (years)",
    y = "Log-Odds of Seeking Mental Health Treatment"
  )

#Conditions are met. next steps are to dc whether i should be removing the 18 rows which aren't producing predictions
#And then interpret model and such.
