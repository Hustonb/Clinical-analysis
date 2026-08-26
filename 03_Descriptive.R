#EDA
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