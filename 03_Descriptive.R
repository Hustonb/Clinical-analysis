#Exploration of data     CLEAN UP THESE PLOTS FOR FINAL PRESENTATION. titles, color coded with colorblindness in mind,
#col and legend labels. maybe add one more interesting viz to look at.
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

ggplot(
  analysis_data, aes(Age,treatment)
) +
  geom_boxplot() +
  labs(
    title = "Distribution of Age by Treatment.",
    y= "Treatment"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
ggplot(
  data = analysis_data,
  mapping = aes(x = Gender, fill = treatment)
) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = okabe_ito)+
  labs(
    title = "Count of Treatment Pursued by Gender.",
    y= "Count",
    fill="Treatment"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(
  data = analysis_data,
  mapping = aes(x = family_history, fill = treatment)
) +
  geom_bar(position = "dodge")  +
  scale_fill_manual(values = okabe_ito)+
  labs(
    title = "Count of Treatment Pursued by Family History.",
    x="Family History",
    y= "Count",
    fill="Treatment"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

#The final step in preparing data for analysis is to exclude columns that aren't relevant in predicting the research 
#question. Cols that don't relate to demographics of survey participants or workplace characteristics will be removed.
#care_options just describes whether the employee knows what mental health options their workplace offers for healthcare.
#Cols such as mental_health_consequence will be removed as they detail someones perception of workplace attitude
#towards mental health, not a characteristic of the workplace they're in.
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

#Create table1 to explore data and present in journal
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