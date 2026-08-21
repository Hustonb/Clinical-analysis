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
    ),
    treatment_binary = if_else(
      treatment == "Yes",
      1,
      0
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
clean_data |>
  count(Country) |>
  arrange(desc(n))

clean_data <- clean_data |>
  mutate(US_Respondent=if_else(
    Country=="United States",1,0
  ))

#remove columns we don't need for analysis towards answering my research question


clean_data <- clean_data |>
  select(-"Timestamp")

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
    no_employees,
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
      no_employees ~ "Company Size",
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
#need to reorder the groups within some vars, ie company size.
#Need to think about the way we want it to display some binary categorical vars like family history. should it disp yes and no below it
#TABLE 1 NEARLY DONE
#Do I want to do covariance stuff before launching into modeling?
