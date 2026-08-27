#Initial inspection of data
options(scipen = 999)
glimpse(raw_data)
summary(raw_data)
#Summary reveals that age is taking on values we know to be incorrect, lets look at these now
erroneous_ages<- raw_data |> 
  filter(!(Age<=110 & Age>=18)) |>
  select(Age)
glimpse(erroneous_ages)

#Replace these 8 records with NA, they'll later be removed for modeling
clean_data <- raw_data |>
  mutate(Age=if_else(
    Age<=110 & Age>=18,
    Age,
    NA
  ))

clean_data |>
  ggplot(aes(Age))+
  geom_boxplot()+
  labs(title = "Distribution of Age (years).")

#Transform outcome variable into a factor
clean_data <- clean_data |>
  mutate(
    treatment = factor(
      treatment,
      levels = c("No", "Yes"),
      labels = c("No Treatment", "Treatment Pursued")
    )
  )

#Categorical vars Gender, Country have many unique values and engineered features may be more useful for analysis
unique(clean_data$Gender)
#Transforming Gender to take on values Male, Female, and Other
clean_data <- clean_data |>
  mutate(Gender=case_when(
    Gender %in% c("M","Male","male","m","Male-ish","maile","Cis Male","Mal","Male (CIS)", "Man","msle","mail",
                  "cis male","Cis Man","Malr","") ~ "Male",
    Gender %in% c("Female","female","Trans-female","Cis Female","F","Woman","f","Femake","woman",
                  "cis-female/femme","Trans woman","Female (trans","Female (cis)","femail") ~ "Female",
    .default = "Other"
  ))

clean_data

#We see that the majority of participants were within the US, create a more useful var for regression from this 
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

#Inspecting View(clean_data), we see that no values that are supposed to be NA were entered incorrectly in the data
#Looking at missing values and how to handle them
clean_data |>
  mutate(across(everything(), as.character)) |>
  pivot_longer(
    cols = everything(),
    names_to = "Col_name",
    values_to = "Value"
  ) |>
  group_by(Col_name) |>
  summarise(N_missing=sum(is.na(Value)),
            Prop_missing=N_missing/n()) |>
  filter(N_missing!=0)|>
  arrange(desc(N_missing))

#Exclude comments, state, and work_interfere as above 20% of the values are missing from these columns: which we
#set as an arbitrary cutoff for exclusion from model
clean_data <- clean_data |>
  select(-"comments",-"state",-"work_interfere")

#The raw dataset contained 1259 rows. We'll be including all of these rows except the 8 with missing ages and 18 with 
#missing self_employed in our final analytic sample for 1233 rows.
clean_data <- clean_data |>
  filter(!is.na(Age) &
           !is.na(self_employed))
clean_data
