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

#The raw dataset contained 1259 rows. We'll be including all of these rows except the 8 with missing ages and 18 with 
#missing self_employed in our final analytic sample.
clean_data <- clean_data |>
  filter(!is.na(Age) &
           !is.na(self_employed))
