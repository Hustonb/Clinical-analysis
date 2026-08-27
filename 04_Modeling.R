#Contingency tables to investigate collinearity between categorical independent variables
#Potential variables to investigate based on our domain knowledge: benefits, wellness_program, seek_help, anonymity

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
#corresponding response. The thing to watch here would be whether multiple people 
#from the same workspace were taking this survey.
#VIF will be used below to further confirm that the lack of multicollinearity condition
#is met
#Will also verify linearity of the logit assumption after fitting first model below

#First version of log model
model1 <- glm(treatment ~ .,
              data = analysis_data,
              family = "binomial"
)

summary(model1)

#Change survey questions to factors and set reference level for modeling then refit
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
#Refit first v of model with proper reference levels
model1 <- glm(treatment ~ .,
              data = analysis_data,
              family = "binomial"
)

summary(model1)

#Now verify model conditions:
vif_val <- as.data.frame(vif(model1))
vif_val
#We see that the adjusted GVIF values are well below the widely used threshold of 5, so we satisfy the assumption
#of no multicollinearity for the sake of fitting the logistic model.

#Linearity of the logit
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
  ) +
  theme(plot.title = element_text(hjust = 0.5))

#Create table displaying odds ratios (univariable model and multivariable) for each variable
univariable_results <- analysis_data |>
  tbl_uvregression(
    method = glm,
    y = treatment,
    method.args = list(family = binomial),
    exponentiate = TRUE,
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
    )
  ) |>
  modify_column_merge(
    pattern = "{estimate} ({ci})",
    rows = !is.na(estimate)
  ) |>
  modify_header(
    estimate = "**OR (95% CI)**"
  ) |>
  modify_column_hide(
    columns = c(ci, conf.low, conf.high)
  )

multivariable_results <- tbl_regression(
  model1,
  exponentiate = TRUE,
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
  )
) |>
  modify_column_merge(
    pattern = "{estimate} ({ci})",
    rows = !is.na(estimate)
  ) |>
  modify_header(
    estimate = "**OR (95% CI)**"
  ) |>
  modify_column_hide(
    columns = c(ci, conf.low, conf.high)
  )

tbl_merge(
  tbls = list(univariable_results,multivariable_results),
  tab_spanner = c("**Univariable**","**Multivariable**")
) |>
  modify_column_hide(columns="stat_n_1")|>
  bold_labels()

model_data <- model.frame(model1)

treatment_numeric <- if_else(
  model_data$treatment == "Treatment Pursued",
  1,
  0
)

hoslem.test(
  treatment_numeric,
  fitted(model1),
  g = 10
)
#p-value of 0.3762>.05 so there's no statistically significant evidence of lack of fit based on the 
#Hosmer-Lemeshow goodness of fit test