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

#change survey questions to factors and set reference level for modeling then refit
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
#of no multi-collinearity for the sake of fitting log model. REWORD AS NEEDED.

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

#Interpret model. maybe fit univariate for each? var? 
#remember that this isn't too much of a ML project and we don't care much about the models predictive power or anything
#
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

#fit uv regression like above and make gtsummary table from it then merge with table for mv log regression
#mv reg table
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
hoslem.test(
  analysis_data$treatment,
  fitted(model1),
  g = 10
)

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
#p-value of 0.3762>.05 so there's no statistically sig evidence of lack of fit based on the Hosmer-Lemeshow goodness
#of fit test.