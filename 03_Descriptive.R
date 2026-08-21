#EDA
#dist of Age
ggplot(
  analysis_data, aes(Age)
)+
  geom_histogram()

ggplot(
  analysis_data, aes(Age,treatment))+
  geom_boxplot()+
  labs(title = "Hey",
       y="Treatment")
#Some of the most interesting visualizations from initial inspection of data
ggplot(
  data = analysis_data,
  mapping = aes(x = Gender, fill = treatment)
) +
  geom_bar(position = "dodge")+
  labs(title="figure 2: etc fix this up",
       y="Count")

ggplot(
  data = analysis_data,
  mapping = aes(x = family_history, fill = treatment)
) +
  geom_bar(position = "dodge")