install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("GGally")




# Loading the libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(GGally)
library(knitr)

#Loading the data set

data <- read.csv("D:/ADOPTING SUSTAINABLE PRACTICES IN MANAGING EMERGING TECHNOLOGIES.csv")
data


colnames(data)

str(data)


# Creating a mapping of text responses to numeric values
How.effective.is.your.organization.s.current.sustainability.program.in.managing.emerging.technologies. <- c(
  "Very effective" = 4,
  "Somewhat effective" = 3,
  "Not very effective" = 2,
  "Not effective at all" = 1
)



# Creating a mapping of text responses to numeric values
Which.area.of.sustainability.do.you.think.needs.the.most.improvement.in.your.organization. <- c(
  "Energy efficiency" = 1,
  "Waste management" = 2,
  "Carbon footprint reduction" = 3,
  "Water conservation" = 4
)


# Creating a mapping of text responses to numeric values
How.well.does.your.organization.incorporate.sustainable.practices.in.the.lifecycle.management.of.emerging.technologies.<- c(
  "Very well" = 4,
  "Moderately well" = 3,
  "Poorly" = 2,
  "Not at all" = 1
)


# Creating a mapping of text responses to numeric values
What.is.the.biggest.challenge.in.implementing.sustainable.practices.in.emerging.technologies.at.your.organization. <- c(
  "Lack of expertise" = 1,
  "Cost" = 2,
  "Regulatory compliance" = 3,
  "Resistance to change" = 4
)




# Creating a mapping of text responses to numeric values
How.often.does.your.organization.review.and.update.its.sustainability.practices.concerning.emerging.technologies. <- c(
  "Never" = 1,
  "Every few years" = 2,
  "Bi-annually" = 3,
  "Annually" = 4
)


# Creating a mapping of text responses to numeric values
To.what.extent.does.your.organization.invest.in.sustainable.technologies. <- c(
  "Not at all" = 1,
  "Minimally" = 2,
  "Moderately" = 3,
  "Heavily" = 4
)


# Creating a mapping of text responses to numeric values
How.satisfied.are.you.with.the.training.provided.on.sustainable.practices.related.to.emerging.technologies. <- c(
  "Very unsatisfied" = 1,
  "Unsatisfied" = 2,
  "Satisfied" = 3,
  "Very satisfied" = 4
)



# Creating a mapping of text responses to numeric values
How.well.do.you.think.your.organizations.sustainability.efforts.are.communicated.to.employees. <- c(
  "Not communicated at all" = 1,
  "Well" = 2,
  "Very well" = 3
)


# Creating a mapping of text responses to numeric values
How.well.do.you.think.your.organizations.sustainability.efforts.are.communicated.to.employees. <- c(
  "Not communicated at all" = 1,
  "Well" = 2,
  "Very well" = 3
)


# Creating a mapping of text responses to numeric values
Which.sustainable.technology.has.had.the.most.positive.impact.on.your.organization. <- c(
  "Energy-efficient IT infrastructure" = 1,
  "Renewable energy systems" = 2,
  "Sustainable supply chain management" = 3,
  "Eco-friendly product design" = 4
)


# Creating a mapping of text responses to numeric values
How.effective.are.your.organization.s.policies.in.reducing.electronic.waste. <- c(
  "Not effective at all" = 1,
  "Not very effective" = 2,
  "Somewhat effective" = 3,
  "Very effective" = 4
)


# Creating a mapping of text responses to numeric values
How.important.is.sustainability.in.your.organization.s.strategy.for.adopting.new.technologies. <- c(
  "Not important at all" = 1,
  "Not very important" = 2,
  "Somewhat important" = 3,
  "Very important" = 4
)

# Creating a mapping of text responses to numeric values
What.is.your.organization.s.primary.motivation.for.adopting.sustainable.practices.in.managing.emerging.technologies. <- c(
  "Cost savings" = 1,
  "Regulatory compliance" = 2,
  "Corporate social responsibility" = 3,
  "Competitive advantage" = 4
)



# Rename the columns with shorter names
colnames(data) <- c(
  "Timestamp",  # 1
  "Effectiveness",  # 2
  "Improvement_Area",  # 3
  "Incorporation",  # 4
  "Biggest_Challenge",  # 5
  "Review_Frequency",  # 6
  "Investment_Level",  # 7
  "Training_Satisfaction",  # 8
  "Communication",  # 9
  "Positive_Impact",  # 10
  "Electronic_Waste_Policies",  # 11
  "Importance_Strategy",  # 12
  "Primary_Motivation"  # 13
)
print(colnames(data))

mapping_list <- list(
  Effectiveness = c(
    "Very effective" = 4,
    "Somewhat effective" = 3,
    "Not very effective" = 2,
    "Not effective at all" = 1
  ),
  Improvement_Area = c(
    "Energy efficiency" = 1,
    "Waste management" = 2,
    "Carbon footprint reduction" = 3,
    "Water conservation" = 4
  ),
  Incorporation = c(
    "Very well" = 4,
    "Moderately well" = 3,
    "Poorly" = 2,
    "Not at all" = 1
  ),
  Biggest_Challenge = c(
    "Lack of expertise" = 1,
    "Cost" = 2,
    "Regulatory compliance" = 3,
    "Resistance to change" = 4
  ),
  Review_Frequency = c(
    "Never" = 1,
    "Every few years" = 2,
    "Bi-annually" = 3,
    "Annually" = 4
  ),
  Investment_Level = c(
    "Not at all" = 1,
    "Minimally" = 2,
    "Moderately" = 3,
    "Heavily" = 4
  ),
  Training_Satisfaction = c(
    "Very unsatisfied" = 1,
    "Unsatisfied" = 2,
    "Satisfied" = 3,
    "Very satisfied" = 4
  ),
  Communication = c(
    "Not communicated at all" = 1,
    "Well" = 2,
    "Very well" = 3
  ),
  Positive_Impact = c(
    "Energy-efficient IT infrastructure" = 1,
    "Renewable energy systems" = 2,
    "Sustainable supply chain management" = 3,
    "Eco-friendly product design" = 4
  ),
  Electronic_Waste_Policies = c(
    "Not effective at all" = 1,
    "Not very effective" = 2,
    "Somewhat effective" = 3,
    "Very effective" = 4
  ),
  Importance_Strategy = c(
    "Not important at all" = 1,
    "Not very important" = 2,
    "Somewhat important" = 3,
    "Very important" = 4
  ),
  Primary_Motivation = c(
    "Cost savings" = 1,
    "Regulatory compliance" = 2,
    "Corporate social responsibility" = 3,
    "Competitive advantage" = 4
  )
)


# Apply mappings to the transform of the columns
for (column in names(mapping_list)) {
  data[[column]] <- mapping_list[[column]][data[[column]]]
}

# Displaying the first few rows of the data frame to verify
head(data)

# Displaying the column names
print(colnames(data))


# Displaying the summary statistics
summary(data)


missing_values <- sapply(data, function(x) sum(is.na(x)))
print(missing_values)



clean_data <- na.omit(data)


print(clean_data)


# Histogram of Distribution of Effectiveness

ggplot(clean_data, aes(x = Effectiveness)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of Effectiveness", x = "Effectiveness", y = "Frequency")

# Box plot of Training Satisfaction by Investment Level
ggplot(clean_data, aes(x = factor(Investment_Level, levels = 1:4, labels = c("Not at all", "Minimally", "Moderately", "Heavily")), y = Training_Satisfaction)) +
  geom_boxplot(fill = "green", color = "blue") +
  labs(title = "Training Satisfaction by Investment Level", x = "Investment Level", y = "Training Satisfaction")

# Bar plot of Incorporation of Sustainable Practices
ggplot(clean_data, aes(x = factor(Incorporation, levels = 1:4, labels = c("Very well", "Moderately well", "Poorly", "Not at all")))) +
  geom_bar(fill = "purple", color = "black") +
  labs(title = "Incorporation of Sustainable Practices", x = "Incorporation", y = "Count")

# Box plot of Review Frequency vs. Effectiveness
ggplot(clean_data, aes(x = factor(Review_Frequency, levels = 1:4, labels = c("Never", "Every few years", "Bi-annually", "Annually")), y = Effectiveness)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Effectiveness by Review Frequency", x = "Review Frequency", y = "Effectiveness")

#  Bar plot of Improvement Areas
ggplot(clean_data, aes(x = factor(Improvement_Area, levels = 1:4, labels = c("Energy efficiency", "Waste management", "Carbon footprint reduction", "Water conservation")))) +
  geom_bar(fill = "green", color = "black") +
  labs(title = "Improvement Areas", x = "Improvement Area", y = "Count")

# Density plot of Effectiveness
ggplot(clean_data, aes(x = Effectiveness)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of Effectiveness", x = "Effectiveness", y = "Density")

# Violin plot of Training Satisfaction by Communication
ggplot(clean_data, aes(x = factor(Communication, levels = 1:3, labels = c("Not communicated at all", "Well", "Very well")), y = Training_Satisfaction)) +
  geom_violin(fill = "lightgreen", color = "black") +
  labs(title = "Training Satisfaction by Communication", x = "Communication", y = "Training Satisfaction")


# Convert Review of Frequency to a factor with levels
clean_data$Review_Frequency <- factor(clean_data$Review_Frequency, levels = 1:4, labels = c("Never", "Every few years", "Bi-annually", "Annually"))

# Line plot of the Training Satisfaction over Review Frequency
ggplot(clean_data, aes(x = Review_Frequency, y = Training_Satisfaction, group = 1)) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "Training Satisfaction Over Review Frequency", x = "Review Frequency", y = "Training Satisfaction")

# Pie chart of the Primary Motivation
primary_motivation_counts <- clean_data %>%
  count(Primary_Motivation)

# Plotting the pie chart
ggplot(primary_motivation_counts, aes(x = "", y = n, fill = factor(Primary_Motivation, levels = 1:4, labels = c("Cost savings", "Regulatory compliance", "Corporate social responsibility", "Competitive advantage")))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Primary Motivation for Adopting Sustainable Practices", fill = "Primary Motivation") +
  theme(axis.text.x = element_blank())



# Pair plot to show the relationships between multiple variables
ggpairs(clean_data, columns = c("Effectiveness", "Investment_Level", "Training_Satisfaction", "Communication"),
        upper = list(continuous = "points"),
        lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"))


#  Linear Regression
model1 <- lm(Effectiveness ~ Investment_Level, data = clean_data)
summary(model1)

# Multiple Linear Regression
model2 <- lm(Effectiveness ~ Investment_Level + Training_Satisfaction + Communication, data = clean_data)
summary(model2)


# Predictions from the model
predictions <- predict(model2, clean_data)

# Calculate residuals
residuals <- clean_data$Effectiveness - predictions

# Plotting residuals
ggplot(data = clean_data, aes(x = predictions, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals")

#Summary of Predicted Effectiveness

predictions <- predict(model2, clean_data)

clean_data <- clean_data %>%
  mutate(Predicted_Effectiveness = predictions)

summary_table <- clean_data %>%
  summarise(
    Mean_Predicted_Effectiveness = mean(Predicted_Effectiveness, na.rm = TRUE),
    Median_Predicted_Effectiveness = median(Predicted_Effectiveness, na.rm = TRUE),
    SD_Predicted_Effectiveness = sd(Predicted_Effectiveness, na.rm = TRUE),
    Min_Predicted_Effectiveness = min(Predicted_Effectiveness, na.rm = TRUE),
    Max_Predicted_Effectiveness = max(Predicted_Effectiveness, na.rm = TRUE),
    .groups = 'drop'
  )
summary_table_df <- as.data.frame(summary_table)

kable(summary_table_df, 
      caption = "Summary of Predicted Effectiveness",
      col.names = c("Mean Effectiveness", "Median Effectiveness", "Standard Deviation", "Minimum Effectiveness", "Maximum Effectiveness"),
      format = "markdown")
