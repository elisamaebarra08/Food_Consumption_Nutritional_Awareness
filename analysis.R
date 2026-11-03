# Food Consumption Patterns and Nutritional Awareness
# Analysis of synthetic sample data—not real survey responses.

library(dplyr)
library(ggplot2)

# Load the CSV created in the previous step
food <- read.csv("food_consumption_nutrition.csv")

# Inspect the dataset
head(food)
str(food)
summary(food)

# Overall averages
overall_summary <- food %>%
  summarise(
    Respondent_Count = n(),
    Average_Meals_Per_Day = mean(MealsPerDay),
    Average_FruitVeg_Servings_Per_Day = mean(FruitVegServingsPerDay),
    Average_Sugary_Drinks_Per_Week = mean(SugaryDrinksPerWeek),
    Average_FastFood_Meals_Per_Week = mean(FastFoodMealsPerWeek),
    Average_Nutrition_Awareness_Score = mean(NutritionAwarenessScore)
  )

print(overall_summary)

# Compare food patterns by age group
age_summary <- food %>%
  group_by(AgeGroup) %>%
  summarise(
    Respondents = n(),
    Avg_FruitVeg_Servings = mean(FruitVegServingsPerDay),
    Avg_Sugary_Drinks = mean(SugaryDrinksPerWeek),
    Avg_FastFood_Meals = mean(FastFoodMealsPerWeek),
    Avg_Awareness_Score = mean(NutritionAwarenessScore),
    .groups = "drop"
  )

print(age_summary)

# Plot average fruit and vegetable servings by age group
ggplot(age_summary, aes(x = AgeGroup, y = Avg_FruitVeg_Servings)) +
  geom_col() +
  labs(
    title = "Average Fruit and Vegetable Servings by Age Group",
    x = "Age Group",
    y = "Average Servings per Day"
  ) +
  theme_minimal()

# Plot nutrition awareness score against fruit and vegetable servings
ggplot(food, aes(x = NutritionAwarenessScore,
                 y = FruitVegServingsPerDay)) +
  geom_point() +
  labs(
    title = "Nutrition Awareness and Fruit/Vegetable Servings",
    x = "Sample Awareness Score",
    y = "Fruit and Vegetable Servings per Day"
  ) +
  theme_minimal()

