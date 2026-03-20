library(tidyverse)
library(readxl)
library(dplyr)
library(tidymodels)
library(ggplot2)
library(janitor)

statsdata <- real.excel("statsdata.xlsx")

wide_statsdata <- statsdata %>%
  select(`Country Name`, `Series Name`, `2016 [YR2016]`) %>%
  pivot_wider(
    names_from = `Series Name`,
    values_from = `2016 [YR2016]`,
    values_fn = list(`2016 [YR2016]` = mean)
  ) %>%
  filter(!is.na(`GDP per capita (current US$)`) & !is.na(`Life expectancy at birth, total (years)`))


#question 1

wide_statsdata %>%
  summarise(
    Mean = mean(`Life expectancy at birth, total (years)`),
    Median = median(`Life expectancy at birth, total (years)`),
    Std_Dev = sd(`Life expectancy at birth, total (years)`),
    Min = min(`Life expectancy at birth, total (years)`),
    Max = max(`Life expectancy at birth, total (years)`),
    IQR = IQR(`Life expectancy at birth, total (years)`)
  )

#question 2
#these two need to be highlighted in order to run

wide_statsdata %>% filter(`Life expectancy at birth, total (years)` == min(`Life expectancy at birth, total (years)`))

wide_statsdata %>% filter(`Life expectancy at birth, total (years)` == max(`Life expectancy at birth, total (years)`))

#question 3

ggplot(wide_statsdata, aes(x = `Life expectancy at birth, total (years)`)) +
  geom_histogram(bins = 15, fill = "#7bc0a3", color = "#2c5d63") + 
  labs(title = "Distribution of Life Expectancy (2016)",
       x = "Life Expectancy (Years)",
       y = "Number of Countries") +
  theme_minimal() +
  theme(plot.title = element_text(color = "#2c5d63", face = "bold"))

#question 4 - scatterplot

wide_statsdata %>%
  ggplot(mapping = aes(x = log(`GDP per capita (current US$)`),
                       y = `Life expectancy at birth, total (years)`,
                       color = "#2c5d63")) +
  geom_point(alpha = 0.2) +
  labs(x = "Log of GDP per Capita ($)",
       y = "Life Expectancy (years)",
       title = "Life Expectancy versus GDP per Capita") 

question4 <- lm(
  `Life expectancy at birth, total (years)` ~ log(`GDP per capita (current US$)`),
  data = wide_statsdata
)

summary(question4)$r.squared


#question 5 - regression

question5 <- lm(
  `Life expectancy at birth, total (years)` ~ log(`GDP per capita (current US$)`),
  data = wide_statsdata
)

summary(question5)


