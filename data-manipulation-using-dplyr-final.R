#-----------------------------------------------------------------
#  Analyze data by calculating summary statistics,  create new 
#  columns derived from existing ones and other data cleaning,
#  transformation, and analysis.
#-----------------------------------------------------------------
### Install the dplyr package (one time install, if not available)
install.packages("dplyr")

### Load necessary libraries
library("dplyr")

### Load and view a real-world dataset (voteincome from AZ and SC)
# setwd() function used to set the working directory
setwd("D:/R programming/Data/")

#import the voteincome.csv
ds <- read.csv("voteincome.csv")

### Look at the voteincome dataset
ds

### Check the structure of the dataset
glimpse(ds)

### This will give the same result
str(ds)

#-----------------------------------------------------------------
### Retrieve columns of the dataset
# Select the desired columns
selected_data <- ds %>%
  select(state, age, female, income, education)

### Store the result in a variable called edu_income
edu_income <- ds %>%
  select(education, income)

## Print out the variable
edu_income

#-----------------------------------------------------------------
### Retrieve rows of columns of the dataset
# Filter the voteincome dataset for the year 2000 for State = SC
selected_data_SC <- ds %>%
  filter(state == "SC")

# Filter for states in 2000 where income is greather than 9
selected_data_ALL <- ds %>%
  filter(year == 2000, income >9)
#-----------------------------------------------------------------
### To sort the result of a column
# Sort in ascending order of age
ds %>%
  arrange(age)

# Sort in descending order of age and 
## select the top thirteen
ds %>%
  arrange(desc(age)) %>%
  slice(1:13)

# Filter for the state AR, 
## then arrange in descending order of age
ds %>%
  filter(state == "AR") %>%
  arrange(desc(age))
#-----------------------------------------------------------------
### Change or add columns in the dataset
# Use mutate to decode education levels and vote
# Use mutate to create a new column called gender 
updated_data <- ds %>%
  mutate(education = factor(education, levels = c(1, 2, 3, 4), labels = c("No HS", "HS", "College", "Graduate")),
         gender = ifelse(female == 0, "F", "M"),
         vote = ifelse(vote == 0, "Did Not Vote", "Voted"))

# Drop female variable
final_data <- updated_data %>%
  select(-female)  # Select all columns except female
#-----------------------------------------------------------------
### Summarize to find the median age
final_data %>%
  summarise(medianAge = median(age))

# Filter for state in AR then summarize the median Age
final_data %>%
  filter(state == "AR") %>%
  summarise(medianAge = median(age))
#-----------------------------------------------------------------
### Find median age and maximum Income per state
df <- ds %>%
  group_by(state) %>%
  summarise(medianAge = median(age),
            maxIncomePerState = max(income))
# END OF PROJECT
#-----------------------------------------------------------------