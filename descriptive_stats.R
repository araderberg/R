#-------------------------------------------------------------------------------
# To calculate descriptive statistical metrics on a dataset
# and create a data quality report file.
#-------------------------------------------------------------------------------
### Load necessary libraries
library(ggplot2)

### Load and view a real-world dataset (USCrimes data per state)
# setwd() function used to set the working directory
setwd("D:/Programming Courses/Coursera Project Network (Hands-on)/Using descriptive statistics to analyze data in R")

#import the USArressts.csv
ds <- read.csv("USArrests.csv", stringsAsFactors = FALSE)

# Assuming 'data' is your data frame
View(ds)

#View and check the dimension of the data sets
dim(ds) #50 rows and 5 columns

#view df using the head function
head(ds)

#view df using tail function
tail(ds)

#Check the internal structure of the data frame
str(ds)

#Count missing values in the variables
#Calculating the number of missing values (NA) in each column of the data frame df.
sapply(ds, function(x) sum(is.na(x)))

#Calculate the summary of all variables in the ds data frame
summary(ds)
#-------------------------------------------------------------------------------
### Calculate the Measure of Frequency metrics 
length(ds$Murder)

length(unique(ds$Murder))

#frequency on a dataset column
freq <- table(ds$Murder)
freq <- sort(freq, decreasing = TRUE)
freq
#-------------------------------------------------------------------------------
### Calculate the Measure of Central Tendency metrics
#mean or average
mean(ds$Assault)
#missing values NA remove this
mean(ds$Assault, na.rm = TRUE) #average assault cases in the dataset

#median / it must be sorted
median(ds$Assault)

median(ds$Assault, na.rm = TRUE) #remove na values

#calculate mode
uniquevalues <- unique(data$Assault)
uniquevalues[which.max(tabulate(match(ds$Assault, uniquevalues)))]

#-------------------------------------------------------------------------------
### Calculate the Measure of Dispersion metrics

min(ds$Assault, na.rm = TRUE)
max(ds$Assault, na.rm = TRUE)

range(ds$Assault, na.rm = TRUE) 

var(ds$Assault, na.rm = TRUE)

sd(ds$Assault, na.rm = TRUE) 

#-------------------------------------------------------------------------------
### Data Quality metrics

as.numeric(2)
as.character(2)

test <- as.character(1:3) #error due to datatype
mean(test)

class(ds$Assault) #integer

test2 <- c(NA, 2, 55, 42, NA)
test2

sum(is.na(test2)) #missing 2 values

sum(is.na(ds$UrbanPop)) #no missing value

sum(is.na(ds$Rape)) #6 missing values
#-------------------------------------------------------------------------------
### to calculate descriptive statistics on any given dataset
apply(ds, MARGIN = 2, length) #all columns 

sapply(ds, function(x) min(x, na.rm = TRUE))

#custom function
quality_data <- function(df=NULL){
  if(is.null(df))  print("Please pass a non-empty data frame")
  summary_table <- do.call(data.frame,
                           list(
                             min = sapply(df, function(x) min(x, na.rm = TRUE)),
                             max = sapply(df, function(x) max(x, na.rm = TRUE)),
                             mean = sapply(df, function(x) mean(x, na.rm = TRUE)),
                             sd = sapply(df, function(x) sd(x, na.rm = TRUE)),
                             Total = apply(df, 2, length),
                             NULLS = sapply(df, function(x) sum(is.na(x))),
                             Unique = sapply(df, function(x) length(unique(x))),
                             dataType = sapply(df, class)
                           ))
  nums <- vapply(summary_table, is.numeric, FUN.VALUE = logical(1))
  summary_table[,nums] <- round(summary_table[, nums], digits = 3)
  return(summary_table)
}
#-------------------------------------------------------------------------------
###Export the results of the descriptive statistics to a data quality report file 
df_quality <- quality_data(ds)

df_quality  <- cbind(columns=row.names(df_quality),
                     data.frame(df_quality, row.names = NULL))

write.csv(df_quality, "Data Quality Report.csv", row.names = FALSE)

#append the date
write.csv(df_quality, paste0("Data Quality Report ",
                             format(Sys.time(), "%d-%m%y-%H%M%S"),
                             ".csv"),
          row.names = FALSE)

#-------------------------------------------------------------------------------
# Plotting excluding the 'UrbanPop' column
barplot(as.matrix(ds[, c('Murder', 'Assault', 'Rape')]), beside = TRUE, col = rainbow(3), 
        main = "Crime Statistics", xlab = "X", ylab = "Count", 
        legend.text = c('Murder', 'Assault', 'Rape'), args.legend = list(x = "topright", cex = 0.8))


#-------------------------------------------------------------------------------
# END OF PROJECT
#-------------------------------------------------------------------------------