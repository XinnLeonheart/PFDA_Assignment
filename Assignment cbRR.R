#To investigate whether technical presentation choices made by attackers

library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)

#import different files
csv <- read_csv("HackingData_Part1.csv")
xlsx <- read_excel("HackingData_Part2.xlsx")
txt <- read_delim("HackingData_Part3.txt")

#rename all column names to make sure all similarity
names(csv) <- tolower(gsub("\\s+", "", names(csv)))
names(xlsx) <- tolower(gsub("\\s+", "", names(xlsx)))
names(txt) <- tolower(gsub("\\s+", "", names(txt)))

#merge datasets
#check data types first
csv[1:20, ]  # first 20 rows, all columns
xlsx[1:20, ]
glimpse(csv)
glimpse(xlsx)
glimpse(txt)
#we observe that the data types of Date column is different data type in each file
fix_types <- function(df) {
  df %>%
    mutate(
      date     = as.character(date),
      ransom   = as.numeric(ransom),
      downtime = as.numeric(downtime),
      loss     = as.numeric(loss)
    )
}

csv  <- fix_types(csv)
xlsx <- fix_types(xlsx)
txt  <- fix_types(txt)
#finally merge all datasets together
fulldata <- bind_rows(csv, xlsx, txt)

#a)validating data types
str(fulldata)

#b)handling missing values + check missing value
colSums(is.na(fulldata))

#Removed rows with missing date, country, or url because these identify the attack
#Imputed ransom, downtime, loss with median to maintain central tendency without being affected by extreme outliers
#Non-critical descriptive variables like notify, webserver, encoding are retained for analysis
#removed missing data
fulldata <- fulldata %>%
  filter(
    !is.na(date),
    !is.na(country),
    !is.na(url)
  )
#invalid values, the downtime and loss must not including negative value
#replace invalid values with NA
fulldata <- fulldata %>%
  mutate(
    ransom   = ifelse(ransom <= 0, NA, ransom),
    downtime = ifelse(downtime < 0, NA, downtime),
    loss     = ifelse(loss <= 0, NA, loss)
  )

#replace NA with median on ransom, downtime, loss without affect outliers
fulldata <- fulldata %>%
  mutate(
    ransom   = ifelse(is.na(ransom), median(ransom, na.rm = TRUE), ransom),
    downtime = ifelse(is.na(downtime), median(downtime, na.rm = TRUE), downtime),
    loss     = ifelse(is.na(loss), median(loss, na.rm = TRUE), loss)
  )

#c)Normalizing or scaling data 
#Scaling prevents variables with large values from dominating analysis.
#(to prevent those variables amount effect the final analysis due to their large amount)
#If you do not scale:loss will dominate calculations,downtime becomes almost meaningless,Statistical models focus mostly on loss
#before scaling 
summary(fulldata[, c("ransom", "downtime", "loss")])
#the downtime 9999 is unrealistic , then the loss 1000000000 is too extreme

fulldata <- fulldata %>%
  mutate(
    ransom_scaled   = as.numeric(scale(ransom)),
    downtime_scaled = as.numeric(scale(downtime)),
    loss_scaled     = as.numeric(scale(loss))
  )
#Z-score standardization transforms each numeric observation by subtracting
#the mean and dividing by the standard deviation. Subtracting the mean centers
#the data around zero, while dividing by the standard deviation scales values 
#according to their variability. As a result, each standardized variable has a 
#mean of zero and a standard deviation of one. The resulting z-scores indicate 
#how many standard deviations a value deviates from the average, enabling fair 
#comparison between variables with different units and magnitudes. This method is
#particularly suitable for statistical and correlation-based analyses.

#Use Z-Score Formula - measures how far each value is from the average relative
#to the overall spread of data.Values above average, while values below zero
#are below average.Larger absolute z-scores indicate more extreme observations.

#Z-score standardization does not remove the extreme value of 
#downtime& loss, it show how extreme those values relative to normal attacks, 
#describe how rare is the event but is real too.
#without scaling the loss will dominates whole data analysis, ransom and
#downtime will appear unimportant
summary(fulldata[, c("ransom_scaled", "downtime_scaled", "loss_scaled")])
#Z-score = how far the value from the average compare to normal  variation
#0 = exactly average
#+- 1 = normal
#+- 2 = unusual
#+- 3 = very rare
#> +- 3 = outlier
#> +- 10 = extreme anomaly
#> +-30 = practically impossible under normal conditions

#d)Identifying and treating outliers 
#Z-score standardization revealed extremely large standardized values 
#for downtime and loss, with maximum Z-scores exceeding 30 and 40 respectively. 
#In statistical analysis, observations exceeding ±3 standard deviations are 
#considered outliers, while values beyond ±10 are regarded as extreme
#anomalies. Therefore, the identified observations represent extreme 
#outliers rather than normal variation. These values likely correspond to 
#exceptionally severe cyber-attack incidents or data irregularities and 
#require careful treatment to prevent distortion of analytical results.

#e)Resolving inconsistent categorical values 
#show raw inconsistencies first , inspecting unique values before cleaning
sort(unique(fulldata$country))
sort(unique(fulldata$webserver))
sort(unique(fulldata$encoding))
sort(unique(fulldata$notify))

colnames(fulldata)

