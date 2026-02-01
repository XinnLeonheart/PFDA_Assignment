library(Hmisc)
library(readr)
library(readxl)
library(dplyr)
library(tidyverse)

#find current file
getwd()
list.files("C:/Users/Xenia Thor/Documents/")

data_csv1 <- read_csv("C:/Users/Xenia Thor/Documents/HackingData_Part1.xlsx")
data_xlsx2 <- read_excel("C:/Users/Xenia Thor/Documents/HackingData_Part2.xlsx")  
data_txt3  <- read_delim("C:/Users/Xenia Thor/Documents/HackingData_Part3.txt",
                         delim = "\t")

# check column name
colnames(data_csv1)
colnames(data_xlsx2)
colnames(data_txt3)

numeric_cols <- c("Ransom", "DownTime", "Loss")

data_csv1 <- data_csv1 |>
  mutate(across(all_of(numeric_cols), as.numeric))

data_xlsx2 <- data_xlsx2 |>
  mutate(across(all_of(numeric_cols), as.numeric))

data_txt3 <- data_txt3 |>
  mutate(across(all_of(numeric_cols), as.numeric))

data_csv1  <- data_csv1  |> mutate(Date = as.character(Date))
data_xlsx2 <- data_xlsx2 |> mutate(Date = as.character(Date))
data_txt3  <- data_txt3  |> mutate(Date = as.character(Date))

# combine rows of 3 files
combined_data <- bind_rows(
  data_csv1,
  data_xlsx2,
  data_txt3
)

dim(combined_data)
glimpse(combined_data)

head(combined_data)

char_cols <- sapply(combined_data, is.character)
names(char_cols[char_cols])

combined_data[char_cols] <- lapply(
  combined_data[char_cols],
  function(x) iconv(x, from = "", to = "UTF-8", sub = "")
)

# see missing content
describe(combined_data)
# handling missing values + check missing value
colSums(is.na(fulldata))

# General summary of your data
summary(combined_data)

#Notify
combined_data$Notify <- trimws(combined_data$Notify)
combined_data$Country <- tolower(combined_data$Notify)

#URL
combined_data$URL <- trimws(combined_data$URL)

#IP
combined_data$IP <- trimws(combined_data$IP)

#Country
# CONVERT Country COLUMN'S DATA TO LOWERCASE
combined_data$Country <- tolower(combined_data$Country)
# MAKE SURE DONT HAVE SPACE IN THE DATA
combined_data$Country <- trimws(combined_data$Country)
# CONVERT UNKNOWN VALUES TO NA
combined_data$Country[combined_data$Country == "unknown"] <- NA

#WebServer
combined_data$WebServer <- tolower(combined_data$WebServer)
combined_data$Country <- trimws(combined_data$WebServer)
combined_data$WebServer[combined_data$WebServer == "unknown"] <- NA

#Encoding
# CONVERT ENCODING COLUMN'S DATA TO LOWERCASE
combined_data$Encoding <- tolower(combined_data$Encoding)
combined_data$Encoding <- trimws(combined_data$Encoding)
combined_data$Encoding[combined_data$Encoding == "null"] <- NA

#Ransom


#DownTime


#Loss


# invalid values, the downtime and loss must not including negative value
# replace invalid values with NA
fulldata <- fulldata %>%
  mutate(
    ransom   = ifelse(ransom <= 0, NA, ransom),
    downtime = ifelse(downtime < 0, NA, downtime),
    loss     = ifelse(loss <= 0, NA, loss)
  )
summary(combined_data[, c("ransom", "downtime", "loss")])

# REMOVE DUPLICATE DATA
combined_data <- combined_data |>
  distinct()

# replace NA with median on ransom, downtime, loss without affect outliers
combined_data <- combined_data %>%
  mutate(
    ransom   = ifelse(is.na(ransom), median(ransom, na.rm = TRUE), ransom),
    downtime = ifelse(is.na(downtime), median(downtime, na.rm = TRUE), downtime),
    loss     = ifelse(is.na(loss), median(loss, na.rm = TRUE), loss)
  )

#REMOVE DUPLICATE ROWS BASED ON ALL COLUMNS
df_unique <- unique (df)

#REMOVE DUPLICATE ROWS BASED ON SPECIFIC COLUMNS
df_unique <-df[!duplicated(df)]

#REMOVE DUPLICATES BASED ON SPECIFIC COLUMNS
df_unique <- df[!duplicated(df$colnames),]

# REMOVE DUPLICATE DATA
df_unique <- df %>% distinct()

#REMOVE DUPLICATED BASED ON SPECIFIC COLUMS
df_unique <- df %>% distinct (column1, column2, .keep_all = TRUE)


# Resolving inconsistent categorical values 
# show raw inconsistencies first , inspecting unique values before cleaning
sort(unique(combined_data$country))
sort(unique(combined_data$webserver))
sort(unique(combined_data$encoding))
sort(unique(combined_data$notify))

