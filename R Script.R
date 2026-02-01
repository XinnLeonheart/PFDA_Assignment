library(Hmisc)
library(readr)
library(readxl)
library(dplyr)
library(tidyverse)
library(stringi)
library(stringr)

data_csv1 <- read_csv("D:/APU/Y2 Sem 3/Programming for Data Analysis/Assignment/AssignmentDatasets/HackingData_Part1.csv")
data_xlsx2 <- read_excel("D:/APU/Y2 Sem 3/Programming for Data Analysis/Assignment/AssignmentDatasets/HackingData_Part2.xlsx")
data_txt3  <- read_delim("D:/APU/Y2 Sem 3/Programming for Data Analysis/Assignment/AssignmentDatasets/HackingData_Part3.txt",
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

names(combined_data) <- tolower(names(combined_data))

dim(combined_data)
glimpse(combined_data)
nrow(combined_data)
head(combined_data)

char_cols <- sapply(combined_data, is.character)
names(char_cols[char_cols])

combined_data[char_cols] <- lapply(
  combined_data[char_cols],
  function(x) iconv(x, from = "", to = "UTF-8", sub = "")
)

# check the structure and summary statistics of the combined dataset
summary(combined_data)

# see missing content
describe(combined_data)

#Notify
combined_data$notify <- trimws(combined_data$notify)
combined_data$notify <- tolower(combined_data$notify)
combined_data$notify[combined_data$notify == "unknown"] <- NA

#URL
combined_data$url <- trimws(combined_data$url)
valid_url <- str_detect(
  combined_data$url,
  "^(https?|ftp)://"
)
combined_data$url[!valid_url] <- NA

#IP
combined_data$ip <- trimws(combined_data$ip)
combined_data$ip <- as.character(combined_data$ip)
combined_data$ip[combined_data$ip == ""] <- NA
combined_data$ip[tolower(combined_data$ip) %in% c("unknown", "null", "na")] <- NA
valid_ip <- str_detect(
  combined_data$ip,
  "^((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)(\\.)){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$"
)

combined_data$ip[!valid_ip] <- NA

#Country
# CONVERT Country COLUMN'S DATA TO LOWERCASE
combined_data$country <- tolower(combined_data$country)
# MAKE SURE DONT HAVE SPACE IN THE DATA
combined_data$country <- trimws(combined_data$country)
# CONVERT UNKNOWN VALUES TO NA
combined_data$country[combined_data$country == "unknown"] <- NA
#Fix spelling
combined_data$country <- recode(combined_data$country,
   # Spelling variants and abbreviations
   "united state"         = "usa",
   "united states"        = "usa",
   "uk"                   = "united kingdom",
   "united kingd"         = "united kingdom",
   "viet nam"             = "vietnam",
   "czech republ"         = "czech republic",
   "new caledoni"         = "new caledonia",
   "costarica"            = "costa rica",
   "bruneidarussalam"     = "brunei darussalam",
   "capeverde"            = "cape verde",
   "elsalvador"           = "el salvador",
   "srilanka"             = "sri lanka",
   "saudiarabia"          = "saudi arabia",
   "puertorico"           = "puerto rico",
   "newzealand"           = "new zealand",
   
   # Additional duplicates
   "america"              = "usa",
   "korea"                = "south korea",
   "korea, repub"         = "south korea",
   "iran, islami"         = "iran",
   "burkinafaso"          = "burkina faso",
   "easttimor"            = "east timor",
   "french polyn"         = "french polynesia",
   "cayman islan"         = "cayman islands",
   "european uni"         = "european union",
   "libyan arab jamahiriya" = "libya",
   "libyanarabjamahiriya" = "libya",
   "macedonia, t"         = "macedonia",
   "moldova, rep"         = "moldova",
   "russian fede"         = "russian federation",
   "syrian arab"          = "syria",
   "syrian arab republic" = "syria",
   "taiwan, prov"         = "taiwan",
   "virgin islan"         = "virgin islands",
   "netherlandsantilles"  = "netherlands antilles",
   "netherlands\""        = "netherlands",
   "new zealand\""        = "new zealand",
   "nigeria\""            = "nigeria",
   "united arabemirates"  = "united arab emirates",
   "virginislands(british)" = "virgin islands (british)",
   "virginislands(u.s.)"  = "virgin islands (u.s.)",
   "turksandcaicosislands" = "turks and caicos islands",
   "saintkittsnevis"      = "saint kitts and nevis",
   "saotomeandprincipe"   = "sao tome and principe",
   "norfolkisland"        = "norfolk island"
)

#put regions of country values to a columns
regions <- c(
  "asia/pacific region", "european union", "asia", "africa", 
  "europe", "southamerica", "westeuro", "easteuro", 
  "middleeast", "oceania", "oseania", "regions"
)
combined_data$country[combined_data$country %in% regions] <- "regions"

#non-country
non_country <- c(
  "anonymous proxy",
  "satellite provider",
  "unknown",
  "NA",
  "ascensionisland"
)
combined_data$country[combined_data$country %in% non_country] <- NA

#WebServer
combined_data$webserver <- tolower(combined_data$webserver)
combined_data$webserver <- trimws(combined_data$webserver)
combined_data$webserver[combined_data$webserver == "unknown"] <- NA

#Encoding
# CONVERT ENCODING COLUMN'S DATA TO LOWERCASE
combined_data$encoding <- tolower(combined_data$encoding)
combined_data$encoding <- trimws(combined_data$encoding)
combined_data$encoding[combined_data$encoding == "null"] <- NA
combined_data$encoding <- stringi::stri_enc_toutf8(combined_data$encoding)
combined_data$encoding[combined_data$encoding %in% c("\\xff\\xfe", "xffxfe")] <- NA
combined_data$notify[
  str_detect(combined_data$notify, "\\?\\?")
] <- NA

#Ransom
#DownTime
#Loss
# invalid values, the downtime and loss must not including negative value
# replace invalid values with NA
combined_data <- combined_data %>%
  mutate(
    ransom   = ifelse(ransom <= 0, NA, ransom),
    downtime = ifelse(downtime < 0, NA, downtime),
    loss     = ifelse(loss <= 0, NA, loss)
  )
combined_data$downtime[combined_data$downtime == 9999] <- NA
summary(combined_data[, c("ransom", "downtime", "loss")])

# handling missing values + check missing value
colSums(is.na(combined_data))

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

# Resolving inconsistent categorical values 
# show raw inconsistencies first , inspecting unique values before cleaning
sort(unique(combined_data$country))
sort(unique(combined_data$webserver))
sort(unique(combined_data$encoding))
sort(unique(combined_data$notify))

summary(combined_data[, c("ransom", "downtime", "loss")])

#Exact duplicates rows
sum(duplicated(combined_data))
#View duplicated row data
combined_data[duplicated(combined_data), ]
#check is the duplicated row been removed?
sum(duplicated(combined_data))

#View finally cleaned data
table(combined_data$country)
