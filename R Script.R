library(Hmisc)
library(readr)
library(readxl)
library(dplyr)
library(tidyverse)
library(stringi)
library(stringr)
library(lubridate)
library(mice)
library(gt)
library(gtExtras)
library(naniar)

data_csv1 <- read_csv("D:/APU/Y2 Sem 3/Programming for Data Analysis/Assignment/AssignmentDatasets/HackingData_Part1.csv", header = TRUE, sep = "," )
data_xlsx2 <- read_excel("D:/APU/Y2 Sem 3/Programming for Data Analysis/Assignment/AssignmentDatasets/HackingData_Part2.xlsx", sheet = 1)
data_txt3  <- read_delim("D:/APU/Y2 Sem 3/Programming for Data Analysis/Assignment/AssignmentDatasets/HackingData_Part3.txt",
                         delim = "\t", col_names = TRUE)
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

# CONVERT ALL COLUMN NAME TO LOWER CASE
names(combined_data) <- tolower(names(combined_data))

# CHECK NUMBER OF ROW AND COLUMN
dim(combined_data)

glimpse(combined_data)

char_cols <- sapply(combined_data, is.character)
names(char_cols[char_cols])

combined_data[char_cols] <- lapply(
  combined_data[char_cols],
  function(x) iconv(x, from = "", to = "UTF-8", sub = "")
)

#Date
combined_data$notify <- trimws(combined_data$notify)
combined_data$notify <- tolower(combined_data$notify)
combined_data$notify[combined_data$notify == "unknown"] <- NA
combined_data$notify[combined_data$notify == "null"] <- NA
sum(is.na(combined_data$date))
# output: 0 (There is no NA value in column Date)

class(combined_data$date)
head(combined_data$date, 10)
combined_data %>% count(date) %>% head(10)
# found that the date format is different

# convert the date format to the same
combined_data$date <- parse_date_time(
  combined_data$date,
  orders = c("mdy", "ymd", "dmy")
)
class(combined_data$date)
combined_data$date <- as.Date(combined_data$date)
sum(is.na(combined_data$date))
# check for bad values
combined_data %>%
  filter(is.na(date)) %>%
  distinct(date) %>%
  head(20)
filter(combined_data, is.na(date))
# double check for date format
head(combined_data$date)
# output: Year-Month-Date

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
# EXPLORE WHAT COUNTRY EXIST
count(combined_data, country) %>% print(n = Inf)

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
   "russian fede"         = "russia",
   "russian federation"   = "russia",
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
count(combined_data, webserver)

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

# REMOVE DUPLICATE DATA
combined_data <- combined_data |>
  distinct()

# check missing value
colSums(is.na(combined_data))

# see missing content
#visual representation of missing data
md.pattern(combined_data)

# see the percentage of missing data
miss_var_summary(combined_data)

# Resolving inconsistent categorical values 
# show raw inconsistencies first , inspecting unique values before cleaning
sort(unique(combined_data$country))
sort(unique(combined_data$webserver))
sort(unique(combined_data$encoding))
sort(unique(combined_data$notify))

#check is the duplicated row been removed before fill in the NA
sum(duplicated(combined_data))

# check the structure and summary statistics of the combined dataset
describe(combined_data)

# General summary 
summary(combined_data)



# fill NA
# create impute dataset
impute_data <- combined_data %>%
  select(
    ransom,
    downtime,
    loss,
    country,
    webserver,
    encoding
  )

# dealing with column notify


# dealing with column url


# dealing with column ip


# dealing with column country


# dealing with column ransom, downtime, loss
# convert the data type to factor
impute_data <- impute_data %>%
  mutate(
    country   = as.factor(country),
    webserver = as.factor(webserver),
    encoding  = as.factor(encoding)
  )

impute_data <- impute_data %>%
  mutate(
    ransom = log1p(ransom),
    loss   = log1p(loss)
  )

meth <- make.method(impute_data)

meth["ransom"]   <- "pmm"
meth["loss"]     <- "pmm"
meth["downtime"] <- "pmm"

# Do NOT impute categorical variables
meth[c("country", "webserver", "encoding")] <- ""

pred <- make.predictorMatrix(impute_data)

# No variable predicts itself
diag(pred) <- 0

# Numeric variables predict each other
pred["ransom",   c("loss", "downtime")] <- 1
pred["loss",     c("ransom", "downtime")] <- 1
pred["downtime", c("ransom", "loss")] <- 1

# Categorical variables do nothing
pred[c("country", "webserver", "encoding"), ] <- 0
pred[, c("country", "webserver", "encoding")] <- 0

set.seed(123)

imp <- mice(
  impute_data,
  method = meth,
  predictorMatrix = pred,
  m = 3,
  maxit = 5,
  printFlag = TRUE
)

completed <- complete(imp)

completed <- completed %>%
  mutate(
    ransom = expm1(ransom),
    loss   = expm1(loss)
  )

combined_data$ransom   <- completed$ransom
combined_data$loss     <- completed$loss
combined_data$downtime <- completed$downtime

# Prepare data for comparison
comparison <- combined_data %>%
  select(ransom, loss, downtime) %>%
  mutate(row_id = row_number()) %>%
  pivot_longer(-row_id, names_to = "variable", values_to = "imputed_value")

# Plot histograms
ggplot(comparison, aes(x = imputed_value)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Distribution of Imputed Numeric Variables")

densityplot(imp)

summary(combined_data[c("ransom", "loss", "downtime")])

#View finally cleaned data
table(combined_data$country)

# Notify 
# check frequency
notify_counts <- combined_data %>%
  filter(!is.na(notify)) %>%
  count(notify, sort = TRUE)
notify_counts





#Objective 4
#validate data type


