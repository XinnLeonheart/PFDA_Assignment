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
library(Matrix)
library(xgboost)

data_csv1 <- read_csv("D:/APU/Y2 Sem 3/Programming for Data Analysis/Assignment/AssignmentDatasets/HackingData_Part1.csv")
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
combined_data$webserver[combined_data$webserver == ""] <- NA
combined_data$webserver[combined_data$webserver == "na"] <- NA
combined_data$webserver[combined_data$webserver == "null"] <- NA
combined_data$webserver[combined_data$webserver == "&quot;&quot;"] <- NA
# remove obvious non-webserver junk patterns
combined_data$webserver[
  str_detect(combined_data$webserver, "<script>|\\*\\*\\*\\*\\*|^[+]$|^\\d{1,3}(\\.\\d{1,3}){3}$")
] <- NA
count(combined_data, webserver)

#Encoding
# CONVERT ENCODING COLUMN'S DATA TO LOWERCASE
combined_data$encoding <- tolower(combined_data$encoding)
combined_data$encoding <- trimws(combined_data$encoding)
combined_data$encoding[combined_data$encoding %in% c("", "na", "null")] <- NA
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



# IMPUTE NA
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

# deal with quantitative data first: column ransom, downtime, loss
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

# deal with qualitative column
# notify
# reduce cardinality
top_notify <- combined_data %>%
  filter(!is.na(notify)) %>%
  count(notify, sort = TRUE) %>%
  slice_head(n = 50) %>%
  pull(notify)

combined_data$notify[is.na(combined_data$notify)] <- "unknown"

train_idx <- !is.na(combined_data$notify)

X_df <- combined_data %>%
  select(country, webserver, encoding, ransom, downtime, loss)

X_df <- X_df %>%
  mutate(
    country   = ifelse(is.na(country),   "unknown_country", country),
    webserver = ifelse(is.na(webserver), "unknown_webserver", webserver),
    encoding  = ifelse(is.na(encoding),  "unknown_encoding", encoding),
    ransom    = ifelse(is.na(ransom),    -1, ransom),
    downtime  = ifelse(is.na(downtime),  -1, downtime),
    loss      = ifelse(is.na(loss),      -1, loss)
  )

X_sparse <- sparse.model.matrix(~ . - 1, data = X_df)

nrow(X_sparse)       # should equal nrow(combined_data)
length(train_idx)    # should equal nrow(combined_data)

X_train <- X_sparse[train_idx, ]
X_miss  <- X_sparse[!train_idx, ]

# url
# no use oh no need clean

# ip


# country
# Prepare data
country_train_idx <- !is.na(combined_data$country)

X_df <- combined_data %>%
  select(webserver, encoding, ransom, downtime, loss) %>%
  mutate(
    webserver = ifelse(is.na(webserver), "unknown_webserver", webserver),
    encoding  = ifelse(is.na(encoding),  "unknown_encoding", encoding),
    ransom    = ifelse(is.na(ransom),    -1, ransom),
    downtime  = ifelse(is.na(downtime),  -1, downtime),
    loss      = ifelse(is.na(loss),      -1, loss)
  )

X_sparse <- sparse.model.matrix(~ . - 1, data = X_df)

y <- as.integer(factor(combined_data$country[country_train_idx])) - 1
num_class <- length(unique(y))

# Train model
dtrain <- xgb.DMatrix(X_sparse[country_train_idx, ], label = y)

params <- list(
  objective = "multi:softprob",
  num_class = num_class,
  max_depth = 6,
  eta = 0.2,
  subsample = 0.8,
  colsample_bytree = 0.8,
  eval_metric = "mlogloss"
)

country_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,
  verbose = 0
)

# Predict missing country
miss_idx <- is.na(combined_data$country)
dmiss <- xgb.DMatrix(X_sparse[miss_idx, ])
pred <- predict(country_model, dmiss)

pred_mat <- matrix(pred, ncol = num_class, byrow = TRUE)
pred_class <- max.col(pred_mat) - 1

country_levels <- levels(factor(combined_data$country[country_train_idx]))
combined_data$country[miss_idx] <- country_levels[pred_class + 1]

# check
sum(is.na(combined_data$country))   # should be 0
count(combined_data, country, sort = TRUE)


# webserver
# prepare predictors
webserver_train_idx <- !is.na(combined_data$webserver)

X_df_ws <- combined_data %>%
  select(country, encoding, ransom, downtime, loss) %>%
  mutate(
    country  = ifelse(is.na(country),  "unknown_country",  country),
    encoding = ifelse(is.na(encoding), "unknown_encoding", encoding),
    ransom   = ifelse(is.na(ransom),   -1, ransom),
    downtime = ifelse(is.na(downtime), -1, downtime),
    loss     = ifelse(is.na(loss),     -1, loss)
  )

X_sparse_ws <- sparse.model.matrix(~ . - 1, data = X_df_ws)

# Train XGBoost multi-class model for WebServer
y_ws <- as.integer(factor(combined_data$webserver[webserver_train_idx])) - 1
num_class_ws <- length(unique(y_ws))

dtrain_ws <- xgb.DMatrix(X_sparse_ws[webserver_train_idx, ], label = y_ws)

params_ws <- list(
  objective = "multi:softprob",
  num_class = num_class_ws,
  max_depth = 6,
  eta = 0.2,
  subsample = 0.8,
  colsample_bytree = 0.8,
  eval_metric = "mlogloss"
)

webserver_model <- xgb.train(
  params = params_ws,
  data = dtrain_ws,
  nrounds = 100,
  verbose = 0
)

# Predict missing WebServer 
miss_ws <- is.na(combined_data$webserver)
dmiss_ws <- xgb.DMatrix(X_sparse_ws[miss_ws, ])

pred_ws <- predict(webserver_model, dmiss_ws)
pred_mat_ws <- matrix(pred_ws, ncol = num_class_ws, byrow = TRUE)
pred_class_ws <- max.col(pred_mat_ws) - 1

webserver_levels <- levels(factor(combined_data$webserver[webserver_train_idx]))
combined_data$webserver[miss_ws] <- webserver_levels[pred_class_ws + 1]

sum(is.na(combined_data$webserver))  

# encoding
# train split mask
enc_train_idx <- !is.na(combined_data$encoding)
# build features
X_df_enc <- combined_data %>%
  select(country, webserver, ransom, downtime, loss) %>%
  mutate(
    country   = ifelse(is.na(country),   "unknown_country",   country),
    webserver = ifelse(is.na(webserver), "unknown_webserver", webserver),
    ransom    = ifelse(is.na(ransom),    -1, ransom),
    downtime  = ifelse(is.na(downtime),  -1, downtime),
    loss      = ifelse(is.na(loss),      -1, loss)
  )

X_sparse_enc <- sparse.model.matrix(~ . - 1, data = X_df_enc)
# encode target labels
y_enc <- as.integer(factor(combined_data$encoding[enc_train_idx])) - 1
num_class_enc <- length(unique(y_enc))

dtrain_enc <- xgb.DMatrix(X_sparse_enc[enc_train_idx, ], label = y_enc)
# train multiclass model
set.seed(123)

params_enc <- list(
  objective = "multi:softprob",
  num_class = num_class_enc,
  max_depth = 6,
  eta = 0.2,
  subsample = 0.8,
  colsample_bytree = 0.8,
  eval_metric = "mlogloss"
)

enc_model <- xgb.train(
  params = params_enc,
  data = dtrain_enc,
  nrounds = 100,
  verbose = 0
)
# predict missing rows and fill NA
miss_enc <- is.na(combined_data$encoding)
dmiss_enc <- xgb.DMatrix(X_sparse_enc[miss_enc, ])

pred_enc <- predict(enc_model, dmiss_enc)
pred_mat_enc <- matrix(pred_enc, ncol = num_class_enc, byrow = TRUE)
pred_class_enc <- max.col(pred_mat_enc) - 1

enc_levels <- levels(factor(combined_data$encoding[enc_train_idx]))
combined_data$encoding[miss_enc] <- enc_levels[pred_class_enc + 1]

sum(is.na(combined_data$encoding))  
count(combined_data, encoding, sort = TRUE)

# Check overall result
str(cleaned_data)
summary(cleaned_data)

# Export cleaned data to CSV
write_csv(combined_data, "cleaned_data.csv")




#Objective 4
#validate data type


