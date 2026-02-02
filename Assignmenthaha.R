#package
library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)


#import file
getwd()
list.files("C:/Users/Xenia Thor/Documents/")

data_csv1 <- read_csv("C:/Users/Xenia Thor/Documents/HackingData_Part1.xlsx")
data_xlsx2 <- read_excel("C:/Users/Xenia Thor/Documents/HackingData_Part2.xlsx")  
data_txt3  <- read_delim("C:/Users/Xenia Thor/Documents/HackingData_Part3.txt",
                         delim = "\t")


