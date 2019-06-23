## MCLA 2019 Analysis

#Load packages
library("koboquest")
library("xlsformfill")
library("hypegrammaR")
library("composr")

# Loading kobo files, dataframe, and sampling frame
kobo_choices <- read.csv("input/questionnaire_choices.csv", stringsAsFactors = F)
kobo_questions <- read.csv("input/questionnaire_questions.csv", stringsAsFactors = F )
sampling_frame <- read.csv("input/sf.csv", stringsAsFactors = F)
dataset <- load_data("input/main_dataset_v1.csv")

# Prepare questionnaire object
questionnaire <- load_questionnaire(dataset, kobo_questions, kobo_choices)
browseVignettes("hypegrammaR")
