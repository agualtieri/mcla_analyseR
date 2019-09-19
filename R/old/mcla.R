# Load first
library(dplyr)
library(koboquest) # manage kobo questionnairs
library(kobostandards) # check inputs for inconsistencies
library(xlsformfill) # generate fake data for kobo
library(hypegrammaR) # simple stats 4 complex samples
library(composr) # horziontal operations
library(tidyr)


# Loading kobo files, dataframe, and sampling frame
choices <- read.csv("input/questionnaire_choices.csv", stringsAsFactors = F)
questions <- read.csv("input/questionnaire_questions.csv", stringsAsFactors = F )
sampling_frame <- read.csv("input/sf.csv", stringsAsFactors = F)
dataset <- read.csv("input/main_dataset_v2.csv", stringsAsFactors = F)

# Prepare questionnaire object
questionnaire <- load_questionnaire(dataset, questions, choices)

sampling_frame_long <- gather(sampling_frame, key = "population_group",
                              value = "population",
                              NonDisplaced, IDPs, Returnees, Refugees, Migrants)

sampling_frame_long <- select(sampling_frame_long, 
                              c("Gov", "District", "GovPcode", "District.Pcode", "TOTAL", "population_group", "population"))

sampling_frame_long <- mutate(sampling_frame_long, 
                              weight.id = paste0(District.Pcode, population_group))


dataset$YE <- "YE"

dataset1 <- mutate(dataset, weight.id = paste0(YE, A3_Metadata))

dataset1 <- dataset1 %>%
  new_recoding(data_stratum_id, source = A1_Metadata) %>%
  recode_to("NonDisplaced",where.selected.any = "a1_1") %>% 
  recode_to("Refugees",where.selected.any = "a1_2") %>% 
  recode_to("IDPs",where.selected.any = "a1_3") %>% 
  recode_to("Migrants",where.selected.any = "a1_4") %>%
  recode_to("Returnees",where.selected.any = "a1_5") %>% 
  end_recoding()

dataset1 <- mutate(dataset1, weight.id = paste0(YE, A3_Metadata, data_stratum_id))

weight.function <- map_to_weighting(sampling.frame = sampling_frame_long, 
                                    data.stratum.column = "weight.id",
                                    sampling.frame.population.column = "population",
                                    sampling.frame.stratum.column = "weight.id")

weight.function(dataset1) %>% write.csv("weighted_dataset.csv") %>% browseURL("weighted_dataset.csv") #not working becase Migrants were entered in areas where the are not present according to the sampling frame

