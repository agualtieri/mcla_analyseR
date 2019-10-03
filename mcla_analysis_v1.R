## MCLA 2019 Analysis
rm(list = ls())

# Install packages (please uncomment these lines the first time you run the code)
# devtools::install_github("mabafaba/hypegrammaR", force = T, build_vignettes = T)
# devtools::install_github("mabafaba/xlsformfill", force = T, build_vignettes = T)
# devtools::install_github("mabafaba/composr", force = T, build_vignettes = T)

#Load packages
library("xlsformfill")
library("hypegrammaR")
library("composr")
library("tidyverse")
library("kableExtra")


# Source
source("./R/cleanHead.R")
source("./R/md_to_custom_table.R")


# load questionnaire inputs
questions <- read.csv("input/questions.csv", 
                      stringsAsFactors=F, check.names=F)

# normalise
questions$name<-tolower(questions$name)
questions$relevant<-tolower(questions$relevant)
questions$calculation<-tolower(questions$calculation)
questions$type<-tolower(questions$type)

choices <- read.csv("input/choices_named_v2.csv", 
                    stringsAsFactors=F, check.names=F)

# remove empty columns
choices <- choices[, colnames(choices)!=""]

# double check for inconsistencies
#questionnaire_issues<-check_input(questions = questions, choices = choices)
#questionnaire_issues %>% write.csv("./output/issues_with_questionnaire.csv", row.names = F)
#browseURL("./output/issues_with_questionnaire.csv")


## Add "YE" to locations
# response$a2_metadata <- paste0("YE", response$a2_metadata)



### Create fake dataset for testing
# sampling frame
samplingframe <- load_samplingframe("./input/sf_district.csv")

# make samplingframe tidy (one row per stratum)
samplingframe <- samplingframe %>% gather(key = "population_group",
                                               value = "population",
                                               idps,
                                               migrants,
                                               nondisplaced,
                                               refugees,
                                               returnees) %>%
  # select interesting columns only 
  select(district_code, district_name, population_group, population )


# remove non existent combinations
samplingframe <- samplingframe %>% filter(!is.na(population))
# add stratum id to sampling frame
samplingframe <- samplingframe %>% mutate(stratum_id = paste0(district_code,"_",population_group))
# remove zeros
samplingframe <- filter(samplingframe, population >0)


# Add district pcodes to choice list
  #sf_based_choices <- samplingframe_tidy %>% 
  #mutate(list_name = "governorate") %>%
  #rename(name = governorate_code,`label::English` = governorate_name) %>%
  #select(list_name, name,`label::English`)


# choices <- plyr::rbind.fill(choices, sf_based_choices)
# choices <- choices[!duplicated(choices$name),]

# Generate dummy data for testing
# questions[questions$name %>% grepl("a3_me",.) %>% which,] %>% t
# choices[choices$list_name=="district","name"]
# response <- xlsform_fill(questions,choices, 27)


# Load Pre-Test sample data
response <- read.csv("input/MCLA_pre test_response.csv", stringsAsFactors = F)
names(response)<- koboquest:::to_alphanumeric_lowercase(names(response))

### Delete unnecesary columns
response <- response %>% select(-c("start", "end", "deviceid", "x_tags", "x_notes", "x__version__", "meta.instanceid", "meta.instancename"))

# Truncate column headers so that they can be read by the script
response <- cleanHead(response)

# horizontal operations / recoding
# ... (none for now)

# vertical operations / aggregation

### Load the questionnare
questionnaire <- load_questionnaire(response,questions,choices,choices.label.column.to.use = "label::REACH")


### create stratm_id into the response dataset
## Recode a1_metadata
response <- response %>%
  new_recoding(data_stratum_id, source = a1_metadata) %>%
  recode_to("nondisplaced",where.selected.any = "a1_1") %>% 
  recode_to("idps",where.selected.any = "a1_2") %>% 
  recode_to("returnees",where.selected.any = "a1_3") %>% 
  recode_to("refugees",where.selected.any = "a1_4") %>%
  recode_to("migrants",where.selected.any = "a1_5") %>% 
  end_recoding()

response <- mutate(response, stratum_id = paste0(a3_metadata,"_",data_stratum_id))


### Weighting function
weight.function <- map_to_weighting(sampling.frame = samplingframe, 
                                    data.stratum.column = "stratum_id",
                                    sampling.frame.population.column = "population",
                                    sampling.frame.stratum.column = "stratum_id")

#weight.function(response) %>% write.csv("weighted_dataset.csv") %>% browseURL("weighted_dataset.csv")


### Delete non-matching obs (it shouldn't happen with a real dataset)
response <- response[(response$stratum_id %in% samplingframe$stratum_id),]

weight.function(response) %>% write.csv("./output/weighted_dataset.csv", row.names = F) 
browseURL("./output/weighted_dataset.csv")

# Truncate column headers so that they can be read by the script
response <- cleanHead(response)

### Create an analysis plan, define the case, and run the analysis function
#source("functions/analysisplan_factory.R")

#analysis_plan <- make_analysisplan_all_vars(response, questionnaire, 
                                            #repeat.for.variable = "a1_metadata",
                                            #independent.variable = "a2_metadata")

#write.csv(analysis_plan, "./output/test_analysisplan.csv")


analysis_plan <- read.csv("./input/test_analysis_plan_v3.csv", 
                          stringsAsFactors=F, check.names=F)


response_analysis_output <- from_analysisplan_map_to_output(data = response,
                                                            weighting = weight.function,
                                                            analysisplan = analysis_plan,
                                                            questionnaire = hypegrammaR::load_questionnaire(response, questions, choices, choices.label.column.to.use = "label::REACH"),
                                                            labeled = T,
                                                            verbose = T)



### Create bulk two-way tables from analysis output
twowaytables <- response_analysis_output$results %>% lapply(map_to_table)

names <- list(name = c("a1_1", "a1_2", "a1_3", "a1_4", "a1_5"),
             label = c("nondisplaced", "idps", "returnees", "refugees", "migrants")) %>% as_tibble
                  

filenames <- paste0("MCLA_analysis_output_", response_analysis_output$analysisplan$sub.research.question,"_",
                    names$label[match(response_analysis_output$analysisplan$repeat.var.value, names$name)], ".xlsx") 

purrr::map2(twowaytables, filenames, xlsx::write.xlsx)
  


### Easier to read .html file for analysis presentation
response_analysis_output$analysisplan
hypegrammaR:::map_to_generic_hierarchical_html(resultlist = response_analysis_output,
                                               render_result_with = my_custom_md_table,
                                               by_analysisplan_columns = c("research.question","sub.research.question"),
                                               by_prefix = c("RQ:", "Indicator: "),
                                               level = 2,
                                               questionnaire = load_questionnaire(response, questions, choices, choices.label.column.to.use = "label::REACH"),
                                               dir = "./output",
                                               filename = "test_output.html")

# hypegrammaR:::from_result_map_to_md_table, 
# my_custom_md_table

### Map to master table for pivoting and other cross indicator analysis
response_analysis_output$results[[1]]$summary.statistic

response_analysis_output$results %>% lapply(map_to_labeled,questionnaire) %>% 
                                 lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .) %>%
                                                                          write.csv("./output/all_results_labeled.csv", row.names = F)

browseURL("./output/all_results_labeled.csv")







