## MCLA 2019 Analysis
rm(list = ls())

# Install packages
# devtools::install_github("mabafaba/hypegrammaR", force = T, build_vignettes = T)
# devtools::install_github("mabafaba/xlsformfill", force = T, build_vignettes = T)
# devtools::install_github("mabafaba/composr", force = T, build_vignettes = T)

#Load packages
library("xlsformfill")
library("hypegrammaR")
library("composr")
library("tidyverse")
library("kableExtra")

#browseVignettes("hypegrammaR")



# load questionnaire inputs
questions <- read.csv("input/questions.csv", 
                      stringsAsFactors=F, check.names=F)

# normalise
questions$name<-tolower(questions$name)
questions$relevant<-tolower(questions$relevant)
questions$calculation<-tolower(questions$calculation)
questions$type<-tolower(questions$type)

choices <- read.csv("input/choices_named.csv", 
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
samplingframe <- load_samplingframe("./input/sf_governorate.csv")

# make samplingframe tidy (one row per stratum)
samplingframe_tidy <- samplingframe %>% gather(key = "population_group",
                                               value = "population",
                                               idps,
                                               migrants,
                                               nondisplaced,
                                               refugees,
                                               returnees) %>%
  # select interesting columns only 
  select(governorate_code, governorate_name, population_group, population )


# remove non existent combinations
samplingframe_tidy<- samplingframe_tidy %>% filter(!is.na(population))
# add stratum id to sampling frame
samplingframe_tidy<- samplingframe_tidy %>% mutate(stratum_id = paste0(governorate_code,"_",population_group))
# remove zeros
samplingframe_tidy<- filter(samplingframe_tidy, population >0)


# add district pcodes to choice list
#sf_based_choices <- samplingframe_tidy %>% 
 # mutate(list_name = "governorate") %>%
#  rename(name = governorate_code,`label::English` = governorate_name) %>%
 # select(list_name, name,`label::English`)


#choices <- plyr::rbind.fill(choices, sf_based_choices)
#choices <- choices[!duplicated(choices$name),]

# generate data
#questions[questions$name %>% grepl("a3_me",.) %>% which,] %>% t
#choices[choices$list_name=="district","name"]


response <- xlsform_fill(questions,choices, 50)


names(response)<- koboquest:::to_alphanumeric_lowercase(names(response))


# horizontal operations / recoding
# ... (none for now)

# vertical operations / aggregation

### Load the questionnare
questionnaire <- load_questionnaire(response,questions,choices,choices.label.column.to.use = "label::English")


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



response <- mutate(response, stratum_id = paste0(a2_metadata,"_",data_stratum_id))



### Weighting function
weight.function <- map_to_weighting(sampling.frame = samplingframe_tidy, 
                                    data.stratum.column = "stratum_id",
                                    sampling.frame.population.column = "population",
                                    sampling.frame.stratum.column = "stratum_id")

#weight.function(response) %>% write.csv("weighted_dataset.csv") %>% browseURL("weighted_dataset.csv")


### Delete non-matching obs (it shouldn't happen with a real dataset)
response <- response[(response$stratum_id %in% samplingframe_tidy$stratum_id),]

weight.function(response) %>% write.csv("./output/weighted_dataset.csv", row.names = F) 
browseURL("./output/weighted_dataset.csv")

### Delete unnecesary columns
response <- response %>% select(-c("start", "end", "deviceid", "formname"))

### Create an analysis plan, define the case, and run the analysis function
#source("functions/analysisplan_factory.R")

#analysis_plan <- make_analysisplan_all_vars(response, questionnaire, 
                                            #repeat.for.variable = "a1_metadata",
                                            #independent.variable = "a2_metadata")

#write.csv(analysis_plan, "./output/test_analysisplan.csv")

analysis_plan <- read.csv("./input/test_analysis_plan_v2.csv", 
                          stringsAsFactors=F, check.names=F)


response_analysis_output <- from_analysisplan_map_to_output(data = response,
                                                            weighting = weight.function,
                                                            analysisplan = analysis_plan,
                                                            questionnaire = hypegrammaR::load_questionnaire(response, questions, choices, choices.label.column.to.use = "label::English"),
                                                            labeled = T,
                                                            verbose = T)


### Create bulk two-way tables from analysis output
# response_analysis_output[["analysisplan"]][["sub.research.question"]]



twowaytables <- response_analysis_output$results %>% lapply(map_to_table)

filenames <- paste0("MCLA_analysis_output_", response_analysis_output[["analysisplan"]][["sub.research.question"]], ".xlsx") 

response_tables <- purrr::map2(twowaytables, filenames, xlsx::write.xlsx)





### Easier to read .html file for analysis presentation
response_analysis_output$analysisplan
hypegrammaR:::map_to_generic_hierarchical_html(resultlist = response_analysis_output,
                                               render_result_with = my_custom_md_table, 
                                               by_analysisplan_columns = c("research.question","sub.research.question"),
                                               by_prefix = c("RQ:", "Indicator: "),
                                               level = 2,
                                               questionnaire = load_questionnaire(response, questions, choices, choices.label.column.to.use = "label::English"),
                                               dir = "./output",
                                               filename = "test_output.html")

# hypegrammaR:::from_result_map_to_md_table, 
my_custom_md_table

### Map to master table for pivoting and other cross indicator analysis
response_analysis_output$results[[1]]$summary.statistic

response_analysis_output$results %>% lapply(map_to_labeled,questionnaire) %>% 
                                 lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .) %>%
                                                                          write.csv("./output/all_results_labeled.csv", row.names = F)

browseURL("./output/all_results_labeled.csv")







