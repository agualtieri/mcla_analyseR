## MCLA 2019 Analysis
rm(list = ls())

#Load packages
library("koboquest")
library("xlsformfill")
library("hypegrammaR")
library("composr")
library("tidyverse")

browseVignettes("hypegrammaR")

devtools::install_github("mabafaba/hypegrammaR", force = T, build_vignettes = T)


# load questionnaire inputs
questions <- read.csv("input/questionnaire_questions.csv", 
                      stringsAsFactors=F, check.names=F)

# normalise
questions$name<-tolower(questions$name)
questions$relevant<-tolower(questions$relevant)
questions$calculation<-tolower(questions$calculation)

choices <- read.csv("input/questionnaire_choices.csv", 
                    stringsAsFactors=F, check.names=F)

# remove empty columns
choices <- choices[, colnames(choices)!=""]

# double check for inconsistencies
questionnaire_issues<-check_input(questions = questions, choices = choices)
questionnaire_issues %>% write.csv("./output/issues_with_questionnaire.csv", row.names = F)
browseURL("./output/issues_with_questionnaire.csv")



### Create fake dataset for testing
# sampling frame
samplingframe <- load_samplingframe("./input/sf.csv")

# make samplingframe tidy (one row per stratum)
samplingframe_tidy <- samplingframe %>% gather(key = "population_group",
                                               value = "population",
                                               nondisplaced,
                                               idps,
                                               returnees,
                                               refugees,
                                               migrants) %>%
  # select interesting columns only 
  select(govpcode,
         district.pcode,
         population_group,
         population,
         district)


# remove non existent combinations
samplingframe_tidy<- samplingframe_tidy %>% filter(!is.na(population))
# add stratum id to sampling frame
samplingframe_tidy<- samplingframe_tidy %>% mutate(stratum_id = paste0(district.pcode,"_",population_group))

# add district pcodes to choice list
sf_based_choices <- samplingframe_tidy %>% 
  mutate(list_name = "district") %>%
  rename(name = district.pcode,`label::English` = district) %>%
  select(list_name,name,`label::English`)


choices <- plyr::rbind.fill(choices, sf_based_choices)
choices <- choices[!duplicated(choices$name),]

# generate data
questions[questions$name %>% grepl("a3_me",.) %>% which,] %>% t
choices[choices$list_name=="district","name"]
response <- xlsform_fill(questions,choices,1000)


names(response)<- koboquest:::to_alphanumeric_lowercase(names(response))


# horizontal operations / recoding
# ... (none for now)

# vertical operations / aggregation

### .. should/can this move up to loading inputs?
questionnaire <- load_questionnaire(response,questions,choices)


### create stratm_id into the response dataset
## Recode a1_metadata
response <- response %>%
  new_recoding(data_stratum_id, source = a1_metadata) %>%
  recode_to("nondisplaced",where.selected.any = "a1_1") %>% 
  recode_to("refugees",where.selected.any = "a1_2") %>% 
  recode_to("idps",where.selected.any = "a1_3") %>% 
  recode_to("migrants",where.selected.any = "a1_4") %>%
  recode_to("returnees",where.selected.any = "a1_5") %>% 
  end_recoding()



response <- mutate(response, stratum_id = paste0(a3_metadata,"_",data_stratum_id))



### Weighting function
weight.function <- map_to_weighting(sampling.frame = samplingframe_tidy, 
                                    data.stratum.column = "stratum_id",
                                    sampling.frame.population.column = "population",
                                    sampling.frame.stratum.column = "stratum_id")

weight.function(response) %>% write.csv("weighted_dataset.csv") %>% browseURL("weighted_dataset.csv")




response_tidy <- response[(response$stratum_id %in% samplingframe_tidy$stratum_id),]

weight.function(response_tidy) %>% write.csv("weighted_dataset.csv", row.names = F)
browseURL("weighted_dataset.csv")


source("functions/analysisplan_factory.R")

analysis_plan <- make_analysisplan_all_vars(response, questionnaire, 
                                            repeat.for.variable = "a2_metadata",
                                            independent.variable = "a1_metadata")


response_analysis_output <- from_analysisplan_map_to_output(data = response_tidy,
                                                            weighting = weight.function,
                                                            analysisplan = analysis_plan,
                                                            questionnaire = questionnaire,
                                                            labeled = T,
                                                            verbose = T)



response_analysis_output$analysisplan
hypegrammaR:::map_to_generic_hierarchical_html(resultlist = response_analysis_output,
                                               render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                               by_analysisplan_columns = c("research.question","dependent.var"),
                                               by_prefix = c("RQ:", "indicator: "),
                                               level = 2,
                                               questionnaire = questionnaire,
                                               dir = ".",
                                               filename = "test_output.html")


map_to_master_table(response_analysis_output, "test.csv")
browseURL("test.csv")



args<-list(1,2,NA,na.rm=T)

response_analysis_output$results[[1]]$summary.statistic

response_analysis_output$results %>% 
  lapply(map_to_labeled,questionnaire) %>% lapply(function(x){x$summary.statistic}) %>% 
  do.call(rbind, .) %>% write.csv("all_results_labeled.csv")

browseURL("./all_results_labeled.csv")


analplan <- load_analysisplan(df = hypegrammaR::test_analysisplan)



rbind(list_of_sumstats[[1]],list_of_sumstats[[2]], ....)






