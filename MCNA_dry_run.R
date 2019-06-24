# setup

library(dplyr)
library(koboquest) # manage kobo questionnairs
library(kobostandards) # check inputs for inconsistencies
library(xlsformfill) # generate fake data for kobo
library(hypegrammaR) # simple stats 4 complex samples
library(composr) # horziontal operations
library(tidyr)

source("functions/to_alphanumeric_lowercase.R")
source("functions/analysisplan_factory.R")

# load questionnaire inputs
questions <- read.csv("input/questionnaire_questions.csv", 
                      stringsAsFactors=F, check.names=F)

# normalise (this should be done on the real questionnaire before collection!)
questions$name<-tolower(questions$name)
questions$relevant<-tolower(questions$relevant)
questions$calculation<-tolower(questions$calculation)
# double check for inconsistencies
questionnaire_issues<-check_input(questions = questions, choices = choices)
questionnaire_issues %>% write.csv("./output/issues_with_questionnaire.csv")
browseURL("./output/issues_with_questionnaire.csv")

# Choices sheet
choices <- read.csv("input/questionnaire_choices.csv", 
                    stringsAsFactors=F, check.names=F)
# remove empty columns
choices <- choices[, colnames(choices)!=""]


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

# problem (?): no sampling frame at subdistrict / location level ??? I thought we stratify on subdistrict!?
# also sampling frame only for 3 population groups!

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
response <- xlsform_fill(questions,choices,1000)
names(response)<- koboquest:::to_alphanumeric_lowercase(names(response))

# add cluster ids
# ...

# horizontal operations / recoding
# ... (none for now)

# vertical operations / aggregation

### .. should/can this move up to loading inputs?
questionnaire <- load_questionnaire(response,questions,choices)


# make analysisplan including all questions as dependent variable by HH type, repeated for each governorate:
analysisplan<-make_analysisplan_all_vars(response,
                                         questionnaire
                                         ,independent.variable = "a1_metadata",
                                         repeat.for.variable = "a4_metadata"
                                         )

# remove metadata we're not interested in analysing
analysisplan<-analysisplan %>% 
  filter(!grepl("metadata|start$|end$|deviceid$|uuid",dependent.variable)) 
# add matching stratum id to data: recode population group and concatenate with p code 
response <- response %>%
  new_recoding(data_stratum_id,source = a1_metadata) %>%
  recode_to("nondisplaced",where.selected.any = "a1_1") %>% 
  recode_to("refugees",where.selected.any = "a1_2") %>% 
  recode_to("idps",where.selected.any = "a1_3") %>% 
  # recode_to("???",where.selected.any = "a1_4") %>%
  # recode_to("???",where.selected.any = "a1_5") %>% 
  end_recoding()
  
response <- response %>% mutate(data_stratum_id = paste0(a3_metadata,"_",data_stratum_id))  

# percent matched in samplingframe?
# note that for real data this will of course be different
cat(crayon::red(mean(response$data_stratum_id %in% samplingframe_tidy$stratum_id) %>% multiply_by(100) %>% round(2) %>% paste0("% of records matched in samplingframe")))

# this line is dangerous. If we end up with missing strata, they're silently removed.
# could we instead kick out more specifically the impossible district/population group combos?
response <- response %>% 
  filter(data_stratum_id %in% samplingframe_tidy$stratum_id)

# now removed, should have all matches
cat(crayon::red(mean(response$data_stratum_id %in% samplingframe_tidy$stratum_id) %>% multiply_by(100) %>% round(2) %>% paste0("% of records matched in samplingframe")))


# create a weighting function from the sampling frame

strata_weight_fun <- map_to_weighting(sampling.frame = samplingframe_tidy,
                 sampling.frame.population.column = "population",
                 sampling.frame.stratum.column = "stratum_id",
                 data.stratum.column = "data_stratum_id")

# store global weights for log purpose only:
strata_weight_fun(response) %>% write.csv("general_weights.csv")

# create cluster id
# we might not need this
# r$cluster_id <- paste(r$cluster_location_id,r$type_hh,sep = "_")

result <- from_analysisplan_map_to_output(response, analysisplan = analysisplan,
                                          weighting = strata_weight_fun,
                                          cluster_variable_name = NULL,
                                          questionnaire)



# exporting only small part of results for speed during testing:
# subset_of_results<- rep(FALSE,length(results$results))
# subset_of_results[500:700]<-TRUE
# some_results<-hypegrammaR:::results_subset(results,logical = subset_of_results)

# not sure if this function should be "user facing" or have some wrappers (@Bouke thoughts?)
# essentially it handles all the looping over different column values as hierarchies.
# then each result is visualised by a function passed here that decides how to render each individual result
# see ?hypegrammaR:::map_to_generic_hierarchical_html
hypegrammaR:::map_to_generic_hierarchical_html(result,
                                               render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                               by_analysisplan_columns = c("dependent.var","repeat.var.value"),
                                               by_prefix =  c("",""),
                                               level = 2,
                                               questionnaire = questionnaire,
                                               label_varnames = TRUE,
                                               dir = "./output",
                                               filename = "summary_by_dependent_var_then_by_repeat_var.html"
                                               )

browseURL("./output/summary_by_dependent_var_then_by_repeat_var.html")
