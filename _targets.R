library(targets)
library(tarchetypes)
library(here)
source(here("code", "functions.R"))

options(tidyverse.quiet = TRUE)

tar_pipeline(
  tar_target(
    lsay2003_data,
    get_lsay2003_data()
  ) ,
  tar_target(
    lsay2015_data,
    get_lsay2015_data()
  ),
  tar_target(
    combined_data,
    combine_lsay(lsay2003_data, lsay2015_data)
  ),
  tar_target(
    alluvial,
    alluvial_plot(combined_data)
  ),
  tar_target(
    codebook,
    dataMaid::makeCodebook(combined_data %>% select(-schid),
                           file = here("documentation", "codebook.pdf"),
                           replace = TRUE,
                           render = FALSE,
                           standAlone = FALSE)
  ),
  tar_target(
    table_one,
    table_one(combined_data)
  ),
  tar_target(
    impute_data,
    mice_impute(combined_data)
  ),
  tar_target(
    data_long,
    data_long(impute_data)
  ),
  tar_target(
    belong_model,
    belong_models(data_long)
    ),
    tar_target(
      neet_model, 
      neet_models_m1(data_long)
    ),
  tar_target(
    neet_interaction,
    neet_models_m2(data_long)
  ),
  tar_target(
    neet_cohort,
    neet_models_m3(data_long)
  ),
  tar_target(
    neet_sch_grad,
    neet_models_m4(data_long)
  ),
  tar_target(
    sch_grad_model,
    neet_models_m5(data_long)
  ),
  tar_target(
    neet_socclass,
    neet_models_socclass(data_long)
  ),
  tar_target(
    neet_gee,
    neet_models_socclass(data_long)
  ),
  tar_target(
    neet_sch_ses,
    neet_models_sch_ses(data_long)
  ),
  tar_target(
    neet_sch_ach,
    neet_models_sch_ach(data_long)
  ),
  tar_target(
    mediation,
    mediation(neet_sch_grad,sch_grad_model)
  ),
  tar_target(
    miss_map,
    miss_map()
  )
)
