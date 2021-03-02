# library calls ####
library(purrr)
library(curl)
library(XML)
library(readit)
library(tidyverse)
library(labelled)
library(corrr)
library(psych)
library(mice)
library(ggeffects)
library(lme4)
library(JuliaCall)
library(psych)
library(broom.mixed)
library(patchwork)
library(dataMaid)
library(gtsummary)
library(dataverse)
library(ggalluvial)
library(dataMaid)
library(mice)
library(gtsummary)
library(tidyMB)
library(mitml)
# Global functions ####
vcov.geeglm <- function(x) summary(x)$cov.scaled

# Create versions of functions with na.rm=TRUE as default
Funs <- Filter(is.function,sapply(ls(baseenv()),get,baseenv()))
na.rm.f <- names(Filter(function(x) any(names(formals(args(x)))%in% 'na.rm'),Funs))
na.rm.f <- c(na.rm.f, "mean", "sd")
# Create strings. Dot "." is optional
fs <- lapply(na.rm.f,
             function(x) paste0(".", x, "=purrr::partial(", x ,", na.rm = T)"))
eval(parse(text = fs)) 

.scale <- function(x){(x - .mean(x))/.sd(x)}
# Cloudstor helper files
cloud_get <- function(user, password, cloud_address, dest){
  p = file.path(tempdir(), dest)
  h = curl::new_handle()
  curl::handle_setopt(h, username = user)
  curl::handle_setopt(h, password = password) 
  curl::curl_download(cloud_address, p, handle = h)
  d = readit::readit(p)
  return(d)
}
#get class codes
egp <- scct::conversion_table %>%
  select(HISEI = isei, egp) %>%
  mutate(across(everything(), ~as.numeric(.))) %>%
  as_tibble() %>%
  distinct() %>%
  group_by(HISEI) %>%
  mutate(egp = .min(egp)) %>%
  ungroup() %>%
  filter(!is.na(HISEI))
# Get cloud files
cloud_list <- function(username, password, relPath = "/", dav = "https://cloudstor.aarnet.edu.au/plus/remote.php/webdav/") {
  uri <- URLencode(paste(dav, relPath, sep=""))
  # fetch directory listing via curl and parse XML response
  h <- new_handle()
  handle_setopt(h, customrequest = "PROPFIND")
  handle_setopt(h, username = username)
  handle_setopt(h, password = password)
  response <- curl_fetch_memory(uri, h)
  text <- rawToChar(response$content)
  doc <- xmlParse(text, asText=TRUE)
  # calculate relative paths
  base <- paste(paste("/", strsplit(uri, "/")[[1]][-1:-3], sep="", collapse=""), "/", sep="")
  result <- unlist(
    xpathApply(doc, "//d:response/d:href", function(node) {
      sub(base, "", URLdecode(xmlValue(node)), fixed=TRUE)
    })
  )
  result[result != ""]
}

cloud_put <- function(filename, inPath, username, password, relPath = "", dav = "https://cloudstor.aarnet.edu.au/plus/remote.php/webdav/") {
  uri <- URLencode(paste(dav, relPath, filename, sep=""))
  print(uri)
  inFile <- paste(path.expand(inPath), filename, sep="")
  print(inFile)
  httr::PUT(uri, body = upload_file(inFile), config = authenticate(username, password))
}

jmer <- function(formula, data, family="Bernoulli()"){
  jf <- deparse(formula,width = 500)
  
  julia_assign("jmerdat",data)
  julia_assign("form", formula(jf))
  julia_command(glue::glue("jmermod = fit(MixedModel, form,jmerdat, {family}, fast=true)") )
  
  julia_eval("robject(:glmerMod, Tuple([jmermod,jmerdat]));",need_return="R")
}

# targets functions ####
# data requests ####
get_lsay2003_data <- function(){
  lsay2003 = cloud_get(user = cloudstor_user,
                       password = cloudstor_password,
                       dest = 'lsay2003.sav',
                       cloud_address = 'https://cloudstor.aarnet.edu.au/plus/remote.php/webdav/Databases/LSAY-v10/LSAY2003/lsay2003.sav') %>%
    select(belong = BELONG,
           sex = SEX,
           indig = INDIG,
           loc = LOC, 
           immig = IMMIG,
           escs = ESCS,
           HISEI,
           math1 = PV1MATH,
           math2 = PV2MATH,
           math3 = PV3MATH,
           math4 = PV4MATH,
           math5 = PV5MATH,
           read1 = PV1READ,
           read2 = PV2READ,
           read3 = PV3READ,
           read4 = PV4READ,
           read5 = PV5READ,
           sci1 = PV1SCIE,
           sci2 = PV2SCIE,
           sci3 = PV3SCIE,
           sci4 = PV4SCIE,
           sci5 = PV5SCIE,
           schid = SCHOOLID,
           xlfs2004 = XLFS2004,
           xlfs2005 = XLFS2005,
           xlfs2006 = XLFS2006,
           xlfs2007 = XLFS2007,
           xcel2004 = XCEL2004,
           xcel2005 = XCEL2005,
           xcel2006 = XCEL2006,
           xcel2007 = XCEL2007,
           xcsl2004 = XCSL2004,
           xcsl2005 = XCSL2005,
           xcsl2006 = XCSL2006,
           xcsl2007 = XCSL2007,
           sch_grad1 = X1222004,
           sch_grad2 = X1222005,
           sch_grad3 = X1222006,
           sch_grad4 = X1222007,
           starts_with("ST27Q0")
    ) %>%
    mutate(across(math1:sci5, .scale)) %>%
    mutate(HISEI = replace(HISEI, HISEI > 96, NA_real_)) %>%
    left_join(.,egp) %>%
    select(-HISEI) %>%
    mutate(egp = case_when(
      egp < 3 ~ "salariat",
      egp == 3 ~ "intermediate",
      is.na(egp) ~ NA_character_,
      TRUE ~ "working"
    ) %>% as.factor()) %>%
    relocate(egp,.before = escs) %>%
    mutate(across(starts_with('xlfs'), ~replace(., .> 90, NA_integer_))) %>%
    mutate(across(starts_with('sch_grad'), ~replace(., .> 90, NA_integer_)-1)) %>%
    mutate(across(starts_with('sch_grad'), ~ifelse(.==1, 0, 1))) %>%
    mutate(belong = replace(belong, belong > 900, NA_integer_),
           escs = replace(escs, escs > 900, NA_integer_),
           loc = ifelse(loc == 1, 1, 0),
           immig = replace(immig, immig > 3, NA_real_),
           immig = ifelse(immig == 1, 0, 1),
           pc1 = principal(.[,c('math1','sci1','read1')])$score %>% as.vector,
           pc2 = principal(.[,c('math2','sci2','read2')])$score %>% as.vector,
           pc3 = principal(.[,c('math3','sci3','read3')])$score %>% as.vector,
           pc4 = principal(.[,c('math4','sci4','read4')])$score %>% as.vector,
           pc5 = principal(.[,c('math5','sci5','read5')])$score %>% as.vector,
           neet1 = case_when(
             xlfs2004 > 1 & xcel2004 == 10 & xcsl2004 == 6 ~ 1,
             is.na(xlfs2004) ~ NA_real_,
             is.na(xcel2004) ~ NA_real_,
             is.na(xcsl2004) ~ NA_real_,
             TRUE ~ 0
           ),
           neet2 = case_when(
             xlfs2005 > 1 & xcel2005 == 10 & xcsl2005 == 6 ~ 1,
             is.na(xlfs2005) ~ NA_real_,
             is.na(xcel2005) ~ NA_real_,
             is.na(xcsl2005) ~ NA_real_,
             TRUE ~ 0
           ),
           neet3 = case_when(
             xlfs2006 > 1 & xcel2006 == 10 & xcsl2006 == 6 ~ 1,
             is.na(xlfs2006) ~ NA_real_,
             is.na(xcel2006) ~ NA_real_,
             is.na(xcsl2006) ~ NA_real_,
             TRUE ~ 0
           ),
           neet4 = case_when(
             xlfs2007 > 1 & xcel2007 == 10 & xcsl2007 == 6 ~ 1,
             is.na(xlfs2007) ~ NA_real_,
             is.na(xcel2007) ~ NA_real_,
             is.na(xcsl2007) ~ NA_real_,
             TRUE ~ 0
           )
    ) %>%
    mutate(across(starts_with("ST27Q0"), ~replace(.,.>4, NA_real_))) %>%
    group_by(schid) %>%
    mutate(across(pc1:pc5, .mean,.names = "sch_{.col}")) %>%
    mutate(sch_escs = .mean(escs)) %>%
    ungroup() %>%
    select(-xlfs2004:-xcsl2007)
  
  rel = lsay2003 %>% select(starts_with("ST27Q0")) %>%
    psych::alpha(check.keys = TRUE)
  
  print(rel)
  
  lsay2003 %>% 
    select(starts_with("ST27Q0")) %>% psych::fa.parallel()
  
  
  lsay2003 = lsay2003 %>% select(-starts_with("ST27Q0"))
    
  
  return(lsay2003)
}

get_lsay2015_data <- function(){
  lsay2015 = cloud_get(user = cloudstor_user,
                       password = cloudstor_password,
                       dest = 'lsay2003.sav',
                       cloud_address = 'https://cloudstor.aarnet.edu.au/plus/remote.php/webdav/lsay/lsay2015_v4.sav') %>%
    select(belong = BELONG,
           sex = ST004D01T,
           indig = INDIG,
           immig = IMMIG,
           loc = `GEOLOC_3`, 
           escs = ESCS,
           HISEI,
           math1 = PV1MATH,
           math2 = PV5MATH,
           math3 = PV10MATH,
           math4 = PV8MATH,
           math5 = PV2MATH,
           read1 = PV1READ,
           read2 = PV5READ,
           read3 = PV10READ,
           read4 = PV8READ,
           read5 = PV2READ,
           sci1 = PV1SCIE,
           sci2 = PV5SCIE,
           sci3 = PV10SCIE,
           sci4 = PV8SCIE,
           sci5 = PV2SCIE,
           schid = CNTSCHID,
           xlfs2016 = XLFS2016,
           xlfs2017 = XLFS2017,
           xlfs2018 = XLFS2018,
           xlfs2019 = XLFS2019,
           xcel2016 = XCEL2016,
           xcel2017 = XCEL2017,
           xcel2018 = XCEL2018,
           xcel2019 = XCEL2019,
           xcsl2016 = XCSL2016,
           xcsl2017 = XCSL2017,
           xcsl2018 = XCSL2018,
           xcsl2019 = XCSL2019,
           sch_grad1 = X1222016,
           sch_grad2 = X1222017,
           sch_grad3 = X1222018,
           sch_grad4 = X1222019,
           starts_with("ST034Q0")
    ) %>%
    mutate(across(math1:sci5, .scale)) %>%
    mutate(HISEI = replace(HISEI, HISEI > 96, NA_real_)) %>%
    left_join(.,egp) %>%
    select(-HISEI) %>%
    mutate(egp = case_when(
      egp < 3 ~ "salariat",
      egp == 3 ~ "intermediate",
      is.na(egp) ~ NA_character_,
      TRUE ~ "working"
    ) %>% as.factor()) %>%
    relocate(egp,.before = escs) %>%
    mutate(across(starts_with('xlfs'), ~replace(., .> 90, NA_integer_))) %>%
    mutate(across(starts_with('sch_grad'), ~replace(., .> 90, NA_integer_)-1)) %>%
    mutate(across(starts_with('sch_grad'), ~ifelse(.==1, 0, 1))) %>%
    mutate(indig = as.numeric(as.factor(indig)),
           immig = replace(immig, immig > 3, NA_integer_),
           immig = ifelse(immig == 1, 0, 1)) %>%
    mutate(belong = replace(belong, belong > 900, NA_integer_),
           escs = replace(escs, escs > 900, NA_integer_),
           loc = ifelse(loc == 1, 1, 0),
           pc1 = principal(.[,c('math1','sci1','read1')])$score %>% as.vector,
           pc2 = principal(.[,c('math2','sci2','read2')])$score %>% as.vector,
           pc3 = principal(.[,c('math3','sci3','read3')])$score %>% as.vector,
           pc4 = principal(.[,c('math4','sci4','read4')])$score %>% as.vector,
           pc5 = principal(.[,c('math5','sci5','read5')])$score %>% as.vector,
           schid = as.character(schid),
           neet1 = case_when(
             xlfs2016 > 1 & xcel2016 == 10 & xcsl2016 == 6 ~ 1,
             is.na(xlfs2016) ~ NA_real_,
             is.na(xcel2016) ~ NA_real_,
             is.na(xcsl2016) ~ NA_real_,
             TRUE ~ 0
           ),
           neet2 = case_when(
             xlfs2017 > 1 & xcel2017 == 10 & xcsl2017 == 6 ~ 1,
             is.na(xlfs2017) ~ NA_real_,
             is.na(xcel2017) ~ NA_real_,
             is.na(xcsl2017) ~ NA_real_,
             TRUE ~ 0
           ),
           neet3 = case_when(
             xlfs2018 > 1 & xcel2018 == 10 & xcsl2018 == 6 ~ 1,
             is.na(xlfs2018) ~ NA_real_,
             is.na(xcel2018) ~ NA_real_,
             is.na(xcsl2018) ~ NA_real_,
             TRUE ~ 0
           ),
           neet4 = case_when(
             xlfs2019 > 1 & xcel2019 == 10 & xcsl2019 == 6 ~ 1,
             is.na(xlfs2019) ~ NA_real_,
             is.na(xcel2019) ~ NA_real_,
             is.na(xcsl2019) ~ NA_real_,
             TRUE ~ 0
           ),
           indig = replace(indig, indig > 2, NA)-1
    ) %>%
    mutate(across(starts_with("ST034Q0"), ~replace(.,.>4, NA_real_))) %>%
    group_by(schid) %>%
    mutate(across(pc1:pc5, .mean,.names = "sch_{.col}")) %>%
    mutate(sch_escs = .mean(escs)) %>%
    ungroup() %>%
    select(-xlfs2016:-xcsl2019)
  
  rel = lsay2015 %>% select(starts_with("ST034Q0")) %>%
    psych::alpha(check.keys = TRUE)
  
  print(rel)
  
  lsay2015 %>% select(starts_with("ST034Q0")) %>% psych::fa.parallel()
  
  lsay2015 = lsay2015 %>% select(-starts_with("ST034Q0"))
  
  
  return(lsay2015)
}

combine_lsay <- function(lsay2003, lsay2015){
  lsay_combined = bind_rows(lsay2003, lsay2015,.id = "cohort") %>%
    mutate(cohort = ifelse(cohort == 1, "cohort_2003", "cohort_2015")) %>%
    select(-math1:-sci5) %>%
    mutate(across(everything(),haven::zap_labels)) %>%
    mutate(across(everything(),haven::zap_label)) %>%
    mutate(sex = as.factor(sex),
           loc = as.factor(loc),
           immig = as.factor(immig),
           indig = as.factor(indig),
           cohort = as.factor(cohort)
    ) %>%
    mutate(across(neet1:neet4, as.factor))
  return(lsay_combined)
}

alluvial_plot <- function(data){
  p1 <- data %>%
    dplyr::select(starts_with('neet'), cohort) %>%
    drop_na() %>%
    dplyr::group_by(neet1, neet2,neet3, neet4, cohort) %>%
    dplyr::count() %>%
    ungroup()%>%
    filter(neet1 == 1 | neet2 == 1 | neet3 == 1 | neet4 == 1) %>%
    mutate(across(neet1:neet4, ~ifelse(. == 1, 'NEET', 'non-Neet'))) %>%
    set_names(c(paste('NEET Year',1:4), 'cohort', 'n')) %>%
    mutate(cohort = str_replace(cohort,"cohort_", "Cohort: ")) %>%
    unite("comb", `NEET Year 1`:`NEET Year 4`,remove = FALSE) %>%
    mutate(comb = as.factor(comb)) %>% 
    ggplot(
      aes(y = n, axis1 = `NEET Year 1`, axis2 = `NEET Year 2`, axis3 = `NEET Year 3`, axis4 = `NEET Year 4`)) +
    geom_flow(aes(fill = `NEET Year 1`)) +
    geom_stratum(width = 1/12)+
    geom_label(stat = "stratum", 
               aes(label = after_stat(stratum),color = after_stat(stratum))) +
    facet_wrap(~cohort,ncol=1,scales = 'free') +
    tidyMB::theme_mb() +
    theme(legend.position = 'none') +
    ggtitle("Flow of NEET Participants", "Year 1 (Grade 9-10) to Year 4 (Post-school 1-2)") +
    labs(y = "")
  return(p1)
}

table_one <- function(data){
  t1 <- data %>%
    mutate(across(starts_with('neet'), ~as.numeric(.)-1))%>% 
    mutate(ever_neet = neet1+neet2+neet3+neet4,
           ever_neet = ifelse(ever_neet > 0, "NEET", "not NEET")) %>% 
    select(ever_neet, Girl = sex, Indigenous = indig,`Immigrant Background` = immig,
           Urban = loc, SES = escs, Achievement = pc1,
           `School Avg Achievement` = sch_pc1,
           `School Avg. SES` = sch_escs) %>%
    mutate(Girl = ifelse(Girl == 2, 'Girl', 'Boy'),
           Indigenous = ifelse(Indigenous == 1, 'Indigenous', 'non-Indigenous'),
           `Immigrant Background` = ifelse(`Immigrant Background` == 1, 'Immigrant', 'non-Immigrnat'),
           Urban = ifelse(Urban == 1, 'Urban', 'Provincial')) %>%
    tbl_summary(by = ever_neet) %>%
    add_p() 
  
  return(t1)
}

mice_impute <- function(data) {
  ini <- mice(data,maxit=0)
  pred1 <- ini$predictorMatrix
  pred1[,'schid'] <- 0
  
  lsay_combined_imputed <- mice(data, pred = pred1)
  
  return(lsay_combined_imputed)
}

data_long <- function(data){
  data_long = data %>% complete(action = 'long', include = TRUE) %>% 
    mutate(across(starts_with("sch_grad"), as.factor)) %>% 
    group_by(.imp) %>% 
    rename(id = '.id') %>%
    pivot_longer(cols = c(starts_with('neet'), starts_with('sch_grad')) ) %>% 
    mutate(variable = str_extract(name, '[a-z_]+'), year = str_extract(name, '[0-9]')) %>%
    select(-name) %>%
    pivot_wider(names_from = variable, values_from = value) %>% 
    mutate(year = year %>% as.numeric) %>%
    rownames_to_column(var = '.id') %>%
    ungroup()
  
  data_long = data_long %>%
    group_split(`.imp`)
  
  data_long2 = list()
  for (i in seq_along(data_long)){
    if(i == 1){j =1
    }else{j = i-1}
    var = glue::glue("pc{j}")
    sch_var = glue::glue("sch_pc{j}")
    tmp = data_long[[i]] %>%
      select(everything(),
             pc = var,
             sch_pc = sch_var)
    
    data_long2[[i]] = tmp
    
  }
  
  data_long2 = data_long2 %>%
    bind_rows() %>%
    mice::as.mids() %>%
    mitml::mids2mitml.list()
  
  return(data_long2)
}

belong_models <- function(data) {
  m_belong_1 <- with(data, lme4::lmer(belong~pc+escs+sex+loc+indig+immig+sch_escs+sch_pc+cohort+
                                             (1|schid),subset=year==1))
  
  m_belong_2 <- with(data, lme4::lmer(belong~(pc+escs+sex+loc+indig+immig)*cohort+sch_escs+sch_pc+
                                             (1|schid),subset=year==1))
  
  
  anova = mitml::testModels(model=m_belong_2, null.model=m_belong_1, method="D1")

  results = list(M1 = m_belong_1, M2 = m_belong_2, comparison = anova)
}


neet_models_m1 <- function(data){
  julia <- julia_setup()
  julia$library("MixedModels")
  julia$library("JellyMe4")
  
  fit <- list()
  
  for ( i in 1:5){
    d = data[[i]] %>%
      dplyr::mutate(id = as.factor(id),
             schid = as.factor(schid))
    
    fit[[i]] = jmer(formula(neet ~ belong+year+pc+escs+sex+loc+indig+immig+sch_escs+sch_pc+cohort+(1|id)+(1|schid)), d)
  }
  
  res <- mitml::testEstimates(fit, var.comp = TRUE)
  
  out <- list(fit = fit, summary = res)
  
  return(out)
   
}

neet_models_m2 <- function(data){
  julia <- julia_setup()
  julia$library("MixedModels")
  julia$library("JellyMe4")
  
  fit <- list()
  
  for ( i in 1:5){
    d = data[[i]] %>%
      dplyr::mutate(id = as.factor(id),
                    schid = as.factor(schid))
    
    fit[[i]] = jmer(formula(neet ~ year+(pc+escs+sex+loc+indig+immig)*belong+sch_escs+sch_pc+cohort+(1|id)+(1|schid)), d)
  }
  
  res <- mitml::testEstimates(fit, var.comp = TRUE)
  
  out <- list(fit = fit, summary = res)
  
  return(out)
  
}

neet_models_m3 <- function(data){
  julia <- julia_setup()
  julia$library("MixedModels")
  julia$library("JellyMe4")
  
  fit <- list()
  
  for ( i in 1:5){
    d = data[[i]] %>%
      dplyr::mutate(id = as.factor(id),
                    schid = as.factor(schid))
    
    fit[[i]] = jmer(formula(neet ~ year+(pc+escs+sex+loc+indig+immig+belong)*cohort+sch_escs+sch_pc+(1|id)+(1|schid)), d)
  }
  
  res <- mitml::testEstimates(fit, var.comp = TRUE)
  
  out <- list(fit = fit, summary = res)
  
  return(out)
  
}

neet_models_m4 <- function(data){
  julia <- julia_setup()
  julia$library("MixedModels")
  julia$library("JellyMe4")
  
  fit <- list()
  
  for ( i in 1:5){
    d = data[[i]] %>%
      dplyr::mutate(id = as.factor(id),
                    schid = as.factor(schid))
    
    fit[[i]] = jmer(formula(neet ~ sch_grad+year+pc+escs+sex+loc+indig+immig+belong+cohort+sch_escs+sch_pc+(1|id)+(1|schid)), d)
  }
  
  res <- mitml::testEstimates(fit, var.comp = TRUE)
  
  out <- list(fit = fit, summary = res)
  
  return(out)
  
}


neet_models_m5 <- function(data){
  julia <- julia_setup()
  julia$library("MixedModels")
  julia$library("JellyMe4")
  
  fit <- list()
  
  for ( i in 1:5){
    d = data[[i]] %>%
      dplyr::mutate(id = as.factor(id),
                    schid = as.factor(schid))
    
    fit[[i]] = jmer(formula(sch_grad ~ year+pc+escs+sex+loc+indig+immig+belong+cohort+sch_escs+sch_pc+(1|id)+(1|schid)), d)
  }
  
  res <- mitml::testEstimates(fit, var.comp = TRUE)
  
  out <- list(fit = fit, summary = res)
  
  return(out)
  
}

gee_model <- function(data){
  require(geepack)
  fit <- list()
  
  for ( i in 1:5){
    d = data[[i]] %>%
      dplyr::mutate(id = as.factor(id),
                    schid = as.factor(schid),
                    neet = as.numeric(neet)-1)
    
    fit[[i]] = geeglm(neet ~ belong+year+pc+escs+sex+loc+indig+immig+sch_escs+sch_pc+cohort, id = id, data = d,family = binomial)
  }
  
  res <- mitml::testEstimates(fit, var.comp=TRUE)
  
  out <- list(fit = fit, summary = res)
}


neet_models_socclass <- function(data){
  julia <- julia_setup()
  julia$library("MixedModels")
  julia$library("JellyMe4")
  
  fit <- list()
  
  for ( i in 1:5){
    d = data[[i]] %>%
      dplyr::mutate(id = as.factor(id),
                    schid = as.factor(schid))
    
    fit[[i]] = jmer(formula(neet ~ belong+year+pc+egp+sex+loc+indig+immig+sch_escs+sch_pc+cohort+(1|id)+(1|schid)), d)
  }
  
  res <- mitml::testEstimates(fit, var.comp = TRUE)
  
  out <- list(fit = fit, summary = res)
  
  return(out)
  
}


neet_models_sch_ses <- function(data){
  julia <- julia_setup()
  julia$library("MixedModels")
  julia$library("JellyMe4")
  
  fit <- list()
  
  for ( i in 1:5){
    d = data[[i]] %>%
      dplyr::mutate(id = as.factor(id),
                    schid = as.factor(schid))
    
    fit[[i]] = jmer(formula(neet ~ belong+year+pc+escs+sex+loc+indig+immig+sch_escs+cohort+(1|id)+(1|schid)), d)
  }
  
  res <- mitml::testEstimates(fit, var.comp = TRUE)
  
  out <- list(fit = fit, summary = res)
  
  return(out)
  
}


neet_models_sch_ach <- function(data){
  julia <- julia_setup()
  julia$library("MixedModels")
  julia$library("JellyMe4")
  
  fit <- list()
  
  for ( i in 1:5){
    d = data[[i]] %>%
      dplyr::mutate(id = as.factor(id),
                    schid = as.factor(schid))
    
    fit[[i]] = jmer(formula(neet ~ belong+year+pc+escs+sex+loc+indig+immig+sch_pc+cohort+(1|id)+(1|schid)), d)
  }
  
  res <- mitml::testEstimates(fit, var.comp = TRUE)
  
  out <- list(fit = fit, summary = res)
  
  return(out)
  
}

