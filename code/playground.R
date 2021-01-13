library(curl)
library(XML)
library(readit)


listFiles <- function(username, password, relPath = "/", dav = "https://cloudstor.aarnet.edu.au/plus/remote.php/webdav/") {
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

listFiles(username="philip.parker@acu.edu.au", password=pw, relPath = 'Databases/LSAY-v10/LSAY2003/')



cloud_get <- function(user, password, cloud_address, dest){
  p = file.path(tempdir(), dest)
  h = curl::new_handle()
  curl::handle_setopt(h, username = user)
  curl::handle_setopt(h, password = password) 
  
  curl::curl_download(cloud_address, p, handle = h)
  
  d = readit::readit(p)
  
  return(d)
  
}

library(tidyverse)
library(corrr)

lsay2003_small <- lsay2003 %>%
  select(belong = BELONG,
         sex = SEX,
         indig = INDIG,
         loc = LOC, 
         escs = ESCS,
         math = PV1MATH,
         read = PV1READ,
         sci = PV1SCIE,
         schid = SCHOOLID,
         xlfs = XLFS2006,
         xcel = XCEL2006) %>%
  mutate(across(math:sci, .scale)) %>%
  mutate(belong = replace(belong, belong > 900, NA_integer_),
         escs = replace(escs, escs > 900, NA_integer_),
         xlfs = replace(xlfs, xlfs > 90, NA_integer_),
         loc = ifelse(loc == 1, 1, 0),
         neet = case_when(
           xlfs > 1 & xcel == 10 ~ 1,
           is.na(xlfs) ~ NA_real_,
           is.na(xcel) ~ NA_real_,
           TRUE ~ 0
         )) %>%
  select(-xlfs, -xcel) %>%
  group_by(schid) %>%
  mutate(across(escs:sci, .mean,.names = "sch_{.col}")) %>%
  ungroup()

prop.table(table(lsay2003_small$neet,useNA = 'always'))

lsay2003_cor <- lsay2003_small %>%
  select(-schid) %>%
  correlate(use = "pairwise.complete.obs",diagonal = 1) %>%
  stretch()

library(ggeffects)
library(lme4)

model <- glm(neet~belong, data=lsay2003_small, family=binomial)
ggeffect(model, terms = "belong [-2,0,2]")

model <- glm(neet~belong+math+sci+read+escs+sex+loc+indig, data=lsay2003_small, family=binomial)
ggeffect(model, terms = "belong [-2,0,2]")

model <- glmer(neet~belong+math+sci+read+escs+sex+loc+indig+(1|schid), data=lsay2003_small, family=binomial)
ggeffect(model, terms = "belong [-2,0,2]")


model <- glmer(neet~belong+math+sci+read+escs+sex+loc+indig+sch_math+sch_escs+(1|schid), data=lsay2003_small, family=binomial)
summary(model)
ggeffect(model, terms = "belong [-2,0,2]")

