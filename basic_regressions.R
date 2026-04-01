rm(list = ls()) 

if(!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, tidyr, readr, readxl, ggplot2, RColorBrewer, stringr, stringi, stringdist, igraph) # add any packages here

if(Sys.info()["user"] %in% c("Amelie.Grosenick")){
  path_wd <- c("D:/Projektfolder1 (Miethe, Grosenick)/zz_AmelieMisc/")
  path_data <- c("D:/Projektfolder1 (Miethe, Grosenick)/zz_AmelieMisc/thankyou/data/")
  path_plots <- c("D:/Projektfolder1 (Miethe, Grosenick)/zz_AmelieMisc/thankyou/output/")
  
}

setwd(path_wd)


# This creates basic regressions for data

# ----------------------------------------------------
# Load data
# ----------------------------------------------------

# paper info
load(file.path(path_data, "gen/dt_paper_level.RData"))

# person level
load(file.path(path_data, "gen/dt_person_level.RData"))




################################
#### CREATE MORE VARS
###############################


dt_paper_level[, n_mentions_star := uniqueN(person_id[!is.na(person_id) & top5_repec == 1]), by = paper_id]
dt_paper_level[, n_mentions_female := uniqueN(person_id[!is.na(person_id) & d_female == 1]), by = paper_id]


################################
#### DATA DESCRIPTION
###############################

# number of unique people mentioned
uniqueN(dt_paper_level$person_id)

# number of unique papers observed
uniqueN(dt_paper_level$paper_id)



################################
#### TRANSFORM TO WIDE
###############################

# variables to keep

keeper <- c("paper_id", "title_paper", "publication_year", "journal_short", "top5", "field", "times_cited_wos_score", "times_cited_all_databases", "n_mentions", "n_mentions_star", "n_mentions_female")
dt_paper_wide <- unique(dt_paper_level[, ..keeper])

# change format to numeric
dt_paper_wide[, times_cited_wos_score := as.numeric(times_cited_wos_score)]
dt_paper_wide[, times_cited_all_databases := as.numeric(times_cited_all_databases)]
dt_paper_wide[, n_mentions := as.numeric(n_mentions)]
dt_paper_wide[, n_mentions_star := as.numeric(n_mentions_star)]
dt_paper_wide[, n_mentions_female := as.numeric(n_mentions_female)]



################################
#### REGRESSIONS
###############################

library(fixest)


# numerical outcome
m1 <- feols(
  times_cited_all_databases ~ n_mentions | field + publication_year,
  data = dt_paper_wide,
  cluster = ~field
)

# IHS specs
m2 <- feols(
  asinh(times_cited_all_databases) ~ n_mentions | field +  publication_year,
  data = dt_paper_wide,
  cluster = ~field
)

m3 <- feols(
  asinh(times_cited_all_databases) ~ n_mentions + n_mentions_star | field + publication_year,
  data = dt_paper_wide,
  cluster = ~field
)

m4 <- feols(
  asinh(times_cited_all_databases) ~ n_mentions + n_mentions_star + top5 | field + publication_year,
  data = dt_paper_wide,
  cluster = ~field
)


summary(m2)

# nicer way to show tables (and combine several models)
etable(m1, m2, m3, m4)

# export
etable(
  m1, m2, m3, m4,
  headers = c("Levels", "IHS", "IHS", "IHS + controls"),
  dict = c(
    n_mentions = "Mentions",
    n_mentions_star = "Mentions × Star",
    top5 = "Top 5 Journal"
  ),
  drop = "Intercept",
  stats = c("n", "r2"),
  cluster = "field"
)
etable(m1, m2, m3, m4, file = paste0(path_plots, "results.tex"))


# other outcome: top 5 publication

model <- feols(
  top5 ~ n_mentions | field + publication_year,
  data = dt_paper_wide,
  cluster = ~field
)
summary(model)



model <- feols(
  top5 ~ n_mentions + n_mentions_star | field + publication_year,
  data = dt_paper_wide,
  cluster = ~field
)
summary(model)




# other outcome: only itations across wos data bases

model <- feols(
  asinh(times_cited_wos_score) ~ n_mentions | field + publication_year,
  data = dt_paper_wide,
  cluster = ~field
)
summary(model)



model <- feols(
  asinh(times_cited_wos_score) ~ n_mentions + n_mentions_star | field + publication_year,
  data = dt_paper_wide,
  cluster = ~field
)
summary(model)

# add in author information, e.g. we know male authors receive more citations ...



