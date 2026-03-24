rm(list = ls()) 

if(!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, tidyr, readr, readxl, ggplot2, RColorBrewer) # add any packages here

if(Sys.info()["user"] %in% c("Amelie.Grosenick")){
  path_wd <- c("D:/Projektfolder1 (Miethe, Grosenick)/zz_AmelieMisc/")
  path_data <- c("D:/Projektfolder1 (Miethe, Grosenick)/zz_AmelieMisc/thankyou/data/")
}

setwd(path_wd)

#-----------------------------------------------
# Load data and combine into one data set
#-----------------------------------------------

files <- list.files(
  path = file.path(path_data, "webofscience"),
  pattern = "\\.txt$",  
  full.names = TRUE,
  recursive = TRUE
)

data_list <- lapply(files, function(file) {
  dt <- read_delim(
    file,
    show_col_types = FALSE
  )
  setDT(dt)
  return(dt)
})



wos_data <- rbindlist(data_list, use.names = TRUE, fill = TRUE)

# get column names
dt_names <- read_excel(file.path(path_data, "webofscience", "column_names.xls"), col_names = TRUE)
names <- colnames(dt_names)
names <- tolower(names)
names <- gsub(" ", "_", names)

setnames(wos_data, old = colnames(wos_data), new = names[1:71])

rm(data_list, dt_names)

# drop duplicates
wos_data <- unique(wos_data)

#-----------------------------------------------
# save all data and just the with texts (for ai)
#-----------------------------------------------

save(wos_data, file = file.path(path_data, "gen/wos_data_clean.RData"))

# data set that stores files with acknowledgments to put into AI

wos_data_ai <- wos_data[!is.na(funding_text), ]
names(wos_data_ai)

wos_data_ai <- wos_data_ai[, .(publication_type, authors, author_full_names, article_title, journal_abbreviation, doi, funding_text)]
View(wos_data_ai[is.na(doi)])

# save
save(wos_data_ai, file = file.path(path_data, "gen/wos_data_for_ai.RData"))

data.table::fwrite(
  wos_data_ai,
  file = file.path(path_data, "gen/wos_data_for_ai.csv")
)


