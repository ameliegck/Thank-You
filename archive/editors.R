rm(list = ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, ggplot2, openxlsx, readxl, dplyr, fuzzyjoin, stringi)

if(Sys.info()["user"] %in% c("Amelie.Grosenick")){
  path_wd <- c("D:/Projektfolder1 (Miethe, Grosenick)/zz_AmelieMisc/")
  path_data <- c("D:/Projektfolder1 (Miethe, Grosenick)/zz_AmelieMisc/thankyou/data/")
}

setwd(path_wd)


###############################
#### To store final result
###############################

top5_editors <- data.table()

###############################
#### AER
###############################

# --- load the data on editors ---
data <- read_xlsx(file.path(path_data, "editors/editors_top5.xlsx"), sheet = "aer_toload")
data <- as.data.table(data)

# for current editors data# for current editors no start date available. set it to 6 years prior
data[year == "current", year := "2019-2025"]

data[, month := NULL]

# just keep name
data[, name := sub(",.*$", "", name)]

#clean name
data[, name := toupper(name)]
data[, name := gsub("[:']", "", name)] # remove ' and :
data[, name := trimws(name)]

data[, role := NULL]


# create start_year (year before - in year) and end_year (year after - in year)
# split on either - or – (normal or en dash)
data[, c("start_year", "end_year") := lapply(
  tstrsplit(year, "[-–]", fixed = FALSE), 
  as.integer
)]


# create panel. for each year between start and end year one row
data_panel <- data[, .(name, start_year, end_year)]

# Remove rows with missing start or end
data_panel <- data_panel[!is.na(start_year) & !is.na(end_year)]

# Expand panel row by row
data_panel <- data_panel[, .(year = seq(start_year[1], end_year[1])), by = name]

data_panel[, journal := "AER"]
top5_editors <- rbind(top5_editors, data_panel)

rm(data, data_panel)

###############################
#### JPE
###############################

data <- read_xlsx(file.path(path_data, "editors/editors_top5.xlsx"), sheet = "jpe_handcollected")
data <- as.data.table(data)

data <- data[, c("name", "role", "month", "year")]

data <- data[role == "Lead Editor" | role == "Editor",]
data[, month := NULL]

data <- unique(data)


#clean name
data[, name := toupper(name)]
data[, name := gsub("[:']", "", name)] # remove ' and :
data[, name := trimws(name)]

data[, role := NULL]

# already panel by how it was collected
data_panel <- data

data_panel[, journal := "JPE"]
top5_editors <- rbind(top5_editors, data_panel)


rm(data, data_panel)

###############################
#### Econometrica
###############################

data <- read_xlsx(file.path(path_data, "editors/editors_top5.xlsx"), sheet = "econometrica_editors")
data <- as.data.table(data)

setnames(data, old = c("Year", "Editor", "Co-Editors"),
         new = c("year", "editor", "co_editor"))

# get co-editors in their own rows

# clean the final "and"
data[, co_editor := gsub(" and ", ", ", co_editor)]

# split into rows
data <- data[, .(co_editor = trimws(unlist(strsplit(co_editor, ",")))), 
             by = .(year, editor)]

data <- data[co_editor != "",]

data <- melt(data, id.vars = "year",
             measure.vars = c("editor","co_editor"),
             variable.name = "role",
             value.name = "name")

data <- unique(data)
data[role == "co_editor", role := "co-editor"]


#clean name
data[, name := toupper(name)]
data[, name := gsub("[:']", "", name)] # remove ' and :
data[, name := trimws(name)]


# create start_year (year before - in year) and end_year (year after - in year)
# split on either - or – (normal or en dash)
data[, c("start_year", "end_year") := lapply(
  tstrsplit(year, "[-–]", fixed = FALSE), 
  as.integer
)]

# create panel. for each year between start and end year one row
data_panel <- data[, .(name, start_year, end_year)]

# Remove rows with missing start or end
data_panel <- data_panel[!is.na(start_year) & !is.na(end_year)]

# Expand panel row by row
data_panel <- data_panel[, .(year = seq(start_year[1], end_year[1])), by = name]
data_panel <- unique(data_panel)

data_panel[, journal := "ECONOMETRICA"]
top5_editors <- rbind(top5_editors, data_panel)


rm(data, data_panel)


###############################
#### RESTUD
###############################

data <- read_xlsx(file.path(path_data, "editors/editors_top5.xlsx"), sheet = "restud_handcollected")
data <- as.data.table(data)

data <- data[,1:5]

setnames(data, old = c("Name", "Uni", "Role", "Month", "Year"),
         new = c("name", "affiliation", "role", "month", "year"))

# keep only (lead editors)
data <- data[role == "Editor" | role == "Lead Editor",]

#clean name
data[, name := toupper(name)]
data[, name := gsub("[:']", "", name)] # remove ' and :
data[, name := trimws(name)]


# move affiliation into right column
data[, `:=`(
  affiliation = fifelse(
    grepl(",", name),
    sub("^[^,]+,\\s*", "", name),     # text after comma
    as.character(affiliation)          # ensure same type
  ),
  name = sub(",.*$", "", name)         # text before comma
)]
data[, affiliation := trimws(affiliation)]

# remove month (and affiliation for now) and make unique

data <- unique(data[, c("name", "year")])
data_panel <- data

data_panel[, journal := "RESTUD"]
top5_editors <- rbind(top5_editors, data_panel)


rm(data, data_panel)

###############################
#### QJE
###############################

data <- read_xlsx(file.path(path_data, "editors/editors_top5.xlsx"), sheet = "qje_handcollected")
data <- as.data.table(data)

data <- data[,1:4]


#clean name
data[, name := toupper(name)]
data[, name := gsub("[:']", "", name)] # remove ' and :
data[, name := trimws(name)]


# make  unique
data_panel <- data[, c("name", "year")]

data_panel[, journal := "QJE"]
top5_editors <- rbind(top5_editors, data_panel)

save(top5_editors, file = paste0(path_data, "gen/top5editors.RData"))
