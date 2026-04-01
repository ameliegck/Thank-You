rm(list = ls()) 

if(!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, tidyr, readr, readxl, ggplot2, RColorBrewer, stringr, stringi, stringdist, igraph) # add any packages here

if(Sys.info()["user"] %in% c("Amelie.Grosenick")){
  path_wd <- c("D:/Projektfolder1 (Miethe, Grosenick)/zz_AmelieMisc/")
  path_data <- c("D:/Projektfolder1 (Miethe, Grosenick)/zz_AmelieMisc/thankyou/data/")
  path_plots <- c("D:/Projektfolder1 (Miethe, Grosenick)/zz_AmelieMisc/thankyou/output/")
  
}

setwd(path_wd)


#
# Clean Data and combine into working data set
# One one person level and one on paper level
#


# ----------------------------------------------------
# Load and merge data
# ----------------------------------------------------
# paper info
load(file.path(path_data, "gen/wos_data_for_ai.RData"))


# info ChatGBT extracted from boxes (commentators and editors)
ai_extracted_1 <- fread(paste0(path_data, "gen/ai_extracted_names_wos.csv"))
ai_extracted_2 <- fread(paste0(path_data, "gen/ai_extracted_names_wos_other_journals.csv"))
ai_extracted_3 <- fread(paste0(path_data, "gen/ai_extracted_names_wos_other_journals_2.csv"))
ai_extracted_4 <- fread(paste0(path_data, "gen/ai_extracted_names_wos_other_journals_3.csv"))

ai_extracted <- rbind(ai_extracted_1, ai_extracted_2, ai_extracted_3, ai_extracted_4, fill = T)

rm(ai_extracted_1, ai_extracted_2, ai_extracted_3, ai_extracted_4)


# merge title_paper (wos_data_ai) and file_name (ai_extracted)
dt_paper_level <- merge(wos_data_ai, ai_extracted, by.x = "title_paper", by.y = "file_name", all.x = T)

# check all papers in wos_data_ai are in data_merged by unique titles
if (uniqueN(dt_paper_level$title_paper) != uniqueN(ai_extracted$file_name)) {
  warning("The number of unique titles in data_merged and wos_data_ai does not match!")
}


# merge further data from papers
load(file.path(path_data, "gen/wos_data_clean.RData"))

dt_paper_level <- merge(dt_paper_level, wos_data[,c("title_paper", "publication_year", "times_cited,_wos_core", "times_cited,_all_databases")], by = "title_paper", all.x = T)
setnames(dt_paper_level, old = c("times_cited,_wos_core", "times_cited,_all_databases"), new = c("times_cited_wos_score", "times_cited_all_databases"))

rm(ai_extracted, wos_data_ai, wos_data)

# ----------------------------------------------------
# Restrict sample
# ----------------------------------------------------

# years
dt_paper_level <- dt_paper_level[publication_year >= 2015 & publication_year <= 2025]

# drop editors
dt_paper_level <- dt_paper_level[grepl("editor", type, ignore.case = TRUE), person_name := NA]
uniqueN(dt_paper_level$title_paper)

# drop anonymous referees 
dt_paper_level <- dt_paper_level[grepl("referee", person_name, ignore.case = TRUE), person_name := NA]

# set whitespace to NA
dt_paper_level[person_name == "", person_name := NA]

dt_paper_level <- unique(dt_paper_level)

# ----------------------------------------------------
# Create and adjust variables
# ----------------------------------------------------

# id
dt_paper_level[, paper_id := .GRP, by = title_paper]

# how many people mentioned by paper
dt_paper_level[, n_mentions := uniqueN(person_name[!is.na(person_name)]), by = paper_id]

# length of funding text
dt_paper_level[, length_text := nchar(funding_text)]



# ----------------------------------------------------
# Load and clean data for individuals (REPEC)
# ----------------------------------------------------

# find all csv files and load them into one data set
files <- list.files(
  path = file.path(path_data, "repec"),
  pattern = "\\.csv$",  
  full.names = TRUE,
  recursive = TRUE
)

data_list <- lapply(files, function(file) {
  dt <- fread(file)
  return(dt)
})

repec_data <- rbindlist(data_list, use.names = TRUE, fill = TRUE)
repec_data <- unique(repec_data)
setDT(repec_data)
rm(data_list, files)

# clean
repec_data <- repec_data[Rank != "Rank",]
repec_data[, Rank := NULL]
setnames(repec_data, old = c("Author", "Score"), new = c("name_orig", "score"))

repec_data[, name := str_replace_all(name_orig, "[^[:alpha:]\\s.-]", "")]
repec_data[, name := str_squish(name)]

# by name, compute average score per year and turn into annual data
repec_data[, avg_score := mean(as.numeric(score), na.rm = TRUE), by = .(name, year)]
repec_data[, month := NULL]
repec_data[, score := NULL]

repec_data <- unique(repec_data)



# ----------------------------------------------------
# Clean names (REPEC and acknowledged people)
# Create unique person ID
# ----------------------------------------------------

# list of all people acknowledged
dt_people <- unique(dt_paper_level[!is.na(person_name), .(person_name)])
dt_people <- dt_people[, orig_name := person_name]
setnames(dt_people, old = "person_name", new = "name")

# clean names in the same way
clean_names <- function(dt, col_name) {
  dt[, (col_name) := get(col_name) |>
       stri_trans_general("Latin-ASCII") |>                  # remove accents
       str_replace_all("\\b(Jr|Sr)\\.?\\b", "") |>           # remove Jr. / Sr. (case-insensitive if needed)
       str_replace_all("[.,]", "") |>                        # remove dots and commas
       str_replace("[^[:alpha:]\\s-]+$", "") |>              # remove trailing symbols like †
       str_squish() |>                                       # clean extra spaces
       str_replace_all("(?<=\\s)[A-Za-z](?=\\s)", "") |>     # remove single-letter middle initials
       str_squish()                                         # clean leftover double spaces
  ]
  return(dt)
}

dt_people <- clean_names(dt_people, "name")
repec_data <- clean_names(repec_data, "name")


# some manual adjustment

manual_adjustments <- function(funcdata, name_col = "name") {
  funcdata <- copy(funcdata)
  
  # Exact replacements
  funcdata[get(name_col) == "Larry Katz", (name_col) := "Lawrence Katz"]
  funcdata[get(name_col) == "Jim Hines", (name_col) := "James Hines"]
  funcdata[get(name_col) == "Rob Porter", (name_col) := "Robert Porter"]
  funcdata[get(name_col) == "BobWilson", (name_col) := "Robert Wilson"]
  funcdata[get(name_col) == "Josh Angrist", (name_col) := "Joshua Angrist"]
  funcdata[get(name_col) == "7111no Santos", (name_col) := "Tano Santos"]
  
  funcdata[get(name_col) == "A Colin Cameron", (name_col) := "Colin Cameron"]
  funcdata[get(name_col) == "A Michael Spence", (name_col) := "Michael Spence"]
  funcdata[get(name_col) == "A Mitchell Polinsky", (name_col) := "Mitchell Polinsky"]
  funcdata[get(name_col) == "A Patrick Minford", (name_col) := "Patrick Minford"]
  funcdata[get(name_col) == "A Ronald Gallant", (name_col) := "Ronald Gallant"]
  funcdata[get(name_col) == "R. Chetty", (name_col) := "Raj Chetty"]
  funcdata[get(name_col) == "Andres Rodrguez-Clare", (name_col) := "Andres Rodriguez-Clare"]
  funcdata[get(name_col) == "A. Finkelstein", (name_col) := "Amy Finkelstein"]
  funcdata[get(name_col) == "IvanWerning", (name_col) := "Ivan Werning"]
  funcdata[get(name_col) == "StevenMorris", (name_col) := "Steven Morris"]
  funcdata[get(name_col) == "Darell Duffie", (name_col) := "Darrell Duffie"]
  funcdata[get(name_col) == "M Mogstad", (name_col) := "Magne Mogstad"]
  
  
  
  
  
  # Prefix replacements
  funcdata[get(name_col) %like% "^Bob ", (name_col) := str_replace(get(name_col), "^Bob ", "Robert ")]
  funcdata[get(name_col) %like% "^Bobby ", (name_col) := str_replace(get(name_col), "^Bobby ", "Robert ")]
  funcdata[get(name_col) %like% "^Rob ", (name_col) := str_replace(get(name_col), "^Rob ", "Robert ")]
  funcdata[get(name_col) %like% "^Larry ", (name_col) := str_replace(get(name_col), "^Larry ", "Lawrence ")]
  funcdata[get(name_col) %like% "^Matt ", (name_col) := str_replace(get(name_col), "^Matt ", "Matthew ")]
  funcdata[get(name_col) %like% "^Dan ", (name_col) := str_replace(get(name_col), "^Dan ", "Daniel ")]
  funcdata[get(name_col) %like% "^Mike ", (name_col) := str_replace(get(name_col), "^Mike ", "Michael ")]
  funcdata[get(name_col) %like% "^Jerpme ", (name_col) := str_replace(get(name_col), "^Jerpme ", "Jerome ")]
  
   
  return(funcdata)
}

dt_people <- manual_adjustments(dt_people, "name")
repec_data <- manual_adjustments(repec_data, "name")
dt_paper_level <- manual_adjustments(dt_paper_level, "person_name")



# within dt_people give them unique identifier
# fuzzy string match to find names that are the same person
# but spelled differently (eg with and without middle name)


# Unique names first
unique_names <- unique(dt_people$name)

# Compute distance matrix (Jaro-Winkler)
dist_matrix <- stringdistmatrix(unique_names, unique_names, method = "jw")


# Create adjacency matrix for names with distance < 0.1
adj <- dist_matrix < 0.1
diag(adj) <- FALSE  # remove self-loops

# Build graph and find connected components (groups)
g <- graph_from_adjacency_matrix(adj, mode = "undirected")
clusters <- components(g)

# Assign group ID to each unique name
name_groups <- data.table(
  name = unique_names,
  person_id = clusters$membership
)

#count if person_id appears more than once
name_groups[, name_count := .N, by = person_id]
View(name_groups[name_count > 1])

# some people get same ID but are distinct people. fix manually
# TO DO: EXTEND! Not all are in here

manual_fix_names <- c(
  "Robert Miller", "Robert Shiller", "Thomas Craemer", "Christopher Walters",
  "Christopher Waller", "Christopher Taber", "Christopher Palmer", "Christopher Phelan",
  "Marco Scarsini", "Marco Marini", "Johannes Hirner", "Johannes Hoerner", "Michael Koelle",
  "Michael Kremer", "Michael Keane", "Michael Keen", "Michael Kearns", "Michael Kromer",
  "Micheal Kremer", "Emily Beam", "Philippe Weil", "Paolo Sodini", "Christopher Timmins",
  "B Forest", "Jian Sun", "Ying Chen", "Yi Chen", "Li Chen", "Ting Chen", "Yajing Chen",
  "Yu Chen", "Martin Meier", "Marti Mestieri", "Federico Bugni", "Antonio Merlo",
  "Hong Ma", "Hong Liu", "Heng Liu", "Tong Li", "Andreas Kern", "Marco Pagano", "Jennifer Cao",
  
  # Michael cluster
  "Michael Klein", "Michael Krause", "Michael Strain", "Michael Hoel", "Michael Powell",
  "Michael Hoy", "Michael Wong", "Michael Fleming", "Michael Shin", "Michael Fehlings",
  "Michael Sockin", "Michael Song", "Michael Rebello", "Micheal Keane", "Michael Konig",
  "Michael Kiley", "Michael Koenig", "Michael Lechner", "Michael Peress", "Michael Peters",
  "Michael Jetter", "Michael Lebacher", "Michael Reiter", "Michael Becher", "Michael Bechtel",
  "Michael Reher", "Michael Gechter",
  
  # Wang
  "Pengfei Wang", "Peng Wang", "Neng Wang", "Ping Wang", "Rong Wang", "Ying Wang", "Cong Wang",
  
  # Li / Liu / Lu / Lin / Xu / Sun
  "Jia Li", "Jian Li", "Qi Liu", "Yan Li", "Ying Li", "Jin Li", "Yang Lu", "Nan Li",
  "Qiao Liu", "Jiasun Li", "Xing Li", "Yan Liu", "Yang K. Lu", "Yang Su", "Yi Li", "Yi Lin",
  "Ming Li", "Jing Li", "Ming Lu", "Mingzhi Xu", "Yang Li", "Bin Li", "Bin Liu", "Qian Li",
  "Xin Liu", "Yang Liu", "Qing Liu", "Nan Liu", "Qihong Liu", "Yanan Li", "Ming Xu",
  "Yicong Lin", "Jaisun Li", "Ting Xu", "Xin Lu", "Yang Sun",
  
  "Dan Clark"
)

  # Step 1: set person_id to NA for these names
  name_groups[name %in% manual_fix_names, person_id := NA]
  
  # Step 2: find the next available ID
  next_id <- max(name_groups$person_id, na.rm = TRUE) + 1
  
  # Step 3: assign new IDs to the NA rows
  name_groups[is.na(person_id), person_id := seq(next_id, length.out = .N)]
  
  # Step 4: verify
  name_groups[, .N, by = person_id]



#merge person_id to dt_people
dt_people <- merge(dt_people, name_groups[, .(name, person_id)], by = "name", all.x = TRUE)
uniqueN(dt_people$person_id)



# save
people_id <- copy(dt_people)
save(people_id, file = file.path(path_data, "gen/acknowledged_with_person_id.RData"))

rm(unique_names, dist_matrix, adj, g, clusters, name_groups, people_id)


# ----------------------------------------------------
# Merge person_id to REPEC Data
# ----------------------------------------------------

# quite complex because fuzzyjoin package not available for t his R version??

# ----------------------------
# Function: Fuzzy merge RePEc names to dt_people
# ----------------------------

fuzzy_merge_people <- function(dt_people, repec_data, name_col = "name", threshold = 0.1) {
  
  # Make copies
  dt_people <- copy(dt_people)
  repec_data <- copy(repec_data)
  
  # -----------------------------
  # Keep only unique names in dt_people (for efficiency)
  # -----------------------------
  dt_people <- unique(dt_people[, .(person_id, name_clean = get(name_col))])
  
  # Keep original order of repec_data
  repec_data[, row_index := .I]
  
  # Add first character column for first-round filtering
  dt_people[, first_char := toupper(substr(name_clean, 1, 1))]
  repec_data[, first_char := toupper(substr(get(name_col), 1, 1))]
  
  results <- list()
  letters_set <- unique(repec_data$first_char)
  
  # -----------------------------
  # First round: fuzzy match by first letter
  # -----------------------------
  for (ch in letters_set) {
    people_subset <- dt_people[first_char == ch]
    repec_subset <- repec_data[first_char == ch]
    
    if (nrow(people_subset) == 0 | nrow(repec_subset) == 0) next
    
    combo <- CJ(name_repec = repec_subset[[name_col]],
                name_people = people_subset$name_clean,
                unique = TRUE)
    
    combo[, dist := stringdist(name_repec, name_people, method = "jw")]
    combo <- combo[dist <= threshold]
    if (nrow(combo) == 0) next
    
    combo_best <- combo[order(dist), .SD[1], by = name_repec]
    combo_best <- merge(combo_best,
                        people_subset[, .(name_clean, person_id)],
                        by.x = "name_people", by.y = "name_clean", all.x = TRUE)
    
    results[[ch]] <- combo_best[, .(name_repec, person_id)]
  }
  
  final_matches <- rbindlist(results)
  
  # -----------------------------
  # Merge first-round matches
  # -----------------------------
  repec_data <- merge(repec_data,
                      final_matches,
                      by.x = name_col,
                      by.y = "name_repec",
                      all.x = TRUE,
                      sort = FALSE)
  
  # -----------------------------
  # Second round: unmatched names (no first-letter restriction)
  # -----------------------------
  unmatched <- repec_data[is.na(person_id), .(name_clean = get(name_col))]
  
  if (nrow(unmatched) > 0) {
    combo2 <- CJ(name_repec = unmatched$name_clean,
                 name_people = dt_people$name_clean,
                 unique = TRUE)
    
    combo2[, dist := stringdist(name_repec, name_people, method = "jw")]
    combo2 <- combo2[dist <= threshold]
    
    if (nrow(combo2) > 0) {
      combo2_best <- combo2[order(dist), .SD[1], by = name_repec]
      combo2_best <- merge(combo2_best,
                           dt_people[, .(name_clean, person_id)],
                           by.x = "name_people", by.y = "name_clean", all.x = TRUE)
      
      # Merge second-round matches back
      repec_data <- merge(repec_data,
                          combo2_best[, .(name_repec, person_id)],
                          by.x = name_col,
                          by.y = "name_repec",
                          all.x = TRUE,
                          suffixes = c("", "_2"))
      
      # Fill missing person_id from second round
      repec_data[, person_id := ifelse(is.na(person_id), person_id_2, person_id)]
      repec_data[, person_id_2 := NULL]
    }
  }
  
  # Cleanup
  repec_data[, first_char := NULL]
  
  # Restore original order
  setorder(repec_data, row_index)
  repec_data[, row_index := NULL]
  
  return(repec_data)
}


# ----------------------------


repec_data <- fuzzy_merge_people(dt_people, repec_data, name_col = "name", threshold = 0.1)
repec_data <- unique(repec_data)

# count how many observations per year per person_id
repec_data[, obs_count := .N, by = .(person_id, year)]
View(repec_data[obs_count > 1])


# save
save(repec_data, file = file.path(path_data, "gen/repec_data_with_person_id.RData"))



# ----------------------------------------------------
# To papers, merge ID and REPEC Data
# ----------------------------------------------------


# name in dt_paper_level corresponds to orig_name in dt_people
dt_paper_level <- merge(
  dt_paper_level,
  dt_people[, .(orig_name, person_id)],
  by.x = "person_name",
  by.y = "orig_name",
  all.x = TRUE
)

# merge repec data to paper level data using ID
# year in repec = publication_year in dt_paper_level
setnames(repec_data, old = "year", new = "publication_year")


dt_paper_level <- merge(
  dt_paper_level,
  repec_data[!is.na(person_id), .(person_id, publication_year, avg_score)],
  by = c("person_id", "publication_year"),
  all.x = TRUE  # keeps all rows of dt_paper_level
)

# indicator that in top5
dt_paper_level[, top5_repec := fifelse(!is.na(avg_score),
  1, 0)]
dt_paper_level[is.na(person_id), top5_repec := NA]


setnames(dt_paper_level, old = "person_name", new = "name")




# ----------------------------------------------------
# Create PERSON-level data set
# ----------------------------------------------------

dt_person_level <- unique(dt_paper_level[!is.na(person_id), c("person_id", "name", "paper_id", "publication_year", "avg_score", "top5_repec")])
dt_person_level <- clean_names(dt_person_level, "name")
dt_person_level <- manual_adjustments(dt_person_level, "name")

# how often mentioned across all years
dt_person_level[, n_mentions := .N, by = person_id]


# ----------------------------------------------------
# Add Gender to PERSON-level data
# ----------------------------------------------------

# need the first names to put into AI
dt_person_level[, first_name := str_split_fixed(name, " ", 2)[,1]]
dt_person_level[!is.na(first_name),
                first_name := gsub("\\.", "", first_name)] # remove .
dt_person_level[, first_name := trimws(first_name)]

# some of them are abbreviations/ short. take ones that are at least 2 characters
dt_person_level[nchar(first_name) < 2, first_name := NA]

# take the longest one per person_id
dt_person_level[, length := nchar(first_name)]
dt_person_level[, max_length := max(length), by = c("person_id")]
dt_person_level[length == max_length, name_to_search := first_name]

dt_person_level[
  ,
  name_to_search := {
    tmp <- name_to_search[!is.na(name_to_search)]
    if (length(tmp) == 0) NA_character_ else tmp[1]
  },
  by = person_id
]

dt_person_level[, name_to_search := tolower(name_to_search)]


# create unique list of first names 
names_to_search <- unique(dt_person_level$name_to_search[!is.na(dt_person_level$name_to_search)])

# exclude the ones I already searched for (expand here if I searched for more names)

names_already_searched <- data.table::fread(
  file.path(path_data, "gen/wos_acknowledged_names_after_gender.csv")
)
names_already_searched <- names_already_searched[-1, ]
names_already_searched <- unique(names_already_searched$found_name)

names_to_search <- setdiff(names_to_search, names_already_searched)


# export the ones to still search
df_to_export <- data.frame(name_to_search = names_to_search)

# Write to Excel
# write.xlsx(df_to_export, file = file.path(path_data, "gen/names_to_search_gender.xlsx"),
#           sheetName = "NamesToSearch", overwrite = TRUE)

# import again

gender_1 <- data.table::fread(
  file.path(path_data, "gen/names_to_search_gender_found_20260126.csv")
)

gender_2 <- data.table::fread(
  file.path(path_data, "gen/wos_acknowledged_names_after_gender.csv")
)

gender <- unique(rbind(gender_1, gender_2))
setnames(gender, "Col 1", "name_to_search")

gender[, name_to_search := tolower(name_to_search)]

rm(gender_1, gender_2, dt_to_export, names_already_searched, names_to_search, df_to_export)


# match to dt_person_level

dt_person_level[is.na(name_to_search), name_to_search := first_name]

dt_person_level <- merge(
  dt_person_level, gender, by = "name_to_search", all.x = TRUE)


# manual adjustments

# Male People (ID)
male_ids <- c("17195", "5244", "346", "9941", "86", "1984")

dt_person_level[person_id %in% male_ids, `:=`(gender = "male", probability = 100)]


# Male Names
male_names <- c(
  "jean-baptiste", "jean-benoit", "jean-claude", "jean-edouard",
  "jean-francois", "jean-francoiswen", "jean-laurent", "jean-marie",
  "jean-michel", "jean-noel", "jean-philippe", "jean-pierre",
  "jean-sebastien", "jean-yves"
)

dt_person_level[name_to_search %in% male_names, `:=`(gender = "male", probability = 100)]



# drop columns no longer needed
dt_person_level <- dt_person_level[, -c("country", "length", "max_length", "first_name", "found_name", "name_to_search"), with = FALSE]

# keep only if more than 90% accuracy
dt_person_level <- dt_person_level[probability < 90, gender := NA]


# also match to paper-level data
dt_id_gender <- unique(dt_person_level[, .(person_id, gender)])

dt_paper_level <- merge(
  dt_paper_level, dt_id_gender,
  by = "person_id", all.x = TRUE
)


# female dummy
dt_person_level[, d_female := ifelse(gender == "female", 1, 0)]
dt_paper_level[, d_female := ifelse(gender == "female", 1, 0)]
dt_person_level[is.na(gender), d_female := NA]
dt_paper_level[is.na(gender), d_female := NA]

table(dt_person_level$d_female)


# match abstract and keywords to papers
load(file.path(path_data, "gen/wos_data_clean.RData"))

to_merge <- unique(wos_data[,c("title_paper", "publication_year", "abstract", "author_keywords", "keywords_plus")])

# only one observation per paper and publication year
to_merge[, obs_count := .N, by = c("title_paper", "publication_year")]
nrow(to_merge)

# if more than one choose random one (very few)
to_merge <- to_merge[obs_count == 1 | (obs_count > 1 & !duplicated(title_paper)), .(title_paper, publication_year, abstract, author_keywords, keywords_plus)]
nrow(to_merge)

uniqueN(dt_paper_level$paper_id)
dt_paper_level <- unique(merge(dt_paper_level, unique(to_merge[,c("title_paper", "publication_year", "abstract", "author_keywords", "keywords_plus")]), by = c("title_paper", "publication_year"), all.x = T))
uniqueN(dt_paper_level$paper_id)

# add fields classification based on keywords and abstracts and titles (done in classify_fields_from_wos_papers.ipynb)
fields <- fread(paste0(path_data, "gen/papers_fields_classification.csv"))

dt_paper_level <- unique(merge(dt_paper_level, unique(fields[,c("paper_id", "field_label")]), by = c("paper_id"), all.x = T))
setnames(dt_paper_level, "field_label", "field")


rm(to_merge, wos_data, fields)


# ----------------------------------------------------
# DATA SET FOR AUTHOR INFORMATION
# ----------------------------------------------------

dt_paper_authors <- dt_paper_level

# turn author_full_names into author_1, author_2 etc
# looks like Bursztyn, Leonardo; Fujiwara, Thomas; Pallais, 
# change to first name last name and capital letters

# function to clean one author (first name last name and capital letters)
# if no comma return upper case original
clean_name <- function(x) {
  parts <- tstrsplit(x, ",\\s*")
  
  # if no comma → return uppercase original
  if (length(parts) < 2 || is.na(parts[[2]])) {
    return(toupper(trimws(x)))
  }
  
  paste(toupper(parts[[2]]), toupper(parts[[1]]))
}

# split authors
dt_paper_authors[, author_list := strsplit(author_full_names, ";\\s*")]

# get max number of authors
max_authors <- max(lengths(dt_paper_authors$author_list))

# create columns
dt_paper_authors[, paste0("author_", 1:max_authors) := {
  lapply(1:max_authors, function(i) {
    sapply(author_list, function(x) {
      if (length(x) >= i) clean_name(x[i]) else NA_character_
    })
  })
}]

# optional: drop helper column
dt_paper_authors[, author_list := NULL]








# ----------------------------------------------------
# SAVE
# ----------------------------------------------------

  
#save(dt_person_level, file = file.path(path_data, "gen/dt_person_level.RData"))
#save(dt_paper_level, file = file.path(path_data, "gen/dt_paper_level.RData"))


load(file.path(path_data, "gen/dt_person_level.RData"))
load(file.path(path_data, "gen/dt_paper_level.RData"))
