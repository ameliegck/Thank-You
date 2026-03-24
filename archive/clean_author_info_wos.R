rm(list = ls()) 

if(!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, tidyr, readr, readxl, ggplot2, RColorBrewer, stringi, openxlsx, jsonlite) # add any packages here

if(Sys.info()["user"] %in% c("Amelie.Grosenick")){
  path_wd <- c("D:/Projektfolder1 (Miethe, Grosenick)/zz_AmelieMisc/")
  path_data <- c("D:/Projektfolder1 (Miethe, Grosenick)/zz_AmelieMisc/thankyou/data/")
}

setwd(path_wd)

# Load data
load(paste0(path_data, "gen/dt_acknowledged_gendered.RData"))




author_info <- read_delim(
  paste0(path_data, "gen/metadata_wos_acknowledged_whilescraping_20260119.csv"),
  show_col_types = FALSE
)
setDT(author_info)

# clean

parse_py_dict <- function(x) {
  if (is.null(x) || is.na(x)) return(NULL)
  
  # Replace Python quotes with JSON quotes
  x <- gsub("'", "\"", x)
  
  # Convert to list using jsonlite
  jsonlite::fromJSON(x)
}

# Parse both columns
author_info[, profile_list := lapply(profile_summary, parse_py_dict)]
author_info[, metrics_list := lapply(metrics, parse_py_dict)]

# Unnest into wide columns
author_info <- cbind(
  author_info[, !c("profile_summary", "metrics", "profile_list", "metrics_list")],
  rbindlist(author_info$profile_list, fill = TRUE),
  rbindlist(author_info$metrics_list, fill = TRUE)
)

# change column names

name_map <- c(
  "first_name" = "first_name",
  "last_name" = "last_name",
  "further_info" = "further_info",
  "RID" = "rid",
  "subjects" = "subjects",
  "Total documents" = "total_docs",
  "Web of Science Core Collection publications" = "wos_core_collection_pubs",
  "Preprints" = "preprints",
  "Awarded grants" = "awarded_grants",
  "Publications indexed in Web of Science" = "publications_in_wos",
  "Dissertations or Theses" = "dissertations_or_theses",
  "Non-indexed publications" = "non_indexed_publications",
  "Verified peer reviews" = "verified_peer_reviews",
  "Verified editor records" = "verified_editor_records",
  "Preprint" = "preprint",
  "H-Index" = "h_index",
  "Publications" = "publications",
  "Sum of Times Cited" = "citations",
  "Citing Articles" = "n_citing_articles",
  "Sum of Times Cited (without self-citations)" = "n_cited_wo_self_citations",
  "Citing Articles (without self-citations)" = "n_citing_articles_wo_self_citations",
  "Sum of Times Cited by Patents" = "n_cited_by_patents",
  "Citing Patents" = "n_citing_patents",
  "Sum of Times Cited by Policy" = "n_cited_by_policy",
  "Citing Policy Documents" = "n_citing_policy_documents",
  "Publication" = "publication"
)

setnames(author_info, old = names(name_map), new = unname(name_map))


# is econ mentioned in subjects? could have gotten wrong person if not...
author_info[, d_econ := fifelse(
  grepl("economics", subjects, ignore.case = TRUE),
  1, 0
)]
table(author_info$d_econ)
View(author_info[d_econ == 0]) # either NA or other subjects. hand collect if NA, refine scraping if other

# Set to NA where economics_subject == 0
cols_to_na <- setdiff(names(author_info), c("first_name", "last_name"))
author_info[d_econ == 0, (cols_to_na) := NA]
author_info[, d_econ := NULL]


# merge info to data on the acknowledged


dt_acknowledged <- merge(
  dt_acknowledged,
  author_info,
  by = c("first_name", "last_name"),
  all.x = TRUE
)

# ------------------------------------------
# add editor info
# ------------------------------------------


load(paste0(path_data, "gen/top5editors.RData"))
setnames(top5_editors, "year", "year")

# expand for two more years to account for delays in publication process 
top5_editors[, last_year := max(year), by = c("name")]

# create future years per editor-journal and merge back
extension <- unique(
  top5_editors,
  by = c("name", "journal", "last_year")
)[
  , .(year = (last_year + 1):(last_year + 2)),
  by = .(name, journal)
]

top5_editors <- rbind(top5_editors, extension, fill = TRUE)
top5_editors[, last_year := NULL]

top5_editors[, c("first_name", "last_name") := .(
  sub(" .*", "", name),                     # everything before the first space → first name
  sub(".* ", "", name)                      # everything after the last space → last name
)]


top5_editors[, d_editor := 1]

# merge to acknowledged people data

dt_acknowledged[, journal := fcase(
  journal_abbreviation == "J POLIT ECON", "JPE",
  journal_abbreviation == "REV ECON STUD", "RESTUD",
  journal_abbreviation == "AM ECON REV", "QJE",
  journal_abbreviation == "ECONOMETRICA", "ECONOMETRICA",
  journal_abbreviation == "Q J ECON", "AER",
  default = journal_abbreviation  # keeps NA or other journals unchanged
)]

dt_acknowledged[, year := publication_year]


dt_acknowledged <- merge(
  dt_acknowledged,
  top5_editors[, .(first_name, last_name, journal, year, d_editor)],
  by = c("first_name", "last_name", "journal", "year"),
  all.x = TRUE
)

dt_acknowledged[is.na(d_editor), d_editor := 0]
table(dt_acknowledged$d_editor)


save(dt_acknowledged, file = file.path(path_data, "gen/dt_acknowledged_authorinfos.RData"))



