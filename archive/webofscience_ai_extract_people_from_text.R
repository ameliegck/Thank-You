rm(list = ls()) 

if(!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, tidyr, readr, readxl, ggplot2, openai, RColorBrewer) # add any packages here

if(Sys.info()["user"] %in% c("Amelie.Grosenick")){
  path_wd <- c("D:/Projektfolder1 (Miethe, Grosenick)/zz_AmelieMisc/")
  path_data <- c("D:/Projektfolder1 (Miethe, Grosenick)/zz_AmelieMisc/thankyou/data/")
}

setwd(path_wd)


# load data
load(paste0(path_data, "gen/wos_data_for_ai.RData"))

wos_data_ai <- wos_data_ai[1:10, ]  # for testing, limit to first 10 rows

# Set your API key 
Sys.setenv(OPENAI_API_KEY = "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")


# --- AI Extraction Function ---
extract_names_ai <- function(text) {
  prompt <- paste0(
    "Extract only the names of people thanked for comments, suggestions, or feedback.\n",
    "If someone is mentioned as editor or co-editor, write that down. For this, create a data structure with two columns: 'person_name' and 'type'.\n",
    "Also mention if someone is named as just edit or co-editor without name. Name is then NA.\n",
    "Ignore mentions of research assistance, data access, seminar participants, or generic terms like 'all participants'.\n",
    "Output the result as an R data.frame or list of named vectors that can be converted to a data.table.\n\n",
    "Text: \"", text, "\""
  )
  
  response <- create_chat_completion(
    model = "gpt-5-mini",
    messages = list(list(role = "user", content = prompt))
  )
  
  result_text <- response$choices[[1]]$message$content
  
  # parse into a data.table
  dt <- tryCatch(
    {
      dt_parsed <- eval(parse(text = result_text))
      setDT(dt_parsed)
      # ensure columns exist
      if (!("person_name" %in% names(dt_parsed))) dt_parsed[, person_name := NA_character_]
      if (!("type" %in% names(dt_parsed))) dt_parsed[, type := NA_character_]
      dt_parsed
    },
    error = function(e) data.table(person_name = character(0), type = character(0))
  )
  
  return(dt)
}

# --- Loop over funding_text column ---
rows <- list()

for (i in seq_len(nrow(wos_data_ai))) {
  file_name <- wos_data_ai$file_name[i]
  funding_text <- wos_data_ai$funding_text[i]
  
  dt_names <- extract_names_ai(funding_text)
  
  if (nrow(dt_names) > 0) {
    dt_names[, file_name := file_name]
    rows[[length(rows) + 1]] <- dt_names
  }
}

# Combine all rows into final table
df_names <- rbindlist(rows, use.names = TRUE, fill = TRUE)


