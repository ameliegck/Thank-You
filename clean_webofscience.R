rm(list = ls()) 

if(!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, tidyr, readr, readxl, ggplot2, RColorBrewer) # add any packages here

if(Sys.info()["user"] %in% c("Amelie.Grosenick")){
  path_wd <- c("D:/Projektfolder1 (Miethe, Grosenick)/zz_AmelieMisc/")
  path_data <- c("D:/Projektfolder1 (Miethe, Grosenick)/zz_AmelieMisc/thankyou/data/")
  path_plots <- c("D:/Projektfolder1 (Miethe, Grosenick)/zz_AmelieMisc/thankyou/output/")
  
}

setwd(path_wd)

#-----------------------------------------------
# Load data and combine into one data set
#-----------------------------------------------


### TOP 5
files <- list.files(
  path = file.path(path_data, "webofscience/top5/2015_2025"),
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


### OTHER JOURNALS
files <- list.files(
  path = file.path(path_data, "webofscience/other_journals/2015_2025"),
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

other_data <- rbindlist(data_list, use.names = TRUE, fill = TRUE)


wos_data <- rbind(wos_data, other_data)



# get column names
dt_names <- read_excel(file.path(path_data, "webofscience", "column_names.xls"), col_names = TRUE)
names <- colnames(dt_names)
names <- tolower(names)
names <- gsub(" ", "_", names)

setnames(wos_data, old = colnames(wos_data), new = names[1:71])

rm(data_list, dt_names, other_data)


#-----------------------------------------------
# Sample restriction and create vars
#-----------------------------------------------

# drop duplicates
wos_data <- unique(wos_data)


# keep only where text is available
uniqueN(wos_data$article_title)
wos_data <- wos_data[!is.na(funding_text), ]
uniqueN(wos_data$article_title)

# change column names
setnames(
  wos_data,
  old = c("source_title", "journal_abbreviation", "article_title"),
  new = c("journal", "journal_short", "title_paper")
)

# create indicator for top 5 journals
# change journal abbrevations to proper ones
wos_data[journal_short == "J POLIT ECON", journal_short := "JPE"]
wos_data[journal_short == "AM ECON REV", journal_short := "AER"]
wos_data[journal_short == "ECONOMETRICA", journal_short := "ECMA"]
wos_data[journal_short == "Q J ECON", journal_short := "QJE"]
wos_data[journal_short == "REV ECON STUD", journal_short := "RESTUD"]
wos_data[journal_short == "QJE", journal := "Quarterly Journal of Economics"]
wos_data[journal_short == "AER", journal := "American Economic Review"]

wos_data[, top5 := fifelse(
  journal_short %in% c("JPE", "AER", "ECMA", "QJE", "RESTUD"),
  1, 0)]


# save
save(wos_data, file = file.path(path_data, "gen/wos_data_clean.RData"))



#-----------------------------------------------
# save data just the with texts (for ai)
#-----------------------------------------------

# data set that stores files with acknowledgments to put into AI

wos_data_ai <- wos_data[, .(publication_type, authors, author_full_names, title_paper, journal_short, doi, funding_text, top5)]
View(wos_data_ai[is.na(doi)])
save(wos_data_ai, file = file.path(path_data, "gen/wos_data_for_ai.RData"))

# only the ones I haven't searched for yet
wos_data_ai <- wos_data_ai[top5 == 0]


# save
save(wos_data_ai, file = file.path(path_data, "gen/wos_data_for_ai_other_journals.RData"))

data.table::fwrite(
  wos_data_ai,
  file = file.path(path_data, "gen/wos_data_for_ai_other_journals.csv")
)


#-----------------------------------------------
# Plot: Length of acknowledgments over time
#-----------------------------------------------

wos_data[, length_text := nchar(funding_text)]

summary(wos_data$length_text)



# Average length per year by top5
avg_length <- wos_data[, top5 := factor(top5, levels = c(1, 0))]  # 1 (Top5) first, 0 second
avg_length <- wos_data[, .(avg_length = mean(length_text, na.rm = TRUE)), by = .(publication_year, top5)]
avg_length <- avg_length[publication_year < 2026]

# Line plot with two lines
p <- ggplot(avg_length, aes(x = publication_year, y = avg_length, color = factor(top5))) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = seq(2015, max(avg_length$publication_year, na.rm = TRUE), by = 2)
  ) +
  scale_y_continuous(limits = c(500, 1100)) +  # keep your y-axis limits
  scale_color_manual(values = c("darkorange", "steelblue"), 
                     labels = c("Top 5", "Other Journal")) +
  labs(
    x = "Publication Year",
    y = "Average Length of Acknowledgments \n (Number of Characters)",
    color = "",
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 12)) 
p

ggsave(p, filename = file.path(path_plots, "length_texts.png"),
  width = 8,  height = 6,  dpi = 300)


uniqueN(wos_data$title_paper)





