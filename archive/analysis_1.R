rm(list = ls()) 

if(!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, tidyr, readr, readxl, ggplot2, RColorBrewer, stringi, openxlsx) # add any packages here

if(Sys.info()["user"] %in% c("Amelie.Grosenick")){
  path_wd <- c("D:/Projektfolder1 (Miethe, Grosenick)/zz_AmelieMisc/")
  path_data <- c("D:/Projektfolder1 (Miethe, Grosenick)/zz_AmelieMisc/thankyou/data/")
  path_plots <- c("D:/Projektfolder1 (Miethe, Grosenick)/zz_AmelieMisc/thankyou/output/gender/")
  
}

setwd(path_wd)


# graph settings
palette <- "RdBu"
colors <- brewer.pal(11, palette)
options(scipen = 999)


################################
#### FIRST ANALYSIS
################################

# load data
load(paste0(path_data, "gen/dt_acknowledged_authorinfos.RData"))

# remove editors
dt_acknowledged <- dt_acknowledged[d_editor == 0, ]
dt_acknowledged <- dt_acknowledged[!grepl("editor", type, ignore.case = TRUE)]

# if less than 90% certain, set gender to unknown
dt_acknowledged[probability < 90, gender := "uncertain"]
# for now: drop if gender not clear
dt_acknowledged <- dt_acknowledged[gender == "female" | gender == "male", ]

# create a numeric unique person identifier based on full_name
dt_acknowledged[, person_id := as.numeric(factor(full_name))]

# change journal abbrevations to proper ones
dt_acknowledged[journal_abbreviation == "J POLIT ECON", journal_abbreviation := "JPE"]
dt_acknowledged[journal_abbreviation == "AM ECON REV", journal_abbreviation := "AER"]
dt_acknowledged[journal_abbreviation == "ECONOMETRICA", journal_abbreviation := "ECMA"]
dt_acknowledged[journal_abbreviation == "Q J ECON", journal_abbreviation := "QJE"]
dt_acknowledged[journal_abbreviation == "REV ECON STUD", journal_abbreviation := "RESTUD"]
dt_acknowledged[journal_abbreviation == "QJE", journal := "Quarterly Journal of Economics"]
dt_acknowledged[journal_abbreviation == "AER", journal := "American Economic Review"]



# keep all people mentioned, also if mentioned double (paper-level)
dt_paper_level <- dt_acknowledged

# keep all people mentioned only once (person-level)
dt_person_level <- unique(dt_acknowledged[, c("person_id", "full_name", "gender", 
                                              "h_index", "citations", "publications", "further_info", "subjects")])

dt_person_level[, citations := as.numeric(gsub(",", "", citations))]
dt_person_level[, citations := as.numeric(citations)]

# Count occurrences by person_id
person_counts <- dt_person_level[, .N, by = person_id]

# Flag if count > 1
person_counts[, multiple_mentions := N > 1]

# Print a warning if any person appears more than once
if (any(person_counts$multiple_mentions)) {
  warning("Some persons appear more than once in the person-level dataset!")
  dt_person_level <- merge(dt_person_level, person_counts[, .(person_id, multiple_mentions)], by = "person_id", all.x = TRUE)
  View(person_counts[multiple_mentions == TRUE])
}

rm(person_counts)



################################
#### DATA DESCRIPTION
###############################

# number of unique people mentioned
uniqueN(dt_person_level)

# number of unique papers observed
uniqueN(dt_paper_level$article_title)

# how many ARTICLES are in my sample each year
# excluding where publication_year NA

plot <- dt_paper_level[, .(N = uniqueN(article_title)), by = publication_year]  

p <- ggplot(plot[!is.na(publication_year)], aes(x = factor(publication_year), y = N)) +
  geom_col(fill = colors[9]) +          # bars
  labs(
    x = "Publication Year",
    y = "Number of Articles"
  ) +
  theme_minimal(base_size = 14)
p

ggsave(filename = paste0(path_plots, "sample_articles_by_year.png"),
  plot = p,  width = 8,  height = 6,  dpi = 300)


# how many people mentioned per article, distribution
# with mean

plot <- dt_paper_level[, .N, by = .(article_title)]
mean <- mean(plot$N)  # mean number of people mentioned per article

p <- ggplot(plot, aes(x = N)) +
  geom_histogram(binwidth = 1, fill = colors[9], color = "black") +
  geom_vline(xintercept = mean, linetype = "dashed", color = colors[2], size = 1) + 
  labs(
    x = "Number of Acknowledged People per Article",
    y = "Number of Articles"
  ) +
  scale_x_continuous(limits = c(0, 85)) +   
  scale_y_continuous(limits = c(0, 150)) +  
  theme_minimal(base_size = 14)
p

ggsave(filename = paste0(path_plots, "distribution_people_per_article.png"),
       plot = p,  width = 8,  height = 6,  dpi = 300)


# how many people mentioned how often, distribution
# with mean

plot <- dt_paper_level[, .N, by = .(person_id)]
mean <- mean(plot$N)  # mean number of people mentioned per article

p <- ggplot(plot, aes(x = N)) +
  geom_histogram(binwidth = 1, fill = colors[9], color = "black") +
  geom_vline(xintercept = mean, linetype = "dashed", color = colors[2], size = 1) + 
  labs(
    x = "Number of Acknowledged People per Article",
    y = "Number of Articles"
  ) +
  scale_x_continuous(limits = c(0, 85)) +   
  scale_y_continuous(limits = c(0, 5000)) +  
  theme_minimal(base_size = 14)
p

ggsave(filename = paste0(path_plots, "distribution_people_mentioned_how_often.png"),
       plot = p,  width = 8,  height = 6,  dpi = 300)


################################
#### GENDER
###############################


# how many females mentioned per article, distribution
# with mean

plot <- dt_paper_level[gender == "female", .N, by = .(article_title)]
mean <- mean(plot$N)  # mean number of people mentioned per article

p <- ggplot(plot, aes(x = N)) +
  geom_histogram(binwidth = 1, fill = colors[9], color = "black") +
  geom_vline(xintercept = mean, linetype = "dashed", color = colors[2], size = 1) + 
  labs(
    x = "Number of Acknowledged Female Contributors per Article",
    y = "Number of Articles"
  ) +
  scale_x_continuous(limits = c(0, 85)) +   
  scale_y_continuous(limits = c(0, 400)) +  
  theme_minimal(base_size = 14)
p

ggsave(filename = paste0(path_plots, "distribution_females_per_article.png"),
       plot = p,  width = 8,  height = 6,  dpi = 300)



# how many males mentioned per article, distribution
# with mean

plot <- dt_paper_level[gender == "male", .N, by = .(article_title)]
mean <- mean(plot$N)  # mean number of people mentioned per article

p <- ggplot(plot, aes(x = N)) +
  geom_histogram(binwidth = 1, fill = colors[9], color = "black") +
  geom_vline(xintercept = mean, linetype = "dashed", color = colors[2], size = 1) + 
  labs(
    x = "Number of Acknowledged Male Contributors per Article",
    y = "Number of Articles"
  ) +
  scale_x_continuous(limits = c(0, 85)) +   
  scale_y_continuous(limits = c(0, 400)) +  
  theme_minimal(base_size = 14)
p

ggsave(filename = paste0(path_plots, "distribution_males_per_article.png"),
       plot = p,  width = 8,  height = 6,  dpi = 300)


# contributor share per article, by gender

plot <- dt_paper_level[, .N, by = .(article_title, gender)]
plot <- dcast(plot, article_title ~ gender, value.var = "N", fill = 0)
plot[, total := female + male]
plot[, female_share := female/total*100]
plot[, male_share := male/total*100]

mean_female <- mean(plot$female_share)  
mean_male <- mean(plot$male_share)  

# female
p <- ggplot(plot, aes(x = female_share)) +
  geom_histogram(
    aes(y = ..count../sum(..count..)*100),  # y-axis in percent
    binwidth = 1, fill = colors[9], color = "black"
  ) +
  geom_vline(xintercept = mean_female, linetype = "dashed", color = colors[2], size = 1) + 
  labs(
    x = "Share of Acknowledged Female Contributors per Article",
    y = "Frequency (%)"
  ) +
  theme_minimal(base_size = 14)

p

ggsave(filename = paste0(path_plots, "female_share_per_article.png"),
       plot = p,  width = 8,  height = 6,  dpi = 300)


# male
p <- ggplot(plot, aes(x = male_share)) +
  geom_histogram(
    aes(y = ..count../sum(..count..)*100),  # y-axis in percent
    binwidth = 1, fill = colors[9], color = "black"
  ) +
  geom_vline(xintercept = mean_male, linetype = "dashed", color = colors[2], size = 1) + 
  labs(
    x = "Share of Acknowledged Male Contributors per Article",
    y = "Frequency (%)"
  ) +
  theme_minimal(base_size = 14)

p

ggsave(filename = paste0(path_plots, "male_share_per_article.png"),
       plot = p,  width = 8,  height = 6,  dpi = 300)


# what is female share among all people ever mentioned?

# calculate shares, person level
dt_summary <- dt_person_level[, .N, by = .(gender)]
dt_summary[, share := N / sum(N) * 100]
dt_summary # 19.29 % of mentioned people are female

# calculate shares, paper level (weighetd by how often mentioned)
dt_summary <- dt_paper_level[, .N, by = .(gender)]
dt_summary[, share := N / sum(N) * 100]
dt_summary # 13.98 % of mentioned people are female



################################
#### SUPERSTARS
###############################


### how often are people mentioned?


# Count total mentions per person
plot <- as.data.table(dt_paper_level)[, .(N = .N), by = person_id]

# Order names so smallest mentions are left
plot <- plot[order(N)]
plot[, person_id := factor(person_id, levels = person_id)]  # make x discrete

# Create numeric index for percentile ticks
plot[, person_index := 1:.N]
n_people <- nrow(plot)
percentiles <- seq(10, 100, by = 10)
tick_indices <- ceiling(n_people * percentiles / 100)
tick_labels <- paste0(percentiles, "%")

# Plot
ggplot(plot, aes(x = person_id, y = N)) +
  geom_col(fill = "steelblue", width = 1) +  # width = 1 removes gaps
  scale_x_discrete(
    name = "Share within all people acknowledged",
    breaks = plot$person_id[tick_indices],
    labels = tick_labels
  ) +
  scale_y_continuous(limits = c(0, 90),
                     name = "Number of Mentions") +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    legend.position = "none"
  )


ggsave(filename = paste0(path_plots, "distribution_mentions.png"),
       plot = p,  width = 8,  height = 6,  dpi = 300)


# how many people do top 10% and top 1% account for? 47.90164% and 11.8967%


# Count total mentions per person
plot <- as.data.table(dt_paper_level)[, .(N = .N), by = person_id]

# Order by mentions descending
plot <- plot[order(-N)]

n_people <- nrow(plot)

# Top 10%
top_10_count <- ceiling(n_people * 0.10)
top_10_mentions <- sum(plot$N[1:top_10_count])
top_10_percent <- top_10_mentions / sum(plot$N) * 100

# Top 1%
top_1_count <- ceiling(n_people * 0.01)
top_1_mentions <- sum(plot$N[1:top_1_count])
top_1_percent <- top_1_mentions / sum(plot$N) * 100

top_10_percent
top_1_percent




# plot (colored, by gender)
ggplot(plot, aes(x = person_index, y = N, fill = gender)) +
  geom_col() +  # remove fill inside geom_col so scale_fill_manual works
  scale_x_continuous(
    name = "All people mentioned") +
  scale_fill_manual(values = c("male" = colors[9], "female" = colors[2]), name = "") +
  scale_y_continuous(limits = c(0, 70)) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(size = 14),
    legend.position = "bottom"
  )
ggsave(filename = paste0(path_plots, "distribution_mentions_by_gender.png"),
       plot = p,  width = 8,  height = 6,  dpi = 300)


### who is mentioned most often?


# ---------------------------------------------------
# function: plot 20 most frequently mentioned ones
# ---------------------------------------------------
plot_top_mentions <- function(dt, 
                              gender_filter = NULL, 
                              top_n = 20,
                              save_path = NULL) {  # add optional path to save
  dt <- as.data.table(dt)
  
  # optional gender filter
  if (!is.null(gender_filter)) {
    dt <- dt[gender == gender_filter]
  }
  
  # count mentions
  dt_counts <- dt[, .N, by = .(full_name, journal_abbreviation)]
  
  # sum over journals to get total mentions per name
  dt_total <- dt_counts[, .(total = sum(N)), by = full_name]
  
  # get top N
  top_names <- dt_total[order(-total)][1:top_n, full_name]
  dt_top <- dt_counts[full_name %in% top_names]
  
  # ensure names are ordered in plot by total mentions
  dt_top[, full_name := factor(full_name, 
                               levels = dt_total[full_name %in% top_names][order(-total), full_name])]
  
  # assign colors
  n_journals <- length(unique(dt_top$journal_abbreviation))
  assigned_colors <- rep(colors[c(3,8,11,10,4)], length.out = n_journals)
  
  # create plot
  p <- ggplot(dt_top, aes(x = full_name, y = N, fill = journal_abbreviation)) +
    geom_col(position = "stack") +
    coord_flip() +
    labs(x = "", y = "Mentions", fill = "Journal") +
    scale_fill_manual(values = setNames(assigned_colors, unique(dt_top$journal_abbreviation))) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(size = 12)
    )
  
  # optionally save
  if (!is.null(save_path)) {
    # create folder if it doesn't exist
    dir.create(dirname(save_path), showWarnings = FALSE, recursive = TRUE)
    ggsave(filename = save_path, plot = p, width = 8, height = 6, dpi = 300)
  }
  
  return(p)
}

p <- plot_top_mentions(
  dt_paper_level, 
  save_path = paste0(path_plots, "top20_mentions_all.png")
)
p

p <- plot_top_mentions(
  dt_paper_level, gender_filter = "male",
  save_path = paste0(path_plots, "top20_mentions_male.png")
)
p

p <- plot_top_mentions(
  dt_paper_level, gender_filter = "female",
  save_path = paste0(path_plots, "top20_mentions_female.png")
)
p




# who are the people that are mentioned most often?

# generate indicator for people that are mentioned in top decile vs all others
dt_paper_level[, mention_count := .N, by = person_id]

dt_person_level <- merge(
  dt_person_level,
  unique(dt_paper_level[, .(person_id, mention_count)]),
  by = "person_id",
  all.x = TRUE
)

# female dummy
dt_person_level[, d_female := ifelse(gender == "female", 1, 0)]


p10_threshold <- quantile(dt_person_level$mention_count, 0.9)
p1_threshold <- quantile(dt_person_level$mention_count, 0.99)
p01_threshold <- quantile(dt_person_level$mention_count, 0.999)

dt_person_level[, p10_mentioned := fifelse(
  mention_count >= p10_threshold,
  1, 0
)]  

dt_person_level[, p1_mentioned := fifelse(
  mention_count >= p1_threshold,
  1, 0
)]  

dt_person_level[, p01_mentioned := fifelse(
  mention_count >= p01_threshold,
  1, 0
)]  


table(dt_person_level$p10_mentioned)
table(dt_person_level$p1_mentioned)
table(dt_person_level$p01_mentioned)


mean_sd_by_threshold <- function(dt, value_var, threshold_var) {
  
  dt <- as.data.table(dt)
  
  dt[, .(
    mean = mean(get(value_var), na.rm = TRUE),
    sd   = sd(get(value_var), na.rm = TRUE),
    median = median(get(value_var), na.rm = TRUE),
    N = .N
  ), by = threshold_var]
}

mean_sd_by_threshold(dt = dt_person_level, value_var = "citations", threshold_var = "p10_mentioned")
mean_sd_by_threshold(dt = dt_person_level, value_var = "citations", threshold_var = "p1_mentioned")
mean_sd_by_threshold(dt = dt_person_level, value_var = "citations", threshold_var = "p01_mentioned")

################################
# Bottom 90: 3538.306 (9216.626)
# Top 10: 4082.507 (10239.702)
# Top 1: 5075.714 (9321.233)
# Top 0.1: 14535.300 (18011.597)


## correlation

reg <- lm(asinh(mention_count) ~ asinh(citations), data = dt_person_level)

dt_clean <- dt_person_level[!is.na(mention_count) & !is.na(citations), ]
dt_clean <- dt_clean[mention_count> 0,]
dt_clean <- dt_clean[citations> 0,]

reg <- lm(mention_count ~ log(citations), data = dt_clean)

summary(reg)
coef_summary <- summary(reg)$coefficients
slope <- coef_summary["log(citations)", "Estimate"]
slope_se <- coef_summary["log(citations)", "Std. Error"]
slope_text <- paste0("Slope: ", round(slope, 2), "\n         (", round(slope_se, 2), ")")

# scatter plot with regression line
p <- ggplot(dt_clean, aes(x = log(citations), y = mention_count)) +
  geom_point(alpha = 0.5, color = colors[9]) +        # semi-transparent points
  geom_smooth(method = "lm", color = colors[2], se = TRUE) +  # linear fit
  annotate("text", 
           x = min(log(dt_clean$citations), na.rm = TRUE) + 0.5,  # adjust position
           y = max(dt_clean$mention_count, na.rm = TRUE) * 0.9,   # adjust position
           label = slope_text,
           size = 4, color = colors[2], hjust = 0) +
  labs(
    x = "log(Citations)",
    y = "Times Acknowledged") +
  theme_minimal(base_size = 14)
p

ggsave(filename = paste0(path_plots, "corr_mentions_citations.png"),
       plot = p,  width = 8,  height = 6,  dpi = 300)


