rm(list = ls()) 

if(!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, tidyr, readr, readxl, ggplot2, RColorBrewer, stringr, stringi, stringdist, igraph) # add any packages here

if(Sys.info()["user"] %in% c("Amelie.Grosenick")){
  path_wd <- c("D:/Projektfolder1 (Miethe, Grosenick)/zz_AmelieMisc/")
  path_data <- c("D:/Projektfolder1 (Miethe, Grosenick)/zz_AmelieMisc/thankyou/data/")
  path_plots <- c("D:/Projektfolder1 (Miethe, Grosenick)/zz_AmelieMisc/thankyou/output/")
  
}

setwd(path_wd)


# This creates all statistics for data, basics

# ----------------------------------------------------
# Load data
# ----------------------------------------------------

# paper info
load(file.path(path_data, "gen/dt_paper_level.RData"))

# person level
load(file.path(path_data, "gen/dt_person_level.RData"))




################################
#### DATA DESCRIPTION
###############################

# number of unique people mentioned
uniqueN(dt_paper_level$person_id)

# number of unique papers observed
uniqueN(dt_paper_level$paper_id)

# ----------------------------------------------------
# Plot: how many ARTICLES are in my sample each year
# excluding where publication_year NA
# ----------------------------------------------------

plot <- dt_paper_level[, .(N = uniqueN(paper_id)), by = .(publication_year, top5)]
plot[, top5 := factor(top5, levels = c(1, 0))]  # so stack other way around
p <- ggplot(plot[!is.na(publication_year)], 
            aes(x = factor(publication_year), y = N, fill = factor(top5))) +
  geom_col() +  # separate bars by top5
  scale_fill_manual(values = c("0" = "steelblue", "1" = "darkorange"),
                    labels = c("Top 5", "Other Journal"),
                    name = "") +  # legend without title
  labs(
    x = "Publication Year",
    y = "Number of Articles"
  ) +
  theme_minimal(base_size = 14) +  
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12)
  )

p

ggsave(filename = paste0(path_plots, "sample_articles_by_year.png"),
       plot = p,  width = 8,  height = 6,  dpi = 300)




# ----------------------------------------------------
# Plot: Length of acknowledgments and number of people mentioned over time
# ----------------------------------------------------

dt_paper_level[, n_contributors := .N, by = .(paper_id)]


plot <- dt_paper_level[, .(
  avg_length = mean(length_text, na.rm = TRUE),
  avg_mentions = mean(n_contributors, na.rm = TRUE)
), by = .(publication_year, top5)]

# Keep relevant years
plot <- plot[publication_year < 2026]

# Ensure top5 factor order
plot[, top5 := factor(top5, levels = c(1, 0))]  # 1 = Top5, 0 = Others


# Plot 1: Both lines

p <- ggplot(plot, aes(x = publication_year, color = top5)) +
  
  # Average length line
  geom_line(aes(y = avg_length), size = 1, linetype = "dashed") +
  geom_point(aes(y = avg_length), size = 2, shape = 16) +
  
  # Average mentions line (scaled for visibility)
  geom_line(aes(y = avg_mentions * 50), size = 1, linetype = "solid") +
  geom_point(aes(y = avg_mentions * 50), size = 2, shape = 17) +
  
  # X-axis
  scale_x_continuous(breaks = seq(2015, max(plot$publication_year, na.rm = TRUE), by = 2)) +
  
  # Y-axis with secondary for mentions
  scale_y_continuous(
    name = "Average Length of Acknowledgments \n (Characters)",
    sec.axis = sec_axis(~./50, name = "Average Number of People Mentioned"),
    limits = c(300, 1300)
  ) +
  
  # Color legend only
  scale_color_manual(values = c("darkorange", "steelblue"),
                     labels = c("Top 5", "Other Journal")) +
  
  labs(x = "Publication Year", color = "") +  # legend only for top5
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12)
  )


# in total four labels

p <- ggplot(plot) +
  
  # Average length line
  geom_line(aes(x = publication_year, y = avg_length,
                color = factor(paste0(top5, "_Length"))),
            size = 1, linetype = "dashed") +
  geom_point(aes(x = publication_year, y = avg_length,
                 color = factor(paste0(top5, "_Length"))),
             size = 2, shape = 16) +
  
  # Average mentions line (scaled for visibility)
  geom_line(aes(x = publication_year, y = avg_mentions * 50,
                color = factor(paste0(top5, "_Mentions"))),
            size = 1, linetype = "solid") +
  geom_point(aes(x = publication_year, y = avg_mentions * 50,
                 color = factor(paste0(top5, "_Mentions"))),
             size = 2, shape = 17) +
  
  # X-axis
  scale_x_continuous(breaks = seq(2015, max(plot$publication_year, na.rm = TRUE), by = 2)) +
  
  # Y-axis with secondary for mentions
  scale_y_continuous(
    name = "Average Length of Acknowledgments \n (Characters)",
    sec.axis = sec_axis(~./50, name = "Average Number of People Mentioned"),
    limits = c(300, 1300)
  ) +
  
  # Manual colors and labels for four legend items
  scale_color_manual(
    values = c(
      "1_Length" = "darkorange",
      "0_Length" = "steelblue",
      "1_Mentions" = "darkorange",
      "0_Mentions" = "steelblue"
    ),
    breaks = c("1_Length", "0_Length", "1_Mentions", "0_Mentions"),  # order you want
    labels = c(
      "1_Length" = "Top 5 (Length Text)",
      "0_Length" = "Other Journals (Length Text)",
      "1_Mentions" = "Top 5 (# Mentions)",
      "0_Mentions" = "Other Journals (# Mentions)"
    )
  ) +
  
  labs(x = "Publication Year", color = "") +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12)
  ) + 
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

p

# Save plot
ggsave(p, filename = file.path(path_plots, "length_text_and_mentions_both.png"),
       width = 8, height = 6, dpi = 300)


# Plot 2: Only first line for presenting

p <- ggplot(plot, aes(x = publication_year, y = avg_length, color = top5)) +
  geom_line(size = 1, linetype = "dashed") +
  geom_point(size = 2, shape = 16) +
  
  # X-axis
  scale_x_continuous(breaks = seq(2015, max(plot$publication_year, na.rm = TRUE), by = 2)) +
  
  # Y-axis only
  scale_y_continuous(name = "Average Length of Acknowledgments \n (Characters)",
                     limits = c(300, 1300)
  ) +
  
  # Color legend for Top 5 vs Other Journal
  scale_color_manual(values = c("darkorange", "steelblue"),
                     labels = c("Top 5 (Length Text)", "Other Journal (Length Text)")) +
  
  labs(x = "Publication Year", color = "") +  # no legend title
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12)
  )

p

# Save plot
ggsave(p, filename = file.path(path_plots, "length_text_and_mentions_one.png"),
       width = 8, height = 6, dpi = 300)

rm(plot)



# ----------------------------------------------------
# Plot: how many people mentioned per article, distribution
# with mean (absolute)
# ----------------------------------------------------

plot <- unique(dt_paper_level[, c("paper_id", "n_mentions")])
mean <- mean(plot$n_mentions)  # mean number of people mentioned per article

p <- ggplot(plot, aes(x = n_mentions)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "steelblue") +
  geom_vline(xintercept = mean, linetype = "dashed", color = "red4", size = 1) + 
  labs(
    x = "Number of Acknowledged People per Article",
    y = "Number of Articles"
  ) +
  scale_x_continuous(limits = c(-1, 85)) +   
  theme_minimal(base_size = 14)
p

ggsave(filename = paste0(path_plots, "distribution_people_per_article.png"),
       plot = p,  width = 8,  height = 6,  dpi = 300)


# Make sure plot has top5 column
# Example: counting number of people per paper
plot <- unique(dt_paper_level[, c("paper_id", "n_mentions", "top5")])
plot[, top5 := factor(top5, levels = c(1, 0), labels = c("Top 5", "Other"))]

# Overlayed histogram
p <- ggplot(plot, aes(x = n_mentions, fill = top5)) +
  geom_histogram(position = "identity", alpha = 0.5, binwidth = 1, color = "steelblue") +
  geom_vline(data = plot[, .(mean_N = mean(n_mentions)), by = top5],
             aes(xintercept = mean_N, color = top5),
             linetype = "dashed", size = 1) +
  scale_fill_manual(values = c("Top 5" = "darkorange", "Other" = "steelblue")) +
  scale_color_manual(values = c("Top 5" = "darkorange", "Other" = "steelblue")) +
  labs(
    x = "Number of Acknowledged People per Article",
    y = "Number of Articles",
    fill = "",
    color = ""
  ) +
  scale_x_continuous(limits = c(-1, 85)) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

p

ggsave(filename = paste0(path_plots, "distribution_people_per_article_with_journaltype.png"),
       plot = p,  width = 8,  height = 6,  dpi = 300)



# ----------------------------------------------------
# Plot: how many people mentioned per article
# RELATIVE distribution (all publications)
# ----------------------------------------------------

plot <- unique(dt_paper_level[, c("paper_id", "n_mentions")])
mean <- mean(plot$n_mentions)

p <- ggplot(plot, aes(x = n_mentions)) +
  geom_histogram(
    aes(y = after_stat(count / sum(count))),
    binwidth = 1,
    fill = "steelblue",
    color = "steelblue"
  ) +
  geom_vline(xintercept = mean, linetype = "dashed", color = "red4", size = 1) +
  labs(
    x = "Number of Acknowledged People per Article",
    y = "Share of Publications",
  ) +
  scale_x_continuous(limits = c(-1, 85)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14)

p

ggsave(
  filename = paste0(path_plots, "distribution_people_per_article_relative.png"),
  plot = p, width = 8, height = 6, dpi = 300
)


plot <- unique(dt_paper_level[, c("paper_id", "n_mentions", "top5")])
plot[, top5 := factor(top5, levels = c(1, 0), labels = c("Top 5", "Other"))]

p <- ggplot(plot, aes(x = n_mentions, fill = top5)) +
  geom_histogram(
    aes(y = after_stat(count / tapply(count, fill, sum)[fill])),
    position = "identity",
    alpha = 0.5,
    binwidth = 1,
    color = "steelblue"
  ) +
  geom_vline(
    data = plot[, .(mean_N = mean(n_mentions)), by = top5],
    aes(xintercept = mean_N, color = top5),
    linetype = "dashed",
    size = 1
  ) +
  scale_fill_manual(values = c("Top 5" = "darkorange", "Other" = "steelblue")) +
  scale_color_manual(values = c("Top 5" = "darkorange", "Other" = "steelblue")) +
  scale_x_continuous(limits = c(-1, 85)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Number of Acknowledged People per Article",
    y = "Share of Publications \n (within journal type)",
    fill = "",
    color = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

p

ggsave(
  filename = paste0(path_plots, "distribution_people_per_article_with_journaltype_relative.png"),
  plot = p, width = 8, height = 6, dpi = 300
)

# ----------------------------------------------------
# Plot: how many people mentioned how often
# ----------------------------------------------------

# Count total mentions per person
plot <- dt_person_level[, .(n_mentions = uniqueN(paper_id)), by = .(person_id)]

# Order by mentions
plot <- plot[n_mentions > 75, n_mentions := 75]
plot <- plot[order(n_mentions)]

# Create a numeric index for plotting
plot[, person_index := 1:.N]

# Percentile ticks
n_people <- nrow(plot)
percentiles <- seq(10, 100, by = 10)
tick_indices <- ceiling(n_people * percentiles / 100)
tick_labels <- paste0(percentiles, "%")

# Plot using numeric x-axis
p <- ggplot(plot, aes(x = person_index, y = n_mentions)) +
  geom_col(fill = "steelblue", width = 1) +
  scale_x_continuous(
    name = "Share within all people acknowledged",
    breaks = tick_indices,
    labels = tick_labels
  ) +
  scale_y_continuous(name = "Number of Mentions",
                     limits = c(-1,75)) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    legend.position = "none"
  )
p

ggsave(filename = paste0(path_plots, "distribution_mentions.png"),
       plot = p, width = 8, height = 6, dpi = 300)


# ----------------------------------------------------
# Table: how many mentions do top 10% and top 1% account for? 
# 49.67043% and 13.13994%
# ----------------------------------------------------

# Count total mentions per person
plot <- dt_person_level[, .(n_mentions = uniqueN(paper_id)), by = .(person_id)]

# Order by mentions descending
plot <- plot[order(-n_mentions)]

n_people <- nrow(plot)

# Top 10%
top_10_count <- ceiling(n_people * 0.10)
top_10_mentions <- sum(plot$n_mentions[1:top_10_count])
top_10_percent <- top_10_mentions / sum(plot$n_mentions) * 100

# Top 1%
top_1_count <- ceiling(n_people * 0.01)
top_1_mentions <- sum(plot$n_mentions[1:top_1_count])
top_1_percent <- top_1_mentions / sum(plot$n_mentions) * 100

top_10_percent
top_1_percent



# ----------------------------------------------------
# Table: how many of the top 10% are in REPEC TOP 5?
# ----------------------------------------------------

# get cut-off of top 10% of column n_mentions

dt_cutoff <- unique(dt_person_level[, .(person_id, n_mentions)])
cutoff_10 <- quantile(dt_cutoff$n_mentions, probs = 0.9, na.rm = TRUE) # 35

uniqueN(dt_person_level[dt_person_level$n_mentions > cutoff_10, person_id])  # 1,769
uniqueN(dt_person_level[dt_person_level$n_mentions <= cutoff_10, person_id])  # 17,033

dt_person_level[, top_10_mentions := fifelse(n_mentions > cutoff_10, 1, 0)]

# only one year (most recent?) per author

dt_mean <- dt_person_level
dt_mean[, max_year := max(publication_year, na.rm = TRUE), by = person_id]
dt_mean <- dt_mean[publication_year == max_year]

dt_mean <- unique(dt_mean[, .(person_id, top_10_mentions, top5_repec, avg_score, d_female)])

# REPEC SCORE
mean(dt_mean$avg_score[dt_mean$top_10_mentions == 1], na.rm = TRUE)  # 1482.054
mean(dt_mean$avg_score[dt_mean$top_10_mentions == 0], na.rm = TRUE)  # 1824.37
mean(dt_mean$avg_score, na.rm = TRUE)  # 1677.512


# Prob of being in REPEC Top 5
mean(dt_mean$top5_repec[dt_mean$top_10_mentions == 1], na.rm = TRUE)  # 0.5328587
mean(dt_mean$top5_repec[dt_mean$top_10_mentions == 0], na.rm = TRUE)  # 0.07532573
mean(dt_mean$top5_repec, na.rm = TRUE)  # 0.1192554


# Gender
mean(dt_mean$d_female[dt_mean$top_10_mentions == 1], na.rm = TRUE)  # 0.1172023
mean(dt_mean$d_female[dt_mean$top_10_mentions == 0], na.rm = TRUE)  # 0.2284639
mean(dt_mean$d_female, na.rm = TRUE)  # 0.2171943


# ----------------------------------------------------
# Plot: Who are most mentioned?
# ----------------------------------------------------

plot_top_mentions <- function(dt, 
                              gender_filter = NULL, 
                              top_n = 20,
                              save_path = NULL) {  
  dt <- as.data.table(dt[!is.na(person_id)])  # ensure person_id not NA
  
  # optional gender filter
  if (!is.null(gender_filter)) {
    dt <- dt[gender == gender_filter]
  }
  
  # For each person_id, take the shortest name (to handle spelling variants)
  dt[, display_name := name[which.min(nchar(name))], by = person_id]
  
  # Sum mentions per person per journal
  dt_counts <- dt[, .(n_mentions = uniqueN(paper_id)), 
                  by = .(person_id, display_name, journal_short)]

    # Total mentions per person
  dt_total <- dt_counts[, .(total_mentions = sum(n_mentions)), by = .(person_id, display_name)]
  
  # Get top N person_ids by total mentions
  top_ids <- dt_total[order(-total_mentions)][1:top_n, person_id]
  dt_top <- dt_counts[person_id %in% top_ids]
  
  # Order names by total mentions for plotting
  dt_top[, display_name := factor(display_name, 
                                  levels = dt_total[person_id %in% top_ids][order(-total_mentions), display_name])]
  
  # Define Top 5 journals and map others to "Other Journals"
  top5_journals <- c("AER", "ECMA", "JPE", "QJE", "RESTUD")
  dt_top[, journal_group := ifelse(journal_short %in% top5_journals, journal_short, "Other Journals")]
  
  # Make journal_group a factor to control stacking order: Top5 first, then Other Journals
  dt_top[, journal_group := factor(journal_group, levels = c(top5_journals, "Other Journals"))]
  
  # Assign colors: Top5 get distinct colors, Other Journals is blue
  top5_colors <- c(
    "tan",  # purple-red
    "lightsalmon",  # dark orange
    "#FF7F00",  # red
    "sienna3",  # pinkish-red
    "red4"   # aubergine
    )  
  color_map <- c(setNames(top5_colors, top5_journals), "Other Journals" = "steelblue")
  
  # Plot
  p <- ggplot(dt_top, aes(x = display_name, y = n_mentions, fill = journal_group)) +
    geom_col(position = "stack") +
    coord_flip() +
    labs(x = "", y = "Mentions", fill = "") +
    scale_fill_manual(values = color_map) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(size = 12)
    )
  
  # Optionally save
  if (!is.null(save_path)) {
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


# ----------------------------------------------------
# Table: Gender Distr. of Acknowledged Contributors
# ----------------------------------------------------

# person_level
uniqueN(dt_person_level$person_id)  # 18,802 unique people
uniqueN(dt_person_level$person_id[dt_person_level$gender == "male"])  # 12,195 unique people
uniqueN(dt_person_level$person_id[dt_person_level$gender == "female"])  # 3,402 unique people
uniqueN(dt_person_level$person_id[is.na(dt_person_level$gender)])  # 3,207 unique people


#paper_level

# all papers
dt_paper_level[,.N] # 70,837
dt_paper_level[,.N, by = "gender"]

# top 5 papers
dt_paper_level[top5 == 1 ,.N,]
dt_paper_level[top5 == 1 ,.N, by = "gender"]

# other journal papers
dt_paper_level[top5 == 0 ,.N,]
dt_paper_level[top5 == 0 ,.N, by = "gender"]

# ----------------------------------------------------
# Plot: Share of Ackn. Females per Paper
# ----------------------------------------------------

# contributor share per article, by gender

plot <- dt_paper_level[, .N, by = .(title_paper, gender)]
plot <- dcast(plot, title_paper ~ gender, value.var = "N", fill = 0)
plot[, total := female + male]
plot[, female_share := female/total*100]
plot[, male_share := male/total*100]
plot <- plot[total > 0, ]  # only keep those with at least one mention

mean_female <- mean(plot$female_share)  
mean_male <- mean(plot$male_share)  

# female
p <- ggplot(plot, aes(x = female_share)) +
  geom_histogram(
    aes(y = ..count../sum(..count..)*100),  # y-axis in percent
    binwidth = 1, fill = "steelblue", color = "black"
  ) +
  geom_vline(xintercept = mean_female, linetype = "dashed", color = "red", size = 1) + 
  labs(
    x = "Share of Acknowledged Female Contributors per Article",
    y = "Frequency (%)"
  ) +
  theme_minimal(base_size = 14)

p

ggsave(filename = paste0(path_plots, "female_share_per_article.png"),
       plot = p,  width = 8,  height = 6,  dpi = 300)


# ----------------------------------------------------
# Plot: How does it vary who I mention by authors?
# ----------------------------------------------------


# author information
load(file.path(path_data, "gen/dt_paper_authors.RData"))

dt_paper_authors <- unique(dt_paper_authors[, .(paper_id, team_size, team_sh_female, team_any_female, team_any_star)])
dt_paper_level <- merge(dt_paper_level, dt_paper_authors, by = "paper_id", all.x = TRUE)



# ----------------------------------------------------
# Plot: Share of Ackn. Females per Paper, by Authors Gender
# ----------------------------------------------------

# contributor share per article, by gender
# build dataset
plot <- dt_paper_level[, .N, by = .(paper_id, gender, team_any_female)]
plot <- dcast(plot, paper_id + team_any_female ~ gender, value.var = "N", fill = 0)

plot[, total := female + male]
plot[, female_share := female / total * 100]
plot <- plot[total > 0]

# compute means per group
means <- plot[, .(mean_female = mean(female_share)), by = team_any_female]

# plot
p <- ggplot(plot, aes(x = female_share)) +
  geom_histogram(
    aes(y = ..count../sum(..count..)*100),
    binwidth = 1, fill = "steelblue", color = "black"
  ) +
  geom_vline(
    data = means,
    aes(xintercept = mean_female),
    linetype = "dashed", color = "red", size = 1
  ) +
  facet_wrap(~ team_any_female, 
             labeller = labeller(team_any_female = c(
               "0" = "No female in team",
               "1" = "At least one female in team"
             ))) +
  labs(
    x = "Share of Acknowledged Female Contributors per Article",
    y = "Frequency (%)"
  ) +
  theme_minimal(base_size = 14)

p

ggsave(filename = paste0(path_plots, "female_share_per_article.png"),
       plot = p,  width = 8,  height = 6,  dpi = 300)




# ----------------------------------------------------
# Plot: how many people mentioned per article, distribution
# with mean (absolute), split by female in authors or not
# ----------------------------------------------------


# Make sure plot has top5 column
# Example: counting number of people per paper
plot <- unique(dt_paper_level[, c("paper_id", "n_mentions", "team_any_female")])
plot[, team_any_female := factor(team_any_female, levels = c(1, 0), labels = c("Min. 1 Female Author", "No Female Author"))]
plot <- plot[!is.na(team_any_female),] # keep only those with known gender

# Overlayed histogram
p <- ggplot(plot, aes(x = n_mentions, fill = team_any_female)) +
  geom_histogram(position = "identity", alpha = 0.5, binwidth = 1, color = "black") +
  geom_vline(data = plot[, .(mean_N = mean(n_mentions)), by = team_any_female],
             aes(xintercept = mean_N, color = team_any_female),
             linetype = "dashed", size = 1) +
  scale_fill_manual(values = c("Min. 1 Female Author" = "darkorange", "No Female Author" = "steelblue")) +
  scale_color_manual(values = c("Min. 1 Female Author" = "darkorange", "No Female Author" = "steelblue")) +
  labs(
    x = "Number of Acknowledged People per Article",
    y = "Number of Articles",
    fill = "",
    color = ""
  ) +
  scale_x_continuous(limits = c(-1, 85)) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

p




# ----------------------------------------------------
# Plot: how many people mentioned per article, distribution
# with mean (absolute), split by star in authors or not
# ----------------------------------------------------


# Make sure plot has top5 column
# Example: counting number of people per paper
plot <- unique(dt_paper_level[, c("paper_id", "n_mentions", "team_any_star")])
plot[, team_any_star := factor(team_any_star, levels = c(1, 0), labels = c("Star Author", "Other"))]

# Overlayed histogram
p <- ggplot(plot, aes(x = n_mentions, fill = team_any_star)) +
  geom_histogram(position = "identity", alpha = 0.5, binwidth = 1, color = "black") +
  geom_vline(data = plot[, .(mean_N = mean(n_mentions)), by = team_any_star],
             aes(xintercept = mean_N, color = team_any_star),
             linetype = "dashed", size = 1) +
  scale_fill_manual(values = c("Star Author" = "darkorange", "Other" = "steelblue")) +
  scale_color_manual(values = c("Star Author" = "darkorange", "Other" = "steelblue")) +
  labs(
    x = "Number of Acknowledged People per Article",
    y = "Number of Articles",
    fill = "",
    color = ""
  ) +
  scale_x_continuous(limits = c(-1, 85)) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

p


# relative makes more sense because there are fewer female authored papers...
# then there is pretty much no gender differences


plot <- unique(dt_paper_level[, c("paper_id", "n_mentions", "team_any_female")])
plot[, team_any_female := factor(team_any_female, levels = c(1, 0), labels = c("Min. 1 Female Author", "No Female Author"))]
plot <- plot[!is.na(team_any_female),] # keep only those with known gender

p <- ggplot(plot, aes(x = n_mentions, fill = team_any_female)) +
  geom_histogram(
    aes(y = after_stat(count / tapply(count, fill, sum)[fill])),
    position = "identity",
    alpha = 0.5,
    binwidth = 1,
    color = "black"
  ) +
  geom_vline(
    data = plot[, .(mean_N = mean(n_mentions)), by = team_any_female],
    aes(xintercept = mean_N, color = team_any_female),
    linetype = "dashed",
    size = 1
  ) +
  scale_fill_manual(values = c("Min. 1 Female Author" = "darkorange", "No Female Author" = "steelblue")) +
  scale_color_manual(values = c("Min. 1 Female Author" = "darkorange", "No Female Author" = "steelblue")) +
  scale_x_continuous(limits = c(-1, 85)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Number of Acknowledged People per Article",
    y = "Share of Publications \n (within author type)",
    fill = "",
    color = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

p


plot[, .(mean_N = mean(n_mentions)), by = team_any_female]
View(plot)


# ----------------------------------------------------
# Plot: Share of Ackn. Females per Paper by Author Team Gender
# ----------------------------------------------------

# contributor share per article, by gender
plot <- dt_paper_level[, .N, by = .(paper_id, gender, team_any_female)]

plot <- dcast(plot, paper_id + team_any_female ~ gender, value.var = "N", fill = 0)

plot[, total := female + male]
plot[, female_share := female / total * 100]
plot <- plot[total > 0]

plot[, team_any_female := factor(
  team_any_female,
  levels = c(1, 0),
  labels = c("Min. 1 Female Author", "No Female Author")
)]
plot <- plot[!is.na(team_any_female)]

p <- ggplot(plot, aes(x = female_share, fill = team_any_female)) +
  geom_histogram(
    aes(y = after_stat(count / tapply(count, fill, sum)[fill])),
    position = "identity",
    alpha = 0.5,
    binwidth = 1,
    color = "black"
  ) +
  geom_vline(
    data = plot[, .(mean_female = mean(female_share)), by = team_any_female],
    aes(xintercept = mean_female, color = team_any_female),
    linetype = "dashed",
    size = 1
  ) +
  scale_fill_manual(values = c("Min. 1 Female Author" = "darkorange", "No Female Author" = "steelblue")) +
  scale_color_manual(values = c("Min. 1 Female Author" = "darkorange", "No Female Author" = "steelblue")) +
  scale_x_continuous(limits = c(-1, 101)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Share of Acknowledged Female Contributors per Article",
    y = "Share of Publications",
    fill = "",
    color = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

p

ggsave(filename = paste0(path_plots, "female_share_per_article_author_gender.png"),
       plot = p,  width = 8,  height = 6,  dpi = 300)


