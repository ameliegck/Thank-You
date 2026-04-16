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

# authors
load(file.path(path_data, "gen/dt_paper_authors.RData"))



################################
#### CREATE MORE VARS
###############################


dt_paper_level[, n_mentions_star := uniqueN(person_id[!is.na(person_id) & top5_repec == 1]), by = paper_id]
dt_paper_level[, n_mentions_female := uniqueN(person_id[!is.na(person_id) & d_female == 1]), by = paper_id]

# merge author information
dt_paper_level <- unique(merge(
  dt_paper_level,
  dt_paper_authors[, .(paper_id, team_size, team_sh_female, team_any_female, team_any_star)],,
  by = "paper_id",
  all.x = TRUE,
  allow.cartesian = T
))


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

keeper <- c("paper_id", "title_paper", "publication_year", "journal_short", "top5", "field", 
            "times_cited_wos_score", "times_cited_all_databases", "n_mentions", "n_mentions_star", "n_mentions_female",
            "team_sh_female", "team_any_female", "team_size", "team_any_star")
dt_paper_wide <- unique(dt_paper_level[, ..keeper])

# change format to numeric
dt_paper_wide[, times_cited_wos_score := as.numeric(times_cited_wos_score)]
dt_paper_wide[, times_cited_all_databases := as.numeric(times_cited_all_databases)]
dt_paper_wide[, n_mentions := as.numeric(n_mentions)]
dt_paper_wide[, n_mentions_star := as.numeric(n_mentions_star)]
dt_paper_wide[, n_mentions_female := as.numeric(n_mentions_female)]
dt_paper_wide[, team_sh_female := as.numeric(team_sh_female)]
dt_paper_wide[, team_any_female := as.numeric(team_any_female)]
dt_paper_wide[, team_size := as.numeric(team_size)]
dt_paper_wide[, team_any_star := as.numeric(team_any_star)]


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
  log(times_cited_all_databases) ~ n_mentions | field +  publication_year,
  data = dt_paper_wide,
  cluster = ~field
)

m3 <- feols(
  log(times_cited_all_databases) ~ n_mentions + n_mentions_star | field + publication_year,
  data = dt_paper_wide,
  cluster = ~field
)

m4 <- feols(
  log(times_cited_all_databases) ~ n_mentions + n_mentions_star + top5 | field + publication_year,
  data = dt_paper_wide,
  cluster = ~field
)

m5 <- feols(
  log(times_cited_all_databases) ~ n_mentions + n_mentions_star | journal_short + field + publication_year,
  data = dt_paper_wide,
  cluster = ~field
)
summary(m5)


m6 <- feols(
  log(times_cited_all_databases) ~ n_mentions + n_mentions_star + team_size | journal_short + field + publication_year,
  data = dt_paper_wide,
  cluster = ~field
)


m7 <- feols(
  log(times_cited_all_databases) ~ n_mentions + n_mentions_star + team_size + team_any_female | journal_short + field + publication_year,
  data = dt_paper_wide,
  cluster = ~field
)


m8 <- feols(
  log(times_cited_all_databases) ~ n_mentions + n_mentions_star + team_size  + team_any_female + team_any_star | journal_short + field + publication_year,
  data = dt_paper_wide,
  cluster = ~field
)



# nicer way to show tables (and combine several models)
# effects of mentions and journals (table 1)
etable(m1, m2, m3, m4, m5)

# export
etable(
  m1, m2, m3, m4, m5,
  headers = c("# Cited", "Log(# Cited)", "Log(# Cited)", "Log(# Cited)", "Log(# Cited)"),
  dict = c(
    n_mentions = "Mentions",
    n_mentions_star = "Mentions $\\times$ Star",
    top5 = "Top 5 Journal"
  ),
  drop = "Intercept",
  fitstat = c("n", "r2"),
  tex = TRUE,
  title = "Effect of Mentions on Citations",
  label = "tab:citations",
  file = paste0(path_plots, "results.tex")
)


# effects of mentions and journals (table 2)
etable(m5, m6, m7, m8)

# export
etable(
  m5, m6, m7, m8,
  headers = c("Log(# Cited)", "Log(# Cited)", "Log(# Cited)", "Log(# Cited)"),
  dict = c(
    n_mentions = "Mentions",
    n_mentions_star = "Mentions $\\times$ Star",
    team_size = "# Authors",
    team_any_female = "Min. 1 Female Author",
    team_any_star = "Min. 1 Star Author"
  ),
  drop = "Intercept",
  fitstat = c("n", "r2"),
  tex = TRUE,
  title = "Effect of Mentions on Citations",
  label = "tab:citations",
  file = paste0(path_plots, "results2.tex")
)


# turn into a plot
# mentions and mentions stars are the two estimates plotted, with 95% CI
# first specification is m3, then m5, m6, m7, m8
# on x-axis add which controls are included (journal, team size, team composition)
# scale on y-axis for effect size


# ----------------------------
# 1. Store models
# ----------------------------
models <- list(
  m3 = m3,
  m5 = m5,
  m6 = m6,
  m7 = m7,
  m8 = m8
)

# ----------------------------
# 2. Define labels ONCE here
# ----------------------------
control_labels <- c(
  m3 = "+ Field FE\n+ Pub. Year FE",
  m5 = "+ Journal FE",
  m6 = "+ Team Size",
  m7 = "+ ≥1 Female Author",
  m8 = "+ ≥1 Star Author"
)

# ----------------------------
# 3. Extract coefficients
# ----------------------------
extract_coefs <- function(model, model_name) {
  coefs <- summary(model)$coeftable
  
  dt <- data.table(
    term = rownames(coefs),
    estimate = coefs[, "Estimate"],
    se = coefs[, "Std. Error"]
  )
  
  dt <- dt[term %in% c("n_mentions", "n_mentions_star")]
  
  dt[, `:=`(
    conf_low = estimate - 1.96 * se,
    conf_high = estimate + 1.96 * se,
    model = model_name
  )]
  
  return(dt)
}

# ----------------------------
# 4. Build plotting dataset
# ----------------------------
dt_plot <- rbindlist(
  lapply(names(models), function(m) extract_coefs(models[[m]], m))
)

# map labels (ONLY place where labels are used)
dt_plot[, controls := control_labels[model]]

# variable labels
dt_plot[, term := fifelse(term == "n_mentions", "Mentions", "Star Mentions")]

# order x-axis automatically from label vector
dt_plot[, controls := factor(controls, levels = control_labels)]

# ----------------------------
# 5. Plot
# ----------------------------
p <- ggplot(dt_plot, aes(x = controls, y = estimate, color = term)) +
  geom_point(position = position_dodge(width = 0.4), size = 2) +
  geom_errorbar(
    aes(ymin = conf_low, ymax = conf_high),
    width = 0.2,
    position = position_dodge(width = 0.4)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c(
    "Mentions" = "#1f77b4",
    "Star Mentions" = "#d62728"
  )) +
  labs(
    x = "",
    y = "Log(citations)",
    color = "",
    title = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 0)  # rotate if needed
  )
p

ggsave(filename = paste0(path_plots, "reg_cits_mentions.png"),
       plot = p,  width = 8,  height = 6,  dpi = 300)




