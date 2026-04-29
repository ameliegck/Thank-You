rm(list = ls()) 

if(!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, tidyr, readr, readxl, ggplot2, RColorBrewer, stringr, stringi, stringdist, igraph, openxlsx) # add any packages here

if(Sys.info()["user"] %in% c("Amelie.Grosenick")){
  path_wd <- c("D:/Projektfolder1 (Miethe, Grosenick)/zz_AmelieMisc/")
  path_data <- c("D:/Projektfolder1 (Miethe, Grosenick)/zz_AmelieMisc/thankyou/data/")
  path_plots <- c("D:/Projektfolder1 (Miethe, Grosenick)/zz_AmelieMisc/thankyou/output/")
  
}

setwd(path_wd)
set.seed(1202)

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
# Clean names (REPEC and acknowledged people and authors)
# Create unique person ID
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
    return(trimws(x))
  }
  
  paste(parts[[2]], parts[[1]])
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

# drop helper column
dt_paper_authors[, author_list := NULL]

# create columns name and author_of_paper_id as only data set

author_cols <- grep("^author_\\d+$", names(dt_paper_authors), value = TRUE)
cat("Max number of author columns:", length(author_cols))

# Melt all author columns to long format
dt_paper_authors <- melt(
  dt_paper_authors,
  id.vars = "paper_id",
  measure.vars = author_cols,
  variable.name = "author_order",
  value.name = "name"
)[!is.na(name) & name != ""]

# Keep only relevant columns
dt_paper_authors <- unique(dt_paper_authors[, .(name, author_of_paper_id = paper_id)])

# this is a mapping from original name to paper they have written.
# if original names differ I could not say they are same author, e.g. Larry Katz and Lawrence Katz are not the same
# keep original name for now and create grouping with cleaned name later when I created person_id
# LIST OF ALL PEOPLE ACKNOWLEDGED
# ----------------------------------------------------


dt_people <- unique(dt_paper_level[!is.na(person_name), .(person_name)])
dt_people <- dt_people[, orig_name := person_name]
setnames(dt_people, old = "person_name", new = "name")


# COMBINE AUTHORS AND ACKNOWLEDGED
# ----------------------------------------------------

dt_paper_authors[, orig_name := name]
dt_people <- unique(rbind(dt_people, dt_paper_authors[, .(name, orig_name)], fill = TRUE))




# CLEAN NAMES IN THE SAME WAY 
# ----------------------------------------------------

# function
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
dt_paper_authors <- clean_names(dt_paper_authors, "name")


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
  funcdata[get(name_col) %like% "^Jeff ", (name_col) := str_replace(get(name_col), "^Jeff ", "Jeffrey ")]
  
   
  return(funcdata)
}

dt_people <- manual_adjustments(dt_people, "name")
dt_paper_authors <- manual_adjustments(dt_paper_authors, "name")
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
  "Michael Reher", "Michael Gechter", "Michael Bailey",   "Michael Baker", "Michael Bates",
  "Michael Bauer", "Michael Burkart", "Michael Choi", "Michael Darden", "Michael Grill", 
  "Michael Hasler", "Michael Hurd", "Michael Magill", "Michael Mandler", "Michael Mas",
  "Michael McAleer", "Michael McMahon", "Michael Mcmahon", "Michael Mi", "Michael Shi",
  "Michael So", "Michael Thaler", "Michael Walker", "Michael Walton", "Michael Waterson",
  
  # Wang
  "Pengfei Wang", "Peng Wang", "Neng Wang", "Ping Wang", "Rong Wang", "Ying Wang", "Cong Wang",
  
  # Li / Liu / Lu / Lin / Xu / Sun
  "Jia Li", "Jian Li", "Qi Liu", "Yan Li", "Ying Li", "Jin Li", "Yang Lu", "Nan Li",
  "Qiao Liu", "Jiasun Li", "Xing Li", "Yan Liu", "Yang K. Lu", "Yang Su", "Yi Li", "Yi Lin",
  "Ming Li", "Jing Li", "Ming Lu", "Mingzhi Xu", "Yang Li", "Bin Li", "Bin Liu", "Qian Li",
  "Xin Liu", "Yang Liu", "Qing Liu", "Nan Liu", "Qihong Liu", "Yanan Li", "Ming Xu",
  "Yicong Lin", "Jaisun Li", "Ting Xu", "Xin Lu", "Yang Sun", "Bing Lu","Bing Zhang",
  "Chang Liu","Chaojun Wang","Chen Li","Chen Lian","Chen Lin","Chen Wang","Chen Xu Li",
  "Chen Zhang","Cheng Liu","Cheng Wang","Cheng Yan","Chenxu Li","Chong Huang","Chong Liang",
  "Chong Liu","Chong Wang","Cong Liu","Deyuan Li","Ding Liu","Dong Li","Dong Liu",
  "Dong Lou","Dong Lu","Dong Wang","Duan Li","Feifang Hu","Feiqing Huang","Feng Huang",
  "Feng Liu","Gang Li","Han Li","Han Liu","Hang Yu","Haoyang Liu","Hong Xiang","Jiacheng Liu",
  "Jiajing Sun","Jianan Liu","Jiang Li","Jiangtao Li","Jianguo Wang","Jianwen Li",
  "Jiawen Liu","Jing Fang","Jing Liu","Jing Zhang","Jinghua Lei","Jingyi Zhang",
  "Jinhu Li","Lei Li","Li Li","Li Liu","Liang Jiang","Liang Wang","Lin Lin","Lin Liu",
  "Lina Lu","Luhang Wang","Luheng Wang","Nan Lin","Ning Zhang","Peng Zhang",
  "Ping Zhang","Qi Li","Qing Jiang","Qiong Zhang","Qiyuan Li","Shengwu Li",
  "Shengyu Li","Teng Li","Ting Zhang","Tong Liu","Tong Wang","Tony Zhang","Xian Jiang",
  "Xin Jin","Xin Lin","Xuan Liang","Yan Lu","Yan Sun","Yanbo Liu","Yang Yao","Yang You",
  "Yang Yu","Yanyan Liu","Ye Lu","Ye Luo","Yi Liu","Yi Lu","Yifan Jiang","Yifan Li",
  "Yin Liu","Yin Lu","Ying Fang","Ying Liu","Yiping Wang","Yiren Wang","Yong Li",
  "Yong Wang","Yu-Ning Li","Yuan Li","Yuan Liao","Yuan Zi","Yulong Wang","Yunan Ji",
  "Yunan Li","Zhaojun Wang","Zhe Li","Zhen Lei","Zheng Fang","Zheng Li",
  "Zheng Liu","Zheng Song","Zheng Wang","Zheng Zhang","Zheng Zhong","Zhengwei Wang","Ziang Li",
  "Jing Shi", "Ying Shi", "Daniel Stein", "Daniel Streitz",
  
  # Chen
  "Bin Chen","Bingxu Chen","Brian Chen","Fan Wang","Fang Fang","Fang Yang",
  "Frank Yang","Han Chen","Ji Zhang","Jia Chen","Jian Chen","Jiaqi Chen",
  "Jidong Chen","Jie Zheng","Jing Chen","Jiwei Zheng","Joyce Chen","Jun Chen",
  "Jun Zhang","Junwei Zhang","Kuo Zhang","Lan Zhang","Li Zhang","Liang Chen",
  "Lin Zhang","Lina Zhang","Linxiu Zhang","Liu Cheng","Liyan Yang","Liyao Wang",
  "Lu Zhang","Min Chen","Mingli Chen","Mu Zhang","Nan Chen","Nan Yang",
  "Nian Yang","Qi Zhang","Qiang Chen","Qiang Shen","Qitong Chen","Rong Chen",
  "Su Zhang","Tao Chen","Tao Wang","Tianyi Wang","Wei Zhong","Weijie Zhong",
  "Xiao Zhang","Xiaobo Zhang","Xiaoyu Zhang","Xibin Zhang","Xin Chang",
  "Xin Chen","Xing Chen","Xinyu Zhang","Xu Cheng","Xu Zhang","Xu Zheng",
  "Xuan Zhang","Yan Chen","Yan Zhang","Yang Tang","Yao Chen","Yao Wang",
  "Yapei Zhang","Yaxing Yang","Ye Chen","Yi Che","Yi Shen","Yi-Ting Chen",
  "Yifan Zhang","Yiling Chen","Ying-Ju Chen","Yiran Zhang","Yiting Chen",
  "Yong Chen","Yu Zheng","Yuehua Tang","Yuhao Wang","Yuting Chen","Yuyu Chen",
  "Zilin Chen","Ziyang Chen",
  "Bei Jiang", "Bin Jiang", "Lei Jiang", "Wei Jiang", "Wei Xiang", "Wei Xiong", 
  "Wenxi Jiang", "Jian Kang","Jian Wang","Jiao Wang","Qian Wang","Qian Weng",
  "Xia Wang","Xiao Wang","Xiao Yu Wang","Xiaohu Wang","Xiaolu Wang",
  "Zian Wang","Zigan Wang","Zihan Wang", "Bin Wang","Di Wang","He Wang",
  "He Yang","Jin Wang","Lei Wang","Si Wang","Siwei Wang","Wei Wang",
  "Xin Wang","Xinjie Wang","Xuexin Wang","Xun Tang","Yiru Wang","Yu Wang","Zhi Wang",
  "Dan Clark", "Laurent Calvet", "Laurent Callot",
  "Christian Hafner", "Christian Hansen", "Christian Henn", "Christian Wagner", "Christina Hans",
  "Christoph Mueller","Christoph Rothe","Christopher Chambers","Christopher Conlon","Christopher Cotton",
  "Christopher Jepsen","Christopher Jones","Christopher Kah","Christopher Kops","Christopher Ksoll",
  "Christopher Malloy","Christopher Mayer","Christopher Parmeter","Christopher Rauh",
  "Christopher Roth","Christopher Ruhm","Christopher Telmer",
  "Matthew Jacob", "Matthew Jackson", "Christoph Frei", "Christoph Siemroth", 
  "Christopher Sims", "Matthew Clance", "Matthew Cole", "Matthew Rabin",
  "Jun Li", "Jun Liao", "Jun Liu", "Junye Li", "Bin Zhu","Jian Zou","Jidong Zhou",
  "Jin Zhou","Jiong Zhu","Li-An Zhou","Liang Zou","Lin Zhou","Lin Zhu","Ling Zhou",
  "Liping Zhu","Lixing Zhu","Christian Kellner","Christian Krekel","Christian Merkl",
  "Christian Moser","Christian Roessler","Christian Saile","Christian Vossler",
  "Daniel Barron","Daniel Barth","Daniel Bennett","Daniel Bougt","Daniel Garrett",
  "Daniel Parent","Daniel Pena","Daniel Levin","Daniel Li","Daniel Liu",
  "Daniel Lu","Daniel Luo","Daniel Pu","Daniel Rees","Daniel Reeves",
  "Daniel Yi Xu","Danielle Li","Hao Zhang","Harold Chiang","Tao Zha",
  "Tao Zhang","Tao Zhu","Tao Zou","Martin Geiger","Martin Hagen","Martin Wagner",
  "Martin Watzinger","Martin Weber","Martin Weidner", "Francesco Campo", "Francisco Castro",
  "Francisco Costa", "Bo Zhou","Bu Zhou","Di Zhou","Yu Zhou","Chen Zhao","Chen Zhou",
  "Shen Zhao","Zhen Zhou", "Hui Chen","Rui Chen","Rui She","Rui Shen","Hui Li","Hui Lin",
  "Huiyu Li","Rui Lin","Jialin Huang","Jian Huang","Jing Huang","Jing-zhi Huang",
  "Jingyi Huang","Jian Xu","Jianpo Xue","Jianwei Xu","Jin Xu","Jianfeng Hu",
  "Jianfeng Yu","Jinfeng Luo","Jinfeng Xu","Jinan Zhang","Jinfan Zhang","Xingtan Zhang",
  "Xinran Zhang","Jingjing Yang","Jingjing Zhang","Qingyin Ma","Yingying Ma","Yingying Zhang",
  "Jonathan Eaton","Jonathan Newton","Jonathan Payne","Jonathan Seaton","Jonathan Zhang",
  "Maria Olsson","Martin Karlsson","Martin Larsson","Martin Olsson",
  "Matthias Meier","Matthias Messner","Matthias Rieger","Matthias Weber","Michael Huebler",
  "Michael Richter","Michael Webb","Michael Weber","Michael Wither","Richard Cole",
  "Richard Holden","Richard Lowery","Richard Tol","Wei Fan","Wei Lan","Wei Li",
  "Wei Liao","Wei Lin","Yan Xu","Yu Xu","Yuan Ju","Yuan Xu","Yuan Xue",
  "Bin Cheng","Bin Zhu","Binkai Chen","Cen Ling","Chan Wang","Chen Huang","Cheng Wan","Chengsi Wang",
  "Chong Xiang","Chris Moser","Christian Kellner","Christian Krekel","Christian Krestel","Christian Merkl",
  "Christian Moser","Christian Oertel","Christian Roessler","Christian Saile","Christian Seel","Christian Volpe",
  "Christian Vossler","Christian Waibel","Christoph Loch","Christoph Merkle","Christophe Chamley","Christopher Campos",
  "Christopher Cronin","Christopher Crowe","Christopher James","Christopher Koch","Christopher Muller","Christopher Powers","Christopher Snyder","Christopher Whaley","Christopher Wimer","Daniel Lee","Daniel Leeds","Daniel Leigh","Daniel Levin","Daniel Lewis","Daniel Li","Daniel Liu","Daniel Lu","Daniel Luo","Daniel Luo","Daniel Lyu","Daniel Pu","Daniel Reese","Daniel Reeves","Daniel Reyes","Daniel Xu","Daniel Xu","Daniel Yi Xu","DanielYi Xu","Danielle Li","Dongri Liu","Fan Yang","Fan Zhang","Feng Hu","Frank Zhang","Geng Li","Hao Chen","Hao Wang","Hayong Yun","Hong Luo","Hong Zhang","J Zhang","Jan Sun","Jane Zhang","Jason Zhang","Ji Shen","Jian Zou","Jiang Jiang","Jiang Wang","Jicheng Liu","Jidong Zhou","Jie Chen","Jie Zhang","Jieying Zhang","Jin Zhou","Jing Zhu","Jiong Zhu","Jipeng Zhang","Joe Chen","Joy Chen","Juan Chen","Juan Zhang","Junfu Zhang","Junjie Zhang","Kun Zhang","L Cohen","Le Wang","Lei Fang","Lei Zhang","Leifu Zhang","Li-An Zhou","Lian Yang","Liang Zou","Lin Chen","Lin Zhou","Lin Zhu","Ling Cen","Ling Zhou","Ling Zhu","Linke Zhu","Liping Zhu","Lixing Zhu","Liyuan Lin","Long Chen","Louis Cheng","Lu Zheng","M Zhu","MM Zhang","Mei Wang","Miao Zhang","Michael Baye","Michael Bayerlein","Michael Best","Michael Binder","Michael Boskin","Michael Brei","Michael Brewer","Michael Burda","Michael Cai","Michael Conlin","Michael Conlin","Michael Dalton","Michael Daly","Michael Gil","Michael Haines","Michael Hanemann","Michael Hanselmann","Michael Hansen","Michael Iselin","Michael Karp","Michael Karpman","Michael Landesmann","Michael Maier","Michael Manove","Michael Manville","Michael Mao","Michael McBride","Michael McKee","Michael McKenzie","Michael Melvin","Michael Schmidt","Michael Sposi","Michael West","Min Wang","Min Zhu","Ming Jiang","Mingyu Chen","Ning Zhu","Sheng Huang","Sheng Li","Su Wang","Tan Wang","Tao Shen","Tong Zhang","Wei Zhang","Weina Zhang","Wenyu Wang","X Chen","Xi Chen","Xin Shan","Xin Shen","Xin Tang","Xin Wan","Xin Zhang","Xinli Wang","Xuan Jiang","Y Lu","Yafei Zhang","Yang Yang","Ye Wang","Ye Zhang","Yi Wang","Yi Zhang","Yiman Sun","Ying Fan","Yiwei Zhang","Yiyao Wang","Yongyou Li","Yu Sheng","Yuhua Wang","Yun Zhang","Yunqi Zhang","Zhe Wang","Zhenyu Wang","Zhiren Wang","Zi Wang","Ziwei Wang","Zong Huang","Zongbo Huang",
  "Adres Rodriguez-Clare","Alexander Gleim","Alexander Hillert","Alexander Himbert","Alexander Klein",
  "Alexander Klemm","Alexander Miller","Alexander Morell","Alexander Willen","Andre Kuhn","Andreas Haller","Andreas Haufler",
  "Andreas Haulier","Andreas Kuhn","Andrew Chang","Andrew Chen","Andrew Coe","Andrew Garin","Andrew Green","Andrew Greenland","Andrew Griffen","Andrew John","Andrew Johnston","Andrew Jones","Andrew Kahn","Andrew Kao",
  "Andrew Karolyi","Andrew Koh","Andrew Rhodes","Andrew Rose","Andy Johnston","Benjamin Jung","Benjamin Junge","Benjamin Wong","Benjamin Young","Bo Zhao","Bo Zhou","Bu Zhou","C-Philipp Heller","Chao Li","Chao Xi","Chen Zhao","Chen Zhou","Daniel Barreto","Daniel Barth","Daniel Bennett","Daniel Bogart","Daniel Bogart","Daniel Bougt","Daniel Brent","Daniel O'Brien","Daniel Parent","Daniel Pena","Dave Brown","David Bravo","David Brown","David Brown","David Cashin","David Chapman","David Cimon","David Kamin","David Kang","David Mare","David Mayer","David Mayhew","David Moore","David Moreno","David Seim","David Simon","David Sims","David Skeel","David Skeie","David Zhang","Di Zhou","Drew Griffen","Francesca Cornelli","Francesco Campo","Francesco Caselli","Francesco Cinerella","Francesco Cinnirella","Francesco Daveri","Francesco Fabbri","Francesco Feri","Francesco Fusari","Francesco Passarelli","Francisco Campos","Francisco Campos-Ortiz","Francisco Ferreira","Haijun Wang","Hans Peter Grainer","Hans Peter Griiner","Hans Peter Griller","Hans Peter Grtiner","Hans Peter Gruener","Hans Peter Gruner","Hao Jiang","Hao Lan","Hao Liang","Hao Zhang","Harold Chiang","Harold Chiang","Harold Zhang","Harold Zhang","Hua Liang","Huiyu Li","Jenny Chan","Jenny Tang","Jenny Yang","Jenny Ying","Jenny Zhang","Jialin Huang","Jian Huang","Jin Yu","Jinan Zhang","Jinfan Zhang","Jing Huang","Jing Wu","Jing You","Jing Yu","Jing-zhi Huang","Jingjing Yang","Jingjing Zhang","Jingyi Huang","Jingzhi Huang","Johnathan Eaton","Johnny Tang","Jonathan Chiu","Jonathan Choi","Jonathan Cohen","Jonathan Colin","Jonathan Eaton","Jonathan Nam","Jonathan Newton","Jonathan Payne","Jonathan Seaton","Jonathan Talmi","Jonathan Tan","Jonathan Wallen","Jonathan Will","Jonathan Williams","Jonathan Zhang","Josh Mollner","Josh Pollet","Joshua Mollner","Joshua Pollet","Joshua Volle","Maria Olsson","Martijn Han","Martin Geiger","Martin Hagen","Martin Hearson","Martin Karlsson","Martin Larsson","Martin Olsson","Martin Wagener","Martin Wagner","Martin Watzinger","Martin Weber","Martin Weidner","Matthew Cain","Matthew Collin","Matthew Collin","Matthew Collin","Matthew Copley","Matthew Doyle","Matthew Rablen","Matthew Ryan","Mei Lin","Michael Bordo","Michael Bordo","Michael Borns","Michael Boutros","Michael Brown","Michael Burrows","Michael Huebler","Michael Reich","Michael Richter","Michael Siemer","Michael Silver","Michael Webb","Michael Weber","Michael Wiener","Michael Wither","Ning Yu","Onathan Cohen","P Philip","Philipp Heller","Philippe Sulger","Ping Yu","Qingyin Ma","Richard Cole","Richard Holden","Richard Howe","Richard Howitt","Richard Lowery","Richard Roll","Richard Tol","Richard Tol","Robert Bauer","Robert Bruno","Robert Burn","Robert Dur","Robert Durand","Robert Sauer","Robert Staiger","Robert Staiger","Robert Staiger","Rudi Bachman","Rui Liang","Rui Lin","Sebastian Braun","Sebastian Braun","Sebastian Grund","Sebastian Kranz","Sebastian Thrun","Sebastian Till Braun","Shan Zhao","Shen Zhao","Stephanie Bonds","Stephen Bond","Stephen Brown","Stephen Donald","Stephen Holland","Tao Jiang","Tao Jin","Tao Shu","Tao Zeng","Tao Zha","Tao Zhang","Tao Zhu","Tao Zou","Thomas Bang","Thomas Baranga","Thomas Dangl","Thomas Osang","Thomas Wang","Wei Fan","Wei Lan","Wei Li","Wei Liao","Wei Lin","Wenli Li","Will Gerken","William Free","William Fried","William Gerken","William Greene","X Li","Xin Huang","Xing Huang","Xingtan Zhang","Xinhan Zhang","Xinran Zhang","Xu Li","Xu Liu","Xu Lu","Xuan Wang","Xun Lu","Yajun Wang","Yan Xu","Yan Zeng","Yao Deng","Yao Zeng","Yao Zheng","Yi Zhou","Ying Wu","Yingying Fan","Yingying Ma","Yingying Zhang","Yu Xu","Yu Zhou","Yu Zhu","Yuan Ju","Yuan Wang","Yuan Xu",
  "Yuan Xue","Yun Zhu","Zehao Li","Zehao Liu","Zhao Li","Zhen Zhou","Zijun Wang",
  "Alexander Klos", "Chris Walker", "Chris Wallace", "Chris Waller", "Chris Walters",
  "Christian Wagner", "Christina Wang", "Christian Hansen", "Christian Hafner", "Christian Henry", "Christina Hans", "Christian Henn"
  )

  # Step 1: set person_id to NA for these names
  name_groups[name %in% manual_fix_names, person_id := NA]
  
  # Step 2: find the next available ID
  next_id <- max(name_groups$person_id, na.rm = TRUE) + 1
  
  # Step 3: assign new IDs to the NA rows
  name_groups[is.na(person_id), person_id := seq(next_id, length.out = .N)]
  
  # Step 4: verify
  name_groups[, .N, by = person_id]



# merge person_id to dt_people
dt_people <- merge(dt_people, name_groups[, .(name, person_id)], by = "name", all.x = TRUE)
uniqueN(dt_people$person_id)


# merge person_id to dt_paper_authors
dt_paper_authors <- merge(dt_paper_authors, name_groups[, .(name, person_id)], by = "name", all.x = TRUE)

# create a unique observation per author, based on her ID, pasting all papers she wrote together
# note: unique only by ID. if two people have same ID because of similar name the names will still be different but the papers collapsed

dt_paper_authors <- dt_paper_authors[, .(paper_ids = paste(unique(author_of_paper_id), collapse = ";"), name), by = person_id]
dt_paper_authors <- unique(dt_paper_authors)



# save
people_id <- copy(dt_people)
save(people_id, file = file.path(path_data, "gen/acknowledged_with_person_id.RData"))

dt_paper_authors <- copy(dt_paper_authors)
save(dt_paper_authors, file = file.path(path_data, "gen/dt_paper_authors.RData"))


# who of people is acknowledged
ids_acknowledged <- unique(dt_people[!is.na(person_id), person_id])


rm(unique_names, dist_matrix, adj, g, clusters, name_groups, people_id)


# ----------------------------------------------------
# Merge person_id to REPEC Data
# ----------------------------------------------------

# quite complex because fuzzyjoin package not available for this R version??

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

# manual adjustments

repec_data[name == "Jeffrey Marc Wooldridge", person_id := 536]


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



# ============================================================
# FUNCTION 1: Prepare names for gender search (to feed in AI)
# ============================================================
prepare_gender_search <- function(
    dt,
    col_name      = "name",
    col_person_id = "person_id",
    path_data,
    file_already_searched
) {
  dt <- copy(dt)
  
  # ---- 1. Extract first name ----
  dt[, first_name := str_split_fixed(get(col_name), " ", 2)[, 1]]
  dt[!is.na(first_name), first_name := gsub("\\.", "", first_name)]
  dt[, first_name := trimws(first_name)]
  dt[nchar(first_name) < 2, first_name := NA]
  
  # ---- 2. Pick longest first name per person ----
  dt[, length     := nchar(first_name)]
  dt[, max_length := max(length, na.rm = TRUE), by = c(col_person_id)]
  dt[length == max_length, name_to_search := first_name]
  dt[,
     name_to_search := {
       tmp <- name_to_search[!is.na(name_to_search)]
       if (length(tmp) == 0) NA_character_ else tmp[1]
     },
     by = c(col_person_id)
  ]
  dt[, name_to_search := tolower(name_to_search)]
  
  # ---- 3. Split into already searched vs still needed ----
  names_already_searched <- data.table::fread(
    file.path(path_data, "gen", file_already_searched)
  )
  names_already_searched <- names_already_searched[-1, ]
  names_already_searched <- unique(names_already_searched$found_name)
  
  all_names      <- unique(dt$name_to_search[!is.na(dt$name_to_search)])
  names_to_search <- setdiff(all_names, names_already_searched)
  
  message(length(names_to_search), " new names to search for gender.")
  message(length(names_already_searched), " names already searched.")
  
  # ---- 4. Export names still to search ----
  df_to_export <- data.frame(name_to_search = names_to_search)
  # Uncomment to export:
  write.xlsx(df_to_export,
             file = file.path(path_data, "gen/names_to_search_gender.xlsx"),
             sheetName = "NamesToSearch", overwrite = TRUE)
  
  return(df_to_export)
}


# ============================================================
# FUNCTION 2: Apply gender after AI search is done
# ============================================================
apply_gender <- function(
    dt,
    col_name      = "name",
    col_person_id = "person_id",
    path_data,
    file_already_searched,
    file_new_gender,
    prob_threshold = 90
) {
  dt <- copy(dt)
  
  # ---- Manual overrides: edit these directly ----
  male_ids <- c(
    "17195", "5244", "346", "9941", "86", "1984", "1688"
  )
  male_names <- c(
    "jean-baptiste", "jean-benoit", "jean-claude", "jean-edouard",
    "jean-francois", "jean-francoiswen", "jean-laurent", "jean-marie",
    "jean-michel", "jean-noel", "jean-philippe", "jean-pierre",
    "jean-sebastien", "jean-yves", "edward", "scott", "stephane", "stefano",
    "gauti", "lawrence", "hunt", "briant", "erik"
  )
  
  female_names <- c("nava", "uta", "alessandra", "barbara")
  
  # -----------------------------------------------
  
  # ---- 1. Reconstruct name_to_search (same as Function 1) ----
  dt[, first_name := str_split_fixed(get(col_name), " ", 2)[, 1]]
  dt[!is.na(first_name), first_name := gsub("\\.", "", first_name)]
  dt[, first_name := trimws(first_name)]
  dt[nchar(first_name) < 2, first_name := NA]
  dt[, length     := nchar(first_name)]
  dt[, max_length := max(length, na.rm = TRUE), by = c(col_person_id)]
  dt[length == max_length, name_to_search := first_name]
  dt[,
     name_to_search := {
       tmp <- name_to_search[!is.na(name_to_search)]
       if (length(tmp) == 0) NA_character_ else tmp[1]
     },
     by = c(col_person_id)
  ]
  dt[, name_to_search := tolower(name_to_search)]
  
  # ---- 2. Load already gendered names ----
  gender <- data.table::fread(file.path(path_data, "gen", file_already_searched))
  setnames(gender, "Col 1", "name_to_search")
  gender[, name_to_search := tolower(name_to_search)]
  
  # ---- 3. Merge gender onto person data ----
  dt[is.na(name_to_search), name_to_search := first_name]
  dt <- merge(dt, gender, by = "name_to_search", all.x = TRUE)
  
  # ---- 4. Manual overrides ----
  if (length(male_ids) > 0)
    dt[get(col_person_id) %in% male_ids, `:=`(gender = "male", probability = 100)]
  
  if (length(male_names) > 0)
    dt[name_to_search %in% tolower(male_names), `:=`(gender = "male", probability = 100)]
  
  if (length(female_names) > 0)
    dt[name_to_search %in% tolower(female_names), `:=`(gender = "female", probability = 100)]
  
  # ---- 5. Clean up temp columns ----
  cols_to_drop <- intersect(
    names(dt),
    c("country", "length", "max_length", "first_name", "found_name", "name_to_search")
  )
  dt <- dt[, !cols_to_drop, with = FALSE]
  
  # ---- 6. Apply probability threshold ----
  dt[probability < prob_threshold, gender := NA]
  
  # ---- 7. Female dummy ----
  dt[, d_female := ifelse(gender == "female", 1L, 0L)]
  dt[is.na(gender), d_female := NA_integer_]
  
  return(dt)
}


# ----------------------------------------------------
# Create PERSON-level data set (of mentioned people, not authors)
# ----------------------------------------------------

dt_person_level <- unique(dt_paper_level[!is.na(person_id), c("person_id", "name", "paper_id", "publication_year", "avg_score", "top5_repec")])
dt_person_level <- clean_names(dt_person_level, "name")
dt_person_level <- manual_adjustments(dt_person_level, "name")

# how often mentioned across all years
dt_person_level[, n_mentions := .N, by = person_id]



# ----------------------------------------------------
# Apply Gender Functions 
# ----------------------------------------------------

# person-level data

# Step 1: prepare & inspect which names still need to be fed into AI search
names_to_search <- prepare_gender_search(
  dt                    = dt_person_level,
  path_data             = path_data,
  file_already_searched = "wos_acknowledged_names_after_gender.csv"
)

# --- go do the gender search, save results to gen/ folder ---

file_already_searched <- "wos_acknowledged_names_after_gender.csv"
file_new <- "names_to_search_gender_after_2026_04_01.csv"

gender_new      <- data.table::fread(file.path(path_data, "gen", file_new))
gender_existing <- data.table::fread(file.path(path_data, "gen", file_already_searched))
gender_existing[, V1 := NULL]
gender <- unique(rbind(gender_new, gender_existing))

write.csv(gender, file.path(path_data, "gen", file_already_searched)) # overwrite file_already_searched


# Step 2: apply gender once search is done
dt_person_level <- apply_gender(
  dt                    = dt_person_level,
  path_data             = path_data,
  file_already_searched = "wos_acknowledged_names_after_gender.csv",
  prob_threshold        = 90
)


# check whether it is the same gender by person_id (or varies)
dt_person_level[, count := uniqueN(gender), by = person_id]
summary(dt_person_level$count)

# it does but for very few where it is one gender and also NA
# View(dt_person_level[count > 1])
dt_person_level <- dt_person_level[count == 1 | (count > 1 & !is.na(gender))]


dt_person_level[, count := uniqueN(gender), by = person_id]
summary(dt_person_level$count)

dt_person_level[, count := NULL]


# match to paper-level data
############################################

dt_id_gender <- unique(dt_person_level[, .(person_id, gender, d_female)])

dt_paper_level <- merge(
  dt_paper_level, dt_id_gender,
  by = "person_id", all.x = TRUE
)

table(dt_person_level$d_female)



# match to author-level data
############################################

dt_paper_authors <- merge(
  dt_paper_authors, dt_id_gender,
  by = "person_id", all.x = TRUE
)

table(dt_paper_authors$d_female)




####################################################
# match abstract and keywords to papers
####################################################

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


# turn author-level data into paper level data
# cols: pub year, author, gender, repec
# can have several observations per paper if several authors, but that's fine for now, we can aggregate later if needed
############################################

dt_paper_authors <- dt_paper_authors[, {
  ids <- unlist(strsplit(paper_ids, ";"))
  .(paper_id  = ids,
    person_id = person_id,
    name      = name,
    gender    = gender,
    d_female  = d_female)
}, by = 1:nrow(dt_paper_authors)][]

# add publication year
dt_paper_authors[, paper_id := as.integer(paper_id)]

dt_paper_authors <- unique(merge(
  dt_paper_authors, dt_paper_level[, .(paper_id, publication_year)],
  by = "paper_id", all.x = TRUE, allow.cartesian = T
))

# add REPEC data

dt_paper_authors <- merge(
  dt_paper_authors,
  repec_data[!is.na(person_id), .(person_id, publication_year, avg_score)],
  by.x = c("person_id", "publication_year"),
  by.y = c("person_id", "publication_year"),
  all.x = TRUE
)

# indicator that in top5
dt_paper_authors[, top5_repec := fifelse(!is.na(avg_score),
                                       1, 0)]
dt_paper_authors[is.na(person_id), top5_repec := NA]



# indicators by paper (aggregated)

dt_paper_authors[, team_size := uniqueN(person_id), by = paper_id]
dt_paper_authors[, team_sh_female := mean(d_female, na.rm = T), by = paper_id]
dt_paper_authors[, team_any_female := max(d_female, na.rm = T), by = paper_id]
dt_paper_authors[, team_any_star := max(top5_repec, na.rm = T), by = paper_id]




# ----------------------------------------------------
# SAVE
# ----------------------------------------------------

  
save(dt_person_level, file = file.path(path_data, "gen/dt_person_level.RData"))
save(dt_paper_level, file = file.path(path_data, "gen/dt_paper_level.RData"))
save(dt_paper_authors, file = file.path(path_data, "gen/dt_paper_authors.RData"))

uniqueN(dt_people$person_id)
uniqueN(dt_paper_level$person_id) # count mentioned
uniqueN(dt_paper_authors$person_id) # count authors
uniqueN(intersect(dt_paper_level$person_id, dt_paper_authors$person_id)) # count appear in both

