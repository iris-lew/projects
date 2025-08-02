load("~/HCMST 2017 to 2022 small public version 2.2.rdata")
d <- `HCMST small public version 2.2`
colnames(d) <- tolower(colnames(d))

# Packages and libraries

# Package names
packages <- c("tidyverse", "networkD3","plotly","devtools","ggalluvial","data.table")
# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# devtools::install_github("davidsjoberg/ggsankey")

# surveyed in w2
d$w2_surveyed

# Not Surveyed
d$w2_status <- "Missed"

d$w2_status <- dplyr::case_when(
  d$w2_surveyed==0 ~ "Not Surveyed",
  
  # Married
  d$w2_section==1 ~ "Still Married",
  d$w1_partnership_status==1 & d$w2_relationship_end==1  ~ "Married -> Divorced", # this should be 82
  d$w1_partnership_status==1 & d$w2_relationship_end==2  ~ "Married -> Separation or Other Breakup", # 
  d$w1_partnership_status==1 & d$w2_relationship_end==3 ~ "Married -> Partner Died",
  # Unmarried Partners
  d$w1_partnership_status==2 & d$w2_section==3 & d$w2_partner_type==1 ~ "Unmarried Partners -> Married", # unmarried, still with unmarried partner but reported marriage.
  d$w1_partnership_status==2 & d$w2_section==3 & d$w2_partner_type==2 ~ "Unmarried Partners -> Unmarried Partners", # unmarried, still with unmarried partner but reported unmarried partnership.
  d$w1_partnership_status==2 & d$w2_relationship_end==1 ~ "Unmarried Partners -> Divorced",
  d$w1_partnership_status==2 & d$w2_relationship_end==2 ~ "Unmarried Partners -> Separation or Other Breakup",
  d$w1_partnership_status==2 & d$w2_relationship_end==3 ~ "Unmarried Partners -> Partner Died",
  # Single but had partner in past
  d$w1_partnership_status==3 & d$w2_relationship_end==1 ~ "Single (past) -> Divorced",
  d$w1_partnership_status==3 & d$w2_relationship_end==2 ~ "Single (past) -> Separation or Other Breakup",
  d$w1_partnership_status==3 & d$w2_relationship_end==3 ~ "Single (past) -> Partner Died",
  d$w1_partnership_status==3 & d$w2_partner_type==1 ~ "Single (past) -> Married",
  d$w1_partnership_status==3 & d$w2_partner_type==2 ~ "Single (past) -> Unmarried Partners",
  d$w1_partnership_status==3 & d$w2_partner_type==3 ~ "Single (past) -> Single",
  # Never had a partner,
  d$w1_partnership_status==4 & d$w2_partner_type==1 ~ "Never in a relationship -> Married",
  d$w1_partnership_status==4 & d$w2_partner_type==2 ~ "Never in a relationship -> Unmarried Partners",
  d$w1_partnership_status==4 & d$w2_partner_type==4 ~ "Never in a relationship",
  
  # Mis-coded data: w1 had partner, w2_section said no longer married/partner  w/ w1
  # but w2_relationship end says no report of breakup.
  (d$w1_partnership_status==1 & d$w2_section==2 & d$w2_relationship_end==0) |
    (d$w1_partnership_status==2 & d$w2_section==4 & d$w2_relationship_end==0) ~ "Miscoded data",
  
  TRUE ~ d$w2_status
)

# nrow(d[d$w2_section==3 & d$w2_partner_type==1,])
table(d$w2_partner_type)
table(d$w1_partnership_status, d$w2_relationship_end)

d$w2_status <- factor(d$w2_status, levels=c(
  "Still Married",
  "Married -> Divorced",
  "Married -> Separation or Other Breakup",
  "Married -> Partner Died",
  "Unmarried Partners -> Married",
  "Unmarried Partners -> Unmarried Partners",
  "Unmarried Partners -> Divorced",
  "Unmarried Partners -> Separation or Other Breakup",
  "Unmarried Partners -> Partner Died",
  "Single (past) -> Divorced",
  "Single (past) -> Separation or Other Breakup",
  "Single (past) -> Partner Died",
  "Single (past) -> Married",
  "Single (past) -> Unmarried Partners",
  "Single (past) -> Single",
  "Never in a relationship -> Married",
  "Never in a relationship -> Unmarried Partners",
  "Never in a relationship",
  "Not Surveyed",
  "Miscoded data",
  "Missed"
))
table(d$w2_status)

# View(d[d$w2_status=="Missed",][c("w1_partnership_status","w2_section","w2_relationship_end")])
# no idea what new partner status is.
# View(d[d$w2_section==5,][c("w1_partnership_status","w2_section","w2_relationship_end")])

table(d$w2_relationship_end)
# table(d$w1_partnership_status, d$w2_section)
# table(d$w1_partnership_status, d$w2_xpartner_type)
# table(d$w2_partner_type, d$w2_xpartner_type)
# table(d$w1_partnership_status, d$w2_partner_type)
table(d$w2_section)
table(d$w2_status)

levels(d$w2_status)
divorced <- levels(d$w2_status)[grep("-> divorced",tolower(levels(d$w2_status)))]
breakup <- levels(d$w2_status)[grep("-> separation",tolower(levels(d$w2_status)))]
married1 <- levels(d$w2_status)[grep("-> married",tolower(levels(d$w2_status)))]
married2 <- levels(d$w2_status)[grep("still married",tolower(levels(d$w2_status)))]
married <- c(married1,married2)
died <- levels(d$w2_status)[grep("died",tolower(levels(d$w2_status)))]
unmarriedpartners <- levels(d$w2_status)[grep("-> unmarried",tolower(levels(d$w2_status)))]
singlepast <- levels(d$w2_status)[grep("-> single",tolower(levels(d$w2_status)))]
singlenever <- "Never in a relationship"
single <- c(singlepast, singlenever)
notsurveyed <- "Not Surveyed"
miscoded <- "Miscoded data"

unique(c(divorced,breakup, married, died, unmarriedpartners, single,notsurveyed,miscoded))
c(divorced,breakup, married, died, unmarriedpartners, single,notsurveyed,miscoded)

d$w2_status_cat <- dplyr::case_when(
  d$w2_status %in% divorced ~ "divorced",
  d$w2_status %in% breakup ~ "breakup",
  d$w2_status %in% married ~ "married",
  d$w2_status %in% died ~ "died",
  d$w2_status %in% unmarriedpartners ~ "unmarriedpartners",
  d$w2_status %in% single ~ "single",
  d$w2_status %in% notsurveyed ~ "notsurveyed",
  d$w2_status %in% miscoded ~ "miscoded",
)

table(d$w2_status, d$w2_status_cat)

# Sankey
# q24 columns -> w1_partnership_status -> w2_status_cat
q24_cols <- colnames(d)[grep("w1_q24_",colnames(d))][c(17:37, 40:44)]

test <- d[c("w1_partnership_status","w2_status_cat")]

links_mine <- test %>% 
  count(w1_partnership_status,w2_status_cat)

links_mine$w1_partnership_status <- dplyr::case_when(
  links_mine$w1_partnership_status==1 ~ "Married",
  links_mine$w1_partnership_status==2 ~ "Unmarried Partners",
  links_mine$w1_partnership_status==3 ~ "Single (past)",
  links_mine$w1_partnership_status==4 ~ "Single",
  )

ordered_status <- factor(links_mine$w2_status_cat,levels=c(
  "married",
  "divorced",
  "unmarriedpartners",
  "breakup",
  "died",
  "single",
  "miscoded",
  "notsurveyed"
))
levels(ordered_status)

nodes_mine <- data.frame(
  name=c(links_mine$w1_partnership_status,levels(ordered_status)) %>% unique()
)
nodes_mine

links_mine$IDsource <- match(links_mine$w1_partnership_status, nodes_mine$name)-1 
links_mine$IDtarget <- match(links_mine$w2_status_cat, nodes_mine$name)-1
links_mine


p_mine <- sankeyNetwork(Links = links_mine, Nodes = nodes_mine,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "n", NodeID = "name", 
                   sinksRight=FALSE, fontSize = 36, nodeWidth = 50)
p_mine
nodes_mine
links_mine


# example
# Make Sankey diagram
q24_cols <- colnames(d)[grep("w1_q24_",colnames(d))][c(17:37, 40:44)]

test <- d[c("w1_partnership_status","w2_status_cat")]

links_mine <- test %>% 
  count(w1_partnership_status,w2_status_cat)

links_mine$w1_partnership_status <- dplyr::case_when(
  links_mine$w1_partnership_status==1 ~ "Married",
  links_mine$w1_partnership_status==2 ~ "Unmarried Partners",
  links_mine$w1_partnership_status==3 ~ "Single (past)",
  links_mine$w1_partnership_status==4 ~ "Single",
)

ordered_status <- factor(links_mine$w2_status_cat,levels=c(
  "married",
  "divorced",
  "unmarriedpartners",
  "breakup",
  "died",
  "single",
  "miscoded",
  "notsurveyed"
))
levels(ordered_status)

nodes_mine <- data.frame(
  name=c(links_mine$w1_partnership_status,levels(ordered_status)) %>% unique()
)
nodes_mine

links_mine$IDsource <- match(links_mine$w1_partnership_status, nodes_mine$name)-1 
links_mine$IDtarget <- match(links_mine$w2_status_cat, nodes_mine$name)-1
links_mine


p_mine <- sankeyNetwork(Links = links_mine, Nodes = nodes_mine,
                        Source = "IDsource", Target = "IDtarget",
                        Value = "n", NodeID = "name", 
                        sinksRight=FALSE, fontSize = 36, nodeWidth = 50)
p_mine
nodes_mine
links_mine

plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(pad = 15,
              thickness = 20,
              line = list(color = "black", width = 0.5),
              label = nodes_mine$name),
  link = list(source = links_mine$IDsource,
              target = links_mine$IDtarget,
              value = links_mine$n),
  textfont = list(size = 10),
  width = 720,
  height = 480
) %>%
  layout(title = "2017 Partnership Status in 2020",
         font = list(size = 20),
         margin = list(t = 50, l = 10, r = 10, b = -10),
         annotations = list(
           list(x = 0, y = -.1, text = "2017", showarrow=FALSE, font = list(size = 16)),
           list(x = 1, y = -.1, text = "2020", showarrow=FALSE, font = list(size = 16))
         )
         ) 

## sankey actually use
q24_cols <- colnames(d)[grep("w1_q24_",colnames(d))][c(17:37, 40:44)]

listed <- list()
listindex <- 1
q24_methods <- c()
for (i in q24_cols){
  temp <- d[c(i,"w1_partnership_status")]
  temp2 <- temp %>%
    count(eval(as.name(i)),w1_partnership_status)
  colnames(temp2) <- c(i,"w1_partnership_status","n")
  temp3 <- subset(temp2,eval(as.name(i))==1)
  temp3[1] <- colnames(temp3)[1]
  colnames(temp3) <- c("source","target","n")
  temp3$source <- gsub("w1_q24_","",temp3$source)
  q24_methods <- c(q24_methods,temp3$source[1])
  listed[[listindex]] <- temp3
  listindex <- listindex+1
}
listed
q24_list <- rbindlist(listed)

q24_list$IDsource <- match(q24_list$source, q24_methods)-1 

q24_list$IDtarget <- q24_list$target+max(q24_list$IDsource)
q24_list$target <- case_when(
  q24_list$target==1 ~ "Married",
  q24_list$target==2 ~ "Unmarried Partners",
  q24_list$target==3 ~ "Single (past)"
)


nodes_2 <- c(q24_list$source,q24_list$target) %>% unique()

w2_stayed_together <- d[c("w1_partnership_status","w2_status_cat")]
w2_stayed_together_n <- w2_stayed_together %>% 
  count(w1_partnership_status,w2_status_cat)
colnames(w2_stayed_together_n) <- c("source","target","n")
w2_stayed_together_n$IDsource <- w2_stayed_together_n$source+max(q24_list$IDsource)

w2_stayed_together_n$source <- dplyr::case_when(
  w2_stayed_together_n$source==1 ~ "Married",
  w2_stayed_together_n$source==2 ~ "Unmarried Partners",
  w2_stayed_together_n$source==3 ~ "Single (past)",
  w2_stayed_together_n$source==4 ~ "Single",
)
unique(w2_stayed_together_n$target)

w2_stayed_together_n$IDtarget <- dplyr::case_when(
  w2_stayed_together_n$target=="breakup" ~ max(w2_stayed_together_n$IDsource)+1,
  w2_stayed_together_n$target=="died" ~ max(w2_stayed_together_n$IDsource)+2,
  w2_stayed_together_n$target=="divorced" ~ max(w2_stayed_together_n$IDsource)+3,
  w2_stayed_together_n$target=="married" ~ max(w2_stayed_together_n$IDsource)+4,
  w2_stayed_together_n$target=="miscoded" ~ max(w2_stayed_together_n$IDsource)+5,
  w2_stayed_together_n$target=="notsurveyed" ~ max(w2_stayed_together_n$IDsource)+6,
  w2_stayed_together_n$target=="unmarriedpartners" ~ max(w2_stayed_together_n$IDsource)+7,
  w2_stayed_together_n$target=="single" ~ max(w2_stayed_together_n$IDsource)+8
)

w2_stayed_together_n$target <- dplyr::case_when(
  w2_stayed_together_n$target=="breakup" ~ "Breakup",
  w2_stayed_together_n$target=="died" ~ "Died",
  w2_stayed_together_n$target=="divorced" ~ "Divorced",
  w2_stayed_together_n$target=="married" ~ "Married",
  w2_stayed_together_n$target=="miscoded" ~ "Miscoded",
  w2_stayed_together_n$target=="notsurveyed" ~ "Not Surveyed",
  w2_stayed_together_n$target=="unmarriedpartners" ~ "Unmarried Partners",
  w2_stayed_together_n$target=="single" ~ "Single"
)
head(q24_list)
head(w2_stayed_together_n)

combined <- rbind(q24_list,w2_stayed_together_n)
unique_w2_partnerstatus <- unique(w2_stayed_together_n$source)
unique_w2_partnerstatus_target <- unique(w2_stayed_together_n$target)

newnodes <- c(nodes_2,"Single",unique_w2_partnerstatus_target)

nodes_3 <- data.frame(
  name=newnodes
)
nodes_3

plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(pad = 15,
              thickness = 20,
              line = list(color = "black", width = 0.5),
              label = nodes_3$name),
  link = list(source = combined$IDsource,
              target = combined$IDtarget,
              value = combined$n),
  textfont = list(size = 10),
  width = 720,
  height = 480
) %>%
  layout(title = "2017 Partnership Status in 2020",
         font = list(size = 20),
         margin = list(t = 50, l = 10, r = 10, b = -10),
         annotations = list(
           list(x = 0, y = -.1, text = "2017", showarrow=FALSE, font = list(size = 16)),
           list(x = 1, y = -.1, text = "2020", showarrow=FALSE, font = list(size = 16))
         )
  ) 

# w3

d$w3_surveyed
d$w3_partner_type
d$w3_new_relationship
d$w3_relationship_end_combo

d$w2_status_cat %>% unique()
d$w3_status_cat <- "Missed"
d$w3_status_cat <- dplyr::case_when(
  d$w3_surveyed==0 ~ "Not Surveyed",
  d$w3_partner_type==1 ~ "Married",
  d$w3_relationship_end_combo==1 ~ "Divorced",
  d$w3_relationship_end_combo==2 ~ "Breakup",
  d$w3_relationship_end_combo==3 ~ "Died",
  d$w3_partner_type==2 & (d$w3_relationship_end_combo==1 | is.na(d$w3_relationship_end_combo)) ~ "Unmarried Partners",
  d$w3_partner_type==3 ~ "Single",
  TRUE ~ d$w3_status_cat
)

table(d$w3_relationship_end_combo, d$w3_partner_type)
table(d$w3_status_cat)

w3_stayed_together <- d[c("w2_status_cat","w3_status_cat")]

w3_stayed_together_n <- w3_stayed_together %>% 
  count(w2_status_cat,w3_status_cat)
colnames(w3_stayed_together_n) <- c("source","target","n")

w2_max <- max(max(combined$IDtarget),max(combined$IDsource))
w3_stayed_together_n$IDtarget <- dplyr::case_when(
  w3_stayed_together_n$target=="Not Surveyed"~1+w2_max,
  w3_stayed_together_n$target=="Married"~2+w2_max,
  w3_stayed_together_n$target=="Divorced"~3+w2_max,
  w3_stayed_together_n$target=="Breakup"~4+w2_max,
  w3_stayed_together_n$target=="Died"~5+w2_max,
  w3_stayed_together_n$target=="Unmarried Partners"~6+w2_max,
  w3_stayed_together_n$target=="Single"~7+w2_max,
  w3_stayed_together_n$target=="Missed"~8+w2_max,
)

w3_stayed_together_n$source <- dplyr::case_when(
  w3_stayed_together_n$source=="breakup"~"Breakup",
  w3_stayed_together_n$source=="died"~"Died",
  w3_stayed_together_n$source=="divorced"~"Divorced",
  w3_stayed_together_n$source=="married"~"Married",
  w3_stayed_together_n$source=="miscoded"~"Miscoded",
  w3_stayed_together_n$source=="notsurveyed"~"Not Surveyed",
  w3_stayed_together_n$source=="single"~"Single",
  w3_stayed_together_n$source=="unmarriedpartners"~"Unmarried Partners",
)

w3_stayed_together_n$IDsource <- dplyr::case_when(
  w3_stayed_together_n$source=="Breakup"~30,
  w3_stayed_together_n$source=="Died"~31,
  w3_stayed_together_n$source=="Divorced"~32,
  w3_stayed_together_n$source=="Married"~33,
  w3_stayed_together_n$source=="Miscoded"~34,
  w3_stayed_together_n$source=="Not Surveyed"~35,
  w3_stayed_together_n$source=="Single"~37,
  w3_stayed_together_n$source=="Unmarried Partners"~36,
)

combined <- rbind(q24_list,w2_stayed_together_n,w3_stayed_together_n)
w3_nodes <- c("Not Surveyed","Married","Divorced","Breakup","Died","Unmarried Partners", "Single","Missed")

newnodes <- c(nodes_2,"Single",unique_w2_partnerstatus_target,w3_nodes)
# checking purposes for newnodes
length(newnodes)
max(combined$IDsource)
max(combined$IDtarget)

nodes_4 <- data.frame(
  name=newnodes
)
nodes_4

plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(pad = 15,
              thickness = 20,
              line = list(color = "black", width = 0.5),
              label = nodes_4$name),
  link = list(source = combined$IDsource,
              target = combined$IDtarget,
              value = combined$n),
  textfont = list(size = 10),
  width = 720,
  height = 480
) %>%
  layout(title = "How US Couples Meet and Whether They Stay Together",
         font = list(size = 20),
         margin = list(t = 50, l = 10, r = 10, b = -10),
         annotations = list(
           list(x = 0, y = -.1, text = "How Met", showarrow=FALSE, font = list(size = 16)),
           list(x = .3, y = -.1, text = "2017", showarrow=FALSE, font = list(size = 16)),
           list(x = .7, y = -.1, text = "2020", showarrow=FALSE, font = list(size = 16)),
           list(x = 1, y = -.1, text = "2022", showarrow=FALSE, font = list(size = 16))
         )
  ) 



### past
ordered_status <- factor(links_mine$w2_status_cat,levels=c(
  "married",
  "divorced",
  "unmarriedpartners",
  "breakup",
  "died",
  "single",
  "miscoded",
  "notsurveyed"
))
levels(ordered_status)

nodes_mine <- data.frame(
  name=c(links_mine$w1_partnership_status,levels(ordered_status)) %>% unique()
)
nodes_mine

links_mine$IDsource <- match(links_mine$w1_partnership_status, nodes_mine$name)-1 
links_mine$IDtarget <- match(links_mine$w2_status_cat, nodes_mine$name)-1
links_mine


plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(pad = 15,
              thickness = 20,
              line = list(color = "black", width = 0.5),
              label = nodes_mine$name),
  link = list(source = links_mine$IDsource,
              target = links_mine$IDtarget,
              value = links_mine$n),
  textfont = list(size = 10),
  width = 720,
  height = 480
) %>%
  layout(title = "2017 Partnership Status in 2020",
         font = list(size = 20),
         margin = list(t = 50, l = 10, r = 10, b = -10),
         annotations = list(
           list(x = 0, y = -.1, text = "2017", showarrow=FALSE, font = list(size = 16)),
           list(x = 1, y = -.1, text = "2020", showarrow=FALSE, font = list(size = 16))
         )
  ) 




# # example starts
# # create a dataframe with 100 participants
# df <- data.frame(id = 1:100)
# 
# # randomly assign gender and personality traits
# df$gender <- sample(c("Male", "Female"), 100, replace = TRUE)
# df$field <- sample(c("Science", "Art", "Business", "Law"), 100, replace = TRUE)
# 
# # assign personality traits based on field of study
# df$personality <- ifelse(df$field %in% c("Science", "Art"),
#                          sample(c("Introverted", "Introverted", 
#                                   "Introverted", "Extroverted"), 100, 
#                                 replace = TRUE),
#                          ifelse(df$field == "Business",
#                                 sample(c("Introverted", "Extroverted", 
#                                          "Extroverted"), 100, replace = TRUE),
#                                 sample(c("Introverted", "Extroverted"),
#                                        100, replace = TRUE)))
# 
# # use ifelse() to set gender proportions based on field of study
# df$gender <- ifelse(df$field %in% c("Science", "Business"),
#                     sample(c("Male", "Female"), 100, replace = TRUE,
#                            prob = c(0.611, 0.389)),
#                     ifelse(df$field == "Art",
#                            sample(c("Male", "Female"), 100, 
#                                   replace = TRUE, prob = c(0.388, 0.612)),
#                            sample(c("Male", "Female"), 100,
#                                   replace = TRUE, prob = c(0.545, 0.455))))
# 
# # example
# library(ggplot2)
# library(ggsankey)
# df_skey <- df %>%
#   make_long(personality, field, gender)
# # Creating a Sankey diagram:
# skeypl <- ggplot(df_skey, aes(x = x
#                               , next_x = next_x
#                               , node = node
#                               , next_node = next_node
#                               , fill = factor(node)
#                               , label = node)) +
#   geom_sankey(flow.alpha = 0.5
#               ,node.color = "black"
#               ,show.legend = FALSE)
# 
# # example
# library(ggplot2)
# library(ggalluvial)
# 
# frequencies <- df %>% 
#   count(personality, field, gender) %>% 
#   arrange(field, desc(n))
# 
# # Create the Sankey plot:
# skeypl2 <- ggplot(data = frequencies,
#                   aes(axis1 = personality,   # First variable on the X-axis
#                       axis2 = field, # Second variable on the X-axis
#                       axis3 = gender,   # Third variable on the X-axis
#                       y = n)) +
#   geom_alluvium(aes(fill = gender)) +
#   geom_stratum() +
#   geom_text(stat = "stratum",
#             aes(label = after_stat(stratum))) +
#   scale_fill_viridis_d() +
#   theme_void()
# skeypl2
# 
# # example
# 
# # create a table of frequencies
# freq_table <- df %>% group_by(personality, field, gender) %>% 
#   summarise(n = n())
# 
# # create a nodes data frame
# nodes <- data.frame(name = unique(c(as.character(freq_table$personality),
#                                     as.character(freq_table$field),
#                                     as.character(freq_table$gender))))
# 
# # create links dataframe
# links <- data.frame(source = match(freq_table$personality, nodes$name) - 1,
#                     target = match(freq_table$field, nodes$name) - 1,
#                     value = freq_table$n,
#                     stringsAsFactors = FALSE)
# 
# links <- rbind(links,
#                data.frame(source = match(freq_table$field, nodes$name) - 1,
#                           target = match(freq_table$gender, nodes$name) - 1,
#                           value = freq_table$n,
#                           stringsAsFactors = FALSE))
# 
# 
# sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
#               Target = "target", Value = "value", NodeID = "name",
#               sinksRight = FALSE)
# 
# 
# # example
# nodes <- data.frame(name = unique(c(as.character(freq_table$personality),
#                                     as.character(freq_table$field),
#                                     as.character(freq_table$gender))))
# 
# links <- data.frame(source = match(freq_table$personality, nodes$name) - 1,
#                     target = match(freq_table$field, nodes$name) - 1,
#                     value = freq_table$n,
#                     stringsAsFactors = FALSE)
# 
# links <- rbind(links,
#                data.frame(source = match(freq_table$field, nodes$name) - 1,
#                           target = match(freq_table$gender, nodes$name) - 1,
#                           value = freq_table$n,
#                           stringsAsFactors = FALSE))
# # Make Sankey diagram
# plot_ly(
#   type = "sankey",
#   orientation = "h",
#   node = list(pad = 15,
#               thickness = 20,
#               line = list(color = "black", width = 0.5),
#               label = nodes$name),
#   link = list(source = links$source,
#               target = links$target,
#               value = links$value),
#   textfont = list(size = 10),
#   width = 720,
#   height = 480
# ) %>%
#   layout(title = "Sankey Diagram: Personality, Field, and Gender",
#          font = list(size = 14),
#          margin = list(t = 40, l = 10, r = 10, b = 10))
# 

# save the widget
# library(htmlwidgets)
# saveWidget(p, file=paste0( getwd(), "/HtmlWidget/sankeyBasic2.html"))

# https://www.marsja.se/create-a-sankey-plot-in-r-ggplot2-plotly/
# https://www.rigordatasolutions.com/post/how-to-create-sankey-diagram-in-r-with-networkd3

