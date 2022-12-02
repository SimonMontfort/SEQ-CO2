######################################################
#### Survey on the Swiss CO2 law
### Simon Montfort, Dr. Lukas Fesenfeld, Prof. Dr. Isabelle Stadelmann, Prof. Dr. Karin Ingold
## Script compiled by Simon Montfort
# 16.12.2022
######################################################
# > R.version
# platform       x86_64-apple-darwin17.0     
# arch           x86_64                      
# os             darwin17.0                  
# system         x86_64, darwin17.0          
# status                                     
# major          4                           
# minor          1.1                         
# year           2021                        
# month          08                          
# day            10                          
# svn rev        80725                       
# language       R                           
# version.string R version 4.1.1 (2021-08-10)
# nickname       Kick Things   

rm(list = ls())

# packages
library(cjoint)
library(gdata)
library(readxl)
library(plyr) 
library(dplyr)
library(ggthemes)
library(ggtext)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(lubridate)
library(zoo)
library(jtools)
library(readr)
library(sandwich)
library(sparsereg)
library(tibble)

setwd("/Users/simon/Documents/repo/SEQ-CO2")
# load data
vote <- read_xlsx("DatenUmfrage/VoteJune21_June 17, 2021_14.06.xlsx", sheet = 1)

## initial cleaning
# first row with rownames
# vote <- vote[-1,]
load("DatenUmfrage/dat_with_stations_3.RData")
vote <- dat
# subset to full completes
vote <- vote[vote$Status == "IP Address",]
vote <- vote[vote$QID6 == "Ja",] # only respondents who are eligible to vote
# subset missing values for conjoint (using the conjoint attribute values shown to the respondent)
vote <- vote[rowSums(is.na(vote[,c("QID430_cjp1",	"QID430_cjp2",	"QID431_cjp1",	"QID431_cjp2", "QID392_cjp1",	"QID392_cjp2", "QID393_cjp1", "QID393_cjp2",
                                   "QID98_1", "QID98_2", "QID270_1", "QID270_2", "QID282_1",	"QID282_2", "QID281_1", "QID281_2",
                                   "QID389_1", "QID389_2", "QID415_1", "QID415_2", "QID412_1",	"QID412_2", "QID414_1", "QID414_2")])) == 0,]
# inspect good completes
unique(vote$gc) # all 1 or NA
vote <- vote[!is.na(vote$gc),]
# assign id
vote$id <- 1:nrow(vote)
# to df
vote <- as.data.frame(vote)
# # convert to date
# vote$StartDate <- as_datetime(as.numeric(vote$StartDate)* 3600*24, origin='1900-01-01')
# # subset to all those respondents after the pretest, this does nothing because 
# vote <- vote[vote$StartDate > as.Date("2021-06-05", "%Y-%m-%d"),]
table(vote$QID14, useNA = "always")
table(vote$QID82_1, useNA = "always")
table(vote$QID618_1, useNA = "always")
table(vote$QID565, useNA = "always")
table(vote$QID16, useNA = "always")
table(vote$QID19, useNA = "always")
table(vote$QID22, useNA = "always") # plz question
sum(is.na(vote$QID22))
table(is.na(vote$no_of_stations_ev))
nrow(vote)

## info on survey used in the text:
# start date
min(openxlsx::convertToDateTime(vote$EndDate))
# end date
max(openxlsx::convertToDateTime(vote$EndDate))
# median time
summary(as.numeric(vote$`Duration (in seconds)`))[3]/60
# # speeding
# gsub("0.", "", )
# library(openxlsx)
# vote$EndDate <- openxlsx::convertToDateTime(vote$EndDate)
# vote$StartDate <- openxlsx::convertToDateTime(vote$StartDate)
# vote$`Duration (in seconds)` <- as.numeric(vote$`Duration (in seconds)`)/60
# med_below40 <- summary(vote$`Duration (in seconds)`)[3]*.4
# # vote <- vote[vote$`Duration (in seconds)` > med_below40,]
# 
# med_below40_prior_benefit <- summary(as.numeric(vote$`QID566_Page Submit`))[3]*.4
# vote <- vote[vote$`QID566_Page Submit` < med_below40_prior_benefit,]
######################################################
# functions and custom theme
######################################################
# function to transform the data from qualtrics to the formate required by the cjoint package
split_cjp <- function(vote, vals, support, activism, choice){
  # initialise list
  shown_conj_values <- list()
  # loop through all values
  for (i in 1:length(vals)){
    # split strings
    shown_conj_values[[vals[i]]] <- strsplit(vote[ , as.character(vals[i])], ",")
    # bind the list together 
    shown_conj_values[[vals[i]]] <- do.call(rbind, shown_conj_values[[vals[i]]])
    # replace col labels
    colnames(shown_conj_values[[vals[i]]]) <- c("attrib1_lab", "attrib2_lab", "attrib3_lab", "attrib4_lab", "attrib5_lab", "attrib6_lab")
    # bind the support variable to it
    shown_conj_values[[vals[i]]] <- cbind(shown_conj_values[[vals[i]]], vote[, as.character(support[i])])
    # rename cols
    colnames(shown_conj_values[[vals[i]]])[length(colnames(shown_conj_values[[vals[i]]]))] <- "rate"
    # bind the activism variable to it
    shown_conj_values[[vals[i]]] <- cbind(shown_conj_values[[vals[i]]], vote[, as.character(activism[i])])
    # rename cols
    colnames(shown_conj_values[[vals[i]]])[length(colnames(shown_conj_values[[vals[i]]]))] <- "rate_activism"
    # choice outcome only appears 4 times, rate outcomes appear 8 times, therefore change the counter
    j <- if(i %in% 1:2) 1 else if (i %in% 3:4) 2 else if (i %in% 5:6) 3 else if (i %in% 7:8) 4
    # bind the choice variable to it
    shown_conj_values[[vals[i]]] <- cbind(shown_conj_values[[vals[i]]], vote[, as.character(choice[j])])
    # rename cols
    colnames(shown_conj_values[[vals[i]]])[length(colnames(shown_conj_values[[vals[i]]]))] <- "choice"
    # # bind vars to keep to it
    shown_conj_values[[vals[i]]] <- cbind(shown_conj_values[[vals[i]]], vote)
  }
  # bind elements in list of conjoint features of package 1
  df_cjp_1 <- do.call(rbind, shown_conj_values[grepl("_cjp1|id", names(shown_conj_values))])
  
  df_cjp_1[, "choice_id"] <- 1
  # bind elements in list of conjoint features of package 2
  df_cjp_2 <- do.call(rbind, shown_conj_values[grepl("_cjp2|id", names(shown_conj_values))])
  
  df_cjp_2[, "choice_id"] <- 2
  # bind these together
  df_cjp <- as.data.frame(rbind(df_cjp_1, df_cjp_2))
  # write this inot the results object
  df_cjp[grepl(paste(qid_values[1], qid_values[2], sep= "|"), rownames(df_cjp)) ,"round"] <- 1
  df_cjp[grepl(paste(qid_values[3], qid_values[4], sep= "|"), rownames(df_cjp)) ,"round"] <- 2
  df_cjp[grepl(paste(qid_values[5], qid_values[6], sep= "|"), rownames(df_cjp)) ,"round"] <- 3
  df_cjp[grepl(paste(qid_values[7], qid_values[8], sep= "|"), rownames(df_cjp)) ,"round"] <- 4
  # order
  df_cjp <- df_cjp[order(df_cjp$id, df_cjp$round),]
  # add choice_id
  # df_cjp$choice_id <- rep(1:2, nrow(df_cjp)/2)
  # recode choice depending on choice id
  df_cjp$choice[df_cjp$choice_id == 1 & df_cjp$choice == "Vorschlag A"] <- 1
  df_cjp$choice[df_cjp$choice_id == 2 & df_cjp$choice == "Vorschlag A"] <- 0
  df_cjp$choice[df_cjp$choice_id == 1 & df_cjp$choice == "Vorschlag B"] <- 0
  df_cjp$choice[df_cjp$choice_id == 2 & df_cjp$choice == "Vorschlag B"] <- 1
  df_cjp$choice <- as.numeric(df_cjp$choice)
  # output object of the function
  df_cjp
}

qid_values <- c("QID430_cjp1",	"QID430_cjp2",	"QID431_cjp1",	"QID431_cjp2", "QID392_cjp1",	"QID392_cjp2", "QID393_cjp1", "QID393_cjp2") # Werte
support <- c("QID98_1", "QID98_2", "QID270_1", "QID270_2", "QID282_1",	"QID282_2", "QID281_1", "QID281_2") # Werte
activism <- c("QID389_1", "QID389_2", "QID415_1", "QID415_2", "QID412_1",	"QID412_2", "QID414_1", "QID414_2") # Werte
choice <- c("QID96", "QID269", "QID277", "QID279") # Werte
vals <- qid_values

# transform the data
# dat <- split_cjp(vote, qid_values, support, activism, choice)
# dat %>% group_by(id, round) %>% summarise(choice = sum(choice)) %>% ungroup() %>% distinct(choice) # should be one
# ## to double check that function works correctly -- values are the same. All good
# # for DV rate
# vote %>% arrange(id) %>% select(id, "QID98_1", "QID98_2", "QID270_1", "QID270_2", "QID282_1", "QID282_2", "QID281_1", "QID281_2", "QID96", "QID269", "QID277", "QID279") %>% head(4)
# dat %>% arrange(id, round) %>% select(id, round, choice_id, rate, activism, choice) %>% head(32)
# # for DV support
# vote %>% arrange(id) %>% select(id, "QID389_1", "QID389_2", "QID415_1", "QID415_2", "QID412_1",	"QID412_2", "QID414_1", "QID414_2") %>% head(4)
# dat %>% arrange(id, round) %>% select(id, round, rate_activism) %>% head(32)
# # for covariate
# vote %>% arrange(id) %>% select(id, "QID519") %>% head(4)
# dat %>% arrange(id, round) %>% select(id, round, QID519) %>% head(32)

# to extract results object from an amce object with an option to retrieve interaction effects supplied through respondent.varying
results <- function(rate, option) {
  # initialise df to store results
  value <- data.frame(matrix(nrow = length(rate$user.levels), ncol = 2))
  # name cols
  colnames(value) <- c("lab", "value")
  # sotre value shown to respondent
  value$value <- unlist(rate$user.levels)
  # transform to factor
  value$value <- factor(value$value, levels = rev(unique(value$value)))
  # give it a label
  value$lab <- names(unlist(rate$user.levels))
  # retrieve results
  s <- base::summary(rate)
  # how many interactions? 1 = no interactions, 3 = binary, 4 = continous
  if (option == 4){
    # create list of results for each subgroup
    l <- list(s[[1]], s[[3]], s[[4]], s[[5]])
  } else if (option  == 3){
    # create list of results for each subgroup
    l <- list(s[[1]], s[[3]], s[[4]])
  } else if (option  == 1){
    # create list of results for baseline
    l <- list(s[[1]])
  } else {
    # error if incorrect option is chosen
    stop("musst supply number of marginal effect results: 1 = only baseline, 3 = unconditional and two conditional, 4 unconditional and three conditional")
  }
  # create ids
  l <- lapply(l, function(x){
    # create id for each variable
    x <- transform(x, id = as.numeric(factor(x$Attribute)))
    # to factor
    # x$Level <- factor(x$Level, levels = unique(value[,"value"]))
    # create id1 for each subgroup
    x$id1 <- rep(parent.frame()$i[], nrow(x)) 
    # output
    x
  })
  # bind lists together
  res <- do.call(rbind, l)
  # unique id for each variable within group
  res$id2 <- as.numeric(paste0(res$id, res$id1))
  # create a new row for each variable that constitutes the baseline
  res <- do.call(rbind, lapply(split(res, res$id2), function(x) rbind(NA, within(x, {NA}))))
  # add the corresponding level used as baseline and shown to the respondent
  res$Level[is.na(res$Level)] <- rep(s$baselines_amce$Level, each = option)
  # add Attribute label 
  res$Attribute <- na.locf(res$Attribute, fromLast = T)
  # add id
  res$id <- ifelse(is.na(res$id), lead(res$id), res$id)
  # add id1
  res$id1 <- ifelse(is.na(res$id1), lead(res$id1), res$id1)
  # mark which is the baseline attribute
  res$Level[is.na(res$Estimate)] <- paste("Baseline:", res$Level[is.na(res$Estimate)])
  # reorder levels
  res$Level <- factor(res$Level, levels = unique(rev(res$Level)))
  # output object of the function
  res
}

# create a custom theme for the layout
theme_SM <- function () { 
  theme_classic() +
    theme(
      text = element_text(size=10, colour = "black"),
      axis.text=element_text(size=10, 
                             # angle = 90
      ),
      plot.margin=unit(c(2,1,1,1),"cm"),
      # plot.title = element_text( margin=margin(20,20,100,20)),
      legend.position = "bottom",
      plot.title = element_textbox_simple(vjust=-1),
      axis.ticks.length.x = unit(.2, "cm"),
      axis.ticks.x = element_blank(),
      axis.line = element_line(size = 0.2),
      axis.text.x = element_text(
        # angle = 90, 
        vjust=0.5, 
        # hjust=1
      ),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.background = element_blank(),
      # panel.spacing = unit(3, "lines"),
      strip.background = element_rect(color = "white"),
      strip.text.x = element_text(size = 15),
      strip.placement = "inside"
      # legend.position = "none"
    )
}


######################################################
# descriptive statistics
######################################################

# recode variable on participation
vote$Abgestimmt_ja <- NA
vote$Abgestimmt_ja[vote$QID9 == "Ich werde sicher teilnehmen."] <- 1
vote$Abgestimmt_ja[vote$QID9 == "Ich bin noch nicht ganz sicher, aber wahrscheinlich nehme ich teil."] <- 1
vote$Abgestimmt_ja[vote$QID9 == "Ich stimme normalerweise ab, diesmal aber wahrscheinlich nicht."] <- 0
vote$Abgestimmt_ja[vote$QID9 == "Ich werde nicht teilnehmen."] <- 0
vote$Abgestimmt_ja[vote$QID9 == "Ich habe bereits brieflich abgestimmt."] <- 1

###### Zustimmung Core Beliefs der nicht teilnehmenden
plot_df_1 <- vote %>% 
  # filter(Abgestimmt_ja == 0) %>% 
  dplyr::select(id, QID82_1, QID82_2, QID82_3, QID82_4, QID82_5) %>% 
  reshape2::melt(., id.vars = c("id"), measure.vars = c("QID82_1", "QID82_2", "QID82_3", "QID82_4", "QID82_5")) %>%
  filter(!is.na(value)) %>% 
  mutate(value = factor(value, ordered = T,
                        levels = c("Weder noch", "Eher", "Voll und ganz", "Eher nicht", "Ganz und gar nicht")),
         variable = recode(variable,
                           QID82_1 = "Ökologische \nWirksamkeit",
                           QID82_2 = "Ökonomische \nEffizienz",
                           QID82_3 = "Wettbewerbsfähigkeit \nder Schweizer Wirtschaft",
                           QID82_4 = "Soziale \nGerechtigkeit",
                           QID82_5 = "Transformationspotential")) %>% 
  group_by(variable, value) %>% 
  dplyr::summarise(count = n()) %>% 
  group_by(variable) %>% 
  dplyr::mutate(sum_var = sum(count),
                pct = count/sum_var*100,
                col = value,
                pct = ifelse(value == "Eher nicht", pct*(-1), pct),
                pct = ifelse(value == "Ganz und gar nicht", pct*(-1), pct),
                pct = ifelse(value == "Weder noch", pct/2, pct),
) 

to_bind <- plot_df_1 %>% 
  filter(value == "Weder noch") %>% 
  mutate(pct = ifelse(value == "Weder noch", pct*(-1), pct))

plot_df_1 <- rbind(to_bind, plot_df_1)  

core_belief <- plot_df_1 %>%
  ggplot(.) + 
  geom_bar(stat = "identity", aes(x = variable, y = pct, fill = value), position = position_stack(reverse = TRUE)) +
  theme_SM() +
  scale_fill_manual(name="Antwort Kategorien", values=c("grey", "lightgreen", "darkgreen", "red3", "red4")) +
  geom_text(mapping = aes(
    label = ifelse(value != "Weder noch", paste0(round(abs(pct), 0), "%"), ""), 
    x = variable, 
    y = pct),
    color = "black", cex = 5,
    position=position_stack(vjust=0.5)
  ) +
  ylab("Prozentuale Zustimmung") +   xlab("Bewertungskriterium") + labs(caption = "(je 114 Antworten)") +
  ggtitle("<b>Core Beliefs:</b> Politische Klimaschutzmassnahmen wie das CO2-Gesetz können auf Basis 
          unterschiedlicher Kriterien beurteilt werden. Bitte lesen Sie sich folgende Kriterien durch und geben Sie bitte an,
          wie sehr das zur Abstimmung stehende CO2-Gesetzes aus Ihrer Sicht das jeweilige Kriterium erfüllt oder nicht erfüllt.") +
  coord_flip() + 
  guides(fill=guide_legend(ncol=2))
core_belief
ggsave(core_belief, filename = "Plots/core_belief.pdf", height = 7, width = 10)


###### Sektoren
plot_df_1 <- vote %>% 
  dplyr::select(id, QID618_1, QID618_2, QID618_3, QID618_4, QID618_5, QID618_6) %>% 
  reshape2::melt(., id.vars = c("id"), measure.vars = c("QID618_1", "QID618_2", "QID618_3", "QID618_4", "QID618_5", "QID618_6")) %>% 
  filter(!is.na(value)) %>% 
  mutate(value = factor(value, ordered = T, levels =  c("Weder noch", "Unterstütze voll und ganz", "Unterstütze",  "Lehne ab", "Lehne voll und ganz ab")),
         variable = recode(variable,
                           QID618_1 = "Im Gebäudebereich",
                           QID618_2 = "Im Strassenverkehr",
                           QID618_3 = "Im Ernährungs- und Agrarbereich",
                           QID618_4 = "Im Flugverkehr",
                           QID618_5 = "Im Industriebereich",
                           QID618_6 = "Im Abfallbereich")) %>% 
  group_by(variable, value) %>% 
  dplyr::summarise(count = n()) %>% 
  group_by(variable) %>% 
  dplyr::mutate(sum_var = sum(count),
                pct = count/sum_var*100,
                col = value,
                pct = ifelse(value == "Lehne ab", pct*(-1), pct),
                pct = ifelse(value == "Lehne voll und ganz ab", pct*(-1), pct),
                pct = ifelse(value == "Weder noch", pct/2, pct)
  ) 

to_bind <- plot_df_1 %>% 
  filter(value == "Weder noch") %>% 
  mutate(pct = ifelse(value == "Weder noch", pct*(-1), pct))

plot_df_1 <- rbind(to_bind, plot_df_1)  

sector <- plot_df_1 %>%
  ggplot(.) + 
  geom_bar(stat = "identity", aes(x = variable, y = pct, fill = value), position = position_stack(reverse = TRUE)) +
  theme_SM() +
  scale_fill_manual(name="Antwort Kategorien", values=c("grey", "lightgreen", "darkgreen", "red3", "red4")) +
  geom_text(mapping = aes(
    label = ifelse(value != "Weder noch", paste0(round(abs(pct), 0), "%"), ""), 
    x = variable, 
    y = pct),
    color = "black", cex = 5,
    position=position_stack(vjust=0.5)
  ) +
  ylab("Prozentuale Zustimmung") +   xlab("Ökonomischer Sektor") + labs( caption = "(je 1452 Antworten)") +
  ggtitle("<b>Alle:</b> Wie sehr unterstützen oder lehnen Sie Emissionsreduktionen in den folgenden Bereichen ab? ") +
  coord_flip() +
  guides(fill=guide_legend(ncol=2)) 
sector
ggsave(sector, filename = "Plots/sector_alle.pdf", height=  10, width = 10)

###### Massnahmetyp
plot_df_1 <- vote %>% 
  dplyr::select(id, QID76_1, QID76_2, QID76_3, QID76_4, QID76_5, QID76_6, QID76_7, QID76_8, QID76_9, QID76_10, QID76_11, QID76_12, QID76_13) %>% 
  dplyr::rename(`…die Wettbewerbsfähigkeit der Schweizer Wirtschaft berücksichtigt werden.` = QID76_1, 
                `…die ökologische Wirksamkeit (Erreichung der Pariser Klimaziele) berücksichtigt werden.` = QID76_2, 
                `…die_ökonomische Effizienz (möglichst geringer Aufwand für die Zielerreichung) berücksichtigt werden.` = QID76_3,
                `…die soziale Gerechtigkeit berücksichtigt werden.` = QID76_4,
                `…die Vorreiterrolle der Schweiz in der internationalen Klimapolitik berücksichtigt werden.` = QID76_5,
                `…auf freiwillige Massnahmen (z.B. Informationskampagnen und Selbstverpflichtungen) gesetzt werden.` = QID76_6,
                `…auf Subventionen gesetzt werden.` = QID76_7,
                `...auf CO2-Abgaben gesetzt werden.` = QID76_8,
                `…auf regulatorische Massnahmen (z.B. Emissionsstandards und Verbote) gesetzt werden.` = QID76_9,
                `...auf Massnahmen zur Förderung technologischer Innovationen gesetzt werden.` = QID76_10,
                `...auf Massnahmen zur Förderung von Verhaltensänderungen gesetzt werden.` = QID76_11,
                `…auf eine Kombination verschiedener Massnahmen gesetzt werden.` = QID76_12,
                `…die Kompetenz beim Bund (nicht den Kantonen) liegen.` = QID76_13,
  ) %>% 
  reshape2::melt(., id.vars = c("id"), 
                 measure.vars = c("…die Wettbewerbsfähigkeit der Schweizer Wirtschaft berücksichtigt werden.", 
                                  "…die ökologische Wirksamkeit (Erreichung der Pariser Klimaziele) berücksichtigt werden.", 
                                  "…die_ökonomische Effizienz (möglichst geringer Aufwand für die Zielerreichung) berücksichtigt werden.",
                                  "…die soziale Gerechtigkeit berücksichtigt werden.",
                                  "…die Vorreiterrolle der Schweiz in der internationalen Klimapolitik berücksichtigt werden.",
                                  "…auf freiwillige Massnahmen (z.B. Informationskampagnen und Selbstverpflichtungen) gesetzt werden.",
                                  "…auf Subventionen gesetzt werden.",
                                  "...auf CO2-Abgaben gesetzt werden.",
                                  "…auf regulatorische Massnahmen (z.B. Emissionsstandards und Verbote) gesetzt werden.",
                                  "...auf Massnahmen zur Förderung technologischer Innovationen gesetzt werden.",
                                  "...auf Massnahmen zur Förderung von Verhaltensänderungen gesetzt werden.",
                                  "…auf eine Kombination verschiedener Massnahmen gesetzt werden.",
                                  "…die Kompetenz beim Bund (nicht den Kantonen) liegen."
                 )) %>% 
  filter(!is.na(value)) 

plot_df_1$variable <- as.character(plot_df_1$variable)
plot_df_1$variable[plot_df_1$variable == "…die Wettbewerbsfähigkeit der Schweizer Wirtschaft berücksichtigt werden."] <- "…die Wettbewerbsfähigkeit der Schweizer \n Wirtschaft berücksichtigt werden."
plot_df_1$variable[plot_df_1$variable ==  "…die ökologische Wirksamkeit (Erreichung der Pariser Klimaziele) berücksichtigt werden."] <-  "…die ökologische Wirksamkeit (Erreichung der \n Pariser Klimaziele) berücksichtigt werden."
plot_df_1$variable[plot_df_1$variable == "…die_ökonomische Effizienz (möglichst geringer Aufwand für die Zielerreichung) berücksichtigt werden."] <- "…die ökonomische Effizienz (möglichst geringer \nAufwand für die Zielerreichung) berücksichtigt werden."
plot_df_1$variable[plot_df_1$variable == "…die soziale Gerechtigkeit berücksichtigt werden."] <- "…die soziale Gerechtigkeit \nberücksichtigt werden."
plot_df_1$variable[plot_df_1$variable == "…die Vorreiterrolle der Schweiz in der internationalen Klimapolitik berücksichtigt werden."] <- "…die Vorreiterrolle der Schweiz in der \ninternationalen Klimapolitik berücksichtigt werden."
plot_df_1$variable[plot_df_1$variable == "…auf freiwillige Massnahmen (z.B. Informationskampagnen und Selbstverpflichtungen) gesetzt werden."] <- "…auf freiwillige Massnahmen (z.B. Informationskampagnen \nund Selbstverpflichtungen) gesetzt werden."
plot_df_1$variable[plot_df_1$variable == "…auf Subventionen gesetzt werden."] <- "…auf Subventionen gesetzt werden."
plot_df_1$variable[plot_df_1$variable == "...auf CO2-Abgaben gesetzt werden."] <- "…auf CO2-Abgaben gesetzt werden.."
plot_df_1$variable[plot_df_1$variable == "…auf regulatorische Massnahmen (z.B. Emissionsstandards und Verbote) gesetzt werden."] <- "…auf regulatorische Massnahmen (z.B. Emissionsstandards\n und Verbote) gesetzt werden."
plot_df_1$variable[plot_df_1$variable == "...auf Massnahmen zur Förderung technologischer Innovationen gesetzt werden."] <- "…auf Massnahmen zur Förderung technologischer \nInnovationen gesetzt werden."
plot_df_1$variable[plot_df_1$variable == "...auf Massnahmen zur Förderung von Verhaltensänderungen gesetzt werden."] <- "…auf Massnahmen zur Förderung von \nVerhaltensänderungen gesetzt werden."
plot_df_1$variable[plot_df_1$variable == "…auf eine Kombination verschiedener Massnahmen gesetzt werden."] <- "…auf eine Kombination verschiedener \nMassnahmen gesetzt werden."
plot_df_1$variable[plot_df_1$variable == "…die Kompetenz beim Bund (nicht den Kantonen) liegen."] <- "…die Kompetenz beim Bund \n(nicht den Kantonen) liegen."

plot_df_1$value <- factor(plot_df_1$value, ordered = T,
                          levels = c("Unterstütze voll und ganz", "Unterstütze eher", "Weder noch", "Lehne eher ab", "Lehne voll und ganz ab")) 

plot_df_1 <- plot_df_1 %>%
  group_by(variable, value) %>% 
  dplyr::summarise(count = n()) %>% 
  group_by(variable) %>% 
  dplyr::mutate(sum_var = sum(count),
                pct = count/sum_var*100,
                col = value
  )

clim_pol_generell <- plot_df_1 %>%
  ggplot(.) + 
  geom_bar(stat = "identity", aes(x = variable, y = pct, fill = value), position = position_stack(reverse = TRUE)) +
  theme_SM() +
  theme(legend.position = "bottom", plot.title = element_textbox_simple()) +
  scale_fill_manual(name="Antwort Kategorien", values=c("darkgreen", "lightgreen", "grey", "red3", "red4")) +
  geom_text(mapping = aes(label = paste0(round(pct, 0), "%"), x = variable, y = pct),
            color = "black", cex = 5,
            position=position_stack(vjust=0.5)
  ) +
  ylab("Prozentuale Zustimmung") +   xlab("Bewertungskriterium") + labs(col ="Antwort Kategorien", caption = "(je 1458 Antworten)") +
  ggtitle("<b>Alle:</b> Geben Sie bitte an, 
          wie sehr Sie die folgenden Aussagen ablehnen oder unterstützten. Bei der Ausgestaltung der Schweizer Klimapolitik 
          sollte vor allem…. ") +
  coord_flip() + 
  guides(fill=guide_legend(ncol=2)) 
clim_pol_generell
ggsave(clim_pol_generell, filename = "Plots/clim_pol_alle.pdf", height=  10, width = 10)

######################################################
# transform data for conjoint
######################################################

## (1) use user-defined function to transform data to long
# variables which will be supplied ot the function below for transformation
qid_values <- c("QID430_cjp1",	"QID430_cjp2",	"QID431_cjp1",	"QID431_cjp2", "QID392_cjp1",	"QID392_cjp2", "QID393_cjp1", "QID393_cjp2") # Werte
support <- c("QID98_1", "QID98_2", "QID270_1", "QID270_2", "QID282_1",	"QID282_2", "QID281_1", "QID281_2") # Werte
activism <- c("QID389_1", "QID389_2", "QID415_1", "QID415_2", "QID412_1",	"QID412_2", "QID414_1", "QID414_2") # Werte
choice <- c("QID96", "QID269", "QID277", "QID279") # Werte
vals <- qid_values

# transform the data
dat <- split_cjp(vote, qid_values, support, activism, choice)

## (2) recode variable format and content (languages)
# to character
dat[,  c("attrib1_lab", "attrib2_lab", "attrib3_lab", "attrib4_lab", "attrib5_lab", "attrib6_lab")] <- sapply(dat[,  c("attrib1_lab", "attrib2_lab", "attrib3_lab", "attrib4_lab", "attrib5_lab", "attrib6_lab")], as.character)

# recode labels of the conjoint (French / German) for uniform labelling
dat$attrib2_lab[grepl("(0 Fr./t CO2)", dat$attrib2_lab, fixed = T)] <- "No tax on petrol (0 Fr./t CO2)"
dat$attrib2_lab[grepl("(60 Fr./t CO2)", dat$attrib2_lab, fixed = T)] <- "0.14 Fr./l petrol (60 Fr./t CO2)"
dat$attrib2_lab[grepl("(120 Fr./t CO2)", dat$attrib2_lab, fixed = T)] <- "0.28 Fr./l petrol (120 Fr./t CO2)"
dat$attrib2_lab[grepl("(180 Fr./t CO2)", dat$attrib2_lab, fixed = T)] <- "0.42 Fr./l petrol (180 Fr./t CO2)"
dat$attrib2_lab[grepl("(240 Fr./t CO2)", dat$attrib2_lab, fixed = T)] <- "0.56 Fr./l petrol (240 Fr./t CO2)"

dat$attrib3_lab[grepl("(0 Fr./t CO2)", dat$attrib3_lab, fixed = T)] <- "No tax on heating oil (0 Fr./t CO2)" 
dat$attrib3_lab[grepl("(60 Fr./t CO2)", dat$attrib3_lab, fixed = T)] <- "0.16 Fr./l heating oil (60 Fr./t CO2)"
dat$attrib3_lab[grepl("(120 Fr./t CO2)", dat$attrib3_lab, fixed = T)] <- "0.31 Fr./l heating oil (120 Fr./t CO2)"
dat$attrib3_lab[grepl("(180 Fr./t CO2)", dat$attrib3_lab, fixed = T)] <- "0.47 Fr./l heating oil (180 Fr./t CO2)"
dat$attrib3_lab[grepl("(240 Fr./t CO2)", dat$attrib3_lab, fixed = T)] <- "0.63 Fr./l heating oil (240 Fr./t CO2)"

dat$attrib4_lab[grepl("(0 Fr./t CO2)", dat$attrib4_lab, fixed = T)] <- "No tax on meat (0 Fr./t CO2)" 
dat$attrib4_lab[grepl("(60 Fr./t CO2)", dat$attrib4_lab, fixed = T)] <- "0.77 Fr./kg meat (60 Fr./t CO2)"
dat$attrib4_lab[grepl("(120 Fr./t CO2)", dat$attrib4_lab, fixed = T)] <- "1.53 Fr./kg meat (120 Fr./t CO2)"
dat$attrib4_lab[grepl("(180 Fr./t CO2)", dat$attrib4_lab, fixed = T)] <- "2.30 Fr./kg meat (180 Fr./t CO2)"
dat$attrib4_lab[grepl("(240 Fr./t CO2)", dat$attrib4_lab, fixed = T)] <- "3.07 Fr./kg meat (240 Fr./t CO2)"

dat$attrib5_lab[dat$attrib5_lab %in% c("Pas de taxe", "Keine Abgabe")] <- "No tax on private flights"
dat$attrib5_lab[dat$attrib5_lab %in% c("10 Fr. pour les vols court-courrier et <br>30 Fr. pour les vols long-courrier", "10 Fr. für Kurz- und <br>30 Fr. für Langstrecken")] <-  "10 Fr. for short- and 30 Fr. for long-distance"
dat$attrib5_lab[dat$attrib5_lab %in% c("25 Fr. pour les vols court-courrier et <br>75 Fr. pour les vols long-courrier", "25 Fr. für Kurz- und <br>75 Fr. für Langstrecken")] <-  "25 Fr. for short- and 75 Fr. for long-distance"
dat$attrib5_lab[dat$attrib5_lab %in% c("40 Fr. pour les vols court-courrier et <br>120 Fr. pour les vols long-courrier", "40 Fr. für Kurz- und <br>120 Fr. für Langstrecken")] <- "40 Fr. for short- and 120 Fr. for long-distance"
dat$attrib5_lab[dat$attrib5_lab %in% c("55 Fr. pour les vols court-courrier et <br>165 Fr. pour les vols long-courrier", "55 Fr. für Kurz- und <br>165 Fr. für Langstrecken")] <- "55 Fr. for short- and 165 Fr. for long-distance"

dat$attrib6_lab[dat$attrib6_lab %in% c("Ausschliesslich pauschale Rückerstattung", "Uniquement des remboursements forfaitaires")] <- "Exclusively lump sum reimbursement"
dat$attrib6_lab[dat$attrib6_lab %in% c("Mehrheitlich pauschale Rückerstattung", "Majoritairement des remboursements forfaitaires")] <- "Mostly lump sum reimbursement"  
dat$attrib6_lab[dat$attrib6_lab %in% c("Pauschale Rückerstattung und Klimaschutzförderbeiträge", "Des remboursements forfaitaires et des investissements dans la protection du climat")] <- "Lump sum reimbursement und investment into climate protection"
dat$attrib6_lab[dat$attrib6_lab %in% c("Mehrheitlich für Klimaschutzförderbeiträge", "Majoritairement des investissements dans la protection du climat")] <- "Mostly investment into climate protection"
dat$attrib6_lab[dat$attrib6_lab %in% c("Ausschliesslich für Klimaschutzförderbeiträge", "Exclusivement des investissements dans la protection du climat")] <- "Exclusively investment into climate protection"

# check that none are NA, should be FALSE
any(sapply(dat[, c( "attrib2_lab", "attrib3_lab", "attrib4_lab", "attrib5_lab", "attrib6_lab")], is.na))

# # delete html column breaks inserted for layout in qualtrics
# dat[, c( "attrib2_lab", "attrib3_lab", "attrib4_lab", "attrib5_lab", "attrib6_lab")] <- lapply(dat[, c( "attrib2_lab", "attrib3_lab", "attrib4_lab", "attrib5_lab", "attrib6_lab")], function(x) {gsub("<br>", "", x)})

## (3) transform DVs

# create numeric rate variable
dat$rate <- as.character(dat$rate)
dat$rate[which(dat$rate == "Lehne voll und ganz ab")] <- 1
dat$rate[which(dat$rate == "Lehne ab")] <- 2
dat$rate[which(dat$rate == "Weder noch")] <- 3
dat$rate[which(dat$rate == "Unterstütze")] <- 4
dat$rate[which(dat$rate == "Unterstütze voll und ganz")] <- 5
dat$rate <- as.numeric(dat$rate)

# create numeric activism_rate variable
dat$rate_activism <- as.character(dat$rate_activism)
dat$rate_activism[dat$rate_activism == "Viel Aufwand zur Verhinderung"] <- 1
dat$rate_activism[dat$rate_activism == "Etwas Aufwand zur Verhinderung"] <- 2
dat$rate_activism[dat$rate_activism == "Weder noch"] <- 3
dat$rate_activism[dat$rate_activism == "Etwas Aufwand zur Unterstützung"] <- 4
dat$rate_activism[dat$rate_activism == "Etwas Aufwand zur Unterstzützung"] <- 4 # typo in the questionnaire
dat$rate_activism[dat$rate_activism == "Viel Aufwand zur Unterstützung"] <- 5
dat$rate_activism <- as.numeric(dat$rate_activism)

## (4) create numeric attributes
dat[, "Attrib1"] <- ""
dat$Attrib1[grepl('40', dat$attrib1_lab)] <- 1
dat$Attrib1[grepl('50', dat$attrib1_lab)] <- 2
dat$Attrib1[grepl('60', dat$attrib1_lab)] <- 3
dat$Attrib1[grepl('70', dat$attrib1_lab)] <- 4
dat$Attrib1[grepl('80', dat$attrib1_lab)] <- 5

dat[, "Attrib2"] <- ""
dat$Attrib2[grepl('0 Fr./t', dat$attrib2_lab)] <- 1
dat$Attrib2[grep('60 Fr./t', dat$attrib2_lab)] <- 2
dat$Attrib2[grepl('120 Fr./t', dat$attrib2_lab)] <- 3
dat$Attrib2[grepl('180 Fr./t', dat$attrib2_lab)] <- 4
dat$Attrib2[grepl('240 Fr./t', dat$attrib2_lab)] <- 5

dat[, "Attrib3"] <- ""
dat$Attrib3[grepl('0 Fr./t', dat$attrib3_lab)] <- 1
dat$Attrib3[grepl('60 Fr./t', dat$attrib3_lab)] <- 2
dat$Attrib3[grepl('120 Fr./t', dat$attrib3_lab)] <- 3
dat$Attrib3[grepl('180 Fr./t', dat$attrib3_lab)] <- 4
dat$Attrib3[grepl('240 Fr./t', dat$attrib3_lab)] <- 5

dat[, "Attrib4"] <- ""
dat$Attrib4[grepl('0 Fr./t', dat$attrib4_lab)] <- 1
dat$Attrib4[grepl('60 Fr./t', dat$attrib4_lab)] <- 2
dat$Attrib4[grepl('120 Fr./t', dat$attrib4_lab)] <- 3
dat$Attrib4[grepl('180 Fr./t', dat$attrib4_lab)] <- 4
dat$Attrib4[grepl('240 Fr./t', dat$attrib4_lab)] <- 5

dat[, "Attrib5"] <- ""
dat$Attrib5[grepl('No tax on private flights', dat$attrib5_lab)] <- 1
dat$Attrib5[grepl('10 Fr.', dat$attrib5_lab)] <- 2
dat$Attrib5[grepl('25 Fr.', dat$attrib5_lab)] <- 3
dat$Attrib5[grepl('40 Fr.', dat$attrib5_lab)] <- 4
dat$Attrib5[grepl('55 Fr.', dat$attrib5_lab)] <- 5

dat[, "Attrib6"] <- ""
dat$Attrib6[grepl("Exclusively lump sum reimbursement", dat$attrib6_lab)] <- 1
dat$Attrib6[grepl("Mostly lump sum reimbursement", dat$attrib6_lab)] <- 2
dat$Attrib6[grepl("Lump sum reimbursement und investment into climate protection", dat$attrib6_lab)] <- 3
dat$Attrib6[grepl("Mostly investment into climate protection", dat$attrib6_lab)] <- 4
dat$Attrib6[grepl("Exclusively investment into climate protection", dat$attrib6_lab)] <- 5

# replace parentheses because they seem to create problems when reading the data into the function
dat$attrib2_lab <- gsub("\\s*\\([^\\)]+\\)", "", as.character(dat$attrib2_lab))
dat$attrib3_lab <- gsub("\\s*\\([^\\)]+\\)", "", as.character(dat$attrib3_lab))
dat$attrib4_lab <- gsub("\\s*\\([^\\)]+\\)", "", as.character(dat$attrib4_lab))
dat$attrib5_lab <- gsub("\\s*\\([^\\)]+\\)", "", as.character(dat$attrib5_lab))
dat$attrib6_lab <- gsub("\\s*\\([^\\)]+\\)", "", as.character(dat$attrib6_lab))

## (4) reformat
# transform all to factor
dat[,  c("Attrib1", "Attrib2", "Attrib3", "Attrib4", "Attrib5", "Attrib6")] <- sapply(dat[,  c("Attrib1", "Attrib2", "Attrib3", "Attrib4", "Attrib5", "Attrib6")], as.factor)

# transform all to factor with ordered levels
dat$attrib1_lab <- factor(dat$attrib1_lab)
dat$attrib2_lab <- factor(dat$attrib2_lab, levels = c("No tax on petrol", "0.14 Fr./l petrol", "0.28 Fr./l petrol", "0.42 Fr./l petrol", "0.56 Fr./l petrol"))
dat$attrib3_lab <- factor(dat$attrib3_lab, levels = c("No tax on heating oil", "0.16 Fr./l heating oil", "0.31 Fr./l heating oil", "0.47 Fr./l heating oil", "0.63 Fr./l heating oil"))
dat$attrib4_lab <- factor(dat$attrib4_lab, levels = c("No tax on meat", "0.77 Fr./kg meat", "1.53 Fr./kg meat", "2.30 Fr./kg meat", "3.07 Fr./kg meat"))
dat$attrib5_lab <- factor(dat$attrib5_lab, levels = c("No tax on private flights", "10 Fr. for short- and 30 Fr. for long-distance", "25 Fr. for short- and 75 Fr. for long-distance", "40 Fr. for short- and 120 Fr. for long-distance", "55 Fr. for short- and 165 Fr. for long-distance"))
dat$attrib6_lab <- factor(dat$attrib6_lab, levels = c("Exclusively lump sum reimbursement", "Mostly lump sum reimbursement", "Lump sum reimbursement und investment into climate protection", "Mostly investment into climate protection", "Exclusively investment into climate protection"))

######################################################
# recode explanatory variables
######################################################

# (1) benefits
dat$prior_benefit[dat$QID565 %in% c("Stark beeinflusst", "Etwas beeinflusst")] <- 1
dat$prior_benefit[dat$QID565 %in% c("Weiss nicht / keine Antwort", "Eher nicht beeinflusst", "Überhaupt nicht beeinflusst")] <- 0

dat$prior_benefit_2[dat$QID565 %in% c("Stark beeinflusst")] <- 5
dat$prior_benefit_2[dat$QID565 %in% c("Etwas beeinflusst")] <- 4
dat$prior_benefit_2[dat$QID565 %in% c("Weiss nicht / keine Antwort")] <- 3
dat$prior_benefit_2[dat$QID565 %in% c("Eher nicht beeinflusst")] <- 2
dat$prior_benefit_2[dat$QID565 %in% c("Überhaupt nicht beeinflusst")] <- 1

table(dat$prior_benefit)

# (2) charging stations to numeric
dat$no_of_stations_ev <- as.numeric(dat$no_of_stations_ev)

# (3) housing
dat$home_owner[dat$QID515 == "Ja"] <- 1
dat$home_owner[dat$QID515 == "Nein"] <- 0
table(dat$home_owner)

dat$renew_heating <- ifelse(grepl("Gasheizung|Ölheizung", dat$QID517) == F, 1, 0)

# (2) driving
dat$driver[dat$QID519 == "Ja, eins" | dat$QID519== "Ja, mehere"] <- 1
dat$driver[dat$QID519 == "Nein"] <- 0
table(dat$driver)
dat$ren_driver <- ifelse(grepl("Strom|Erdgas|Biotreibstoffe", dat$QID535) & !is.na(dat$QID535), 1, 0)

dat$vege <- ifelse(dat$QID582 == "Ja", 0, 1)

# (8) co2 law beliefs
# is forced choice
transform <- function(x){
  unlist(lapply(x, function(x)
    if (is.na(x)) {NA} 
    else if (x == "Voll und ganz") {5}
    else if (x == "Eher") {4}
    else if (x == "Weder noch") {3}
    else if (x == "Eher nicht") {2}
    else if (x == "Ganz und gar nicht") {1}
  )
  )
}

dat$co2_law_effect <- transform(dat$QID82_1)
dat$co2_law_effic <- transform(dat$QID82_2)
dat$co2_law_compet <- transform(dat$QID82_3)
dat$co2_law_just <- transform(dat$QID82_4)
dat$co2_law_transf <- transform(dat$QID82_5)
table(dat$co2_law_transf, useNA = "always") 



## (9) salience, # no 0_2:0_4 not in excel file
# top
dat$top_sal_ahv <- ifelse(dat$QID432_0_1_RANK == "1.0" & !is.na(dat$QID432_0_1_RANK), 1, 0) # Altersvorsorge/AHV
dat$top_sal_unemp <- ifelse(dat$QID432_0_5_RANK == "1.0" & !is.na(dat$QID432_0_5_RANK), 1, 0) # Arbeitslosigkeit
dat$top_sal_refug <- ifelse(dat$QID432_0_6_RANK == "1.0" & !is.na(dat$QID432_0_6_RANK), 1, 0) # Flüchtlinge
dat$top_sal_eu <- ifelse(dat$QID432_0_7_RANK == "1.0" & !is.na(dat$QID432_0_7_RANK), 1, 0) # Verhältnis der Schweiz zur Europäischen Union
dat$top_sal_health <- ifelse(dat$QID432_0_8_RANK == "1.0" & !is.na(dat$QID432_0_8_RANK), 1, 0) # Gesundheitswesen / Krankenversicherung
dat$top_sal_energy <- ifelse(dat$QID432_0_9_RANK == "1.0" & !is.na(dat$QID432_0_9_RANK), 1, 0) # Energieversorgung
dat$top_sal_traff <- ifelse(dat$QID432_0_10_RANK == "1.0" & !is.na(dat$QID432_0_10_RANK), 1, 0) # Verkehr
dat$top_sal_glob <- ifelse(dat$QID432_0_11_RANK == "1.0" & !is.na(dat$QID432_0_11_RANK), 1, 0) # Globalisierung der Wirtschaft / Freihandel
dat$top_sal_env <- ifelse(dat$QID432_0_12_RANK == "1.0" & !is.na(dat$QID432_0_12_RANK), 1, 0) # Umweltschutz / Klimawandel
dat$top_sal_crim <- ifelse(dat$QID432_0_13_RANK == "1.0" & !is.na(dat$QID432_0_13_RANK), 1, 0) # Kriminalität
dat$top_sal_uneq <- ifelse(dat$QID432_0_14_RANK == "1.0" & !is.na(dat$QID432_0_14_RANK), 1, 0) # Ungleichheit bei Einkommen und Vermögen
dat$top_sal_cult <- ifelse(dat$QID432_0_15_RANK == "1.0" & !is.na(dat$QID432_0_15_RANK), 1, 0) # Zusammenleben von Menschen unterschiedlicher Kulturen und Religionen
dat$top_sal_for <- ifelse(dat$QID432_0_16_RANK == "1.0" & !is.na(dat$QID432_0_16_RANK), 1, 0) # Ausländische Arbeitskräfte in der Schweiz
dat$top_sal_pop <- ifelse(dat$QID432_0_17_RANK == "1.0" & !is.na(dat$QID432_0_17_RANK), 1, 0) # Zunahme der Schweizer Wohnbevölkerung / Zersiedelung / Verstädterung
table(dat$top_sal_pop, useNA = "always") 

# any of three
dat$sal_ahv <- ifelse(is.na(dat$QID432_0_1_RANK), 0, 1)
dat$sal_unemp <- ifelse(is.na(dat$QID432_0_5_RANK), 0, 1)
dat$sal_refug <- ifelse(is.na(dat$QID432_0_6_RANK), 0, 1)
dat$sal_eu <- ifelse(is.na(dat$QID432_0_7_RANK), 0, 1)
dat$sal_health <- ifelse(is.na(dat$QID432_0_8_RANK), 0, 1)
dat$sal_energy <- ifelse(is.na(dat$QID432_0_9_RANK), 0, 1)
dat$sal_traff <- ifelse(is.na(dat$QID432_0_10_RANK), 0, 1) 
dat$sal_glob <- ifelse(is.na(dat$QID432_0_11_RANK), 0, 1)
dat$sal_env <- ifelse(is.na(dat$QID432_0_12_RANK), 0, 1)
dat$sal_crim <- ifelse(is.na(dat$QID432_0_13_RANK), 0, 1)
dat$sal_uneq <- ifelse(is.na(dat$QID432_0_14_RANK), 0, 1)
dat$sal_cult <- ifelse(is.na(dat$QID432_0_15_RANK), 0, 1)
dat$sal_for <- ifelse(is.na(dat$QID432_0_16_RANK), 0, 1)
dat$sal_pop <- ifelse(is.na(dat$QID432_0_17_RANK), 0, 1)
table(dat$sal_pop, useNA = "always") 

transform <- function(x){
  unlist(lapply(x, function(x)
    if (is.na(x)) {NA} 
    else if (x == "Unterstütze voll und ganz") {5}
    else if (x == "Unterstütze eher") {4}
    else if (x == "Weder noch") {3}
    else if (x == "Lehne eher ab") {2}
    else if (x == "Lehne voll und ganz ab") {1}
  )
  )
}
# (10) swiss climate policy beliefs
dat$swiss_pol_comp <- transform(dat$QID76_1)
dat$swiss_pol_effect <- transform(dat$QID76_2)
dat$swiss_pol_effic <-transform( dat$QID76_3)
dat$swiss_pol_just <- transform(dat$QID76_4)
dat$swiss_pol_lead <- transform(dat$QID76_5)
dat$swiss_pol_vol <- transform(dat$QID76_6)
dat$swiss_pol_subs <- transform(dat$QID76_7) 
dat$swiss_pol_tax <- transform(dat$QID76_8)
dat$swiss_pol_regu <- transform(dat$QID76_9)
dat$swiss_pol_tech <- transform(dat$QID76_10)
dat$swiss_pol_beha <- transform(dat$QID76_11)
dat$swiss_pol_comb <- transform(dat$QID76_12)
dat$swiss_pol_fed <- transform(dat$QID76_13)
table(dat$swiss_pol_fed, useNA = "always") 

## demographics
# (10) demos: education
dat$educ[dat$QID16 == "Keine Ausbildung abgeschlossen" | 
         dat$QID16 == "Ich bin noch in der obligatorischen Schule" |
         dat$QID16 == "Obligatorische Schule" |
         dat$QID16 == "Übergangsausbildung (z.B. Anlehre, 10. Schuljahr, Haushaltsjahr, Sprachschule mit Zertifikat)"] <- 0
dat$educ[dat$QID16 == "Berufslehre, BMS, Vollzeitberufsschule (Handelsmittelschule/Lehrwerkstätte)" | 
           dat$QID16 == "Berufsmaturität" |
           dat$QID16 == "Diplommittelschule, allgemein bild. Schule ohne Maturität (Verkehrsschule)" |
           dat$QID16 == "Maturitätsschule, Lehrkräfte-Seminar (vorbereitende Ausbildung für Lehrkräfte von Kindergarten, Primarschule, Handarbeit, Hauswirtschaft)"] <- 1
dat$educ[dat$QID16 == "Höhere Berufsausbildung mit Meisterdiplom, Eidg. Fachausweis" | 
           dat$QID16 == "Techniker- oder Fachschule (2 Jahre Voll- oder 3 Jahre Teilzeit)" |
           dat$QID16 == "Höhere Fachschule/Fachhochschule, HTL, HMV (3 Jahre Voll- oder 4 Jahre Teilzeit)" |
           dat$QID16 == "Universität, ETH, Fachhochschule, Pädagogische Hochschule"] <- 2
table(dat$educ, useNA = "always") # Weiss nicht / keine Antwort --> NA
# dat$educ <- ifelse(is.na(dat$educ), median(dat$educ), dat$educ)

# (11) demos: employment condition
dat$empl_cond <- 0 # 
dat$empl_cond[grepl("Teilzeit arbeitstätig (21-34.9 Std. pro Woche)", dat$QID19) == T | 
                grepl("Teilzeit arbeitstätig (9-20.9 Std. pro Woche)", dat$QID19) == T |
                grepl("Teilzeit arbeitstätig (1-8.9 Std. pro Woche)", dat$QID19) == T ] <- 1
dat$empl_cond[grepl("Vollzeit arbeitstätig (35 Std. und mehr pro Woche)", dat$QID19) == T ] <- 2
table(dat$empl_cond, useNA = "always") # Weiss nicht / keine Antwort --> NA

# (11) demos: employment sect 
dat$empl_sect <- 0 # includes all those with "Weiss nicht / keine Antwort" and all those those that do not work
dat$empl_sect[dat$QID20 == "Primärer Sektor, u.a. Landwirtschaft, Forstwirtschaft"] <- 1
dat$empl_sect[dat$QID20 == "Sekundärer Sektor, u.a. Industrie, Gewerbe, Handwerk"] <- 2
dat$empl_sect[dat$QID20 == "Tertiärer Sektor, u.a. Dienstleistungen, Verwaltungen"] <- 3
table(dat$empl_sect, useNA = "always") # Weiss nicht / keine Antwort --> NA

# (12) demos: financial conditions
dat$fin_cond[dat$QID21 == "Ja"] <- 3
dat$fin_cond[dat$QID21 == "Es geht so"] <- 2
dat$fin_cond[dat$QID21 == "Nein"] <- 1
dat$fin_cond[dat$QID21 == "Weiss nicht / keine Antwort"] <- 0
table(dat$fin_cond, useNA = "always") # Weiss nicht / keine Antwort --> NA

dat$age[dat$QID2 == "18 bis 24 Jahre"] <- 1
dat$age[dat$QID2 == "25 bis 34 Jahre"] <- 2
dat$age[dat$QID2 == "35 bis 44 Jahre"] <- 3
dat$age[dat$QID2 == "45 bis 54 Jahre"] <- 4
dat$age[dat$QID2 == "55 bis 64 Jahre"] <- 5
dat$age[dat$QID2 == "65 bis 74 Jahre"] <- 6
dat$age[dat$QID2 == "75+ Jahre"] <- 7

# gender
dat$gender[dat$QID3 == "Mann"] <- 1
dat$gender[dat$QID3 == "Frau"] <- 0

# language
unique(dat$Q_Language)
dat$language[dat$Q_Language == "FR"] <- 1
dat$language[dat$Q_Language == "DE"] <- 0

# (13) demos: left-right
sum(is.na(dat$QID27_1))/sum(!is.na(dat$QID27_1)) # 12% NA, keine Antwort 
dat$left_right <- as.numeric(dat$QID27_1)

# demos not included: civil status (QID17), political party preference (QID28) has less NAs than left-right

# recode number of stations
dat$no_of_stations_ev_1 <- factor(ifelse(dat$no_of_stations_ev >=1, 1, 0))
dat$no_of_stations_ev_2 <- factor(ifelse(dat$no_of_stations_ev >=2, 1, 0))
dat$no_of_stations_ev_3 <- factor(ifelse(dat$no_of_stations_ev >=3, 1, 0))
dat$no_of_stations_ev_4 <- factor(ifelse(dat$no_of_stations_ev >=4, 1, 0))
dat$no_of_stations_ev_5 <- factor(ifelse(dat$no_of_stations_ev >=5, 1, 0))

dat$ratio_ev_to_muni_area <- as.numeric(dat$ratio_ev_to_muni_area)

unique(dat$QID4)
dat$region[dat$QID4 %in% c("Waadt", "Genf", "Valais (francophone)", "Wallis (deutschsprachig)")] <- 1 # Genfersee Region
dat$region[dat$QID4 %in% c("Berne (francophone)", "Bern (deutschsprachig)", "Solothurn", "Neuchâtel", "Jura", "Fribourg (francophone)", "Freiburg (deutschsprachig)")] <- 2 # Mittelland
dat$region[dat$QID4 %in% c("Aargau", "Basel-Landschaft", "Basel-Stadt")] <- 3 # Nordwestschweiz
dat$region[dat$QID4 %in% c("Zürich")] <- 4 # Zürich 
dat$region[dat$QID4 %in% c("St. Gallen", "Thurgau", "Schaffhausen", "Graubünden/Grischun (deutsch/rätoromanisch)", "Appenzell-Ausserrhoden", "Appenzell-Innerrhoden" )] <- 5 # Ostschweiz
dat$region[dat$QID4 %in% c("Schwyz", "Luzern", "Zug", "Glarus", "Nidwalden", "Obwalden", "Uri")] <- 6 # Zentralschweiz
dat$region[dat$QID4 %in% c("Tessin")] <- 7 # Tessin
table(dat$region, useNA = "always")

library(sf)
rural_urban <- read_csv("geoDaten/plz_merged_raumtypo/plz_raumtypo.csv")
rural_urban <- rural_urban[!duplicated(rural_urban$plz),]
table(rural_urban$Kategorien, useNA = "always")

nrow(dat)  
dat <- left_join(dat, rural_urban, by = c("plz"))
nrow(dat)

dat$urban_rural[grepl("1", dat$Kategorien)] <- 1
dat$urban_rural[grepl("2", dat$Kategorien)] <- 2
dat$urban_rural[grepl("3", dat$Kategorien)] <- 3
table(dat$urban_rural, useNA = "always")
unique(dat$plz[is.na(dat$urban_rural)])

unique(dat$plz[!is.na(dat$urban_rural) & is.na(dat$ratio_ev_to_muni_area)])
unique(dat$plz[!is.na(dat$urban_rural) & is.na(dat$ratio_ev_to_muni_area)])


vars_to_transform2 <- c("prior_benefit_2", "sal_env", "sal_glob", "driver", "ren_driver", "educ", "fin_cond", "age",
                       "home_owner", "empl_sect")

# need to be factors for conjoint
dat[vars_to_transform2] <- lapply(dat[vars_to_transform2], factor)


dat_desc <- dat %>% 
  dplyr::select(rate, choice, prior_benefit_2, ratio_ev_to_muni_area, driver, home_owner, age, educ, language, empl_sect, fin_cond, 
                left_right, sal_glob, sal_env, region, urban_rural) %>% 
  mutate("Region: Geneva" = ifelse(region == 1, 1, 0), 
         "Region: Middle Land" = ifelse(region == 2, 1, 0),
         "Region: North East" = ifelse(region == 3, 1, 0),
         "Region: Zurich" = ifelse(region == 4, 1, 0),
         "Region: East" = ifelse(region == 5, 1, 0),
         "Region: Central" = ifelse(region == 6, 1, 0),
         "Region: Ticino" = ifelse(region == 7, 1, 0),
         "Urban Area" = ifelse(urban_rural == 1, 1, 0),
         "Intermediate Area" = ifelse(urban_rural == 2, 1, 0),
         "Rural Area" = ifelse(urban_rural == 3, 1, 0),) %>% 
  dplyr::select(-region, -urban_rural) %>% 
  mutate_all(as.character) %>% 
  mutate_all(as.numeric) 

p_desc <- dat_desc %>% 
  mutate(ratio_ev_to_muni_area = round(ratio_ev_to_muni_area, 0)) %>% 
  pivot_longer(., everything(), names_to = "Question", values_to = "Response") %>% 
  mutate(Question = factor(Question, levels = c("rate", "choice", "prior_benefit_2", "ratio_ev_to_muni_area", "driver", "home_owner", "age",
                                                "educ", "language", "empl_sect", "fin_cond", 
                                                "left_right", "sal_glob", "sal_env",
                                                "Region: Geneva",
                                                "Region: Middle Land",
                                                "Region: North East",
                                                "Region: Zurich",
                                                "Region: East",
                                                "Region: Central",
                                                "Region: Ticino",
                                                "Urban Area",
                                                "Intermediate Area",
                                                "Rural Area"))) %>% 
  group_by(Question, Response) %>% 
  count() %>% 
  mutate(Response = factor(Response, levels = as.character(c(0:14, NA)))) %>% 
  ggplot(aes(x = Response, y = n)) +
  geom_col() +
  facet_wrap(~Question, scales = "free_x", 
             labeller = labeller(Question = c("driver" = "Driver",
                                              "educ" = "Education",
                                              "age" = "Age",
                                              "empl_sect" = "Employment Sector",
                                              "fin_cond" = "Financial Condition",
                                              "home_owner" = "Home Owner",
                                              "left_right" = "Left−Right",
                                              "prior_benefit_2" = "Percieved Prior Benefit",
                                              "rate" = "Support \n(Rate Outcome)",
                                              "choice" = "Support \n(Choice Outcome)",
                                              "ratio_ev_to_muni_area" = "EV Charging Stations",
                                              "sal_env" = "Salience: \nEnvironment and Climate",
                                              "sal_glob" = "Salience: \nGlobalisation",
                                              "language" = "French",
                                              "Urban Area" = "Urban Area",
                                              "Intermediate Area" = "Intermediate Area",
                                              "Rural Area"  = "Rural Area",
                                              "Region: Geneva" = "Region: Geneva",
                                              "Region: Middle Land" = "Region: Middle Land",
                                              "Region: North East" = "Region: North East",
                                              "Region: Zurich" = "Region: Zurich",
                                              "Region: East" = "Region: East",
                                              "Region: Central" = "Region: Central",
                                              "Region: Ticino" = "Region: Ticino"))) +
  theme_light()
p_desc
ggsave(p_desc, filename = "Plots/p_desc.pdf", height = 16, width = 10)


labs_desc <- c("Support (Rate Outcome)", "Support (Choice Outcome)", "Perceived Prior Benefit",
               "EV Charging Stations",  "Driver", "Home Owner", "Age", "Education", "French",
               "Employment Sector", "Financial Condition", "Left-Right", "Salience: Globalisation", "Salience: Environment and Climate",
               "Region: Geneva", "Region: Middle Land", "Region: North East", "Region: Zurich", "Region: East", "Region: Central", "Region: Ticino", "Urban Area", "Intermediate Area", "Rural Area")

library(stargazer)
stargazer(dat_desc,
          out.header = F,
          no.space = TRUE, 
          label = "tab:summary_stats",
          column.sep.width = "3pt",
          font.size = "footnotesize",
          covariate.labels = labs_desc
          )

library(Hmisc)
correlation_matrix <- cor(dat_desc %>% mutate_all(., as.numeric), use = "pairwise.complete.obs")
correlation_matrix <- round(correlation_matrix, 2)
correlation_matrix[upper.tri(correlation_matrix)] <- NA
diag(correlation_matrix) <- NA
colnames(correlation_matrix) <- rownames(correlation_matrix) <- labs_desc
correlation_matrix1 <- correlation_matrix[,1:12]
correlation_matrix2 <- correlation_matrix[,13:ncol(correlation_matrix)]

stargazer(correlation_matrix1, title="Correlation Matrix Part 1", 
          float.env = "sidewaystable", 
          type = "latex", 
          out.header = F,
          no.space = TRUE, # to remove the spaces after each line of coefficients
          column.sep.width = "1pt", # to reduce column width
          font.size = "footnotesize", # to make font size smaller
          label = "tab:correlation_pt1"
          # covariate.labels = labs_desc,
          # dep.var.labels = labs_desc
)
stargazer(correlation_matrix2, title="Correlation Matrix  Part 2", 
          float.env = "sidewaystable", 
          type = "latex", 
          out.header = F,
          no.space = TRUE, # to remove the spaces after each line of coefficients
          column.sep.width = "1pt", # to reduce column width
          font.size = "footnotesize", # to make font size smaller
          label = "tab:correlation_pt2"
)


# ######################################################
# # weighted linear regression
# ######################################################

# sample distribution for quota variables
dat %>% group_by(age) %>% dplyr::summarise(n = n()/nrow(.))
dat %>% group_by(gender) %>% dplyr::summarise(n = n()/nrow(.))
dat %>% group_by(language) %>% dplyr::summarise(n = n()/nrow(.))

nrow(dat)
# Generate age x gender cross quota variable
dat <- dat %>%
  # filter(!is.na(gender) & !is.na(age) & !is.na(urban_rural)) %>%
  mutate(age_x_urban_rural = case_when(
    # urban:
    age == 1 & urban_rural == 1 ~ "1",
    age == 2 & urban_rural == 1 ~ "2",
    age == 3 & urban_rural == 1 ~ "3",
    age == 4 & urban_rural == 1 ~ "4",
    age == 5 & urban_rural == 1 ~ "5",
    age == 6 & urban_rural == 1 ~ "6",
    age == 7 & urban_rural == 1 ~ "7",

    # intermediary:
    age == 1 & urban_rural == 2 ~ "8",
    age == 2 & urban_rural == 2 ~ "9",
    age == 3 & urban_rural == 2 ~ "10",
    age == 4 & urban_rural == 2 ~ "11",
    age == 5 & urban_rural == 2 ~ "12",
    age == 6 & urban_rural == 2 ~ "13",
    age == 7 & urban_rural == 2 ~ "14",

    # rural
    age == 1 & urban_rural == 3 ~ "15",
    age == 2 & urban_rural == 3 ~ "16",
    age == 3 & urban_rural == 3 ~ "17",
    age == 4 & urban_rural == 3 ~ "18",
    age == 5 & urban_rural == 3 ~ "19",
    age == 6 & urban_rural == 3 ~ "20",
    age == 7 & urban_rural == 3 ~ "21",

  ))

as.numeric(dat$age_x_urban_rural)

# Import age x gender cross quota variable for the population
pop_x_age_x_urban_rural <- read.csv("DataWeighting/Data_bfs_direct/pop_x_age_x_urban_rural.csv")
pop_x_age_x_urban_rural$percentage_of_total_population
# compare distributions visually
p_pop_sample_dist <- dat %>%
  group_by(age_x_urban_rural) %>%
  drop_na(age_x_urban_rural) %>%
  tally() %>%
  summarise(proportion_sample = n /sum(n)) %>%
  bind_cols(.,pop_x_age_x_urban_rural$percentage_of_total_population) %>%
  mutate(labs = rep(c("18 bis 24 Jahre", "25 bis 34 Jahre", "35 bis 44 Jahre", "45 bis 54 Jahre", "55 bis 64 Jahre", "65 bis 74 Jahre", "75+ Jahre"), 3),
         urban_rural = rep(c("Urban", "Intermediary", "Rural"), 7),
         labs = factor(labs, levels = c("18 bis 24 Jahre", "25 bis 34 Jahre", "35 bis 44 Jahre", "45 bis 54 Jahre", "55 bis 64 Jahre", "65 bis 74 Jahre", "75+ Jahre"))) %>%
  pivot_longer(., cols =  c("proportion_sample",  "...2")) %>%
  mutate(name = ifelse(name == "...2", "Population Distribution", name),
         name = ifelse(name == "proportion_sample", "Sample Distribution", name),
         urban_rural = factor(urban_rural, levels = c("Urban", "Intermediary", "Rural"))) %>%
  ggplot(.,aes(x = labs, y = value)) +
  geom_col() +
  labs(x = "", y = "") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust  = 1)) +
  facet_grid(urban_rural~name)
ggsave(p_pop_sample_dist, file = "Plots/pop_sample_dist.pdf", width = 8, height = 8)

## Generate unweighted object
dat_w_o_na <- dat %>% 
  drop_na(rate,
          attrib1_lab, attrib2_lab,  attrib3_lab, attrib4_lab, attrib5_lab, attrib6_lab,
          home_owner,
          driver,
          ratio_ev_to_muni_area,
          prior_benefit, prior_benefit_2,
          vege,
          urban_rural,
          age,
          language,
          gender,
          sal_glob, sal_env,
          educ,
          empl_sect, fin_cond,
          region,
          left_right)
d_unweighted <- svydesign(ids = ~1, weights = NULL, strata = NULL, data = dat_w_o_na)
## Set up population data frames
age_x_urban_rural_pop <- data.frame(age_x_urban_rural = rep(1:nrow(pop_x_age_x_urban_rural)), Freq = pop_x_age_x_urban_rural$Anzahl)
gender_pop <- data.frame(gender = rep(1:0), Freq = c(4302599, 4367701))

## test if the two distributions are not the same
dat_wilcox_test <- dat %>% 
  filter(!is.na(age_x_urban_rural)) %>% 
  mutate(age_x_urban_rural= as.numeric(age_x_urban_rural)) %>% 
  group_by(age_x_urban_rural) %>% 
  tally() %>% 
  arrange(age_x_urban_rural)

ks.test(age_x_urban_rural_pop$Freq, dat_wilcox_test$n, paired = F) # test is rejected

table(age_x_urban_rural_pop$age_x_urban_rural, useNA = "always")
d_weighted <- rake(design = d_unweighted, sample.margins = list(~age_x_urban_rural, ~gender), population.margins = list(age_x_urban_rural_pop, gender_pop))

dat2 <- dat_w_o_na %>%
  dplyr::filter(!is.na(age_x_urban_rural)) %>%
  dplyr::mutate(weight = weights(d_weighted))

######################################################
# weighted regression analysis
######################################################

covars <- c("prior_benefit_2", "ratio_ev_to_muni_area", "driver", "home_owner", "age",  "educ", "language", "empl_sect", "fin_cond", "gender",  "left_right",
            "sal_glob", "sal_env", "urban_rural", "region")
expvars <- c("attrib1_lab", "attrib2_lab", "attrib3_lab", "attrib4_lab", "attrib5_lab", "attrib6_lab")
outcomes <- c("rate","choice")

#----------------------------------------------------#
# operationalisation: expvars as factor levels
#----------------------------------------------------#

reg_dat <- dat2 %>%
  dplyr::select(outcomes, expvars, covars, weight, id) %>%
  mutate_at(vars(covars), as.character) %>% 
  mutate_at(vars(outcomes, covars, weight), as.numeric) %>% 
  mutate_at(vars(empl_sect, urban_rural, region), as.factor)

# test if weighting is necessary
model1.7 <- lm(rate ~  attrib1_lab + attrib2_lab +  attrib3_lab +  attrib4_lab + attrib5_lab + attrib6_lab + 
                 + prior_benefit_2 + driver + home_owner + age + educ + language + empl_sect + fin_cond + left_right  + sal_glob + sal_env + region 
               + urban_rural, data = reg_dat)
weights_tests(model = model1.7, data = reg_dat, weights = weight, sims = 10000) # they are: p-value of near 0

## A note on variable transformation, see Gelmann 2007 http://www.stat.columbia.edu/~gelman/research/unpublished/standardizing.pdf
# to compare continuous variables' effect sizes between each other --> demean and standardise by one standard error 
# to compare continuous to binary variables' effect sizes: divide by two times the standard error
vars_to_transform <- c("prior_benefit_2", "ratio_ev_to_muni_area", "left_right", "age", "educ", "fin_cond")
sdFunc <- function(x){(x)/2*sd(x, na.rm = T)} # binary : CHECK AGAIN
reg_dat[vars_to_transform] <- lapply(reg_dat[vars_to_transform], sdFunc)

design <- svydesign(~id, data = reg_dat, weights = reg_dat$weight)
## (3) linear regression
model1.1 <- svyglm(rate ~ attrib1_lab + attrib2_lab +  attrib3_lab +  attrib4_lab + attrib5_lab + attrib6_lab, 
                   data = reg_dat, weights = weight, design = design)
model1.2 <- svyglm(rate ~ attrib1_lab + attrib2_lab +  attrib3_lab +  attrib4_lab + attrib5_lab + attrib6_lab 
                   + prior_benefit_2, data = reg_dat, weights = weight, design = design)
model1.3 <- svyglm(rate ~ attrib1_lab + attrib2_lab +  attrib3_lab +  attrib4_lab + attrib5_lab + attrib6_lab 
                   + sqrt(ratio_ev_to_muni_area), data = reg_dat, weights = weight, design = design)
model1.4 <- svyglm(rate ~ attrib1_lab + attrib2_lab +  attrib3_lab +  attrib4_lab + attrib5_lab + attrib6_lab 
                   + prior_benefit_2 + driver + home_owner + age + educ + language + empl_sect + fin_cond + left_right  + sal_glob + sal_env + region 
                   + urban_rural, data = reg_dat, weights = weight, design = design)
model1.5 <- svyglm(rate ~ attrib1_lab + attrib2_lab +  attrib3_lab +  attrib4_lab + attrib5_lab + attrib6_lab  
                   + sqrt(ratio_ev_to_muni_area) + driver + home_owner + age + educ + language + empl_sect + fin_cond + left_right  + sal_glob + sal_env + region
                   + urban_rural, data = reg_dat, weights = weight, design = design)
texreg::texreg(list(model1.1, model1.2, model1.3, model1.4, model1.5), digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
               fontsize = "tiny", longtable = T, no.margin = T,
               omit.coef = "region",
               custom.coef.names = c("Intercept",
                                     # Experimental
                                     "50$\\%$", "60$\\%$", "70$\\%$", "80$\\%$",
                                     "0.14 Fr./l petrol", "0.28 Fr./l petrol", "0.42 Fr./l petrol", "0.56 Fr./l petrol",
                                     "0.16 Fr./l heating oil", "0.31 Fr./l heating oil", "0.47 Fr./l heating oil", "0.63 Fr./l heating oil",
                                     "0.77 Fr./kg meat", "1.53 Fr./kg meat", "2.30 Fr./kg meat", "3.07 Fr./kg meat",
                                     "10 Fr. for short- and 30 Fr. for long-distance", "25 Fr. for short- and 75 Fr. for long-distance", "40 Fr. for short- and 120 Fr. for long-distance", "55 Fr. for short- and 165 Fr. for long-distance",
                                     "Mostly reimbursement", "Reimbursement und climate protection", "Mostly climate protection", "Exclusively climate protection",
                                     # EVs
                                     "Percieved Prior Benefit", "EV Charging Stations", 
                                     # CTRLs
                                     "Driver", "Home Owner", "Age", "Education", "French", "Primary Employment Sector", "Secondary Employment Sector", "Tertiary Employment Sector", "Financial Condition", "Left-Right",
                                     "Salience: Globalisation", "Salience: Environment and Climate",
                                     "Intermediate Area", "Rural Area"
                                     # "Preference: Regulatory Instruments", "Preference: Voluntary Instruments", "Preference: Subsidies", "Preference: Technical Solutions",
                                     # "Belief: Effectiveness", "Belief: Efficiency", "Belief: Competitiveness", "Belief: Justice", "Belief: Transformation"
               ),
               groups = list("Experimental: Reduction Target" = 2:5, "Experimental: Tax Road Transport" = 6:9, "Experimental: Tax Housing" = 10:13,
                             "Experimental: Tax Food" = 14:17, "Experimental: Tax Aviation Transport" = 18:21, "Experimental: Revenue Use" = 22:25,
                             "Explanatory Variables" = 26:27, "Controls" = 28:41
               ),
               custom.gof.rows = list("Region Controls" = c("No", "No", "No", "Yes", "Yes"),
                                      "Observations" = lapply(list(model1.1, model1.2, model1.3, model1.4, model1.5), nobs),
                                      "R^2" = sapply(lapply(list(model1.1, model1.2, model1.3, model1.4, model1.5), summ), attr, "rsq"),
                                      "Adj. R^2" = sapply(lapply(list(model1.1, model1.2, model1.3, model1.4, model1.5), summ), attr, "arsq")
               ),
               include.deviance = F,
               label = "table:weighted_direct_exp_factor",
               file = "Tables/weighted_direct_exp_factor.tex",
               use.packages = F, 
               caption = "Survey-weighted generalised linear model with direct effects using the rate outcome. Conjoint attributes are operationalised as ordered factor levels. Normalisation: continous variables are normalised by two times 
               the standard error to make them comparable to the estimates of binary variables following Gelman (2007)")



res <- summary(model1.1)
p_revenue <- res$coefficients %>% 
  as.data.frame() %>% 
  dplyr::filter(grepl("attrib6_lab", rownames(res$coefficients))) %>% 
  rownames_to_column("name") %>% 
  mutate(name = gsub("attrib6_lab", "", name),
         name = ifelse(name == "Mostly lump sum reimbursement", "Mostly reimbursement", name), 
         name = ifelse(name == "Lump sum reimbursement und investment into climate protection", "Reimbursement und climate protection", name), 
         name = ifelse(name == "Mostly investment into climate protection", "Mostly climate protection", name),
         name = ifelse(name == "Exclusively investment into climate protection", "Exclusively climate protection", name),
         name = factor(name, levels = c("Mostly reimbursement", "Reimbursement und climate protection", "Mostly climate protection", "Exclusively climate protection")),
         ci_lo = Estimate - 1.96*`Std. Error`,
         ci_hi = Estimate + 1.96*`Std. Error`) %>% 
  ggplot(., aes(x = name, y = Estimate)) +
  geom_pointrange(aes(ymin = ci_lo, ymax = ci_hi)) +
  coord_flip() +
  labs(
    x = "",
    y = "Estimate",
    subtitle = "Baseline: Exclusively Reimbursement"
  ) +
  geom_hline(yintercept = 0, linetype = 'dashed', col = 'red') +
  theme_light() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=12),
        axis.text.x = element_text(angle = 45, hjust = 1))
p_revenue
ggsave(p_revenue, filename = "Plots/p_revenue.pdf", height = 5, width = 10)

model1.1 <- svyglm(choice ~ attrib1_lab + attrib2_lab +  attrib3_lab +  attrib4_lab + attrib5_lab + attrib6_lab, data = reg_dat, weights = weight, design = design)
summary(model1.1)
##### interactions rate
model1.1 <- svyglm(rate ~ 
                     attrib1_lab*prior_benefit_2 + attrib2_lab*prior_benefit_2 +  attrib3_lab*prior_benefit_2 +  attrib4_lab*prior_benefit_2 + attrib5_lab*prior_benefit_2 + attrib6_lab*prior_benefit_2 +
                     + prior_benefit_2 + driver + home_owner + age + educ + language + empl_sect + fin_cond + left_right  + sal_glob + sal_env + region 
                   + urban_rural
                   , data = reg_dat, weights = weight, design = design)
model1.2 <- svyglm(rate ~ 
                     attrib1_lab*sqrt(ratio_ev_to_muni_area) + attrib2_lab*sqrt(ratio_ev_to_muni_area) +  attrib3_lab*sqrt(ratio_ev_to_muni_area) +  attrib4_lab*sqrt(ratio_ev_to_muni_area) + attrib5_lab*sqrt(ratio_ev_to_muni_area) + attrib6_lab*sqrt(ratio_ev_to_muni_area) +
                     sqrt(ratio_ev_to_muni_area) + driver + home_owner + age + educ + language + empl_sect + fin_cond + left_right  + sal_glob + sal_env + region 
                   + urban_rural
                   , data = reg_dat, weights = weight, design = design)
texreg::texreg(list(model1.1, model1.2), digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
               fontsize = "tiny", longtable = T, no.margin = T,
               reorder.coef = c(1:5, 7:26, 6, 65, 27:41, 42:64, 66:89),
               omit.coef = "region",
               custom.coef.names = c("Intercept",
                                     # Experimental
                                     "50$\\%$", "60$\\%$", "70$\\%$", "80$\\%$", "Percieved Prior Benefit",
                                     "0.14 Fr./l petrol", "0.28 Fr./l petrol", "0.42 Fr./l petrol", "0.56 Fr./l petrol",
                                     "0.16 Fr./l heating oil", "0.31 Fr./l heating oil", "0.47 Fr./l heating oil", "0.63 Fr./l heating oil",
                                     "0.77 Fr./kg meat", "1.53 Fr./kg meat", "2.30 Fr./kg meat", "3.07 Fr./kg meat",
                                     "10 Fr. for short- and 30 Fr. for long-distance", "25 Fr. for short- and 75 Fr. for long-distance", "40 Fr. for short- and 120 Fr. for long-distance", "55 Fr. for short- and 165 Fr. for long-distance",
                                     "Mostly reimbursement", "Reimbursement und climate protection", "Mostly climate protection", "Exclusively climate protection",
                                     # EVs
                                     
                                     # CTRLs
                                     "Driver", "Home Owner", "Age", "Education", "French", "Primary Employment Sector", "Secondary Employment Sector", "Tertiary Employment Sector", "Financial Condition", "Left-Right",
                                     "Salience: Globalisation", "Salience: Environment and Climate",
                                     "Intermediate Area", "Rural Area",
                                     # "Preference: Regulatory Instruments", "Preference: Voluntary Instruments", "Preference: Subsidies", "Preference: Technical Solutions",
                                     # "Belief: Effectiveness", "Belief: Efficiency", "Belief: Competitiveness", "Belief: Justice", "Belief: Transformation"
                                     
                                     # 1
                                     "50\\% $\\times$ Prior Benefit", "60\\% $\\times$ Prior Benefit", "70\\% $\\times$ Prior Benefit", "80\\% $\\times$ Prior Benefit",
                                     "0.14 Fr./l petrol $\\times$ Prior Benefit", "0.28 Fr./l petrol $\\times$ Prior Benefit", "0.42 Fr./l petrol $\\times$ Prior Benefit", "0.56 Fr./l petrol $\\times$ Prior Benefit",
                                     "0.16 Fr./l heating oil $\\times$ Prior Benefit", "0.31 Fr./l heating oil $\\times$ Prior Benefit", "0.47 Fr./l heating oil $\\times$ Prior Benefit", "0.63 Fr./l heating oil $\\times$ Prior Benefit",
                                     "0.77 Fr./kg meat $\\times$ Prior Benefit", "1.53 Fr./kg meat $\\times$ Prior Benefit", "2.30 Fr./kg meat $\\times$ Prior Benefit", "3.07 Fr./kg meat $\\times$ Prior Benefit",
                                     "10 Fr. for short- and 30 Fr. for long-distance $\\times$ Prior Benefit", "25 Fr. for short- and 75 Fr. for long-distance $\\times$ Prior Benefit", "40 Fr. for short- and 120 Fr. for long-distance$\\times$ Prior Benefit", "55 Fr. for short- and 165 Fr. for long-distance$\\times$ Prior Benefit",
                                     "Mostly reimbursement $\\times$ Prior Benefit", "Reimbursement and climate protection $\\times$ Prior Benefit", "Mostly climate protection $\\times$ Prior Benefit", "Exclusively climate protection $\\times$ Prior Benefit",
                                     
                                     "EV Charging Stations",
                                     # 2
                                     "50\\% $\\times$ EV Stations", "60\\% $\\times$ EV Stations", "70\\% $\\times$ EV Stations", "80\\% $\\times$ EV Stations",
                                     "0.14 Fr./l petrol $\\times$ EV Stations", "0.28 Fr./l petrol $\\times$ EV Stations", "0.42 Fr./l petrol $\\times$ EV Stations", "0.56 Fr./l petrol $\\times$ EV Stations",
                                     "0.16 Fr./l heating oil $\\times$ EV Stations", "0.31 Fr./l heating oil $\\times$ EV Stations", "0.47 Fr./l heating oil $\\times$ EV Stations", "0.63 Fr./l heating oil $\\times$ EV Stations",
                                     "0.77 Fr./kg meat $\\times$ EV Stations", "1.53 Fr./kg meat $\\times$ EV Stations", "2.30 Fr./kg meat $\\times$ EV Stations", "3.07 Fr./kg meat $\\times$ EV Stations",
                                     "10 Fr. for short- and 30 Fr. for long-distance $\\times$ EV Stations", "25 Fr. for short- and 75 Fr. for long-distance $\\times$ EV Stations", "40 Fr. for short- and 120 Fr. for long-distance$\\times$ EV Stations", "55 Fr. for short- and 165 Fr. for long-distance$\\times$ EV Stations",
                                     "Mostly reimbursement $\\times$ EV Stations", "Reimbursement and climate protection $\\times$ EV Stations", "Mostly climate protection $\\times$ EV Stations", "Exclusively climate protection $\\times$ EV Stations"
                                     
               ),
               groups = list("Experimental: Reduction Target" = 2:5, "Experimental: Tax Road Transport" = 6:9, "Experimental: Tax Housing" = 10:13,
                             "Experimental: Tax Food" = 14:17, "Experimental: Tax Aviation Transport" = 18:21, "Experimental: Revenue Use" = 22:25,
                             "Explanatory Variables" = 26:27, "Controls" = 28:41,
                             "Interactions: Prior Benefit and Experimental Variables" = 42:64, "Interactions: EV Stations and Experimental Variables" = 66:89),
               custom.gof.rows = list("Region Controls" = c("Yes", "Yes"),
                                      "Observations" = lapply(list(model1.1, model1.2), nobs),
                                      "R^2" = sapply(lapply(list(model1.1, model1.2), summ), attr, "rsq"),
                                      "Adj. R^2" = sapply(lapply(list(model1.1, model1.2), summ), attr, "arsq")
               ),
               include.deviance = F,
               label = "table:weighted_interactions_exp_factor",
               file = "Tables/weighted_interactions_exp_factor.tex",
               use.packages = F, 
               caption = "Survey-weighted generalised linear model with interaction effects using the rate outcome. Conjoint attributes are operationalised as ordered factor levels. Normalisation: continous variables are normalised by two times 
               the standard error to make them comparable to the estimates of binary variables following Gelman (2007)"
)



library(emmeans)
emm_options(rg.limit = 5.67e+08)
x_lab <- "Tax: Road Transport"
labs_legend <- c("no influence at all", "strong influence")
vals_legend <- c("red4", "darkgreen")
legend_title <- "Perceived Prior Benefit"
x_ticks <- c("No Tax", "0.14 Fr./l petrol", "0.28 Fr./l petrol", "0.42 Fr./l petrol", "0.56 Fr./l petrol")
means_dat <- emmeans(model1.1, "attrib2_lab", by = "prior_benefit_2",  cov.keep = c("prior_benefit_2", "attrib2_lab")) 
p_prior_benefit_tax_road_fact <- means_dat %>% 
  as.data.frame(.) %>% 
  filter(prior_benefit_2 %in% c(head(prior_benefit_2, n = 1), tail(prior_benefit_2, n = 1))) %>% 
  mutate(prior_benefit_2 = as.character(prior_benefit_2),
         # attrib2_lab = factor(attrib2_lab, levels = sort(unique(attrib2_lab)))
         ) %>% 
  ggplot(., aes(x = attrib2_lab, y = emmean, group = prior_benefit_2, col = prior_benefit_2)) +
  geom_line() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL, col = prior_benefit_2), width = .1, position = position_dodge(.1)) +
  scale_x_discrete(labels = x_ticks) +
  scale_color_manual(legend_title, values=vals_legend, labels = labs_legend) + 
  scale_fill_manual(legend_title, values=vals_legend, labels = labs_legend) + 
  guides(col = guide_legend(nrow = 2)) +
  labs(
    title = "Perceived Prior Benefit",
    x = x_lab,
    y = "Policy Support"
  ) + 
  # ylim(.3, .7) +
  theme_light() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=12),
        axis.text.x = element_text(angle = 45, hjust = 1))
p_prior_benefit_tax_road_fact
ggsave(p_prior_benefit_tax_road_fact, filename = "Plots/p_prior_benefit_tax_road_fact.pdf", height = 5, width = 10)

x_lab <- "Tax: Road Transport"
labs_legend <- c("0", "2")
vals_legend <- c("red4", "darkgreen")
legend_title <- "EV Chargin Stations"
x_ticks <- c("No Tax", "0.14 Fr./l petrol", "0.28 Fr./l petrol", "0.42 Fr./l petrol", "0.56 Fr./l petrol")
means_dat <- emmeans(model1.2, "attrib2_lab", by = "ratio_ev_to_muni_area",  cov.keep = c("ratio_ev_to_muni_area", "attrib2_lab")) 
p_EV_tax_road_fact <- means_dat %>% 
  as.data.frame(.) %>% 
  filter(ratio_ev_to_muni_area %in% c(head(ratio_ev_to_muni_area, n = 1), tail(ratio_ev_to_muni_area, n = 1))) %>% 
  mutate(ratio_ev_to_muni_area = as.character(ratio_ev_to_muni_area),
         # attrib2_lab = factor(attrib2_lab, levels = sort(unique(attrib2_lab)))
  ) %>% 
  ggplot(., aes(x = attrib2_lab, y = emmean, group = ratio_ev_to_muni_area, col = ratio_ev_to_muni_area)) +
  geom_line() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL, col = ratio_ev_to_muni_area), width = .1, position = position_dodge(.1)) +
  scale_x_discrete(labels = x_ticks) +
  scale_color_manual(legend_title, values=vals_legend, labels = labs_legend) + 
  scale_fill_manual(legend_title, values=vals_legend, labels = labs_legend) + 
  guides(col = guide_legend(nrow = 2)) +
  labs(
    title = "EV Charging Stations",
    x = x_lab,
    y = "Policy Support"
  ) + 
  # ylim(.3, .7) +
  theme_light() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=12),
        axis.text.x = element_text(angle = 45, hjust = 1))
p_EV_tax_road_fact
ggsave(p_EV_tax_road_fact, filename = "Plots/p_EV_tax_road_fact.pdf", height = 5, width = 10)


##### interactions choice
model1.1 <- svyglm(choice ~ 
                     attrib1_lab*prior_benefit_2 + attrib2_lab*prior_benefit_2 +  attrib3_lab*prior_benefit_2 +  attrib4_lab*prior_benefit_2 + attrib5_lab*prior_benefit_2 + attrib6_lab*prior_benefit_2 +
                     + prior_benefit_2 + driver + home_owner + age + educ + language + empl_sect + fin_cond + left_right  + sal_glob + sal_env + region 
                   + urban_rural
                   , data = reg_dat, weights = weight, design = design)
model1.2 <- svyglm(choice ~ 
                     attrib1_lab*sqrt(ratio_ev_to_muni_area) + attrib2_lab*sqrt(ratio_ev_to_muni_area) +  attrib3_lab*sqrt(ratio_ev_to_muni_area) +  attrib4_lab*sqrt(ratio_ev_to_muni_area) + attrib5_lab*sqrt(ratio_ev_to_muni_area) + attrib6_lab*sqrt(ratio_ev_to_muni_area) +
                     sqrt(ratio_ev_to_muni_area) + driver + home_owner + age + educ + language + empl_sect + fin_cond + left_right  + sal_glob + sal_env + region 
                   + urban_rural
                   , data = reg_dat, weights = weight, design = design)

texreg::texreg(list(model1.1, model1.2), digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
               fontsize = "tiny", longtable = T, no.margin = T,
               reorder.coef = c(1:5, 7:26, 6, 65, 27:41, 42:64, 66:89),
               omit.coef = "region",
               custom.coef.names = c("Intercept",
                                     # Experimental
                                     "50$\\%$", "60$\\%$", "70$\\%$", "80$\\%$", "Percieved Prior Benefit",
                                     "0.14 Fr./l petrol", "0.28 Fr./l petrol", "0.42 Fr./l petrol", "0.56 Fr./l petrol",
                                     "0.16 Fr./l heating oil", "0.31 Fr./l heating oil", "0.47 Fr./l heating oil", "0.63 Fr./l heating oil",
                                     "0.77 Fr./kg meat", "1.53 Fr./kg meat", "2.30 Fr./kg meat", "3.07 Fr./kg meat",
                                     "10 Fr. for short- and 30 Fr. for long-distance", "25 Fr. for short- and 75 Fr. for long-distance", "40 Fr. for short- and 120 Fr. for long-distance", "55 Fr. for short- and 165 Fr. for long-distance",
                                     "Mostly reimbursement", "Reimbursement und climate protection", "Mostly climate protection", "Exclusively climate protection",
                                     # EVs
                                     
                                     # CTRLs
                                     "Driver", "Home Owner", "Age", "Education", "French", "Primary Employment Sector", "Secondary Employment Sector", "Tertiary Employment Sector", "Financial Condition", "Left-Right",
                                     "Salience: Globalisation", "Salience: Environment and Climate",
                                     "Intermediate Area", "Rural Area",
                                     # "Preference: Regulatory Instruments", "Preference: Voluntary Instruments", "Preference: Subsidies", "Preference: Technical Solutions",
                                     # "Belief: Effectiveness", "Belief: Efficiency", "Belief: Competitiveness", "Belief: Justice", "Belief: Transformation"
                                     
                                     # 1
                                     "50\\% $\\times$ Prior Benefit", "60\\% $\\times$ Prior Benefit", "70\\% $\\times$ Prior Benefit", "80\\% $\\times$ Prior Benefit",
                                     "0.14 Fr./l petrol $\\times$ Prior Benefit", "0.28 Fr./l petrol $\\times$ Prior Benefit", "0.42 Fr./l petrol $\\times$ Prior Benefit", "0.56 Fr./l petrol $\\times$ Prior Benefit",
                                     "0.16 Fr./l heating oil $\\times$ Prior Benefit", "0.31 Fr./l heating oil $\\times$ Prior Benefit", "0.47 Fr./l heating oil $\\times$ Prior Benefit", "0.63 Fr./l heating oil $\\times$ Prior Benefit",
                                     "0.77 Fr./kg meat $\\times$ Prior Benefit", "1.53 Fr./kg meat $\\times$ Prior Benefit", "2.30 Fr./kg meat $\\times$ Prior Benefit", "3.07 Fr./kg meat $\\times$ Prior Benefit",
                                     "10 Fr. for short- and 30 Fr. for long-distance $\\times$ Prior Benefit", "25 Fr. for short- and 75 Fr. for long-distance $\\times$ Prior Benefit", "40 Fr. for short- and 120 Fr. for long-distance$\\times$ Prior Benefit", "55 Fr. for short- and 165 Fr. for long-distance$\\times$ Prior Benefit",
                                     "Mostly reimbursement $\\times$ Prior Benefit", "Reimbursement and climate protection $\\times$ Prior Benefit", "Mostly climate protection $\\times$ Prior Benefit", "Exclusively climate protection $\\times$ Prior Benefit",
                                     
                                     "EV Charging Stations",
                                     # 2
                                     "50\\% $\\times$ EV Stations", "60\\% $\\times$ EV Stations", "70\\% $\\times$ EV Stations", "80\\% $\\times$ EV Stations",
                                     "0.14 Fr./l petrol $\\times$ EV Stations", "0.28 Fr./l petrol $\\times$ EV Stations", "0.42 Fr./l petrol $\\times$ EV Stations", "0.56 Fr./l petrol $\\times$ EV Stations",
                                     "0.16 Fr./l heating oil $\\times$ EV Stations", "0.31 Fr./l heating oil $\\times$ EV Stations", "0.47 Fr./l heating oil $\\times$ EV Stations", "0.63 Fr./l heating oil $\\times$ EV Stations",
                                     "0.77 Fr./kg meat $\\times$ EV Stations", "1.53 Fr./kg meat $\\times$ EV Stations", "2.30 Fr./kg meat $\\times$ EV Stations", "3.07 Fr./kg meat $\\times$ EV Stations",
                                     "10 Fr. for short- and 30 Fr. for long-distance $\\times$ EV Stations", "25 Fr. for short- and 75 Fr. for long-distance $\\times$ EV Stations", "40 Fr. for short- and 120 Fr. for long-distance$\\times$ EV Stations", "55 Fr. for short- and 165 Fr. for long-distance$\\times$ EV Stations",
                                     "Mostly reimbursement $\\times$ EV Stations", "Reimbursement and climate protection $\\times$ EV Stations", "Mostly climate protection $\\times$ EV Stations", "Exclusively climate protection $\\times$ EV Stations"
                                     
               ),
               groups = list("Experimental: Reduction Target" = 2:5, "Experimental: Tax Road Transport" = 6:9, "Experimental: Tax Housing" = 10:13,
                             "Experimental: Tax Food" = 14:17, "Experimental: Tax Aviation Transport" = 18:21, "Experimental: Revenue Use" = 22:25,
                             "Explanatory Variables" = 26:27, "Controls" = 28:41,
                             "Interactions: Prior Benefit and Experimental Variables" = 42:65, "Interactions: EV Stations and Experimental Variables" = 66:89),
               custom.gof.rows = list("Region Controls" = c("Yes", "Yes"),
                                      "Observations" = lapply(list(model1.1, model1.2), nobs),
                                      "R^2" = sapply(lapply(list(model1.1, model1.2), summ), attr, "rsq"),
                                      "Adj. R^2" = sapply(lapply(list(model1.1, model1.2), summ), attr, "arsq")
               ),
               include.deviance = F,
               label = "table:weighted_interactions_exp_factor_choice",
               file = "Tables/weighted_interactions_exp_factor_choice.tex",
               use.packages = F, 
               caption = "Survey-weighted generalised linear model with interaction effects using the choice outcome. Conjoint attributes are operationalised as ordered factor levels. Normalisation: continous variables are normalised by two times 
               the standard error to make them comparable to the estimates of binary variables following Gelman (2007)"
)

#----------------------------------------------------#
# operationalisation: expvars as continuous
#----------------------------------------------------#

reg_dat <- dat2 %>%
  dplyr::select(outcomes, expvars, covars, weight, id) %>%
  mutate_at(vars(covars), as.character) %>% 
  mutate_at(vars(outcomes, covars, weight, expvars), as.numeric) %>% 
  mutate_at(vars(empl_sect, urban_rural, region), as.factor)

reg_dat[c(vars_to_transform, expvars)] <- lapply(reg_dat[c(vars_to_transform, expvars)], sdFunc)

# design object for new data 
design <- svydesign(~1 , data = reg_dat, weights=reg_dat$weight)

## 
model1.1 <- svyglm(rate ~ attrib1_lab + attrib2_lab +  attrib3_lab +  attrib4_lab + attrib5_lab + attrib6_lab, data = reg_dat, weights = weight, design = design)
model1.2 <- svyglm(rate ~ attrib1_lab + attrib2_lab +  attrib3_lab +  attrib4_lab + attrib5_lab + attrib6_lab + 
                     + prior_benefit_2, data = reg_dat, weights = weight, design = design)
model1.3 <- svyglm(rate ~ attrib1_lab + attrib2_lab +  attrib3_lab +  attrib4_lab + attrib5_lab + attrib6_lab + 
                     sqrt(ratio_ev_to_muni_area), data = reg_dat, weights = weight, design = design)
model1.4 <- svyglm(rate ~ attrib1_lab + attrib2_lab +  attrib3_lab +  attrib4_lab + attrib5_lab + attrib6_lab + 
                     + prior_benefit_2 + driver + home_owner + age + educ + language + empl_sect + fin_cond + left_right  + sal_glob + sal_env + region 
                   + urban_rural, data = reg_dat, weights = weight, design = design)
model1.5 <- svyglm(rate ~ attrib1_lab + attrib2_lab +  attrib3_lab +  attrib4_lab + attrib5_lab + attrib6_lab + 
                     sqrt(ratio_ev_to_muni_area) + driver + home_owner + age + educ + language + empl_sect + fin_cond + left_right  + sal_glob + sal_env + region
                   + urban_rural, data = reg_dat, weights = weight, design = design)
texreg::texreg(list(model1.1, model1.2, model1.3, model1.4, model1.5), digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
               fontsize = "tiny", longtable = T, no.margin = T,
               omit.coef = "region",
               custom.coef.names = c("Intercept", "Reduction Target", "Tax Road Transport", "Tax Housing", "Tax Food", "Tax Aviation Transport", "Revenue Use",
                                     # EVs
                                     "Percieved Prior Benefit", "EV Charging Stations",
                                     # CTRLs
                                     "Driver", "Home Owner", "Age", "Education", "French", "Primary Employment Sector", "Secondary Employment Sector", "Tertiary Employment Sector", "Financial Condition", "Left-Right",
                                     "Salience: Globalisation", "Salience: Environment and Climate",
                                     "Intermediate Area", "Rural Area"
                                     ),
               groups = list("Experimental" = 2:7, "Explanatory Variables" = 8:9, "Controls" = 10:23),
               custom.gof.rows = list("Region Controls" = c("No", "No", "No", "Yes", "Yes"),
                                      "Observations" = lapply(list(model1.1, model1.2, model1.3, model1.4, model1.5), nobs),
                                      "R^2" = sapply(lapply(list(model1.1, model1.2, model1.3, model1.4, model1.5), summ), attr, "rsq"),
                                      "Adj. R^2" = sapply(lapply(list(model1.1, model1.2, model1.3, model1.4, model1.5), summ), attr, "arsq")
               ),
               include.deviance = F,
               label = "table:weighted_direct_exp_continous",
               file = "Tables/weighted_direct_exp_continous.tex",
               use.packages = F, 
               caption = "Survey-weighted generalised linear model with direct effects using the rate outcome. Conjoint attributes are operationalised as continuous variables. Normalisation: continuous variables are normalised by two times 
               the standard error to make them comparable to the estimates of binary variables following Gelman (2007)")

##### interactions rate
model1.1 <- svyglm(rate ~ attrib1_lab*prior_benefit_2 + attrib2_lab*prior_benefit_2 +  attrib3_lab*prior_benefit_2 +  attrib4_lab*prior_benefit_2 + attrib5_lab*prior_benefit_2 + attrib6_lab*prior_benefit_2 +
                     + prior_benefit_2 + driver + home_owner + age + educ + language + empl_sect + fin_cond + left_right  + sal_glob + sal_env + region + urban_rural
                   , data = reg_dat, weights = weight, design = design)
model1.2 <- svyglm(rate ~ attrib1_lab* sqrt(ratio_ev_to_muni_area) + attrib2_lab* sqrt(ratio_ev_to_muni_area) +  attrib3_lab* sqrt(ratio_ev_to_muni_area) +  attrib4_lab* sqrt(ratio_ev_to_muni_area) + attrib5_lab* sqrt(ratio_ev_to_muni_area) + attrib6_lab* sqrt(ratio_ev_to_muni_area) +
                     sqrt(ratio_ev_to_muni_area) + driver + home_owner + age + educ + language + empl_sect + fin_cond + left_right  + sal_glob + sal_env + region + urban_rural
                   , data = reg_dat, weights = weight, design = design)

texreg::texreg(list(model1.1, model1.2), digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
               fontsize = "tiny", longtable = T, no.margin = T,
               reorder.coef = c(1:2, 4:8, 3, 29, 9:19, 20:28, 30:35),
               omit.coef = "region",
               custom.coef.names = c("Intercept", "Percieved Prior Benefit", "Reduction Target", "Tax Road Transport", "Tax Housing", "Tax Food", "Tax Aviation Transport", "Revenue Use",
                                     # CTRLs
                                     "Driver", "Home Owner", "Age", "Education", "French", "Primary Employment Sector", "Secondary Employment Sector", "Tertiary Employment Sector", "Financial Condition", "Left-Right",
                                     "Salience: Globalisation", "Salience: Environment and Climate",
                                     "Intermediate Area", "Rural Area",
                                     # Interactions
                                     "Reduction Target $\\times$ Prior Benefit", "Road Transport $\\times$ Prior Benefit", "Housing $\\times$ Prior Benefit", "Food $\\times$ Prior Benefit", "Aviation Transport $\\times$ Prior Benefit", "Revenue Use $\\times$ Prior Benefit",
                                     "EV Charging Stations", "Reduction Target $\\times$ EV Charging Stations", "Road Transport $\\times$ EV Charging Stations", "Housing $\\times$ EV Charging Stations", "Food $\\times$ EV Charging Stations", "Aviation Transport $\\times$ EV Charging Stations", "Revenue Use $\\times$ EV Charging Stations"
                                     ),
               groups = list("Experimental" = 2:7, "Explanatory Variables" = 8:9, "Controls" = 10:21,
                             "Prior Benefit and Experimental" = 24:29, "EV Stations and Experimental" = 30:35
                             ),
               custom.gof.rows = list("Region Controls" = c("Yes", "Yes"),
                                      "Observations" = lapply(list(model1.1, model1.2), nobs),
                                      "R^2" = sapply(lapply(list(model1.1, model1.2), summ), attr, "rsq"),
                                      "Adj. R^2" = sapply(lapply(list(model1.1, model1.2), summ), attr, "arsq")
               ),
               include.deviance = F,
               label = "table:weighted_interactions_exp_continous",
               file = "Tables/weighted_interactions_exp_continous.tex",
               use.packages = F, 
               caption = "Survey-weighted generalised linear model with interaction effects using the rate outcome. Conjoint attributes are operationalised as continuous variables. Normalisation: continuous variables are normalised by two times 
               the standard error to make them comparable to the estimates of binary variables following Gelman (2007)")

##### interactions choice
model1.1 <- svyglm(choice ~ 
                     attrib1_lab*prior_benefit_2 + attrib2_lab*prior_benefit_2 +  attrib3_lab*prior_benefit_2 +  attrib4_lab*prior_benefit_2 + attrib5_lab*prior_benefit_2 + attrib6_lab*prior_benefit_2 +
                     + prior_benefit_2 + driver + home_owner + age + educ + language + empl_sect + fin_cond + left_right  + sal_glob + sal_env + region + urban_rural
                   , data = reg_dat, weights = weight, design = design)
model1.2 <- svyglm(choice ~ 
                     attrib1_lab* sqrt(ratio_ev_to_muni_area) + attrib2_lab* sqrt(ratio_ev_to_muni_area) +  attrib3_lab* sqrt(ratio_ev_to_muni_area) +  attrib4_lab* sqrt(ratio_ev_to_muni_area) + attrib5_lab* sqrt(ratio_ev_to_muni_area) + attrib6_lab* sqrt(ratio_ev_to_muni_area) +
                     sqrt(ratio_ev_to_muni_area) + driver + home_owner + age + educ + language + empl_sect + fin_cond + left_right  + sal_glob + sal_env + region + urban_rural
                   , data = reg_dat, weights = weight, design = design)

texreg::texreg(list(model1.1, model1.2), digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
               fontsize = "tiny", longtable = T, no.margin = T,
               reorder.coef = c(1:2, 4:8, 3, 29, 9:19, 20:28, 30:35),
               omit.coef = "region",
               custom.coef.names = c("Intercept", "Percieved Prior Benefit", "Reduction Target", "Tax Road Transport", "Tax Housing", "Tax Food", "Tax Aviation Transport", "Revenue Use",
                                     
                                     # CTRLs
                                     "Driver", "Home Owner", "Age", "Education", "French", "Primary Employment Sector", "Secondary Employment Sector", "Tertiary Employment Sector", "Financial Condition", "Left-Right",
                                     "Salience: Globalisation", "Salience: Environment and Climate",
                                     "Intermediate Area", "Rural Area",
                                     # "Preference: Regulatory Instruments", "Preference: Voluntary Instruments", "Preference: Subsidies", "Preference: Technical Solutions",
                                     # "Belief: Effectiveness", "Belief: Efficiency", "Belief: Competitiveness", "Belief: Justice", "Belief: Transformation"
                                     
                                     # Interactions
                                     "Reduction Target $\\times$ Prior Benefit", "Road Transport $\\times$ Prior Benefit", "Housing $\\times$ Prior Benefit", "Food $\\times$ Prior Benefit", "Aviation Transport $\\times$ Prior Benefit", "Revenue Use $\\times$ Prior Benefit",
                                     "EV Charging Stations", "Reduction Target $\\times$ EV Charging Stations", "Road Transport $\\times$ EV Charging Stations", "Housing $\\times$ EV Charging Stations", "Food $\\times$ EV Charging Stations", "Aviation Transport $\\times$ EV Charging Stations", "Revenue Use $\\times$ EV Charging Stations"
               ),
               groups = list("Experimental" = 2:7, "Explanatory Variables" = 8:9, "Controls" = 10:21,
                             "Prior Benefit and Experimental" = 24:29, "EV Stations and Experimental" = 30:35
               ),
               custom.gof.rows = list("Region Controls" = c("Yes", "Yes"),
                                      "Observations" = lapply(list(model1.1, model1.2), nobs),
                                      "R^2" = sapply(lapply(list(model1.1, model1.2), summ), attr, "rsq"),
                                      "Adj. R^2" = sapply(lapply(list(model1.1, model1.2), summ), attr, "arsq")
               ),
               include.deviance = F,
               label = "table:weighted_interactions_exp_continous_choice",
               file = "Tables/weighted_interactions_exp_continous_choice.tex",
               use.packages = F,
               caption = "Survey-weighted generalised linear model with interaction effects using the choice outcome. Conjoint attributes are operationalised as continuous variables. Normalisation: continuous variables are normalised by two times 
               the standard error to make them comparable to the estimates of binary variables following Gelman (2007)")


x_lab <- "Tax: Road Transport"
labs_legend <- c("no influence at all", "strong influence")
vals_legend <- c("red4", "darkgreen")
legend_title <- "Perceived Prior Benefit"
x_ticks <- c("No Tax", "0.14 Fr./l petrol", "0.28 Fr./l petrol", "0.42 Fr./l petrol", "0.56 Fr./l petrol")
p_prior_benefit_tax_road <- emmeans(model1.1, "attrib2_lab", by = "prior_benefit_2",  cov.keep = c("prior_benefit_2", "attrib2_lab")) %>% 
  as.data.frame(.) %>% 
  filter(prior_benefit_2 %in% c(head(prior_benefit_2, n = 1), tail(prior_benefit_2, n = 1))) %>% 
  mutate(prior_benefit_2 = as.character(prior_benefit_2),
         attrib2_lab = factor(attrib2_lab, levels = sort(unique(attrib2_lab)))) %>% 
  ggplot(., aes(x = attrib2_lab, y = emmean, group = prior_benefit_2, col = prior_benefit_2)) +
  geom_line() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL, col = prior_benefit_2), width = .1, position = position_dodge(.1)) +
  scale_x_discrete(labels = x_ticks) +
  scale_color_manual(legend_title, values=vals_legend, labels = labs_legend) + 
  scale_fill_manual(legend_title, values=vals_legend, labels = labs_legend) + 
  guides(col = guide_legend(nrow = 2)) +
  labs(
    title = "Perceived Prior Benefit",
    x = x_lab,
    y = "Policy Support"
  ) + 
  ylim(.3, .7) +
  theme_light() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=12),
        axis.text.x = element_text(angle = 45, hjust = 1))

emm_options(rg.limit = 181443)
x_lab <- "Tax: Road Transport"
labs_legend <- c("0", "2")
vals_legend <- c("red4", "darkgreen")
legend_title <- "EV Chargin Stations"
x_ticks <- c("No Tax", "0.14 Fr./l petrol", "0.28 Fr./l petrol", "0.42 Fr./l petrol", "0.56 Fr./l petrol")
p_EV_tax_road <- emmeans(model1.2, "attrib2_lab", by = "ratio_ev_to_muni_area",  cov.keep = c("ratio_ev_to_muni_area", "attrib2_lab")) %>% 
  as.data.frame(.) %>% 
  filter(ratio_ev_to_muni_area == 0 | (ratio_ev_to_muni_area > 1.99 & ratio_ev_to_muni_area < 2)) %>% 
  mutate(ratio_ev_to_muni_area = as.character(ratio_ev_to_muni_area),
         attrib2_lab = factor(attrib2_lab, levels = sort(unique(attrib2_lab)))) %>% 
  ggplot(., aes(x = attrib2_lab, y = emmean, group = ratio_ev_to_muni_area, col = ratio_ev_to_muni_area)) +
  geom_line() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL, col = ratio_ev_to_muni_area), width = .1, position = position_dodge(.1)) +
  scale_x_discrete(labels = x_ticks) +
  scale_color_manual(legend_title, values=vals_legend, labels = labs_legend) + 
  scale_fill_manual(legend_title, values=vals_legend, labels = labs_legend) + 
  guides(col = guide_legend(nrow = 2)) +
  labs(
    title = "EV Charging Stations",
    x = x_lab,
    y = "Policy Support"
  ) + 
  ylim(.3, .7) +
  theme_light() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=12),
        axis.text.x = element_text(angle = 45, hjust = 1))


ggsave(p_prior_benefit_tax_road, filename = "Plots/p_prior_benefit_tax_road.pdf", height = 7, width = 10)
ggsave(p_EV_tax_road, filename = "Plots/p_EV_tax_road.pdf", height = 7, width = 10)

marginals_arranged <- ggarrange(p_prior_benefit_tax_road, p_EV_tax_road, nrow=1, common.legend = FALSE)
marginals_arranged
ggsave(marginals_arranged, filename = "Plots/marginals_arranged.pdf", height = 5, width = 15)

##### interactions with covariates, clean up
model1.1 <- svyglm(rate ~ attrib1_lab + attrib2_lab +  attrib3_lab +  attrib4_lab + attrib5_lab + attrib6_lab +
                     + prior_benefit_2 + driver + home_owner + age + educ + language + empl_sect + fin_cond + left_right*prior_benefit_2 + sal_env*prior_benefit_2 + sal_glob + sal_env + region 
                   + urban_rural
                   , data = reg_dat, weights = weight, design = design)
model1.2 <- svyglm(rate ~ attrib1_lab + attrib2_lab +  attrib3_lab +  attrib4_lab + attrib5_lab + attrib6_lab +
                     + prior_benefit_2 + driver + home_owner + age + educ + language + empl_sect + fin_cond + left_right*ratio_ev_to_muni_area + sal_env*ratio_ev_to_muni_area  + sal_glob + sal_env + region 
                   + urban_rural
                   , data = reg_dat, weights = weight, design = design)

texreg::texreg(list(model1.1, model1.2), digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
               fontsize = "tiny", longtable = T, no.margin = T,
               # reorder.coef = c(1:5, 6:8, 9:28),
               omit.coef = "region",
               # custom.coef.names = c("Intercept", "Percieved Prior Benefit", "Reduction Target", "Tax Road Transport", "Tax Housing", "Tax Food", "Tax Aviation Transport", "Revenue Use",
               # 
               #                       # CTRLs
               #                       "Driver", "Home Owner", "Age", "Education", "French", "Primary Employment Sector", "Secondary Employment Sector", "Tertiary Employment Sector", "Financial Condition", "Left-Right",
               #                       "Salience: Globalisation", "Salience: Environment and Climate",
               #                       "Intermediate Area", "Rural Area"
               #                       # "Preference: Regulatory Instruments", "Preference: Voluntary Instruments", "Preference: Subsidies", "Preference: Technical Solutions",
               #                       # "Belief: Effectiveness", "Belief: Efficiency", "Belief: Competitiveness", "Belief: Justice", "Belief: Transformation"
               # 
               #                       ),
               # groups = list("Experimental" = 2:7, "Explanatory Variables" = 8:10, "Controls" = 11:22,
               #               "Prior Benefit and Experimental" = 25:30, "EV Stations and Experimental" = 31:36, "Switch and Experimental" = 37:42
               # ),
               # custom.gof.rows = list("Region Controls" = c("Yes", "Yes", "Yes"),
               #                        "Observations" = lapply(list(model1.1, model1.2, model1.3), nobs),
               #                        "R^2" = sapply(lapply(list(model1.1, model1.2, model1.3), summ), attr, "rsq"),
               #                        "Adj. R^2" = sapply(lapply(list(model1.1, model1.2, model1.3), summ), attr, "arsq")
               # ),
               # include.deviance = F,
               # label = "table:weighted_interactions_exp_continous_choice",
               # caption = "Survey-weighted generalised linear model with interaction effects using the choice outcome. Conjoint attributes are operationalised as continuous variables. Normalisation: continuous variables are normalised by two times 
               # the standard error to make them comparable to the estimates of binary variables following Gelman (2007)"
               )

library(emmeans)
x_lab <- "Left-right\n(0 = left, 10 = right)"
labs_legend <- c("no influence at all", "strong influence")
vals_legend <- c("red4", "darkgreen")
legend_title <- "Perceived Prior Benefit"
x_ticks <- seq(0,10,1)
means_dat <- emmeans(model1.1, "left_right", by = "prior_benefit_2",  cov.keep = c("prior_benefit_2", "left_right"))
p_prior_benefit_left_right <- means_dat %>% 
  as.data.frame(.) %>% 
  filter(prior_benefit_2 %in% c(head(prior_benefit_2, n = 1), tail(prior_benefit_2, n = 1))) %>% 
  mutate(prior_benefit_2 = as.character(prior_benefit_2),
         left_right = factor(left_right, levels = sort(unique(left_right)))) %>% 
  ggplot(., aes(x = left_right, y = emmean, group = prior_benefit_2, col = prior_benefit_2)) +
  geom_line() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL, col = prior_benefit_2), width = .1, position = position_dodge(.1)) +
  scale_x_discrete(labels = x_ticks) +
  scale_color_manual(legend_title, values=vals_legend, labels = labs_legend) + 
  scale_fill_manual(legend_title, values=vals_legend, labels = labs_legend) + 
  guides(col = guide_legend(nrow = 2)) +
  labs(
    title = "Perceived Prior Benefit and Left-Right",
    x = x_lab,
    y = "Policy Support"
  ) + 
  # ylim(.3, .7) +
  theme_light() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=12),
        axis.text.x = element_text(angle = 0, hjust = 1))
p_prior_benefit_left_right
ggsave(p_prior_benefit_left_right, filename = "Plots/p_prior_benefit_left_right.pdf", height = 7, width = 10)


x_lab <- "Issue Salience\nof Environment and Climate"
labs_legend <- c("no influence at all", "strong influence")
vals_legend <- c("red4", "darkgreen")
legend_title <- "Perceived Prior Benefit"
x_ticks <- c("not salient", "among the top\n three issues")
means_dat <- emmeans(model1.1, "sal_env", by = "prior_benefit_2",  cov.keep = c("prior_benefit_2", "sal_env"))
p_prior_benefit_sal_env <- means_dat %>% 
  as.data.frame(.) %>% 
  filter(prior_benefit_2 %in% c(head(prior_benefit_2, n = 1), tail(prior_benefit_2, n = 1))) %>% 
  mutate(prior_benefit_2 = as.character(prior_benefit_2),
         sal_env = factor(sal_env, levels = sort(unique(sal_env)))) %>% 
  ggplot(., aes(x = sal_env, y = emmean, group = prior_benefit_2, col = prior_benefit_2)) +
  geom_line() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL, col = prior_benefit_2), width = .1, position = position_dodge(.1)) +
  scale_x_discrete(labels = x_ticks) +
  scale_color_manual(legend_title, values=vals_legend, labels = labs_legend) + 
  scale_fill_manual(legend_title, values=vals_legend, labels = labs_legend) + 
  guides(col = guide_legend(nrow = 2)) +
  labs(
    title = "Perceived Prior Benefit and Left-Right",
    x = x_lab,
    y = "Policy Support"
  ) + 
  # ylim(.3, .7) +
  theme_light() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=12),
        axis.text.x = element_text(angle = 45, hjust = 1))
p_prior_benefit_sal_env
ggsave(p_prior_benefit_sal_env, filename = "Plots/p_prior_benefit_sal_env.pdf", height = 7, width = 10)


# plot_model(model1.3, type = "pred", terms = c("sal_env", "ratio_ev_to_muni_area"), vcov.type = "HC1", robust = T) 
x_lab <- "Issue Salience\nof Environment and Climate"
labs_legend <- c("0", "2")
vals_legend <- c("red4", "darkgreen")
legend_title <- "EV Chargin Stations"
x_ticks <- c("not salient", "among the top\n three issues")
means_dat <- emmeans(model1.2, "sal_env", by = "ratio_ev_to_muni_area",  cov.keep = c("ratio_ev_to_muni_area", "sal_env"))
p_ratio_ev_to_muni_area_sal_env <- means_dat %>% 
  as.data.frame(.) %>% 
  filter(ratio_ev_to_muni_area == 0 | (ratio_ev_to_muni_area > 1.95 & ratio_ev_to_muni_area < 2)) %>%
  mutate(ratio_ev_to_muni_area = as.character(ratio_ev_to_muni_area),
         sal_env = factor(sal_env, levels = sort(unique(sal_env)))) %>% 
  ggplot(., aes(x = sal_env, y = emmean, group = ratio_ev_to_muni_area, col = ratio_ev_to_muni_area)) +
  geom_line() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL, col = ratio_ev_to_muni_area), width = .1, position = position_dodge(.1)) +
  scale_x_discrete(labels = x_ticks) +
  scale_color_manual(legend_title, values=vals_legend, labels = labs_legend) + 
  scale_fill_manual(legend_title, values=vals_legend, labels = labs_legend) + 
  guides(col = guide_legend(nrow = 2)) +
  labs(
    title = "EV Charging Stations and Salience: Environment and Climate",
    x = x_lab,
    y = "Policy Support"
  ) + 
  # ylim(.3, .7) +
  theme_light() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=12),
        axis.text.x = element_text(angle = 45, hjust = 1))
p_ratio_ev_to_muni_area_sal_env
ggsave(p_ratio_ev_to_muni_area_sal_env, filename = "Plots/p_ratio_ev_to_muni_area_sal_env.pdf", height = 7, width = 10)

x_lab <- "Left-right\n(0 = left, 10 = right)"
labs_legend <- c("0", "2")
vals_legend <- c("red4", "darkgreen")
legend_title <- "EV Chargin Stations"
x_ticks <- seq(0,10,1)
means_dat <- emmeans(model1.1, "left_right", by = "ratio_ev_to_muni_area",  cov.keep = c("ratio_ev_to_muni_area", "left_right"))
p_ratio_ev_to_muni_area_left_right <- means_dat %>% 
  as.data.frame(.) %>% 
  filter(ratio_ev_to_muni_area == 0 | (ratio_ev_to_muni_area > 1.95 & ratio_ev_to_muni_area < 2)) %>%
  mutate(ratio_ev_to_muni_area = as.character(ratio_ev_to_muni_area),
         left_right = factor(left_right, levels = sort(unique(left_right)))) %>% 
  ggplot(., aes(x = left_right, y = emmean, group = ratio_ev_to_muni_area, col = ratio_ev_to_muni_area)) +
  geom_line() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL, col = ratio_ev_to_muni_area), width = .1, position = position_dodge(.1)) +
  scale_x_discrete(labels = x_ticks) +
  scale_color_manual(legend_title, values=vals_legend, labels = labs_legend) + 
  scale_fill_manual(legend_title, values=vals_legend, labels = labs_legend) + 
  guides(col = guide_legend(nrow = 2)) +
  labs(
    title = "Left-Right and EV Charging Stations",
    x = x_lab,
    y = "Policy Support"
  ) + 
  # ylim(.3, .7) +
  theme_light() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=12),
        axis.text.x = element_text(angle = 0, hjust = 1))
p_ratio_ev_to_muni_area_left_right
ggsave(p_ratio_ev_to_muni_area_left_right, filename = "Plots/p_ratio_ev_to_muni_area_left_right.pdf", height = 7, width = 10)

######################################################
# linear regression analysis
######################################################

#----------------------------------------------------#
# operationalisation: expvars as factors
#----------------------------------------------------#

reg_dat <- dat %>%
  dplyr::select(outcomes, expvars, covars, id) %>%
  mutate_at(vars(covars), as.character) %>% 
  mutate_at(vars(outcomes, covars), as.numeric) %>% 
  mutate(educ = ifelse(is.na(educ), median(educ, na.rm = T), educ),
         left_right = ifelse(is.na(left_right), median(left_right, na.rm = T), left_right),
         age = ifelse(is.na(age), median(age, na.rm = T), age),
         urban_rural = ifelse(is.na(urban_rural), median(urban_rural, na.rm = T), urban_rural),
         ratio_ev_to_muni_area = ifelse(is.na(ratio_ev_to_muni_area), median(ratio_ev_to_muni_area, na.rm = T), ratio_ev_to_muni_area),
         region = ifelse(is.na(region), median(region, na.rm = T), region),
  ) %>% 
  mutate_at(vars(empl_sect, urban_rural, region), as.factor)

reg_dat[vars_to_transform] <- lapply(reg_dat[vars_to_transform], sdFunc)

## (3) linear regression
library(sandwich)
model1.1 <- lm(rate ~ attrib1_lab + attrib2_lab +  attrib3_lab +  attrib4_lab + attrib5_lab + attrib6_lab, data = reg_dat)
model1.2 <- lm(rate ~ attrib1_lab + attrib2_lab +  attrib3_lab +  attrib4_lab + attrib5_lab + attrib6_lab + prior_benefit_2, data = reg_dat)
model1.3 <- lm(rate ~ attrib1_lab + attrib2_lab +  attrib3_lab +  attrib4_lab + attrib5_lab + attrib6_lab + sqrt(ratio_ev_to_muni_area), data = reg_dat)
model1.4 <- lm(rate ~ attrib1_lab + attrib2_lab +  attrib3_lab +  attrib4_lab + attrib5_lab + attrib6_lab + prior_benefit_2 
               + driver + home_owner + age + educ + language + empl_sect + fin_cond + left_right  + sal_glob + sal_env + region 
               + urban_rural, data = reg_dat)
model1.5 <- lm(rate ~ attrib1_lab + attrib2_lab +  attrib3_lab +  attrib4_lab + attrib5_lab + attrib6_lab + sqrt(ratio_ev_to_muni_area) 
               + driver + home_owner + age + educ + language + empl_sect + fin_cond + left_right  + sal_glob + sal_env + region 
               + urban_rural, data = reg_dat)
model1.1_r <- coeftest(model1.1, cluster = reg_dat$id,  vcov = vcovHC(model1.1, type="HC1"))
model1.2_r <- coeftest(model1.2, cluster = reg_dat$id,  vcov = vcovHC(model1.2, type="HC1"))
model1.3_r <- coeftest(model1.3, cluster = reg_dat$id,  vcov = vcovHC(model1.3, type="HC1"))
model1.4_r <- coeftest(model1.4, cluster = reg_dat$id,  vcov = vcovHC(model1.4, type="HC1"))
model1.5_r <- coeftest(model1.5, cluster = reg_dat$id,  vcov = vcovHC(model1.5, type="HC1"))
texreg::texreg(list(model1.1_r, model1.2_r, model1.3_r, model1.4_r, model1.5_r), digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
               fontsize = "tiny", longtable = T, no.margin = T,
               omit.coef = "region",
               custom.coef.names = c("Intercept",
                                     # Experimental
                                     "50$\\%$", "60$\\%$", "70$\\%$", "80$\\%$",
                                     "0.14 Fr./l petrol", "0.28 Fr./l petrol", "0.42 Fr./l petrol", "0.56 Fr./l petrol",
                                     "0.16 Fr./l heating oil", "0.31 Fr./l heating oil", "0.47 Fr./l heating oil", "0.63 Fr./l heating oil",
                                     "0.77 Fr./kg meat", "1.53 Fr./kg meat", "2.30 Fr./kg meat", "3.07 Fr./kg meat",
                                     "10 Fr. for short- and 30 Fr. for long-distance", "25 Fr. for short- and 75 Fr. for long-distance", "40 Fr. for short- and 120 Fr. for long-distance", "55 Fr. for short- and 165 Fr. for long-distance",
                                     "Mostly reimbursement", "Reimbursement und climate protection", "Mostly climate protection", "Exclusively climate protection",
                                     # EVs
                                     "Percieved Prior Benefit", "EV Charging Stations", 
                                     # CTRLs
                                     "Driver", "Home Owner", "Age", "Education", "French", "Primary Employment Sector", "Secondary Employment Sector", "Tertiary Employment Sector", "Financial Condition", "Left-Right",
                                     "Salience: Globalisation", "Salience: Environment and Climate",
                                     "Intermediate Area", "Rural Area"
                                     # "Preference: Regulatory Instruments", "Preference: Voluntary Instruments", "Preference: Subsidies", "Preference: Technical Solutions",
                                     # "Belief: Effectiveness", "Belief: Efficiency", "Belief: Competitiveness", "Belief: Justice", "Belief: Transformation"
               ),
               groups = list("Experimental: Reduction Target" = 2:5, "Experimental: Tax Road Transport" = 6:9, "Experimental: Tax Housing" = 10:13,
                             "Experimental: Tax Food" = 14:17, "Experimental: Tax Aviation Transport" = 18:21, "Experimental: Revenue Use" = 22:25,
                             "Explanatory Variables" = 26:27, "Controls" = 28:41
               ),
               custom.gof.rows = list("Region Controls" = c("No", "No", "No", "Yes", "Yes"),
                                      "Observations" = lapply(list(model1.1, model1.2, model1.3, model1.4, model1.5), nobs),
                                      "R^2" = unlist(sapply(lapply(list(model1.1, model1.2, model1.3, model1.4, model1.5), summary), "[", 8)),
                                      "Adj. R^2" = unlist(sapply(lapply(list(model1.1, model1.2, model1.3, model1.4, model1.5), summary), "[", 9))),
               include.deviance = F,
               label = "table:linear_direct_exp_factor", 
               file = "Tables/linear_direct_exp_factor.tex",
               use.packages = F,
               caption = "Ordinary least squares model with direct effects using the rate outcome. Conjoint attributes are operationalised as ordered factor levels. Normalisation: continuous variables are normalised by two times 
               the standard error to make them comparable to the estimates of binary variables following Gelman (2007). Standard errors are cluster robust by respondent id.")


##### interactions rate
model1.1 <- lm(rate ~ attrib1_lab*prior_benefit_2 + attrib2_lab*prior_benefit_2 +  attrib3_lab*prior_benefit_2 +  attrib4_lab*prior_benefit_2 + attrib5_lab*prior_benefit_2 + attrib6_lab*prior_benefit_2 +
                 + prior_benefit_2 + driver + home_owner + age + educ + language + empl_sect + fin_cond + left_right  + sal_glob + sal_env + region + urban_rural
               , data = reg_dat)
model1.2 <- lm(rate ~ attrib1_lab* sqrt(ratio_ev_to_muni_area) + attrib2_lab* sqrt(ratio_ev_to_muni_area) +  attrib3_lab* sqrt(ratio_ev_to_muni_area) +  attrib4_lab* sqrt(ratio_ev_to_muni_area) + attrib5_lab* sqrt(ratio_ev_to_muni_area) + attrib6_lab* sqrt(ratio_ev_to_muni_area) +
                 sqrt(ratio_ev_to_muni_area) + driver + home_owner + age + educ + language + empl_sect + fin_cond + left_right  + sal_glob + sal_env + region + urban_rural
               , data = reg_dat)

model1.1_r <- coeftest(model1.1, cluster = reg_dat$id,  vcov = vcovHC(model1.1, type="HC1"))
model1.2_r <- coeftest(model1.2, cluster = reg_dat$id,  vcov = vcovHC(model1.2, type="HC1"))

texreg::texreg(list(model1.1_r, model1.2_r), digits = 3, stars = c(0.001, 0.01, 0.05, 0.1), 
               fontsize = "tiny", longtable = T, no.margin = T,
               reorder.coef = c(1:5, 7:26, 6, 65, 27:41, 42:64, 66:89),
               omit.coef = "region",
               custom.coef.names = c("Intercept",
                                     # Experimental
                                     "50$\\%$", "60$\\%$", "70$\\%$", "80$\\%$", "Percieved Prior Benefit",
                                     "0.14 Fr./l petrol", "0.28 Fr./l petrol", "0.42 Fr./l petrol", "0.56 Fr./l petrol",
                                     "0.16 Fr./l heating oil", "0.31 Fr./l heating oil", "0.47 Fr./l heating oil", "0.63 Fr./l heating oil",
                                     "0.77 Fr./kg meat", "1.53 Fr./kg meat", "2.30 Fr./kg meat", "3.07 Fr./kg meat",
                                     "10 Fr. for short- and 30 Fr. for long-distance", "25 Fr. for short- and 75 Fr. for long-distance", "40 Fr. for short- and 120 Fr. for long-distance", "55 Fr. for short- and 165 Fr. for long-distance",
                                     "Mostly reimbursement", "Reimbursement und climate protection", "Mostly climate protection", "Exclusively climate protection",
                                     # EVs
                                     
                                     # CTRLs
                                     "Driver", "Home Owner", "Age", "Education", "French", "Primary Employment Sector", "Secondary Employment Sector", "Tertiary Employment Sector", "Financial Condition", "Left-Right",
                                     "Salience: Globalisation", "Salience: Environment and Climate",
                                     "Intermediate Area", "Rural Area",
                                     # "Preference: Regulatory Instruments", "Preference: Voluntary Instruments", "Preference: Subsidies", "Preference: Technical Solutions",
                                     # "Belief: Effectiveness", "Belief: Efficiency", "Belief: Competitiveness", "Belief: Justice", "Belief: Transformation"
                                     
                                     # 1
                                     "50\\% $\\times$ Prior Benefit", "60\\% $\\times$ Prior Benefit", "70\\% $\\times$ Prior Benefit", "80\\% $\\times$ Prior Benefit",
                                     "0.14 Fr./l petrol $\\times$ Prior Benefit", "0.28 Fr./l petrol $\\times$ Prior Benefit", "0.42 Fr./l petrol $\\times$ Prior Benefit", "0.56 Fr./l petrol $\\times$ Prior Benefit",
                                     "0.16 Fr./l heating oil $\\times$ Prior Benefit", "0.31 Fr./l heating oil $\\times$ Prior Benefit", "0.47 Fr./l heating oil $\\times$ Prior Benefit", "0.63 Fr./l heating oil $\\times$ Prior Benefit",
                                     "0.77 Fr./kg meat $\\times$ Prior Benefit", "1.53 Fr./kg meat $\\times$ Prior Benefit", "2.30 Fr./kg meat $\\times$ Prior Benefit", "3.07 Fr./kg meat $\\times$ Prior Benefit",
                                     "10 Fr. for short- and 30 Fr. for long-distance $\\times$ Prior Benefit", "25 Fr. for short- and 75 Fr. for long-distance $\\times$ Prior Benefit", "40 Fr. for short- and 120 Fr. for long-distance$\\times$ Prior Benefit", "55 Fr. for short- and 165 Fr. for long-distance$\\times$ Prior Benefit",
                                     "Mostly reimbursement $\\times$ Prior Benefit", "Reimbursement and climate protection $\\times$ Prior Benefit", "Mostly climate protection $\\times$ Prior Benefit", "Exclusively climate protection $\\times$ Prior Benefit",
                                     
                                     "EV Charging Stations", 
                                     # 2
                                     "50\\% $\\times$ EV Stations", "60\\% $\\times$ EV Stations", "70\\% $\\times$ EV Stations", "80\\% $\\times$ EV Stations",
                                     "0.14 Fr./l petrol $\\times$ EV Stations", "0.28 Fr./l petrol $\\times$ EV Stations", "0.42 Fr./l petrol $\\times$ EV Stations", "0.56 Fr./l petrol $\\times$ EV Stations",
                                     "0.16 Fr./l heating oil $\\times$ EV Stations", "0.31 Fr./l heating oil $\\times$ EV Stations", "0.47 Fr./l heating oil $\\times$ EV Stations", "0.63 Fr./l heating oil $\\times$ EV Stations",
                                     "0.77 Fr./kg meat $\\times$ EV Stations", "1.53 Fr./kg meat $\\times$ EV Stations", "2.30 Fr./kg meat $\\times$ EV Stations", "3.07 Fr./kg meat $\\times$ EV Stations",
                                     "10 Fr. for short- and 30 Fr. for long-distance $\\times$ EV Stations", "25 Fr. for short- and 75 Fr. for long-distance $\\times$ EV Stations", "40 Fr. for short- and 120 Fr. for long-distance$\\times$ EV Stations", "55 Fr. for short- and 165 Fr. for long-distance$\\times$ EV Stations",
                                     "Mostly reimbursement $\\times$ EV Stations", "Reimbursement and climate protection $\\times$ EV Stations", "Mostly climate protection $\\times$ EV Stations", "Exclusively climate protection $\\times$ EV Stations"
                                     
               ),
               custom.gof.rows = list("Region" = c("Yes", "Yes"),
                 "Observations" = lapply(list(model1.1, model1.2), nobs),
                 "R^2" = unlist(sapply(lapply(list(model1.1, model1.2), summary), "[", 8)),
                 "Adj. R^2" = unlist(sapply(lapply(list(model1.1, model1.2), summary), "[", 9))),
               groups = list("Experimental: Reduction Target" = 2:5, "Experimental: Tax Road Transport" = 6:9, "Experimental: Tax Housing" = 10:13,
                             "Experimental: Tax Food" = 14:17, "Experimental: Tax Aviation Transport" = 18:21, "Experimental: Revenue Use" = 22:25,
                             "Explanatory Variables" = 26:27, "Controls" = 28:41,
                             "Interactions: Prior Benefit and Experimental Variables" = 42:65, "Interactions: EV Stations and Experimental Variables" = 66:88),   
               include.deviance = F,
               label = "table:linear_interactions_exp_factor",
               file = "Tables/linear_interactions_exp_factor.tex",
               use.packages = F,
               caption = "Ordinary least squares model with interaction effects using the rate outcome. Conjoint attributes are operationalised as ordered factor levels.  Normalisation: continuous variables are normalised by two times 
               the standard error to make them comparable to the estimates of binary variables following Gelman (2007). Standard errors are cluster robust by respondent id.")
  
##### interactions choice
model1.1 <- lm(choice ~ 
                 attrib1_lab*prior_benefit_2 + attrib2_lab*prior_benefit_2 +  attrib3_lab*prior_benefit_2 +  attrib4_lab*prior_benefit_2 + attrib5_lab*prior_benefit_2 + attrib6_lab*prior_benefit_2 +
                 + prior_benefit_2 + driver + home_owner + age + educ + language + empl_sect + fin_cond + left_right  + sal_glob + sal_env + region + urban_rural
               , data = reg_dat)
model1.2 <- lm(choice ~ 
                 attrib1_lab* sqrt(ratio_ev_to_muni_area) + attrib2_lab* sqrt(ratio_ev_to_muni_area) +  attrib3_lab* sqrt(ratio_ev_to_muni_area) +  attrib4_lab* sqrt(ratio_ev_to_muni_area) + attrib5_lab* sqrt(ratio_ev_to_muni_area) + attrib6_lab* sqrt(ratio_ev_to_muni_area) +
                 sqrt(ratio_ev_to_muni_area) + driver + home_owner + age + educ + language + empl_sect + fin_cond + left_right  + sal_glob + sal_env + region + urban_rural
               , data = reg_dat)

model1.1_r <- coeftest(model1.1, cluster = reg_dat$id,  vcov = vcovHC(model1.1, type="HC1"))
model1.2_r <- coeftest(model1.2, cluster = reg_dat$id,  vcov = vcovHC(model1.2, type="HC1"))

texreg::texreg(list(model1.1_r, model1.2_r), digits = 3, stars = c(0.001, 0.01, 0.05, 0.1), 
               fontsize = "tiny", longtable = T, no.margin = T,
               reorder.coef = c(1:5, 7:26, 6, 65, 27:41, 42:64, 66:89),
               omit.coef = "region",
               custom.coef.names = c("Intercept",
                                     # Experimental
                                     "50$\\%$", "60$\\%$", "70$\\%$", "80$\\%$", "Percieved Prior Benefit",
                                     "0.14 Fr./l petrol", "0.28 Fr./l petrol", "0.42 Fr./l petrol", "0.56 Fr./l petrol",
                                     "0.16 Fr./l heating oil", "0.31 Fr./l heating oil", "0.47 Fr./l heating oil", "0.63 Fr./l heating oil",
                                     "0.77 Fr./kg meat", "1.53 Fr./kg meat", "2.30 Fr./kg meat", "3.07 Fr./kg meat",
                                     "10 Fr. for short- and 30 Fr. for long-distance", "25 Fr. for short- and 75 Fr. for long-distance", "40 Fr. for short- and 120 Fr. for long-distance", "55 Fr. for short- and 165 Fr. for long-distance",
                                     "Mostly reimbursement", "Reimbursement und climate protection", "Mostly climate protection", "Exclusively climate protection",
                                     # EVs
                                     
                                     # CTRLs
                                     "Driver", "Home Owner", "Age", "Education", "French", "Primary Employment Sector", "Secondary Employment Sector", "Tertiary Employment Sector", "Financial Condition", "Left-Right",
                                     "Salience: Globalisation", "Salience: Environment and Climate",
                                     "Intermediate Area", "Rural Area",
                                     # "Preference: Regulatory Instruments", "Preference: Voluntary Instruments", "Preference: Subsidies", "Preference: Technical Solutions",
                                     # "Belief: Effectiveness", "Belief: Efficiency", "Belief: Competitiveness", "Belief: Justice", "Belief: Transformation"
                                     
                                     # 1
                                     "50\\% $\\times$ Prior Benefit", "60\\% $\\times$ Prior Benefit", "70\\% $\\times$ Prior Benefit", "80\\% $\\times$ Prior Benefit",
                                     "0.14 Fr./l petrol $\\times$ Prior Benefit", "0.28 Fr./l petrol $\\times$ Prior Benefit", "0.42 Fr./l petrol $\\times$ Prior Benefit", "0.56 Fr./l petrol $\\times$ Prior Benefit",
                                     "0.16 Fr./l heating oil $\\times$ Prior Benefit", "0.31 Fr./l heating oil $\\times$ Prior Benefit", "0.47 Fr./l heating oil $\\times$ Prior Benefit", "0.63 Fr./l heating oil $\\times$ Prior Benefit",
                                     "0.77 Fr./kg meat $\\times$ Prior Benefit", "1.53 Fr./kg meat $\\times$ Prior Benefit", "2.30 Fr./kg meat $\\times$ Prior Benefit", "3.07 Fr./kg meat $\\times$ Prior Benefit",
                                     "10 Fr. for short- and 30 Fr. for long-distance $\\times$ Prior Benefit", "25 Fr. for short- and 75 Fr. for long-distance $\\times$ Prior Benefit", "40 Fr. for short- and 120 Fr. for long-distance$\\times$ Prior Benefit", "55 Fr. for short- and 165 Fr. for long-distance$\\times$ Prior Benefit",
                                     "Mostly reimbursement $\\times$ Prior Benefit", "Reimbursement and climate protection $\\times$ Prior Benefit", "Mostly climate protection $\\times$ Prior Benefit", "Exclusively climate protection $\\times$ Prior Benefit",
                                     
                                     "EV Charging Stations", 
                                     # 2
                                     "50\\% $\\times$ EV Stations", "60\\% $\\times$ EV Stations", "70\\% $\\times$ EV Stations", "80\\% $\\times$ EV Stations",
                                     "0.14 Fr./l petrol $\\times$ EV Stations", "0.28 Fr./l petrol $\\times$ EV Stations", "0.42 Fr./l petrol $\\times$ EV Stations", "0.56 Fr./l petrol $\\times$ EV Stations",
                                     "0.16 Fr./l heating oil $\\times$ EV Stations", "0.31 Fr./l heating oil $\\times$ EV Stations", "0.47 Fr./l heating oil $\\times$ EV Stations", "0.63 Fr./l heating oil $\\times$ EV Stations",
                                     "0.77 Fr./kg meat $\\times$ EV Stations", "1.53 Fr./kg meat $\\times$ EV Stations", "2.30 Fr./kg meat $\\times$ EV Stations", "3.07 Fr./kg meat $\\times$ EV Stations",
                                     "10 Fr. for short- and 30 Fr. for long-distance $\\times$ EV Stations", "25 Fr. for short- and 75 Fr. for long-distance $\\times$ EV Stations", "40 Fr. for short- and 120 Fr. for long-distance$\\times$ EV Stations", "55 Fr. for short- and 165 Fr. for long-distance$\\times$ EV Stations",
                                     "Mostly reimbursement $\\times$ EV Stations", "Reimbursement and climate protection $\\times$ EV Stations", "Mostly climate protection $\\times$ EV Stations", "Exclusively climate protection $\\times$ EV Stations"
                                     
               ),
               custom.gof.rows = list("Region" = c("Yes", "Yes"),
                                      "Observations" = lapply(list(model1.1, model1.2), nobs),
                                      "R^2" = unlist(sapply(lapply(list(model1.1, model1.2), summary), "[", 8)),
                                      "Adj. R^2" = unlist(sapply(lapply(list(model1.1, model1.2), summary), "[", 9))),
               groups = list("Experimental: Reduction Target" = 2:5, "Experimental: Tax Road Transport" = 6:9, "Experimental: Tax Housing" = 10:13,
                             "Experimental: Tax Food" = 14:17, "Experimental: Tax Aviation Transport" = 18:21, "Experimental: Revenue Use" = 22:25,
                             "Explanatory Variables" = 26:27, "Controls" = 28:41,
                             "Interactions: Prior Benefit and Experimental Variables" = 42:65, "Interactions: EV Stations and Experimental Variables" = 66:89),  
               include.deviance = F,
               label = "table:linear_interactions_exp_factor_choice",
               file = "Tables/linear_interactions_exp_factor_choice.tex",
               use.packages = F,
               caption = "Ordinary least squares model with interaction effects using the choice outcome. Conjoint attributes are operationalised as ordered factor levels.  Normalisation: continuous variables are normalised by two times 
               the standard error to make them comparable to the estimates of binary variables following Gelman (2007). Standard errors are cluster robust by respondent id.")


#----------------------------------------------------#
# operationalisation: expvars as continuous
#----------------------------------------------------#

reg_dat <- dat %>%
  dplyr::select(outcomes, expvars, covars, id) %>%
  mutate_at(vars(covars), as.character) %>% 
  mutate_at(vars(outcomes, covars, expvars), as.numeric) %>% 
  mutate(educ = ifelse(is.na(educ), median(educ, na.rm = T), educ),
         left_right = ifelse(is.na(left_right), median(left_right, na.rm = T), left_right),
         age = ifelse(is.na(age), median(age, na.rm = T), age),
         urban_rural = ifelse(is.na(urban_rural), median(urban_rural, na.rm = T), urban_rural),
         ratio_ev_to_muni_area = ifelse(is.na(ratio_ev_to_muni_area), median(ratio_ev_to_muni_area, na.rm = T), ratio_ev_to_muni_area),
         region = ifelse(is.na(region), median(region, na.rm = T), region),
  ) %>% 
  mutate_at(vars(empl_sect, urban_rural, region), as.factor)

reg_dat[c(vars_to_transform, expvars)] <- lapply(reg_dat[c(vars_to_transform, expvars)], sdFunc)

## (3) linear regression
library(sandwich)
model1.1 <- lm(rate ~ attrib1_lab + attrib2_lab +  attrib3_lab +  attrib4_lab + attrib5_lab + attrib6_lab, data = reg_dat)
model1.2 <- lm(rate ~ attrib1_lab + attrib2_lab +  attrib3_lab +  attrib4_lab + attrib5_lab + attrib6_lab +
                 + prior_benefit_2, data = reg_dat)
model1.3 <- lm(rate ~ attrib1_lab + attrib2_lab +  attrib3_lab +  attrib4_lab + attrib5_lab + attrib6_lab +
                 sqrt(ratio_ev_to_muni_area), data = reg_dat)
model1.4 <- lm(rate ~ attrib1_lab + attrib2_lab +  attrib3_lab +  attrib4_lab + attrib5_lab + attrib6_lab +
                 + prior_benefit_2 + driver + home_owner + age + educ + language + empl_sect + fin_cond + left_right  + sal_glob + sal_env + region 
               + urban_rural, data = reg_dat)
model1.5 <- lm(rate ~ attrib1_lab + attrib2_lab +  attrib3_lab +  attrib4_lab + attrib5_lab + attrib6_lab +
                 sqrt(ratio_ev_to_muni_area) + driver + home_owner + age + educ + language + empl_sect + fin_cond + left_right + sal_glob + sal_env + region 
               + urban_rural , data = reg_dat)
model1.1_r <- coeftest(model1.1, cluster = reg_dat$id,  vcov = vcovHC(model1.1, type="HC1"))
model1.2_r <- coeftest(model1.2, cluster = reg_dat$id,  vcov = vcovHC(model1.2, type="HC1"))
model1.3_r <- coeftest(model1.3, cluster = reg_dat$id,  vcov = vcovHC(model1.3, type="HC1"))
model1.4_r <- coeftest(model1.4, cluster = reg_dat$id,  vcov = vcovHC(model1.4, type="HC1"))
model1.5_r <- coeftest(model1.5, cluster = reg_dat$id,  vcov = vcovHC(model1.5, type="HC1"))
texreg::texreg(list(model1.1_r, model1.2_r, model1.3_r, model1.4_r, model1.5_r), digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
               fontsize = "tiny", longtable = T, no.margin = T,
               omit.coef = "region",
               custom.coef.names = c("Intercept", "Reduction Target", "Tax Road Transport", "Tax Housing", "Tax Food", "Tax Aviation Transport", "Revenue Use",
                                     # EVs
                                     "Percieved Prior Benefit", "EV Charging Stations",
                                     # CTRLs
                                     "Driver", "Home Owner", "Age", "Education", "French", "Primary Employment Sector", "Secondary Employment Sector", "Tertiary Employment Sector", "Financial Condition", "Left-Right",
                                     "Salience: Globalisation", "Salience: Environment and Climate",
                                     "Intermediate Area", "Rural Area"
                                     # "Preference: Regulatory Instruments", "Preference: Voluntary Instruments", "Preference: Subsidies", "Preference: Technical Solutions" ,
                                     # "Belief: Effectiveness", "Belief: Efficiency", "Belief: Competitiveness", "Belief: Justice", "Belief: Transformation"
               ),
               groups = list("Experimental" = 2:7, "Explanatory Variables" = 8:9, "Controls" = 10:23),
               # groups = list("Experimental" = 2:7, "Behavioural Change" = 8:11, "Renewable Transport" = 12:13, "Controls" = 14:29),
               custom.gof.rows = list("Region Controls" = c("No", "No", "No", "Yes", "Yes"),
                                      "Observations" = lapply(list(model1.1, model1.2, model1.3, model1.4, model1.5), nobs),
                                      "R^2" = unlist(sapply(lapply(list(model1.1, model1.2, model1.3, model1.4, model1.5), summary), "[", 8)),
                                      "Adj. R^2" = unlist(sapply(lapply(list(model1.1, model1.2, model1.3, model1.4, model1.5), summary), "[", 9))),
               include.deviance = F,
               label = "table:linear_direct_exp_continuous",
               file = "Tables/linear_direct_exp_continuous.tex",
               use.packages = F, 
               caption = "Ordinary least squares model with direct effects using the rate outcome. Conjoint attributes are operationalised as continuous variables. Normalisation: continuous variables are normalised by two times
               the standard error to make them comparable to the estimates of binary variables following Gelman (2007). Standard errors are cluster robust by respondent id.")

##### interactions rate
model1.1 <- lm(rate ~ 
                 attrib1_lab*prior_benefit_2 + attrib2_lab*prior_benefit_2 +  attrib3_lab*prior_benefit_2 +  attrib4_lab*prior_benefit_2 + attrib5_lab*prior_benefit_2 + attrib6_lab*prior_benefit_2 +
                 + prior_benefit_2 + driver + home_owner + age + educ + language  + empl_sect + fin_cond + left_right + sal_glob + sal_env + urban_rural
               , data = reg_dat)
model1.2 <- lm(rate ~ 
                 attrib1_lab* sqrt(ratio_ev_to_muni_area) + attrib2_lab* sqrt(ratio_ev_to_muni_area) +  attrib3_lab* sqrt(ratio_ev_to_muni_area) +  attrib4_lab* sqrt(ratio_ev_to_muni_area) + attrib5_lab* sqrt(ratio_ev_to_muni_area) + attrib6_lab* sqrt(ratio_ev_to_muni_area) +
                 sqrt(ratio_ev_to_muni_area) + driver + home_owner + age + educ + language + empl_sect + fin_cond + left_right + sal_glob + sal_env + urban_rural
               , data = reg_dat)

model1.1_r <- coeftest(model1.1, cluster = reg_dat$id,  vcov = vcovHC(model1.1, type="HC1"))
model1.2_r <- coeftest(model1.2, cluster = reg_dat$id,  vcov = vcovHC(model1.2, type="HC1"))

texreg::texreg(list(model1.1_r, model1.2_r), digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
               fontsize = "tiny", longtable = T, no.margin = T,
               reorder.coef = c(1:2, 4:8, 3, 29, 9:19, 20:28, 30:35),
               omit.coef = "region",
               custom.coef.names = c("Intercept", "Percieved Prior Benefit", "Reduction Target", "Tax Road Transport", "Tax Housing", "Tax Food", "Tax Aviation Transport", "Revenue Use",
                                     # CTRLs
                                     "Driver", "Home Owner", "Age", "Education", "French", "Primary Employment Sector", "Secondary Employment Sector", "Tertiary Employment Sector", "Financial Condition", "Left-Right",
                                     "Salience: Globalisation", "Salience: Environment and Climate",
                                     "Intermediate Area", "Rural Area",
                                     # "Preference: Regulatory Instruments", "Preference: Voluntary Instruments", "Preference: Subsidies", "Preference: Technical Solutions",
                                     # "Belief: Effectiveness", "Belief: Efficiency", "Belief: Competitiveness", "Belief: Justice", "Belief: Transformation"
                                     
                                     # Interactions
                                     "Reduction Target $\\times$ Prior Benefit", "Road Transport $\\times$ Prior Benefit", "Housing $\\times$ Prior Benefit", "Food $\\times$ Prior Benefit", "Aviation Transport $\\times$ Prior Benefit", "Revenue Use $\\times$ Prior Benefit",
                                     "EV Charging Stations", "Reduction Target $\\times$ EV Charging Stations", "Road Transport $\\times$ EV Charging Stations", "Housing $\\times$ EV Charging Stations", "Food $\\times$ EV Charging Stations", "Aviation Transport $\\times$ EV Charging Stations", "Revenue Use $\\times$ EV Charging Stations"
               ),
               groups = list("Experimental" = 2:7, "Explanatory Variables" = 8:9, "Controls" = 10:23,
                             "Prior Benefit and Experimental" = 24:29, "EV Stations and Experimental" = 30:35
               ),
               custom.gof.rows = list("Region Controls" = c("Yes", "Yes"),
                                      "Observations" = lapply(list(model1.1, model1.2), nobs),
                                      "R^2" = unlist(sapply(lapply(list(model1.1, model1.2), summary), "[", 8)),
                                      "Adj. R^2" = unlist(sapply(lapply(list(model1.1, model1.2), summary), "[", 9))),
               include.deviance = F,
               label = "table:linear_interactions_exp_continuous",
               file = "Tables/linear_interactions_exp_continuous.tex",
               use.packages = F, 
               caption = "Ordinary least squares model with interaction effects using the rate outcome. Conjoint attributes are operationalised as continuous variables. Normalisation: continuous variables are normalised by two times
               the standard error to make them comparable to the estimates of binary variables following Gelman (2007). Standard errors are cluster robust by respondent id.")

##### interactions choice
model1.1 <- lm(choice ~ 
                 attrib1_lab*prior_benefit_2 + attrib2_lab*prior_benefit_2 +  attrib3_lab*prior_benefit_2 +  attrib4_lab*prior_benefit_2 + attrib5_lab*prior_benefit_2 + attrib6_lab*prior_benefit_2 +
                 + prior_benefit_2 + driver + home_owner + age + educ + language  + empl_sect + fin_cond + left_right + sal_glob + sal_env + urban_rural
               , data = reg_dat)
model1.2 <- lm(choice ~ 
                 attrib1_lab* sqrt(ratio_ev_to_muni_area) + attrib2_lab* sqrt(ratio_ev_to_muni_area) +  attrib3_lab* sqrt(ratio_ev_to_muni_area) +  attrib4_lab* sqrt(ratio_ev_to_muni_area) + attrib5_lab* sqrt(ratio_ev_to_muni_area) + attrib6_lab* sqrt(ratio_ev_to_muni_area) +
                 sqrt(ratio_ev_to_muni_area) + driver + home_owner + age + educ + language + empl_sect + fin_cond + left_right + sal_glob + sal_env + urban_rural
               , data = reg_dat)
model1.1_r <- coeftest(model1.1, cluster = reg_dat$id,  vcov = vcovHC(model1.1, type="HC1"))
model1.2_r <- coeftest(model1.2, cluster = reg_dat$id,  vcov = vcovHC(model1.2, type="HC1"))

texreg::texreg(list(model1.1_r, model1.2_r), digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
               fontsize = "tiny", longtable = T, no.margin = T,
               reorder.coef = c(1:2, 4:8, 3, 29, 9:19, 20:28, 30:35),
               omit.coef = "region",
               custom.coef.names = c("Intercept", "Percieved Prior Benefit", "Reduction Target", "Tax Road Transport", "Tax Housing", "Tax Food", "Tax Aviation Transport", "Revenue Use",
                                     # CTRLs
                                     "Driver", "Home Owner", "Age", "Education", "French", "Primary Employment Sector", "Secondary Employment Sector", "Tertiary Employment Sector", "Financial Condition", "Left-Right",
                                     "Salience: Globalisation", "Salience: Environment and Climate",
                                     "Intermediate Area", "Rural Area",
                                     # "Preference: Regulatory Instruments", "Preference: Voluntary Instruments", "Preference: Subsidies", "Preference: Technical Solutions",
                                     # "Belief: Effectiveness", "Belief: Efficiency", "Belief: Competitiveness", "Belief: Justice", "Belief: Transformation"
                                     
                                     # Interactions
                                     "Reduction Target $\\times$ Prior Benefit", "Road Transport $\\times$ Prior Benefit", "Housing $\\times$ Prior Benefit", "Food $\\times$ Prior Benefit", "Aviation Transport $\\times$ Prior Benefit", "Revenue Use $\\times$ Prior Benefit",
                                     "EV Charging Stations", "Reduction Target $\\times$ EV Charging Stations", "Road Transport $\\times$ EV Charging Stations", "Housing $\\times$ EV Charging Stations", "Food $\\times$ EV Charging Stations", "Aviation Transport $\\times$ EV Charging Stations", "Revenue Use $\\times$ EV Charging Stations"
               ),
               groups = list("Experimental" = 2:7, "Explanatory Variables" = 8:9, "Controls" = 10:23,
                             "Prior Benefit and Experimental" = 24:29, "EV Stations and Experimental" = 30:35
               ),
               custom.gof.rows = list("Region Controls" = c("Yes", "Yes"),
                                      "Observations" = lapply(list(model1.1, model1.2), nobs),
                                      "R^2" = unlist(sapply(lapply(list(model1.1, model1.2), summary), "[", 8)),
                                      "Adj. R^2" = unlist(sapply(lapply(list(model1.1, model1.2), summary), "[", 9))),
               include.deviance = F,
               label = "table:linear_interactions_exp_continuous_choice",
               file = "Tables/linear_interactions_exp_continuous_choice.tex",
               use.packages = F, 
               caption = "Ordinary least squares model with interaction effects using the choice outcome. Conjoint attributes are operationalised as continuous variables. Normalisation: continuous variables are normalised by two times
               the standard error to make them comparable to the estimates of binary variables following Gelman (2007). Standard errors are cluster robust by respondent id.")

model1.1 <- lm(rate ~ attrib1_lab*prior_benefit_2 + attrib2_lab*prior_benefit_2 +  attrib3_lab*prior_benefit_2 +  attrib4_lab*prior_benefit_2 + attrib5_lab*prior_benefit_2 + attrib6_lab*prior_benefit_2 +
                     + prior_benefit_2 + driver + home_owner + age + educ + language + empl_sect + fin_cond + left_right*prior_benefit_2  + sal_glob + sal_env + region 
                   + urban_rural
                   , data = reg_dat)
model1.2 <- lm(rate ~ attrib1_lab*prior_benefit_2 + attrib2_lab*prior_benefit_2 +  attrib3_lab*prior_benefit_2 +  attrib4_lab*prior_benefit_2 + attrib5_lab*prior_benefit_2 + attrib6_lab*prior_benefit_2 +
                     + prior_benefit_2 + driver + home_owner + age + educ + language + empl_sect + fin_cond + sal_env*prior_benefit_2  + sal_glob + sal_env + region 
                   + urban_rural
                   , data = reg_dat)
model1.3 <- lm(rate ~ attrib1_lab*prior_benefit_2 + attrib2_lab*prior_benefit_2 +  attrib3_lab*prior_benefit_2 +  attrib4_lab*prior_benefit_2 + attrib5_lab*prior_benefit_2 + attrib6_lab*prior_benefit_2 +
                     + prior_benefit_2 + driver + home_owner + age + educ + language + empl_sect + fin_cond + educ*prior_benefit_2  + sal_glob + sal_env + region 
                   + urban_rural
                   , data = reg_dat)
model1.4 <- lm(rate ~ attrib1_lab*prior_benefit_2 + attrib2_lab*prior_benefit_2 +  attrib3_lab*prior_benefit_2 +  attrib4_lab*prior_benefit_2 + attrib5_lab*prior_benefit_2 + attrib6_lab*prior_benefit_2 +
                     + prior_benefit_2 + driver + home_owner + age + educ + language + empl_sect + fin_cond + driver*prior_benefit_2  + sal_glob + sal_env + region 
                   + urban_rural
                   , data = reg_dat)
texreg::texreg(list(model1.1, model1.2, model1.3, model1.4), digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
               fontsize = "tiny", longtable = T, no.margin = T,
               # reorder.coef = c(1:2, 4:8, 3, 29, 36, 9:19, 20:28, 30:35, 37:42),
               omit.coef = "region",
               # custom.coef.names = c("Intercept", "Percieved Prior Benefit", "Reduction Target", "Tax Road Transport", "Tax Housing", "Tax Food", "Tax Aviation Transport", "Revenue Use",
               #                       
               #                       # CTRLs
               #                       "Driver", "Home Owner", "Age", "Education", "French", "Primary Employment Sector", "Secondary Employment Sector", "Tertiary Employment Sector", "Financial Condition", "Left-Right",
               #                       "Salience: Globalisation", "Salience: Environment and Climate",
               #                       "Intermediate Area", "Rural Area",
               #                       # "Preference: Regulatory Instruments", "Preference: Voluntary Instruments", "Preference: Subsidies", "Preference: Technical Solutions",
               #                       # "Belief: Effectiveness", "Belief: Efficiency", "Belief: Competitiveness", "Belief: Justice", "Belief: Transformation"
               #                       
               #                       # Interactions
               #                       "Reduction Target $\\times$ Prior Benefit", "Road Transport $\\times$ Prior Benefit", "Housing $\\times$ Prior Benefit", "Food $\\times$ Prior Benefit", "Aviation Transport $\\times$ Prior Benefit", "Revenue Use $\\times$ Prior Benefit",
               #                       "EV Charging Stations", "Reduction Target $\\times$ EV Charging Stations", "Road Transport $\\times$ EV Charging Stations", "Housing $\\times$ EV Charging Stations", "Food $\\times$ EV Charging Stations", "Aviation Transport $\\times$ EV Charging Stations", "Revenue Use $\\times$ EV Charging Stations",
               #                       "Switch", "Reduction Target $\\times$ Switch", "Road Transport $\\times$ Switch", "Housing $\\times$ Switch", "Food $\\times$ Switch", "Aviation Transport $\\times$ Switch", "Revenue Use $\\times$ Switch"
               # ),
               # groups = list("Experimental" = 2:7, "Explanatory Variables" = 8:10, "Controls" = 11:22,
               #               "Prior Benefit and Experimental" = 25:30, "EV Stations and Experimental" = 31:36, "Switch and Experimental" = 37:42
               # ),
               # custom.gof.rows = list("Region Controls" = c("Yes", "Yes", "Yes"),
               #                        "Observations" = lapply(list(model1.1, model1.2, model1.3), nobs),
               #                        "R^2" = sapply(lapply(list(model1.1, model1.2, model1.3), summ), attr, "rsq"),
               #                        "Adj. R^2" = sapply(lapply(list(model1.1, model1.2, model1.3), summ), attr, "arsq")
               # ),
               # include.deviance = F,
               # label = "table:weighted_interactions_exp_continous_choice",
               # caption = "Survey-weighted generalised linear model with interaction effects using the choice outcome. Conjoint attributes are operationalised as continuous variables. Normalisation: continuous variables are normalised by two times 
               # the standard error to make them comparable to the estimates of binary variables following Gelman (2007)"
)

model1.1 <- lm(rate ~ attrib1_lab*ratio_ev_to_muni_area + attrib2_lab*ratio_ev_to_muni_area +  attrib3_lab*ratio_ev_to_muni_area +  attrib4_lab*ratio_ev_to_muni_area + attrib5_lab*ratio_ev_to_muni_area + attrib6_lab*ratio_ev_to_muni_area +
                 + prior_benefit_2 + driver + home_owner + age + educ + language + empl_sect + fin_cond + left_right*ratio_ev_to_muni_area  + sal_glob + sal_env + region 
               + urban_rural
               , data = reg_dat)
model1.3 <- lm(rate ~ attrib1_lab*ratio_ev_to_muni_area + attrib2_lab*ratio_ev_to_muni_area +  attrib3_lab*ratio_ev_to_muni_area +  attrib4_lab*ratio_ev_to_muni_area + attrib5_lab*ratio_ev_to_muni_area + attrib6_lab*ratio_ev_to_muni_area +
                 + prior_benefit_2 + driver + home_owner + age + educ + language + empl_sect + fin_cond + sal_env*ratio_ev_to_muni_area  + sal_glob + sal_env + region 
               + urban_rural
               , data = reg_dat)
model1.4 <- lm(rate ~ attrib1_lab*ratio_ev_to_muni_area + attrib2_lab*ratio_ev_to_muni_area +  attrib3_lab*ratio_ev_to_muni_area +  attrib4_lab*ratio_ev_to_muni_area + attrib5_lab*ratio_ev_to_muni_area + attrib6_lab*ratio_ev_to_muni_area +
                 + prior_benefit_2 + driver + home_owner + age + educ + language + empl_sect + fin_cond + educ*ratio_ev_to_muni_area  + sal_glob + sal_env + region 
               + urban_rural
               , data = reg_dat)
model1.5 <- lm(rate ~ attrib1_lab*ratio_ev_to_muni_area + attrib2_lab*ratio_ev_to_muni_area +  attrib3_lab*ratio_ev_to_muni_area +  attrib4_lab*ratio_ev_to_muni_area + attrib5_lab*ratio_ev_to_muni_area + attrib6_lab*ratio_ev_to_muni_area +
                 + prior_benefit_2 + driver + home_owner + age + educ + language + empl_sect + fin_cond + driver*ratio_ev_to_muni_area  + sal_glob + sal_env + region 
               + urban_rural
               , data = reg_dat)
texreg::texreg(list(model1.1, model1.3, model1.4, model1.5), digits = 3, stars = c(0.001, 0.01, 0.05, 0.1),
               fontsize = "tiny", longtable = T, no.margin = T,
               # reorder.coef = c(1:2, 4:8, 3, 29, 36, 9:19, 20:28, 30:35, 37:42),
               omit.coef = "region",
               # custom.coef.names = c("Intercept", "Percieved Prior Benefit", "Reduction Target", "Tax Road Transport", "Tax Housing", "Tax Food", "Tax Aviation Transport", "Revenue Use",
               #                       
               #                       # CTRLs
               #                       "Driver", "Home Owner", "Age", "Education", "French", "Primary Employment Sector", "Secondary Employment Sector", "Tertiary Employment Sector", "Financial Condition", "Left-Right",
               #                       "Salience: Globalisation", "Salience: Environment and Climate",
               #                       "Intermediate Area", "Rural Area",
               #                       # "Preference: Regulatory Instruments", "Preference: Voluntary Instruments", "Preference: Subsidies", "Preference: Technical Solutions",
               #                       # "Belief: Effectiveness", "Belief: Efficiency", "Belief: Competitiveness", "Belief: Justice", "Belief: Transformation"
               #                       
               #                       # Interactions
               #                       "Reduction Target $\\times$ Prior Benefit", "Road Transport $\\times$ Prior Benefit", "Housing $\\times$ Prior Benefit", "Food $\\times$ Prior Benefit", "Aviation Transport $\\times$ Prior Benefit", "Revenue Use $\\times$ Prior Benefit",
               #                       "EV Charging Stations", "Reduction Target $\\times$ EV Charging Stations", "Road Transport $\\times$ EV Charging Stations", "Housing $\\times$ EV Charging Stations", "Food $\\times$ EV Charging Stations", "Aviation Transport $\\times$ EV Charging Stations", "Revenue Use $\\times$ EV Charging Stations",
               #                       "Switch", "Reduction Target $\\times$ Switch", "Road Transport $\\times$ Switch", "Housing $\\times$ Switch", "Food $\\times$ Switch", "Aviation Transport $\\times$ Switch", "Revenue Use $\\times$ Switch"
               # ),
               # groups = list("Experimental" = 2:7, "Explanatory Variables" = 8:10, "Controls" = 11:22,
               #               "Prior Benefit and Experimental" = 25:30, "EV Stations and Experimental" = 31:36, "Switch and Experimental" = 37:42
               # ),
               # custom.gof.rows = list("Region Controls" = c("Yes", "Yes", "Yes"),
               #                        "Observations" = lapply(list(model1.1, model1.2, model1.3), nobs),
               #                        "R^2" = sapply(lapply(list(model1.1, model1.2, model1.3), summ), attr, "rsq"),
               #                        "Adj. R^2" = sapply(lapply(list(model1.1, model1.2, model1.3), summ), attr, "arsq")
               # ),
               # include.deviance = F,
               # label = "table:weighted_interactions_exp_continous_choice",
               # caption = "Survey-weighted generalised linear model with interaction effects using the choice outcome. Conjoint attributes are operationalised as continuous variables. Normalisation: continuous variables are normalised by two times 
               # the standard error to make them comparable to the estimates of binary variables following Gelman (2007)"
)

######################################################
# sparsereg
######################################################
# orgpanisation of the code
# (1) clean data (impute missings)
# (2) without the variable group on siwss climate policy (the question: "Geben Sie bitte an, wie sehr Sie die folgenden Aussagen ablehnen oder unterstützten. 
#     Bei der Ausgestaltung der Schweizer Klimapolitik sollte vor allem…)  
# (3) only the variables on sal_glob, sal_env, ren_driver, no_of_stations_ev, no_of_stations_pt, prior_benefit

clean <- dat %>% 
  dplyr::select(outcomes, covars, expvars, id, choice_id, round) %>% 
  mutate_all(as.numeric) %>% 
  mutate(ratio_ev_to_muni_area = sqrt(ifelse(is.na(ratio_ev_to_muni_area), median(ratio_ev_to_muni_area, na.rm = T), ratio_ev_to_muni_area)),
         educ = ifelse(is.na(educ), median(educ, na.rm = T), educ),
         left_right = ifelse(is.na(left_right), median(left_right, na.rm = T), left_right),
         age = ifelse(is.na(age), median(age, na.rm = T), age),
         urban_rural = ifelse(is.na(urban_rural), median(urban_rural, na.rm = T), urban_rural),
         region = ifelse(is.na(region), median(region, na.rm = T), region)
  ) %>%
  drop_na() %>% 
  mutate(
    Geneva = ifelse(region == 1, 1, 0),
    Middle_Land = ifelse(region == 2, 1, 0),
    North_West = ifelse(region == 3, 1, 0),
    Zurich = ifelse(region == 4, 1, 0),
    East = ifelse(region == 5, 1, 0),
    Central = ifelse(region == 6, 1, 0),
    Ticino = ifelse(region == 7, 1, 0),
    urban = ifelse(urban_rural == 1, 1, 0),
    intermediary = ifelse(urban_rural == 2, 1, 0),
    rural = ifelse(urban_rural == 3, 1, 0)) %>% 
  mutate(attrib1_lab = ifelse(attrib1_lab == "40%", "Control", attrib1_lab),
         attrib2_lab = ifelse(attrib2_lab == "Keine Abgabe auf Benzin", "Control", attrib2_lab),
         attrib3_lab = ifelse(attrib3_lab == "Keine Abgabe auf Heizöl", "Control", attrib3_lab),
         attrib4_lab = ifelse(attrib4_lab == "Keine Abgabe auf Fleisch", "Control", attrib4_lab),
         attrib5_lab = ifelse(attrib5_lab == "Keine Abgabe", "Control", attrib5_lab),
         attrib6_lab = ifelse(attrib6_lab == "Pauschale Rückerstattung und Klimaschutzförderbeiträge", "Control", attrib6_lab)) 
  
treat <- clean %>% 
  as_tibble() %>% 
  dplyr::select(starts_with("attrib") & ends_with("lab"))


pre_treat_1 <- clean %>%
  dplyr::select(
    prior_benefit_2,
    ratio_ev_to_muni_area,
    driver, home_owner, age, educ, language, empl_sect, fin_cond, 
    left_right,
    Geneva, Middle_Land, North_West, Zurich, East, Central, Ticino, 
    sal_glob, sal_env,
    urban, rural, intermediary
    ) %>% 
  remove_rownames() %>% 
  as.matrix()

clean[vars_to_transform] <- lapply(clean[vars_to_transform], sdFunc)

out <- sparsereg::sparsereg(y = clean$choice, 
                            X = pre_treat_1,
                            treat = treat,
                            id = clean$id,
                            id2 = clean$choice_id,
                            id3 = clean$round,
                            scale.type = "TX",
                            baseline.vec = "Control",
                            conservative = F,
                            gibbs = 50, burnin = 50)

summary(out)
sparsereg_res <- print(out)$statistics
r_names_spreg <- rownames(sparsereg_res)
sparsereg_res_clean <- sparsereg_res %>% 
  as_tibble() %>% 
  dplyr::mutate(variables = gsub(": ", "_", r_names_spreg),
                attribute = NA,
                attribute = ifelse(grepl("attrib1_lab", variables), "Reduction Target", attribute),
                attribute = ifelse(grepl("attrib2_lab", variables), "Road Transport", attribute),
                attribute = ifelse(grepl("attrib3_lab", variables), "Housing", attribute),
                attribute = ifelse(grepl("attrib4_lab", variables), "Food", attribute),
                attribute = ifelse(grepl("attrib5_lab", variables), "Aviation Transport", attribute),
                attribute = ifelse(grepl("attrib6_lab", variables), "Revenue Use", attribute),
                attribute = factor(attribute, levels = c("Reduction Target", "Road Transport", "Housing", "Food", "Aviation Transport", "Revenue Use")),
                
                cjoin_level = NA,
                cjoin_level = ifelse(grepl("attrib1_lab_1", variables), "40%", cjoin_level),
                cjoin_level = ifelse(grepl("attrib1_lab_2", variables), "50%", cjoin_level),
                cjoin_level = ifelse(grepl("attrib1_lab_3", variables), "60%", cjoin_level),
                cjoin_level = ifelse(grepl("attrib1_lab_4", variables), "70%", cjoin_level),
                cjoin_level = ifelse(grepl("attrib1_lab_5", variables), "80%", cjoin_level),
                
                cjoin_level = ifelse(grepl("attrib2_lab_1", variables), "No tax on petrol", cjoin_level),
                cjoin_level = ifelse(grepl("attrib2_lab_2", variables), "0.14 Fr./l petrol", cjoin_level),
                cjoin_level = ifelse(grepl("attrib2_lab_3", variables), "0.28 Fr./l petrol", cjoin_level),
                cjoin_level = ifelse(grepl("attrib2_lab_4", variables), "0.42 Fr./l petrol", cjoin_level),
                cjoin_level = ifelse(grepl("attrib2_lab_5", variables), "0.56 Fr./l petrol", cjoin_level),
                
                cjoin_level = ifelse(grepl("attrib3_lab_1", variables), "No tax on heating oil", cjoin_level),
                cjoin_level = ifelse(grepl("attrib3_lab_2", variables), "0.16 Fr./l heating oil", cjoin_level),
                cjoin_level = ifelse(grepl("attrib3_lab_3", variables), "0.31 Fr./l heating oil", cjoin_level),
                cjoin_level = ifelse(grepl("attrib3_lab_4", variables), "0.47 Fr./l heating oil", cjoin_level),
                cjoin_level = ifelse(grepl("attrib3_lab_5", variables), "0.63 Fr./l heating oil", cjoin_level),
                
                cjoin_level = ifelse(grepl("attrib4_lab_1", variables), "No tax on meat", cjoin_level),
                cjoin_level = ifelse(grepl("attrib4_lab_2", variables), "0.77 Fr./kg meat", cjoin_level),
                cjoin_level = ifelse(grepl("attrib4_lab_3", variables), "1.53 Fr./kg meat", cjoin_level),
                cjoin_level = ifelse(grepl("attrib4_lab_4", variables), "2.30 Fr./kg meat", cjoin_level),
                cjoin_level = ifelse(grepl("attrib4_lab_5", variables), "3.07 Fr./kg meat", cjoin_level),
                
                cjoin_level = ifelse(grepl("attrib5_lab_1", variables), "No tax on private flights", cjoin_level),
                cjoin_level = ifelse(grepl("attrib5_lab_2", variables), "10 Fr. for short- and 30 Fr. for long-distance", cjoin_level),
                cjoin_level = ifelse(grepl("attrib5_lab_3", variables), "25 Fr. for short- and 75 Fr. for long-distance", cjoin_level),
                cjoin_level = ifelse(grepl("attrib5_lab_4", variables), "40 Fr. for short- and 120 Fr. for long-distance", cjoin_level),
                cjoin_level = ifelse(grepl("attrib5_lab_5", variables), "55 Fr. for short- and 165 Fr. for long-distance", cjoin_level),
                
                cjoin_level = ifelse(grepl("attrib6_lab_1", variables), "Exclusively climate protection", cjoin_level),
                cjoin_level = ifelse(grepl("attrib6_lab_2", variables), "Mostly reimbursement", cjoin_level),
                cjoin_level = ifelse(grepl("attrib6_lab_3", variables), "Reimbursement and climate protection", cjoin_level),
                cjoin_level = ifelse(grepl("attrib6_lab_4", variables), "Mostly climate protection", cjoin_level),
                cjoin_level = ifelse(grepl("attrib6_lab_5", variables), "Exclusively reimbursement", cjoin_level),
                cjoin_level = factor(cjoin_level, levels = c(
                                                       "40%", "50%", "60%", "70%", "80%",
                                                       "No tax on petrol", "0.14 Fr./l petrol", "0.28 Fr./l petrol", "0.42 Fr./l petrol", "0.56 Fr./l petrol",
                                                       "No tax on heating oil", "0.16 Fr./l heating oil", "0.31 Fr./l heating oil", "0.47 Fr./l heating oil", "0.63 Fr./l heating oil",
                                                       "No tax on meat", "0.77 Fr./kg meat", "1.53 Fr./kg meat", "2.30 Fr./kg meat", "3.07 Fr./kg meat",
                                                       "No tax on private flights", "10 Fr. for short- and 30 Fr. for long-distance", "25 Fr. for short- and 75 Fr. for long-distance", "40 Fr. for short- and 120 Fr. for long-distance", "55 Fr. for short- and 165 Fr. for long-distance",
                                                       "Exclusively reimbursement", "Mostly reimbursement", "Reimbursement and climate protection", "Mostly climate protection", "Exclusively climate protection")),
                
                var_group = NA,
                var_group = ifelse(grepl(("driver|home_owner|vege"), variables) & !grepl(("ren_driver"), variables), "Behavioural\n Controls", var_group),
                var_group = ifelse(grepl(c("prior_benefit_2|ren_driver|renew_heating|switch|ratio_ev_to_muni_area|ratio_pt_to_muni_area"), variables), "Explanatory\n Variables", var_group),
                var_group = ifelse(grepl(c("sal_glob|sal_env"), variables),  "Salience\n Controls", var_group),
                var_group = ifelse(grepl(c("Geneva|Middle_Land|North_West|Zurich|East|Central|Ticino|urban|intermediary|rural"), variables), "Geographic\n Controls", var_group),
                var_group = ifelse(grepl(c("co2_law_effect|co2_law_effic|co2_law_compet|co2_law_just|co2_law_transf"), variables), "Belief\n Controls", var_group),
                var_group = ifelse(grepl(c("swiss_pol_regu|swiss_pol_vol|swiss_pol_subs|swiss_pol_tech"), variables), "Preference\n Controls", var_group),
                var_group = ifelse(grepl(c("age|educ|empl_sect|fin_cond|left_right|language"), variables), "Demographic\n Controls", var_group),
                var_group = factor(var_group, levels = c("Explanatory\n Variables", "Behavioural\n Controls", "Demographic\n Controls", "Salience\n Controls", "Geographic\n Controls", "Belief\n Controls", "Preference\n Controls")),
                
                covar = as.factor(NA),
                covar = ifelse(grepl("prior_benefit_2 ",variables),"Percieved Prior Benefit",covar),
                covar = ifelse(grepl("driver ",variables) & !grepl("ren_driver ",variables),"Driver",covar),
                covar = ifelse(grepl("ren_driver ",variables),"Renewable Driver",covar),
                covar = ifelse(grepl("renew_heating ",variables),"Renewable Heater",covar),
                covar = ifelse(grepl("ratio_ev_to_muni_area ",variables),"EV Charging Stations",covar),
                covar = ifelse(grepl("home_owner ",variables),"Home Owner",covar),
                covar = ifelse(grepl("age ",variables),"Age",covar),
                covar = ifelse(grepl("educ ",variables),"Education",covar),
                covar = ifelse(grepl("language ",variables),"French",covar),
                covar = ifelse(grepl("empl_sect ",variables),"Primary Employment Sector",covar),
                covar = ifelse(grepl("fin_cond ",variables),"Financial Condition",covar),
                covar = ifelse(grepl("left_right ",variables),"Left-Right",covar),
                covar = ifelse(grepl("sal_glob ",variables),"Salience: Globalisation",covar),
                covar = ifelse(grepl("sal_env ",variables),"Salience: Environment and Climate",covar),
                covar = ifelse(grepl("urban_rural2 ",variables),"Rural Area",covar),
                covar = ifelse(grepl("urban_rural3 ",variables),"Intermediate Area",covar),
                covar = ifelse(grepl("urban" ,variables),"Urban",covar),
                covar = ifelse(grepl("intermediary ",variables),"Intermediary",covar),
                covar = ifelse(grepl("rural ",variables),"Rural",covar),
                
                covar = factor(covar, levels = c("Percieved Prior Benefit", 
                                                 "EV Charging Stations", "PT Stations", "Switch",
                                                 # CTRLs
                                                 "Driver", "Home Owner", "Vegetarian", "Age", "Education", "French", "Primary Employment Sector", "Secondary Employment Sector", "Tertiary Employment Sector", "Financial Condition", "Left-Right",
                                                 "Salience: Globalisation", "Salience: Environment and Climate",
                                                 "Intermediate Area",  "Rural Area",  
                                                 "Belief: Effectiveness", "Belief: Efficiency", "Belief: Competitiveness", "Belief: Justice", "Belief: Transformation",
                                                 "Preference: Regulatory Instruments", "Preference: Voluntary Instruments", "Preference: Subsidies", "Preference: Technical Solutions",
                                                 "Geneva", "Middle Land", "North West", "Zurich", "East", "Central", "Ticino", "Urban", "Intermediary", "Rural"))
  ) %>% 
  dplyr::mutate(Mean = Mean/max(abs(Mean))) %>% 
  dplyr::filter(!is.na(attribute) & !is.na(covar) & !is.na(var_group)) %>% 
  dplyr::mutate(significant = ifelse((Mean-1.96*SD > 0 & Mean+1.96*SD > 0) | (Mean-1.96*SD < 0 & Mean+1.96*SD < 0), 1 ,0)) 

p_sparse_reg_heat_out_1.4 <- sparsereg_res_clean %>% 
  ggplot2::ggplot(., ggplot2::aes_string(x = "covar",
                                         y = "cjoin_level",
                                         fill = "Mean")) +
  ggplot2::facet_grid(attribute ~ var_group, 
                      space = "free",
                      scales = "free",
                      # switch = "y"
  ) +
  ggplot2::geom_tile() +
  ggplot2::geom_tile(data = sparsereg_res_clean[sparsereg_res_clean$significant == 1 & sparsereg_res_clean$Mean > 0,], fill = NA, color = "darkgreen", size = 1) +
  ggplot2::geom_tile(data = sparsereg_res_clean[sparsereg_res_clean$significant == 1 & sparsereg_res_clean$Mean < 0,], fill = NA, color = "darkred", size = 1) +
  ggplot2::scale_fill_gradient2(high = "darkgreen", low = "darkred", limits = c(-1, 1), midpoint = 0, mid = "grey90") +
  ggplot2::scale_y_discrete(limits = rev) +
  ggplot2::labs(x = "", y = "", fill = "Mean") +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle= 45, vjust = 1, hjust = 1), 
                 strip.placement = "outside", legend.position = "bottom", text = element_text(size=14, colour = "black"),)
p_sparse_reg_heat_out_1.4
ggsave(p_sparse_reg_heat_out_1.4, file = "Plots/sparse_reg_heat_out_1_4_new_controls.pdf", height = 15, width = 11)

######################################################
# CJBART
######################################################

library(cjbart)
bart_dat <- dat %>% 
  dplyr::select(renew_heating,
             ren_driver,
             ratio_ev_to_muni_area,
             prior_benefit_2,
             driver, home_owner, vege, age, educ, empl_sect, fin_cond, left_right,
             co2_law_effect, co2_law_effic, co2_law_compet, co2_law_just, co2_law_transf, 
             sal_glob, sal_env,
             swiss_pol_regu, swiss_pol_vol, swiss_pol_subs, swiss_pol_tech,
             urban_rural, 
             "rate",
             "attrib1_lab", "attrib2_lab", "attrib3_lab", "attrib4_lab", "attrib5_lab", "attrib6_lab", "id" )%>%
  # mutate_at(vars("choice", "gender", "id"), as.numeric) %>% 
  mutate_at(vars("attrib1_lab", "attrib2_lab", "attrib3_lab", "attrib4_lab", "attrib5_lab", "attrib6_lab"), as.factor) %>% 
  drop_na() %>% 
  mutate(rate = case_when(rate == 1 ~ 0,
                          rate == 2 ~ 0,
                          rate == 3 ~ 0,
                          rate == 4 ~ 1,
                          rate == 5 ~ 1)) %>% 
    mutate(
      # Geneva = ifelse(region == 1, 1, 0),
      # Middle_Land = ifelse(region == 2, 1, 0),
      # North_West = ifelse(region == 3, 1, 0),
      # Zurich = ifelse(region == 4, 1, 0),
      # East = ifelse(region == 5, 1, 0),
      # Central = ifelse(region == 6, 1, 0),
      # Ticino = ifelse(region == 7, 1, 0),
      urban = ifelse(urban_rural == 1, 1, 0),
      intermediary = ifelse(urban_rural == 2, 1, 0),
      rural = ifelse(urban_rural == 3, 1, 0)
    ) 

# save.image("conjointScripts/data_sparsereg_and_cjbart.RData")
# load("conjointScripts/data_sparsereg_and_cjbart.RData")

res <- cjbart(bart_dat, Y = "rate", id = "id")
res_imce <- cjbart::IMCE(data = bart_dat,
                         model = res,
                         attribs = c("attrib1_lab", "attrib2_lab", "attrib3_lab", "attrib4_lab", "attrib5_lab", "attrib6_lab"),
                         ref_levels = c("40%", "No tax on petrol", "No tax on heating oil", "No tax on meat", "No tax on private flights", "Exclusively investment into climate protection"),
                         cores = 4)

plot(res_imce)
summary(res_imce)

levels <- c("40%", "50%", "60%", "70%", "80%",
            "No tax on petrol", "0.14 Fr./l petrol", "0.28 Fr./l petrol", "0.42 Fr./l petrol", "0.56 Fr./l petrol",
            "No tax on heating oil", "0.16 Fr./l heating oil", "0.31 Fr./l heating oil", "0.47 Fr./l heating oil", "0.63 Fr./l heating oil",
            "No tax on meat", "0.77 Fr./kg meat", "1.53 Fr./kg meat", "2.30 Fr./kg meat", "3.07 Fr./kg meat",
            "No tax on private flights","10 Fr. for short- and 30 Fr. for long-distance", "25 Fr. for short- and 75 Fr. for long-distance", "40 Fr. for short- and 120 Fr. for long-distance", "55 Fr. for short- and 165 Fr. for long-distance",
            "Exclusively investment into climate protection", "Mostly lump sum reimbursement", "Lump sum reimbursement und investment into climate protection", "Mostly investment into climate protection", "Exclusively lump sum reimbursement")

imce_het_cjbart_prior_benefit_transport <- plot(res_imce, covar = "prior_benefit_2", 
                                        plot_levels = c("No tax on petrol", "0.14 Fr./l petrol", "0.28 Fr./l petrol", "0.42 Fr./l petrol", "0.56 Fr./l petrol")) + 
  scale_color_manual(values = c("black", "grey25", "grey50", "lightgreen", "darkgreen")) +
  theme_light() + theme(legend.position = "bottom") + labs(colour = "Prior Benefit")
ggsave(imce_het_cjbart_prior_benefit_transport, file = "Plots/imce_het_cjbart_prior_benefit_transport.pdf", height = 15, width = 10)

imce_het_cjbart_prior_benefit_2 <- plot(res_imce, covar = "prior_benefit_2", 
     plot_levels = levels) + 
  theme_light() + theme(legend.position = "bottom")
ggsave(imce_het_cjbart_prior_benefit_transport, file = "Plots/imce_het_cjbart_prior_benefit_transport.pdf", height = 15, width = 10)
imce_het_cjbart_ren_driver <- plot(res_imce, covar = "ren_driver", 
                        plot_levels = levels) + 
  theme_light() + theme(legend.position = "bottom")
ggsave(imce_het_cjbart_ren_driver, file = "Plots/imce_het_cjbart_ren_driver.pdf", height = 15, width = 10)
imce_het_cjbart_renew_heating <- plot(res_imce, covar = "renew_heating", 
                        plot_levels = levels) + 
  theme_light() + theme(legend.position = "bottom")
ggsave(imce_het_cjbart_renew_heating, file = "Plots/imce_het_cjbart_renew_heating.pdf", height = 15, width = 10)
imce_het_cjbart_switch <- plot(res_imce, covar = "switch", 
                        plot_levels = levels) + 
  theme_light() + theme(legend.position = "bottom")
ggsave(imce_het_cjbart_switch, file = "Plots/imce_het_cjbart_switchp.df", height = 15, width = 10)
imce_het_cjbart_ratio_ev_to_muni_area <- plot(res_imce, covar = "ratio_ev_to_muni_area", 
                        plot_levels = levels) + 
  theme_light() + theme(legend.position = "bottom")
ggsave(imce_het_cjbart_ratio_ev_to_muni_area, file = "Plots/imce_het_cjbart_ratio_ev_to_muni_area.pdf", height = 15, width = 10)
imce_het_cjbart_ratio_pt_to_muni_area <- plot(res_imce, covar = "ratio_pt_to_muni_area", 
                        plot_levels = levels) + 
  theme_light() + theme(legend.position = "bottom")
ggsave(imce_het_cjbart_ratio_pt_to_muni_area, file = "Plots/imce_het_cjbart_ratio_pt_to_muni_area.pdf", height = 15, width = 10)

vip_bart <- het_vimp(res_imce) 


p_dat <- vip_bart %>% 
  dplyr::arrange(-importance) %>% 
  dplyr::mutate(attribute = "",
                attribute = ifelse(outcome %in% c("40%", "50%", "60%", "70%", "80%"), "Reduction Target", attribute),
                attribute = ifelse(outcome %in% c("No tax on petrol", "0.14 Fr./l petrol", "0.28 Fr./l petrol", "0.42 Fr./l petrol", "0.56 Fr./l petrol"), "Tax Road Transport", attribute),
                attribute = ifelse(outcome %in% c("No tax on heating oil", "0.16 Fr./l heating oil", "0.31 Fr./l heating oil", "0.47 Fr./l heating oil", "0.63 Fr./l heating oil"), "Tax Housing", attribute),
                attribute = ifelse(outcome %in% c("No tax on meat", "0.77 Fr./kg meat", "1.53 Fr./kg meat", "2.30 Fr./kg meat", "3.07 Fr./kg meat"), "Tax Food", attribute),
                attribute = ifelse(outcome %in% c("No tax on private flights", "10 Fr. for short- and 30 Fr. for long-distance", "25 Fr. for short- and 75 Fr. for long-distance", "40 Fr. for short- and 120 Fr. for long-distance", "55 Fr. for short- and 165 Fr. for long-distance"), "Tax Aviation Transport", attribute),
                attribute = ifelse(outcome %in% c("Exclusively investment into climate protection", "Mostly lump sum reimbursement", "Lump sum reimbursement und investment into climate protection", "Mostly investment into climate protection", "Exclusively lump sum reimbursement"), "Revenue Use", attribute),
                attribute = factor(attribute, levels =  c("Reduction Target", "Tax Road Transport", "Tax Housing", "Tax Food", "Tax Aviation Transport", "Revenue Use")),
                outcome = factor(outcome, levels = levels),
                covar = factor(covar, levels = c("prior_benefit_2", "ratio_ev_to_muni_area", "switch", 
                                                 "ren_driver", "renew_heating", "driver", "home_owner", "vege", "age", "educ", "empl_sect", "fin_cond", "left_right", 
                                                 "sal_glob", "sal_env", 
                                                 "urban", "intermediary", "rural")),
                var_group = "",
                var_group = ifelse(covar %in% c("prior_benefit_2", "ratio_ev_to_muni_area"), "Explanatory\n Variables", var_group),
                var_group = ifelse(covar %in% c( "ren_driver", "renew_heating", "driver", "home_owner", "vege"), "Behavioural\n Controls", var_group),
                var_group = ifelse(covar %in% c("sal_glob", "sal_env"), "Salience\n Controls", var_group),
                var_group = ifelse(covar %in% c("urban", "intermediary", "rural"), "Geographic\n Controls", var_group),
                var_group = ifelse(covar %in% c("co2_law_effect", "co2_law_effic", "co2_law_compet", "co2_law_just", "co2_law_transf"), "Belief\n Controls", var_group),
                var_group = ifelse(covar %in% c("swiss_pol_regu", "swiss_pol_vol", "swiss_pol_subs", "swiss_pol_tech"), "Preference\n Controls", var_group),
                var_group = ifelse(covar %in% c("age", "educ", "empl_sect", "fin_cond", "left_right"), "Demographic\n Controls", var_group),
                var_group = factor(var_group, levels = c("Explanatory\n Variables", "Behavioural\n Controls", "Demographic\n Controls", "Salience\n Control", "Geographic\n Controls", "Belief\n Controls", "Preference\n Controls"))
                ) %>%
  dplyr::mutate(covar = forcats::fct_recode(covar,
                            "Prior Benefit" = "prior_benefit_2",
                            "Renewable Driver" = "ren_driver",
                            "Renewable Heater" = "renew_heating",
                            "EV Charging Stations" = "ratio_ev_to_muni_area",
                            "PT Stations" = "ratio_pt_to_muni_area",
                            "Driver" = "driver",
                            "Home Owner" = "home_owner",
                            "Vegetarian" = "vege",
                            "Age" = "age",
                            "Education" = "educ",
                            "Primary Employment Sector" = "empl_sect",
                            # "prior_benefit_2" = "Secondary Employment Sector",
                            # "prior_benefit_2" = "Tertiary Employment Sector",
                            "Financial Condition"= "fin_cond",
                            "Left-Right" = "left_right",
                            "Salience: Globalisation" = "sal_glob",
                            "Salience: Environment and Climate" = "sal_env",
                            "Rural Area" = "rural",
                            "Intermediate Area" = "intermediary",
                            "Urban Area" = "urban")
  ) %>% 
  dplyr::mutate(lower2.5 = as.numeric(lower2.5),
                upper97.5 = as.numeric(upper97.5),
                significant = ifelse((lower2.5 > 0 & upper97.5 > 0) | (lower2.5 < 0 & upper97.5 < 0), 1 ,0)) %>% 
  filter(!is.na(covar)) %>% 
  dplyr::mutate(importance_norm = importance/max(importance))

p_bart_vip_heat <- p_dat %>% 
  ggplot2::ggplot(., ggplot2::aes_string(x = "covar",
                                      y = "outcome",
                                      fill = "importance_norm")) +
  ggplot2::facet_grid(attribute ~ var_group, 
                      space = "free",
                      scales = "free",
                      # switch = "y"
                      ) +
  ggplot2::geom_tile() +
  ggplot2::geom_tile(data = p_dat[p_dat$significant == 1,], fill = NA, color = "white", size = 1) +
  ggplot2::scale_fill_gradient(low = "#132B43", high = "#56B1F7", breaks = c(0,0.5,1)) +
  ggplot2::scale_y_discrete(limits = rev) +
  ggplot2::labs(x = "", y = "", fill = "Importance") +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle= 45, vjust = 1, hjust = 1), 
                 strip.placement = "outside", legend.position = "bottom", text = element_text(size=14, color = "black"),
                 strip.background = element_blank())
p_bart_vip_heat  
ggsave(p_bart_vip_heat, file = "Plots/bart_vip_heat_1.pdf", height = 15, width = 13)
