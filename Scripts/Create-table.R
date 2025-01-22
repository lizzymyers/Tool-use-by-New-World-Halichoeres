# Code of the paper 'Tool use in New World Halichoeres' 
# Author of the code: Juliette Tariel-Adam
# Date: 21th January 2025
# This script builds Table 1 about literature on fish anvil use
#and Table 2 in which the new observations of fish anvil use are reported
# The script reads the exported tables from Notion and exports them to:
# - a csv for archive
# - a csv file for later use in the map 
# - to a google sheet to share with the co-authors
# This script also calculates summary statistics reported in the paper

# Packages
library(tidyverse)
library(googlesheets4)
library(ggplot2)

# Table literature
TabLit <- read.csv(unz("Table literature/Exported table Literature from Notion.zip",
                      "Observations literature 58b11884235147759bfeb18befe9c7bb_all.csv"), 
                  header = TRUE, stringsAsFactors = TRUE)
TabLit <- TabLit %>%
  select(Species = X...Species ,
    Common_name, 
    Level.of.evidence, 
    Location_observation = X..,
    Prey, 
    Species_repartition,
    Paper,
    Approx_lat,
    Approx_long) %>% 
  mutate(Species = sub("\\(.*", "", Species),
         Paper = gsub("^(.*\\d{4}).*$", "\\1", Paper)) # \\d{4} matches four consecutive digits (representing a year),
## export table for archive
subset(TabLit, select = -c(Approx_long, Approx_lat)) %>% 
  arrange(Species) %>% 
  write.csv(. , "Table literature/Table-literature.csv", row.names = FALSE)
## export table for future map
TabLit %>% mutate(Type = "Literature") %>%  
  # filter book of Carvalho-Filho 1993 with Halichoeres poeyi observations that we consider new obs than a literature obs
  # the book was not included in the last paper of Pryor 2023
  subset(Paper != "Carvalho-Filho 1993", select = c(Species, Approx_lat, Approx_long,Type)) %>% 
  write.csv(., "Figure map/Table-literature-for-map.csv", row.names = FALSE)
## export table to Google Sheets
subset(TabLit, select = -c(Approx_long, Approx_lat)) %>% 
  arrange(Species) %>% 
  write_sheet(
    ss = gs4_get("https://docs.google.com/spreadsheets/d/1NG4qFIaKwSTOqqojQhD_AjBGYFvN0_0F5HTm2qap7kI/edit#gid=1361383803"),
    sheet = "Sheet1")


# Table observations
TabObs <- read.csv(unz("Table Observations/Exported table Observations from Notion.zip",
                      "Observations ce91032879d24fd5aef1bc7034e59ab9.csv"), 
                  header = TRUE, stringsAsFactors = TRUE)

## Calculate laterality index
TabObs$Side.striked[TabObs$Side.striked == "no info"] <- NA
TabObs$RL <- as.numeric(substr(TabObs$Side.striked, 1, 1))
TabObs$LR <- as.numeric(substr(TabObs$Side.striked, 6, 6))
TabObs$Laterality.index <- round((TabObs$RL - TabObs$LR) / (TabObs$RL + TabObs$LR),2)
select(TabObs, Side.striked, RL, LR,Laterality.index )

## export table for archive
TabObs <- TabObs %>%
  arrange(X...Species,Video, Video.name.Youtube) %>% 
  mutate(Species = sub("\\(.*", "", X...Species),
         obs = 1:n()) %>% 
  select(obs, # gives an unique ID number for each obs
         Species,
         Common.name = X...Common.name,
         Video.name = Video.name.Youtube,
         Location = Location.more.general,
         Date = Date.observation,
         Observer,
         Level.of.evidence,
         Prey.info.source,
         Prey,
         Anvil.info.source,
         Anvil,
         Fish.life.stage.info.source,
         Fish.life.stage,
         Anvil.use.info.source,
         Type,
         All.anvil.use.captured,
         Duration.event,
         Successful.anvil.use,
         Preferred.side.to.strike,
         Side.striked,
         Laterality.index,
         Other.fish.around.anvils,
         Species.of.fish.around,
         Tool.using.fish.chasing.other.fish = Fish.chasing.other.fish,
         Nb.of.times.prey.dropped = X..of.times.prey.dropped,
         Nb.of.times.prey.spit = X..of.times.prey.spit,
         Nb.anvils = X..anvils,
         Nb.of.strikes,
         Striking.points,
         Nb.anvil.changes = X..anvil.changes, 
         Nb.anvil.stays = X..anvil.stays, 
         Nb.anvil.changes.with.fish.around = X..anvil.changes.with.fish.around, 
         Nb.anvil.stays.with.fish.around = X..anvil.stays.with.fish.around, 
         Approx_lat,
         Approx_long,
         Paper = X...Paper,
         ID) # unique ID observation to link back to Notion table


## export table for archive
subset(TabObs, select = -c(Approx_long, Approx_lat,Paper)) %>% 
  arrange(Species) %>% 
  write.csv(., "Table observations/Table-observations.csv" , row.names = FALSE)

## export table for map
TabObs %>%
  subset(Paper =="") %>% 
  select(c(Species, Approx_long, Approx_lat)) %>% 
  mutate(Type = "New Observation") %>%
  write.csv(., "Figure map/Table-observations-for-map.csv", row.names = FALSE)

## export table to Google Sheets
subset(TabObs, select = -c(Approx_long, Approx_lat, Paper,ID)) %>% 
  arrange(Species) %>% 
  write_sheet(
    ss = gs4_get("https://docs.google.com/spreadsheets/d/1NGtS-gS3mHAnuJx4HcGOtnQD4f3V_zJZ7pslKgBYF2w/edit#gid=0" ),
    sheet = "Sheet1")
  
# Calculation of summary statistics
## Count number of striking points
### Count unique strikes on a striking point '1S' in the table
count_uniqueSP <-  function(x){
    (y <- gregexpr("-1S", x)[[1]])
    (z <- ifelse(y[1]== -1, 0, length(y)))
    return(z)
  }
sum(sapply(TabObs$Striking.points,count_uniqueSP))

### Count when multiple strikes on a striking point
count_multipleSP <-  function(x){
  (y1 <- gregexpr("-1S", x)[[1]])
  (y2 <- gregexpr("-.S", x)[[1]])
  z <- 0
  if(y2[1]!=-1){ # filter unknown or empty elements
    if(y1[1]==-1){ # no 1S in the element 
      z <- length(y2)
    } else {
      z <- length(y2) - length(y1)
    }
  }
  return(z)
}
sum(sapply(TabObs$Striking.points,count_multipleSP))

## Count ratio anvil change
count.ratio.anvil.change <- function(data = TabObs){
temp <- data %>% filter(Nb.anvil.changes !=0) %>% 
  mutate(changes = as.numeric(Nb.anvil.changes),
         stays = as.numeric(Nb.anvil.stays),
         perc.anvil.change = changes / (stays + changes),
         perc.anvil.stays = stays / (stays + changes)) %>% 
  select(changes, stays, perc.anvil.change, perc.anvil.stays)
print(temp)
apply(temp, 2, mean)
}         
count.ratio.anvil.change()

## Correlation nb of anvil changes to duration 
temp <- data.frame(change = TabObs$Nb.anvil.changes, 
                   duration = TabObs$Duration.event,
                   censoring = TabObs$All.anvil.use.captured)
temp <- filter(temp, grepl("\\d+", temp$duration)) # extract values with a numeric
temp
temp$duration <- as.numeric(sub(">", "",temp$duration))
temp$change <- as.numeric(temp$change)
temp$censoring <- recode(temp$censoring, Yes = "No")
str(temp)
ggplot(temp, aes(x = duration, y = change, fill = censoring))+
  geom_point(size =2, pch = 21)+
  theme_bw()+
  ylab("Nb of anvil changes")

## Test laterality.index == 0
t_test <- t.test(temp2$LI, mu = 0)  # Test against the null hypothesis mean of 0
print(t_test)
wilcox_test <- wilcox.test(temp2$LI, mu = 0) # Same but non parametric test
print(wilcox_test)