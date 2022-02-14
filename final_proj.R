library(data.table)
library(tidyverse)
library(ggmap)
library(tidygeocoder)
library(stringr)
library(kableExtra)
library(modelsummary)

dt_exp <- data.table(fread("https://raw.githubusercontent.com/BognarAndras/dv2_Final/main/clean/expeditions.csv"))
dt_member <- data.table(fread("https://raw.githubusercontent.com/BognarAndras/dv2_Final/main/clean/members.csv"))
dt_peaks <- data.table(fread("https://raw.githubusercontent.com/BognarAndras/dv2_Final/main/clean/peaks.csv"))
max(dt_exp$year)

########## 0. MAPS of Peaks above 8k

dt_peaks_temp <-  dt_peaks[ height_metres > 8000 , .( height_metres ,
                                    peak_name ) ]

geocodes <- geocode(dt_peaks_temp, 'peak_name')
geocodes <- data.table(geocodes2)[!is.na(lat) & lat > 27]

bbox <- c(bottom = min(geocodes$lat - 0.5) , 
          left = min(geocodes$long) - 0.05, 
          top = max(geocodes$lat) + 0.5, 
          right = max(geocodes$long) + 0.05)

NE <- get_stamenmap(bbox, zoom = 10, crop = FALSE , maptype = "terrain-background")

geocodes[7]$lat  <-  geocodes[8]$lat - 0.05
geocodes[8]$long  <-  geocodes[8]$long - 0.2
geocodes[11]$long  <-  geocodes[11]$long + 0.2

ggmap(NE) + 
  geom_text( data = geocodes ,
             aes( long  , lat , label = peak_name), size = 4 ,
             fontface = "bold" , colour = "red")  + 
  theme_void() + theme(legend.position = 'none')  

############## EDA

summary(dt_exp)
glimpse(dt_exp)
glimpse(dt_member)
glimpse(dt_peaks)
setwd("C:/Users/abogn/CEU/Study/da2/final_proj")
source("theme_himalaya.R")

############## 1. Distribution of heights -- Bar/histogram

dt_peaks[ , .(height_metres , peak_name )  ][order(height_metres)] 

ggplot( data = dt_peaks , aes( height_metres )  ) +
  geom_histogram(  colour="white" , binwidth = 125 ,  fill = "slateblue3" ) + 
  geom_label(aes(x = max(dt_peaks$height_metres) - 250 , y = 10),
             label = paste0("Mount Everest: " , max(dt_peaks$height_metres)) ,
             size = 4.5, color = 'red', fill = "white") +
  ggtitle("Distribution of Peak Heights") +
  scale_x_continuous( breaks = seq(5400 , 8900, 500)) +
  xlab("Peak Heights (meters)") +
  ylab("Count") +
  theme_Himalaya()

############## 2. Number of members success category -- Density

unique(dt_exp$termination_reason)

exclude <- c( "Did not attempt climb" , "Attempt rumoured"  , "Unknown")
success <-  c("Success (main peak)" , "Success (claimed)")

dt_exp[, ':='(outcome = ifelse(termination_reason %in% success , 
                            "Success" , 
                        ifelse(termination_reason %in% exclude , 
                            dt_exp$termination_reason , 
                        ifelse(termination_reason == "Success (subpeak)" ,
                            "Partial Success" ,
                            "Failure")))) ]

dt_exp$he
dt_exp[dt_peaks, on = 'peak_id', bb := i.b]

dt_exp_outcome <- dt_exp[!(outcome %in% exclude)]
median_crew <- median(dt_exp_outcome$members)

dt_exp_outcome[ , .(.N) , by = members ][members > 29 ][order(members)]
dt_exp_outcome[ , ':='(members = ifelse(members > 29 , 30 , members ) )] 


ggplot( data = dt_exp_outcome , aes( members , fill = factor( outcome ) )) +
  geom_density( alpha = 0.35 ) + 
  geom_segment(data = dt_exp_outcome, aes(x=median(members), xend=median(members),
                                  y=0,
                                  yend=0.16),
               color = "red" ) +
  geom_label(aes(x = median_crew + 10 , y = 0.125),
             label = paste0("Median Crew has " , median_crew , " members.") ,
             size = 4.5, color = 'red', fill = "white") +
  ggtitle("Members of expedition based on outcome*") +
  labs(caption = "*Up to 30 members") +
  guides( fill = guide_legend( title = "Outcome" )) +
  scale_x_continuous( limit = c(0 , 30)) +
  xlab("Number of members") +
  ylab("Density") +
  theme_Himalaya()

############## 3. Age of leader/Number of expedition  success -- Density

median_hired <- median(dt_exp_outcome$hired_staff)

dt_exp_outcome[ , .(.N) , by = hired_staff ][hired_staff > 25 ][order(hired_staff)]
dt_exp_outcome[ , ':='(hired_staff = ifelse(hired_staff > 25 , 25 , hired_staff ) )] 

ggplot( data = dt_exp_outcome , aes( hired_staff , fill = factor( outcome ) )) +
  geom_density( alpha = 0.35 ) + 
  # geom_segment(data = dt_exp_outcome, aes(x=median(hired_staff), 
  #                                         xend=median(hired_staff),
  #                                         y=0,
  #                                         yend=0.16),
  #              color = "red" ) +
  # geom_label(aes(x = median_hired + 7 , y = 0.125),
  #            label = paste0("Median Staff is " , median_hired , ".") ,
             # size = 4.5, color = 'red', fill = "white") +
  ggtitle("Hired staff for expedition based on outcome*") +
  labs(caption = "*Up to 25 staff") +
  guides( fill = guide_legend( title = "Outcome" )) +
  scale_x_continuous( limit = c(0 , 25)) +
  xlab("Number of hired staff") +
  ylab("Density") +
  theme_Himalaya()

############## 31. Outcome violin

dt_exp_outcome <- dt_exp[!(outcome %in% exclude)]
dt_exp_outcome <- dt_exp_outcome[!is.na(height_metres)]

ggplot( data = dt_exp_outcome , 
        aes( factor( outcome  ) , height_metres )) +
  geom_violin(  fill = "slateblue3" , color = "slateblue3" , 
                alpha = 0.5 , size = 0.7  ) +
  xlab("Outcome of expedition") +
  ylab("Target Peak Height") +
  theme_Himalaya()


############## 4. How height/season affects deaths -- scatter + regression

dt_exp[ , ':='(deaths = member_deaths + hired_staff_deaths )]

dt_exp[dt_peaks, on = 'peak_id', height_metres := i.height_metres]
dt_exp_height <-  dt_exp[!is.na(height_metres)]
dt_exp_height <-  dt_exp_height[!(season == "Unknown")]

ggplot( data = dt_exp_height , 
        aes( deaths , height_metres   )) +
  geom_point( alpha = 0.5 ) + 
  geom_smooth( method = 'lm' , data = dt_exp_height ,
               aes( color = factor( season )) , se = FALSE , size = 1.5 ) +
  scale_x_continuous(breaks = seq(0 , max(dt_exp_height$deaths) , 3)) +
  ggtitle("How height and seasons affect fatalities") +
  guides( color = guide_legend( title = "Seasons" )) +
  xlab("Number of fatalities in an expedition") +
  ylab("Target peak height") +
  theme_Himalaya()

dt_exp_outcome[ , .(.N) , by = season ]
dt_exp_outcome[  , .(sum(deaths) , .N) , by = year ][order(year)]
min(dt_exp$year)
############## 5. AVG. #  members by country, maybe top 10 -- boxplot 

dt_exp_outcome <- dt_exp[!(outcome %in% exclude)]
dt_member_leader  <-  dt_member[expedition_role == "Leader"]

exp_ids <- unique(dt_exp_outcome$expedition_id)
duplicate_leaders_expedition <- dt_member_leader[expedition_id %in% exp_ids , .(.N) , by = expedition_id][N > 1][, expedition_id]


duplicate_ids <- dt_member_leader[expedition_id %in% duplicate_leaders_expedition 
                 , .(expedition_id , 
                    member_id)][str_sub(member_id,-2,-1) == "01"][
                      , member_id ]
nonduplicate_ids <-  dt_member_leader[!(expedition_id %in% duplicate_leaders_expedition) ,
                  expedition_id, member_id][, member_id]
unique_ids <- c(duplicate_ids , nonduplicate_ids)

dt_exp_outcome_lead <- dt_exp_outcome[dt_member_leader[member_id %in% unique_ids],
               on = 'expedition_id', leader_country := i.citizenship]

dt_exp_outcome_lead <- dt_exp_outcome_lead[!is.na(leader_country)]


success_countries <- dt_exp_outcome_lead[outcome == "Success" 
                                    , .(number_of_success = .N ) ,
                    by = leader_country][order(-number_of_success)][1:10]$leader_country

success_crew_size <- dt_exp_outcome_lead[leader_country %in% success_countries ,
                    .(crew_size = members + hired_staff , leader_country)]
success_crew_size$leader_country <- factor(success_crew_size$leader_country, 
                                    levels = success_countries)

ggplot( data = success_crew_size , 
        aes( factor( leader_country ) , crew_size  )) +
  geom_boxplot(  fill = "#006D77" , color = "#006D77" , 
                 alpha = 0.5 , outlier.size = 1.5 ) + 
  ggtitle("Top 10 Countries by succesful expeditions") +
  xlab("") +
  ylab("Crew Size") +
  theme_Himalaya()

############## 6. Avg. # injuries + deaths  trekking agency + without -- violin

dt_exp_trek <- dt_exp[ , ':='(trekking_corrected = ifelse(is.na(trekking_agency) |
                           trekking_agency == "None",
                          "No agency" , trekking_agency))] 

dt_exp_injured <- dt_member[ , .(sum(injured)) , by = expedition_id]

dt_trek <-  merge(dt_exp_trek , dt_exp_injured , key = expedition_id)

dt_trek[ , ':='(death_num = member_deaths + hired_staff_deaths  )]
dt_trek_test <- dt_trek[ ,  .(sum(death_num) , .N  ),
         by = .(trekking_corrected)][N > 100][, ':='(death_rate = V1/N)]

top_10_agencies <- dt_trek_test[order(death_rate)][1:10]
no_agency_rating <- dt_trek_test[trekking_corrected == "No agency"]
all_agency_rating <-  dt_trek[trekking_corrected != "No agency",  
                              .(sum(death_num), .N  )][, ':='(
                                trekking_corrected = "All agencies",death_rate = V1/N)]

agency_death_table <- rbind(top_10_agencies , no_agency_rating , all_agency_rating)
agency_death_table[1]$trekking_corrected <- "Mountain Exp."
agency_death_table[2]$trekking_corrected <- "Thamserku Trek."

ggplot( data = agency_death_table , 
        aes( reorder(trekking_corrected , death_rate ), death_rate , 
             fill = ifelse(trekking_corrected %in%
             c("All agencies" , "No agency") , "darkorange3" , "slateblue3" ))) +
  geom_bar( stat = "identity" ) + 
  geom_text(label = agency_death_table$trekking_corrected , angle = 90, hjust = 1.1, 
            size = 4 , colour = "black" ) +
  ggtitle("Top 10 Agencies* by least deaths") +
  labs(caption = "*At least 100 expeditions") +
  xlab("") +
  ylab("Avg. Fatalities per expedition") +
  theme_Himalaya() +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

############## 7. Animation: Top 3 Peaks each year + Nations colored?


dt_exp[ , ':='( decade = floor( year * 0.1 ) * 10 )]
dt_exp[ , .(sum(deaths)) , by = decade ][order(decade)]
dt_exp[ , .(max(height_metres)) , by = decade ][order(decade)]
dt_exp_success <- dt_exp[outcome == "Success"]
dt_exp_success <- dt_exp_success[!is.na(height_metres)]
dt_exp_success_top3 <- dt_exp_success[ year < 1991 , .(
                    top1 = max(height_metres) , 
                    top2 = ifelse(is.na(sort(unique(height_metres),TRUE)[2]),
                           max(height_metres), sort(unique(height_metres),TRUE)[2]),
                    top3 = ifelse(is.na(sort(unique(height_metres),TRUE)[3]),
                           max(height_metres), sort(unique(height_metres),TRUE)[3])), 
                by = year ][order(year)]


dt_exp_success_top3_long <- gather(dt_exp_success_top3 , "rank" , "height" , 2:4)
 
ggplot( data = dt_exp_success_top3_long , aes(  rank , height )  ) +
  geom_bar( stat = "identity" ) + 
  transition_states(year , transition_length = 5, state_length = 2) +
  labs(
    title = paste0("Heighest 3 Peaks reach in Year: " , "{closest_state}")) +
  coord_cartesian(ylim = c(5500 , 9000)) +
  xlab("") +
  ylab("Peak Height") +
  theme_Himalaya() 


  
  scale_y_continuous(limits = c( 5500 , 9000 ) , breaks = seq( 5500 , 9000 , 500 ) )

library(gganimate)


n <- nrow(dt_exp_success)
sort(as.integer(dt_exp_success$height_metres),partial=n-2)[n-2]
sort(unique(dt_exp_success$height_metres),TRUE)[2]


ggplot( data = dt , aes( Price )  ) +
  geom_histogram( colour="white" , binwidth = 31250 ,  fill = "#006D77" ) +
  transition_states(District) +
  labs(
    title = paste0("District " , "{closest_state}"),
    subtitle = paste0('Number of flats: {nrow(subset(dt, District == closest_state))}',
                      '\nMean price: {dt[ District == closest_state , .(as.integer(mean( Price )))]}' , " Ft")) +
  scale_y_continuous(limits = c( 0 , 500 ) , breaks = seq( 0 , 500 , 100 ) ) +
  scale_x_continuous( breaks = seq( 0 , 1000000 , 250000 ) )