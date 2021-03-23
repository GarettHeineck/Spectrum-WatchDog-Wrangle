#### Script for wrangling WatchDog data logger data
#### Depending on the data logger model and age this may need some modification
## Required packages
require(ggplot2)
require(readr)
require(tibbletime)
require(purrr)
require(lubridate)
require(forcats)
require(dplyr)
require(tidyverse)
## ggplot2 theme for faster implementation of pretty plotting 
WatchDog_theme<- function(base_size = 20) {
  theme_minimal(base_size = base_size) %+replace%
    theme(strip.background = element_rect(fill = "grey85", color = "black", linetype = 1),
          legend.background =  element_rect(fill = "white", linetype = 0),
          legend.position = "bottom",
          panel.grid.major.y = element_line(linetype = "dotted", color = "grey40", size = .3),
          panel.grid.major.x = element_line(linetype = "dotted", color = "grey40", size = .3),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(fill = alpha("white", 0), color = "grey85"),
          axis.line = element_line(size = 1.0, colour = "grey80"),
          complete = TRUE)
}





## Enter file path where WatchDog data is stored
## NOTE: THIS WILL NEED TO BE CHANGED
dir.path<- "/Users/garettheineck/OneDrive - Washington State University (email.wsu.edu)/WSU projects/Lentil pathology/Material and methods/LSPresFRR/LSPresFRR_envir.data"





## FOR ONE FILE AT A TIME
## Select a single file is of interest
selected.file<- list.files(dir.path, 
                           pattern = "*.TXT", #could be .txt or .TXT***
                           full.names = T
                           )[1] #for EXAMPLE, selecting the first file***
## Read file and add columns for accumulated minutes, days, and hours
watchDog.single<- read_tsv(selected.file,
                        skip=3) %>%
  rename(date_time = `Date and Time`) %>%
  mutate(date_time =as_datetime(date_time), #making sure date_time is specified as time variable
         start_date=as_datetime(rep(date_time[1])), #adding temporary column to calculate accumulated time***
         accum_min = difftime(date_time, start_date, units = 'mins'),
         accum_hrs = round(difftime(date_time, start_date, units = 'hours'), 
                           1), #change the rounding if needed***
         accum_day = round(difftime(date_time, start_date, units = 'days'), 2)) 





## FOR MULTIPLE FILES AT ONCE
## NOTE: this can get troublesome if multiple models of WatchDogs were used
all.files<- dir(dir.path, pattern = "*.TXT")
## Read multiple files from the 'all.files' object
watchDog.multi <- tibble(filename = all.files) %>% # create a data frame
  # holding the file names
  mutate(file_contents = map(filename,          # read files into a new data column
                             ~ read_tsv(file.path(dir.path, .),
                                        skip = 3)) #skiop three rows in the orinigal watch dog data
  )  %>%
  unnest() %>%
  rename(date_time=`Date and Time`) %>%
  group_by(filename) %>% #group by each file to calculate cumulative time
  mutate(start_date = rep(date_time[1]), #adding temporary column to calculate accumulated time***
         accum_min = difftime(date_time, start_date, units = 'mins'),
         accum_hrs = round(difftime(date_time, start_date, units = 'hours'), 
                           1), #change the rounding if needed***
         accum_day = round(difftime(date_time, start_date, units = 'days'), 2)) %>%
  ungroup()





## Filter between dates can be useful, however this isn't used in the rest of the script
as_tbl_time(watchDog.single, #use tibble time for easier date manipulation***
                          index = date_time) %>%
  filter_time('2020-09-30 18:00:00' ~ '2020-09-30 22:30:00') #for EXAMPLE, copy and paste start and end dates***
## OR 
filter(watchDog.single, accum_day > 1 & accum_day < 2) #for EXAMPLE, or just use the accumulated time points***





## Changing from wide format to long format 
## This is useful when there multiple sensors are collecting data on the same logger
watchDog.single_long<- watchDog.single %>%
  select(-c(DEW,HMD, #dropping dew point and humidity***
            start_date, #dropping start date column***
            TMPD) #TMP D is not useful in this data set
         ) %>% 
  pivot_longer(!c(date_time, accum_min, accum_hrs, accum_day),
               names_to = "position",
               values_to = "temperature")
## OR for multiple loggers
watchDog.multi_long<- watchDog.multi %>%
  select(-c(DEW,HMD, #dropping dew point and humidity***
            start_date, #dropping start date column***
            TMPD) #TMP D is not useful in this data set
  ) %>% 
  pivot_longer(!c(date_time, accum_min, accum_hrs, accum_day,
                  filename), #NOTE this was added to accommodate multiple loggers***
               names_to = "position",
               values_to = "temperature")





## Plotting data for a SINGLE data logger
watchDog.single_long$position<- fct_recode(watchDog.single_long$position, #changing position names in the data frame***
                                           "Under Containers"="TMPA", #renaming facet panels***
                                           "Above Canopy"="TMP")

ggplot(watchDog.single_long) +
  facet_wrap(~position)+
  geom_area(mapping = aes(x=accum_hrs,
                          y=temperature, 
                          color=position,
                          fill=position,
                          ),
            alpha=0.5) +
  scale_color_manual(values = c("#00B2E2", "#F18B21")) +
  scale_fill_manual(values = c("#00B2E2", "#F18B21")) +
  labs(x="Hours",
       y="Temperature (C)",
       fill="Position",
       color="Position")+
  coord_cartesian(ylim = c(10, 30),
                  xlim = c(0,520)
                  ) +
  WatchDog_theme(base_size = 22)




## Plotting data for a Multiple data logger
watchDog.multi_long$position<- fct_recode(watchDog.multi_long$position, #changing position names in the data frame***
                                           "Under Containers"="TMPA", #renaming facet panels***
                                           "Above Canopy"="TMP")
watchDog.multi_long$filename<- fct_recode(watchDog.multi_long$filename, #changing filename names in the data frame***
                                     "Experiment 1"="LSPresFRR_envit.data_01.TXT", #renaming facet panels***
                                     "Experiment 2"="LSPresFRR_envit.data_02.TXT")

ggplot(watchDog.multi_long) +
  facet_grid(filename~position)+ #NOTE the change here to accommodate the different loggers
  geom_area(mapping = aes(x=accum_hrs,
                          y=temperature, 
                          color=position,
                          fill=position
  ),
  alpha=0.5) +
  scale_color_manual(values = c("#00B2E2", "#F18B21")) +
  scale_fill_manual(values = c("#00B2E2", "#F18B21")) +
  labs(x="Hours",
       y="Temperature (C)",
       fill="Position",
       color="Position")+
  coord_cartesian(ylim = c(10, 30),
                  xlim = c(0,520)
  ) +
  WatchDog_theme(base_size = 22)



