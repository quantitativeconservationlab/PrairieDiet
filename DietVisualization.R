#########################################################################
## This script was created by Jen Cruz and Andrew Baker to summarize #
# preliminary diet data from 4 nest cameras placed on Prairie Falcon #
# nests during the breeding season in 2023.                          #
#########################################################################
##############################################################
##### Set up your workspace and load relevant packages -----
# install relevant package
# 

# load packages relevant to this script:
library( tidyverse ) #easy data manipulation
options( dplyr.width = Inf, dplyr.print_min = 100 )
#library( lubridate ) #easy date and time manipulation
#library( sf ) #for spatial data
## end of package load ###############

##########################################################
#### Load or create data -----------------------------------------
# Clean your workspace to reset your R environment. #
rm( list = ls() )

# set path to data#
#datapath <- "Z:/Common/PrairieFalcons/DietDataCameras/"
datapath <- "G:/My Drive/QCLabShared/Projects/Prairie Falcons/Diet/CameraDatabases/"

#import csv extracted from access database
dietraw <- read.csv( file = paste0( datapath,"DietPRFANest2023.csv" ),
                     header = TRUE )
#import drop down options used in data table
dropdown <- read.csv(file = paste0( datapath, "Column Lookup Entries.csv"),
                     header = TRUE )

##############################################################
################ clean and manipulate data ##########################
#check diet table
head(dietraw)
str( dietraw)

#check prey list
dropdown

#create cleand diet dataframe
dietdf <- dietraw
#check
head( dietdf);dim(dietdf)
#change column name to match dropdown names
dietdf <- dietdf %>% 
  mutate( ID = Prey.Item ) %>% 
  select( -Prey.Item )
#check
head( dietdf);dim(dietdf)
#join with relevant columns of dropdown
dietdf <- dropdown %>% 
  #select relevant columns that we want to join
  select( ID, Prey.Item ) %>% 
  # join dropdown to dietdf using ID as common column
  right_join( dietdf, by = "ID" )

#check
head( dietdf);dim(dietdf)

#remove superfluous columns 
dietdf <- dietdf %>% 
  select( -Number, -Recorder, -Check )

#create lubridate date
dietdf <- dietdf %>% 
  mutate( date = lubridate::mdy_hm( paste( Date, Time.of.Delivery, sep = " "),
                                    tz = "MST" ) )
#check
head( dietdf);dim(dietdf)
#create date summaries
dietdf <- dietdf %>% 
  mutate( hr = hour(date),
          dayofyear = yday(date) )

#check
head( dietdf);dim(dietdf)
#remove year from nest label
dietdf <- dietdf %>% 
  dplyr::rowwise() %>% 
mutate( Nest = strsplit( Nest.ID, split = "_" )[[1]][1] )
#check
unique(dietdf$Nest)
#add new labels summarizing diet at coarser levels
birdnames <- c( "bird", "horned lark", "starling" )
smallnames <- c( "kangaroo rat", "small mammal" )
dietdf <- dietdf %>% 
  mutate( Prey = ifelse( Prey.Item %in% birdnames, "Bird",
ifelse( Prey.Item %in% smallnames, "Mammal",
ifelse( Prey.Item == "piute ground squirrel", "Ground Squirrel",
  ifelse( Prey.Item == "other", "Reptile", "Unidentified" ) ) ) ) )
#check
tail( dietdf);dim(dietdf)

#now we reorder prey times to match Marzluff et al. 1997 The Condor.
dietdf$Prey <- factor( dietdf$Prey, 
                    levels = c( "Unidentified",  "Reptile","Bird",
                                "Mammal","Ground Squirrel") )


##########
############### visualize data summaries #######################

#bargraphs of prey items by nest
dietdf %>% 
  group_by( Nest ) %>% 
  count( Prey ) %>% 
ggplot(.) +
  theme_bw( base_size = 15 ) +
  geom_bar( aes( x = Nest, y = n, fill = Prey ),
            stat = "identity" )

#create pallete using colors we like
cbpalette <- c( "#56B4E9","#E69F00","#009E73", "#CC79A7","#D55E00" )

#maybe we remove the other and unknowns from the list
#bargraphs of prey items by nest
dietdf %>% 
#  filter( Prey != "other" ) %>% 
  group_by( Nest ) %>% 
  count( Prey ) %>% 
  mutate( perc = n / sum( n ) * 100 ) %>% 
  ggplot(., aes( x = Nest, y = perc, fill = Prey ) ) +
  theme_classic( base_size = 15 ) +
  theme( legend.position = "top" ) +
  geom_bar( stat = "identity" ) + 
  scale_fill_manual(values=cbpalette) +
  geom_text( aes( label = paste0( round( perc,0 ), " % (",
                                  n, ")") ),
             position = position_stack(vjust = 0.5 ) ) +
  labs( x = "Nest ID", y = "Prey items (%)", fill = "Prey groups" )

############### save relevant objects ###########################
#########
### save desired plot
ggsave( paste0( datapath, "Prey_percentage_plot.png"),
            dpi = 500, height = 15, width = 20, units = "cm" )
################## end of script ####################################