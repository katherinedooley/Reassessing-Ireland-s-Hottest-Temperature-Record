###############################################################################
################################### Intoduction  ##############################
# The following script will produce Time series plot, Station Location Map and 
# Frequency Distribution Plots. This script was produced by Ciaran Kelly & Natascha Seifert.
# All data used in this paper is attached as .csv files in Station_Investigation.zip folder. 

###############################################################################
##################################  Time Series  ##############################

# To complie the time series plot tmax data for the month of June 1887 was extracted from a .xsl file 
# named (All_Tmax_data) sourced from knmi explorer and met eireann. 
# The data was then inserted into a new .csv file for ease of use and named (Time_1887.csv"). 
# The folllowing codes produce time series. Install Packages & then load 

# install.packages("gghighlight")
# install.packages("extrafont")
library(ggplot2)
library(reshape2)
library(gghighlight)

# Important .csv file that contains 1887 tmax data and save as a named data file ie:Time
Time <-read.csv("C:/Station_Investigation/Time_1887.csv")
Time <- Time[,1:9]#change number to suit amount of coloums
names(Time)

# Reshape 1887 tmax data from wide to long
df.melt <- melt(Time, id.vars=c("Year"))
head(df.melt)

# Plot Time series for 1887
df.melta <- ggplot(df.melt, aes(x=Year , y=value, col=variable)) +
  geom_line()+
  xlab('June') +
  ylab('Celsius')+
  scale_y_continuous(limits = c(10,36), breaks=seq(10,36,2)) +
  scale_x_continuous(limits = c(1,31), breaks=seq(1,31,1))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Max Temperature 1887") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Arial",face="bold", size=9))+ geom_point(aes(x=26, y=33.3), 
             shape = 21, colour = "black", fill = "red", size = 1.5, stroke =1.5)

# Overlay point data frame A ontop of data frame gg1 and save in plots folder with a res=300

setwd("C:/Station_Investigation/plots")
png("df.melta.png", width = 12, height = 9, units = 'in', res = 300)
plot(df.melta)
dev.off()

###############################################################################
########################### Station Location Map ##############################
###############################################################################
###############################################################################

# The following codes will produce a Station Location Map for all the station used in this reserch. 

# Install pakages 
library(ggspatial)
library(sf)
library(rnaturalearth)       
library(rnaturalearthdata)
library("ggplot2")
library("rgeos")

# Load data frame from the (rnaturalearthdata) library
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

# generate world map set the xlim & ylim to your coordinates and save as a named data file ie: gg1

gg1 <-ggplot(data=world)+
  geom_sf(data = world, colour = "black", fill = "seagreen2") +
  labs( x = "Longitude", y = "Latitude") +
  ggtitle("Station Location 1887") +
  coord_sf(xlim = c(-11, 2), ylim = c(50,60), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tl", which_north = "true",  
                         height = unit(2.5, "cm"),
                         width = unit(2.5, "cm"),
                         pad_x = unit(0.01, "in"), pad_y = unit(0.22, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(text=element_text(family="Arial",face="bold", size=9))
theme_bw()

# Create point data to be overlayed on the map as a named data frame called A

A <- data.frame(
  long = c(-6.65, -8.47, -6.3192, -7.88, -8.2208,-1.49, -7.254),
  lat = c(54.35, 54.13, 53.3639, 53.08,51.7953,53.381, 52.661 ),
  Stations = c("Armagh","Markree","Phoenix Park","Birr", "Roches Point","Sheffield", "Kilkenny"),
  stringsAsFactors = FALSE
)  

# Overlay point data frame A ontop of data frame gg1

gg1 + 
  geom_point(data = A, aes(x = long, y = lat, color=Stations) , size = 2.5)

# Overlay point data frame A ontop of data frame gg1 and save in plots folder with a res=300

setwd("C:/Station_Investigation/plots")
png("gg1.png", width = 9, height = 12, units = 'in', res = 300)
plot(gg1 + 
    geom_point(data = A, aes(x = long, y = lat, color=Stations) , size = 2.5))
dev.off()

###############################################################################
########################### Station Investigation Histogram ###################
###############################################################################
###############################################################################


# To following codes complie the Frequency Distribution Plots 

#preamble: load package, clear variables, setwd
library(tidyverse)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

# (not essential) clear variables and any open plots before starting
graphics.off()
rm(list=ls())

# Set your working directory
setwd("C:/")

# Makree
M <-read.csv("C:/Station_Investigation/markree.csv")
Mhist <- ggplot(M, aes(ï..Markree)) +
  geom_histogram() +labs(y= "Frequency", x = "Markree (JJA, 2010-2019)") +
  geom_point(aes(x=9.9, y=2.5), 
             shape = 21, colour = "black", fill = "red", size = 3.5, stroke =1.5)+ 
theme(text=element_text(family="Arial",face="bold", size=9))
theme_bw()

# Sheffield
S <-read.csv("C:/Station_Investigation/sheffield.csv")
Shist <- ggplot(S, aes(ï..sheffield)) +
  geom_histogram() + labs(y= "Frequency", x = "Sheffield (JJA, 2010-2017)") +
  geom_point(aes(x=9.4, y=2.5), 
             shape = 21, colour = "black", fill = "red", size = 3.5, stroke =1.5)+ 
  theme(text=element_text(family="Arial",face="bold", size=9))
theme_bw()

# PP
P <-read.csv("C:/Station_Investigation/pp.csv")
Phist <- ggplot(P, aes(ï..phoenixpark)) +
  geom_histogram() +labs(y= "Frequency", x = "Phoenix Park (JJA, 2013-2019)") + 
  geom_point(aes(x=9, y=2.5), 
             shape = 21, colour = "black", fill = "red", size = 3.5, stroke =1.5)+ 
  theme(text=element_text(family="Arial",face="bold", size=9))
theme_bw()

# Roches Roint
R <-read.csv("C:/Station_Investigation/rpoint.csv")
Rhist <- ggplot(R, aes(ï..rochespoint)) +
  geom_histogram()  +labs(y= "Frequency", x = "Roches Point (JJA, 2010-2019)") + 
  geom_point(aes(x=6.6, y=2.5), 
             shape = 21, colour = "black", fill = "red", size = 3.5, stroke =1.5)+ 
  theme(text=element_text(family="Arial",face="bold", size=9))
theme_bw()

# Armagh
A <-read.csv("C:/Station_Investigation/armagh.csv")
Ahist <- ggplot(A, aes(ï..Armagh)) +
  geom_histogram()  +labs(y= "Frequency", x = "Armagh (JJA, 2010-2018)") + 
  geom_point(aes(x=6.4, y=2.5), 
             shape = 21, colour = "black", fill = "red", size = 3.5, stroke =1.5)+ 
  theme(text=element_text(family="Arial",face="bold", size=9))
theme_bw()

### combine them together
library(ggplot2)
grid.arrange(Mhist, Shist, Phist, Rhist, Ahist, nrow = 2, top = "Frequency Distribution in Tmax(Â°C) between Kilkenny and Markree, Sheffield, Phoenix Park, Roches Point, Armagh for June, July and August")+ 
  theme(text=element_text(family="Arial",face="bold", size=9))

setwd("C:/Station_Investigation/plots")
png("FD.png", width = 9, height = 12, units = 'in', res = 300)
FD <-plot(grid.arrange(Mhist, Shist, Phist, Rhist, Ahist, nrow = 2, top = "Frequency Distribution in Tmax(Â°C) between Kilkenny and Markree, Sheffield, Phoenix Park, Roches Point, Armagh for June, July and August")+ 
       theme(text=element_text(family="Arial",face="bold", size=9)))
dev.off()

###############################################################################
################################ End ##########################################