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
setwd("/Users/natascha/Documents/MSc Climate Change/GY652 - Applied Climate Sciences/Semester 1/Part 1/Data/Updated Histogram/R Histogram")


# Makree

M <-read.csv("/Users/natascha/Documents/MSc Climate Change/GY652 - Applied Climate Sciences/Semester 1/Part 1/Data/Updated Histogram/R Histogram/makree.csv")

Mhist <- ggplot(M, aes(Makree)) +
  geom_histogram() +labs(y= "Frequency", x = "Markree (JJA, 2010-2019)") +
  geom_point(aes(x=9.9, y=2.5), 
             shape = 21, colour = "black", fill = "red", size = 3.5, stroke =1.5)

Mhist                    

# Sheffield

S <-read.csv("/Users/natascha/Documents/MSc Climate Change/GY652 - Applied Climate Sciences/Semester 1/Part 1/Data/Updated Histogram/R Histogram/sheffield.csv")

Shist <- ggplot(S, aes(sheffield)) +
  geom_histogram() + labs(y= "Frequency", x = "Sheffield (JJA, 2010-2017)") +
  geom_point(aes(x=9.4, y=2.5), 
             shape = 21, colour = "black", fill = "red", size = 3.5, stroke =1.5)
Shist

# PP

P <-read.csv("/Users/natascha/Documents/MSc Climate Change/GY652 - Applied Climate Sciences/Semester 1/Part 1/Data/Updated Histogram/R Histogram/pp.csv")

Phist <- ggplot(P, aes(phoenixpark)) +
  geom_histogram() +labs(y= "Frequency", x = "Phoenix Park (JJA, 2013-2019)") + 
  geom_point(aes(x=9, y=2.5), 
             shape = 21, colour = "black", fill = "red", size = 3.5, stroke =1.5)
Phist

# Roches Roint
R <-read.csv("/Users/natascha/Documents/MSc Climate Change/GY652 - Applied Climate Sciences/Semester 1/Part 1/Data/Updated Histogram/R Histogram/rpoint.csv")


Rhist <- ggplot(R, aes(rochespoint)) +
  geom_histogram()  +labs(y= "Frequency", x = "Roches Point (JJA, 2010-2019)") + 
  geom_point(aes(x=6.6, y=2.5), 
             shape = 21, colour = "black", fill = "red", size = 3.5, stroke =1.5)
Rhist

# Armagh
A <-read.csv("/Users/natascha/Documents/MSc Climate Change/GY652 - Applied Climate Sciences/Semester 1/Part 1/Data/Updated Histogram/R Histogram/armagh.csv")

Ahist <- ggplot(A, aes(Armagh)) +
  geom_histogram()  +labs(y= "Frequency", x = "Armagh (JJA, 2010-2018)") + 
  geom_point(aes(x=6.4, y=2.5), 
             shape = 21, colour = "black", fill = "red", size = 3.5, stroke =1.5)
Ahist

### combine them together
library(ggplot2)
grid.arrange(Mhist, Shist, Phist, Rhist, Ahist, nrow = 2, top = "Frequency Distribution in Tmax(°C) between Kilkenny and Markree, Sheffield, Phoenix Park, Roches Point, Armagh for June, July and August")


