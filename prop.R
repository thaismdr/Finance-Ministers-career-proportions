## R Code for the Finance Ministers' career proportions with a radar plot ##
## May, 14, 2018 ##

# This is to read in the data and save it as prop_1889-1930, along with the mean for the period # #
prop_1889 <- read.csv2(file = "~/Dropbox/Projetos_R/prop_periodos/1889-1930.csv", header = T, sep = ",")
media1 <- read.csv2(file = "~/Dropbox/Projetos_R/prop_periodos/media_1889-1930.csv", header = T, sep = ",")

# This is to read in the data and save it as prop_1930-1964 #
prop_1930 <- read.csv2(file = "~/Dropbox/Projetos_R/prop_periodos/1930-1964.csv", header = T, sep = ",")

# This is to read in the data and save it as prop_1964-2015 #
prop_1964 <- read.csv2(file = "~/Dropbox/Projetos_R/prop_periodos/1964-2015.csv", header = T, sep = ",")

# Attach the data 
attach(prop_1889)
attach(media1)
attach(prop_1930)
attach(prop_1964)

# This is to set the working directory
# First, save the path to the WD in the object ?ProjectProp?
ProjectProp <- "~/Dropbox/Projetos_R/prop_periodos"
# Then set the WD using this object
setwd(ProjectProp)

# Save workspace
save.image(file = "~/Dropbox/Projetos_R/prop_periodos/ProjectProp.Rdata")

# Load packages
library(ggplot2)
library(reshape2)
library(gghighlight)

# Set the data as long
primeiro_melted <- melt(prop_1889, id.vars = "nome")
segundo_melted <- melt(prop_1930, id.vars = "nome")
terceiro_melted <- melt(prop_1964, id.vars = "nome")


# Set the radar coordinates #

coord_radar <- function (theta = "x", start = 0, direction = 1)
{
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x")
    "y"
  else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start,
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

# Create the objects for the plots #

radarprimeiro <- ggplot(primeiro_melted, aes(x = variable, y = value))
radarsegundo <- ggplot(segundo_melted, aes(x = variable, y = value))
radarterceiro <- ggplot(terceiro_melted, aes(x = variable, y = value))

# Set the plots #

attach(prop_1889)
plotum <- radarprimeiro + geom_polygon(aes(group = as.factor(nome), colour = as.factor(nome)), fill = NA, size = .1, show.legend = FALSE, colour = "#000000") +
  geom_line(aes(group = as.factor(nome), colour = as.factor(nome)), size = .1, colour = "#000000") + 
  xlab("") + ylab("") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_radar()

attach(prop_1930)
plotdois <- radarsegundo + geom_polygon(aes(group = as.factor(nome), colour = as.factor(nome)), fill = NA, size = .1, show.legend = FALSE, colour = "#000000") +
  geom_line(aes(group = as.factor(nome), colour = as.factor(nome)), size = .1, colour = "#000000") + 
  xlab("") + ylab("") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_radar()

attach(prop_1964)
plottres <- radarterceiro + geom_polygon(aes(group = as.factor(nome), colour = as.factor(nome)), fill = NA, size = .1, show.legend = FALSE, colour = "#000000") +
  geom_line(aes(group = as.factor(nome), colour = as.factor(nome)), size = .1, colour = "#000000") + 
  xlab("") + ylab("") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_radar()

# Load gridExtra to arrange all plot in a single page
library(gridExtra)

# Arrange plots
grid.arrange(plotum, plotdois, plottres, nrow = 3)