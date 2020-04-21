#FINAL RMD - RAW

library(MASS)
library(rvest)
library(dplyr)
library(ggplot2)

#
##
### DATA PREP
##
#

#upsert
dat.std <- read.csv("/Users/Computer/Downloads/MLS data standard.csv")
dat.gls <- read.csv("/Users/Computer/Downloads/MLS data goals.csv")
dat.pos <- read.csv("/Users/Computer/Downloads/MLS data posession.csv")

#remove ID
dat.std <- dat.std[, -c(1)]
dat.gls <- dat.gls[, -c(1)]
dat.pos <- dat.pos[, -c(1)]

#merge
dat.raw <- merge(dat.std, dat.gls, by = c("Player"))
dat.raw <- merge(dat.raw, dat.pos, by = c("Player"))

#dedupe
MLS.raw <- dat.raw[!duplicated(dat.raw$Player), ]

#clean and refine to find players who have played over 1500 minutes
MLS.raw$Player <- as.character(MLS.raw$Player)
MLS.raw <- MLS.raw[MLS.raw$Min > 1500, ]


#export
# write.csv(MLS, "/Users/Computer/Documents/2019MLS.csv", row.names = FALSE)

# url <- "https://www.thebluetestament.com/2019/6/13/18678329/complete-2019-mls-major-league-soccer-salaries-millionaires-sporting-kc"

#Bringing in player salary
salary <- read.table("/Users/Computer/Desktop/MLS19 salary.txt", sep = "\t")
salary$Player <- paste(salary$V1, salary$V2, sep = "-")
salary <- salary[, -c(1, 2, 6)]
colnames(salary) <- c("Team", "Pos", "Salary", "Player")

MLS.raw <- left_join(MLS.raw, salary, by = "Player")

#Export for final cleaning touches
write.csv(MLS.raw, "/Users/Computer/Documents/MLSraw.csv", row.names = FALSE)
### EXPORTED TO EXCEL
### FILLED IN MISSING SALARIES BY HAND
### REMOVED COLUMNS I DIDN'T WANT
### SIMPLIFIED POSITIONS TO D, M, M-F, F

#Bring back
MLS <- read.csv("/Users/Computer/Documents/MLSraw.csv")


#
##
### EDA
##
#

#Look into Positions
summary(MLS$Pos)
# We have four main positions:
# D - center backs and full backs
# M - Defensive midfielders and true midfielders
# M-F - attacking midfielders and wingers
# F - Strikers

#Goals+assists/90 by Targets
ggplot(MLS, aes(G.A.90, Targ, color = as.factor(MLS$Pos))) + geom_point()

#Salary by Goals+assists/90
ggplot(MLS, aes(G.A.90, Salary, color = as.factor(MLS$Pos))) + geom_point()


#
##
### MODEL BUILDS
##
#

## Cluster
#Scale
MLS.scale <- MLS[, -c(1, 2, 3, 4, 25)] #rm all categoricals and salary
MLS.scale <- scale(MLS.scale, center = T, scale = T) #standardize remaining metrics


#Cluster on production metrics
rownames(MLS.scale) <- MLS$Player
d <- dist(MLS.scale, method = "euclidean")
mdl.clust <- hclust(d)
plot(mdl.clust, cex = .75)

#Applying groups to dataset
k = 4
clusters <- as.data.frame(cutree(mdl.clust, k=k))
colnames(clusters) <- c("bin")
MLS$Cluster <- clusters$bin
rect.hclust(mdl.clust, k=k, border="orange")



## PCA 
mdl.pca <- prcomp(MLS[, -c(1:4)], scale = TRUE)
screeplot(mdl.pca)
mdl.pca



## LDA

