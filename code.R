#FINAL RMD - RAW

library(MASS)
library(rvest)
library(dplyr)

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
MLS <- dat.raw[!duplicated(dat.raw$Player), ]

#clean and refine to find players who scored a goal
MLS$Player <- as.character(MLS$Player)
MLS <- MLS[MLS$Gls. > 0, ]


#export
# write.csv(MLS, "/Users/Computer/Documents/2019MLS.csv", row.names = FALSE)
# write.csv(MLS, "/Users/Computer/Documents/2019MLS.csv", row.names = FALSE)

# url <- "https://www.thebluetestament.com/2019/6/13/18678329/complete-2019-mls-major-league-soccer-salaries-millionaires-sporting-kc"

#Bringing in player salary
salary <- read.table("/Users/Computer/Desktop/MLS19 salary.txt", sep = "\t")
salary$Player <- paste(salary$V1, salary$V2, sep = "-")
salary <- salary[, -c(1, 2, 6)]
colnames(salary) <- c("Team", "Pos", "Salary", "Player")

MLS.new <- left_join(MLS, salary, by = "Player")

### EXPORTED TO EXCEL:
### FILLED IN MISSING SALARIES BY HAND
### REMOVED COLUMNS I DIDN'T WANT
### SIMPLIFIED POSITIONS

write.csv(MLS.new, "/Users/Computer/Documents/MLSraw.csv", row.names = FALSE)
MLS <- read.csv("/Users/Computer/Documents/MLSraw.csv")

#
##
### EDA
##
#

summary(MLS)

# Categoricals
table(MLS$Pos.x)
plot(table(MLS$Squad.x))





#
##
### MODEL BUILD
##
#


#PCA 
mdl.pca <- prcomp(MLS[, -c(1:4)], scale = TRUE)
screeplot(mdl.pca)
mdl.pca

#Cluster
MLS <- MLS[-c(30), ]
rownames(MLS) <- MLS$Player
d <- dist(MLS[, -c(2:4)], method = "euclidean")
mdl.clust <- hclust(d)
plot(mdl.clust)
