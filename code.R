library(rvest)
library(dplyr)


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

url <- "https://www.thebluetestament.com/2019/6/13/18678329/complete-2019-mls-major-league-soccer-salaries-millionaires-sporting-kc"

#Bringing in player salary
salary <- read.table("/Users/Computer/Desktop/MLS19 salary.txt", sep = "\t")
# salary$Player <- paste(salary$V1, salary$V2)
# salary$V1 <- salary$Player
# salary <- salary[, -c(5)]
colnames(salary) <- c("Player", "Team", "Pos", "Salary")

MLS.new <- left_join(MLS, salary, by = "Player")


