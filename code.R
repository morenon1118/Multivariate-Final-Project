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

#export
write.csv(MLS, "/Users/Computer/Documents/2019MLS.csv", row.names = FALSE)
