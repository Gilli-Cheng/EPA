origin <- setwd("D:/Rstudio/workplace/datasciencecoursera/Exploratory-Data-Analysis/EPA")
getwd()

if(!file.exists("data")){
        dir.create("data")
}
setwd("./data")

# url3 <- "https://github.com/bcaffo/courses/raw/master/04_ExploratoryAnalysis/CaseStudy/pm25_data.zip"
# 
# filename3 <- "pm25_data.zip"
# if(!file.exists(filename3)){
#         download.file(url = url3,destfile = filename3,method = "curl")
#         unzip(zipfile = filename3)
# }
# list.files()

# library("data.table")
# pm0 <- data.table::fread(input = "./RD_501_88101_1999-0.txt",
#                          header = FALSE, sep = "|", 
#                          na.strings = "")[!grepl( "^#", V1 )]
# rm(list = "pm0")
# dim(pm0)
# 
# pm1 <- data.table::fread(input = "./RD_501_88101_2012-0.txt",
#                          header = FALSE, sep = "|", 
#                          na.strings = "")

pm0 <- read.table(file = "./RD_501_88101_1999-0.txt",
                  header = FALSE, sep = "|",na.strings = "", 
                  comment.char = "#")

pm1 <- read.table(file = "./RD_501_88101_2012-0.txt", 
                  header = FALSE, sep ="|", na.strings = "",
                  comment.char = "#")

dim(pm0);head(pm0)
cnames <- readLines("./RD_501_88101_1999-0.txt",1)
cnames
str(cnames)
cnames <- strsplit(cnames,split = "|",fixed = TRUE)
str(cnames)
cnames[[1]]
#gsub(" ",".",cnames[[1]])
names(pm0) <- cnames[[1]]
head(pm0)
names(pm0) <- make.names(cnames[[1]])
head(pm0)

x0 <- pm0$Sample.Value
class(x0)
str(x0)
summary(x0)
mean(is.na(x0))

dim(pm1);head(pm1)
cnames1 <- readLines("./RD_501_88101_2012-0.txt",1)
cnames1
cnames1 <- strsplit(cnames1, split = "|", fixed = TRUE)
cnames1
names(pm1) <- cnames1[[1]]
names(pm1)
names(pm1) <- make.names(names(pm1))
names(pm1)

x1 <- pm1$Sample.Value
class(x1)
str(x1)
summary(x1)
mean(is.na(x1))
table(is.na(x1))

summary(x0)
summary(x1)
boxplot(x0,x1)
boxplot(log10(x0), log10(x1))
summary(x1)
negative <- x1 < 0
table(negative)
str(negative)
sum(negative, na.rm = TRUE)
mean(negative, TRUE)

dates <- pm1$Date
str(dates)
dates <- as.Date(dates,"%Y%m%d")
dates <- as.Date(as.character(dates),"%Y%m%d")
str(dates)
hist(dates,"month")
hist(dates,12)
hist(dates[negative],12)

# site0 <- subset(pm0, pm0$State.Code == 36)
# any intersect in NY state?
site0 <- unique(subset(pm0, pm0$State.Code == 36, c(County.Code, Site.ID)))
site1 <- unique(subset(pm1, pm1$State.Code == 36, c(County.Code, Site.ID)))

head(site0)
site0 <- paste(site0[, 1], site0[, 2],sep = ".")
site1 <- paste(site1[, 1], site1[, 2],sep = ".")
head(site0); head(site1)
both <- intersect(site0, site1)
str(both)

# handlie the origin data
pm0$County.Site <- with(pm0, paste(pm0$County.Code, pm0$Site.ID, sep = "."))
pm1$County.Site <- with(pm1, paste(pm1$County.Code, pm1$Site.ID, sep = "."))
ny0 <- subset(pm0,State.Code == 36 & County.Site %in% both)
ny1 <- subset(pm1, State.Code == 36 & County.Site %in% both)
str(ny0)
sapply(split(ny0,ny0$County.Site),nrow)
sapply(split(ny1,ny1$County.Site),nrow)
# county.Site 63.2008

pm0sub <- subset(pm0,pm0$State.Code == 36 & pm0$County.Code == 63 & pm0$Site.ID == 2008)
pm1sub <- subset(pm1,pm1$State.Code == 36 & pm1$County.Code == 63 & pm1$Site.ID == 2008)
dim(pm0sub)
dim(pm1sub)

# 1999
dates0 <- pm0sub$Date
x0sub <- pm0sub$Sample.Value
plot(dates0,x0sub)
class(dates0)
dates0 <- as.Date(as.character(dates0),"%Y%m%d")
plot(dates0,x0sub)

# 2012
dates1 <- pm1sub$Date
x1sub <- pm1sub$Sample.Value
plot(dates1,x1sub)
class(dates1)
dates1 <- as.Date(as.character(dates1),"%Y%m%d")
plot(dates1,x1sub)

dev.off()
#windows(480,480)
par(mfrow = c(1,2),mar = c(4,2,2,1))
plot(dates0, x0sub)
abline(h = median(x0sub,na.rm = TRUE),col = "blue")
plot(dates1, x1sub)
abline(h = median(x1sub,na.rm = TRUE),col = "blue")

range(x0sub, x1sub, na.rm = TRUE)
rng <- range(x0sub, x1sub, na.rm = TRUE)
plot(dates0, x0sub, ylim = rng, pch = 19)
abline(h = median(x0sub,na.rm = TRUE),col = "blue")
plot(dates1, x1sub, ylim = rng, pch = 19)
abline(h = median(x1sub,na.rm = TRUE),col = "blue")

head(pm0)
mn0 <- with(pm0,tapply(Sample.Value,State.Code,mean,na.rm = TRUE))
str(mn0)
head(mn0)
summary(mn0)

mn1 <- with(pm1,tapply(Sample.Value, State.Code, mean,na.rm = TRUE))
class(mn1)
summary(mn1)
names(mn0)

d0 <- data.frame(state = names(mn0),value = mn0)
d1 <- data.frame(state = names(mn1),value = mn1)
head(d0);head(d1)
mrg <- merge(d0,d1,by = "state",)
head(mrg)
str(mrg)

par(mfrow = c(1,1))
?rep
plot(rep(1999,52),mrg$value.x,xlim = c(1998,2013))
points(rep(2012,52),mrg$value.y)

segments(x0 = rep(1999,52),y0 = mrg[,2],x1 = rep(2012,52), y1 = mrg[,3])

mrg[,4] <- mrg[,3]-mrg[,2]
mrg[,5] <- mrg[,4]>0
mrg[,5] <- as.factor(mrg[,5])
head(mrg)
table(mrg[,5])
segments(x0 = rep(1999,52),y0 = mrg[,2],x1 = rep(2012,52), y1 = mrg[,3],col = mrg$V5)