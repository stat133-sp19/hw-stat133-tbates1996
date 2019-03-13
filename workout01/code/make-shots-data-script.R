library(dplyr)
library(plyr)

colClass <-c("character", "character", "integer", "integer",
             "integer","integer","factor","factor","factor",
             "integer","character","integer","integer")

setwd("BerkeleyCS/stat133/workout/workout01/code")
curry <- read.csv("../data/stephen-curry.csv",stringsAsFactors = FALSE,colClasses = colClass)
durant <- read.csv("../data/kevin-durant.csv",stringsAsFactors = FALSE,colClasses = colClass)
green <- read.csv("../data/draymond-green.csv",stringsAsFactors = FALSE,colClasses = colClass)
thompson <- read.csv("../data/klay-thompson.csv",stringsAsFactors = FALSE,colClasses = colClass)
iguodala <- read.csv("../data/andre-iguodala.csv",stringsAsFactors = FALSE,colClasses = colClass)

curry$name = c("Stephen Curry")
durant$name = c("Kevin Durant")
green$name = c("Draymond Green")
thompson$name = c("Klay Thompson")
iguodala$name = c("Andre Iguodala")
summary(curry)

curry$shot_made_flag <- revalue(curry$shot_made_flag, 
                                c("y" = "shot_yes", "n" = "shot_no"))
durant$shot_made_flag <- revalue(durant$shot_made_flag, 
                                 c("y" = "shot_yes", "n" = "shot_no"))
green$shot_made_flag <- revalue(green$shot_made_flag, 
                                c("y" = "shot_yes", "n" = "shot_no"))
thompson$shot_made_flag <- revalue(thompson$shot_made_flag, 
                                   c("y" = "shot_yes", "n" = "shot_no"))
iguodala$shot_made_flag <- revalue(iguodala$shot_made_flag, 
                                   c("y" = "shot_yes", "n" = "shot_no"))

curry$minute = (curry$period - 1) * 12 + (12 - curry$minutes_remaining)
durant$minute = (durant$period - 1) * 12 + (12 - durant$minutes_remaining)
green$minute = (green$period - 1) * 12 + (12 - green$minutes_remaining)
thompson$minute = (thompson$period - 1) * 12 + (12 - thompson$minutes_remaining)
iguodala$minute = (iguodala$period - 1) * 12 + (12 - iguodala$minutes_remaining)

sink("../output/stephen-curry-summary.txt")
summary(curry)
sink()

sink("../output/kevin-durant-summary.txt")
summary(durant)
sink()

sink("../output/draymond-green-summary.txt")
summary(green)
sink()

sink("../output/klay-thompson-summary.txt")
summary(thompson)
sink()

sink("../output/andre-iguodala-summary.txt")
summary(iguodala)
sink()

team <- rbind(curry, durant, green, thompson, iguodala)
write.csv(team, "../data/shots-data.csv")

sink("../output/shots-data-summary.txt")
summary(team)
sink()