###Effect of clinical variables on bacterial abundances

load("data.Rdata")
###Effect of clinical variables on species abundances
library(randomForest)
library(dplyr)
library(reshape2)
library(ggplot2)

species.df <- dcast(data=species.melt, formula = Sample~genus.other, value.var = "value", fun.aggregate = sum)
species.df <- species.df[, - which(colnames(species.df) == "Other")]
species.df$Sample <- as.numeric(species.df$Sample)
species.df <- species.df[order(species.df$Sample),]
all(species.df$Sample == infant.file$Sample)
infant.numbers <- unlist(sapply(X = species.df$Sample, FUN = function(x){return(infant.file[which(infant.file$Sample==as.character(x)),"Patient"])}))
species.df$Patient <- infant.numbers
species.by.infant <- group_by(species.df, Patient) %>%
  summarize_all(.funs = mean)
species.by.infant <- species.by.infant[,-which(colnames(species.by.infant)=="Sample")]
View(species.by.infant)


#delivery mode
delivery.df <- species.by.infant
delivery.status <- unlist(sapply(delivery.df$Patient, FUN = function(x){return(infant.file[which(infant.file$Patient==x)[1],"delivery_mode"])}))
delivery.df$Patient <- delivery.status
delivery.df <- subset(delivery.df, !Patient=='')
delivery.df$Patient <- droplevels(delivery.df$Patient)
rf <- randomForest(Patient~., data = delivery.df)
varImpPlot(rf)


ggplot(data=delivery.df, aes(x=Patient, y= Bacteroides)) +
  geom_jitter()

#feeding
feeding.df <- species.by.infant
feeding.status <- unlist(sapply(feeding.df$Patient, FUN = function(x){return(infant.file[which(infant.file$Patient==x)[1],"feeding_type"])}))
feeding.df$Patient <- feeding.status
rf2 <- randomForest(Patient~., data=feeding.df)
varImpPlot(rf2)

ggplot(data=feeding.df, aes(x=Patient, y= Staphylococcus)) +
  geom_jitter(width=.2)

#Antibiotics
antibiotic.df <- species.by.infant
antibiotic.status <- unlist(sapply(antibiotic.df$Patient, FUN = function(x){return(infant.file[which(infant.file$Patient==x)[1],"Antibiotics"])}))
antibiotic.df$Patient <- antibiotic.status
rf3 <- randomForest(factor(Patient)~., data=antibiotic.df)
varImpPlot(rf3)

ggplot(data=antibiotic.df, aes(x=factor(Patient), y= Bacteroides)) +
  geom_jitter()

