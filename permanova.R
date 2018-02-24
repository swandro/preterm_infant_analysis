###PERMANOVA
load("data.RData")

#Bacteria
max(species.file$`Unassigned;Other;Other;Other;Other;Other`)

species.melt$permanova <- make.other.genus(species.melt, number=30)
species.df.perma <- subset(species.melt, !permanova%in%c("Other", "f_NA"))
species.df.perma <- dcast(species.df.perma, formula = Sample~permanova, value.var = "value", fun.aggregate = sum)
species.df.perma$Patient <- unlist(sapply(X=species.df.perma$Sample, FUN=function(x){return(infant.file[which(infant.file$Sample==x),"Patient"])}))
species.df.perma.patient <- group_by(species.df.perma, Patient) %>%
  summarize_all(.funs = mean)
View(species.df.perma.patient)
species.df.perma.patient <- species.df.perma.patient[, -which(colnames(species.df.perma.patient)=="Sample")]

apply(species.df.perma.patient[,-1],1,sum)


library(vegan)


#Health
temp <- subset(species.df.perma.patient, !Patient==17)
health <- sapply(X = temp$Patient, FUN = function(x){return(infant.file[which(infant.file$Patient==x)[1], "Group"])})

adonis(formula = temp[,-1]~ health)
#R2 = 0.079
#p= 0.64

#Delivery
temp <- subset(species.df.perma.patient, !Patient==32)
delivery <- sapply(X = temp$Patient, FUN = function(x){return(infant.file[which(infant.file$Patient==x)[1], "delivery_mode"])})
adonis( formula = temp[,-1]~ delivery )
#R2 = 0.12
#p = 0.046

#feeding
feed <- sapply(X = species.df.perma.patient$Patient, FUN = function(x){return(infant.file[which(infant.file$Patient==x)[1], "feeding_type"])})
adonis( formula = species.df.perma.patient[,-1]~ feed )
#R2 = 0.07
#p = 0.076

#antibiotics
antibiotics <- sapply(X = species.df.perma.patient$Patient, FUN = function(x){return(infant.file[which(infant.file$Patient==x)[1], "Antibiotics"])})
adonis( formula = species.df.perma.patient[,-1]~ factor(antibiotics) )
#R2 = 0.039
#p = 0.29




    
