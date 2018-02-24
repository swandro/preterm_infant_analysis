#PCoA of bacteria and metabolites

#Load in data
load("data.RData")

library(ape)
library(ggplot2)
library(vegan)


##Set up the metadata
exclude <- c(7,17,46,44,40,39,22,61,71)
longitudinal.infant.file <- subset(infant.file, longitudinal=='y' & !Sample%in%exclude)
longitudinal.infant.file <- longitudinal.infant.file[order(longitudinal.infant.file$Sample),]
longitudinal.samples <- longitudinal.infant.file$Sample

#################
#Bacteria PCoA###
#################

#Calculate bray distances
bray.dist <- vegdist(otu.file[,-1], method = "bray")
#Calculate PCoA
pcoa.bacteria.bray <- pcoa(bray.dist)
variance.axis1 <- pcoa.bacteria.bray$values$Rel_corr_eig[1]
#Extract vectors to plot and order them
scores <- as.data.frame(pcoa.bacteria.bray$vectors)
scores <- scores[order(as.numeric(rownames(scores))),]
#take only longitudinal samples
longitudinal.scores <- scores[which(rownames(scores)%in%longitudinal.samples),]
#Check the order of the samples is the same in the data and metadata
all(rownames(longitudinal.scores) == longitudinal.infant.file$Sample)

#Plot PCoA
ggplot(data= longitudinal.scores, 
       aes(x= as.numeric(longitudinal.infant.file$day), 
           y= Axis.1)) +
  geom_line(aes(color=factor(longitudinal.infant.file$Patient)), size=1.5) +
  geom_point(aes(fill= factor(longitudinal.infant.file$Patient)),size=5, shape=21, color="black") +
  scale_fill_brewer(type = "qual", palette = 3 ) +
  scale_color_brewer(type = "qual", palette = 3 ) +
  guides(color=FALSE) +
  labs(x= "Day",
       y= paste("PC1 (",round(variance.axis1*100,1),"%)",sep = ''),
       fill="Infant") +
  theme_classic() +
  theme(axis.title=element_text(size=20), 
        axis.text=element_text(size=15),
        legend.text=element_text(size=15),
        legend.title=element_text(size=20),
        panel.border=element_rect(color="black",fill=NA)) 

###################
#Metabolite PCoA###
###################

#Set up the metadata. Remove sample 35 and 50 because no metabolomics data from those samples
longitudinal.infant.file.met <- subset(infant.file, longitudinal=='y' & !Sample%in%c(exclude, 35,50))
longitudinal.infant.file.met <- longitudinal.infant.file.met[order(longitudinal.infant.file.met$Sample),]
longitudinal.samples.met <- longitudinal.infant.file.met$Sample

metabolite.dist <- vegdist(log(met.file[,-1]), method = "canberra")
#Calculate PCoA
pcoa.met <- pcoa(metabolite.dist)
variance.axis1.met <- pcoa.met$values$Cum_corr_eig[1]
#Extract vectors to plot and order them
scores.met <- as.data.frame(pcoa.met$vectors)
scores.met <- scores.met[order(as.numeric(rownames(scores.met))),]
#Subset only longitudinal infant samples
longitudinal.met.scores <- scores.met[which(rownames(scores.met)%in%longitudinal.samples.met),]
#Check the order of the samples is the same in the data and metadata
all(rownames(longitudinal.met.scores) == longitudinal.infant.file.met$Sample)

#Plot PCoA
ggplot(data= longitudinal.met.scores, 
       aes(x= as.numeric(longitudinal.infant.file.met$day), 
           y= Axis.1)) +
  geom_line(aes(color=factor(longitudinal.infant.file.met$Patient)), size=1.5) +
  geom_point(aes(fill= factor(longitudinal.infant.file.met$Patient)),size=5, shape=21, color="black") +
  scale_fill_brewer(type = "qual", palette = 3 ) +
  scale_color_brewer(type = "qual", palette = 3 ) +
  guides(color=FALSE) +
  labs(x= "Day",
       y= paste("PC1 (",round(variance.axis1.met*100,1),"%)",sep = ''),
       fill="Infant") +
  theme_classic() +
  theme(axis.title=element_text(size=20), 
        axis.text=element_text(size=15),
        legend.text=element_text(size=15),
        legend.title=element_text(size=20),
        panel.border=element_rect(color="black",fill=NA)) 

