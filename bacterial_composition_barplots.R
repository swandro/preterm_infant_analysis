#Make bacterial composition barplots
#Stephen Wandro 9/21/2017

#Load in data
load("data.RData")

#Libraries
library(ggplot2)

#####Helper functions and definitions######

#For the figures, exclude samples taken on the same day as another sample in the same infant 
exclude <- c(7,17,46,44,40,39,22,61,71)
genus.colors <- c("#a6cee3", "#1f78b4", "#b2df8a","#33a02c","#fb9a99",
                  "#e31a1c", "#fdbf6f", "#ff7f00", "#6a3d9a","grey",
                  "#ffff99", "#b15928")

species.melt$genus.other2 <- make.other.genus.bifido(species.melt,number= 9)

make.barplot <- function(df, title=''){
  return(ggplot(df ,aes(x=factor(Age_at_sample_collection),y=value,fill=genus.other2)) + 
           geom_bar(stat="identity") + 
           scale_y_continuous(expand = c(0,0)) +
           labs(title=" ",x="Day of life",y="Relative Abundance", fill= "Bacteria") +
           ggtitle(title) +
           theme(plot.title=element_text(size=30,hjust= 0.5),
                 axis.title=element_text(size=20),
                 axis.text=element_text(size=15),
                 axis.text.x = element_text(angle=-90,vjust=.5),
                 strip.text=element_text(size=9),
                 legend.text=element_text(size=15),
                 legend.title=element_text(size=20),
                 strip.background=element_rect(color="black", fill=NA,size=1),
                 panel.background=element_rect(fill=NA, color="black",size=1),
                 panel.grid=element_blank()) +
           scale_fill_manual(values=genus.colors) +
           facet_grid(~Patient,scales="free_x",space="free_x") 
  )
}



################################################################################
####Plot the conposition for each infant, grouped by health status##############

#plot NEC infants
data.subset <- subset(species.melt, !Sample%in%exclude & Group%in%"nec")
nec.bac.plot <- make.barplot(data.subset, "NEC infants")

#plot septic infants
data.subset <- subset(species.melt, !Sample%in%exclude & Group%in%"septic")
nec.bac.plot <- make.barplot(data.subset, "Septic infants")

#Plot control infants
data.subset <- subset(species.melt, !Sample%in%exclude & Group%in%"control")
nec.bac.plot <- make.barplot(data.subset, "Control infants")

########################################################
###Plot Twin compositions side by side##################

otu.melt$all.twin.other <- make.subset.other(otu.melt,c(12,13,15,16,24,25,26,27),"variable", 8)

#Plot twin 1
data.subset <- subset(species.melt, Twin_set == 1 & !Sample%in%exclude)
twin1.bac.plot <- make.barplot(data.subset, "Twin set 1")

#Plot twin 2
data.subset <- subset(species.melt, Twin_set == 2 & !Sample%in%exclude)
twin2.bac.plot <- make.barplot(data.subset, "Twin set 2")

#Plot twin 3
data.subset <- subset(species.melt, Twin_set == 3 & !Sample%in%exclude)
twin3.bac.plot <- make.barplot(data.subset, "Twin set 3")

#Plot twin 4
data.subset <- subset(species.melt, Twin_set == 4 & !Sample%in%exclude)
twin4.bac.plot <- make.barplot(data.subset, "Twin set 4\n(Late-onset sepsis)")

rm(data.subset)