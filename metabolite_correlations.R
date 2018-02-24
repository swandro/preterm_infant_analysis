
load("data.RData")

#Correlations between bacteria and metabolites

make.other.genus <- function(dataframe, taxa.col="genus", number,value.col="value"){
  #If the taxa col has NA value, then takes the value from the previous col
  family.genus <- function(x){
    if (is.na(x["genus"])){
      return(paste("f_",x["family"], sep = ''))}else{
        return(x["genus"])
      }
  }
  dataframe$taxa.col2 <- factor(apply(dataframe, 1,family.genus))
  dataframe[,"taxa.col2"][which(is.na(dataframe[,"taxa.col2"]))] <- 
    genus.fac <- levels(dataframe[,"taxa.col2"])
  sums<- c()
  for(ph in genus.fac){sums <- c(sums,sum(dataframe[which(dataframe[,"taxa.col2"]==ph),][,value.col]))}
  genus.fac<- genus.fac[order(sums,decreasing=TRUE)]
  sums <- sums[order(sums,decreasing=T)]
  genus.other.f <- function(x)
  {if(x["taxa.col2"]%in%genus.fac[1:number]){return(x["taxa.col2"])}
    else{return("Other")}}
  result <- apply(dataframe,1,genus.other.f)
  result <- factor(result,levels=c(genus.fac,"Other"))
  result <- droplevels(result)
  return(result)
}
species.melt$genus.other <- make.other.genus(species.melt, number=8)

levels(factor(species.melt$genus.other))
bacteria <- c("f_Enterobacteriaceae","Enterococcus","Staphylococcus","Bacteroides")

#make the bacteria matrix
bacteria.df <- dcast(subset(species.melt, genus.other%in%bacteria & !Sample%in%c(35,50)),
                     formula = Sample~genus.other, 
                     value.var = "value", 
                     fun.aggregate = sum)
bacteria.df <- bacteria.df[order(as.numeric(bacteria.df$Sample)),]
#ensure same order
all(bacteria.df$Sample == met.file$Sample)

#Between bacteria and metabolites
metaboltie.correlations <- cor(bacteria.df[,-1], met.file[,-1])
metaboltie.correlations.melt <- melt(metaboltie.correlations)
View(metaboltie.correlations.melt)

######Plot individual bacteria metabolite correlations
#Get infant numbers for plotting
temp <- subset(infant.file, !Sample%in%c(35,50))
all(temp$Sample == met.file$Sample)
pat.num <- temp$Patient
#Plotting function
plot.cor <- function(m, b){
  return(
    ggplot() +
      geom_text(aes(x=bacteria.df[,b],y=met.file[,m]), label=pat.num) +
      labs(x= paste(b, "relative abundance"), y= paste(m, "relative intensity")) +
      scale_x_continuous(limits=c(0,1)) +
      theme_bw() +
      theme(text = element_text(size=15))
  )
}

mm <- "succinic acid"
bb <- "Bacteroides"
plot.cor(mm,bb)

mm <- "4-aminobutyric acid"
bb <- "Bacteroides"
plot.cor(mm,bb)

mm <- "2-hydroxyhexanoic acid"
bb <- "Bacteroides"
plot.cor(mm,bb)

mm <- "phosphoethanolamine"
bb <- "Bacteroides"
plot.cor(mm,bb)


 ###########Betwen bacteria and classes of metabolites
#Get the average z.score for each class of metabolites
temp <- group_by(subset(met.melt, !category2%in%c("xother","zunknown")), Sample, category2) %>% 
  summarize(z.score=mean(z.score))
met.category.df <- dcast(data=temp, value.var = "z.score", formula = Sample~category2)
#ensure proper order
all(bacteria.df$Sample == met.category.df$Sample)

metabolite.category.correlations <- cor(bacteria.df[,-1], met.category.df[,-1])
metabolite.category.correlations.melt <- melt(metabolite.category.correlations)
View(metabolite.category.correlations.melt)



#Make heatmap showing correlations between bacteria and metabolite categories
#Cluster the axis
clust <- hclust(d = dist(metabolite.category.correlations))
ord <- clust$labels[clust$order]
metabolite.category.correlations.melt$Var1 <- factor(metabolite.category.correlations.melt$Var1, levels = ord, ordered = T)

clust2 <- hclust(d = dist(t(metabolite.category.correlations)))
ord2 <- clust2$labels[clust2$order]
metabolite.category.correlations.melt$Var2 <- factor(metabolite.category.correlations.melt$Var2, levels = ord2, ordered = T)

#make the plot
ggplot(data = metabolite.category.correlations.melt) +
  geom_tile(aes(x= Var1, y= Var2, fill = value)) +
  scale_fill_gradient2(low="red", mid="white", high="blue",midpoint = 0 , limits=c(-.6, .6), guide = "colourbar",na.value = "green") +
  labs(x= '', y= "Metabolite categories", fill = "Correlation") +
  theme(text = element_text(size=15),
        axis.text.x = element_text(angle = 45 , hjust=1, vjust=1),
        legend.title = element_text(size = 10),
        panel.border = element_rect(fill=NA, color="grey"),
        panel.spacing = unit(.1, "lines"),
        panel.background = element_rect(fill=NA, color="NA"))
