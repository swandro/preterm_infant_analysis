#Metabolite heatmap
load("data.RData")

met.melt$z.score2 <- met.melt$z.score
met.melt$z.score2[which(met.melt$z.score2 > 3)] <- 3

#Plotting function
make.heatmap <- function(df, group){
  return(
    ggplot(data = subset(df, Group==group & !category2%in%c("xother", "zunknown", "amino sugar","blie acid"))) +
      geom_tile(aes(x = factor(Age_at_sample_collection), y= variable, fill = z.score2)) +
      facet_grid(category2.format~Patient, scales = "free", space = "free") +
      scale_fill_gradientn(colours = c("red","white","lightblue","#3182bd"))  +
      labs(x="Age", y="Metabolites", fill="Z-score") +
      theme(text = element_text(size=11),
            axis.text.y = element_blank(),
            axis.text.x = element_text(angle=90),
            strip.text.y = element_text(angle=0),
            strip.text.x = element_text(size=9),
            panel.border = element_rect(fill=NA, color="black"),
            panel.spacing = unit(.1, "lines"),
            strip.background = element_rect(fill=NA, color= "black"),
            axis.ticks.y = element_blank())
  )
}

#Cluster the metabolites
L <- list()
i <- 1
for (category in levels(factor(met.melt$category2))){
  temp <- dcast(subset(met.melt, category2%in%category), formula = Sample~variable, value.var = "z.score")
  clust <- hclust(dist(t(temp[,-1])))
  L[[i]] <- clust$labels[clust$order]
  i <- i + 1
}
ord <- unlist(L)
met.melt$variable <- factor(met.melt$variable, levels = ord, ordered = T)

#Plot control
make.heatmap(met.melt, "control")
make.heatmap(met.melt, "septic")
make.heatmap(met.melt, "nec")

