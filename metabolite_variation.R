#Variation in each category
library(dplyr)
library(ggplot2)

load("data.RData")

sums <- apply(met.file[,-1], 2, sum)
means <- apply(met.file[,-1], 2, mean)
sds <- apply(met.file[,-1], 2, sd)
cov <- sds/means
metabolites <- colnames(met.file)[-1]
variation.df <- data.frame(metabolites, cov, sums, means, sds)
variation.df<- merge(variation.df, met.info, by.x = "metabolites",by.y="BinBase.name")

variation.categories <- group_by(variation.df, category2) %>%
  summarize(avg.cov = mean(cov),
            avg.sum = mean(sums),
            avg.sds = mean(sds),
            avg.means = mean(means))


variation.df$category2 <- factor(variation.df$category2, ordered= T, levels=variation.categories$category2[order(variation.categories$avg.cov)])

#Make the plot
ggplot(data = subset(variation.df, !category2%in%c("xother", "zunknown", "amino sugar"))) +
  geom_boxplot(aes(x=category2, y= cov), outlier.color = NA) +
  geom_jitter(aes(x=category2, y= cov), width=.2) +
  theme_bw() +
  theme(text=element_text(size=15),
        axis.text.x = element_text(angle=90))
