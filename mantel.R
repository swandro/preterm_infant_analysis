#Mantel test
#10/17/17
#Stephen Wandro

library(vegan)

View(otu.file)

otu.file.ss <- otu.file[which(otu.file$Sample%in%met.file$Sample),]

#Get data frames in same order
otu.file.ss <- otu.file.ss[order(otu.file.ss$Sample),]
met.file <- met.file[order(met.file$Sample),]

seed <- 101

sig <- c()
stat <- c()

for (i in seq(30)){
random.ss <- randomize(infant.file)$Sample
otu.file.ss.1 <- otu.file.ss[otu.file.ss$Sample%in%random.ss,]
met.file.1 <- met.file[which(met.file$Sample%in%random.ss),]


otu.dist <- vegdist(otu.file.ss.1[,-1], method = "bray")
met.dist <- vegdist(met.file.1[,-1], method = "bray")

mant <-  mantel(xdis = otu.dist, ydis = met.dist,method = "spearman")

sig[i] <- mant$signif
stat[i] <- mant$statistic
}

mean(stat)
mean(sig)
sd(stat)
sd(sig)
hist(stat)
hist(sig)

######Testing correlations at the class level############
met.file <- met.file[order(met.file$Sample),]
class.ss <- subset(class.df, !Sample%in%c(35,50))


met.file <- met.file[order(met.file$Sample),]
class.ss <- class.ss[order(class.ss$Sample),]

sig.class <- c()
stat.class <- c()

for (i in seq(20)){
  random.ss <- randomize(infant.file)$Sample
  class.ss.1 <- class.ss[class.ss$Sample%in%random.ss,]
  met.file.1 <- met.file[which(met.file$Sample%in%random.ss),]
  
  
  otu.dist <- vegdist(class.ss.1[,-1], method = "bray")
  met.dist <- vegdist(met.file.1[,-1], method = "bray")
  
  mant <- mantel(xdis = otu.dist, ydis = met.dist,method = "pearson")
  
  sig.class[i] <- mant$signif
  stat.class[i] <- mant$statistic
}

mean(sig.class)
mean(stat.class)
hist(sig.class)
hist(stat.class)


##Visualizing correlation###
otu.file.ss <- otu.file.ss[order(otu.file.ss$Sample),]
met.file <- met.file[order(met.file$Sample),]

random.ss <- randomize(infant.file)$Sample
otu.file.ss.1 <- otu.file.ss[otu.file.ss$Sample%in%random.ss,]
met.file.1 <- met.file[which(met.file$Sample%in%random.ss),]

otu.file.ss.1.dat <- otu.file.ss.1[,-1]
met.file.1.dat <- met.file.1[,-1]
otu.dist <- as.matrix(vegdist(otu.file.ss.1[,-1], method = "bray"))
met.dist <- as.matrix(vegdist(met.file.1[,-1], method = "bray"))

a <- c()
b <- c()
for (i in seq(length(otu.dist))){
    a[i] <- otu.dist[i]
    b[i] <- met.dist[i]
}

plot(a,b)
