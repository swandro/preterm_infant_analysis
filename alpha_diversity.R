#Clinical factors and alpha diversity

load("data.RData")

infant.alpha <- merge(infant.file, alpha.mean, by = "Sample")
#add column for mean shannon div for each patient
infant.alpha.avg <- summarize(group_by(infant.alpha, Patient) , 
                              shannon.avg = mean(shannon), 
                              Group=Group[1], 
                              delivery = delivery_mode[1],
                              feed = feeding_type[1])

###Effect of clinical variables on alpha diversity########

#Infant age
alpha.age.lm <- lm(infant.alpha$shannon ~ infant.alpha$Age_at_sample_collection)
summary(alpha.age.lm)

ggplot(data=infant.alpha) +
  geom_point(aes(x=Age_at_sample_collection, y=shannon), size = 3) +
  geom_abline(slope= alpha.age.lm$coefficients[2], intercept = alpha.age.lm$coefficients[1], size=1.5, linetype="dashed" ) +
  labs(x= "Infant age (days)",
       y= " Shannon diversity") +
  theme_classic() +
  theme(axis.title=element_text(size=20), axis.text=element_text(size=20),
        legend.text=element_text(size=15),legend.title=element_text(size=20),
        panel.border=element_rect(color="black",fill=NA),
        panel.grid.major = element_line(color="grey"))

#health outcome
infant.alpha.avg$Group2 <- infant.alpha.avg$Group
infant.alpha.avg$Group2 <- factor(infant.alpha.avg$Group2, levels = c("nec","septic","control"), 
                                  labels = c("Necrotizing\nenterocolitis","Late-onset\nsepsis", "Control"))

ggplot(data=subset(infant.alpha.avg, Group!="GI")) +
  geom_boxplot(aes(y=shannon.avg, x=Group2), size=1.5) +
  geom_jitter(aes(y=shannon.avg, x=Group2), size=2, width = .2) +
  facet_grid(.~Group, space = "free",scales = "free") +
  labs(x= "Health outcome", y= "Shannon Diversity") +
  theme_classic() +
  theme(axis.title=element_text(size=20), axis.text=element_text(size=15),
        legend.text=element_text(size=15),legend.title=element_text(size=20),
        panel.border=element_rect(color="black",fill=NA),
        strip.background = element_blank(), strip.text=element_blank())


#Delivery mode
##alpha diversity in c-section vs vaginal birth
infant.alpha.avg$delivery <- factor(infant.alpha.avg$delivery, levels = c("c-section","vaginal"), labels = c("C-section","Vaginal"))

ggplot(data=subset(infant.alpha.avg, delivery!='')) +
  geom_boxplot(aes(y=shannon.avg, x=delivery), size=1.5) +
  geom_jitter(aes(y=shannon.avg, x=delivery), size=2, width = .2) +
  facet_grid(.~delivery, scales = "free") +
  labs(x= "Delivery Mode", y= "Shannon Diversity") +
  theme_classic() +
  theme(axis.title=element_text(size=20), axis.text=element_text(size=15),
        legend.text=element_text(size=15),legend.title=element_text(size=20),
        panel.border=element_rect(color="black",fill=NA),
        strip.background = element_blank(), strip.text=element_blank())

##Breastmilk vs breastmilk and formula
infant.alpha.avg$feed <- factor(infant.alpha.avg$feed, levels= c("BM","BM,F"), labels = c("Breastmilk","Breastmilk\nand Formula"))

ggplot(data=infant.alpha.avg) +
  geom_boxplot(aes(y=shannon.avg, x=feed), size=1.5) +
  geom_jitter(aes(y=shannon.avg, x=feed), size=2, width = .2) +
  facet_grid(.~feed, scales = "free") +
  labs(x= "", y= "Shannon Diversity") +
  theme_classic() +
  theme(axis.title=element_text(size=20), axis.text=element_text(size=15),
        legend.text=element_text(size=15),legend.title=element_text(size=20),
        panel.border=element_rect(color="black",fill=NA),
        strip.background = element_blank(), strip.text=element_blank())
############################################################


