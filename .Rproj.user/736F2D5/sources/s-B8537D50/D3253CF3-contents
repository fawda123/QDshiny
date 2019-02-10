head(mydf1)


xdf<-mydf1 %>%
  select(Status=Determination_Exp,
         AquaticInverts,
         Ephemeroptera,
         PerennialIndicators,
         Plants,
         PctSlope,
         WetDry) %>%
  mutate(Status=factor(Status, levels=c("E","I","P")),
         AquaticInverts = ifelse(AquaticInverts=="Yes",1,0),
         Ephemeroptera= ifelse(Ephemeroptera=="Yes",1,0),
         PerennialIndicators= ifelse(PerennialIndicators=="Yes",1,0),
         Plants= ifelse(Plants=="Yes",1,0),
         WetDry= ifelse(WetDry=="Yes",1,0))

library(randomForest)

FlowStatus.rf<-
  randomForest(y=xdf$Status,
               x=xdf[,c("AquaticInverts","PerennialIndicators","Plants","PctSlope","WetDry")],
  importance = T)


newdf<-data.frame(AquaticInverts=1,
                  PerennialIndicators=0,
                  Plants=1,
                  PctSlope=1,
                  WetDry=1)

pred.df<-as.data.frame(predict(FlowStatus.rf, newdata=newdf, type="prob")) %>%
  melt(variable.name="Class", value.name = "Probability") %>%
  mutate(Class=factor(Class, levels=c("E","I","P"),
                      labels=c("Ephemeral","Intermittent", "Perennial")))

ggplot(data=pred.df, aes(x=Class, y=Probability, fill=Class))+
  geom_bar(stat="identity")+
  geom_bar(data=pred.df[which.max(pred.df$Probability),], stat="identity", color="black", size=2)+
  scale_x_discrete(name="")+
  ggtitle(paste("Your site is likely:",pred.df$Class[which.max(pred.df$Probability)] ))+
  scale_fill_manual(guide=F, values=c("#fdae61","#abd9e9","#2c7bb6"))


save(FlowStatus.rf,file="FlowStatus.rf.Rdata")




