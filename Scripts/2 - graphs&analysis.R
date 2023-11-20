###data engineering to get psuedo individuals
Observations_4<-read.csv("Data/Caterpillar_1.2.csv",header = T)



SurvivalData<-data.frame()

i<-1
for (i in 1:nrow(Observations_4)) { print(i)
  if(Observations_4[i,"NumberDead"]==0){
    
    rowstoadd<- Observations_4[rep(i, 6), ]
    rowstoadd$Alive<-1
    SurvivalData<-rbind(SurvivalData,rowstoadd)
  }else{
    Number_Dead<-Observations_4[i,"NumberDead"]
    
    
    #dead rows
    rowstoadd<- Observations_4[rep(i, Number_Dead), ]
    rowstoadd$Alive<-0
    SurvivalData<-rbind(SurvivalData,rowstoadd)
    
    
  }}


SurvivalData$PsuedoIndividuals<-rownames(SurvivalData)

#ready for analysis 
SurvivalData$CageID<-as.integer(SurvivalData)

#create a model 
CaterpillarSurvival<-survfit(formula = Surv(SurvivalData$DPI) ~ SurvivalData$BBthenBC + (1|SurvivalData$CageID) )

#testModel
CaterpillarSurvival_Test<-survdiff(formula = Surv(SurvivalData$DPI) ~ SurvivalData$BBthenBC+ SurvivalData$BCthenBB+(1|SurvivalData$CageID))


summary(CaterpillarSurvival)

plot(CaterpillarSurvival)


COXmodel<-summary(coxme(Surv(SurvivalData$DPI) ~ SurvivalData$BBthenBC + (1|SurvivalData$CageID) ))


autoplot(CaterpillarSurvival)



aa_fit<-aareg(Surv(SurvivalData$DPI) ~ SurvivalData$Beauveria_ID + SurvivalData$Botrytis_ID+SurvivalData$BBthenBC)

summary(aa_fit)
