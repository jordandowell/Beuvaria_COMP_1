####

install.packages("survival")
library(survival)

library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)





#read in data

Observations<-read.csv("Data/caterpillarobservation.csv", header = T)


str(Observations)
###



  
#add rownames as a column


SurvivalData$PsuedoIndividuals<-row.names(SurvivalData)

#Remove alive rows at less than 6 DPI
Observations_2<-Observations[Observations$DPI == 6 & Observations$per_dead == 0,]

#remove alive rowsn

Observations_3<-Observations[Observations$per_dead > 0 ,]
View(Observations_3)

#keep time points when things first dies
Timepointstokeep<-data.frame()

for (j in unique(Observations_3$CageID)) {

  Cage<-Observations_3[Observations_3$CageID==j,]
  Cage_2<-   Cage[!duplicated(Cage$per_dead),]
  
  
  Timepointstokeep<-rbind(Timepointstokeep,Cage_2)
    
}

Timepointstokeep<-rbind(Timepointstokeep,Observations_2)
View(Timepointstokeep)
#write csv and fix number dead
write.csv(Timepointstokeep,"Time.csv") 
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


CaterpillarSurvival<-survfit(formula = Surv(SurvivalData$DPI) ~ SurvivalData$BBthenBC+SurvivalData$BCthenBB+ (1|SurvivalData$CageID))


CaterpillarSurvival_Test<-survdiff(formula = Surv(SurvivalData$DPI) ~ SurvivalData$BBthenBC+ SurvivalData$BCthenBB+(1|SurvivalData$CageID))


summary(CaterpillarSurvival)

plot(CaterpillarSurvival)


COXmodel<-summary(coxme(Surv(SurvivalData$DPI) ~ SurvivalData$BBthenBC + SurvivalData$BCthenBB+(1|SurvivalData$CageID) ))


autoplot(CaterpillarSurvival)



aa_fit<-aareg(Surv(SurvivalData$DPI) ~ SurvivalData$Beauveria_ID + SurvivalData$Botrytis_ID+SurvivalData$BBthenBC)

summary(aa_fit)


