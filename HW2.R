setwd("~/Documents/Advanced MA/HW2")
Price.raw <- read.csv("HW2 - Prices.csv")
Unit.raw <- read.csv("HW2 - Units.csv")
names(Price.raw)[1] <- "consumerID"
names(Unit.raw)[1] <- "consumerID"


###############    PartA : Data Cleaning and Discrete Choice  #############
###Question 1
library(reshape2)

PriceDB <- melt(Price.raw, id.vars=c("consumerID"), variable.name = "weekNum", value.name = "pricePerUnit")
UnitDB <- melt(Unit.raw, id.vars=c("consumerID"), variable.name = "weekNum", value.name = "units")

nrow(PriceDB)  ###35900 rows
q1DB = matrix(NA, 35900, 5)
q1DB[, 1] <- PriceDB$consumerID
q1DB[, 2] <- PriceDB$weekNum
q1DB[, 3] <- PriceDB$pricePerUnit
q1DB[, 4] <- UnitDB$units
q1DB[, 5] <- ifelse(UnitDB$units==0, 0, 1)

q1DB <- data.frame(q1DB)
colnames(q1DB) = c("consumerID","weekNum","pricePerUnit","units","isPurchase")



###Question3
LinearModel <- lm(isPurchase~pricePerUnit,data = q1DB)
summary(LinearModel)
LogitModel <- glm(isPurchase~pricePerUnit,family=binomial(link=logit),data=q1DB)
summary(LogitModel)
ProbitModel <-glm(isPurchase~pricePerUnit,family=binomial(link=probit),data=q1DB)
summary(ProbitModel)

##Predict Probabiility between -5,5.1
dt <- data.frame(matrix(seq(-5,5.1,0.1),nrow=102,ncol=1)) 
names(dt)[1]='pricePerUnit'

dt$predict.linear <- predict(LinearModel,dt,type ="response" )
dt$predict.logit <-predict.glm(LogitModel,dt,type = "response")
dt$predict.probit <-predict.glm(ProbitModel,dt,type = "response")

plot(dt$pricePerUnit, dt$predict.linear, pch = 5, cex=1,col = "purple",main = "3 models", 
     xlab = "price per unit", ylab = "probability of a Skim Milk purchase")
points(dt$pricePerUnit, dt$predict.logit, pch = 20, cex=1,col = "red", 
       xlab = "price per unit", ylab = "probability of a Skim Milk purchase")
points(dt$pricePerUnit,dt$predict.probit,pch = 10, cex=1,col = "green",
       xlab = "price per unit", ylab = "probability of a Skim Milk purchase")
legend(pch = )
###Question 5 
#refer to PDF

##Question 6
#Clean data
QA6.Price <- read.csv("HW2 - QA6 - Prices .csv")
QA6.Units <- read.csv("HW2 - QA6 - Units.csv")
names(QA6.Price)[1] <- "consumerID"
names(QA6.Units)[1] <- "consumerID"

PriceDB.2 <- melt(QA6.Price, id.vars=c("consumerID"), variable.name = "weekNum", value.name = "pricePerUnit")
UnitDB.2 <- melt(QA6.Units, id.vars=c("consumerID"), variable.name = "weekNum", value.name = "units")

nrow(PriceDB.2)  ###71800 rows
QA6.dt = matrix(NA, 71800, 5)
QA6.dt[, 1] <- PriceDB.2$consumerID
QA6.dt[, 2] <- PriceDB.2$weekNum
QA6.dt[, 3] <- PriceDB.2$pricePerUnit
QA6.dt[, 4] <- UnitDB.2$units
QA6.dt[, 5] <- ifelse(UnitDB.2$units==0, 0, 1)

QA6.dt <- data.frame(QA6.dt)
colnames(QA6.dt) = c("consumerID","weekNum","pricePerUnit","units","isPurchase")

##build  3 models
LinearModel.2 <- lm(isPurchase~pricePerUnit,data = QA6.dt)
summary(LinearModel.2)
LogitModel.2 <- glm(isPurchase~pricePerUnit,family=binomial(link=logit),data=QA6.dt)
summary(LogitModel.2)
ProbitModel.2 <-glm(isPurchase~pricePerUnit,family=binomial(link=probit),data=QA6.dt)
summary(ProbitModel.2)

###Question 7
dt2 <- data.frame(matrix(5,nrow=1,ncol=1)) 
names(dt2)[1]='pricePerUnit'

#predict demand model
dt2$predict2.linear <- predict(LinearModel.2,dt2,type ="response" )
dt2$predict2.logit <-predict.glm(LogitModel.2,dt2,type = "response")
dt2$predict2.probit <-predict.glm(ProbitModel.2,dt2,type = "response")

##total number of consumers when price is 5
demand.linear1 <- 100*0.01943378
demand.logit1 <-100*0.0735
demand.probit1 <-100*0.0676

demand.linear2 <- 200*dt2$predict2.linear ##1.943378
demand.linear2  
demand.logit2 <-200*dt2$predict2.logit ##7.706914
demand.logit2
demand.probit2 <-200*dt2$predict2.probit ##7.162821
demand.probit2  
###################Part B: Heterogeneity####################
##question1
#build 4 models
require('plm')
data("q1DB", package="plm")
withinModel = plm(units~pricePerUnit,data=q1DB,model="within")
summary(withinModel)

randomModel = plm(units~pricePerUnit,data=q1DB,model="random")
summary(randomModel)

differModel = plm(units~pricePerUnit,data=q1DB,model="fd")
summary(differModel)

poolingModel = plm(units~pricePerUnit,data=q1DB,model="pooling")
summary(poolingModel)
  
##Question2
#using lm recreate models
altWithinModel =  lm(units~pricePerUnit+factor(consumerID),data=q1DB)

altPoolingModel = lm(units~pricePerUnit,data=q1DB)    

##Question3
#calculate AIC for two models 
AIC(altWithinModel) ###65390.89
AIC(altPoolingModel) ##67727.86

##Question4
#subset data only contains consumers who purchased
PurchasedDB <- subset(q1DB,q1DB$units>0) 
#build model
withinModel2 = plm(units~pricePerUnit,data=PurchasedDB,model="within")
summary(withinModel2)

##Question 5
#aggregate each consumerID's total purchase
aggdata <- aggregate(units~consumerID,data=q1DB,sum)
#get each consumer's coefficient
fixedEffect <- as.data.frame(summary(altWithinModel)$coefficients[3:101,1])
names(fixedEffect) <- c('consumer effect')

#get consumer 1's coefficient,and combine all consumer coefficient to one data set
consumer1 <- as.data.frame(0,row.names="factor(consumerID)1")
names(consumer1) <- c('consumer effect')
fixedEffect <- rbind(fixedEffect,consumer1)
##order the fixedEffect
fixedEffect <- fixedEffect[order(fixedEffect$`consumer effect`),]

#############Part C:Consumer Types##############
##Question1
#aggrgate units and order 
aggdata <- aggregate(q1DB$units, list(q1DB$consumerID),sum)
colnames(aggdata) <- c('consumerID','units')
consumer <- aggdata[order(aggdata$units,decreasing = T),]

#Segment consumers into two groups
top50 <- consumer[1:50,]
bottom50 <- consumer[51:100,]
group1 <- q1DB[q1DB$consumerID %in% top50$consumerID,]
group2 <- q1DB[q1DB$consumerID %in% bottom50$consumerID,]

Group1Model <- glm(isPurchase~pricePerUnit,family=binomial(link=logit),data=group1)
summary(Group1Model) ##-0.2346
Group2Model <- glm(isPurchase~pricePerUnit,family=binomial(link=logit),data=group2)
summary(Group2Model) ##-1.03036

##convert to fraction
exp(-0.2346) #0.79
exp(-1.03036) #0.35

####Question 3
##Calculate AIC
AIC(Group1Model)  #21030.12 
AIC(Group2Model)  #15119.84
sumAIC <- 21030.12+15119.84 #36149.96
AIC(LogitModel) #37012.04

##### 5 segmentations
t1 <- consumer[1:20,]
t2<- consumer[21:40,]
t3<- consumer[41:60,]
t4<- consumer[61:80,]
t5<- consumer[81:100,]

g1 <- q1DB[q1DB$consumerID %in% t1$consumerID,]
g2 <- q1DB[q1DB$consumerID %in% t2$consumerID,]
g3 <- q1DB[q1DB$consumerID %in% t3$consumerID,]
g4 <- q1DB[q1DB$consumerID %in% t4$consumerID,]
g5 <- q1DB[q1DB$consumerID %in% t5$consumerID,]


GModel1 <- glm(isPurchase~pricePerUnit,family=binomial(link=logit),data=g1)
GModel2 <- glm(isPurchase~pricePerUnit,family=binomial(link=logit),data=g2)
GModel3 <- glm(isPurchase~pricePerUnit,family=binomial(link=logit),data=g3)
GModel4 <- glm(isPurchase~pricePerUnit,family=binomial(link=logit),data=g4)
GModel5 <- glm(isPurchase~pricePerUnit,family=binomial(link=logit),data=g5)

AIC(GModel1) ##9345.017
AIC(GModel2) ##7659.413
AIC(GModel3) ##7110.944
AIC(GModel4) ##6087.013
AIC(GModel5) ##5452.801

sumAIC.5 <-9435.017+7659.413+6087.013+5452.801+7110.944

###10 Segmentations
t.1 <- consumer[1:10,]
t.2 <- consumer[11:20,]
t.3 <- consumer[21:30,]
t.4 <- consumer[31:40,]
t.5 <- consumer[41:50,]
t.6 <- consumer[51:60,]
t.7 <- consumer[61:70,]
t.8 <- consumer[71:80,]
t.9 <- consumer[81:90,]
t.10 <- consumer[91:100,]



g.1 <- q1DB[q1DB$consumerID %in% t.1$consumerID,]
g.2 <- q1DB[q1DB$consumerID %in% t.2$consumerID,]
g.3 <- q1DB[q1DB$consumerID %in% t.3$consumerID,]
g.4 <- q1DB[q1DB$consumerID %in% t.4$consumerID,]
g.5 <- q1DB[q1DB$consumerID %in% t.5$consumerID,]
g.6 <- q1DB[q1DB$consumerID %in% t.6$consumerID,]
g.7 <- q1DB[q1DB$consumerID %in% t.7$consumerID,]
g.8 <- q1DB[q1DB$consumerID %in% t.8$consumerID,]
g.9 <- q1DB[q1DB$consumerID %in% t.9$consumerID,]
g.10 <- q1DB[q1DB$consumerID %in% t.10$consumerID,]



GM1 <- glm(isPurchase~pricePerUnit,family=binomial(link=logit),data=g.1)
GM2 <- glm(isPurchase~pricePerUnit,family=binomial(link=logit),data=g.2)
GM3 <- glm(isPurchase~pricePerUnit,family=binomial(link=logit),data=g.3)
GM4 <- glm(isPurchase~pricePerUnit,family=binomial(link=logit),data=g.4)
GM5 <- glm(isPurchase~pricePerUnit,family=binomial(link=logit),data=g.5)
GM6 <- glm(isPurchase~pricePerUnit,family=binomial(link=logit),data=g.6)
GM7 <- glm(isPurchase~pricePerUnit,family=binomial(link=logit),data=g.7)
GM8 <- glm(isPurchase~pricePerUnit,family=binomial(link=logit),data=g.8)
GM9 <- glm(isPurchase~pricePerUnit,family=binomial(link=logit),data=g.9)
GM10 <- glm(isPurchase~pricePerUnit,family=binomial(link=logit),data=g.10)




AIC(GM1) #4870.927
AIC(GM2) #4357.022
AIC(GM3) #3943.065
AIC(GM4) #3713.413
AIC(GM5) #3610.393
AIC(GM6) #3498.963
AIC(GM7) #3076.328
AIC(GM8) #3031.696
AIC(GM9) #2884.102
AIC(GM10) #2561.907


sumAIC.10 <- 4870.927+4357.022+3943.065+3713.413+3610.393+3498.963+3076.328+3031.696+2884.102+2561.907















