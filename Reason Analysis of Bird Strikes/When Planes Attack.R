######## Decision Model Final Project ########
## Team Members: Gengzhe Li, Jiaying Ni, Pujiao Zhu, Shuqiao Li ##

# 0. Data Cleaning 
Wildlife <- read.csv("wildlife_log.csv", na.strings = "?")
View(Wildlife)
Wildlife <- na.omit(Wildlife)

dim(Wildlife)

sum(is.na(Wildlife))

# 1. Logistic Regression to find out relevent varaibles
logreg.fit <- glm(Damage~Type_ENG+AC_Mass+Num_ENGS+Month+Year+Time+Height+Speed+Phase+Sky+Precip+Birds_Seen+Birds_Struck+Size,data=Wildlife,family=binomial)
summary(logreg.fit)

logreg.fit <- glm(Damage~Type_ENG+AC_Mass+Speed+Month+Year+Birds_Seen+Size,data=Wildlife,family=binomial)
summary(logreg.fit)

logreg.fit <- glm(Damage~AC_Mass+Speed+Month+Year+Birds_Seen+Size,data=Wildlife,family=binomial)
summary(logreg.fit)

# 2. Predict and evaluate model accuracy
set.seed(1)

train = sample(1:nrow(Wildlife),nrow(Wildlife)/2)
Wildlife.train=Wildlife[train,]
View(Wildlife.train)
Damage.train=Wildlife$Damage[train]

Wildlife.test=Wildlife[-train,]
View(Wildlife.test)
Damage.test=Wildlife$Damage[-train]

prop.table(table(Wildlife.train$Damage))
prop.table(table(Wildlife.test$Damage))

glm.fit=glm(Damage~AC_Mass+Speed+Month+Year+Birds_Seen+Size, family=binomial,data=Wildlife.train)
summary(glm.fit)

glm.probs=predict(glm.fit,newdata=Wildlife.test,type="response")
glm.probs

glm.pred=rep("0",479)
glm.pred[glm.probs>.5]="1"
table(glm.pred,Wildlife.test$Damage)
mean(glm.pred==Wildlife.test$Damage)

# 3. Forward method to find best sebset of the liner relationship between damage cost and other vriables
Wildlife.cost <- read.csv("wildlife_cost.csv", na.strings = "?")
Wildlife.cost <- na.omit(Wildlife.cost)
View(Wildlife.cost)
library(leaps)

regfit.fwd=regsubsets(Cost~.,data=Wildlife.cost,nvmax=15, method="forward")
summary(regfit.fwd)

reg.fwd=summary(regfit.fwd)
names(reg.fwd)
reg.fwd$cp

plot(reg.fwd$cp, xlab="Number of predictors", ylab="Cp")
which.min(reg.fwd$cp)
coef(regfit.fwd,8)

# 4. KNN to predict damage
Wildlife <- read.csv("wildlife_KNN.csv", na.strings = "?")
Wildlife <- na.omit(Wildlife)
View(Wildlife)
set.seed(1)

train = sample(1:nrow(Wildlife),nrow(Wildlife)/2)
Wildlife.train.preditors =Wildlife[train,-1]
Damage.train=Wildlife$Damage[train]

Wildlife.test.preditors =Wildlife[-train,-1]
Damage.test=Wildlife$Damage[-train]

library(class)

knn.pred = knn(Wildlife.train.preditors,Wildlife.test.preditors,Damage.train,k=1)
prop.table(table(knn.pred))
table(knn.pred,Damage.test)
mean(knn.pred==Damage.test)

knn.pred = knn(Wildlife.train.preditors,Wildlife.test.preditors,Damage.train,k=3)
prop.table(table(knn.pred))
table(knn.pred,Damage.test)
mean(knn.pred==Damage.test)

knn.pred = knn(Wildlife.train.preditors,Wildlife.test.preditors,Damage.train,k=5)
prop.table(table(knn.pred))
table(knn.pred,Damage.test)
mean(knn.pred==Damage.test)


knn.pred = knn(Wildlife.train.preditors,Wildlife.test.preditors,Damage.train,k=7)
prop.table(table(knn.pred))
table(knn.pred,Damage.test)
mean(knn.pred==Damage.test)


knn.pred = knn(Wildlife.train.preditors,Wildlife.test.preditors,Damage.train,k=9)
prop.table(table(knn.pred))
table(knn.pred,Damage.test)
mean(knn.pred==Damage.test)


knn.pred = knn(Wildlife.train.preditors,Wildlife.test.preditors,Damage.train,k=11)
prop.table(table(knn.pred))
table(knn.pred,Damage.test)
mean(knn.pred==Damage.test)

# 5. Kmeans and Hierachical Clusters
########  Kmeans ########
Wildlife <- read.csv( "wildlife_cluster.csv" )
Wildlife <- na.omit(Wildlife)
View(Wildlife)
set.seed(1)

Wildlife_cluster <- kmeans(Wildlife[,c(2,3)],5,nstart=20)
plot(Wildlife$Average.of.AC_Mass, Wildlife$Count.of.Damage, col=Wildlife_cluster$cluster, pch=Wildlife_cluster$cluster, 
     xlab="Average.of.AC_Mass", ylab="Count.of.Damage")
text(x=Wildlife$Average.of.AC_Mass,y=Wildlife$Count.of.Damage-0.2,
     labels=Wildlife$A_Type2,col=Wildlife_cluster$cluster)

###### Hierarchical ######
hc.complete=hclust(dist(Wildlife[,-1]),method="complete")
plot(hc.complete)
hc.out <- cutree(hc.complete,5)
plot(Wildlife$Average.of.AC_Mass, Wildlife$Count.of.Damage, col=hc.out,pch=hc.out, xlab="Average.of.AC_Mass", ylab="Count.of.Damage")
