
####voir les données manquantes 
sapply(Classeur,function(x) sum(is.na(x)))
#####voir les données manquantes ##############
library(Amelia)
missmap(Classeur, main = "Missing values vs observed")
Classeur$BLAU_INDEX <- NULL
##################corriger les variables ##########
Classeur$INNOV_VERTE <-as.factor(Classeur$INNOV_VERTE) 
is.factor(Classeur$INNOV_VERTE)
Classeur$INNOV_VERTE2 <-as.factor(Classeur$INNOV_VERTE2) 
is.factor(Classeur$INNOV_VERTE2)
contrasts(Classeur$INNOV_VERTE)
contrasts(Classeur$INNOV_VERTE2)
Classeur <- Classeur[!is.na(Classeur$INNOV_VERTE2),]
Classeur$TAILLE<- as.numeric(Classeur$TAILLE)
#########correlation
library(corrplot)
data2<- Classeur[,-3]
cor(data2)

###################modèle#########
summary(Classeur)
model <- glm(INNOV_VERTE2~., family="binomial",data=data2)
summary(model)
anova(model, test="Chisq")
