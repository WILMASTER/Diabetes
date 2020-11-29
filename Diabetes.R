#Packages----

library('tidyverse')
library('psych')
library("corrplot")
library('caret')
library('plyr')
library('MASS')


#Data Loading----
setwd('C:/Users/wilhem.castro/Documents/R/dataset_diabetes')
getwd()
ds<- read.csv('diabetic_data.csv')
ds
dim(ds)
names(ds)
head(ds)

#Data Preparation---
round(sapply(ds, function(x) sum(x=="?"))/101766*100,2) # No se tandrán en cuenta las variables weight,payer,code,medical speciality
ds<-ds[,c(-6,-11,-12)]
str(ds)
dim(ds)
#Cambiar alguno numéricos a factor
ds$admission_source_id<- as.factor(ds$admission_source_id)
levels(ds$admission_source_id)
ds$admission_type_id<- as.factor(ds$admission_type_id)
levels(ds$admission_type_id)
ds$discharge_disposition_id<-as.factor(ds$discharge_disposition_id)
levels(ds$discharge_disposition_id)
#Numéricos
num_cols <- unlist(lapply(ds, is.numeric))         # hay 13 númericas incliyendo Ids
num<-ds[,num_cols]
dim(num)
num<-num[,c(-1,-2)]
names(num)
dim(num)

#Análisis univariado
levels(ds$readmitted)
ggplot(num, aes(x=time_in_hospital, fill=ds$readmitted)) + geom_density(alpha=.3)
ggplot(num, aes(x=time_in_hospital, fill=ds$readmitted)) + geom_boxplot(alpha=.3)
describeBy(time_in_hospital,ds$readmitted)
TukeyHSD(aov(time_in_hospital~ds$readmitted),data=ds)
  # Se puede observar que el tiempo en el hospital el significativo para estimar el tipo de readmision.

ggplot(num, aes(x=num_lab_procedures, fill=ds$readmitted)) + geom_density(alpha=.3)
ggplot(num, aes(x=num_lab_procedures, fill=ds$readmitted)) + geom_boxplot(alpha=.3)
describeBy(num_lab_procedures,ds$readmitted)
TukeyHSD(aov(num_lab_procedures~ds$readmitted),data=ds)# Good Candidate for Classification
  # Se puede observar que el numero de exámenes de lab es significativo para estimar la no readmision.

ggplot(num, aes(x=num_procedures, fill=ds$readmitted)) + geom_density(alpha=.3)
ggplot(num, aes(x=num_procedures, fill=ds$readmitted)) + geom_boxplot(alpha=.3)
describeBy(num_procedures,ds$readmitted)
TukeyHSD(aov(num_procedures~ds$readmitted),data=ds)# Good Candidate for Classification
  # Se puede observar que el numero de procedimientos es significativo para estimar la no readmision.

ggplot(num, aes(x=num_medications, fill=ds$readmitted)) + geom_density(alpha=.3)
ggplot(num, aes(x=num_medications, fill=ds$readmitted)) + geom_boxplot(alpha=.3)
describeBy(num_medications,ds$readmitted)
TukeyHSD(aov(num_medications~ds$readmitted),data=ds)# Good Candidate for Classification
  # Se puede observar que el numero de medicamentos es significativo para estimar el tipo de readmision.

ggplot(num, aes(x=number_outpatient, fill=ds$readmitted)) + geom_density(alpha=.3)
ggplot(num, aes(x=number_outpatient, fill=ds$readmitted)) + geom_boxplot(alpha=.3)
describeBy(number_outpatient,ds$readmitted)
TukeyHSD(aov(number_outpatient~ds$readmitted),data=ds)# Good Candidate for Classification
  # Se puede observar que el numero de tratamientos ambulatorios es significativo para estimar el tipo de readmision.

ggplot(num, aes(x=number_emergency, fill=ds$readmitted)) + geom_density(alpha=.3)
ggplot(num, aes(x=number_emergency, fill=ds$readmitted)) + geom_boxplot(alpha=.3)
describeBy(number_emergency,ds$readmitted)
TukeyHSD(aov(number_emergency~ds$readmitted),data=ds)# Good Candidate for Classification
  # Se puede observar que el numero de asistencia de urgencia es significativo para estimar el tipo de readmision.

ggplot(num, aes(x=number_inpatient, fill=ds$readmitted)) + geom_density(alpha=.3)
ggplot(num, aes(x=number_inpatient, fill=ds$readmitted)) + geom_boxplot(alpha=.3)
describeBy(number_inpatient,ds$readmitted)
TukeyHSD(aov(number_inpatient~ds$readmitted),data=ds)# Good Candidate for Classification
  # Se puede observar que el numero de hospitalizaciones es significativo para estimar el tipo de readmision.

ggplot(num, aes(x=number_diagnoses, fill=ds$readmitted)) + geom_density(alpha=.3)
ggplot(num, aes(x=number_diagnoses, fill=ds$readmitted)) + geom_boxplot(alpha=.3)
describeBy(number_diagnoses,ds$readmitted)
TukeyHSD(aov(number_diagnoses~ds$readmitted),data=ds)# Good Candidate for Classification
  # Se puede observar que el numero de diagnosticos es significativo para estimar la no readmision.
names(num)

#Asociaciones numericas
par(mfrow=c(1,1))
DatosCor<- cor(num, use="pairwise", method="spearman")
corrplot(DatosCor,method="number", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45,diag=FALSE)

# pairs.panels(num, 
#              method = "pearson", # correlation method
#              hist.col = "#00AFBB",
#              density = TRUE,  # show density plots
#              ellipses = TRUE # show correlation ellipses
# )
my_cols <- c("#00AFBB", "#E7B800", "#FC4E07")
levels(readmitted)
pairs(num, pch = 19,  cex = 0.5,
      col = my_cols[readmitted],
      lower.panel=NULL)

num_s<-scale(num,center=T,scale=T)
describe(num_s)
#Categóricos----
fact_cols <- unlist(lapply(ds, is.factor))         # Categpricos
fact<-ds[,fact_cols]
names(fact)
dim(fact)
prop.table(table(fact$readmitted))# Hay un pequeño desbalance para <30
str(fact)
names(fact)
for (i in seq(1,26)){
print(i)
print((names(fact[i])))
print(round(prop.table(table(fact$readmitted,fact[,i]),1)*100,1))
print(chisq.test(fact$readmitted,fact[,i]))#Ho: Independence
  }
for (i in seq(29,36)){
  print(i)
  print((names(fact[i])))
  print(round(prop.table(table(fact$readmitted,fact[,i]),1)*100,1))
  print(chisq.test(fact$readmitted,fact[,i]))#Ho: Independence
}

#Los siguientes factores parecen ser no brindar información significatuva para el modelo
#14,15,17,19,20,24,25,26,27,28,30,31,32,33,34
names(fact)
fact$readmitted<-ifelse(fact$readmitted=="<30",0,1)
fact$readmitted<-as.factor(fact$readmitted)
prop.table(table(fact$readmitted))# Hay un pequeño desbalance para <30
fact_r<-fact[,c(1:13,16,18,21,22,23,29,35,36,37)]
head(fact_r)
names(fact_r)
fact_r$readmitted<-as.factor(fact_r$readmitted)
class(fact_r$readmitted)
#One hot Encoding
#mapvalues(fact$readmitted, from = c("<30", ">30","NO"), to = c("0", "1","2"))
dmy <- dummyVars(" ~ .", data = fact_r[,-22])
fact_d <- data.frame(predict(dmy, newdata = fact_r[,-22]))#Factores numerización n:n
fact_dfact_d<-fact_dfact_d[,-fact_d$discharge_disposition_id.11] 
readmitted<-fact_r$readmitted
fact_dr<-cbind(fact_d,readmitted)
dim(fact_d)
dim(fact_dr)
#Vista minable----
vm<-cbind(num_s,fact_dr)#Factores con numericos standarizados
dim(vm)

#creo entrenamiento y validacion
unos<-subset(vm,vm$readmitted==1)#white
cero<-subset(vm,vm$readmitted==0)#red
sample <- sample.int(nrow(unos), round(.5*nrow(unos)))
unos.train <- unos[sample, ]
unos.test <- unos[-sample, ]
#porcentaje que debo obtener para balancear entrenamiento
value=nrow(unos.train)/nrow(unos)
value
#sacar ese numero en entrenamiento, el resto en validacion
sample <- sample.int(nrow(cero), round(value*nrow(cero)))
cero.train <- cero[sample, ]
cero.test <- cero[-sample, ]
dim(cero.train)
dim(cero.test)
#creo entrenamiento y validacion
data.train<-rbind(cero.train,unos.train)
data.test<-rbind(cero.test,unos.test)


library('xgboost')
library('data.table')
library('mlr')


train<-data.train
test<-data.test
setDT(train) 
setDT(test)

#using one hot encoding 
labels <- train$readmitted
ts_label <- test$readmitted
new_tr <- model.matrix(~.+0,data = train[,-c("readmitted"),with=F]) 
new_ts <- model.matrix(~.+0,data = test[,-c("readmitted"),with=F])

#convert factor to numeric 
labels <- as.numeric(labels)-1
ts_label <- as.numeric(ts_label)-1


#preparing matrix 
dtrain <- xgb.DMatrix(data = new_tr,label = labels) 
dtest <- xgb.DMatrix(data = new_ts,label=ts_label)

#default parameters
params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)


xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 14, nfold = 5, showsd = T, stratified = T, print_every_n = 10, early_stop_round = 20, maximize = F)

#first default - model training
xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 13, watchlist = list(val=dtest,train=dtrain), print_every_n = 10, early_stop_round = 10, maximize = F , eval_metric = "error")
#model prediction
xgbpred <- predict (xgb1,dtest)
xgbpred <- ifelse (xgbpred > 0.5,1,0)

#confusion matrix
library(caret)
xgbpred<-as.factor(xgbpred)
ts_label<-as.factor(ts_label)
cm<-confusionMatrix (xgbpred,ts_label)
cm$table
fourfoldplot(cm$table, color = c("#F70A4A", "#14F48E"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")

#view variable importance plot
mat <- xgb.importance (feature_names = colnames(new_tr),model = xgb1)
xgb.plot.importance (importance_matrix = mat[1:12])

ggplot(num, aes(x=number_inpatient, fill=ds$readmitted)) + geom_density(alpha=.3)
ggplot(num, aes(x=number_inpatient, fill=ds$readmitted)) + geom_boxplot(alpha=.3)
describeBy(num$number_inpatient,ds$readmitted)
TukeyHSD(aov(num$number_inpatient~ds$readmitted),data=ds)
round(prop.table(table(fact$readmitted,fact_d$discharge_disposition_id.11),1)*100,1)
round(prop.table(table(fact$readmitted,fact_d$discharge_disposition_id.1),1)*100,1)
round(prop.table(table(fact$readmitted,fact_d$discharge_disposition_id.22),1)*100,1)
