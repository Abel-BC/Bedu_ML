#packages usados
library(corrplot) 
library(psych) 
library(ggplot2)
library(ggpubr)
library(scales)
library(lattice)
library(caret)
library(yardstick) 
library(DMwR)
#remotes::install_github("cran/DMwR")
library(pROC) 
library(rattle)

# DATOS -------------------------------------------------------------------
#Acceder a la base de datos
churn_data <- read.csv("C:/Users/LENOVO/Desktop/EQUIPO 23 R/churn_database.csv")

#Verifico datos faltantes
anyNA(churn_data)
#En este caso no existen datos faltantes

#Verifico los tipos de variables y cantidad de registros
summary(churn_data)
str(churn_data)
#3333 obs., 20 variables predictoras (4 categ. y 16 continuas)
# y 1 variable respuesta (churn = FALSE / TRUE).

#Se observa un abandono de clientes de 14.5% aprox.
table(churn_data$churn) #Realizo un conteo y calculo el %
Tot<-data.frame(categorias=c("Churn","Active"),
                porcentaje=c(14.5,85.5))
#Ring graphic (x=1 pastel, x=2 anillo)
ggplot(Tot,aes(x=2,y=porcentaje, fill=categorias))+
  geom_bar(stat = "identity",
           color="white")+
  geom_text(aes(label=percent(porcentaje/100)),
            position=position_stack(vjust=0.5),color="black",size=6)+
  coord_polar(theta = "y")+
  scale_fill_manual(values=c("deepskyblue3","green"))+
  theme_void()+
  ggtitle('Abandono de clientes')+
  theme(plot.title = element_text(size=14, hjust=0.5))+
  xlim(0.5,2.5)


# MANEJO DE VARIABLES -----------------------------------------------------
#Grafica del promedio por estado, analizo los estados con mayor y menor churn
bar0 <- ggplot(churn_data, aes(state, fill = churn)) + 
  geom_bar(stat = "fill") + 
  labs(x = "Area code", y = "") + theme(legend.position="bottom")


bar0

#ELIMINO VARIABLES (state y phone.number)
churn_data$state <- NULL
churn_data$phone.number <- NULL


#VARIABLES CATEG?RICAS
churn_data$area.code <- as.factor(churn_data$area.code)

bar1 <- ggplot(churn_data, aes(area.code, fill = churn)) + 
  geom_bar(position = "fill") + 
  labs(x = "Area code", y = "") + theme(legend.position="bottom")
bar2 <- ggplot(churn_data, aes(international.plan, fill = churn)) + geom_bar(position = "fill") + labs(x = "International plan", y = "") + theme(legend.position="bottom")
bar3 <- ggplot(churn_data, aes(voice.mail.plan, fill = churn)) + geom_bar(position = "fill") + labs(x = "Voicemail plan", y = "") + theme(legend.position="bottom")
bar4 <- ggplot(churn_data, aes(customer.service.calls, fill = churn)) + geom_bar(position = "fill") + labs(x = "Customer calls", y = "") + theme(legend.position="bottom")
bar <- ggarrange(bar1, bar2, bar3, bar4, ncol = 4, nrow=1, common.legend = TRUE, legend="bottom")
bar <- annotate_figure(bar, top = text_grob("Proporción de abandono (Var. Categóricas)"))
bar
# Area code, no interviene en el abandono
# Es mayor el abandono si no tiene Voicemail(correo de voz)
# Con International plan hay 3 veces m?s abandono que el promedio
# Si aumentan las Customer call a m?s de 4 aumenta el abandono 

#ELIMINO area code
churn_data$area.code <- NULL


#VARIABLES CONTINUAS
#Reviso correlaci?n de variables num?ricas totales
corrplot::corrplot(cor(churn_data[sapply(churn_data, is.numeric)]))

#Observo correlaci?n perfecta en 4 pares de variables
#ELIMINO total.day.minutes, total.eve.minutes, total.night.minutes y total.intl.minutes
churn_data$total.day.minutes <- NULL
churn_data$total.eve.minutes <- NULL
churn_data$total.night.minutes <- NULL
churn_data$total.intl.minutes <- NULL


#Reviso correlaci?n de variables num?ricas restantes con su distribuci?n
pairs.panels((churn_data[sapply(churn_data, is.numeric)]), 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)



# PARTICION DE DATOS ------------------------------------------------------

#Particionamos los datos de entrenamiento y validaci?n
set.seed(123)
part <- caret::createDataPartition(churn_data$churn, p = .8, list = FALSE, times = 1)
Train <- churn_data[ part,]
Test <- churn_data[-part, ]
#Reviso balanceo en los datos particionados
table(Train$churn)
table(Test$churn)
#Tenemos la misma proporci?n 

#Cross-validation (k=10)
Control <- trainControl(method = "cv", number = 10)



# MODELOS -----------------------------------------------------------------

# Modelo Log?stico --------------------------------------------------------

#customer.service.calls debe ser integer para evitar que se separe
set.seed(123)
model.log <- train(churn ~ ., method = "glm", family = "binomial", data = Train, trControl = Control)
summary(model.log)

#Enlistamos la importancia de las variables predictoras
car.log <- ggplot(varImp(model.log)) + ggtitle('Caracter?sticas importantes')+
  theme(plot.title = element_text(size=12, hjust=0.1))

#rendimiento del modelo
pred.log <- predict(model.log, newdata = Test)
rend.log <- round(1-mean(pred.log!=Test$churn),2)

#Confusion matrix
cm.log <- conf_mat(table(Truth=Test$churn, Prediction=pred.log))
gcm.log <- autoplot(cm.log, type = "heatmap") + 
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1") + ggtitle("Confusión matrix") + 
  theme(plot.title = element_text(size=12, hjust=0.5))

#Desempe?o del modelo (Sensibilidad)
roc.log <- roc(response = Test$churn, predictor = as.numeric(pred.log))

auc.log <- ggroc(roc.log, colour = "blue") + ggtitle(label = "AUC=", subtitle = round((roc.log$auc),4)) + 
  theme(plot.title = element_text(size=11, hjust=0.3), plot.subtitle = element_text(size=11, hjust=0.7, vjust = 8))+
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="red", linetype="dashed")


r.log <- ggarrange(car.log,                                                 # First row with scatter plot
          ggarrange(gcm.log, auc.log, ncol = 2, labels = c("B", "C")), # Second row with box and dot plots
          nrow = 2, 
          labels = "A"                                        # Labels of the scatter plot
) 
r.log



# Modelo Log?stico con SMOTE (datos desbalanceados) -----------------------
str(Train)
#tengo que tener la variable respuesta en factor y las otras 2 var también
Train$churn <- as.factor(Train$churn)
Train$international.plan <- as.factor(Train$international.plan)
Train$voice.mail.plan <- as.factor(Train$voice.mail.plan)

#hago un remuestreo SMOTE para equilibrar los datos
Train <- SMOTE (churn ~., Train, perc.over = 100, perc.under = 200)

#Verifico equilibrio
prop.table(table(Train$churn))

#Pruebo nuevamente el modelo
set.seed(123)
model.logS <- train(churn ~ ., method = "glm", family = "binomial", data = Train, trControl = Control)
summary(model.logS)

#Enlistamos la importancia de las variables predictoras
car.logS <- ggplot(varImp(model.logS)) + ggtitle('Características importantes')+
  theme(plot.title = element_text(size=12, hjust=0.1))

#rendimiento del modelo
pred.logS <- predict(model.logS, newdata = Test)
rend.logS <- round(1-mean(pred.logS!=Test$churn),2)

#Confusion matrix
cm.logS <- conf_mat(table(Truth=Test$churn, Prediction=pred.logS))
gcm.logS <- autoplot(cm.logS, type = "heatmap") + 
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1") + ggtitle("Confusión matrix") + 
  theme(plot.title = element_text(size=12, hjust=0.5))

#Desempe?o del modelo (Sensibilidad)
roc.logS <- roc(response = Test$churn, predictor = as.numeric(pred.logS))

auc.logS <- ggroc(roc.logS, colour = "blue") + ggtitle(label = "AUC=", subtitle = round((roc.logS$auc),4)) + 
  theme(plot.title = element_text(size=11, hjust=0.3), plot.subtitle = element_text(size=11, hjust=0.7, vjust = 8))+
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="red", linetype="dashed")

r.logS <- ggarrange(car.logS,                                                 # First row with scatter plot
                   ggarrange(gcm.logS, auc.logS, ncol = 2, labels = c("B", "C")), # Second row with box and dot plots
                   nrow = 2, 
                   labels = "A"                                        # Labels of the scatter plot
) 
r.logS



# Classification And Regression Tree (CART) (rpart) -----------------------
#Vuelvo a realizar la partici?n de datos para reiniciar Train y Test
set.seed(123)
model.rpart <- train(churn ~ ., method = "rpart", data = Train, trControl = Control)

#Enlistamos la importancia de las variables predictoras
car.rpart <- ggplot(varImp(model.rpart)) + ggtitle('Características importantes')+
  theme(plot.title = element_text(size=12, hjust=0.1))

#rendimiento del modelo
pred.rpart <- predict(model.rpart, newdata = Test)
rend.rpart <- round(1-mean(pred.rpart!=Test$churn),2)

#Confusion matrix
cm.rpart <- conf_mat(table(Truth=Test$churn, Prediction=pred.rpart))
gcm.rpart <- autoplot(cm.rpart, type = "heatmap") + 
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1") + ggtitle("Confusión matrix") + 
  theme(plot.title = element_text(size=12, hjust=0.5))

#Desempe?o del modelo (Sensibilidad)
roc.rpart <- roc(response = Test$churn, predictor = as.numeric(pred.rpart))

auc.rpart <- ggroc(roc.rpart, colour = "blue") + ggtitle(label = "AUC=", subtitle = round((roc.rpart$auc),4)) + 
  theme(plot.title = element_text(size=11, hjust=0.3), plot.subtitle = element_text(size=11, hjust=0.7, vjust = 8))+
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="red", linetype="dashed")

r.rpart <- ggarrange(car.rpart,                                                 # First row with scatter plot
                     ggarrange(gcm.rpart, auc.rpart, ncol = 2, labels = c("B", "C")), # Second row with box and dot plots
                     nrow = 2, 
                     labels = "A"                                        # Labels of the scatter plot
) 
r.rpart

#Ver arbol
fancyRpartPlot(model.rpart$finalModel, caption ="Arbol de clasificación (rpart)")



# CART (rpart2) -----------------------------------------------------------
set.seed(123)
model.rpart2 <- train(churn ~ ., method = "rpart2", data = Train, trControl = Control)

#Enlistamos la importancia de las variables predictoras
car.rpart2 <- ggplot(varImp(model.rpart2)) + ggtitle('Características importantes')+
  theme(plot.title = element_text(size=12, hjust=0.1))

#rendimiento del modelo
pred.rpart2 <- predict(model.rpart2, newdata = Test)
rend.rpart2 <- round(1-mean(pred.rpart2!=Test$churn),2)

#Confusion matrix
cm.rpart2 <- conf_mat(table(Truth=Test$churn, Prediction=pred.rpart2))
gcm.rpart2 <- autoplot(cm.rpart2, type = "heatmap") + 
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1") + ggtitle("Confusión matrix") + 
  theme(plot.title = element_text(size=12, hjust=0.5))

#Desempe?o del modelo (Sensibilidad)
roc.rpart2 <- roc(response = Test$churn, predictor = as.numeric(pred.rpart2))

auc.rpart2 <- ggroc(roc.rpart2, colour = "blue") + ggtitle(label = "AUC=", subtitle = round((roc.rpart2$auc),4)) + 
  theme(plot.title = element_text(size=11, hjust=0.3), plot.subtitle = element_text(size=11, hjust=0.7, vjust = 8))+
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="red", linetype="dashed")

r.rpart2 <- ggarrange(car.rpart2,                                                 # First row with scatter plot
                      ggarrange(gcm.rpart2, auc.rpart2, ncol = 2, labels = c("B", "C")), # Second row with box and dot plots
                      nrow = 2, 
                      labels = "A"  # Labels of the scatter plot
) 
r.rpart2

#Ver arbol
fancyRpartPlot(model.rpart2$finalModel, caption ="Arbol de clasificación (rpart)")



# CART (rf) ---------------------------------------------------------------
set.seed(123)
model.rf <- train(churn ~ ., method = "rf", data = Train, trControl = Control)

#Enlistamos la importancia de las variables predictoras
car.rf <- ggplot(varImp(model.rf)) + ggtitle('Características importantes')+
  theme(plot.title = element_text(size=12, hjust=0.1))

#rendimiento del modelo
pred.rf <- predict(model.rf, newdata = Test)
rend.rf <- round(1-mean(pred.rf!=Test$churn),2)

#Confusion matrix
cm.rf <- conf_mat(table(Truth=Test$churn, Prediction=pred.rf))
gcm.rf <- autoplot(cm.rf, type = "heatmap") + 
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1") + ggtitle("Confusión matrix") + 
  theme(plot.title = element_text(size=12, hjust=0.5))

#Desempe?o del modelo (Sensibilidad)
roc.rf <- roc(response = Test$churn, predictor = as.numeric(pred.rf))

auc.rf <- ggroc(roc.rf, colour = "blue") + ggtitle(label = "AUC=", subtitle = round((roc.rf$auc),4)) + 
  theme(plot.title = element_text(size=11, hjust=0.3), plot.subtitle = element_text(size=11, hjust=0.7, vjust = 8))+
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="red", linetype="dashed")

r.rf <- ggarrange(car.rf,                                                 # First row with scatter plot
                  ggarrange(gcm.rf, auc.rf, ncol = 2, labels = c("B", "C")), # Second row with box and dot plots
                  nrow = 2, 
                  labels = "A"                                        # Labels of the scatter plot
) 
r.rf



# CART (Bag) -----------------------------------------------------------------
set.seed(123)
model.bag <- train(churn ~ ., method = "treebag", data = Train, trControl = Control)

#Enlistamos la importancia de las variables predictoras
car.bag <- ggplot(varImp(model.bag)) + ggtitle('Características importantes')+
  theme(plot.title = element_text(size=12, hjust=0.1))

#rendimiento del modelo
pred.bag <- predict(model.bag, newdata = Test)
rend.bag <- round(1-mean(pred.bag!=Test$churn),2)

#Confusion matrix
cm.bag <- conf_mat(table(Truth=Test$churn, Prediction=pred.bag))
gcm.bag <- autoplot(cm.bag, type = "heatmap") + 
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1") + ggtitle("Confusión matrix") + 
  theme(plot.title = element_text(size=12, hjust=0.5))

#Desempe?o del modelo (Sensibilidad)
roc.bag <- roc(response = Test$churn, predictor = as.numeric(pred.bag))

auc.bag <- ggroc(roc.bag, colour = "blue") + ggtitle(label = "AUC=", subtitle = round((roc.bag$auc),4)) + 
  theme(plot.title = element_text(size=11, hjust=0.3), plot.subtitle = element_text(size=11, hjust=0.7, vjust = 8))+
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="red", linetype="dashed")

r.bag <- ggarrange(car.bag,                                                 # First row with scatter plot
                   ggarrange(gcm.bag, auc.bag, ncol = 2, labels = c("B", "C")), # Second row with box and dot plots
                   nrow = 2, 
                   labels = "A"                                        # Labels of the scatter plot
) 
r.bag



# Gradient Boost Machines (gbm) -------------------------------------------
set.seed(123)
model.gbm <- train(churn ~ ., method = "gbm", data = Train, trControl = Control)

#Enlistamos la importancia de las variables predictoras
car.gbm <- summary(model.gbm)
summary(model.gbm)

#rendimiento del modelo
pred.gbm <- predict(model.gbm, newdata = Test)
rend.gbm <- round(1-mean(pred.gbm!=Test$churn),2)

#Confusion matrix
cm.gbm <- conf_mat(table(Truth=Test$churn, Prediction=pred.gbm))
gcm.gbm <- autoplot(cm.gbm, type = "heatmap") + 
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1") + ggtitle("Confusión matrix") + 
  theme(plot.title = element_text(size=12, hjust=0.5))

#Desempe?o del modelo (Sensibilidad)
roc.gbm <- roc(response = Test$churn, predictor = as.numeric(pred.gbm))

auc.gbm <- ggroc(roc.gbm, colour = "blue") + ggtitle(label = "AUC=", subtitle = round((roc.gbm$auc),4)) + 
  theme(plot.title = element_text(size=11, hjust=0.3), plot.subtitle = element_text(size=11, hjust=0.7, vjust = 8))+
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="red", linetype="dashed")

r.gbm <- ggarrange(car.rf,                                                 # First row with scatter plot
                   ggarrange(gcm.gbm, auc.gbm, ncol = 2, labels = c("B", "C")), # Second row with box and dot plots
                   nrow = 2, 
                   labels = "A"                                        # Labels of the scatter plot
) 
r.gbm



# SVM Radial (svm) --------------------------------------------------------------
set.seed(123)
model.svm <- train(churn ~ ., method = "svmRadial", data = Train, trControl = Control)

#Enlistamos la importancia de las variables predictoras
car.svm <- ggplot(varImp(model.svm)) + ggtitle('Características importantes')+
  theme(plot.title = element_text(size=12, hjust=0.1))

#rendimiento del modelo
pred.svm <- predict(model.svm, newdata = Test)
rend.svm <- round(1-mean(pred.svm!=Test$churn),2)

#Confusion matrix
cm.svm <- conf_mat(table(Truth=Test$churn, Prediction=pred.svm))
gcm.svm <- autoplot(cm.svm, type = "heatmap") + 
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1") + ggtitle("Confusión matrix") + 
  theme(plot.title = element_text(size=12, hjust=0.5))

#Desempe?o del modelo (Sensibilidad)
roc.svm <- roc(response = Test$churn, predictor = as.numeric(pred.svm))

auc.svm <- ggroc(roc.svm, colour = "blue") + ggtitle(label = "AUC=", subtitle = round((roc.svm$auc),4)) + 
  theme(plot.title = element_text(size=11, hjust=0.3), plot.subtitle = element_text(size=11, hjust=0.7, vjust = 8))+
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="red", linetype="dashed")

r.svm <- ggarrange(car.svm,                                                 # First row with scatter plot
                   ggarrange(gcm.svm, auc.svm, ncol = 2, labels = c("B", "C")), # Second row with box and dot plots
                   nrow = 2, 
                   labels = "A"                                        # Labels of the scatter plot
) 
r.svm



# ADA Boost (ada) ---------------------------------------------------------------
set.seed(123)
model.ada <- train(churn ~ ., method = "ada", data = Train, trControl = Control)

#Enlistamos la importancia de las variables predictoras
car.ada <- ggplot(varImp(model.ada)) + ggtitle('Características importantes')+
  theme(plot.title = element_text(size=12, hjust=0.1))

#rendimiento del modelo
pred.ada <- predict(model.ada, newdata = Test)
rend.ada <- round(1-mean(pred.ada!=Test$churn),2)

#Confusion matrix
cm.ada <- conf_mat(table(Truth=Test$churn, Prediction=pred.ada))
gcm.ada <- autoplot(cm.ada, type = "heatmap") + 
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1") + ggtitle("Confusión matrix") + 
  theme(plot.title = element_text(size=12, hjust=0.5))

#Desempe?o del modelo (Sensibilidad)
roc.ada <- roc(response = Test$churn, predictor = as.numeric(pred.ada))

auc.ada <- ggroc(roc.ada, colour = "blue") + ggtitle(label = "AUC=", subtitle = round((roc.ada$auc),4)) + 
  theme(plot.title = element_text(size=11, hjust=0.3), plot.subtitle = element_text(size=11, hjust=0.7, vjust = 8))+
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="red", linetype="dashed")

r.ada <- ggarrange(car.ada,                                                 # First row with scatter plot
                   ggarrange(gcm.ada, auc.ada, ncol = 2, labels = c("B", "C")), # Second row with box and dot plots
                   nrow = 2, 
                   labels = "A") 
r.ada



# Naive Bayes (bay) -------------------------------------------------------------
set.seed(123)
model.bay <- train(churn ~ ., method = "nb", data = Train, trControl = Control)

#Enlistamos la importancia de las variables predictoras
car.bay <- ggplot(varImp(model.bay)) + ggtitle('Caracter?sticas importantes')+
  theme(plot.title = element_text(size=12, hjust=0.1))

#rendimiento del modelo
pred.bay <- predict(model.bay, newdata = Test)
rend.bay <- round(1-mean(pred.bay!=Test$churn),2)

#Confusion matrix
cm.bay <- conf_mat(table(Truth=Test$churn, Prediction=pred.bay))
gcm.bay <- autoplot(cm.bay, type = "heatmap") + 
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1") + ggtitle("Confusi?n matrix") + 
  theme(plot.title = element_text(size=12, hjust=0.5))

#Desempe?o del modelo (Sensibilidad)
roc.bay <- roc(response = Test$churn, predictor = as.numeric(pred.bay))

auc.bay <- ggroc(roc.bay, colour = "blue") + ggtitle(label = "AUC=", subtitle = round((roc.bay$auc),4)) + 
  theme(plot.title = element_text(size=11, hjust=0.3), plot.subtitle = element_text(size=11, hjust=0.7, vjust = 8))+
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="red", linetype="dashed")

r.bay <- ggarrange(car.bay,                                                 # First row with scatter plot
                   ggarrange(gcm.bay, auc.bay, ncol = 2, labels = c("B", "C")), # Second row with box and dot plots
                   nrow = 2, 
                   labels = "A") 
r.bay


# GRAPHICS AUC ----------------------------------------------------------------
barauc <- ggarrange(auc.log, auc.logS, auc.rpart, auc.rpart2, auc.rf, auc.bag, auc.gbm, auc.svm, auc.ada, auc.bay, ncol = 5, nrow=2, common.legend = TRUE, legend="bottom")
barauc


install.packages("xgboost")
library("xgboost")

# CART (XGBoost) -----------------------------------------------------------------
set.seed(123)

model.xgb <- train(churn ~ ., method = "xgbTree", 
                   data = Train, trControl = Control, 
                   metric = "Accuracy",
                   verbose = FALSE,
                   verbosity = 0)
                  
max(model.xgb$results$Accuracy)


#Enlistamos la importancia de las variables predictoras
car.xgb <- ggplot(varImp(model.xgb)) + ggtitle('Características importantes')+
  theme(plot.title = element_text(size=12, hjust=0.1))

#rendimiento del modelo
pred.xgb <- predict(model.xgb, newdata = Test)
rend.xgb <- round(1-mean(pred.xgb!=Test$churn),2)

#Confusion matrix
cm.xgb <- conf_mat(table(Truth=Test$churn, Prediction=pred.xgb))
gcm.xgb <- autoplot(cm.bag, type = "heatmap") + 
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1") + ggtitle("Confusión matrix") + 
  theme(plot.title = element_text(size=12, hjust=0.5))

#Desempe?o del modelo (Sensibilidad)
roc.xgb <- roc(response = Test$churn, predictor = as.numeric(pred.xgb))

auc.xgb <- ggroc(roc.xgb, colour = "blue") + ggtitle(label = "AUC=", subtitle = round((roc.xgb$auc),4)) + 
  theme(plot.title = element_text(size=11, hjust=0.3), plot.subtitle = element_text(size=11, hjust=0.7, vjust = 8))+
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="red", linetype="dashed")

r.xgb <- ggarrange(car.xgb,                                                 # First row with scatter plot
                   ggarrange(gcm.xgb, auc.xgb, ncol = 2, labels = c("B", "C")), # Second row with box and dot plots
                   nrow = 2, 
                   labels = "A"                                        # Labels of the scatter plot
) 
r.xgb
