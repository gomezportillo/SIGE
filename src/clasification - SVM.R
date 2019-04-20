## -------------------------------------------------------------------
## Sistemas Inteligentes para la Gestión en la Empresa
## Pedro Manuel Gómez-Portillo López
## -------------------------------------------------------------------

# 0. Configuración del entorno ---------------------------------------

setwd("~/GitHub/SIGE/")

library(caret)
library(tidyverse)
library(funModeling)
library(pROC)
library(DMwR)
library(lubridate)

options(max.print = 999)

set.seed(0)

# 1. Lectura de datos ya preprocesados --------------------------------

data_raw <- read_csv('out/processed.csv')
data <- data_raw %>%
  na.exclude() %>%
  mutate(target = as.factor(target))

# # reducir datos al 5% para pruebas más rápidas
# tmp_index <- createDataPartition(data$target,
#                                  p = .5,
#                                  list = FALSE,
#                                  times = 1)
# data <- data[tmp_index,]

# 2. Clasificación de los datos ---------------------------------------

## Crear dos particiones aleatoria

part_index <-
  createDataPartition(data$target,
                      p = .75,
                      list = FALSE,
                      times = 1)

train_data <- data[part_index, ]
val_data   <- data[-part_index, ]
table(train_data$target)
table(val_data$target)

## Configurar los datos para entrenar los modelso

# RepeatedCV: resampling method: diviging the dataset into 5 subsets and testing each one and averaging the error
train_Ctrl <- trainControl(
  verboseIter = F,
  classProbs = TRUE,
  method = "repeatedcv",
  number = 5,
  repeats = 1,
  summaryFunction = twoClassSummary
)

# tune grid lets us decide which values the main parameter will take
train_TuneGrid <- expand.grid(.mtry = c(sqrt(ncol(train_data))))

# SVM lineal

time_prev <- now()

svmLinealModel <-
  train(
    target ~ .,
    data = train_data,
    method = "svmLinear",
    metric = "ROC",
    trControl = train_Ctrl
  )

(elapsed_time <- now() - time_prev)

saveRDS(svmLinealModel, file = 'out/svmLinealModel.rds')
print(svmLinealModel)

prediction_p <- predict(svmLinealModel, val_data, type = "prob")
prediction_r <- predict(svmLinealModel, val_data, type = "raw")
(conf_matrix <- confusionMatrix(prediction_r, val_data$target))

plotdata <- val_data %>%
  select(target) %>%
  bind_cols(prediction_p) %>%
  bind_cols(Prediction = prediction_r)

table(plotdata$target, plotdata$Prediction)
ggplot(plotdata) +
  geom_bar(aes(x = target, fill = Prediction), position = position_fill())


# SVM radial

time_prev <- now()

svmRadialModel <-
  train(
    target ~ .,
    data = train_data,
    method = "svmRadial",
    metric = "ROC",
    trControl = train_Ctrl
  )

(elapsed_time <- now() - time_prev)

saveRDS(svmRadialModel, file = 'out/svmRadialModel.rds')
print(svmRadialModel)

prediction_p <- predict(svmRadialModel, val_data, type = "prob")
prediction_r <- predict(svmRadialModel, val_data, type = "raw")
(conf_matrix <- confusionMatrix(prediction_r, val_data$target))

plotdata <- val_data %>%
  select(target) %>%
  bind_cols(prediction_p) %>%
  bind_cols(Prediction = prediction_r)

table(plotdata$target, plotdata$Prediction)
ggplot(plotdata) +
  geom_bar(aes(x = target, fill = Prediction), position = position_fill())
