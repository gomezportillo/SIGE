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

options(max.print = 999)

set.seed(0)

# 1. Definición de funciones ------------------------------------------

my_roc <-
  function(data,
           predictionProb,
           target_var,
           positive_class) {
    auc <-
      roc(data[[target_var]], predictionProb[[positive_class]], levels = unique(data[[target_var]]))
    roc <-
      plot.roc(
        auc,
        ylim = c(0, 1),
        type = "S" ,
        print.thres = T,
        main = paste('AUC:', round(auc$auc[[1]], 2))
      )
    return(list("auc" = auc, "roc" = roc))
  }


trainRF <-
  function(train_data) {
    rfCtrl <- trainControl(
      method = "repeatedcv",
      number = 10,
      classProbs = TRUE,
      repeats = 1,
      summaryFunction = twoClassSummary
    )
    
    rfParametersGrid <-
      expand.grid(.mtry = c(sqrt(ncol(train_data))))
    
    rfModel <- train(
      target ~ .,
      data = train_data,
      method = "rf",
      metric = "ROC",
      verbose = FALSE,
      trControl = rfCtrl,
      tuneGrid = rfParametersGrid
    )
    
    return(rfModel)
  }


# 2. Lectura de datos ya preprocesados --------------------------------

data_raw <- read_csv('out/processed.csv')
data <- data_raw %>%
  na.exclude() %>%
  mutate(target = as.factor(target))

# 3. Clasificación de los datos ---------------------------------------

## Estudio del equilibrio de clases

table(data$target)
ggplot(data) +
  geom_histogram(aes(x = target, fill = target), stat = 'count')


## Crear modelo de predicción usando rf con partición aleatoria

trainIndex <-
  createDataPartition(data$target,
                      p = .75,
                      list = FALSE,
                      times = 1)
train_data <- data[trainIndex,]
val   <- train_data[-trainIndex,]
table(train_data$target)
table(val$target)

### Arreglar nombres inválidos https://stackoverflow.com/a/34833973/3594238

feature.names=names(train_data)

for (f in feature.names) {
  if (class(train_data[[f]])=="factor") {
    levels <- unique(c(train_data[[f]]))
    train_data[[f]] <- factor(train_data[[f]],
                         labels=make.names(levels))
  }
}

train_data$Class <- factor(train_data$Class)

rfModel <- trainRF(train_data)
saveRDS(rfModel, file = 'out/model1.rds')
rfModel <- readRDS('out/model1.rds')
print(rfModel)


prediction_p <- predict(rfModel, val, type = "prob")
prediction_r <- predict(rfModel, val, type = "raw")
result <- my_roc(val, prediction_p, "target", "Paid")

plotdata <- val %>%
  select(target) %>%
  bind_cols(prediction_p) %>%
  bind_cols(Prediction = prediction_r)

table(plotdata$target, plotdata$Prediction)  # columnas son predicciones
ggplot(plotdata) +
  geom_bar(aes(x = target, fill = Prediction), position = position_fill())