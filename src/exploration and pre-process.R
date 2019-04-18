## -------------------------------------------------------------------
## Sistemas Inteligentes para la Gestión en la Empresa
## Pedro Manuel Gómez-Portillo López
## -------------------------------------------------------------------

# 0. Configuración del entorno ---------------------------------------

setwd("~/GitHub/SIGE/")

library(tidyverse)
library(dplyr)
library(funModeling)
library(corrplot)
library(Hmisc)
library(ggplot2)
library(NoiseFiltersR)

options(max.print = 999)

set.seed(0)


# 1. Lectura de datos -----------------------------------------------

data_raw <- read_csv('data/train.csv') # na = c('NA')

str(data_raw)

summary(data_raw)

status <-
  df_status(data_raw) # ???con esto sabemos que hay columnas con un 0.02% de NA

glimpse(data_raw)


# 2. Eliminación de columnas y filas no útiles ----------------------

## Eliminar la columna ID por no aportar nada
data <- select(data_raw,-ID_code)


## Eliminar las filas con valores NA (https://stackoverflow.com/a/4862264/3594238)
data <- na.omit(data)


# Cálculo del diagrama de dispersión
bp <- boxplot(data)
bp$stats


## Es imposible saber de momento si el resto de columnas son útiles o no,
##  así que vamos a intentar averiguarlo

## Calcular la correlación entre las variables
data_num <- data %>%
  na.exclude() %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.factor, as.numeric)

cor_target <- correlation_table(data_num, target = 'target')
correlated_vars <- cor_target %>%
  filter(abs(target) >= 0.05)


## Cuando tenemos las columnas las seleccionamos del dataset
data_num <- data_num %>%
  select(one_of(correlated_vars$Variable))


## Alta correlacion entre sí
rcorr_result <- rcorr(as.matrix(data_num))
cor_matrix <- as_tibble(rcorr_result$r, rownames = "variable")
corrplot(
  rcorr_result$r,
  type = "upper",
  order = "original",
  tl.col = "black",
  tl.srt = 45
)


## Representarlo en un diagrama
v <- varclus(as.matrix(data_num), similarity = "pearson")
plot(v)


## Sustitir los 0 y 1 de target pro Yes y No
data_num <-
  data_num %>%
  mutate(target = as.factor(ifelse(target == 1, 'Yes', 'No')))


## Eliminar ruido
# noise_filter <- AENN(target ~ ., data_num)
# summary(noise_filter)
# saveRDS(noise_filter, "out/noise_filter.Rdata")
# identical(noise_filter$cleanData, data_num[setdiff(1:nrow(data_num), noise_filter$remIdx), ])


# Guardar los cambios hechos en un nuevo archivo
write_csv(data_num, "out/processed.csv")
