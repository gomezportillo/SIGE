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

options(max.print = 999)

set.seed(0)


# 1. Lectura de datos -----------------------------------------------

data_raw <- read_csv('data/train.csv') # na = c('NA')

str(data_raw)

summary(data_raw)

status <- df_status(data_raw) # ???con esto sabemos que hay columnas con un 0.02% de NA

glimpse(data_raw)


# 2. Eliminación de columnas y filas no útiles ----------------------

## Eliminar la columna ID por no aportar nada
data <- select(data_raw, -ID_code)

## Eliminar las filas con valores NA (https://stackoverflow.com/a/4862264/3594238)
data <- na.omit(data)

## Es imposible saber de momento si el resto de columnas son útiles o no, así que vamos
## a intentar averiguarlo

## Calcular la correlación entre las variables
# cor_matrix <- cor(data, use = "complete.obs")
# round(cor_matrix, 2)

cor_matrix <- rcorr(as.matrix(data))
corrplot(cor_matrix$r, type = "upper", order = "hclust",  p.mat = cor_matrix$P, sig.level = 0.01, insig = "blank")

## Usar un mapa de calor (no vale pa na)
# palette <- colorRampPalette(c("blue", "white", "red"))(20)
# heatmap(x = cor_matrix$r, col = palette, symm = TRUE)


## Ahora borraremos las columnas que tengan menos de un 0.02% de correlacion con la columna target
# data <- data %>%
#   mutate(target = case_when(
#     target == 0  ~ 'Yes',
#     target == 1 ~ 'No'))

data_num <- data %>%
  na.exclude() %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.factor, as.numeric)

cor_target <- correlation_table(data_num, target='target')
correlated_vars <- cor_target %>% 
  filter(abs(target) >= 0.05)

## Cuando tenemos las columnas las seleccionamos del dataset
data_num <- data_num %>%
  select(one_of(correlated_vars$Variable))

## Alta correlacion entre sí
rcorr_result <- rcorr(as.matrix(data_num))
cor_matrix <- as_tibble(rcorr_result$r, rownames = "variable")
corrplot(rcorr_result$r, type = "upper", order = "original", tl.col = "black", tl.srt = 45)

## Representarlo en un diagrama
v <- varclus(as.matrix(data_num), similarity="pearson") 
plot(v)

write_csv(data_num, 'out/processed.csv')

## Eliminar ruido

### TODO