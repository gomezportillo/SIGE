## -------------------------------------------------------------------
## Sistemas Inteligentes para la Gestión en la Empresa
## Pedro Manuel Gómez-Portillo López
## -------------------------------------------------------------------

setwd("~/GitHub/SIGE/")
  
library(tidyverse)
library(dplyr)
library(funModeling)
options(max.print = 999)

set.seed(1)


# 1. Lectura de datos ----------------------------------------------

data_raw <- read_csv('data/train.csv') # na = c('NA')

str(data_raw)

summary(data_raw)

status <- df_status(data_raw) # ???con esto sabemos que hay columnas con un 0.02% de NA

# glimpse(data_raw)


# 2. Eliminación de columnas no útiles -----------------------------

## Identificar columnas con más del 0.01% de los valores a NA (aunque actualmente no exista ninguno)

na_cols <- status %>%
  filter(p_na > 0.01) %>%
  select(variable)

## Eliminarlas

remove_cols <- bind_rows(
  list(
    na_cols
  )
)

## Actualizar el dataset

data <- data_raw %>%
  select(-one_of(remove_cols$variable))

