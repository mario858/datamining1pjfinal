# Proyecto final 
# Grupo 3
# Estuardo Umana
# Mario Morales 
# Alejandro Cortes

# Librerias
library(readr)

library(dplyr)
library(ggplot2)

library(rpart)
library(rpart.plot)

library(corrplot)

library(vcd)
library(vcdExtra)

library(caret)
library(weights)
library(e1071)
library(pROC)

# Set de data
train <- read_csv("Data/train.csv", col_types = cols(id = col_number(), 
                                                   Age = col_number(), 
                                                   Driving_License = col_number(), 
                                                   Region_Code = col_number(), 
                                                   Previously_Insured = col_number(), 
                                                   Annual_Premium = col_number(), 
                                                   Policy_Sales_Channel = col_number(), 
                                                   Vintage = col_number(),
                                                   Response = col_number()
                                                   )
                 )
# Ver informacion
View(train)

# Explorando un par de datos
head(train)
summary(train)

# Exploremos un poco cada variable para encontrar nulls/nas
table(train$id)
table(train$Gender)
table(train$Age)
table(train$Driving_License)
table(train$Region_Code)
table(train$Previously_Insured)
table(train$Vehicle_Age)
table(train$Vehicle_Damage)
table(train$Annual_Premium)
table(train$Policy_Sales_Channel)
table(train$Vintage)
table(train$Response)

# Estamos libres de pecado, amen /\
for (i in train) {
  View(train[is.na(i),])
}

# Preparamos el set
train$Gender <- as.factor(train$Gender)
train$Vehicle_Age <- as.factor(train$Vehicle_Age)
train$Vehicle_Damage <- as.factor(train$Vehicle_Damage)
train$Previously_Insured<- as.factor(train$Previously_Insured)
train$Response <- as.factor(train$Response)

# Como se divide el valor de Response
table(train$Response)
# Respuesta positiva
(46710/(334399+46710)) * 100
# Respuesta negativa
(334399/(334399+46710)) * 100

# Revisamos todas las variables
variables <- c(
  train[2],
  train[4],
  train[6],
  train[7],
  train[8]
  )

# Revisamos categoricas
varNameCount <- 1
varPos <- c(2,4,6,7,8)
for (i in variables) {
  mosaicplot(
    Response ~ i,
    data = train,
    ylab = names(train)[varPos[varNameCount]],
    color = train$Response,
    shade = TRUE
  )
  varNameCount <- varNameCount + 1
}

# Revisamos variables cuantitativas
train %>% 
  ggplot() +
  aes(x = Gender, y = Age, color = Response) +
  geom_boxplot() +
  ggtitle("Por Genero y Edad")

train %>% 
  ggplot() +
  aes(x = Age, y = Annual_Premium, color = Response) +
  geom_histogram(stat='identity') +
  ggtitle("Por Edad y Monto acumulado de poliza")
