rm(list = ls())
graphics.off()

pacman::p_load(caret,GGally,magrittr,pacman,parallel,randomForest,rattle,rio,tictoc,tidyverse)

setwd("C:/Users/iande/Documentos/proyectos/ML with lumi data/")
data = readxl::read_excel("osl_parnaiba_table.xlsx", sheet = "by_aliquot") %>%
  print()

data$BOSLF = as.numeric(data$BOSLF)
data$BOSLM = as.numeric(data$BOSLM)
data$BOSLS = as.numeric(data$BOSLS)

colnames(data)[9] = "IRSL"

data$BOSLF[is.na(data$BOSLF)] = 33.333
data$BOSLM[is.na(data$BOSLM)] = 33.333
data$BOSLS[is.na(data$BOSLS)] = 33.333

set.seed(123)

training.samples <- data$BOSL1s %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]

ggplot(train.data, aes(BOSLS, BOSL1s) ) +
  geom_point() +
  stat_smooth()

# Build the model
model <- lm(BOSL1s ~ BOSLS, data = train.data)
# Make predictions
predictions <- model %>% predict(test.data)
# Model performance
data.frame(
  RMSE = RMSE(predictions, test.data$BOSL1s),
  R2 = R2(predictions, test.data$BOSL1s))

ggplot(train.data, aes(BOSLS, BOSL1s) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x)

#lm(BOSL1s ~ BOSLS + I(BOSLS^6), data = train.data)
lm(BOSL1s ~ poly(BOSLS, 4, raw = TRUE), data = train.data) %>% summary()

# Build the model
model <- lm(BOSL1s ~ poly(BOSLS, 4, raw = TRUE), data = train.data)
# Make predictions
predictions <- model %>% predict(test.data)
# Model performance
data.frame(
  RMSE = RMSE(predictions, test.data$BOSL1s),
  R2 = R2(predictions, test.data$BOSL1s))

ggplot(train.data, aes(BOSLS, BOSL1s) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE))


#splines##########################################
library(splines)
knots <- quantile(train.data$BOSLS, p = c(0.25, 0.5, 0.75))

# Build the model
knots <- quantile(train.data$BOSLS, p = c(0.25, 0.5, 0.75))
model <- lm (BOSL1s ~ bs(BOSLS, knots = knots), data = train.data)
# Make predictions
predictions <- model %>% predict(test.data)
# Model performance
data.frame(
  RMSE = RMSE(predictions, test.data$BOSL1s),
  R2 = R2(predictions, test.data$BOSL1s)
)

ggplot(train.data, aes(BOSLS, BOSL1s) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3))


#generalized additive models
library(mgcv)
# Build the model
model <- gam(BOSL1s ~ s(BOSLS), data = train.data)
# Make predictions
predictions <- model %>% predict(test.data)
# Model performance
data.frame(
  RMSE = RMSE(predictions, test.data$BOSL1s),
  R2 = R2(predictions, test.data$BOSL1s)
)
