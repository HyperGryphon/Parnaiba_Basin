rm(list = ls())
graphics.off()

pacman::p_load(caret,GGally,magrittr,pacman,parallel,randomForest,rattle,rio,tictoc,tidyverse)

setwd("C:/Users/iande/Documentos/proyectos/ML with lumi data/")
data <- readxl::read_excel("osl_parnaiba_table.xlsx", sheet = "by_aliquot") %>%
  print()

data <- rename(data, 'IRSL'='IRSL1.2s/BOSL1s')%>%
  print()
units <- unique(data$Unit)

data$BOSLF = as.numeric(data$BOSLF)
data$BOSLM = as.numeric(data$BOSLM)
data$BOSLS = as.numeric(data$BOSLS)

na.samples <- data$sample[which(is.na(data$BOSLF))]

for (i in 1:length(na.samples)) {
  data$BOSLF[which(data$sample==na.samples[i] & is.na(data$BOSLF))] <- mean(na.omit(data$BOSLF[which(data$sample==na.samples[i])]))
  data$BOSLM[which(data$sample==na.samples[i] & is.na(data$BOSLM))] <- mean(na.omit(data$BOSLM[which(data$sample==na.samples[i])]))
  data$BOSLS[which(data$sample==na.samples[i] & is.na(data$BOSLS))] <- mean(na.omit(data$BOSLS[which(data$sample==na.samples[i])]))
}

data.scaled <- data %>% mutate_at(c(5:12), ~(scale(.) %>% as.vector))

frac <- .5
u1 <- data.scaled[which(data$Unit==units[1]),] %>% sample_frac(frac)
u2 <- data.scaled[which(data$Unit==units[2]),] %>% sample_frac(frac)
u3 <- data.scaled[which(data$Unit==units[3]),] %>% sample_frac(frac)
u4 <- data.scaled[which(data$Unit==units[4]),] %>% sample_frac(frac)
u5 <- data.scaled[which(data$Unit==units[5]),] %>% sample_frac(frac)
u6 <- data.scaled[which(data$Unit==units[6]),] %>% sample_frac(frac)
u7 <- data.scaled[which(data$Unit==units[7]),] %>% sample_frac(frac)
u8 <- data.scaled[which(data$Unit==units[8]),] %>% sample_frac(frac)

# Random forest of decision trees on training data ####
#set.seed(123)
train = rbind(u1,u2,u3,u4,u5,u6,u7,u8)
test = anti_join(data.scaled, train)

control = trainControl(method = "repeatedcv", number = 10, repeats = 3, search = "random",
                       allowParallel = T)

model = train(Unit ~ BOSL1s + IRSL + TL110oC + TL110pos + TL325pos,
               data = train, method = "svmLinear3", trControl = control,
               metric = "Accuracy")
              #, tuneLength = 15, ntree = 300)

model
model %>% plot()
model$finalModel
model$finalModel %>% plot()

data.predict = predict(model, newdata = train)
table(actualclass = train$Unit, predictedclass = data.predict) %>% 
  confusionMatrix() %>% 
  print()

#Random forest on test data
data.predict = predict(model, newdata = test)
table(actualclass = test$Unit, predictedclass = data.predict) %>% 
  confusionMatrix() %>% 
  print()

