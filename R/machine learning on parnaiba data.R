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

data %>%
  pull(BOSL1s) %T>%
  boxplot() %>%
  median()

osl.t = 1:length(data$BOSL1s)
for (i in 1:length(data$BOSL1s)) {
  if (data$BOSL1s[i]>9) {
    osl.t[i] = "High"
  } else if (data$BOSL1s[i]<4) {
    osl.t[i] = "Low"
  } else {
    osl.t[i] = "Medium"}
}

data %<>%
  #mutate(osl.t = ifelse(osl>=9,"High","Medium"), osl.t = as_factor(osl.t)) %>%
  mutate(osl.t = as.factor(osl.t)) %>%
  print()

data %>%
  ggplot()+
  geom_bar(aes(x = osl.t, fill = osl.t))+
  theme(legend.position = "none")

# compare every variable ####
tic("Scatterplot Matrix")
data %>%
  select(osl.t, BOSL1s, BOSLF:TL325pos, Group:Unit) %>%
  ggpairs()
toc()

t.test(data$BOSL1s,data$BOSLF)

# fit linear regression ####
fit.lm = data %>%
  select(BOSL1s, BOSLF:TL325pos, Group:Unit) %>%
  lm()
fit.lm %>% summary()
#fit.lm %>% plot()

# K nearest neighbour (KNN) on training data ####
set.seed(123)

#random subsample
#data %>% sample_n(250)

train = data %>% sample_frac(.66)
test = anti_join(data, train)

statctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)

fit.knn = train(Unit ~ BOSL1s + BOSLF + BOSLM + BOSLS + IRSL + TL110oC + TL110pos + TL325pos,
                data = train, method = "knn", trControl = statctrl, tuneLength = 20,
                na.action = "na.omit")

tic("KNN")
fit.knn
toc()

osl.p = fit.knn %>% 
  predict(newdata = train)

table(actualclass = train$Unit, predictedclass = osl.p) %>% 
  confusionMatrix() %>% 
  print()

#KNN on test data
osl.p = predict(fit.knn, newdata = test)
table(actualclass = test$Unit, predictedclass = osl.p) %>% 
  confusionMatrix() %>% 
  print()

#Decision tree on training data ####
set.seed(10000)
train = data %>% sample_frac(.50)
test = anti_join(data, train)

train %>%
  gather(var, val, IRSL, BOSLF) %>%
  ggplot(aes(val,group = Unit, fill = Unit)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~var)+
  theme(legend.position = "bottom")

#tic("Decision tree") # train data
fit.dt = train(Unit ~ BOSL1s + TL110oC + TL110pos + TL325pos,
               data = train, method = "rpart", trControl = trainControl(method = "cv"),
               na.action = "na.omit")
#toc()
fit.dt

fit.dt$finalModel
fit.dt$finalModel %>% 
  fancyRpartPlot(main = "Predicting Unit", sub = "Training data")

osl.p = fit.dt %>% predict(newdata = train)
table(actualclass = train$Unit, predictedclass = osl.p) %>% 
  confusionMatrix() %>% 
  print()

#Decision tree on test data
osl.p = fit.dt %>% predict(newdata = test)
table(actualclass = test$Unit, predictedclass = osl.p) %>% 
  confusionMatrix() %>% 
  print()

# Random forest of decision trees on training data ####
#set.seed(123)
train = data %>% sample_frac(.66)
test = anti_join(data, train)

control = trainControl(method = "repeatedcv", number = 10, repeats = 3, search = "random",
                       allowParallel = T)

#tic("Random forest")
fit.rf = train(Unit ~ BOSL1s + BOSLF + BOSLM + BOSLS + IRSL + TL110oC + TL110pos + TL325pos,
               data = train, method = "rf", trControl = control,
               metric = "Accuracy", tuneLength = 15, ntree = 300,
               na.action = "na.omit")
#toc()
fit.rf
fit.rf %>% plot()
fit.rf$finalModel
fit.rf$finalModel %>% plot()

osl.p = predict(fit.rf, newdata = train)
table(actualclass = train$Unit, predictedclass = osl.p) %>% 
  confusionMatrix() %>% 
  print()

#Random forest on test data
osl.p = predict(fit.rf, newdata = test)
table(actualclass = test$Unit, predictedclass = osl.p) %>% 
  confusionMatrix() %>% 
  print()

# Summary of results ####
test %>% pull(osl.t) %>% summary()

# Hierarchical clustering #####
library(factoextra); library(cluster); library(magrittr)
hc = data %>%
  select(BOSL1s:TL110oC, Unit) %>% 
  dist %>% hclust

hc %>% plot(labels = data$Unit, hang = -1, cex = 0.6)

#optimal number of clusters
data %>% 
  fviz_nbclust(FUN = hcut, method = "wss")+
  geom_vline(xintercept = 3, color = "red", linetype = "dotted")

data %>% 
  fviz_nbclust(FUN = hcut, method = "silhouette")

data %>% 
  na.omit(data) %>%
  select(-Unit, -Group, -sample, -osl.t, -...1) %>%
  clusGap(FUN = hcut, nstart = 50, K.max = 10, B = 100) %>%
  fviz_gap_stat()

hc %>% rect.hclust(k = 3, border = 2:5)

y.hc = cutree(hc, 3)
fviz_cluster(list(data = data %>% 
                    select(BOSL1s, BOSLF, BOSLM, BOSLS, IRSL, TL110oC),
                  cluster = y.hc))

# K-means clustering #####
data.s = data %>% 
  select(-...1, -sample, -Group, -Unit, -osl.t) %>%
  scale() %>% 
  print()

km = data.s %>% kmeans(3) %>% print()
data.s %>% clusplot(km$cluster, color = T, lines = 0, labels = 2)

#add clusters to the dataframe
data %<>%
  mutate(cluster = km$cluster) %>% 
  select(-...1) %>%
  arrange(cluster) %>% 
  print()
