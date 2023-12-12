################# PARTE 1  #################################################################################
rm(list=ls())
setwd("C:/Users/DELL/Downloads")
data <- read.csv2("C:/Users/DELL/Downloads/bank-additional-full.csv")
df <- as.data.frame(data)
summary(df)
df$duration <- NULL
#variable pdays numeric a factor
df$pdays <- cut(df$pdays, breaks = c(-Inf, 7, 14, 21, Inf), labels = c("last week", "last 2 weeks", "last 3 weeks", "not contacted before"))
df$pdays <- as.factor(df$pdays)
#variables caracter y entero a numeric
df[,c("previous", "campaign", "emp.var.rate", "cons.price.idx", "cons.conf.idx", "euribor3m", "nr.employed")] <- sapply(df[,c("previous", "campaign", "emp.var.rate", "cons.price.idx", "cons.conf.idx", "euribor3m", "nr.employed")], as.numeric)
#variables character a factor
df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], as.factor)
#eliminar las observaciones "unknown" de las variables job, marital, education, default, loan, housing
df <- subset(df, job != "unknown" & marital != "unknown" & education != "unknown" & default != "unknown" & loan != "unknown" & housing != "unknown")
#scale
library(dplyr)
df <- df %>%
  mutate_at(vars(age, campaign, previous, emp.var.rate, cons.price.idx, cons.conf.idx, euribor3m, nr.employed), scale)
#########################################################################################

#Ejecutar solo para la regresión logística
df <- subset(df, select = -c(nr.employed, euribor3m, emp.var.rate))
             
################# PARTE 2  #################################################################################
#EJECUTAR SOLO PARA REGRESIÓN LINEAL

library(caret)
var <- df[, !(names(df) %in% "y")]
dummies_model <- dummyVars(~ .-1, data = var)
datos <- predict(dummies_model, newdata = var)
datos <- as.data.frame(datos)
df <- cbind(datos, y = df$y)
df$y <- as.numeric(df$y)
df$y[df$y == "1"] <- "0"
df$y[df$y == "2"] <- "1"
df$y <- as.numeric(df$y)
summary(df)

library(caret)
set.seed(123)
trainIndex <- createDataPartition(df$y, p = .8, list = FALSE, times = 1)
trainSet <- df[trainIndex,]
validationSet <- df[-trainIndex,]
trainSet$y <- as.factor(trainSet$y)

install.packages("C:/Users/DELL/Downloads/DMwR_0.4.1.tar.gz", repos = NULL, type = "source")
library(DMwR)
trainSet <- SMOTE(y ~ ., data  = trainSet, k = 5, perc.over = 100, perc.under = 200)

trainSet$y <- as.numeric(trainSet$y)
trainSet$y[trainSet$y == "1"] <- "0"
trainSet$y[trainSet$y == "2"] <- "1"
validationSet$y <- as.numeric(validationSet$y)
validationSet$y[validationSet$y == "1"] <- "0"
validationSet$y[validationSet$y == "2"] <- "1"

library(caret)
ctrl <- trainControl(method = "cv", number = 10)
modelo <- train(y~., data = trainSet, method = "lm", trControl = ctrl)
pred <- predict(modelo, newdata= validationSet)
summary(pred)
MSE <- mean((validationSet$y-pred)^2)
MSE
resultado <- postResample(pred, validationSet$y)
resultado

################# PARTE 3  #################################################################################
#VOLVER A EJECUTAR PARTE 1

library(caret)
set.seed(123)
trainIndex <- createDataPartition(df$y, p = .8, list = FALSE, times = 1)
trainSet <- df[trainIndex,]
validationSet <- df[-trainIndex,]
trainSet$y <- as.factor(trainSet$y)

install.packages("C:/Users/DELL/Downloads/ROSE_0.0-4.tar.gz", repos = NULL, type = "source")
library(ROSE)
subset_vector <- df$y %in% trainSet$y
ovun_sample <- ovun.sample(y ~ ., data = df, seed = 123, method = "over", subset = subset_vector)$data
table(ovun_sample$y)
trainSet <- ovun_sample
################################################################################################

#Ejecutar solo para regresión logística
trainSet$y <- as.factor(trainSet$y)
validationSet$y <- as.factor(validationSet$y)

################# PARTE 4  #################################################################################
#Volver a ejecutar parte 1 y 3
x_train <- model.matrix(y~., trainSet)[,-1]
y_train <- as.numeric(trainSet$y)
x_val <- model.matrix(y~., validationSet)[,-1]
y_val <- as.numeric(validationSet$y)

library(glmnet)
#REGRESION RIGDE
ridge_model <- glmnet(x_train, y_train, family = "binomial", alpha = 0)
cv.ridge <- cv.glmnet(x_train, y_train, type.measure="mse", alpha=0)
best_lambda_ridge <- cv.ridge$lambda.min
ridge_pred <- predict(ridge_model, s = best_lambda_ridge, newx = x_val)
ridge_mse <- mean((ridge_pred - y_val)^2)

#REGRESION LASSO
lasso_model <- glmnet(x_train, y_train, family = "binomial", alpha = 1)
cv.lasso <- cv.glmnet(x_train, y_train, type.measure="mse", alpha=1)
best_lambda_lasso <- cv.lasso$lambda.min
lasso_pred <- predict(lasso_model, s = best_lambda_lasso, newx = x_val)
lasso_mse <- mean((lasso_pred - y_val)^2)

################# PARTE 5  #################################################################################
#Volver a ejecutar PARTE 1
#REGRESIÓN LOGISTÍCA
library(caret)
k <- 10
best_cutoff <- 0
best_f1 <- 0
probabilities <- seq(from = 0, to = 1, by = 0.01)

folds <- createFolds(trainSet$y, k = k)
for(i in 1:k) {
  validation_indices <- folds[[i]]
  validationSet <- trainSet[validation_indices, ]
  train_indices <- setdiff(1:nrow(trainSet), validation_indices)
  trainSet_fold <- trainSet[train_indices, ]
  
  model_fold <- glm(y ~ ., family = binomial, data = trainSet_fold)
  predicted_probs <- predict(model_fold, newdata = validationSet, type = "response")
  
  for(cutoff in probabilities) {
    predicted_classes <- ifelse(predicted_probs > cutoff, 'yes', 'no')
    predicted_classes <- factor(predicted_classes, levels = levels(validationSet$y))
    confusion <- confusionMatrix(predicted_classes, validationSet$y)
    f1 <- 2 * (confusion$table[2,2] / (2 * confusion$table[2,2] + confusion$table[2,1] + confusion$table[1,2]))
    if(f1 > best_f1) {
      best_f1 <- f1
      best_cutoff <- cutoff
    }
  }
}
print(paste("The best cutoff is", best_cutoff, "with an F1 score of", best_f1))
predicted_classes <- ifelse(predict(model_fold, newdata = validationSet, type = "response") > best_cutoff, 1, 0)
predicted_classes <- factor(predicted_classes, levels = levels(validationSet$y))
confusion <- confusionMatrix(predicted_classes, validationSet$y)
print(confusion$byClass)


################# PARTE 6  #################################################################################
#VOLVER A EJECUTAR PARTE 1 Y PARTE 3
#ÁRBOL DE DECISIÓN
library(tree)
set.seed(123)
tree_model <- tree(y~ ., data = trainSet)
par(mar = c(1,1,1,1))
plot(x = tree_model, type = "proportional")
text(x = tree_model, splits = TRUE, pretty = 0, cex = 0.7, col = "firebrick")

cv_tree <- cv.tree(tree_model, FUN = prune.misclass, K = 5)
size_optimo <- rev(cv_tree$size)[which.min(rev(cv_tree$dev))]
size_optimo
resultados_cv <- data.frame(n_nodos = cv_tree$size, clas_error = cv_tree$dev, alpha = cv_tree$k)
tree_final <- prune.misclass(tree = tree_model, best = size_optimo)
predicciones <- predict(tree_model, newdata = validationSet, type = "class")
table(predicciones, validationSet$y)


########################### PARTE 7 #################################
#KMEANS
#VOLVER A EJECUTAR PARTE 1
install.packages("clustMixType")
library(clustMixType)
df$y <- NULL 
k_proto <- kproto(df, 2, lambda = NULL, iter.max = 100, nstart = 1, na.rm = "yes", keep.data = TRUE, verbose = TRUE)
clprofiles(k_proto, df, vars = NULL, col = NULL)
##############################################################################################################################################################################################################


#missing values
library(DataExplorer)
plot_missing(df)

#matrix correlation variable numerical
df_num <- df[,c("age", "campaign", "previous", "emp.var.rate","cons.price.idx", "cons.conf.idx","euribor3m", "nr.employed")]
plot_correlation(df_num, type = 'continuous','Review.Date')
df_numeric <- df[,c("age", "campaign", "previous","cons.price.idx", "cons.conf.idx")]
plot_correlation(df_numeric, type = 'continuous','Review.Date')
#porcentaje de observaciones "unknown" en las variables job, marital, education, default, loan, housing
cat <- c("job", "marital", "education", "default", "loan", "housing") 
for (var in cat) {
  cat("\nProportion for", var, "\n")
  freq_table <- table(df[[var]])
  prop_table <- prop.table(freq_table)
  print(prop_table)
}

#####GRAFICOS######
#histogramas
library(ggplot2)
library(cowplot)
h.age <- ggplot(df, aes(x = age)) +
  geom_histogram(fill = "#FFB6C1", color = "#54FF9F", position = "identity") +
  ggtitle("Age distribution") +
  theme_classic()

table(df$campaign)
h.campaign <- ggplot(df, aes(x = campaign)) +
  geom_histogram(fill = "#FFB6C1", color = "#54FF9F", position = "identity") +
  ggtitle("Campaign distribution") +
  theme_classic()

table(df$previous)
h.previous <- ggplot(df, aes(x = previous)) +
  geom_histogram(fill = "#FFB6C1", color = "#54FF9F", position = "identity") +
  ggtitle("Previous distribution") +
  theme_classic()

h.emp.var.rate <- ggplot(df, aes(x = emp.var.rate)) +
  geom_histogram(fill = "#FFB6C1", color = "#54FF9F", position = "identity") +
  ggtitle("Var.emp.rate distribution") +
  theme_classic()

h.cons.price.idx  <- ggplot(df, aes(x = cons.price.idx )) +
  geom_histogram(fill = "#FFB6C1", color = "#54FF9F", position = "identity") +
  ggtitle("Cons.price.idx distribution") +
  theme_classic()

h.cons.conf.idx <- ggplot(df, aes(x = cons.conf.idx)) +
  geom_histogram(fill = "#FFB6C1", color = "#54FF9F", position = "identity") +
  ggtitle("Cons.conf.idx distribution") +
  theme_classic()

h.euribor3m <- ggplot(df, aes(x = euribor3m)) +
  geom_histogram(fill = "#FFB6C1", color = "#54FF9F", position = "identity") +
  ggtitle("euribor3m distribution") +
  theme_classic()

h.nr.employed <- ggplot(df, aes(x = nr.employed)) +
  geom_histogram(fill = "#FFB6C1", color = "#54FF9F", position = "identity") +
  ggtitle("nr.employed distribution") +
  theme_classic()

plot_grid(h.age,h.nr.employed, h.emp.var.rate, h.cons.conf.idx, nrow = 2, ncol = 2)

plot_grid(h.cons.price.idx,h.euribor3m,h.campaign, h.previous, nrow = 2, ncol = 2)

#boxplot
b.age <- ggplot(df, aes(y = age)) + 
  geom_boxplot(fill = "#7FFFD4", alpha = 0.5) +
  theme_minimal()

b.campaign <- ggplot(df, aes(y = campaign)) + 
  geom_boxplot(fill = "#7FFFD4", alpha = 0.5) +
  theme_minimal()

b.previous <- ggplot(df, aes(y = previous)) + 
  geom_boxplot(fill = "#7FFFD4", alpha = 0.5) +
  theme_minimal()

b.emp.var.rate <- ggplot(df, aes(y = emp.var.rate)) + 
  geom_boxplot(fill = "#7FFFD4", alpha = 0.5) +
  theme_minimal()

b.cons.price.idx <- ggplot(df, aes(y = cons.price.idx)) + 
  geom_boxplot(fill = "#7FFFD4", alpha = 0.5) +
  theme_minimal()

b.cons.conf.idx <- ggplot(df, aes(y = cons.conf.idx)) + 
  geom_boxplot(fill = "#7FFFD4", alpha = 0.5) +
  theme_minimal()

b.euribor3m <- ggplot(df, aes(y = euribor3m)) + 
  geom_boxplot(fill = "#7FFFD4", alpha = 0.5) +
  theme_minimal()

b.nr.employed <- ggplot(df, aes(y = nr.employed)) + 
  geom_boxplot(fill = "#7FFFD4", alpha = 0.5) +
  theme_minimal()

plot_grid(b.age, b.nr.employed, b.emp.var.rate, b.cons.conf.idx , nrow = 2, ncol = 2)

plot_grid(b.cons.price.idx,b.euribor3m, b.campaign, b.previous, nrow = 2, ncol = 2)

#variables categorical
library(ggplot2)
library(dplyr)
library(cowplot)

p.job <- df %>%
  group_by(job) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency)) %>%
  mutate(Cumulative_Frequency = cumsum(Frequency),
         Proportion = Frequency / sum(Frequency),
         Cumulative_Proportion = cumsum(Proportion)) %>%
  ggplot(aes(x = reorder(job, -Frequency), y = Proportion)) +
  geom_bar(stat = "identity", fill = "#36648B") +
  geom_line(aes(y = Cumulative_Proportion), group = 1, color = "#FF6347", size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Cumulative Proportion")) +
  labs(x = "Job", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p.marital <- df %>%
  group_by(marital) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency)) %>%
  mutate(Cumulative_Frequency = cumsum(Frequency),
         Proportion = Frequency / sum(Frequency),
         Cumulative_Proportion = cumsum(Proportion)) %>%
  ggplot(aes(x = reorder(marital, -Frequency), y = Proportion)) +
  geom_bar(stat = "identity", fill = "#36648B") +
  geom_line(aes(y = Cumulative_Proportion), group = 1, color = "#FF6347", size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Cumulative Proportion")) +
  labs(x = "marital", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p.education <- df %>%
  group_by(education) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency)) %>%
  mutate(Cumulative_Frequency = cumsum(Frequency),
         Proportion = Frequency / sum(Frequency),
         Cumulative_Proportion = cumsum(Proportion)) %>%
  ggplot(aes(x = reorder(education, -Frequency), y = Proportion)) +
  geom_bar(stat = "identity", fill = "#36648B") +
  geom_line(aes(y = Cumulative_Proportion), group = 1, color = "#FF6347", size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Cumulative Proportion")) +
  labs(x = "education", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p.default <- df %>%
  group_by(default) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency)) %>%
  mutate(Cumulative_Frequency = cumsum(Frequency),
         Proportion = Frequency / sum(Frequency),
         Cumulative_Proportion = cumsum(Proportion)) %>%
  ggplot(aes(x = reorder(default, -Frequency), y = Proportion)) +
  geom_bar(stat = "identity", fill = "#36648B") +
  geom_line(aes(y = Cumulative_Proportion), group = 1, color = "#FF6347", size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Cumulative Proportion")) +
  labs(x = "default", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p.housing <- df %>%
  group_by(housing) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency)) %>%
  mutate(Cumulative_Frequency = cumsum(Frequency),
         Proportion = Frequency / sum(Frequency),
         Cumulative_Proportion = cumsum(Proportion)) %>%
  ggplot(aes(x = reorder(housing, -Frequency), y = Proportion)) +
  geom_bar(stat = "identity", fill = "#36648B") +
  geom_line(aes(y = Cumulative_Proportion), group = 1, color = "#FF6347", size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Cumulative Proportion")) +
  labs(x = "housing", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p.loan <- df %>%
  group_by(loan) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency)) %>%
  mutate(Cumulative_Frequency = cumsum(Frequency),
         Proportion = Frequency / sum(Frequency),
         Cumulative_Proportion = cumsum(Proportion)) %>%
  ggplot(aes(x = reorder(loan, -Frequency), y = Proportion)) +
  geom_bar(stat = "identity", fill = "#36648B") +
  geom_line(aes(y = Cumulative_Proportion), group = 1, color = "#FF6347", size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Cumulative Proportion")) +
  labs(x = "loan", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p.contact <- df %>%
  group_by(contact) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency)) %>%
  mutate(Cumulative_Frequency = cumsum(Frequency),
         Proportion = Frequency / sum(Frequency),
         Cumulative_Proportion = cumsum(Proportion)) %>%
  ggplot(aes(x = reorder(contact, -Frequency), y = Proportion)) +
  geom_bar(stat = "identity", fill = "#36648B") +
  geom_line(aes(y = Cumulative_Proportion), group = 1, color = "#FF6347", size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Cumulative Proportion")) +
  labs(x = "contact", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p.month <- df %>%
  group_by(month) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency)) %>%
  mutate(Cumulative_Frequency = cumsum(Frequency),
         Proportion = Frequency / sum(Frequency),
         Cumulative_Proportion = cumsum(Proportion)) %>%
  ggplot(aes(x = reorder(month, -Frequency), y = Proportion)) +
  geom_bar(stat = "identity", fill = "#36648B") +
  geom_line(aes(y = Cumulative_Proportion), group = 1, color = "#FF6347", size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Cumulative Proportion")) +
  labs(x = "month", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p.day_of_week <- df %>%
  group_by(day_of_week) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency)) %>%
  mutate(Cumulative_Frequency = cumsum(Frequency),
         Proportion = Frequency / sum(Frequency),
         Cumulative_Proportion = cumsum(Proportion)) %>%
  ggplot(aes(x = reorder(day_of_week, -Frequency), y = Proportion)) +
  geom_bar(stat = "identity", fill = "#36648B") +
  geom_line(aes(y = Cumulative_Proportion), group = 1, color = "#FF6347", size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Cumulative Proportion")) +
  labs(x = "day_of_week", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p.pdays <- df %>%
  group_by(pdays) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency)) %>%
  mutate(Cumulative_Frequency = cumsum(Frequency),
         Proportion = Frequency / sum(Frequency),
         Cumulative_Proportion = cumsum(Proportion)) %>%
  ggplot(aes(x = reorder(pdays, -Frequency), y = Proportion)) +
  geom_bar(stat = "identity", fill = "#36648B") +
  geom_line(aes(y = Cumulative_Proportion), group = 1, color = "#FF6347", size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Cumulative Proportion")) +
  labs(x = "pdays", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p.poutcome <- df %>%
  group_by(poutcome) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency)) %>%
  mutate(Cumulative_Frequency = cumsum(Frequency),
         Proportion = Frequency / sum(Frequency),
         Cumulative_Proportion = cumsum(Proportion)) %>%
  ggplot(aes(x = reorder(poutcome, -Frequency), y = Proportion)) +
  geom_bar(stat = "identity", fill = "#36648B") +
  geom_line(aes(y = Cumulative_Proportion), group = 1, color = "#FF6347", size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Cumulative Proportion")) +
  labs(x = "poutcome", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p.y <- df %>%
  group_by(y) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency)) %>%
  mutate(Cumulative_Frequency = cumsum(Frequency),
         Proportion = Frequency / sum(Frequency),
         Cumulative_Proportion = cumsum(Proportion)) %>%
  ggplot(aes(x = reorder(y, -Frequency), y = Proportion)) +
  geom_bar(stat = "identity", fill = "#36648B") +
  geom_line(aes(y = Cumulative_Proportion), group = 1, color = "#FF6347", size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Cumulative Proportion")) +
  labs(x = "y", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot_grid(p.job, p.marital, p.education, p.default, nrow = 2, ncol = 2)
plot_grid(p.housing, p.loan,p.contact, p.month, nrow = 2, ncol = 2)
plot_grid(p.day_of_week, p.pdays, p.poutcome, p.y, nrow = 2, ncol = 2)
