install.packages("corrplot")
install.packages("caTools")
install.packages("quadprog")
install.packages("pROC")

library(corrplot)
library(caTools)
library(quadprog)
library(pROC)

data = read.csv("/Users/meshokk/RStudio_apps/Cancer_Data.csv")
# https://www.kaggle.com/datasets/erdemtaha/cancer-data
head(data)
str(data)
summary(data)
nrow(data)

# NA odstranenie
na_counts <- numeric(length(colnames(data)))

for (i in 1:length(colnames(data))) {
  col_name <- colnames(data)[i]
  na_counts[i] <- sum(is.na(data[[col_name]]))
  print(paste("Stlpec:", col_name, "- NA:", na_counts[i]))
}

valid_cols <- c()
for (i in 1:length(colnames(data))) {
  if (sum(is.na(data[[colnames(data)[i]]])) < nrow(data)) {
    valid_cols <- c(valid_cols, colnames(data)[i])
  }
}
data <- data[, valid_cols]

# ID odstranenie
if ("id" %in% colnames(data)) {
  data <- data[, !colnames(data) %in% c("id")]
}

# Kodovanie cielovej premennej (M = 1, B = -1)
data$diagnosis <- ifelse(data$diagnosis == "M", 1, -1)
print(table(data$diagnosis))
#summary(data)

# Korelacia
numeric_cols <- sapply(data, is.numeric)
numeric_cols["diagnosis"] <- FALSE
cor_matrix <- cor(data[, numeric_cols])
png("correlation_matrix_readable.png", width = 1000, height = 1000)
corrplot(cor_matrix, method = "color", col = colorRampPalette(c("blue", "white", "red"))(200),
         type = "upper", order = "hclust", addCoef.col = "black", tl.cex = 0.8, number.cex = 0.7,
         tl.col = "black", cl.cex = 1, cl.pos = "r", mar = c(1, 1, 1, 1))
dev.off()
print("korelačná matica uložená do correlation_matrix_readable.png")

cor_threshold <- 0.9
high_correlations <- which(abs(cor_matrix) > cor_threshold, arr.ind = TRUE)
high_correlations <- high_correlations[high_correlations[,1] != high_correlations[,2], ]

print("Atribúty s vysokou koreláciou (>|0.9|):")
for (i in 1:nrow(high_correlations)) {
  row <- high_correlations[i, ]
  print(paste(rownames(cor_matrix)[row[1]], "<->", colnames(cor_matrix)[row[2]], "=", round(cor_matrix[row[1], row[2]], 2)))
}

# Odstranenie atributov s vysokou korelaciou
remove_features <- c()
for (i in 1:(ncol(cor_matrix) - 1)) {
  for (j in (i + 1):ncol(cor_matrix)) {
    if (abs(cor_matrix[i, j]) > cor_threshold) {
      feature1 <- colnames(cor_matrix)[i]
      feature2 <- colnames(cor_matrix)[j]
      
      # uprednostniť "worst" alebo "se" 
      if (grepl("_worst", feature2) | grepl("_se", feature2)) {
        remove_features <- c(remove_features, feature2)
      } else {
        remove_features <- c(remove_features, feature1)
      }
    }
  }
}
remove_features <- unique(remove_features)
data <- data[, !colnames(data) %in% remove_features]
print("Boli odstránené vysoko korelované atribúty:")
print(remove_features)

# Delime na dataset na mnoziny
set.seed(123)
split <- sample.split(data$diagnosis, SplitRatio = 0.7)
train <- subset(data, split == TRUE)
test <- subset(data, split == FALSE)

# Kontrola distribucii tried
print("Distribúcia tried")
table(train$diagnosis)

X_train <- as.matrix(train[, !colnames(train) %in% "diagnosis"])
y_train <- train$diagnosis
X_test <- as.matrix(test[, !colnames(test) %in% "diagnosis"])
y_test <- test$diagnosis

# Uloženie priemeru a štandardnej odchylky pre každy atribut
scaling_info <- lapply(colnames(X_train), function(col) {
  values <- X_train[, col]
  list(mean = mean(values), sd = sd(values))
})
names(scaling_info) <- colnames(X_train)
saveRDS(scaling_info, file = "/Users/meshokk/RStudio_apps/scaling_info.rds")

# Štandardizacia
X_train <- scale(X_train)
X_test <- scale(X_test)

svm_train <- function(X, y, C = 1) {
  N <- nrow(X)
  Dmat <- (y %*% t(y)) * (X %*% t(X))
  dvec <- rep(1, N)
  Amat <- cbind(matrix(y, N, 1), diag(N))
  bvec <- c(0, rep(0, N))
  meq <- 1
  
  result <- solve.QP(Dmat + diag(1e-5, N), dvec, Amat, bvec, meq)
  alphas <- result$solution
  
  w <- t(X) %*% (alphas * y)
  sv_idx <- which(alphas > 1e-5)
  b <- mean(y[sv_idx] - X[sv_idx, ] %*% w)
  
  list(w = w, b = b)
}

svm_predict <- function(model, X) {
  pred <- X %*% model$w + model$b
  ifelse(pred >= 0, 1, -1)
}

model <- svm_train(X_train, y_train)
predictions <- svm_predict(model, X_test)

# ROC-krivka и AUC
logits <- X_test %*% model$w + model$b
roc_obj <- roc(response = y_test, predictor = as.numeric(logits))
plot(roc_obj, col = "darkblue", lwd = 2,
     main = "ROC-krivka")
auc_value <- auc(roc_obj)
cat("AUC:", auc_value, "\n")


# Residuals
errors <- predictions != y_test
plot(as.numeric(y_test), col = ifelse(errors, "red", "green"),
     pch = 16, main = "Chyby klasifikácie (residuály)",
     ylab = "Skutočná diagnóza", xlab = "Index vzorky")
legend("topright", legend = c("Správne", "Chybne"),
       col = c("green", "red"), pch = 16)

# Vypocet metrik
compute_metrics <- function(real, pred) {
  real <- factor(real, levels = c(-1, 1))
  pred <- factor(pred, levels = c(-1, 1))
  
  confusion <- table(pred, real)
  print("Confusion Matrix:")
  print(confusion)
  
  
  TP <- confusion["1", "1"]
  TN <- confusion["-1", "-1"]
  FP <- confusion["1", "-1"]
  FN <- confusion["-1", "1"]
  
  accuracy <- (TP + TN) / sum(confusion)
  precision <- if ((TP + FP) == 0) NA else TP / (TP + FP)
  recall <- if ((TP + FN) == 0) NA else TP / (TP + FN)
  f1 <- if (is.na(precision) || is.na(recall) || (precision + recall) == 0) NA else
    2 * precision * recall / (precision + recall)
  
  metrics <- list(
    Accuracy = accuracy,
    Precision = precision,
    Recall = recall,
    F1 = f1
  )
  
  return(metrics)
}

# Metriky (Accuracy, Precision, Recall, F1)
metrics <- compute_metrics(y_test, predictions)
print(metrics)

# Vizualizacia vah atributov
feature_weights <- data.frame(
  Feature = colnames(X_train),
  Weight = as.numeric(model$w)
)
feature_weights$AbsWeight <- abs(feature_weights$Weight)
feature_weights <- feature_weights[order(-feature_weights$AbsWeight), ]
par(mar = c(5, 8, 4, 2))  
barplot(
  height = rev(feature_weights$AbsWeight),
  names.arg = rev(feature_weights$Feature),
  horiz = TRUE,
  las = 1,
  col = "steelblue",
  main = "Dôležitosť atribútov",
  xlab = "Absolútna hodnota váhy",
  cex.names = 0.7
)

# Uloženie modelu,zoznamu atributov
saveRDS(model, file = "/Users/meshokk/RStudio_apps/svm_trained_model.rds")
saveRDS(colnames(X_train), file = "/Users/meshokk/RStudio_apps/model_features.rds")
