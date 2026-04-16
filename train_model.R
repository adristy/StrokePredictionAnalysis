library(caret)
library(randomForest)

setwd("/Users/adristyrizki/Documents/Stroke Analysis")
df_raw = read.csv("healthcare-dataset-stroke-data.csv")
df_bersih = read.csv("datastrokeclean.csv")
colnames(df_bersih) = c("Gender", "Age", "Hypertension", "Heart Disease", "Married",
                        "Work Type", "Residence Type", "Glucose", 
                        "BMI","Smoking Status", "Stroke")
df_bersih = df_bersih %>%
  mutate(`Hypertension` = recode(`Hypertension`,
                                 "Yes" = "Iya",
                                 "No" = "Tidak"),
         `Heart Disease` = recode(`Heart Disease`,
                                  "Yes" = "Iya",
                                  "No" = "Tidak"))
df_encoded = read.csv("resampled_data.csv")
head(df_bersih)

# random forest untuk prediksi
train_index <- createDataPartition(df_encoded$Stroke, p = 0.8, list = FALSE)
train_data <- df_encoded[train_index, ]
test_data <- df_encoded[-train_index, ]
train_data$Stroke <- as.factor(train_data$Stroke)
test_data$Stroke <- as.factor(test_data$Stroke)
train_data$Hypertension <- as.factor(train_data$Hypertension)
test_data$Hypertension <- as.factor(test_data$Hypertension)
train_data$Heart.Disease <- as.factor(train_data$Heart.Disease)
test_data$Heart.Disease <- as.factor(test_data$Heart.Disease)

# Model formula
formula <- Stroke ~ Age + Hypertension + Average.Glucose.Level + BMI + Heart.Disease
tuneGrid <- expand.grid(mtry = c(2, 3, 4, 5))
set.seed(123)
rf_model <- train(formula, data = train_data, method = "rf", tuneGrid = tuneGrid, trControl = trainControl(method = "cv"))
# SAVE MODEL
saveRDS(rf_model, "rf_model.rds")
