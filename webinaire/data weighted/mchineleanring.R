#Packages

install.packages("caret")
library(caret)
library(MASS)

data(BostonHousing)

# Import data
boston <- Boston
head(boston)

# structure des données
str(boston)

# résumé statistique
summary(boston)

# melange aléatoire des données : Train / Test

seed <- 123
set.seed(seed)

rows <- sample(nrow(boston))
boston_mel <- boston[rows,]
head(boston_mel)

# Division aléatoire des données 80% train; 20%test

split <- round(nrow(boston_mel)* 0.80)

train <- boston_mel[1:split,]
test <- boston_mel[(split + 1):nrow(boston_mel),]
dim(test)
dim(train)

# Construction du modèle à partir des données d'entrainement

model <- lm(medv ~ ., data = train)
model

# fonction d'évaluation

model_evaluation <- function(model){
       
       #prediction train
       preds_train <- predict(model, train)
       
       #prediction test
       preds_test <- predict(model, test)
       
       #Erreur sur le train
       error_train <- preds_train-train[,"medv"]
       
       #Erreur sur le test
       error_test <- preds_test-test[,"medv"]
       
       #RMSE train
       rmse_train <- sqrt(mean(error_train**2))
       
       #RMSE test
       rmse_test <- sqrt(mean(error_test**2))
       
       print(paste("RMSE sur le train data :", rmse_train))
       print(paste("RMSE sur le test data :", rmse_test))
       
       if(rmse_train > rmse_test){
              print(paste("Overfitting"))
       }
       else{
              print(paste("performant"))
       }
}

model_evaluation(model)

# Validation croisée // regression

model_cv <- train(medv ~ .,
                  data = train,
                  method ="lm",
                  trControl = trainControl(method = "cv",
                            number = 5
                            )
                  )
model_cv
model_evaluation(model_cv)

# Validation croisée répétée

model_cv2 <- train(medv ~ .,
                  data = train,
                  method ="lm",
                  trControl = trainControl(method = "repeatedcv",
                                           number = 5,
                                           repeats = 5
                  )
)
model_cv2
model_evaluation(model_cv2)

# model avec prétraitement

#standardisation des données

model_stand <- train(medv ~ .,
                  data = train,
                  method ="lm",
                  preProcess = c("center", "scaler"),
                  trControl = trainControl(method = "cv",
                                           number = 5
                                           )
                   )
model_stand
model_evaluation(model_stand)

# Utiliser une ACP

model_pca<- train(medv ~ .,
                     data = train,
                     method ="lm",
                     preProcess = c("center", "scaler","pca"),
                     trControl = trainControl(method = "cv",
                                              number = 5
                     )
)
model_pca
model_evaluation(model_pca)










