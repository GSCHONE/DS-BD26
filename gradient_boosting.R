library(gbm)
library(caret)
library(tidyverse)


##### CLASSIF

DON_CLASS$Y=as.factor(DON_CLASS$Y)
parts = createDataPartition(DON_CLASS$Y, p = 0.7, list = F)
train = DON_CLASS[parts, ]
test = DON_CLASS[-parts, ]


model_gbm = gbm(Y ~.,
                data = train,
                distribution = "multinomial",
                cv.folds = 10,
                shrinkage = .01,
                n.minobsinnode = 10,
                n.trees = 500)   
ntree_class_cv<-gbm.perf(model_gbm,method="cv")
ntree_class_oob<-gbm.perf(model_gbm,method="OOB")
ntree_class_oob

pred_test = predict.gbm(object = model_gbm,
                        newdata = test,
                        n.trees = ntree_class_cv,         
                        type = "response")
class_names = colnames(pred_test)[apply(pred_test, 1, which.max)]
result = data.frame(test$Y, class_names)
 

conf_mat = confusionMatrix(test$Y, as.factor(class_names))
print(conf_mat)

##### REGRESSION
   #AWAY  
 
DON_A %>% 
     select(-away_team_goal)->DON_A
parts = createDataPartition(DON_A$Y, p = 0.7, list = F)
train_A = DON_A[parts, ]
test_A = DON_A[-parts, ]

model_gbmA = gbm(Y ~.,
                data = train_A,
                distribution = "poisson",
                cv.folds = 10,
                shrinkage = .01,
                n.minobsinnode = 10,
                n.trees = 500)   

ntree_class_cv_A<-gbm.perf(model_gbmA,method="cv")

pred_test_A = predict.gbm(object = model_gbmA,
                        newdata = test_A,
                        n.trees = ntree_class_cv_A,          
                        type = "response")
result_A=data.frame(PRED=round(pred_test_A,0),test_A$Y)
summary(result_A$PRED)
RMSE(result_A$PRED,result_A$test_A.Y)

#HOME


DON_H %>% 
     select(-away_team_goal)->DON_H
parts = createDataPartition(DON_H$Y, p = 0.7, list = F)
train_H = DON_H[parts, ]
test_H = DON_H[-parts, ]

model_gbmH = gbm(Y ~.,
                 data = train_H,
                 distribution = "poisson",
                 cv.folds = 10,
                 shrinkage = .01,
                 n.minobsinnode = 10,
                 n.trees = 500)   

ntree_class_cv_H<-gbm.perf(model_gbmH,method="cv")

pred_test_H = predict.gbm(object = model_gbmH,
                          newdata = test_H,
                          n.trees = ntree_class_cv_H,          
                          type = "response")
result_H=data.frame(PRED=round(pred_test_H,0),test_H$Y)
summary(result_H$PRED)
RMSE(result_H$PRED,result_H$test_H.Y)
