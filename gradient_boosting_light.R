library(gbm)
library(caret)
library(tidyverse)


##### CLASSIF
parts = createDataPartition(DON_VL_CLASS$Y, p = 0.7, list = F)
train = DON_VL_CLASS[parts, ]
test = DON_VL_CLASS[-parts, ]

##### REGRESSION
   #AWAY  

train_A = DON_VL_A[parts, ]
test_A = DON_VL_A[-parts, ]

model_gbm_VLA = gbm(Y ~.,
                data = train_A,
                distribution = "poisson",
                cv.folds = 10,
                shrinkage = .01,
                n.minobsinnode = 5,
                n.trees = 2000)   

ntree_class_cv_VLA<-gbm.perf(model_gbm_VLA,method="cv")

pred_test_A = predict.gbm(object = model_gbm_VLA,
                        newdata = test_A,
                        n.trees = ntree_class_cv_VLA,          
                        type = "response")
result_A=data.frame(PRED=round(pred_test_A,0),test_A$Y)
summary(result_A$PRED)
RMSE(result_A$PRED,result_A$test_A.Y)

#HOME

train_H = DON_VL_H[parts, ]
test_H = DON_VL_H[-parts, ]

model_gbm_VLH = gbm(Y ~.,
                 data = train_H,
                 distribution = "poisson",
                 cv.folds = 10,
                 shrinkage = .01,
                 n.minobsinnode = 5,
                 n.trees = 2000)   

ntree_class_cv_VLH<-gbm.perf(model_gbm_VLH,method="cv")

pred_test_H = predict.gbm(object = model_gbm_VLH,
                          newdata = test_H,
                          n.trees = ntree_class_cv_VLH,          
                          type = "response")
result_H=data.frame(PRED=round(pred_test_H,0),test_H$Y)
summary(result_H$PRED)
RMSE(result_H$PRED,result_H$test_H.Y)


####

final=data.frame(test$Y,pred_h=round(pred_test_H,0),pred_a=round(pred_test_A,0))
final %>% 
     mutate(pred_poisson=case_when(
          pred_h>pred_a~"H",
          pred_a>pred_h~"A",
          pred_a==pred_h~"D")) ->final
final %>% select(test.Y,pred_poisson)->final     
final$pred_poisson=as.factor(final$pred_poisson)          

conf_mat2 = confusionMatrix(final$test.Y, final$pred_poisson)
conf_mat2

