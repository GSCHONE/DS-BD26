library(gbm)
library(caret)
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


pred_test = predict.gbm(object = model_gbm,
                        newdata = test,
                        n.trees = 500,           # 500 tress to be built
                        type = "response")
class_names = colnames(pred_test)[apply(pred_test, 1, which.max)]
result = data.frame(test$Y, class_names)
 

conf_mat = confusionMatrix(as.factor(test$Y), as.factor(class_names))
print(conf_mat)
