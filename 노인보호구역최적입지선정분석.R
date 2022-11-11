################ 데이터 불러오기
library(sf)
dd <- st_read("C:/Users/user/Desktop/R/35_1.shp") 
sum(is.na(dd))

#################### 회귀분석 -전체변수로
m1<- point_acc~ bus_stop + market + parking + road + cctv + senior + pharmacy + public_bat+ food +c_center+bank+hospital+subway_ent+church+pop_old+light
model_1<- lm(m1, data=dd)
summary(model_1)

#################### 단계적 변수선택법 

step_model <- step(lm(point_acc~1, data=dd), scope=list(lower=~1,upper=~cctv + bus_stop + parking + pop_old + food + pharmacy + church 
                                                        + market + road + hospital + public_bat + bank +
                                                          c_center + subway_ent + senior + light),direction = "both")
summary(step_model)

#################### 회귀분석-선정된 변수들로 
m2<- point_acc~ bus_stop + market + pharmacy + road + bank + subway_ent + light
model_2 <- lm(m2, data=dd)
summary(model_2)

#################### VIF (다중공선성 검정)
library(car)
vif(model_2)

#################### 모란지수 
# 주변과 비슷한지 1번이랑 붙어있는 2번지역이 비슷한 경향으로 나타나는지
library(spdep)
nb0 <- poly2nb(dd)
lw0 <- nb2listw(nb0)

dd$residuals_1 <- residuals(model_2)
moran.mc(dd$residuals_1, lw0, 999)
moran.test(dd$residuals_1, lw0)


#################### Durbin-Watson (자기상관성검정)
# dw = 1.7705, 2에 가까으므로 독립
library(lmtest)
dwtest(model_2)

#################### 라그랑주 승수(LMtest = Lagrange Multiplier test)
#p-value가 모두 유의, 공간적 자기상관성 존재 
nb0 <- poly2nb(dd)
lw0 <- nb2listw(nb0,zero.policy=TRUE)

lm.LMtests(model_2,lw0, test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))


#데이터 불러오기 
#################### 데이터 불러오기
library(sf)
d<- st_read("C:/Users/user/Desktop/R/35_1.shp") 

#NULL 확인 = 0
sum(is.na(d))

#################### d를 Train 80 test 20 으로 분리 
library(caret)
set.seed(525)
indexes = createDataPartition(d$point_acc, p = .80, list = F)
d_train = d[indexes, ]
d_test = d[-indexes, ]

# train = grid 삭제
names(d_train)
d_train = d_train[, -1]
names(d_train)

#train 데이터에서 X에서 종속변수 삭제, y에 종속변수 할당
train_x = d_train[, -16]
names(train_x)
train_y = d_train[,16]
names(train_y)

#test =grid 삭제
names(d_test)
d_test = d_test[, -1]
names(d_test)

#train 데이터에서 X에서 종속변수 삭제, y에 종속변수 할당
test_x = d_test[, -16]
names(train_x)
test_y = d_test[,16]
names(test_y)

#################### d_train data로 k-fold 검정 실행(요약)
# 코드 돌리지 마세요

# fold는 10개로 나눠준 후, 같은값을 도출하기 위해 set.seed(525)로 설정
k <- 10
times <- 3
set.seed(525)
cv <- createMultiFolds(d_train$point_acc, k=10)

#################### lm_model

sink("lm_test_kfoldtest.txt")

k <- 10
times <- 3
set.seed(525)
cv <- createMultiFolds(d_train$point_acc, k=10, times=3)

for (i in 1:times) {
  for (j in 1:k) {
    #train 80, test 20
    train_idx <-createDataPartition(d_train$point_acc , p = .80, list = F)
    d_train.train <- d_train[train_idx, ]
    d_train.validation <- d_train[-train_idx, ]
    
    #lm_model
    lm_model<- lm(point_acc~ bus_stop + market + pharmacy + road + bank + subway_ent + light, data=d_train.train)
    
    
    #회귀분석 오차검정
    rmse<-function(y1,y2){sqrt(mean((y1-y2)^2))}
    mae<-function(y1,y2){mean(abs(y1-y2))}
    y_predict_test_lm<-predict(lm_model,newdata=d_train.validation)
    rmse_test_lm<-rmse(y_predict_test_lm,d_train.validation$point_acc)
    mae_adj_test_lm <- mae(y_predict_test_lm,d_train.validation$point_acc) 
    
    #결과값 출력
    print(summary(lm_model))
    print(rmse_test_lm)
    print(mae_adj_test_lm )
    print(AIC(lm_model))
    
  }
}
sink()

#################### SLM_model
sink("slm_test_kfoldtest.txt")

k <- 10
times <- 3
set.seed(525)
cv <- createMultiFolds(d_train$point_acc, k=10, times=3)

for (i in 1:times) {
  for (j in 1:k) {
    train_idx <-createDataPartition(d_train$point_acc , p = .80, list = F)
    d_train.train <- d_train[train_idx, ]
    d_train.validation <- d_train[-train_idx, ]
    
    library(spdep)
    nb <- poly2nb(d_train.train)
    lw <- nb2listw(nb,zero.policy=TRUE)
    
    # slm_model
    library(spatialreg)
    slm_model <- lagsarlm(point_acc~ bus_stop + market + pharmacy + road + bank + subway_ent + light, data=d_train.train, listw=lw, tol.solve=1.0e-10, zero.policy = TRUE)
    
    # 회귀분석 오차검정
    rmse<-function(y1,y2){sqrt(mean((y1-y2)^2))}
    mae<-function(y1,y2){mean(abs(y1-y2))}
    
    nb2 <- poly2nb(d_train.validation)
    lw2 <- nb2listw(nb2,zero.policy=TRUE)
    
    y_predict_test_slm<-predict(slm_model, newdata=d_train.validation, listw=lw2,zero.policy=TRUE)
    rmse_test_slm<-rmse(y_predict_test_slm,d_train.validation$point_acc)
    mae_adj_test_slm <- mae(y_predict_test_slm,d_train.validation$point_acc) 
    
    # 결과값 출력
    print(summary(slm_model))
    print(rmse_test_slm)
    print(mae_adj_test_slm) 
  }
}
sink()

#################### sem_model
sink("sem_test_kfoldtest.txt")

k <- 10
times <- 3
set.seed(525)
cv <- createMultiFolds(d_train$point_acc, k=10, times=3)

for (i in 1:times) {
  for (j in 1:k) {
    train_idx <-createDataPartition(d_train$point_acc , p = .80, list = F)
    d_train.train <- d_train[train_idx, ]
    d_train.validation <- d_train[-train_idx, ]
    
    library(spdep)
    nb <- poly2nb(d_train.train)
    lw <- nb2listw(nb,zero.policy=TRUE)
    
    # sem_model
    library(spatialreg)
    sem_model <- errorsarlm(point_acc~ bus_stop + market + pharmacy + road + bank + subway_ent + light, data=d_train.train, listw=lw, tol.solve=1.0e-10, zero.policy = TRUE)
    
    #회귀분석 오차검정
    rmse<-function(y1,y2){sqrt(mean((y1-y2)^2))}
    mae<-function(y1,y2){mean(abs(y1-y2))}
    
    nb2 <- poly2nb(d_train.validation)
    lw2 <- nb2listw(nb2,zero.policy=TRUE)
    
    y_predict_test_sem<-predict(sem_model,newdata=d_train.validation, listw=lw2,zero.policy=TRUE)
    rmse_test_sem<-rmse(y_predict_test_sem,d_train.validation$point_acc)
    mae_adj_test_sem <- mae(y_predict_test_sem,d_train.validation$point_acc) 
    
    # 결과값 출력
    print(summary(sem_model))
    print(rmse_test_sem)
    print(mae_adj_test_sem) 
  }
}
sink()



####################   K-fold 결과, lm / slm /sem 중 ~~~ 가 선정됨.

####################  LM_MODEL 
final_1<- point_acc~ bus_stop + market + pharmacy + road + bank + subway_ent + light
final_LM_1<- lm(final_1, data=d_train)
summary(final_LM_1)

rmse<-function(y1,y2){sqrt(mean((y1-y2)^2))}
mae<-function(y1,y2){mean(abs(y1-y2))}
final_y_predict_test_lm<-predict(final_LM_1,newdata=d_test)
final_rmse_test_lm<-rmse(final_y_predict_test_lm,d_test$point_acc)
final_mae_adj_test_lm <- mae(final_y_predict_test_lm,d_test$point_acc) 

#결과값 출력
print(summary(final_LM_1))
print(final_rmse_test_lm)
print(final_mae_adj_test_lm)
print(AIC(final_LM_1))

#################### SLM_MODEL

nb3 <- poly2nb(d_train)
lw3 <- nb2listw(nb3,zero.policy=TRUE)

final_slm_model <- lagsarlm(point_acc~ bus_stop + market + pharmacy + road + bank + subway_ent + light, data=d_train, listw=lw3, tol.solve=1.0e-10, zero.policy = TRUE)

# 회귀분석 오차검정
rmse<-function(y1,y2){sqrt(mean((y1-y2)^2))}
mae<-function(y1,y2){mean(abs(y1-y2))}

nb4 <- poly2nb(d_test)
lw4 <- nb2listw(nb4,zero.policy=TRUE)

final_y_predict_test_slm<-predict(final_slm_model, newdata=d_test, listw=lw4,zero.policy=TRUE)
final_rmse_test_slm<-rmse(final_y_predict_test_slm,d_test$point_acc)
final_mae_adj_test_slm <- mae(final_y_predict_test_slm,d_test$point_acc) 

# 결과값 출력
print(summary(final_slm_model))
print(final_rmse_test_slm)
print(final_mae_adj_test_slm) 

#################### SEM_MODEL

nb3 <- poly2nb(d_train)
lw3 <- nb2listw(nb3,zero.policy=TRUE)

# sem_model
library(spatialreg)
final_sem_model <- errorsarlm(point_acc~ bus_stop + market + pharmacy + road + bank + subway_ent + light, data=d_train, listw=lw3, tol.solve=1.0e-10, zero.policy = TRUE)

#회귀분석 오차검정
rmse<-function(y1,y2){sqrt(mean((y1-y2)^2))}
mae<-function(y1,y2){mean(abs(y1-y2))}

nb4 <- poly2nb(d_test)
lw4 <- nb2listw(nb4,zero.policy=TRUE)

final_y_predict_test_sem<-predict(final_sem_model, newdata=d_test, listw=lw4,zero.policy=TRUE)
final_rmse_test_sem<-rmse(final_y_predict_test_sem,d_test$point_acc)
final_mae_adj_test_sem <- mae(final_y_predict_test_sem,d_test$point_acc) 

# 결과값 출력
print(summary(final_sem_model))
print(final_rmse_test_sem)
print(final_mae_adj_test_sem ) 

