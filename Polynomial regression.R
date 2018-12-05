library(ggplot2)
df = read.csv("C:/Users/AINI/Downloads/cars.csv")

#=============================================================================================
# POLYNOMIAL REGRESSION OF ORDER 7 of sample size 20
#=============================================================================================
set.seed(2)
ord=c(20,30,50,70,100,200,350)
Test_Error=c()
for (i in ord){
  s=sample(1:nrow(df),i)
  train=df[s,]
  test=df[-s,]
  model=lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)+ I(Weight^5) 
                 + I(Weight^6) + I(Weight^7), train)
  
  plot(main=paste('Model on order 7, n=',i),train$Weight,train$MPG, pch=19, cex=0.5,xlab = "Weight",ylab = "MPG")
  lines(sort(train$Weight), fitted(model)[order(train$Weight)], col='red', type='l',pch=20)
  
  pred = predict(model, newdata=test)
  Test_Error=append(Test_Error,sum((pred-test$MPG)^2))
  
}

plot(ord,Test_Error,pch=20,cex=0.5,xlab="Sample size",ylab="Test Error",ylim=c(0,200000), main="Test Error vs sample size" )

lines(ord,Test_Error,col="blue",type="l")


#=============================================================================================
# POLYNOMIAL REGRESSION OF ORDER 1,2,7,8,9,10 of size 20 for four different samples
#=============================================================================================
A=c(5,10,15,20)

for (i in A) {
    set.seed(i)
    s = sample(1:nrow(train),20)
    train3 = train[s, ]
    
    m1 <- lm(MPG ~ Weight, train3)
    m2 <- lm(MPG ~ Weight + I(Weight^2), train3)
    m7 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)+ I(Weight^5) 
             + I(Weight^6) + I(Weight^7), train3)
    m8 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)+ I(Weight^5) 
             + I(Weight^6) + I(Weight^7)+I(Weight^8), train3)
    m9 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)+ I(Weight^5) 
             + I(Weight^6) + I(Weight^7)+I(Weight^8)+I(Weight^9), train3)
    m10 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)+ I(Weight^5) 
              + I(Weight^6) + I(Weight^7)+I(Weight^8)+I(Weight^9)+I(Weight^10), train3)
    
    #PLOTTING THE MODEL OVER THE DATA
    plot(main='Regression Model on order 1,2,7,8,9,10',train3$Weight,train3$MPG, pch=19, cex=0.5,xlab = "Weight",ylab = "MPG")
    lines(sort(train3$Weight), fitted(m1)[order(train3$Weight)], col='red', type='l',pch=19)
    lines(sort(train3$Weight), fitted(m2)[order(train3$Weight)], col='blue', type='l',pch=19)
    lines(sort(train3$Weight), fitted(m7)[order(train3$Weight)], col='brown', type='l',pch=19)
    lines(sort(train3$Weight), fitted(m8)[order(train3$Weight)], col='green', type='l',pch=19)
    lines(sort(train3$Weight), fitted(m9)[order(train3$Weight)], col='yellow', type='l',pch=19)
    lines(sort(train3$Weight), fitted(m10)[order(train3$Weight)], col='skyblue', type='l',pch=19)
    
    Trn_RMSE1=sqrt(mean(sum(m1$residuals^2)))
    pred = predict(m1, newdata=test)
    Tst_RMSE1=sqrt(mean(sum((pred-test$MPG)^2)))
    Trn_RMSE2=sqrt(mean(sum(m2$residuals^2)))
    pred = predict(m2, newdata=test)
    Tst_RMSE2=sqrt(mean(sum((pred-test$MPG)^2)))
    Trn_RMSE7=sqrt(mean(sum(m7$residuals^2)))
    pred = predict(m7, newdata=test)
    Tst_RMSE7=sqrt(mean(sum((pred-test$MPG)^2)))
    Trn_RMSE8=sqrt(mean(sum(m8$residuals^2)))
    pred = predict(m8, newdata=test)
    Tst_RMSE8=sqrt(mean(sum((pred-test$MPG)^2)))
    Trn_RMSE9=sqrt(mean(sum(m9$residuals^2)))
    pred = predict(m9, newdata=test)
    Tst_RMSE9=sqrt(mean(sum((pred-test$MPG)^2)))
    Trn_RMSE10=sqrt(mean(sum(m10$residuals^2)))
    pred = predict(m10, newdata=test)
    Tst_RMSE10=sqrt(mean(sum((pred-test$MPG)^2)))
    
    Test_RMSE=c(Tst_RMSE1,Tst_RMSE2,Tst_RMSE7,Tst_RMSE8,Tst_RMSE9,Tst_RMSE10)
    Train_RMSE=c(Trn_RMSE1,Trn_RMSE2,Trn_RMSE7,Trn_RMSE8,Trn_RMSE9,Trn_RMSE10)
    order=c(1,2,7,8,9,10)
    plot(main=" RMSE VS MODEL COMPLEXITY(n=20)",range(1:10), range(11:60), xlab = "Order",  ylab = "RMSE")
    lines(order,Test_RMSE, col='red', type='l')
    lines(order,Train_RMSE, col='green', type='l')
    
    legend("topleft", legend = c("Test", "Train"), col = c("red","green"), pch = 19, cex = 0.75)
}


#=============================================================================================
# POLYNOMIAL REGRESSION OF ORDER 1,2,7,8,9,10 of size 100 for four different samples
#=============================================================================================
B=c(6,12,18,24)

for (i in B) {
  set.seed(5)
  s = sample(1:nrow(train),100)
  train3 = train[s, ]
  
  m1 <- lm(MPG ~ Weight, train3)
  m2 <- lm(MPG ~ Weight + I(Weight^2), train3)
  m7 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)+ I(Weight^5) 
           + I(Weight^6) + I(Weight^7), train3)
  m8 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)+ I(Weight^5) 
           + I(Weight^6) + I(Weight^7)+I(Weight^8), train3)
  m9 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)+ I(Weight^5) 
           + I(Weight^6) + I(Weight^7)+I(Weight^8)+I(Weight^9), train3)
  m10 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)+ I(Weight^5) 
            + I(Weight^6) + I(Weight^7)+I(Weight^8)+I(Weight^9)+I(Weight^10), train3)
  
  #PLOTTING THE MODEL OVER THE DATA
  plot(main='Regression Model on order 1,2,7,8,9,10',train3$Weight,train3$MPG, pch=19, cex=0.5,xlab = "Weight",ylab = "MPG")
  lines(sort(train3$Weight), fitted(m1)[order(train3$Weight)], col='red', type='l',pch=19)
  lines(sort(train3$Weight), fitted(m2)[order(train3$Weight)], col='blue', type='l',pch=19)
  lines(sort(train3$Weight), fitted(m7)[order(train3$Weight)], col='brown', type='l',pch=19)
  lines(sort(train3$Weight), fitted(m8)[order(train3$Weight)], col='green', type='l',pch=19)
  lines(sort(train3$Weight), fitted(m9)[order(train3$Weight)], col='yellow', type='l',pch=19)
  lines(sort(train3$Weight), fitted(m10)[order(train3$Weight)], col='skyblue', type='l',pch=19)
  
  Trn_RMSE1=sqrt(mean(sum(m1$residuals^2)))
  pred = predict(m1, newdata=test)
  Tst_RMSE1=sqrt(mean(sum((pred-test$MPG)^2)))
  Trn_RMSE2=sqrt(mean(sum(m2$residuals^2)))
  pred = predict(m2, newdata=test)
  Tst_RMSE2=sqrt(mean(sum((pred-test$MPG)^2)))
  Trn_RMSE7=sqrt(mean(sum(m7$residuals^2)))
  pred = predict(m7, newdata=test)
  Tst_RMSE7=sqrt(mean(sum((pred-test$MPG)^2)))
  Trn_RMSE8=sqrt(mean(sum(m8$residuals^2)))
  pred = predict(m8, newdata=test)
  Tst_RMSE8=sqrt(mean(sum((pred-test$MPG)^2)))
  Trn_RMSE9=sqrt(mean(sum(m9$residuals^2)))
  pred = predict(m9, newdata=test)
  Tst_RMSE9=sqrt(mean(sum((pred-test$MPG)^2)))
  Trn_RMSE10=sqrt(mean(sum(m10$residuals^2)))
  pred = predict(m10, newdata=test)
  Tst_RMSE10=sqrt(mean(sum((pred-test$MPG)^2)))
  
  Test_RMSE=c(Tst_RMSE1,Tst_RMSE2,Tst_RMSE7,Tst_RMSE8,Tst_RMSE9,Tst_RMSE10)
  Train_RMSE=c(Trn_RMSE1,Trn_RMSE2,Trn_RMSE7,Trn_RMSE8,Trn_RMSE9,Trn_RMSE10)
  order=c(1,2,7,8,9,10)
  plot(main=" RMSE VS MODEL COMPLEXITY(n=100)",range(1:10), range(11:60), xlab = "Order",  ylab = "RMSE")
  lines(order,Test_RMSE, col='red', type='l')
  lines(order,Train_RMSE, col='green', type='l')
  
  legend("topleft", legend = c("Test", "Train"), col = c("red","green"), pch = 19, cex = 0.75)
}

##To Export the plots##

#jpeg(filename="D:\\Graph\\Order7.png")
#dev.off()



