data1 = c(13.3, 12,11.4,8.9,6.3,8.2,8.6,6.9,7.1,7.2,9.9,14.8,10.1,13.1,16.9,21.1,25.9,21.1,16.6,16,15.5)
data2 = c(1.4,1.7,2,2,1.8,1.9,1.6,1.5,1.5,1.7,1.6,1.9,2.1,2.3,2.7,2.9,2.9,2.6,2.5,2.4,2.2)


diff = c()

X <- matrix(cbind(rep(1,21),data2), ncol =2)
beta <- solve(t(X)%*%X)%*%t(X)%*%data1

ans = (9.69*data2) - 7.03
ans
eResiduals = data1 - ans
rVar = sum(eResiduals^2)/19

bwtlm <- lm(log(FEV) ~ Sex+Age+Sex*Age, data = fev)
summary(bwtlm)
View(fev)
qt(0.975,df = 650)
ggplot(data = fev, mappin = aes(x = Height, y = FEV))+geom_point()+geom_smooth(method = lm)

mean1 = mean(data1)
mean2 = mean(data2)
meanDiff = mean1-mean2

var1 = var(data1)
sd1 = sd(data1)
var2 = var(data2)

ans = data1-data2
ans
d = mean1-mean2
ans_mean = mean(ans)
ans_var = var(ans)

t = ans_mean/(sqrt(ans_var)/((10)^(1/2)))
pValue = 2*(1-pt(-t,df=9))
print(ans)
  

pooled  = (((length(data1)- 1)*var1) +((length(data2)-1)*var2))/(length(data1)+length(data2)-2)
sqrt(pooled)

t = (mean1-mean2) / (sqrt(pooled)*sqrt(2/10))
pt(t, df = 18)


B = 50000
n = length(data1)
meanBS = rep(0,B)
for(i in 1:B){
  dataB = sample(data1,n,replace = T)
  meanBS[i] = mean(dataB)

}
confInt <- quantile(meanBS, c(0.05, 0.95))

t = qt(0.05, 9)
ans = mean1 - (t*sd1)/((10)^1/2)
temp = 1-(2*(0.90))
temp

0.8/(16.8*sqrt(10))

qf(0.95,4,35)


