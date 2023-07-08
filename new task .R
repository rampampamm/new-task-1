xy=read.csv('/Users/inchakovslava9/Desktop/Данные лаб2.txt', dec = ',' , sep = ";")
xy
x=xy [ , 1]
x
y=xy [ , 2]
y

## Линейная модель----

xy1=data.frame(cbind(x, y))
xy1
plot(y ~ x, data = xy1)

reg1=lm(y ~ x, xy1)
summary(reg1)
abline(reg1, col='red')

y1=reg1$fitted.values
y1

sum1=summary(reg1)
R1=sum1$r.squared
R1
A1=(1/20)*sum(abs((y-y1)/y))*100
A1

int=predict(reg1,interval='conf',level=0.95)
matlines(xy1$x,cbind(int),lty=c(1,2,2),col=c(2,4,4))

intpred=predict(reg1,interval='pred',level=0.95)
matlines(xy1$x,cbind(intpred),lty=c(1,2,2),col=c(2,3,3))

## Степенная модель----

lx=log(x)
ly=log(y)

plot(lx,ly)

xy2=data.frame(cbind(lx,ly))
xy2

reg2=lm(ly~lx,data=xy2)
summary(reg2)
exp(reg2$coefficients[1])
abline(reg2,col="red") 

y2=reg2$fitted.values
y2

sum2=summary(reg2)
R2=sum2$r.squared
R2
A2=(1/20)*sum(abs((y-y2)/y))*100
A2

int=predict(reg2,interval='conf',level=0.95)
matlines(xy2$lx,cbind(int),lty=c(1,2,2),col=c(2,4,4))

intpred=predict(reg2,interval='pred',level=0.95)
matlines(xy2$lx,cbind(intpred),lty=c(1,2,2),col=c(2,3,3))

## Экспоненциальная модель----

ly=log(y)

plot(x, ly)

xy3=data.frame(cbind(x,ly))
xy3

reg3=lm(ly~x,data=xy3)
summary(reg3)
exp(reg3$coefficients[1])
abline(reg3,col="red")

y3=reg3$fitted.values
y3

sum3=summary(reg3)
R3=sum3$r.squared
R3
A3=(1/20)*sum(abs((y-y3)/y))*100
A3

int=predict(reg3,interval='conf',level=0.95)
matlines(xy3$x,cbind(int),lty=c(1,2,2),col=c(2,4,4))

intpred=predict(reg3,interval='pred',level=0.95)
matlines(xy3$x,cbind(intpred),lty=c(1,2,2),col=c(2,3,3))

## Логарифмическая модель----

lx=log(x)

plot(lx,y)

xy4=data.frame(cbind(lx,y))
xy4

reg4.1=lm(y~lx,data=xy4)
summary(reg4.1)
reg4.2=lm(y~lx+0,data=xy4)
summary(reg4.2)
abline(reg4.2,col="red")

y4.2=reg4.2$fitted.values
y4.2

R4=1-(sum((y-y4.2)^2)/sum((y-mean(y))^2))
R4
A4=(1/20)*sum(abs((y-y4.2)/y))*100
A4

int=predict(reg4.2,interval='conf',level=0.95)
matlines(xy4$lx,cbind(int),lty=c(1,2,2),col=c(2,4,4))

intpred=predict(reg4.2,interval='pred',level=0.95)
matlines(xy4$lx,cbind(intpred),lty=c(1,2,2),col=c(2,3,3))

## Параболическая модель----

x1=sqrt(x)

plot(x1,y)

xy5=data.frame(cbind(x1,y))
xy5

reg5=lm(y~x1,data=xy5)
summary(reg5)
abline(reg5,col="red") 

y5=reg5$fitted.values

sum5=summary(reg5)
R5=sum5$r.squared
R5
A5=(1/20)*sum(abs((y-y5)/y))*100
A5

int=predict(reg5,interval='conf',level=0.95)
matlines(xy5$x1,cbind(int),lty=c(1,2,2),col=c(2,4,4))

intpred=predict(reg5,interval='pred',level=0.95)
matlines(xy5$x1,cbind(intpred),lty=c(1,2,2),col=c(2,3,3))

## Гиперболическая модель----

x2=(1/x)

plot(x2,y)

xy6=data.frame(cbind(x2,y))
xy6

reg6=lm(y~x2,data=xy6)
summary(reg6)
abline(reg6,col="red")

y6=reg6$fitted.values
y6

sum6=summary(reg6)
R6=sum6$r.squared
R6
A6=(1/20)*sum(abs((y-y6)/y))*100
A6

int=predict(reg6,interval='conf',level=0.95)
matlines(xy6$x2,cbind(int),lty=c(1,2,2),col=c(2,4,4))

intpred=predict(reg6,interval='pred',level=0.95)
matlines(xy6$x2,cbind(intpred),lty=c(1,2,2),col=c(2,3,3))

## Выбор лучшей модели----

R1; A1
R2; A2
R3; A3
R4; A4
R5; A5
R6; A6

#Лучшей моделью оказалась гиперболическая (мин А, макс R)

## Прогноз----

x_prognoz=44
X_prognoz=c(1, x_prognoz)
y_prognoz=reg1$coefficients%*%X_prognoz
y_prognoz
