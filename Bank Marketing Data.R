# EDA for Real Data Sets -->> Bank Marketing Data Set

library(tidyverse)
library(ggplot2)
library(cluster)
library(knitr)

setwd("C:/Users/student/Downloads")
getwd()

#sep : 공백, header : 제목
bank <- read.csv("bank.csv", sep=";")
bank <- read.csv("bank.csv", sep=";",header = T)
bank

#기본적으로 데이터 정보 파악
names(bank) # bank데이터셋 변수 뭐가 있는지 살펴보기기
str(bank)
head(bank,10)
summary(bank)
dim(bank) # 행, 열 개수

#col=rainbow : 색을 자동으로 넣어주는 옵션
plot(balance~age, data=bank, col=rainbow(10)) #x축 age, y축 balance(은행의 잔고)
plot(bank$age, bank$balance, col=rainbow(10))

#cor(상관계수) : 두개의 변수가 필요. (-1(음의 상관관계)에서 1사이의 값을 가짐)
cor(bank$age, bank$balance) # 0.08382014 (음의 상관관계다, 나이가 많아질수록 잔고가 적어짐)


# 고객 기본 정보
# 나이 (수치형)

table(bank$age)
table(bank$job)
table(bank$marital)
table(bank$education)
table(bank$default)
table(bank$housing)

summary(bank$age)
age.1 <- bank$age
age.1

result <- hist(bank$age, border='blue', col=rainbow(15), breaks=10)
result
freq <- result$counts
names(freq) <- result$breaks[-1] 
freq

boxplot(bank$age)
ggplot(data=bank, aes(x="", y=age)) +   geom_boxplot()


# 나이 (범주형)

bank$age_c = cut(bank$age, c(0, 30, 40, 50, 60, 100))
table(bank$age_c)
ggplot(data=bank, aes(age_c)) + geom_bar() 


# 직업

table(bank$job)
par(mar=c(8,3,1,1))
barplot(table(bank$job), col=rainbow(12), las=2)
barplot(table(bank$job), col=rainbow(12), las=2, horiz=TRUE)

# hist( ) 함수의 매개변수

# border='blue' : 막대의 테두리 색을 지정한다.
# breaks=5 : 데이터 내 구간을 몇 개로 나눌지를 지정하며, 값이 커질수록 막대의 개수도 늘어난다.

# las값에 따른 출력 방향
# 0 : 축 방향(기본값)
# 1 : 수평 방향(축 방향과 상관없음)
# 2 : 축을 기준으로 수직 방향
# 3 : 수직 방향(축 방향과 상관없음)

# par( ) 함수에 대한 매개변수
# mfrow=c(1, 1) : c(1,1)은 창을 분할하지 않음 의미.
# mar=c(5, 5, 5, 7) : 그래프 출력 창과 그래프 출력영역 밖의 여유 공간을 지정. 
# c(bottom, left, top, right)의 순서로 숫자를 지정.

ggplot(data=bank, aes(job)) + geom_bar() + 
  coord_flip() #flip cordinates

# dt <- as.data.frame(table(bank$job))
# ggplot(data=dt, aes(x=Var1, y=Freq)) +
#     geom_bar(stat="identity") + coord_flip() #flip cordinates

# 결혼상태

p <- table(bank$marital)
addmargins(p)

p2 <- xtabs(~job+marital, data=bank)
p2

kable(addmargins(prop.table(p2, 1)))
kable(addmargins(prop.table(p2, 1)*100, 2), digits = 2)
kable(addmargins(proportions(p2, 1)*100, 2), digits = 2)

# addmargins() 함수는 데이터 테이블 내에서 열, 행, 행&열의 합산 값을 구해주는 함수이다.
# addmargins(데이터명 , margin = 숫자)  <숫자: 1 = 열 단위 합산, 2 = 행 단위 합산>

ggplot(data=bank, aes(job)) +
  geom_bar(aes(fill=marital), width=0.7, position = "dodge") + coord_flip() #flip cordinates

# balance: 연평균잔고, in euros (numeric)

summary(bank$balance)
ggplot(bank, aes(age, balance)) + geom_point()
ggplot(bank, aes(education, balance)) + geom_boxplot()

ss <- aggregate(balance~education+marital, bank, mean)
ggplot(data=ss, aes(fill=education, x=marital, y=balance)) +
  geom_bar(stat="identity", width=0.7, position = "dodge") 

# 정기예금 가입여부와의 관계
# 나이 (범주화)

tt = table(bank$age_c, bank$y); tt
ss <- prop.table(table(bank$age_c, bank$y), 1)
plot(ss, main="연령대별 정기예금 가입률")
