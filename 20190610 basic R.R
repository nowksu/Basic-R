rm(list=ls())
install.packages('dplyr')
library('dplyr')
data.raw <- read.csv("C:/Users/nowks/Downloads/titanic (1).csv")
head(data.raw)
data.raw %>% head()
sapply(data.raw,class) #변수에 대한 객체를 봄  
#survived랑 pclass는 integer를 factor로 바꿔줘야한다. 숫자에 의미가 있음
data.raw$Survived <- factor(data.raw$Survived)
#data.raw$Survived <- data.raw$Survived %>% factor()
data.raw$Pclass <- data.raw$Pclass %>% factor()

data.use <- data.raw %>% select(Survived,Pclass,Sex:Embarked) #필요한 컬럼만 가지고옴
data.use %>% head()
data.use %>%str()#structure을 보고싶을 때
data.use %>% summary() #Age같은 연속형 변수는 iqr나옴

boxplot(data.use$Age) #연속형 변수는 박스플랏으로 보는게 제일 편리
boxplot(data.use$Fare)

tb <- table(data.use$Sex) #빈도수 세줌
barplot(tb)

###############################6월3일 ###############################
#cheking the structure
data.use %>% str()
summary(data.use)
sapply(data.use,class)
###boxplot
bp<-boxplot(data.use$Age)
hist(data.use$Age)
boxplot(Age ~ Sex, data=data.use) #data.use에 있는 Age(numeric)라는 변수로 그리는데 sex(factor)로 나눠서
boxplot(Age ~ Survived, data=data.use)
tb <- table(data.use$Survived)
barplot(tb) #막대 그래프

#cross table 교차 빈도 표 그려줌
tb <- table(data.use$Sex,data.use$Survived)
barplot(tb,legend =T, beside =T) #legend는 범례를 그려줌 #beside는 누적 분리 


##등석에 따라 생존율 파악 
tb <- table(data.use$Pclass ,data.use$Survived)
barplot(tb,legend =T, beside =T)
#동승자 수에 따라 생존율 파악
tb <- table(data.use$SibSp ,data.use$Survived)
barplot(tb,legend =T, beside =T)
tb <- table(data.use$Survived ,data.use$Parch)
barplot(tb,legend =T, beside =T)
#pulling = 재범주화 (카운터가 너무 작을경우 오차가 커져서 합쳐줌)
new <- data.use$Parch
Parch.cat <- ifelse(new>0,1,0) #0보다 크면 전부 1, 작으면 0 
new %>% head(30)
Parch.cat %>% head(30)
Parch.cat <- ifelse(new>=2,2,new) #이렇게하면 2보다 큰것은 2 나머지는 기존 그대로(0,1)

########100%로 비율을 선정하여 생존자 수치를 비교하기 위해 
tb <- table(data.use$Pclass ,data.use$Survived)
barplot(tb)

tb.sum <- apply(tb,2,sum)
tb.ratio <- t(t(tb)/tb.sum) #t는 transpose 시킨것 . (행과 열을 바꾼것) 
#(위랑 동일함) 간편하게 만드는 방식 
c1 <- tb[,1] / tb.sum[1]
c2 <- tb[,2] / tb.sum[2]

c3 <- c(c1,c2)
matrix(c3,3,2)
#####
barplot(tb.ratio)
#다음시간에는 나이로 여섯가지 카테고리를 만듦. 그 카테고리별로 생존률에 차이가 있는지 볼것 


##################################6월 10일 ###############################
#범주를 만듦 0~5 아기 5~12 아이 12~18 청소년 18~25 student 25~35 Young 35~60 Adult 60~100 Senior
#개수 - 1의 범주가 생ㄱ
#구글에서도 다 if문과 ifelse를 주지 이번에 한것은 없음
#cut함수 쓰면 됨
x <- data.use$Age
boxplot(x)
x11()
hist(x)
##Age categories 
cut.points <- c(0,5,12,18,25,35,60,100)
cut.labels <- c("Babies", "Children", "Teen", "Student", "Young",
                "Adult","Senior")
cut(data.use$Age,cut.points,cut.labels)
#새로운 변수 추가할 때는 그냥 기존 데이터에 $ 쓰고 붙여주면됨
data.use$Age.cat <- cut(data.use$Age,cut.points,cut.labels)
head(data.use)
summary(data.use$Age.cat)
tb <- table(data.use$Age.cat) #barplot은 요약된 값 table로 그려줘야한다.
tb <- table(data.use$Sex, data.use$Age.cat)
barplot(tb, beside= T) #beside로 남녀가 같이 나오게함
#연령대별로 어떻게 생존이 되있는지 
tb <- table(data.use$Survived, data.use$Age.cat)
barplot(tb,beside=T,legend.text = c("X","O"))
tb #0이 죽은사람 1이 생존한 사람인거 확인
#tb는 matrix형태이기 때문에 0,1만 rownames로 가지고와서 이런식으로 해도됨  
rownames(tb)
tb <- table(data.use$Survived, data.use$Age.cat)
barplot(tb,beside=T,legend.text = rownames(tb),
        col=c("red","blue"))
#남여로 구별해서 각각 연령대별의 생존률을 구하고싶을때 
summary(data.use)
#data.use.f <- subset(data.use,Sex=="female")
#data.use.f <- data.use %>% subset(Sex=="female") #위랑 같은것 이것을 쓰기를 추천함
data.use.f <- data.use %>% filter(Sex=="female") # 셋다 같은것. 액셀 filter랑 같은 역할. 
dim(data.use.f)
#여성의 전체 생존률 구함
data.use.f$Survived #level은 팩터인데 컴퓨터에서는 문자 형태지만 보여지기만 int형태를 가지고 있음. 때문에 숫자로 바꿔줘야한다 
as.numeric(data.use.f$Survived)#숫자로 바꿔라// 두번째 level이라 2 첫번째 levelt은 1 
#다시 int형태0,1로 바꿔주는법 이분법은 보통 0,1로 함
as.numeric(data.use.f$Survived)-1
Survived.f.int <- as.numeric(data.use.f$Survived)-1
mean(Survived.f.int)

#각각 범주별로 뽑아줌 
tf <- data.use.f$Age.cat=="Babies"
#True 값만 int형태로 뽑아옴
tf.val <- Survived.f.int[tf]
#babies의 평균
mean(tf.val,na.rm=T) #결측치가 있으면 지워라 / baby의 생존률은 76%
#tf[is.na(tf)]<-F 라고 하면 false만 추출. 

#children의 평균 
tf <- data.use.f$Age.cat=="Children"
#True 값만 int형태로 뽑아옴
tf.val <- Survived.f.int[tf]
mean(tf.val,na.rm=T) #children의 생존률은 27%

#청소년의 평균  
tf <- data.use.f$Age.cat=="Teen"
#True 값만 int형태로 뽑아옴
tf.val <- Survived.f.int[tf]
mean(tf.val,na.rm=T) 
#Babies만 생존률로 바꿈 다바꿀라면 일일이 해서 붙여주면 됨. 이작업을 아래 tapply함수로 한번에 해결 
tb.Age.cat <- table(data.use.f$Age.cat)
tb.Age.cat["Babies"] <- mean(tf.val, na.rm=T)
tb.Age.cat

#tapply() 적용. 카테고리 별로 평균을 구하고싶을때 tapply(값, 값의 그룹(카테고리) , 카테고리에 따른 함수이름, 이후 추가값은 mean에 들어감)
female <- tapply(Survived.f.int, data.use.f$Age.cat, mean, na.rm=T) #여자 일때
female
#남자일때
data.use.m <- data.use %>% filter(Sex=="male")
Survived.m.int <- as.numeric(data.use.m$Survived)-1
male <- tapply(Survived.m.int, data.use.m$Age.cat, mean, na.rm=T)
male
#이것들을 이제 barplot을 그릴건데 crosstable형태로 만들어 줘야한다. 
#행을 붙이기 때문에 rbind
Survival.ratio <- rbind(female,male)
barplot(Survival.ratio,beside = T,legend.text = c("female","male"))


#barplot 그릴때 rainbow(8) 이런식으로 하면 8개의 무지개 색깔을 만들어줌 
