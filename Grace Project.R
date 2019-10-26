Project<- read.csv("C:/Users/Isaac/Desktop/Project.csv")
attach(Project)
##########
### AGe
#########
a <- ggplot(Project., aes(Age))
a + geom_bar(fill='red') +ylim(0,20)
table(Age)
########
###Education
#######
plot(Education,ylab='Count',xlab='Education',ylim=c(0,25),col='red')
b <- ggplot(Project., aes(Education))
b + geom_bar(fill='red')+ylim(0,25)
table(Education)
#######
###Age and Education
######
ss=table(Age,Education)
ss
plot(ss)

######
###Occupation
######
plot(Occupation,ylim=c(0,30),ylab='Count',xlab='Employment Status',col='red')
table(Occupation)
c <- ggplot(Project.,aes(Occupation))
c+ geom_bar(fill = 'red')+ylim(0,30)
r + coord_polar( direction=1 )
######
###Relation of Age and Education
#####
ss=table(Age,Education)
ss

######
###Gender
######
table(Gender)
x<-c(36,14)
labels<-c('Female','Male')
piepercent<-round(100*x/sum(x),2)
pielabels<-paste(piepercent,'%',sep='')
pie(x,labels = pielabels,col=c('red','skyblue'))
legend('topright',c('Female','Male'),cex = 1,fill=c('red','skyblue'))
plot(Gender,col='red')
d <- ggplot(Project., aes(Gender))
d + geom_bar(fill='red') +ylim(0,40)
geom_bar(aes(fill = fl)) 
######
###Age and Gender
######
hh=table(Age,Gender)
hh

######
###Stress to patient
#####
d <- ggplot(Project., aes(Stress_act))
d + geom_bar(fill='red') +ylim(0,20)
table(Stress_act)

######
###Work as Stressor to patient
#####
table(Intense_Work)

######
###Vigorous sports/activity as Stressor to patient
#####
table(Vigorous_sports)

par(mfrow=c(1,3))
######
###Exercise
######
table(Exercise_reg)
x<-c(24,26)
labels<-c('No','Yes')
piepercent<-round(100*x/sum(x),2)
pielabels<-paste(piepercent,'%',sep='')
pie(x,labels = pielabels,col=c('red','skyblue'),main = 'Exercise')
legend('bottom',c('No','Yes'),cex = 1,fill=c('red','skyblue'))
d <- ggplot(Project., aes(Exercise_reg))
d + geom_bar(fill='red') +ylim(0,30)+ xlab("Regular Exercise")
######
###Alcohol use
#####
table(Alcohol_Status)
x<-c(40,10)
labels<-c('No','Yes')
piepercent<-round(100*x/sum(x),2)
pielabels<-paste(piepercent,'%',sep='')
pie(x,labels = pielabels,col=c('red','skyblue'),main = 'Alcohol Intake')
legend('topright',c('No','Yes'),cex = 1,fill=c('red','skyblue'))
d <- ggplot(Project., aes(Alcohol_Status))
d + geom_bar(fill='red') + xlab("Alcohol Intake")
########
###Smoking
######
table(Smoke_Status)
x<-c(49,1)
labels<-c('No','Yes')
piepercent<-round(100*x/sum(x),2)
pielabels<-paste(piepercent,'%',sep='')
pie(x,labels = pielabels,col=c('red','skyblue'),main = 'Smoking Status')
legend('bottom',c('No','Yes'),lty =c(1,1),lwd = c(2.5,2.5),col=c('red','skyblue'))
d <- ggplot(Project., aes(Smoke_Status))
d + geom_bar(fill='red') + xlab("Smoking Status")
##########
###Dietary management (Salt,Potassium,Vitamin D)
#########
table(Salt_intake)
table(LessPota_intake)
table(LessVitD_intake)

########
###Dietary management (Fruit,Vegetables)
######
s <- ggplot(Project, aes(Fruit_Intake, fill = Vegetable_Intake))
s + geom_bar(position = "dodge") + xlab("Fruit Intake")+ scale_fill_discrete(name = "Vegetable Intake") +scale_y_continuous(breaks = c(1,2,3,4,6,9,11,12)) 
table(Fruit_intake)
table(Vegetable_intake)
ss=table(Fruit_Intake,Vegetable_Intake)
ss
##########
###Advice by doctors on BP and BC
#########
table(DocAd_BP)
table(DocAd_BC)
ss=table(DocAd_BP,DocAd_BC)
ss
d <- ggplot(Project, aes(DocAd_BP, fill = DocAd_BC))
d + geom_bar(position = "dodge") + xlab("Blood Pressure")+ scale_fill_discrete(name = "Blood Cholesterol")+scale_y_continuous(breaks = c(7,11,13,19)) 
d+scale_y_continuous(breaks = c(7,11,13))

##########
###Respondents diagnosed of Diabetes,Heart disease,Kidney disease
#########
table(Chronic_cond)
a <- ggplot(Project, aes(Chronic_cond))

a + geom_bar(aes(fill = Chronic_cond))+theme(axis.ticks = element_blank(),axis.text.x = element_blank()) + xlab("Chronic Condition")+ scale_fill_discrete(name = "Chronic Condition")+scale_y_continuous(breaks = c(2,3,7,36))

a+scale_y_continuous(breaks = c(2,3,7,36))

#########
###Patients hypertension and family history of hypertension
########
ss=table(Family_hist,Docdiag_Hyp)
ss=table(Family_hist,Docdiag_Hyp)
ss
table(Docdiag_Hyp)
f<- ggplot(Project, aes(Family_hist,fill = Docdiag_Hyp))
f+ geom_bar(position = "dodge")+xlab("Family History of Hypertension")+ scale_fill_discrete(name = "Patient diagnosed of Hypertension")+scale_y_continuous(breaks = c(1,3,5,11,19))

###########
###Blood pressure reading and Cholesterol intake
#########
ss=table(BP_readn,Cholest_intake)
ss

###########
###Doctor diagnosed of Hypertension and Diabetes
###########
table(Docdiag_Hyp)
table(Docdiag_Diab)

##########
###Relationship between Hypertension and Age
#######
ss=table(Age,Docdiag_Hyp)
ss

##########
###Relationship between Hypertension and Sex
#######
ss=table(Gender,Docdiag_Hyp)
ss

##########
###Relationship between Hypertension and exercise
#######
ss=table(Exercise_reg,Docdiag_Hyp)
ss

#########
###Relationship between Hypertension and Alcohol use
#######
ss=table(Alcohol_Status,Docdiag_Hyp)
ss

########
###Relationship between Hypertension and smoking
#######
ss=table(Smoke_Status,Docdiag_Hyp)
ss

########
###Relationship between Hypertension and Cholestrol intake
#######
ss=table(Cholest_intake,Docdiag_Hyp)
ss

########
###Relationship between Hypertension and Salt intake
#######
ss=table(Salt_intake,Docdiag_Hyp)
ss


########
###Relationship between Hypertension and Potassium intake
#######
ss=table(LessPota_intake,Docdiag_Hyp)
ss


########
###Relationship between Hypertension and Vitamin D intake
#######
ss=table(LessVitD_intake,Docdiag_Hyp)
ss

########
###Relationship between Hypertension and Chronic disease
#######
ss=table(Chronic_cond,Docdiag_Hyp)
ss

#######
###Relationship between hypertension and stress
######
ss=table(Stress_act,Docdiag_Hyp)
ss

###########
###Code use to recode days of fruit intake
#########
Project$Fruit_Intake[Project$Fruit_Intake=='1-4 days']='4-7 days'



