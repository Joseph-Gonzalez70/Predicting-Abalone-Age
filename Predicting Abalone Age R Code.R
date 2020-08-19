#Analysis of Abalone data set
#Goal: Develop a predictive model for abalone age
abaloneData=read.table("abalone.txt",sep=",",header=FALSE) #Read in the data
colnames(abaloneData)=c("Sex","Length","Diameter","Height","Whole.weight", "Shucked.weight","Viscera.weight","Shell.weight", "Rings") #Name the columns in the data

#Check for Missing values:
sapply(abaloneData,class)
any(is.na(abaloneData)) ##doesn’t appear to be any NA in data 
any(abaloneData=="?")
any(abaloneData=="")
any(abaloneData==0)
which(abaloneData$Height==0)
which(abaloneData$Rings==0)
#There are a few entries that come up as zero, will confirm which ones
#I will assume that the measurements were too small to calculate and keep these zeros in 
#The zeros may have to do with the abalone being categorized as infants. 
#In the real world, I seek clarification for the zeros
#No NAs or other conflicting values, so I will continue with the investigation.

#Next,I will look at some summary stats for the variables: 
sapply(abaloneData, summary)
#Next, I will look at the histograms for the variables:
#First is the rings variable(the response variable)
hist(abaloneData$Rings, xlab="Number of Rings",
     main="Histogram of Number of Abalone Rings")
#The distribution of the response variables is a bit right skewed.

#we can test some transformations for this variable to see if we can adjust the skewness:
logrings=log(abaloneData$Rings)
sqrtrings=sqrt(abaloneData$Rings)
fracrings=1/abaloneData$Rings
par(mfrow = c(2, 2))
hist(logrings, xlab="Number of Rings",
     main="Histogram of Number of log(Rings)")
#log looks normal, sqrt in sec, original in third
hist(sqrtrings, xlab="Number of Rings",
     main="Histogram of Number of sqrt(Rings)")
hist(fracrings, xlab="Number of Rings",
     main="Histogram of Number of 1/Rings")

#Boxplot comparison:
par(mfrow = c(2, 2))
boxplot(abaloneData$Rings, main="Boxplot of Rings",
        xlab="Rings", horizontal=TRUE)
boxplot(sqrtrings, main="Boxplot of sqrt(Rings)",
        xlab="sqrt(Rings)", horizontal= TRUE)
boxplot(logrings, main="Boxplot of sqrt(Rings)",
        xlab="sqrt(Rings)", horizontal= TRUE)

#Histograms for the quantitative variables:
par(mfrow = c(3, 3))
index=c(2:8)
for(i in index) {hist(abaloneData[, i],
                      main=paste("Histogram of", names(abaloneData)[i]),xlab=names(abaloneData[i]))}

#box plot for quantitative variables:
par(mfrow = c(3, 3))
index=c(2:8)
for(i in index) {boxplot(abaloneData[, i], main=paste("Boxplot of",names(abaloneData)[i]),xlab=names(abaloneData[i]), horizontal = TRUE)}

#Bar graph and pie chart for gender:
par(mfrow = c(1, 2))
table(abaloneData$Sex)
barplot(table(abaloneData$Sex),col=rainbow(3),main='Barplot of Sex', ylim=c(0,1700))
n=4177

#pie chart for sex variable:
lbls=c('Female','Infant','Male')
pct=round(100*table(abaloneData$Sex)/n)
lab=paste(lbls,pct)
lab=paste(lab,'%',sep='')
pie(table(abaloneData$Sex),
    labels=lab,col=c('blue','purple','green'),main='Pie Chart of Sex')

#box plot for variables with respect to sex:
par(mfrow = c(3, 3))
index=c(2:9)
for(i in index) {boxplot(abaloneData[, i]~abaloneData$Sex, main= paste("Boxplot of", names(abaloneData)[i],"Vs Sex"), xlab=names(abaloneData)[i], ylab="Sex",col=rainbow(3),horizontal = TRUE)}

#Test to confirm transformation:
library(MASS)
boxcox(abaloneData$Rings~., data=abaloneData)

#transform rings
abaloneData$Rings=log(abaloneData$Rings)

#Scatter plot and correlation matrix for quantitative variables 
quandata=data.frame(cbind(abaloneData$Rings, abaloneData$Length,abaloneData$Diameter,abaloneData$Height, abaloneData$Whole.weight,
abaloneData$Shucked.weight, abaloneData$Viscera.weight, abaloneData$Shell.weight)) 
colnames(quandata)=c("Rings","Length", "Diameter","Height","Whole.weight", "Shucked.weight","Viscera.weight","Shell.weight")
pairs(quandata)
cor(quandata)

#finding VIF 
lenglm=lm(abaloneData$Length~abaloneData$Diameter+ abaloneData$Height+abaloneData$Whole.weight+ 
            abaloneData$Shucked.weight+ abaloneData$Viscera.weight+ abaloneData$Shell.weight, data= abaloneData)

diameterlm=lm(abaloneData$Diameter~abaloneData$Length+ abaloneData$Height+abaloneData$Whole.weight+
                abaloneData$Shucked.weight+ abaloneData$Viscera.weight+abaloneData$Shell.weight, data= abaloneData)

Heightlm=lm(abaloneData$Height~abaloneData$Length+a baloneData$Diameter+abaloneData$Whole.weight+ 
              abaloneData$Shucked.weight+abaloneData$Viscera.weight+ abaloneData$Shell.weight, data= abaloneData)

wholeweightlm=lm(abaloneData$Whole.weight~abaloneData$Length+ abaloneData$Diameter+abaloneData$Height+
                   abaloneData$Shucked.weight+ abaloneData$Viscera.weight+abaloneData$Shell.weight, data= abaloneData)

shuckedlm=lm(abaloneData$Shucked.weight~abaloneData$Length+ abaloneData$Diameter+abaloneData$Height+ 
               abaloneData$Whole.weight+abaloneData$Viscera.weight+abaloneData$Shell.weight, data= abaloneData)

Visceralm=lm(abaloneData$Viscera.weight~abaloneData$Length+ abaloneData$Diameter+abaloneData$Height+ 
               abaloneData$Whole.weight+abaloneData$Shucked.weight+ abaloneData$Shell.weight, data= abaloneData)

shellweightlm=lm(abaloneData$Shell.weight~abaloneData$Length+ abaloneData$Diameter+abaloneData$Height+ 
                   abaloneData$Whole.weight+abaloneData$Shucked.weight+ abaloneData$Viscera.weight, data= abaloneData)

VIFmatrix=rbind(1/(1-summary(lenglm)$r.squared), 1/(1-summary(diameterlm)$r.squared),
                1/(1-summary(Heightlm)$r.squared), 1/(1-summary(wholeweightlm)$r.squared),
                1/(1-summary(shuckedlm)$r.squared), 1/(1-summary(Visceralm)$r.squared), 1/(1-summary(shellweightlm)$r.squared))

colnames(VIFmatrix)= "VIF" 
rownames(VIFmatrix)= c("Length", "Diameter", "Height", "Whole.weight", "Shucked.Weight", "Viscera.weight", "Shell.weight")
VIFmatrix

#Boxplot for rings vs. gender:
boxplot(abaloneData$Rings~abaloneData$Sex, main='Rings and Sex',xlab='Sex', ylab='log(Rings)',col=rainbow(3))

#Training and validation data sets:
set.seed(10)
n.s=nrow(abaloneData)
index.s=sample(1: n.s, size=2089, replace=FALSE) 
data.c=abaloneData[index.s,] #training set 
data.v=abaloneData[-index.s,] #validation set

#test to see that the training and validation sets look alike for the quantitative data 
par(mfrow=c(3,3))
boxplot(data.c$Rings,data.v$Rings, main='log(Rings)',names=c('data.c','data.v'))
boxplot(data.c$Length,data.v$Length, main='Length',names=c('data.c','data.v'))
boxplot(data.c$Diameter,data.v$Diameter, main='Diameter',names=c('data.c','data.v'))
boxplot(data.c$Height,data.v$Height, main='Height',names=c('data.c','data.v'))
boxplot(data.c$Whole.weight,data.v$Whole.weight, main='Whole Weight',names=c('data.c','data.v'))
boxplot(data.c$Shucked.weight,data.v$Shucked.weight, main='shucked weight',names=c('data.c','data.v'))
boxplot(data.c$Viscera.weight,data.v$Viscera.weight, main='Viscera Weight',names=c('data.c','data.v'))
boxplot(data.c$Shell.weight,data.v$Shell.weight, main='Shell Weight',names=c('data.c','data.v'))
##summary of categorical variables from each set: 
summary(data.c$Sex) #F=673 I=657 M=759 
summary(data.v$Sex) #F=634 I=685 M=769

#----------------------------------------------------------------------------------------------------------------------------------------
#Model with all first order coefficients on training data:
basemodelfit=lm(data.c$Rings~., data= data.c) #model has all first order variables 
summary(basemodelfit)
par(mfrow=c(2,2))
plot(basemodelfit, which=1)
plot(basemodelfit, which=2)
plot(basemodelfit$fitted.values,data.c$Rings,
     main="Observed vs. Fitted", xlab="Fitted", ylab="Observed")
abline(-1.214e-14, 1)
boxcox(data.c$Rings~.,data= data.c)

PRESS_full = sum((basemodelfit$residuals/ (1-influence(basemodelfit)$hat))^2) 
PRESS_full

#------------------------------------------------------------------------------
#Best subset approach
library(leaps)
sub_set=regsubsets(data.c$Rings~.,data=data.c, nbest=1,nvmax=11,method="exhaustive")
sum_sub=summary(sub_set)
n=nrow(data.c)
p.m=as.integer(as.numeric(rownames(sum_sub$which))+1)
sse=sum_sub$rss
aic=n*log(sse/n)+2*p.m
bic=n*log(sse/n)+log(n)*p.m
res_sub=cbind(sum_sub$which,sse,sum_sub$rsq,sum_sub$adjr2,sum_sub$cp,aic, bic) 
fit0=lm(data.c$Rings~1,data=data.c)
sse1=sum(fit0$residuals^2)
p=1
c1=sse1/0.001384-(n-2*p)
aic1=n*log(sse1/n)+2*p
bic1=n*log(sse1/n)+log(n)*p
none=c(1,rep(0,9),sse1,0,0,c1,bic1,aic1)
res_sub=rbind(none,res_sub)
colnames(res_sub)=c(colnames(sum_sub$which),"sse",
                    "R^2", "R^2_a", "Cp","aic", "bic")
round(res_sub, 4)
#------------------------------------------------------------------------------------- 
##Model Selection: By AIC
#I will mainly consider stepwise methods because it is not recommended to do forward selection/backwards elimination with high multicollinearity present
#Forward stepwise:
stepAIC(fit0, scope=list(upper=basemodelfit,lower=~1), direction="both", k=2)
fs1_model1=lm(formula = data.c$Rings ~ Shell.weight + Shucked.weight + Diameter + Sex + Height + Whole.weight + Viscera.weight + Length, data = data.c) 
summary(fs1_model1) #FULL MODEL WITH FIRST ORDER ONLY
#Forward selection:
stepAIC(fit0, scope=list(upper=basemodelfit, lower=~1), direction="both", k=2)
#ends up that forward selection selects the same first order model


#testing backwards selection method:
stepAIC(basemodelfit, scope=list(upper=basemodelfit, lower=~1), direction="backward", k=2)
#also agrees with full first order model.
#backward stepwise:
stepAIC(basemodelfit, scope=list(lower=~1),direction="both", k=2)
fs2_model2=lm(formula = data.c$Rings ~ Sex + Length + Diameter + Height + Whole.weight + Shucked.weight + Viscera.weight + Shell.weight, data = data.c) 
summary(fs2_model2) #SAME MODEL FOR BACKWARD STEPWISE

#-----------------------------------------------------------------
#stepwise with BIC test
stepAIC(fit0, scope=list(upper=basemodelfit,
                         lower=~1), direction="both", k=log(n))
fs2_model2=lm(formula = data.c$Rings ~ Shell.weight + Shucked.weight + Diameter + Sex + Height + Whole.weight +
                Viscera.weight, data = data.c)
summary(fs2_model2)
#The model does not have length

#BIC backwards stepwise:
stepAIC(basemodelfit, scope=list(lower=~1), direction="both", k=log(n))
#results in the same model without length 

#BIC forward selection:
stepAIC(fit0, scope=list(upper=basemodelfit, lower=~1), direction="forward", k=log(n)) 
#same model without length

#BIC backward selection
stepAIC(basemodelfit, scope=list(lower=~1), direction="backward", k=log(n)) 
#same model without length 

#Diagnostics of the two first order only models:
par(mfrow=c(2,2))
plot(basemodelfit, which=1)
plot(basemodelfit, which=2)
plot(fs2_model2, which=1)
plot(fs2_model2, which=2)
##plots look similar... no distinguishable difference
##-----------------------------------------------------------------------
#Testing the 2nd order interactions
#Forward stepwise: by AIC
#Full model with all interactions:
fitfullinters=lm(data.c$Rings~.^2, data=data.c) #FULL MODEL WITH INTERACTIONS 
length(fitfullinters$coefficients)
##45 interaction terms
##Forward stepwise:
stepAIC(fit0, scope=list(upper=fitfullinters, lower=~1), direction="both", k=2) 
fs3_model3inters= lm(formula = data.c$Rings ~ Shell.weight +  Shucked.weight + Diameter + Whole.weight + Sex +
                                               Viscera.weight + Height + Length +  Shucked.weight:Diameter + Shucked.weight:Sex + Shell.weight:Sex
                                              + Diameter:Height + Diameter:Length + Height:Length +Diameter:Sex + Shell.weight:Shucked.weight +
                                               Diameter:Whole.weight + Shucked.weight:Height, data = data.c)
summary(fs3_model3inters)
length(fs3_model3inters$coefficients)
stepAIC(fit0, scope=list(upper=fitfullinters, lower=~1), direction="forward", k=2) #use for reference

#From here on out I decided to focus on using BIC
#These predictive models are large and BIC will control the amount of variables
#Forward stepwise selection:
stepAIC(fit0, scope=list(upper=fitfullinters,
                         lower=~1), direction="both", k=log(n))
fs4_model4inters=lm(formula = data.c$Rings ~ Shell.weight + Shucked.weight + Diameter + Whole.weight + Sex +
                      Viscera.weight + Shell.weight:Diameter + Shucked.weight:Diameter + Shucked.weight:Whole.weight + 
                      Shucked.weight:Sex, data = data.c) 
summary(fs4_model4inters)
#Forward selection:
stepAIC(fit0, scope=list(upper=fitfullinters, lower=~1), direction="forward", k=log(n))
#Forward selection produces the same model as forward stepwise selection

#Backward stepwise selection:
stepAIC(fitfullinters, scope=list(lower=~1),
        direction="both", k=log(n))
fs5_model5inters=lm(formula = data.c$Rings ~ Sex + Length + Diameter + Height + Whole.weight +
                    Shucked.weight + Viscera.weight + Shell.weight + Sex:Shucked.weight +
                      Length:Diameter + Length:Height + Diameter:Height + Diameter:Shucked.weight +
                      Shucked.weight:Shell.weight, data = data.c) 
summary(fs5_model5inters)

#backward selection: A
stepAIC(fitfullinters, scope=list(lower=~1), direction="backward", k=log(n))
#Backward selection produces the same model as backward stepwise Selection



#Diagnostics for models with interaction terms:
par(mfrow=c(2,2))
plot(fitfullinters, which=1) 
plot(fitfullinters, which=2) 
plot(fs3_model3inters, which=1) 
plot(fs3_model3inters, which=2) 
plot(fs4_model4inters, which=1) 
plot(fs4_model4inters, which=2)
plot(fs5_model5inters, which=1) 
plot(fs5_model5inters, which=2)
#Nonlinearity appears to be resolved, Q-Q right tail heavy

#Test for polynomials:
#Subtract mean to reduce correlation between the variable and its quadratic:
center=function(x) x-mean(x)
Lengthcent=center(data.c$Length)
Diametercent=center(data.c$Diameter) 
heightcent=center(data.c$Height)
Whole.weightcent=center(data.c$Whole.weight) 
Shucked.weightcent=center(data.c$Shucked.weight) 
Viscera.weightcent=center(data.c$Viscera.weight) 
Shell.weightcent=center(data.c$Shell.weight) 
Lengthcent2=center(data.c$Length^2) 
Diametercent2=center(data.c$Diameter^2) 
heightcent2=center(data.c$Height^2) 
Whole.weightcent2=center(data.c$Whole.weight^2) 
Shucked.weightcent2=center(data.c$Shucked.weight^2)
Viscera.weightcent2=center(data.c$Viscera.weight^2) 
Shell.weightcent2=center(data.c$Shell.weight^2)
polyfitdata=data.frame(cbind(data.c$Rings,factor(data.c$Sex), Lengthcent,Diametercent,heightcent, Whole.weightcent,Shucked.weightcent,
Viscera.weightcent, Shell.weightcent,Lengthcent2, Diametercent2,heightcent2,Whole.weightcent2,Shucked.weightcent2,Viscera.weightcent2,Shell.weightcent2))


#Look to see if there are relationships between the polynomials and the residuals:
par(mfrow=c(2,4))
plot(Lengthcent2,basemodelfit$residuals, main="Residuals Vs Length", xlab="Length", ylab="Residuals") 
plot(Diametercent2,basemodelfit$residuals, main="Residuals Vs Diameter", xlab="Diameter", ylab="Residuals")
plot(heightcent2,basemodelfit$residuals, main="Residuals Vs Height", xlab="Height", ylab="Residuals") 
plot(Whole.weightcent2,basemodelfit$residuals,main="Residuals Vs Whole Weight", xlab="Whole Weight", ylab="Residuals")
plot(Shucked.weightcent2, basemodelfit$residuals,  main="Residuals Vs Shucked Weight", xlab="Shucked Weight", ylab="Residuals") 
plot(Viscera.weightcent2,basemodelfit$residuals,main="Residuals Vs Viscera Weight", xlab="Viscera Weight", ylab="Residuals") 
plot(Shell.weightcent2,basemodelfit$residuals,main="Residuals Vs Shell Weight",xlab="Shell Weight", ylab="Residuals")
#No sign of a relationship here, but we previously found possible non-linear relationships between some variables and the response variable
#from the pairwise scatter plots

#Now, we can look at full model with all polynomials
polyfit=lm(polyfitdata$V1~factor(polyfitdata$V2)+Lengthcent+ Diametercent+heightcent+Whole.weightcent+
Shucked.weightcent+ Viscera.weightcent+ Shell.weightcent+Lengthcent2+Diametercent2+ heightcent2+Whole.weightcent2+
  Shucked.weightcent2+ Viscera.weightcent2+ Shell.weightcent2, data=polyfitdata)
summary(polyfit)
plot(polyfit,which=1)
stepAIC(polyfit, scope=list( lower=~1), direction="both", k=log(n)) 
polyfitmodel6=lm(formula = polyfitdata$V1 ~ factor(polyfitdata$V2) + Diametercent + heightcent + 
                   Whole.weightcent + Shucked.weightcent + Viscera.weightcent + Shell.weightcent + Diametercent2 + 
                   heightcent2 + Whole.weightcent2 +Shucked.weightcent2 + Viscera.weightcent2, data = polyfitdata) 
summary(polyfitmodel6)
plot(polyfitmodel6,which=1)

#We narrow the number of models to 2 models based on best criteria values(CP,BIC,AIC, PRESS_P):
aicfs1=length(fs1_model1$fitted.values)* log(87.696/length(fs1_model1$fitted.values))+ 2*length(fs1_model1$coefficients) 
aicfs2=length(fs2_model2$fitted.values)* log(87.997/length(fs2_model2$fitted.values))+ 2*length(fs2_model2$coefficients) 
aicfullinters=length(fitfullinters$fitted.values)* log(72.202/length(fitfullinters$fitted.values))+ 2*length(fitfullinters$coefficients) 
aicfs3=length(fs3_model3inters$fitted.values)* log(73.060/length(fs3_model3inters$fitted.values))+ 2*length(fs3_model3inters$coefficients) 
aicfs4=length(fs4_model4inters$fitted.values)* log(77.092/length(fs4_model4inters$fitted.values))+ 2*length(fs4_model4inters$coefficients)
aicfs5=length(fs5_model5inters$fitted.values)* log(73.995/length(fs5_model5inters$fitted.values))+ 2*length(fs5_model5inters$coefficients)
aicfspoly=length(polyfit$fitted.values)* log(75.252/length(polyfit$fitted.values))+ 2*length(polyfit$coefficients)
aicfspolyfit6=length(polyfitmodel6$fitted.values)* log(75.501/length(polyfitmodel6$fitted.values))+ 2*length(polyfitmodel6$coefficients)

bicfs1=length(fs1_model1$fitted.values)* log(87.696/length(fs1_model1$fitted.values))+
                          log(length(fs1_model1$fitted.values))*length(fs1_model1$coefficients) 

bicfs2=length(fs2_model2$fitted.values)* log(87.997/length(fs2_model2$fitted.values))+ log(length(fs2_model2$fitted.values))*length(fs2_model2$coefficients)

bicfullinters=length(fitfullinters$fitted.values)* log(72.202/length(fitfullinters$fitted.values))+ 
                                      log(length(fitfullinters$fitted.values))*length(fitfullinters$coefficients)

bicfs3=length(fs3_model3inters$fitted.values)* log(73.060/length(fs3_model3inters$fitted.values))+ 
                                      log(length(fs3_model3inters$fitted.values))*length(fs3_model3inters$coefficients)

bicfs4=length(fs4_model4inters$fitted.values)* log(77.092/length(fs4_model4inters$fitted.values))+
                                              log(length(fs4_model4inters$fitted.values))*length(fs4_model4inters$coefficients)

bicfs5=length(fs5_model5inters$fitted.values)* log(73.995/length(fs5_model5inters$fitted.values)) + 
                                          log(length(fs5_model5inters$fitted.values))*length(fs5_model5inters$coefficients) 

bicfspoly=length(polyfit$fitted.values)* log(75.252/length(polyfit$fitted.values))+ 
                        log(length(polyfit$fitted.values))*length(polyfit$coefficients) 
bicfspolyfit6=length(polyfitmodel6$fitted.values)* log(75.501/length(polyfitmodel6$fitted.values))+ 
                        log(length(polyfitmodel6$fitted.values))*length(polyfitmodel6$coefficients)

cpfs1=87.696/0.03530641-(length(fs1_model1$fitted.values)- 2*length(fs1_model1$coefficients)) 
cpfs2=87.997/0.03530641-(length(fs2_model2$fitted.values)- 2*length(fs2_model2$coefficients)) 
cpfsfullinters=72.202/0.03530641-(length(fitfullinters$fitted.values)- 2*length(fitfullinters$coefficients))
cpfs3=73.060/0.03530641- (length(fs3_model3inters$fitted.values)- 2*length(fs3_model3inters$coefficients))
cpfs4=77.092/0.03530641-(length(fs4_model4inters$fitted.values)- 2*length(fs4_model4inters$coefficients))
cpfs5=73.995/0.03530641-(length(fs5_model5inters$fitted.values)- 2*length(fs5_model5inters$coefficients))
cpfspolyfull=75.252/0.03530641-(length(polyfit$fitted.values)- 2*length(polyfit$coefficients)) 
cpfs6=75.501/0.03530641-(length(polyfitmodel6$fitted.values)- 2*length(polyfitmodel6$coefficients))

PRESS_fullfs1 = sum((fs1_model1$residuals /(1-influence(fs1_model1)$hat))^2)
PRESS_fullfs2 = sum((fs2_model2$residuals /(1-influence(fs2_model2)$hat))^2) 
PRESS_fullfsfullinters = sum((fitfullinters$residuals /(1-influence(fitfullinters)$hat))^2)
PRESS_fullfs3 = sum((fs3_model3inters$residuals /(1-influence(fs3_model3inters)$hat))^2) 
PRESS_fullfs4 = sum((fs4_model4inters$residuals /(1-influence(fs4_model4inters)$hat))^2)
PRESS_fullfs5 = sum((fs5_model5inters$residuals /(1-influence(fs5_model5inters)$hat))^2) 
PRESS_fullfsfullpoly = sum((polyfit$residuals /(1-influence(polyfit)$hat))^2)
PRESS_fullfs6 = sum((polyfitmodel6$residuals/(1-influence(polyfitmodel6)$hat))^2)


Criteria.values.matrix= data.frame(cbind(c(87.696,87.997,72.202,73.060,77.092,73.995,75.252,75.501), c(0.6054,0.604,0.6751,0.6712,0.6531,0.667,0.6614,0.6602),
                                          c(0.6036,0.6025, 0.6681,0.6677,0.6511, 0.6644, 0.6587,0.6581), c(aicfs1,aicfs2,aicfullinters,aicfs3,aicfs4,aicfs5, aicfspoly,aicfspolyfit6),
                                          c(bicfs1,aicfs2,bicfullinters,bicfs3,bicfs4,bicfs5, bicfspoly,bicfspolyfit6), c(cpfs1,cpfs2,cpfsfullinters, cpfs3,cpfs4, cpfs5,cpfspolyfull,cpfs6),
                                         c(PRESS_fullfs1,PRESS_fullfs2, PRESS_fullfsfullinters,PRESS_fullfs3, PRESS_fullfs4, PRESS_fullfs5,PRESS_fullfsfullpoly,PRESS_fullfs6))) 

colnames(Criteria.values.matrix)=c("SSE", "R^2","R_a^2", "aic", "bic","C_p", "Press_p**") 

row.names(Criteria.values.matrix)=c("Model 1", "Model 2", "Full Model(Interactions)","Model 3", "Model 4", "Model 5", "Full Model(Quadratics)","Model 6")

#Looking at the table, Model 3 generated by AIC criteria has the lowest Press_p,Lowest C_P(Bias and variance), lowest aic and relatively low bic
#In addition, it has the best R^2 and R_a^2 compared to the other models sum 

#Model 5 generated by BIC criteria looks to be the second best model: second lowest aic, lowest bic, relatively low Cp and second lowest Press_p 

#Validation for Model 3 and model 5:
#We can see which model has the best MSPE_v
#For model 3 Cp is approximately p(little bias), and the press_p is not much larger than SSE(no overfitting)
#Model 5’s Cp more than p indicating bias, press_p is also close to SSE 
#We will fit these two models to the validation data: 
fs3_model3inters
fs5_model5inters
modelevalfs3=lm(data.v$Rings ~ Shell.weight + Shucked.weight +
                  Diameter + Whole.weight + Sex + Viscera.weight +
                  Height + Length + Shucked.weight:Diameter +
                  Shucked.weight:Sex + Shell.weight:Sex + Diameter:Height +
                  Diameter:Length + Height:Length + Diameter:Sex + Shell.weight:Shucked.weight +
                  Diameter:Whole.weight + Shucked.weight:Height,data=data.v)

modelevalfs5=lm(data.v$Rings ~ Sex + Length + Diameter + Height + Whole.weight + 
                  Shucked.weight + Viscera.weight + Shell.weight + 
                  Sex:Shucked.weight + Length:Diameter +
                  Length:Height + Diameter:Height + 
                  Diameter:Shucked.weight + Shucked.weight:Shell.weight,data=data.v)
summary(fs3_model3inters)
summary(modelevalfs3)
#Multiple sign changes
#Changes in estimation

#For the third model--Estimation and standard error percent changes: 
round(abs(coef(fs3_model3inters)-coef(modelevalfs3))/abs(coef(fs3_model3inters))*100,3)
sd.fs3= summary(fs3_model3inters)$coefficients[,"Std. Error"] 
sd.fs3.v= summary(modelevalfs3)$coefficients[,"Std. Error"] round(abs(sd.fs3-sd.fs3.v)/sd.fs3*100,3)
#MSPE:
newdata1=data.v[,-9]
pred.fs3 = predict.lm(fs3_model3inters, newdata1) 
mspev.fs3=mean((pred.fs3-data.v[,9])^2)
mspev.fs3
#MSPE_v = 0.03678189
PRESS_fullfs3/2089 #0.037147
73.060/2089 #0.03497367
#No severe over-fitting

#MODEL 5
summary(fs5_model5inters)
summary(modelevalfs5)
#One sign change, some changes in estimation and standard errors 
#For 5th model--Estimation and standard error percent changes: 
round(abs(coef(fs5_model5inters)-coef(modelevalfs5)) /abs(coef(fs5_model5inters))*100,3)
sd.fs5= summary(fs5_model5inters)$coefficients[,"Std. Error"] 
sd.fs5.v= summary(modelevalfs5)$coefficients[,"Std. Error"]
round(abs(sd.fs5-sd.fs5.v)/sd.fs5*100,3)
#MSPE
newdata2=data.v[,-9]
pred.fs5 = predict.lm(fs5_model5inters, newdata2)
mspe.fs5=mean((pred.fs5-data.v[,9])^2)
mspe.fs5
#MSPEv = 0.03650119
PRESS_fullfs5/2089 ##0.03732952
73.995/2089 ##0.03542125
#No severe over-fitting
#Model 5 has a smaller mspev than model 3.
#This model has better predictability than model 3.
#In addition, it has less coefficients, so it’s simpler.
#Furthermore, the changes in its estimation parameters and standard errors is 
#less in magnitude. It only has one change in coefficient sign. Whereas, model 3 has multiple 
#changes in sign from the training data fit to the validation data fit.
#I believe model 5 will be the best model to use for predicitng abalone age.
#Now, we can fit model to entire data set.
fit.fs1.final=lm(Rings ~ Sex + Length + Diameter +
                   Height + Whole.weight + Shucked.weight +
                   Viscera.weight + Shell.weight + Sex:Shucked.weight +
                   Length:Diameter + Length:Height + Diameter:Height + Diameter:Shucked.weight + Shucked.weight:Shell.weight, data=abaloneData) summary(fit.fs1.final)
anova(fit.fs1.final)
#Model Diagnostics 
par(mfrow=c(2,1)) 
plot(fit.fs1.final, which=1) 
plot(fit.fs1.final, which=2)
#Check outliers in Y 
res=residuals(fit.fs1.final)
n = nrow(abaloneData)
p = 23
h1 = influence(fit.fs1.final)$hat 
d.res.std=studres(fit.fs1.final)
max(abs(d.res.std)) 
sort(abs(d.res.std),decreasing=T) 
qt(1-0.1/(2*n),n-p-1)
idx.Y = as.vector(which(abs( d.res.std)>=qt(1-0.1/(2*n),n-p-1)))
idx.Y
##Outliers in Y 237,1217,2052,2184,2628
##leverages and outlying X
idx.X = as.vector(which(h1>(2*23/n)))
idx.X 
plot(h1,res,xlab="leverage",ylab="residuals") #many leverage points in x
##influence plot
plot(fit.fs1.final, which=4)
cook.d = res^2*h1/(p*1.293*(1-h1)^2)
cook.max = cook.d[which(cook.d==max(cook.d))] 
pf(cook.max,p,n-p)
idx = c(idx.X,idx.Y)
cook.d[idx]
pf(cook.d[idx],p,n-p)
#2052 has biggest cooks distance
#end of code