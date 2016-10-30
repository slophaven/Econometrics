#####################################################################################################################
# Objective:
# To predict the high and low peaks of a time series based on a number of explanatory variables (predictors).
#
# Dataset:
# The dataset consisted of 146355 observations and 288 variables. The response variable of primary interest is
# called swingRecordType and has three possible outcomes, i.e. -1=low peak (n=10708), 0=no peak (n=126107) and 
# 1=high peak (n=9540). There are 285 potential explanatory variables (columns 4-288). Some of these are factors 
# with two possible outcomes (true and false). Others are handled as continuous variables. The explanatory 
# variables in columns 85-96 were not taken into account in any of the analyses because they only contained zeros
# and therefore had no predictive value. Thus, 273 explanatory variables were available.
#
# Methodology:
# Ordinal logistic regression was used for prediction. The dataset was split randomly into a training and a test set. 
# The test set consisted of 10% of the observations and the training set of the remaining 90%. The training dataset
# was used to develop the model, and the predictive performance of the model was tested on the test dataset. As a
# starting point the full model consisting of all 273 explanatory variables was fitted (model 1). In a second step
# all explanatory variables with a p-values above 0.05 were removed from the model, and the reduced model was
# reestimated (model 2).
#
# Results:
# The results are reported for one particular split of the dataset into a training and a test part. Model 1 included
# 273 explanatory variables. The output from model 2 showed that 155 of the 273 explanatory variables were 
# non-significant on a 5% level and therefore removed from the model. Thus, model 2 had 118 explanatory variables 
# included. The test dataset consisted of 14635 observations with 1122 low peaks, 12535 no peaks and 978 high peaks. 
# With model 1 739 of the 1122 low peaks (66%) were correctly predicted. For no peak the corresponding numbers were 
# 12182 out of 12535 (97%), and for high peaks 630 out of 978 (64%) were correctly predicted. With model 2 740 of the
# 1122 low peaks (66%) were correctly predicted. For no peak the corresponding numbers were 12194 out of 12535 (97%), 
# and for high peaks 626 out of 978 (64%) were correctly predicted. It is seen that the predictive performance of the 
# two models is very similar. To summarize, our modelling approach is able to predict approximately 2/3 of the
# peaks correctly.
#####################################################################################################################

# Load library
if (!require("ordinal")){install.packages("ordinal")}

# Make the results reproduceable
set.seed(125)

# Load dataset
swing.hi.lo<-read.csv(url("http://daytradinglogic.com/_neal/swing/csv/swingData_ES_1.50.csv"))

# Change variable names to something human
for(i in 4:288){
	names(swing.hi.lo)[i]<-paste("x",i,sep="")
} 
swing.hi.lo$swingRecordType<-swing.hi.lo$swingRecordType+1
swing.hi.lo$y<-as.factor(swing.hi.lo$swingRecordType)

# Build formula and remove columns 85-96 as they only contain zeros
form<-"y~"
for(i in 4:288){
	form<-paste(form,"x",i,"+",sep="")
}
form<-substr(form,1,nchar(form)-1)

for(i in 85:96){
	form<-gsub(paste("+x",i,sep=""),"",form,fixed=TRUE)
}

# Split dataset
proportion.train<-0.9
n.train<-round(dim(swing.hi.lo)[1]*proportion.train)

train.in<-sample(seq_len(nrow(swing.hi.lo)),size=n.train)
train<-swing.hi.lo[train.in, ]
test<-swing.hi.lo[-train.in, ]

# Modelling

# Model 1: Containing all 273 predictors potentially carrying information
mod1<-clm(as.formula(form),data=train)

# Model 2: Remove non-significant predictors
tt<-summary(mod1)$coef[,4]>0.05
remove.terms.mod2<-rownames(summary(mod1)$coef[tt,])

for(i in 1:length(remove.terms.mod2)){
	remove.terms.mod2[i]<-gsub(" FALSE","",remove.terms.mod2[i],fixed=TRUE)
}

remove.terms.mod2<-remove.terms.mod2[!is.na(remove.terms.mod2)]

for(i in 1:length(remove.terms.mod2)){

	if(remove.terms.mod2[i]=="x4"){form<-gsub(paste(remove.terms.mod2[i],"+",sep=""),"",form,fixed=TRUE)} 
	else if(remove.terms.mod2[i]=="x288"){form<-gsub(paste("+",remove.terms.mod2[i],sep=""),"",form,fixed=TRUE)} 
	else form<-gsub(paste("+",remove.terms.mod2[i],"+",sep=""),"+",form,fixed=TRUE)

}

mod2<-clm(as.formula(form),data=train)

pred1<-predict(mod1,test,type="class")
dat1<-data.frame(obs=test$y,fit=pred1)
xtabs(formula= ~obs+fit,data=dat1)

pred2<-predict(mod2,test,type="class")
dat2<-data.frame(obs=test$y,fit=pred2)
xtabs(formula= ~obs+fit,data=dat2)

