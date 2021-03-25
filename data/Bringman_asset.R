###############################################################
################# General information #########################
###############################################################

#RCODE of MANUSCRIPT BRINGMANN ET AL. 2013 PlosOne;
#Title: A network approach to psychopathology: New insights into clinical longitudinal data

#Data are a part of the dataset reported in Geschwind et al. 2011 
#(Geschwind N, Peeters F, Drukker M, van Os J, Wichers M (2011) Mindfulness training increases momentary positive emotions and reward experience in adults vulnerable to depression: A randomized controlled trial. J Consult Clin Psychol 79: 618-628. doi: 10.1037/a002459.)
  
#Figures of qgraph can look different since the paper used the qgraph version 1.0.4

###############################################################
################# Where the Figures will be saved #############
###############################################################

#For the best quality of the figures, the figures will not be opened in R immediately 
#but saved in the working direction.
#This can be for example the desktop or the document folder. 

getwd()# to see were you're figures are saved 
#With setwd() you can change the working directory
#For example: setwd("C:/Main folder/Subfolder1/Subfolder2")

###############################################################
################# LOAD the data ###############################
###############################################################

#First save the networkdata.txt file in your working directory.
#Then load a workspace (data) into the current session.

networkdata=read.table(file="Data S4.txt",header=TRUE,sep=",")
head(networkdata)#should look like this:

# subjno dayno beepno informat04 st_period opgewkt_ onplplez pieker angstig_ somber__ ontspann neur
# 1  10720     1      1         NA         0       NA       NA     NA       NA       NA       NA   NA
# 2  10720     1      2         NA         0       NA       NA     NA       NA       NA       NA   NA
# 3  10720     1      3         NA         0       NA       NA     NA       NA       NA       NA   NA
# 4  10720     1      4          0         0        4        3      6        5        4        2   34
# 5  10720     1      5          0         0        2        1      5        4        4        2   34
# 6  10720     1      6          0         0        3        2      5        3        5        5   34


########################################################################
################# Install and Load Packages ############################
########################################################################

#R PACKAGES you have to install & load in order to perform the analyses 

#install
install.packages("mvtnorm")
install.packages("arm")
install.packages("qgraph")

#load into your workspace
library(mvtnorm)
library(arm)
library(qgraph)

#Once you have loaded the networkdata and the packages, you
#can copy-paste the rest of the code into the R console, and
#you will get the pictures automatically in your working
#directory.


###############################################################
################# Transparency function #######################
###############################################################

#This function is needed for an optimal display of the networks with qgraph.

addTrans <- function(color,trans)
{
  # This function adds transparency to a color.
  # Define transparency with an integer between 0 and 255
  # 0 being fully transparent and 255 being fully visable
  # Works with either color and trans a vector of equal length,
  # or one of the two of length 1.
  
  if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) 
    stop("Vector lengths not correct")
  if (length(color)==1 & length(trans)>1) color <- rep(color,length(trans))
  if (length(trans)==1 & length(color)>1) trans <- rep(trans,length(color))
  
  num2hex <- function(x)
  {
    hex <- unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb <- rbind(col2rgb(color),trans)
  res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}

###############################################################
################# Lag function ################################
###############################################################

#This function lags the data appropriately (within person, within day).

lagK_data=function(x,vv,K){
  
  #x the data you want to be lagged
  #vv are the names of the columns of the variable you want to lag
  #k the number of lags
  
  dimens=dim(x)
  pp=unique(x$subjno)
  npp=length(pp)
  LEvv=length(vv)
  nday=10
  x=cbind(x,matrix(NA,dimens[1],K*LEvv+1))
  for (j in 1:npp){
    for (dd in 1:nday){
      for (ss in 0:1){
        uu=x[(x$subjno==pp[j])&(x$dayno==dd)&(x$st_p==ss),vv]
        end=dim(uu)
        Luu=array(NA,end)
        for (k in 1:K){
          x[(x$subjno==pp[j])&(x$dayno==dd)&(x$st_p==ss),(dimens[2]+1+(k-1)*LEvv):(dimens[2]+k*LEvv)]=rbind(t(rep(NA,LEvv)%*%t(rep(1,k))),as.matrix(uu[1:(end[1]-k),]))
        }
        x[(x$subjno==pp[j])&(x$dayno==dd)&(x$st_p==ss),(dimens[2]+1+K*LEvv)] = c(rep(0,K),rep(1,end[1]-K))
        
      }}}
  colnames(x)[(dimens[2]+1):(dimens[2]+K*LEvv+1)]<-c(paste(rep(vv,times=K),rep("L",K*LEvv),rep(1:K,each=LEvv),sep=""),"lagK")
  
  # change the names of the lagged columns to names with an extra L (lagged)
  
  return(x)
}


###############################################################
################# Lagging the data ############################
###############################################################

columns=c("opgewkt_","onplplez","pieker","angstig_","somber__","ontspann")
#these are the six variables used in the analysis

#A crash course in Dutch
#opgewekt= cheerful
#onplez= pleasantness of the event
#pieker= worry
#angstig= fearful
#somber= sad
#ontspann= relaxed

#data are lagged (with K=1, thus lag 1)
networkdataL=lagK_data(networkdata,columns,1) # this may take a few minutes
head(networkdataL)

# subjno dayno beepno informat04 st_period opgewkt_ onplplez pieker angstig_ somber__ ontspann neur opgewkt_L1
# 1  10720     1      1         NA         0       NA       NA     NA       NA       NA       NA   NA         NA
# 2  10720     1      2         NA         0       NA       NA     NA       NA       NA       NA   NA         NA
# 3  10720     1      3         NA         0       NA       NA     NA       NA       NA       NA   NA         NA
# 4  10720     1      4          0         0        4        3      6        5        4        2   34         NA
# 5  10720     1      5          0         0        2        1      5        4        4        2   34          4
# 6  10720     1      6          0         0        3        2      5        3        5        5   34          2
# onplplezL1 piekerL1 angstig_L1 somber__L1 ontspannL1 lagK
# 1         NA       NA         NA         NA         NA    0
# 2         NA       NA         NA         NA         NA    1
# 3         NA       NA         NA         NA         NA    1
# 4         NA       NA         NA         NA         NA    1
# 5          3        6          5          4          2    1
# 6          1        5          4          4          2    1


#####################################################################
################# The Analyses ######################################
#####################################################################

        ######################################################
        ####### Figure 1: The population network ###############
        ######################################################

###Fitting the data with multilevel-VAR method with the lme4 or lmer function

model1=list()
columns=c("opgewkt_","onplplez","pieker","angstig_","somber__","ontspann")

pred1 = "(opgewkt_L1+onplplezL1+piekerL1+angstig_L1+somber__L1+ontspannL1)+st_period+(opgewkt_L1+onplplezL1+piekerL1+angstig_L1+somber__L1+ontspannL1):st_period+st_period:informat04+st_period:(opgewkt_L1+onplplezL1+piekerL1+angstig_L1+somber__L1+ontspannL1):informat04+(factor(st_period)-1+opgewkt_L1+onplplezL1+piekerL1+angstig_L1+somber__L1+ontspannL1|subjno)"
for (j in 1:6){
  ff=as.formula(paste(columns[j],"~",pred1,sep=""))
  model1[[j]]<-lmer(ff,data=networkdataL,control=list(maxIter=800),REML=FALSE)
  
  print(j)
}

BIC1=unlist(lapply(model1,BIC))

sum(BIC1)




###inferring the coefficients or connection strengths for the network from the fitted model1

coef1=data.frame(matrix(unlist(lapply(model1,fixef),use.names=FALSE),byrow=TRUE, ncol=21)) 
se.coef1=data.frame(matrix(unlist(lapply(model1,se.fixef),use.names=FALSE),byrow=TRUE,ncol=21)) 
colnames(coef1)=names(fixef(model1[[1]]))
colnames(se.coef1)=names(fixef(model1[[1]]))
rownames(coef1)=columns
rownames(se.coef1)=columns



###making a Figure of the baseline average population network with Qgraph; pvalue<0.05
###Only the coefficients from the columns 2 to 7 (not the intercepts) of the six items are needed. Thus, not the six items with study period or therapy(informat04).

pdf("Figure1.pdf", width=6.83,height=6.83,useDingbats=F)

E=cbind(from=rep(1:6,each=6),to=rep(1:6,6),weigth=unlist(coef1[,2:7]))
pvals=2*(1-pnorm(abs(unlist(coef1[,2:7]/se.coef1[,2:7]))))
edge.color <- addTrans(ifelse(E[,3]>0, "green3", "red3"), ifelse(pvals<0.05, 255, 0))
qgraph(E,fade=FALSE,layout="circular",labels=c("C","E","W","F","S","R"),lty=ifelse(E[,3]>0,1,5),edge.labels=F,edge.color=edge.color)

dev.off()


        ######################################################
        ####### Figure 2 and 3: Individual differences #######
        ######################################################

###with "VV" the individual differences are taken from the fitted model1, each link now indicates the amount of individual variability

VV=sqrt(t(matrix(unlist(lapply(model1,function(x){VV=diag(VarCorr(x)$subjno[3:8,3:8])})),6,6)))


###the network figure of individual differences
pdf("Figure2.pdf", width=6.83,height=6.83,useDingbats=F)

E=cbind(from=rep(1:6,each=6),to=rep(1:6,6),weigth=as.vector(VV))
edge.color <- addTrans("blue", ifelse(E[,3]>.095, 255, 20))
qgraph(E,fade=FALSE,layout="circular",labels=c("C","E","W","F","S","R"),lty=ifelse(E[,3]>0,1,5),edge.labels=F,edge.color=edge.color)

dev.off()


###Network figure of two individuals; N=1 networks
###First the coefficients for all individuals are inferred
cc=list(model1[[1]],model1[[2]],model1[[3]],model1[[4]],model1[[5]],model1[[6]])
mat.ind=array(0,c(6,6,129))
for (x in 1:129){
  for (ii in 1:6){
    mat.ind[,ii,x] = as.numeric((fixef(cc[[ii]])[2:7]+ranef(cc[[ii]])$subjno[x,3:8]))
    
  }}
mat.ind


###The networks are made

pdf("Figure3.pdf", width=6.83*2,height=4*2,useDingbats=F)
par(mfcol=1:2)

jj=rep(1:6,each=6)
jk=rep(1:6,6)
E1=data.frame(from=jk,to=jj,weight=as.vector(mat.ind[,,1]))##individual 1 and subject 80 were taken as an example in the paper
qgraph(E1,layout="circular",labels=c("C","E","W","F","S","R"),lty=ifelse(E1[,3]>0,1,5),edge.labels=F,mar=c(5,5,5,5))#,filetype="pdf")

E2=data.frame(from=jk,to=jj,weight=as.vector(mat.ind[,,80]))##individual 1 and subject 80 were taken as an example in the paper
qgraph(E2,layout="circular",labels=c("C","E","W","F","S","R"),lty=ifelse(E2[,3]>0,1,5),edge.labels=F,mar=c(5,5,5,5))#,filetype="pdf")

dev.off()



        ######################################################
        ####### Figure 4: Centrality betweenness #############
        ######################################################

###Global network analyses: Neuroticism, centrality betweenness

#Making three neuroticism groups:low, mid, high
breaks = c(-Inf,quantile(networkdataL$neur,c(0.25,0.75),na.rm=T),Inf)
neur_mid_low_high=cut(networkdataL$neur,breaks=breaks,include.lowest=TRUE)
neur_mid_low_high = factor(neur_mid_low_high,levels(neur_mid_low_high)[c(2,1,3)])# to make the middle group the refrence group, we put it at the firs instead of the second level
#factor(neur_mid_low_high)# by checking the levels of the factor you see that it was done correctly
networkdataL$neur_mid_low_high=factor(neur_mid_low_high,levels(neur_mid_low_high)[c(2,1,3)])

#Fitting the neuroticism model without random effects
columns=c("opgewkt_","onplplez","pieker","angstig_","somber__","ontspann")
pred1Nlm = "(opgewkt_L1+onplplezL1+piekerL1+angstig_L1+somber__L1+ontspannL1)+st_period+(opgewkt_L1+onplplezL1+piekerL1+angstig_L1+somber__L1+ontspannL1):st_period+st_period:informat04+st_period:(opgewkt_L1+onplplezL1+piekerL1+angstig_L1+somber__L1+ontspannL1):informat04+neur_mid_low_high*(opgewkt_L1+onplplezL1+piekerL1+angstig_L1+somber__L1+ontspannL1)"
modelNlm=list()
for (j in 1:6){
  ff=as.formula(paste(columns[j],"~",pred1Nlm,sep=""))
  modelNlm[[j]]<-lm(ff,data=networkdataL,na.action=na.exclude)
  print(j)
}


#Taking out the coefficients
coefNlm=data.frame(matrix(unlist(lapply(modelNlm,coef),use.names=FALSE),byrow=TRUE,ncol=35))
colnames(coefNlm)=names(coef(modelNlm[[1]]))
predX=data.frame(matrix(unlist(lapply(modelNlm,fitted),use.names=FALSE),byrow=FALSE,ncol=6))
colnames(predX)=columns

#For the residuals na's are not excluded
columns=c("opgewkt_","onplplez","pieker","angstig_","somber__","ontspann")
pred1Nlm = "(opgewkt_L1+onplplezL1+piekerL1+angstig_L1+somber__L1+ontspannL1)+st_period+(opgewkt_L1+onplplezL1+piekerL1+angstig_L1+somber__L1+ontspannL1):st_period+st_period:informat04+st_period:(opgewkt_L1+onplplezL1+piekerL1+angstig_L1+somber__L1+ontspannL1):informat04+neur_mid_low_high*(opgewkt_L1+onplplezL1+piekerL1+angstig_L1+somber__L1+ontspannL1)"
modelNlm=list()
for (j in 1:6){
  ff=as.formula(paste(columns[j],"~",pred1Nlm,sep=""))
  modelNlm[[j]]<-lm(ff,data=networkdataL)
  print(j)
}

#Taking out the residuals
residX=data.frame(matrix(unlist(lapply(modelNlm,resid),use.names=FALSE),byrow=TRUE,ncol=6))


#Simulation of betweenness centrality using a parametric bootstrap method


alpha=1.5 #(see paperof Opsahl et al., 2010)
MM=networkdataL[,c("subjno","dayno","beepno","informat04","st_period","neur_mid_low_high")]
MM=cbind(MM,1:28600)
colnames(MM)[7]<-"ind"
MMlag=lagK_data(MM,c("ind","neur_mid_low_high"),1)
Xfix=MMlag[,1:6]
lag.ind=MMlag[,8]

Nrep=1000
cLOW.REP=matrix(NA,Nrep,6)
cMID.REP=matrix(NA,Nrep,6)
cHIGH.REP=matrix(NA,Nrep,6)

for (rep in 1:Nrep){
  err.ind=sample(8773,28600,replace=TRUE)
  error=residX[err.ind,]
  yrep=predX+error
  colnames(yrep)=columns
  yrepL=yrep[lag.ind,]
  colnames(yrepL)=paste(columns,"L1",sep="")
  Yrep=cbind(Xfix,yrep,yrepL)
  print(rep)
  modelNlmREP=list()
  for (j in 1:6){
    ff=as.formula(paste(columns[j],"~",pred1Nlm,sep=""))
    modelNlmREP[[j]]<-lm(ff,data=Yrep)
  }
  coefNlmREP=data.frame(matrix(unlist(lapply(modelNlmREP,coef),use.names=FALSE),byrow=TRUE,ncol=35))
  colnames(coefNlmREP)=names(coef(modelNlmREP[[1]]))
  coefLOWREP=coefNlmREP[,2:7]
  coefMIDREP=coefLOWREP+coefNlmREP[,seq(18,29,2)]
  coefHIGHREP=coefLOWREP+coefNlmREP[,seq(19,29,2)]
  
  E=cbind(from=rep(1:6,each=6),to=rep(1:6,6),weigth=unlist(coefLOWREP))
  Qlow=qgraph(E,DoNotPlot=TRUE)
  cLOW.REP[rep,]=centrality(Qlow,alpha=alpha)$Betweenness
  
  E=cbind(from=rep(1:6,each=6),to=rep(1:6,6),weigth=unlist(coefMIDREP))
  Qmid=qgraph(E,DoNotPlot=TRUE)
  cMID.REP[rep,]=centrality(Qmid,alpha=alpha)$Betweenness
  
  E=cbind(from=rep(1:6,each=6),to=rep(1:6,6),weigth=unlist(coefHIGHREP))
  Qhigh=qgraph(E,DoNotPlot=TRUE)
  cHIGH.REP[rep,]=centrality(Qhigh,alpha=alpha)$Betweenness
}


#Making the figure

pdf("Figure4.pdf",useDingbats=F)

error.bar <- function(x, y, upper, lower=upper, length=0.0,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

letvec=c(64+3,64+5,64+23,64+6,64+19,64+18)#letters
strletvec=c("C","E","W","F","S","R")
par(mfrow=c(1,3),mar=c(4,4.5,1,2))

lwdg=1.5
lwdc=2

lowq=.025
highq=.975

lowq2=.25
highq2=.75

### LOW NEUR
MLOW=apply(cLOW.REP,2,median)
Q25LOW=apply(cLOW.REP,2,quantile,lowq)
Q75LOW=apply(cLOW.REP,2,quantile,highq)

Q25LOW2=apply(cLOW.REP,2,quantile,lowq2)
Q75LOW2=apply(cLOW.REP,2,quantile,highq2)
ord=c(1,5,6,3,4,2)
plot(1,0,col=0,xlim=c(1,6.5),ylim=c(0,12),xlab="",bty="n",ylab="Centrality index (betweenness)",xaxt="n",cex.lab=1.3,cex.axis=1.5,main="LOW")#yaxt='n',ylab='',bty="n")#,ylab="centrality index")
axis(1,pos=0,at=1:6,labels=strletvec[ord],cex.axis=1.25)
error.bar(x=1:6,y=MLOW[ord],upper=Q75LOW[ord]-MLOW[ord],lower=MLOW[ord]-Q25LOW[ord],lwd=lwdg,col="grey85")
error.bar(x=1:6,y=MLOW[ord],upper=Q75LOW2[ord]-MLOW[ord],lower=MLOW[ord]-Q25LOW2[ord],lwd=lwdc,col="grey50")

points(MLOW[ord],pch=19,type="b")

### MID NEUR
MMID=apply(cMID.REP,2,median)
meanMID=apply(log(cMID.REP+1),2,mean)
Q25MID=apply(cMID.REP,2,quantile,lowq)
Q75MID=apply(cMID.REP,2,quantile,highq)

Q25MID2=apply(cMID.REP,2,quantile,lowq2)
Q75MID2=apply(cMID.REP,2,quantile,highq2)

plot(1,0,col=0,xlim=c(1,6.5),ylim=c(0,12),xlab="",bty="n",ylab="",yaxt="n",xaxt="n",cex.lab=1.3,cex.axis=1.3,main="MID")#yaxt='n',ylab='',bty="n")#,ylab="centrality index")
axis(1,pos=0,at=1:6,labels=strletvec[ord],cex.axis=1.25)

error.bar(x=1:6,y=MMID[ord],upper=Q75MID[ord]-MMID[ord],lower=MMID[ord]-Q25MID[ord],lwd=lwdg,col="grey85")
error.bar(x=1:6,y=MMID[ord],upper=Q75MID2[ord]-MMID[ord],lower=MMID[ord]-Q25MID2[ord],lwd=lwdc,col="grey50")
points(MMID[ord],pch=19,type="b")

### HIGH NEUR
MHIGH=apply(cHIGH.REP,2,median)
meanHIGH=apply(log(cHIGH.REP+1),2,mean)
Q25HIGH=apply(cHIGH.REP,2,quantile,lowq)
Q75HIGH=apply(cHIGH.REP,2,quantile,highq)
Q25HIGH2=apply(cHIGH.REP,2,quantile,lowq2)
Q75HIGH2=apply(cHIGH.REP,2,quantile,highq2)
plot(1,0,col=0,xlim=c(1,6.5),ylim=c(0,12),xlab="",bty="n",ylab="",yaxt="n",xaxt="n",cex.lab=1.3,cex.axis=1.3,main="HIGH")#yaxt='n',ylab='',bty="n")#,ylab="centrality index")
axis(1,pos=0,at=1:6,labels=strletvec[ord],cex.axis=1.25)
error.bar(x=1:6,y=MHIGH[ord],upper=Q75HIGH[ord]-MHIGH[ord],lower=MHIGH[ord]-Q25HIGH[ord],lwd=lwdg,col="grey85")
error.bar(x=1:6,y=MHIGH[ord],upper=Q75HIGH2[ord]-MHIGH[ord],lower=MHIGH[ord]-Q25HIGH2[ord],lwd=lwdc,col="grey50")
points(MHIGH[ord],pch=19,type="b")



dev.off()