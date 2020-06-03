rm(list=ls())

setwd("C:/Users/charl/OneDrive/Documents/Master 2/Mémoire didactique/Cours d'été physique")
data<-read.csv("DATA_conceptions FCI.csv",header=TRUE,sep=";",na.strings=c("NA"))
data$Etudes<- factor(data$Etudes,exclude=NULL)
data$Heures.phys<- factor(data$Heures.phys,exclude=NULL)
data$Groupe<- factor(data$Groupe,exclude=NULL)

#Permet de ne garder que les élèves présent au pré ET au post
data<-data[is.na(data$Diff.Score)==FALSE,]

#Vecteurs réponses correctes au FCI
Items<-c("C","A","C","E","B","B","B","B","E","A","D","B","D","D","A","A","B","B","E","D","E","B","B","A","C","E","C","E","B","C")
Items<-as.factor(Items)

#Création des variables score.pré.test, score.post.test et gain FCI
data$Score.pré.test=0
data$Score.post.test=0
for(i in 1:nrow(data)){
  for (j in 9:38){
    if(data[i,j]==Items[j-8] & is.na(data[i,j])==FALSE)
    {data$Score.pré.test[i]=data$Score.pré.test[i]+1}
  }
}

for(i in 1:nrow(data)){
  for (j in 40:69){
    if(data[i,j]==Items[j-39] & is.na(data[i,j])==FALSE)
    {data$Score.post.test[i]=data$Score.post.test[i]+1}
  }
}
data$Diff.Score=data$Score.post.test-data$Score.pré.test

Items1 <-c(3,1,3,5,2,2,2,2,5,1,4,2,4,4,1,1,2,2,5,4,5,2,2,1,3,5,3,5,2,3)

#Retire les facteurs -1,0,1 des réponses au questions FCI 

for (i in 9:38) {
  data[,i]<-droplevels(data[,i])
}
for (i in 40:69) {
  data[,i]<-droplevels(data[,i])
}

data_F<-data[data$Sexe=="F",]
data_H<-data[data$Sexe=="M",]
data_I0<-data[data$Inhibition==0,]
data_I1<-data[data$Inhibition==1,]

data_I1_H<-data_I1[data_I1$Sexe=="M",]
data_I1_F<-data_I1[data_I1$Sexe=="F",]

#Vecteur couleurs FCI (rouge réponses incorrectes et vert lorsque réponse est correcte)
colors<-c()
for(i in 1:150) {
  if (i==3 |i==6|i==13|i==20|i==22|i==27|i==32|i==37|i==45|i==46|i==54|i==57|i==64|i==69|i==71|i==76|i==82|i==87|i==95|i==99|i==105
      |i==107|i==112|i==116|i==123|i==130|i==133|i==140|i==142|i==148)
  {
    colors[i]<-"Lawngreen"
  }
  else {
    colors[i]<-"tomato"
  }
}
####################################################comparaison pré-post##############################################################

#Calcul taux de réponses correctes au pré-test
AbsPré<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CorrectPré<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WrongPré<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 9:38) {
  for (i in 1:nrow(data)) {
    if (is.na(data[i,j])==TRUE){
      AbsPré[j-8]=AbsPré[j-8]+1
    }
    else if (is.na(data[i,j])==FALSE & data[i,j]!=Items[j-8])
    {WrongPré[j-8]<-WrongPré[j-8]+1}
    else if (is.na(data[i,j])==FALSE & data[i,j]==Items[j-8]) 
    {CorrectPré[j-8]<-CorrectPré[j-8]+1}
  }
}

print(CorrectPré+WrongPré+AbsPré)

#Calcul taux de réponses correctes au post-test
AbsPost<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CorrectPost<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WrongPost<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 40:69) {
  for (i in 1:nrow(data)) {
    if (is.na(data[i,j])==TRUE){
      AbsPost[j-39]=AbsPost[j-39]+1
    }
    else if (is.na(data[i,j])==FALSE & data[i,j]!=Items[j-39])
    {WrongPost[j-39]<-WrongPost[j-39]+1}
    else if (is.na(data[i,j])==FALSE & data[i,j]==Items[j-39]) 
    {CorrectPost[j-39]<-CorrectPost[j-39]+1}
  }
}

print(CorrectPost+WrongPost+AbsPost)

Présence.Pré<- nrow(data)-AbsPré 
Présence.Post<- nrow(data)-AbsPost

CorrectPré<-CorrectPré/Présence.Pré
CorrectPost<-CorrectPost/Présence.Post

#Barplot comparaison entre taux de réponses correctes entre le pré et le post-test

Comp_correct_prépost <- rbind(100*CorrectPré,100*CorrectPost)
Questions<-c(1:30)
mp<-barplot(Comp_correct_prépost, main = "Score par question au FCI", 
            xlab = "Numéro des questions au FCI", ylab = "Score", 
            ylim=c(0,110),beside=TRUE,names.arg=Questions, col=c("cornflowerblue","tomato"),cex.lab=0.85)
grid(nx=NA, ny=NULL, col="gray19")
abline(h = mean(100*CorrectPré),
       col = "cornflowerblue",
       lwd = 2)
abline(h = mean(100*CorrectPost),
       col = "tomato",
       lwd = 2)
text(x=c(94,94),y=c(mean(100*CorrectPré)+5,mean(100*CorrectPost)+5),xpd=TRUE,labels=c("31%","45%"),
     col=c("cornflowerblue","tomato"), cex=1.5)
legend("topleft",c("pré-test (N=85)","post-test (N=85)"),cex = 0.85, fill = c("cornflowerblue","tomato"))

#Barplot du gain question par question
CorrectPost<-100*CorrectPost
CorrectPré<-100*CorrectPré
Diffscore <-(CorrectPost-CorrectPré)
Gainnorm<-Diffscore/(100-CorrectPré)
SizeEffect<-(mean(CorrectPost)-mean(CorrectPré))/(0.5*(sd(CorrectPost)+sd(CorrectPré)))
Questions<-c(1:30)
barplot(Gainnorm, main = "Gain normalisé en fonction des différentes questions du FCI", 
        space=0.75, xlab = "Numéro des questions du FCI",names.arg=Questions,
        ylab = "Gain normalisé",ylim=c(-0.2,1),col="palegreen3",cex.lab=0.85)
grid(nx=NA, ny=NULL, col="gray19")
abline(h = mean(Gainnorm),
       col = "palegreen3",
       lwd = 2)
text(x=55,y=mean(Gainnorm)+0.09,xpd=TRUE,labels="0.21",col="palegreen3",cex=1.5)

#Classification des réponses par dimension
Cin.pré <- 100*c(CorrectPré[12],CorrectPré[14],CorrectPré[19],CorrectPré[20])
Cin.post <-100*c(CorrectPost[12],CorrectPost[14],CorrectPost[19],CorrectPost[20])

N1.pré <-100*c(CorrectPré[6],CorrectPré[7],CorrectPré[8],CorrectPré[10],CorrectPré[17],CorrectPré[23],CorrectPré[24],CorrectPré[25])
N1.post<-100*c(CorrectPost[6],CorrectPost[7],CorrectPost[8],CorrectPost[10],CorrectPost[17],CorrectPost[23],CorrectPost[24],CorrectPost[25])

N2.pré<-100*c(CorrectPré[21],CorrectPré[22],CorrectPré[26],CorrectPré[27])
N2.post<-100*c(CorrectPost[21],CorrectPost[22],CorrectPost[26],CorrectPost[27])

N3.pré<-100*c(CorrectPré[4],CorrectPré[15],CorrectPré[16],CorrectPré[28])
N3.post<-100*c(CorrectPost[4],CorrectPost[15],CorrectPost[16],CorrectPost[28])

Superposition.pré <-100*c(CorrectPré[9])
Superposition.post <-100*c(CorrectPost[9])

Forces.pré<-100*c(CorrectPré[1],CorrectPré[2],CorrectPré[3],CorrectPré[5],CorrectPré[11],CorrectPré[13],
                  CorrectPré[18],CorrectPré[29],CorrectPré[30])
Forces.post<- 100*c(CorrectPost[1],CorrectPost[2],CorrectPost[3],CorrectPost[5],CorrectPost[11],
                    CorrectPost[13],CorrectPost[18],CorrectPost[29],CorrectPost[30])

#Barplot de la moyenne du taux de réponses correctes par dimension
Concept.FCI <- matrix(c(mean(Cin.pré),mean(N1.pré),mean(N2.pré),mean(N3.pré),mean(Superposition.pré),mean(Forces.pré),
                        mean(Cin.post),mean(N1.post),mean(N2.post),mean(N3.post),mean(Superposition.post),mean(Forces.post)),
                      nrow=2,byrow=TRUE)

mydata<-data.frame(
  mean= c(mean(Cin.pré),mean(N1.pré),mean(N2.pré),mean(N3.pré),mean(Superposition.pré),mean(Forces.pré),
           mean(Cin.post),mean(N1.post),mean(N2.post),mean(N3.post),mean(Superposition.post),mean(Forces.post)),
  sd= c(sd(Cin.pré),sd(N1.pré),sd(N2.pré),sd(N3.pré),sd(Superposition.pré),sd(Forces.pré),
         sd(Cin.post),sd(N1.post),sd(N2.post),sd(N3.post),sd(Superposition.post),sd(Forces.post))
)

mydata <- do.call(data.frame, mydata)
colnames(mydata) <- c("mean", "sd")

barCenters <- barplot(height = Concept.FCI,
                      beside = TRUE, las = 2,
                      ylim=c(0,65),
                      cex.names = 0.75, xaxt = "n",
                      main = "Pourcentage de réponses correctes en fonction de la dimension
 conceptuelle au FCI (N=85)",
                      ylab = "Réponses correctes (%)",
                      border = "black", axes = TRUE, col=c("gray92","gray45") )

#c("gray96","lightskyblue3","gray96","lightskyblue4")
text(x=c(3,5.3,8.2,11.3,15.1,17.6),y = par("usr")[3]- 3,
     adj = 1, xpd = TRUE, labels =c("Cinématique","N1","N2","N3","Superposition","Forces"))
text(x=barCenters+0.2,y=Concept.FCI+1.5,adj = 1, xpd = TRUE, labels =signif(Concept.FCI,2))

legend("top",c("pré-test","post-test"),
       cex = 0.8, fill=c("gray92","gray45")) 

#Différence entre le registre verbal et visuel pour la dimension N1

N1.visuel.pré<-c(N1.post[1],N1.post[2],N1.post[3],N1.post[6])
N1.verbal.pré<-c(N1.post[4],N1.post[5],N1.post[7],N1.post[8])

  ##Test statistique de permutation
library(coin)
x<-N1.visuel.pré
y<-N1.verbal.pré
list = c(x,y)
noms = factor(rep(c("A", "B"), c(length(x), length(y))))
boxplot(list~noms)
oneway_test(list ~ noms)
  ## Histogramme afin de vérifier le type de distribution
par(mfrow = c(1,2))
hist(x,ylab='densité',breaks=15,prob=TRUE)
hist(y,ylab='densité',breaks=15,prob=TRUE)


###############################################comparaison item par item#######################################

#Calcul des taux des réponse item par item au pré
AbsPré<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
APré<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
BPré<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CPré<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
DPré<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
EPré<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 9:38) {
  for (i in 1:nrow(data)) {
    if (is.na(data[i,j])==TRUE){
      AbsPré[j-8]=AbsPré[j-8]+1
    }
    else if (is.na(data[i,j])==FALSE & data[i,j]=="A")
    {APré[j-8]<-APré[j-8]+1}
    else if (is.na(data[i,j])==FALSE  & data[i,j]=="B")
    {BPré[j-8]<-BPré[j-8]+1}
    else if (is.na(data[i,j])==FALSE  & data[i,j]=="C")
    {CPré[j-8]<-CPré[j-8]+1}
    else if (is.na(data[i,j])==FALSE & data[i,j]=="D")
    {DPré[j-8]<-DPré[j-8]+1}
    else if (is.na(data[i,j])==FALSE & data[i,j]=="E")
    {EPré[j-8]<-EPré[j-8]+1}
  }
}
print(APré+BPré+CPré+DPré+EPré+AbsPré)

#Calcul des taux des réponse item par item au post
AbsPost<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
APost<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
BPost<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CPost<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
DPost<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
EPost<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 40:69) {
  for (i in 1:nrow(data)) {
    if (is.na(data[i,j])==TRUE){
      AbsPost[j-39]=AbsPost[j-39]+1
    }
    else if (is.na(data[i,j])==FALSE & data[i,j]=="A")
    {APost[j-39]<-APost[j-39]+1}
    else if (is.na(data[i,j])==FALSE & data[i,j]=="B")
    {BPost[j-39]<-BPost[j-39]+1}
    else if (is.na(data[i,j])==FALSE & data[i,j]=="C")
    {CPost[j-39]<-CPost[j-39]+1}
    else if (is.na(data[i,j])==FALSE & data[i,j]=="D")
    {DPost[j-39]<-DPost[j-39]+1}
    else if (is.na(data[i,j])==FALSE & data[i,j]=="E")
    {EPost[j-39]<-EPost[j-39]+1}
  }
}
AbsPost[25]=2
AbsPost[29]=5
print(APost+BPost+CPost+DPost+EPost+AbsPost)

#Conversion en pourcentage 
Présence.Pré<- nrow(data)-AbsPré 
Présence.Post<- nrow(data)-AbsPost

APré<-APré/Présence.Pré
BPré<-BPré/Présence.Pré
CPré<-CPré/Présence.Pré
DPré<-DPré/Présence.Pré
EPré<-EPré/Présence.Pré
APost<-APost/Présence.Post
BPost<-BPost/Présence.Post
CPost<-CPost/Présence.Post
DPost<-DPost/Présence.Post
EPost<-EPost/Présence.Post

#Graphique taux de réponses question par question pour les différents item
par(mfrow = c(2,1))

Comp_pré <- rbind(100*APré,100*BPré,100*CPré,100*DPré,100*EPré)
Comp_post<-rbind(100*APost,100*BPost,100*CPost,100*DPost,100*EPost)
Comp_prépost <- Comp_post - Comp_pré
Questions<-c(1:30)

mp<-barplot(Comp_pré, main = "Taux de réponses des items FCI au pre-test", 
            xlab = "Numéro des questions du FCI", ylab = "Taux de réponses des items FCI", 
            ylim=c(0,100),beside=TRUE,names.arg=Questions, col=colors,cex.lab=0.85)
grid(nx=NA, ny=NULL, col="gray19")
legend("topleft",c("réponse incorrecte","réponse correcte"),
       cex = 0.6, fill = c("tomato","lawngreen"),bty="n")
text(x=180,y=120,xpd=TRUE,labels="a)",cex=1.5)

mp<-barplot(Comp_post, main = "Taux de réponses des items FCI au post-test", 
            xlab = "Numéro des questions du FCI", ylab = "Taux de réponses des items FCI", 
            ylim=c(0,100),beside=TRUE,names.arg=Questions, col=colors,cex.lab=0.85)
grid(nx=NA, ny=NULL, col="gray19")
legend("topleft",c("réponse incorrecte","réponse correcte"),
       cex = 0.6, fill = c("tomato","lawngreen"),bty="n")
text(x=180,y=120,xpd=TRUE,labels="b)",cex=1.5)

######################################distribution comparaison item par item##############################
#Base de données en fonction du genre et le fait d'avoir participé au WCST
data_F_I1<-data_F[data_F$Inhibition==1,]
data_H_I1<-data_H[data_H$Inhibition==1,]

#Calcul des taux de réponses par "préconception" pour chaque question
  ##Base de donnée data.pré/data.post permet de sélectionner les 30 questions pour une base de données souhaitée (par ex: data_H,data_I1,...)
data.pré<-data_H[,9:38]
data.post<-data_H[,40:69]
  ##Conversion réponse A,B,C,D,E en préconception associée pour le pré et le post
Conceptions <-matrix(data=NA,nrow=nrow(data.pré[,]),ncol=ncol(data.pré[,]))
Conceptions1 <-matrix(data=NA,nrow=nrow(data.post[,]),ncol=ncol(data.post[,]))
for(i in 1:nrow(data.pré[,])){
    if ( !is.na(data.pré$Q1[i]) & (data.pré$Q1[i]=="A" | data.pré$Q1[i]=="D" )){
      Conceptions[i,1]="G3"
    }
  else if (!is.na(data.pré$Q1[i]) & (data.pré$Q1[i]=="B" | data.pré$Q1[i]=="E") ){
    Conceptions[i,1]="G6"
  }
  else if (!is.na(data.pré$Q1[i]) & data.pré$Q1[i]=="C"){
    Conceptions[i,1]="Correct"
  }
  
  #Q2
  if (!is.na(data.pré$Q2[i]) & (data.pré$Q2[i]=="B" | data.pré$Q2[i]=="D") ){
    Conceptions[i,2]="G3"
  }
  else if (!is.na(data.pré$Q2[i]) & data.pré$Q2[i]=="A" ){
    Conceptions[i,2]="Correct"
  }
  else if (!is.na(data.pré$Q2[i]) & (data.pré$Q2[i]=="C" | data.pré$Q2[i]=="E") ){
    Conceptions[i,2]="NA"
  }
  else if(is.na(data.pré$Q2[i])==TRUE){
    Conceptions[i,2]="0"
  }
  
  
  #Q3
  if (!is.na(data.pré$Q3[i]) & data.pré$Q3[i]=="A"){
    Conceptions[i,3]="AF6"
  }
  else if (!is.na(data.pré$Q3[i]) & data.pré$Q3[i]=="B"){
    Conceptions[i,3]="AF5,G4"
  }
  else if (!is.na(data.pré$Q3[i]) & data.pré$Q3[i]=="C"){
    Conceptions[i,3]="Correct"
  }
  else if (!is.na(data.pré$Q3[i]) & data.pré$Q3[i]=="D"){
    Conceptions[i,3]="G2"
  }
  else if (!is.na(data.pré$Q3[i]) & data.pré$Q3[i]=="E"){
    Conceptions[i,3]="G1"
  }
  
  #Q4
  if (!is.na(data.pré$Q4[i]) & (data.pré$Q4[i]=="A" | data.pré$Q4[i]=="D") ){
    Conceptions[i,4]="AR1"
  }
  else if (!is.na(data.pré$Q4[i]) & data.pré$Q4[i]=="E"  ){
    Conceptions[i,4]="Correcte"
  }
  else if (!is.na(data.pré$Q4[i]) & data.pré$Q4[i]=="B"  ){
    Conceptions[i,4]="NA"
  }
  else if (!is.na(data.pré$Q4[i]) & data.pré$Q4[i]=="C"  ){
    Conceptions[i,4]="OB"
  }
  
  #!Q5
  if (!is.na(data.pré$Q5[i]) & (data.pré$Q5[i]=="C" | data.pré$Q5[i]=="D") ){
    Conceptions[i,5]="AR1"
  }
  else if (!is.na(data.pré$Q5[i]) & data.pré$Q5[i]=="E"  ){
    Conceptions[i,5]="CF,AF2"
  }
  else if (!is.na(data.pré$Q5[i]) & data.pré$Q5[i]=="B"  ){
    Conceptions[i,5]="Correct"
  }
  else if (!is.na(data.pré$Q5[i]) & data.pré$Q5[i]=="A"  ){
    Conceptions[i,5]="NA"
  }
  
  #Q6
  if (!is.na(data.pré$Q6[i]) & (data.pré$Q6[i]=="C" | data.pré$Q6[i]=="E") ){
    Conceptions[i,6]="CF"
  }
  else if (!is.na(data.pré$Q6[i]) & data.pré$Q6[i]=="A"  ){
    Conceptions[i,6]="I5"
  }
  else if (!is.na(data.pré$Q6[i]) & data.pré$Q6[i]=="B"  ){
    Conceptions[i,6]="Correct"
  }
  else if (!is.na(data.pré$Q6[i]) & data.pré$Q6[i]=="D"  ){
    Conceptions[i,6]="CF,CI2"
  }
  
  #Q7
  if (!is.na(data.pré$Q7[i]) & data.pré$Q7[i]=="A"  ){
    Conceptions[i,7]="I5"
  }
  else if (!is.na(data.pré$Q7[i]) & data.pré$Q7[i]=="B"  ){
    Conceptions[i,7]="Correct"
  }
  else if (!is.na(data.pré$Q7[i]) & data.pré$Q7[i]=="C"  ){
    Conceptions[i,7]="CI2,CF"
  }
  else if (!is.na(data.pré$Q7[i]) & data.pré$Q7[i]=="D"  ){
    Conceptions[i,7]="I2,I5,CF"
  }
  else if (!is.na(data.pré$Q7[i]) & data.pré$Q7[i]=="E"  ){
    Conceptions[i,7]="CF"
  }
  
  #Q8
  if (!is.na(data.pré$Q8[i]) & data.pré$Q8[i]=="A"  ){
    Conceptions[i,8]="CI3"
  }
  else if (!is.na(data.pré$Q8[i]) & data.pré$Q8[i]=="B"  ){
    Conceptions[i,8]="Correct"
  }
  else if (!is.na(data.pré$Q8[i]) & (data.pré$Q8[i]=="C" | data.pré$Q8[i]=="E" )){
    Conceptions[i,8]="I2"
  }
  else if (!is.na(data.pré$Q8[i]) & data.pré$Q8[i]=="D"){
    Conceptions[i,8]="I4"
  }
  
  #Q9
  if (!is.na(data.pré$Q9[i]) & (data.pré$Q9[i]=="A" | data.pré$Q9[i]=="D" )){
    Conceptions[i,9]="NA"
  }
  else if (!is.na(data.pré$Q9[i]) & data.pré$Q9[i]=="B"){
    Conceptions[i,9]="CI3"
  }
  else if (!is.na(data.pré$Q9[i]) & data.pré$Q9[i]=="C"){
    Conceptions[i,9]="K3"
  }
  else if (!is.na(data.pré$Q9[i]) & data.pré$Q9[i]=="E"){
    Conceptions[i,9]="Correct"
  }
  
  #Q10
  if (!is.na(data.pré$Q10[i]) & (data.pré$Q10[i]=="B" | data.pré$Q10[i]=="D" )){
    Conceptions[i,10]="I4"
  }
  else if (!is.na(data.pré$Q10[i]) & data.pré$Q10[i]=="A"){
    Conceptions[i,10]="Correct"
  }
  else if (!is.na(data.pré$Q10[i]) & data.pré$Q10[i]=="E"){
    Conceptions[i,10]="NA"
  }
  else if (!is.na(data.pré$Q10[i]) & data.pré$Q10[i]=="C"){
    Conceptions[i,10]="I3"
  }
  
  #Q11
  if (!is.na(data.pré$Q11[i]) & data.pré$Q11[i]=="A"){
    Conceptions[i,11]="OB,G1"
  }
  else if (!is.na(data.pré$Q11[i]) & data.pré$Q11[i]=="B"){
    Conceptions[i,11]="OB,I1"
  }
  else if (!is.na(data.pré$Q11[i]) & data.pré$Q11[i]=="C"){
    Conceptions[i,11]="I1"
  }
  else if (!is.na(data.pré$Q11[i]) & data.pré$Q11[i]=="D"){
    Conceptions[i,11]="Correct"
  }
  else if (!is.na(data.pré$Q11[i]) & data.pré$Q11[i]=="E"){
    Conceptions[i,11]="G2"
  }
  
  #12
  if (!is.na(data.pré$Q12[i]) & data.pré$Q12[i]=="A"){
    Conceptions[i,12]="CI2"
  }
  else if (!is.na(data.pré$Q12[i]) & data.pré$Q12[i]=="B"){
    Conceptions[i,12]="Correct"
  }
  else if (!is.na(data.pré$Q12[i]) & (data.pré$Q12[i]=="C" |data.pré$Q12[i]=="D" )){
    Conceptions[i,12]="I3"
  }
  else if (!is.na(data.pré$Q12[i]) & data.pré$Q12[i]=="E"){
    Conceptions[i,12]="G5"
  }
  
  #!Q13
  if (!is.na(data.pré$Q13[i]) & data.pré$Q13[i]=="D"){
    Conceptions[i,13]="Correct"
  }
  else if (!is.na(data.pré$Q13[i]) & (data.pré$Q13[i]=="A" |data.pré$Q13[i]=="B" |data.pré$Q13[i]=="C" )){
    Conceptions[i,13]="AF7/I3"
  }
  else if (!is.na(data.pré$Q13[i]) & data.pré$Q13[i]=="E"){
    Conceptions[i,13]="NA"
  }
  
  #Q14
  if (!is.na(data.pré$Q14[i]) & (data.pré$Q14[i]=="A" |data.pré$Q14[i]=="B")){
    Conceptions[i,14]="R1"
  }
  else if (!is.na(data.pré$Q14[i]) & data.pré$Q14[i]=="C"){
    Conceptions[i,14]="CI2"
  }
  else if (!is.na(data.pré$Q14[i]) & data.pré$Q14[i]=="D"){
    Conceptions[i,14]="Correct"
  }
  else if (!is.na(data.pré$Q14[i]) & data.pré$Q14[i]=="E"){
    Conceptions[i,14]="I3,G5"
  }
  
  #Q15
  if (!is.na(data.pré$Q15[i]) & data.pré$Q15[i]=="A"){
    Conceptions[i,15]="Correct"
  }
  else if (!is.na(data.pré$Q15[i]) & data.pré$Q15[i]=="B"){
    Conceptions[i,15]="AR1"
  }
  else if (!is.na(data.pré$Q15[i]) & data.pré$Q15[i]=="C"){
    Conceptions[i,15]="AR2"
  }
  else if (!is.na(data.pré$Q15[i]) & data.pré$Q15[i]=="D"){
    Conceptions[i,15]="AF1"
  }
  else if (!is.na(data.pré$Q15[i]) & data.pré$Q15[i]=="E"){
    Conceptions[i,15]="OB"
  }
  
  #Q16
  if (!is.na(data.pré$Q16[i]) & data.pré$Q16[i]=="A"){
    Conceptions[i,16]="Correct"
  }
  else if (!is.na(data.pré$Q16[i]) & data.pré$Q16[i]=="B"){
    Conceptions[i,16]="AR1"
  }
  else if (!is.na(data.pré$Q16[i]) & data.pré$Q16[i]=="C"){
    Conceptions[i,16]="AR2"
  }
  else if (!is.na(data.pré$Q16[i]) & data.pré$Q16[i]=="D"){
    Conceptions[i,16]="AF1"
  }
  else if (!is.na(data.pré$Q16[i]) & data.pré$Q16[i]=="E"){
    Conceptions[i,15]="OB"
  }
  
  #Q17
  if (!is.na(data.pré$Q17[i]) & data.pré$Q17[i]=="A"){
    Conceptions[i,17]="CI1"
  }
  else if (!is.na(data.pré$Q17[i]) & data.pré$Q17[i]=="B"){
    Conceptions[i,17]="Correct"
  }
  else if (!is.na(data.pré$Q17[i]) & data.pré$Q17[i]=="C"){
    Conceptions[i,17]="NA"
  }
  else if (!is.na(data.pré$Q17[i]) & data.pré$Q17[i]=="D"){
    Conceptions[i,17]="CI1,G1"
  }
  else if (!is.na(data.pré$Q17[i]) & data.pré$Q17[i]=="E"){
    Conceptions[i,17]="AF1"
  }
  
  
  #!Q18
  if (!is.na(data.pré$Q18[i]) & data.pré$Q18[i]=="A"){
    Conceptions[i,18]="OB"
  }
  else if (!is.na(data.pré$Q18[i]) & data.pré$Q18[i]=="B"){
    Conceptions[i,18]="Correct"
  }
  else if (!is.na(data.pré$Q18[i]) & data.pré$Q18[i]=="C"){
    Conceptions[i,18]="AF2"
  }
  else if (!is.na(data.pré$Q18[i]) & data.pré$Q18[i]=="D"){
    Conceptions[i,18]="AF2,OB"
  }
  else if (!is.na(data.pré$Q18[i]) & data.pré$Q18[i]=="E"){
    Conceptions[i,18]="AF2,CF"
  }
  
  #Q19
  if (!is.na(data.pré$Q19[i]) & data.pré$Q19[i]=="A"){
    Conceptions[i,19]="K2"
  }
  else if (!is.na(data.pré$Q19[i]) & (data.pré$Q19[i]=="B" | data.pré$Q19[i]=="C" |data.pré$Q19[i]=="D")){
    Conceptions[i,19]="K1"
  }
  else if (!is.na(data.pré$Q19[i]) & data.pré$Q19[i]=="E"){
    Conceptions[i,19]="Correct"
  }
  
  #Q20
  if (!is.na(data.pré$Q20[i]) & data.pré$Q20[i]=="D"){
    Conceptions[i,20]="Correct"
  }
  else if (!is.na(data.pré$Q20[i]) & (data.pré$Q20[i]=="B" |data.pré$Q20[i]=="C")){
    Conceptions[i,20]="K2"
  }
  else if (!is.na(data.pré$Q20[i]) & (data.pré$Q20[i]=="A" |data.pré$Q20[i]=="E")){
    Conceptions[i,20]="NA"
  }
  
  #Q21
  if (!is.na(data.pré$Q21[i]) & data.pré$Q21[i]=="A"){
    Conceptions[i,21]="I2"
  }
  else if (!is.na(data.pré$Q21[i]) & data.pré$Q21[i]=="B"){
    Conceptions[i,21]="CI3"
  }
  else if (!is.na(data.pré$Q21[i]) & data.pré$Q21[i]=="C"){
    Conceptions[i,21]="CI2"
  }
  else if (!is.na(data.pré$Q21[i]) & data.pré$Q21[i]=="D"){
    Conceptions[i,21]="I4"
  }
  else if (!is.na(data.pré$Q21[i]) & data.pré$Q21[i]=="E"){
    Conceptions[i,21]="Correct"
  }
  
  #Q22
  if (!is.na(data.pré$Q22[i]) & data.pré$Q22[i]=="A"){
    Conceptions[i,22]="AF4"
  }
  else if (!is.na(data.pré$Q22[i]) & data.pré$Q22[i]=="B"){
    Conceptions[i,22]="Correct"
  }
  else if (!is.na(data.pré$Q22[i]) & (data.pré$Q22[i]=="C"|data.pré$Q22[i]=="E")){
    Conceptions[i,22]="AF7"
  }
  else if (!is.na(data.pré$Q22[i]) & data.pré$Q22[i]=="D"){
    Conceptions[i,22]="AF6"
  }
  
  #Q23
  if (!is.na(data.pré$Q23[i]) & data.pré$Q23[i]=="B"){
    Conceptions[i,23]="Correct"
  }
  else if (!is.na(data.pré$Q23[i]) & (data.pré$Q23[i]=="A"|data.pré$Q23[i]=="D"|data.pré$Q23[i]=="E")){
    Conceptions[i,23]="I2"
  }
  else if (!is.na(data.pré$Q23[i]) & data.pré$Q23[i]=="C"){
    Conceptions[i,23]="CI3"
  }
  
  #Q24
  if (!is.na(data.pré$Q24[i]) & data.pré$Q24[i]=="A"){
    Conceptions[i,24]="Correct"
  }
  else if (!is.na(data.pré$Q24[i]) & (data.pré$Q24[i]=="C"|data.pré$Q24[i]=="E")){
    Conceptions[i,24]="I3"
  }
  else if (!is.na(data.pré$Q24[i]) & (data.pré$Q24[i]=="B"|data.pré$Q24[i]=="D")){
    Conceptions[i,24]="NA"
  }
  
  #Q25
  if (!is.na(data.pré$Q25[i]) & data.pré$Q25[i]=="A"){
    Conceptions[i,25]="AF4"
  }
  else if (!is.na(data.pré$Q25[i]) & data.pré$Q25[i]=="C"){
    Conceptions[i,25]="Correct"
  }
  else if (!is.na(data.pré$Q25[i]) & (data.pré$Q25[i]=="B"|data.pré$Q25[i]=="D")){
    Conceptions[i,25]="R2"
  }
  else if (!is.na(data.pré$Q25[i]) & data.pré$Q25[i]=="E"){
    Conceptions[i,25]="R3"
  }
  
  #!Q26
  if (!is.na(data.pré$Q26[i]) & (data.pré$Q26[i]=="A"| data.pré$Q26[i]=="B")){
    Conceptions[i,26]="AF4"
  }
  else if (!is.na(data.pré$Q26[i]) & data.pré$Q26[i]=="E"){
    Conceptions[i,26]="Correct"
  }
  else if (!is.na(data.pré$Q26[i]) & data.pré$Q26[i]=="C"){
    Conceptions[i,26]="NA"
  }
  else if (!is.na(data.pré$Q26[i]) & data.pré$Q26[i]=="D"){
    Conceptions[i,26]="AF6"
  }
  
  #Q27
  if (!is.na(data.pré$Q27[i]) & data.pré$Q27[i]=="A"){
    Conceptions[i,27]="AF2,R1"
  }
  else if (!is.na(data.pré$Q27[i]) & data.pré$Q27[i]=="B"){
    Conceptions[i,27]="I3,R1"
  }
  else if (!is.na(data.pré$Q27[i]) & data.pré$Q27[i]=="C"){
    Conceptions[i,27]="Correct"
  }
  else if (!is.na(data.pré$Q27[i]) & data.pré$Q27[i]=="D"){
    Conceptions[i,27]="I1"
  }
  else if (!is.na(data.pré$Q27[i]) & data.pré$Q27[i]=="E"){
    Conceptions[i,27]="I4"
  }
  
  #Q28
  if (!is.na(data.pré$Q28[i]) & data.pré$Q28[i]=="E"){
    Conceptions[i,28]="Correct"
  }
  else if (!is.na(data.pré$Q28[i]) & (data.pré$Q28[i]=="A" |data.pré$Q28[i]=="C" )){
    Conceptions[i,28]="NA"
  }
  else if (!is.na(data.pré$Q28[i]) & data.pré$Q28[i]=="D"){
    Conceptions[i,28]="AR1,AR2"
  }
  else if (!is.na(data.pré$Q28[i]) & data.pré$Q28[i]=="B"){
    Conceptions[i,28]="AF1"
  }
  
  #Q29
  if (!is.na(data.pré$Q29[i]) & data.pré$Q29[i]=="A"){
    Conceptions[i,29]="OB"
  }
  else if (!is.na(data.pré$Q29[i]) & data.pré$Q29[i]=="B"){
    Conceptions[i,29]="AF1"
  }
  else if (!is.na(data.pré$Q29[i]) & data.pré$Q29[i]=="C"){
    Conceptions[i,29]="G1"
  }
  else if (!is.na(data.pré$Q29[i]) & data.pré$Q29[i]=="D"){
    Conceptions[i,29]="Correct"
  }
  else if (!is.na(data.pré$Q29[i]) & data.pré$Q29[i]=="E"){
    Conceptions[i,29]="AF3"
  }
  
  #Q30
  if (!is.na(data.pré$Q30[i]) & data.pré$Q30[i]=="A"){
    Conceptions[i,30]="AF1"
  }
  else if (!is.na(data.pré$Q30[i]) &(data.pré$Q30[i]=="B"|data.pré$Q30[i]=="D"|data.pré$Q30[i]=="E")){
    Conceptions[i,30]="I1"
  }
  else if (!is.na(data.pré$Q30[i]) & data.pré$Q30[i]=="A"){
    Conceptions[i,30]="AF1"
  }
  else if (!is.na(data.pré$Q30[i]) & data.pré$Q30[i]=="C"){
    Conceptions[i,30]="Correct"
  }
}
for(i in 1:nrow(data.pré[,])){
  for(j in 1:ncol(data.pré[,])){
       if(is.na(data.pré[i,j])==TRUE){
    Conceptions[i,j]="No answer"
       }
  }
}

for(i in 1:nrow(data.post[,])){
  if ( !is.na(data.post$Q1.[i]) & (data.post$Q1.[i]=="A" | data.post$Q1.[i]=="D" )){
    Conceptions1[i,1]="G3"
  }
  else if (!is.na(data.post$Q1.[i]) & (data.post$Q1.[i]=="B" | data.post$Q1.[i]=="E") ){
    Conceptions1[i,1]="G6"
  }
  else if (!is.na(data.post$Q1.[i]) & data.post$Q1.[i]=="C"){
    Conceptions1[i,1]="Correct"
  }
  
  #Q2
  if (!is.na(data.post$Q2.[i]) & (data.post$Q2.[i]=="B" | data.post$Q2.[i]=="D") ){
    Conceptions1[i,2]="G3"
  }
  else if (!is.na(data.post$Q2.[i]) & data.post$Q2.[i]=="A" ){
    Conceptions1[i,2]="Correct"
  }
  else if (!is.na(data.post$Q2.[i]) & (data.post$Q2.[i]=="C" | data.post$Q2.[i]=="E") ){
    Conceptions1[i,2]="NA"
  }
  else if(is.na(data.post$Q2.[i])==TRUE){
    Conceptions1[i,2]="0"
  }
  
  
  #Q3
  if (!is.na(data.post$Q3.[i]) & data.post$Q3.[i]=="A"){
    Conceptions1[i,3]="AF6"
  }
  else if (!is.na(data.post$Q3.[i]) & data.post$Q3.[i]=="B"){
    Conceptions1[i,3]="AF5,G4"
  }
  else if (!is.na(data.post$Q3.[i]) & data.post$Q3.[i]=="C"){
    Conceptions1[i,3]="Correct"
  }
  else if (!is.na(data.post$Q3.[i]) & data.post$Q3.[i]=="D"){
    Conceptions1[i,3]="G2"
  }
  else if (!is.na(data.post$Q3.[i]) & data.post$Q3.[i]=="E"){
    Conceptions1[i,3]="G1"
  }
  
  #Q4
  if (!is.na(data.post$Q4.[i]) & (data.post$Q4.[i]=="A" | data.post$Q4.[i]=="D") ){
    Conceptions1[i,4]="AR1"
  }
  else if (!is.na(data.post$Q4.[i]) & data.post$Q4.[i]=="E"  ){
    Conceptions1[i,4]="Correcte"
  }
  else if (!is.na(data.post$Q4.[i]) & data.post$Q4.[i]=="B"  ){
    Conceptions1[i,4]="NA"
  }
  else if (!is.na(data.post$Q4.[i]) & data.post$Q4.[i]=="C"  ){
    Conceptions1[i,4]="OB"
  }
  
  #!Q5
  if (!is.na(data.post$Q5.[i]) & (data.post$Q5.[i]=="C" | data.post$Q5.[i]=="D") ){
    Conceptions1[i,5]="AR1"
  }
  else if (!is.na(data.post$Q5.[i]) & data.post$Q5.[i]=="E"  ){
    Conceptions1[i,5]="CF,AF2"
  }
  else if (!is.na(data.post$Q5.[i]) & data.post$Q5.[i]=="B"  ){
    Conceptions1[i,5]="Correct"
  }
  else if (!is.na(data.post$Q5.[i]) & data.post$Q5.[i]=="A"  ){
    Conceptions1[i,5]="NA"
  }
  
  #Q6
  if (!is.na(data.post$Q6.[i]) & (data.post$Q6.[i]=="C" | data.post$Q6.[i]=="E") ){
    Conceptions1[i,6]="CF"
  }
  else if (!is.na(data.post$Q6.[i]) & data.post$Q6.[i]=="A"  ){
    Conceptions1[i,6]="I5"
  }
  else if (!is.na(data.post$Q6.[i]) & data.post$Q6.[i]=="B"  ){
    Conceptions1[i,6]="Correct"
  }
  else if (!is.na(data.post$Q6.[i]) & data.post$Q6.[i]=="D"  ){
    Conceptions1[i,6]="CF,CI2"
  }
  
  #Q7
  if (!is.na(data.post$Q7.[i]) & data.post$Q7.[i]=="A"  ){
    Conceptions1[i,7]="I5"
  }
  else if (!is.na(data.post$Q7.[i]) & data.post$Q7.[i]=="B"  ){
    Conceptions1[i,7]="Correct"
  }
  else if (!is.na(data.post$Q7.[i]) & data.post$Q7.[i]=="C"  ){
    Conceptions1[i,7]="CI2,CF"
  }
  else if (!is.na(data.post$Q7.[i]) & data.post$Q7.[i]=="D"  ){
    Conceptions1[i,7]="I2,I5,CF"
  }
  else if (!is.na(data.post$Q7.[i]) & data.post$Q7.[i]=="E"  ){
    Conceptions1[i,7]="CF"
  }
  
  #Q8
  if (!is.na(data.post$Q8.[i]) & data.post$Q8.[i]=="A"  ){
    Conceptions1[i,8]="CI3"
  }
  else if (!is.na(data.post$Q8.[i]) & data.post$Q8.[i]=="B"  ){
    Conceptions1[i,8]="Correct"
  }
  else if (!is.na(data.post$Q8.[i]) & (data.post$Q8.[i]=="C" | data.post$Q8.[i]=="E" )){
    Conceptions1[i,8]="I2"
  }
  else if (!is.na(data.post$Q8.[i]) & data.post$Q8.[i]=="D"){
    Conceptions1[i,8]="I4"
  }
  
  #Q9
  if (!is.na(data.post$Q9.[i]) & (data.post$Q9.[i]=="A" | data.post$Q9.[i]=="D" )){
    Conceptions1[i,9]="NA"
  }
  else if (!is.na(data.post$Q9.[i]) & data.post$Q9.[i]=="B"){
    Conceptions1[i,9]="CI3"
  }
  else if (!is.na(data.post$Q9.[i]) & data.post$Q9.[i]=="C"){
    Conceptions1[i,9]="K3"
  }
  else if (!is.na(data.post$Q9.[i]) & data.post$Q9.[i]=="E"){
    Conceptions1[i,9]="Correct"
  }
  
  #Q10
  if (!is.na(data.post$Q10.[i]) & (data.post$Q10.[i]=="B" | data.post$Q10.[i]=="D" )){
    Conceptions1[i,10]="I4"
  }
  else if (!is.na(data.post$Q10.[i]) & data.post$Q10.[i]=="A"){
    Conceptions1[i,10]="Correct"
  }
  else if (!is.na(data.post$Q10.[i]) & data.post$Q10.[i]=="E"){
    Conceptions1[i,10]="NA"
  }
  else if (!is.na(data.post$Q10.[i]) & data.post$Q10.[i]=="C"){
    Conceptions1[i,10]="I3"
  }
  
  #Q11
  if (!is.na(data.post$Q11.[i]) & data.post$Q11.[i]=="A"){
    Conceptions1[i,11]="OB,G1"
  }
  else if (!is.na(data.post$Q11.[i]) & data.post$Q11.[i]=="B"){
    Conceptions1[i,11]="OB,I1"
  }
  else if (!is.na(data.post$Q11.[i]) & data.post$Q11.[i]=="C"){
    Conceptions1[i,11]="I1"
  }
  else if (!is.na(data.post$Q11.[i]) & data.post$Q11.[i]=="D"){
    Conceptions1[i,11]="Correct"
  }
  else if (!is.na(data.post$Q11.[i]) & data.post$Q11.[i]=="E"){
    Conceptions1[i,11]="G2"
  }
  
  #12
  if (!is.na(data.post$Q12.[i]) & data.post$Q12.[i]=="A"){
    Conceptions1[i,12]="CI2"
  }
  else if (!is.na(data.post$Q12.[i]) & data.post$Q12.[i]=="B"){
    Conceptions1[i,12]="Correct"
  }
  else if (!is.na(data.post$Q12.[i]) & (data.post$Q12.[i]=="C" |data.post$Q12.[i]=="D" )){
    Conceptions1[i,12]="I3"
  }
  else if (!is.na(data.post$Q12.[i]) & data.post$Q12.[i]=="E"){
    Conceptions1[i,12]="G5"
  }
  
  #!Q13
  if (!is.na(data.post$Q13.[i]) & data.post$Q13.[i]=="D"){
    Conceptions1[i,13]="Correct"
  }
  else if (!is.na(data.post$Q13.[i]) & (data.post$Q13.[i]=="A" |data.post$Q13.[i]=="B" |data.post$Q13.[i]=="C" )){
    Conceptions1[i,13]="AF7/I3"
  }
  else if (!is.na(data.post$Q13.[i]) & data.post$Q13.[i]=="E"){
    Conceptions1[i,13]="NA"
  }
  
  #Q14
  if (!is.na(data.post$Q14.[i]) & (data.post$Q14.[i]=="A" |data.post$Q14.[i]=="B")){
    Conceptions1[i,14]="R1"
  }
  else if (!is.na(data.post$Q14.[i]) & data.post$Q14.[i]=="C"){
    Conceptions1[i,14]="CI2"
  }
  else if (!is.na(data.post$Q14.[i]) & data.post$Q14.[i]=="D"){
    Conceptions1[i,14]="Correct"
  }
  else if (!is.na(data.post$Q14.[i]) & data.post$Q14.[i]=="E"){
    Conceptions1[i,14]="I3,G5"
  }
  
  #Q15
  if (!is.na(data.post$Q15.[i]) & data.post$Q15.[i]=="A"){
    Conceptions1[i,15]="Correct"
  }
  else if (!is.na(data.post$Q15.[i]) & data.post$Q15.[i]=="B"){
    Conceptions1[i,15]="AR1"
  }
  else if (!is.na(data.post$Q15.[i]) & data.post$Q15.[i]=="C"){
    Conceptions1[i,15]="AR2"
  }
  else if (!is.na(data.post$Q15.[i]) & data.post$Q15.[i]=="D"){
    Conceptions1[i,15]="AF1"
  }
  else if (!is.na(data.post$Q15.[i]) & data.post$Q15.[i]=="E"){
    Conceptions1[i,15]="OB"
  }
  
  #Q16
  if (!is.na(data.post$Q16.[i]) & data.post$Q16.[i]=="A"){
    Conceptions1[i,16]="Correct"
  }
  else if (!is.na(data.post$Q16.[i]) & data.post$Q16.[i]=="B"){
    Conceptions1[i,16]="AR1"
  }
  else if (!is.na(data.post$Q16.[i]) & data.post$Q16.[i]=="C"){
    Conceptions1[i,16]="AR2"
  }
  else if (!is.na(data.post$Q16.[i]) & data.post$Q16.[i]=="D"){
    Conceptions1[i,16]="AF1"
  }
  else if (!is.na(data.post$Q16.[i]) & data.post$Q16.[i]=="E"){
    Conceptions1[i,15]="OB"
  }
  
  #Q17
  if (!is.na(data.post$Q17.[i]) & data.post$Q17.[i]=="A"){
    Conceptions1[i,17]="CI1"
  }
  else if (!is.na(data.post$Q17.[i]) & data.post$Q17.[i]=="B"){
    Conceptions1[i,17]="Correct"
  }
  else if (!is.na(data.post$Q17.[i]) & data.post$Q17.[i]=="C"){
    Conceptions1[i,17]="NA"
  }
  else if (!is.na(data.post$Q17.[i]) & data.post$Q17.[i]=="D"){
    Conceptions1[i,17]="CI1,G1"
  }
  else if (!is.na(data.post$Q17.[i]) & data.post$Q17.[i]=="E"){
    Conceptions1[i,17]="AF1"
  }
  
  
  #!Q18
  if (!is.na(data.post$Q18.[i]) & data.post$Q18.[i]=="A"){
    Conceptions1[i,18]="OB"
  }
  else if (!is.na(data.post$Q18.[i]) & data.post$Q18.[i]=="B"){
    Conceptions1[i,18]="Correct"
  }
  else if (!is.na(data.post$Q18.[i]) & data.post$Q18.[i]=="C"){
    Conceptions1[i,18]="AF2"
  }
  else if (!is.na(data.post$Q18.[i]) & data.post$Q18.[i]=="D"){
    Conceptions1[i,18]="AF2,OB"
  }
  else if (!is.na(data.post$Q18.[i]) & data.post$Q18.[i]=="E"){
    Conceptions1[i,18]="AF2,CF"
  }
  
  #Q19
  if (!is.na(data.post$Q19.[i]) & data.post$Q19.[i]=="A"){
    Conceptions1[i,19]="K2"
  }
  else if (!is.na(data.post$Q19.[i]) & (data.post$Q19.[i]=="B" | data.post$Q19.[i]=="C" |data.post$Q19.[i]=="D")){
    Conceptions1[i,19]="K1"
  }
  else if (!is.na(data.post$Q19.[i]) & data.post$Q19.[i]=="E"){
    Conceptions1[i,19]="Correct"
  }
  
  #Q20
  if (!is.na(data.post$Q20.[i]) & data.post$Q20.[i]=="D"){
    Conceptions1[i,20]="Correct"
  }
  else if (!is.na(data.post$Q20.[i]) & (data.post$Q20.[i]=="B" |data.post$Q20.[i]=="C")){
    Conceptions1[i,20]="K2"
  }
  else if (!is.na(data.post$Q20.[i]) & (data.post$Q20.[i]=="A" |data.post$Q20.[i]=="E")){
    Conceptions1[i,20]="NA"
  }
  
  #Q21
  if (!is.na(data.post$Q21.[i]) & data.post$Q21.[i]=="A"){
    Conceptions1[i,21]="I2"
  }
  else if (!is.na(data.post$Q21.[i]) & data.post$Q21.[i]=="B"){
    Conceptions1[i,21]="CI3"
  }
  else if (!is.na(data.post$Q21.[i]) & data.post$Q21.[i]=="C"){
    Conceptions1[i,21]="CI2"
  }
  else if (!is.na(data.post$Q21.[i]) & data.post$Q21.[i]=="D"){
    Conceptions1[i,21]="I4"
  }
  else if (!is.na(data.post$Q21.[i]) & data.post$Q21.[i]=="E"){
    Conceptions1[i,21]="Correct"
  }
  
  #Q22
  if (!is.na(data.post$Q22.[i]) & data.post$Q22.[i]=="A"){
    Conceptions1[i,22]="AF4"
  }
  else if (!is.na(data.post$Q22.[i]) & data.post$Q22.[i]=="B"){
    Conceptions1[i,22]="Correct"
  }
  else if (!is.na(data.post$Q22.[i]) & (data.post$Q22.[i]=="C"|data.post$Q22.[i]=="E")){
    Conceptions1[i,22]="AF7"
  }
  else if (!is.na(data.post$Q22.[i]) & data.post$Q22.[i]=="D"){
    Conceptions1[i,22]="AF6"
  }
  
  #Q23
  if (!is.na(data.post$Q23.[i]) & data.post$Q23.[i]=="B"){
    Conceptions1[i,23]="Correct"
  }
  else if (!is.na(data.post$Q23.[i]) & (data.post$Q23.[i]=="A"|data.post$Q23.[i]=="D"|data.post$Q23.[i]=="E")){
    Conceptions1[i,23]="I2"
  }
  else if (!is.na(data.post$Q23.[i]) & data.post$Q23.[i]=="C"){
    Conceptions1[i,23]="CI3"
  }
  
  #Q24
  if (!is.na(data.post$Q24.[i]) & data.post$Q24.[i]=="A"){
    Conceptions1[i,24]="Correct"
  }
  else if (!is.na(data.post$Q24.[i]) & (data.post$Q24.[i]=="C"|data.post$Q24.[i]=="E")){
    Conceptions1[i,24]="I3"
  }
  else if (!is.na(data.post$Q24.[i]) & (data.post$Q24.[i]=="B"|data.post$Q24.[i]=="D")){
    Conceptions1[i,24]="NA"
  }
  
  #Q25
  if (!is.na(data.post$Q25.[i]) & data.post$Q25.[i]=="A"){
    Conceptions1[i,25]="AF4"
  }
  else if (!is.na(data.post$Q25.[i]) & data.post$Q25.[i]=="C"){
    Conceptions1[i,25]="Correct"
  }
  else if (!is.na(data.post$Q25.[i]) & (data.post$Q25.[i]=="B"|data.post$Q25.[i]=="D")){
    Conceptions1[i,25]="R2"
  }
  else if (!is.na(data.post$Q25.[i]) & data.post$Q25.[i]=="E"){
    Conceptions1[i,25]="R3"
  }
  
  #!Q26
  if (!is.na(data.post$Q26.[i]) & (data.post$Q26.[i]=="A"| data.post$Q26.[i]=="B")){
    Conceptions1[i,26]="AF4"
  }
  else if (!is.na(data.post$Q26.[i]) & data.post$Q26.[i]=="E"){
    Conceptions1[i,26]="Correct"
  }
  else if (!is.na(data.post$Q26.[i]) & data.post$Q26.[i]=="C"){
    Conceptions1[i,26]="NA"
  }
  else if (!is.na(data.post$Q26.[i]) & data.post$Q26.[i]=="D"){
    Conceptions1[i,26]="AF6"
  }
  
  #Q27
  if (!is.na(data.post$Q27.[i]) & data.post$Q27.[i]=="A"){
    Conceptions1[i,27]="AF2,R1"
  }
  else if (!is.na(data.post$Q27.[i]) & data.post$Q27.[i]=="B"){
    Conceptions1[i,27]="I3,R1"
  }
  else if (!is.na(data.post$Q27.[i]) & data.post$Q27.[i]=="C"){
    Conceptions1[i,27]="Correct"
  }
  else if (!is.na(data.post$Q27.[i]) & data.post$Q27.[i]=="D"){
    Conceptions1[i,27]="I1"
  }
  else if (!is.na(data.post$Q27.[i]) & data.post$Q27.[i]=="E"){
    Conceptions1[i,27]="I4"
  }
  
  #Q28
  if (!is.na(data.post$Q28.[i]) & data.post$Q28.[i]=="E"){
    Conceptions1[i,28]="Correct"
  }
  else if (!is.na(data.post$Q28.[i]) & (data.post$Q28.[i]=="A" |data.post$Q28.[i]=="C" )){
    Conceptions1[i,28]="NA"
  }
  else if (!is.na(data.post$Q28.[i]) & data.post$Q28.[i]=="D"){
    Conceptions1[i,28]="AR1,AR2"
  }
  else if (!is.na(data.post$Q28.[i]) & data.post$Q28.[i]=="B"){
    Conceptions1[i,28]="AF1"
  }
  
  #Q29
  if (!is.na(data.post$Q29.[i]) & data.post$Q29.[i]=="A"){
    Conceptions1[i,29]="OB"
  }
  else if (!is.na(data.post$Q29.[i]) & data.post$Q29.[i]=="B"){
    Conceptions1[i,29]="AF1"
  }
  else if (!is.na(data.post$Q29.[i]) & data.post$Q29.[i]=="C"){
    Conceptions1[i,29]="G1"
  }
  else if (!is.na(data.post$Q29.[i]) & data.post$Q29.[i]=="D"){
    Conceptions1[i,29]="Correct"
  }
  else if (!is.na(data.post$Q29.[i]) & data.post$Q29[i]=="E"){
    Conceptions1[i,29]="AF3"
  }
  
  #Q30
  if (!is.na(data.post$Q30.[i]) & data.post$Q30.[i]=="A"){
    Conceptions1[i,30]="AF1"
  } 
  else if (!is.na(data.post$Q30.[i]) & data.post$Q30.[i]=="c"){
    Conceptions1[i,30]="Correct"
  }
  else if (!is.na(data.post$Q30.[i]) &(data.post$Q30.[i]=="B"|data.post$Q30.[i]=="D"|data.post$Q30.[i]=="E")){
    Conceptions1[i,30]="I1"
  }
 
}
for(i in 1:nrow(data.post[,])){
  for(j in 1:ncol(data.post[,])){
    if(is.na(data.post[i,j])==TRUE){
      Conceptions1[i,j]="No answer"
    }
  }
}


  ##Graphe distribution des résultats par question en fonction des préconception
  ## Les facteurs entre le pré (Conceptions) et le post(Conceptions1) sont parfois différents.
  ## C'est pourquoi j'utilise la matrice M après avoir afficher Conception et Conception1 pour reconstruire qql chose de cohérent avec tt le facteurs


M<-matrix(c(4,2,25,1,3,15,15,0,25,0,2,8),nrow=2,byrow=TRUE, 
          dimnames = list(NULL,c("Correct","G2","I1","no answer","OB,G1","OB,I1")))
print(M)

test<-rbind(table(Conceptions[,3]),table(Conceptions1[,3]))
print(test)
#Distribution des préconceptions pour la question i (à partir de la matrice test)
bar<-barplot(100*test/37, beside = TRUE,ylab="pourcentage de réponse",ylim=c(0,100),
        main="Q3 (garçons N=37) ",col=c("skyblue1","skyblue4"))
text(x=bar+0.2,y=100*test/37+3.5,adj = 1, xpd = TRUE, labels =signif(100*test/37,2))

legend("topright",c("pré-test","post-test"),
       cex = 0.6, fill =c("skyblue1","skyblue4"))
#Distribution des préconceptions pour la question i (à partir de la matrice M)
bar<-barplot(100*M/50, beside = TRUE,ylab="pourcentage de réponse",ylim=c(0,100),
             main="Q11 (avec WCST N=50)",col=c("darkseagreen1","darkseagreen4"))
text(x=bar+0.2,y=100*M/50+3.5,adj = 1, xpd = TRUE, labels =signif(100*M/50,2))

legend("topright",c("pré-test","post-test"),
       cex = 0.6, fill = c("darkseagreen1","darkseagreen4"))

 #Différentes couleurs utilisées pour les graphes
#c("gray96","gray45") / c("rosybrown1","palevioletred4") / c("skyblue1","skyblue4") / c("darkseagreen1","darkseagreen4")
########################################concentration analysis ensemble des élèves##############################################
#Calcul taux de réponses correctes au pré-test
AbsPré<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CorrectPré<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WrongPré<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 9:38) {
  for (i in 1:nrow(data)) {
    if (is.na(data[i,j])==TRUE){
      AbsPré[j-8]=AbsPré[j-8]+1
    }
    else if (is.na(data[i,j])==FALSE & data[i,j]!=Items[j-8])
    {WrongPré[j-8]<-WrongPré[j-8]+1}
    else if (is.na(data[i,j])==FALSE & data[i,j]==Items[j-8]) 
    {CorrectPré[j-8]<-CorrectPré[j-8]+1}
  }
}

print(CorrectPré+WrongPré+AbsPré)

#Calcul taux de réponses correctes au post-test
AbsPost<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CorrectPost<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WrongPost<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 40:69) {
  for (i in 1:nrow(data)) {
    if (is.na(data[i,j])==TRUE){
      AbsPost[j-39]=AbsPost[j-39]+1
    }
    else if (is.na(data[i,j])==FALSE & data[i,j]!=Items[j-39])
    {WrongPost[j-39]<-WrongPost[j-39]+1}
    else if (is.na(data[i,j])==FALSE & data[i,j]==Items[j-39]) 
    {CorrectPost[j-39]<-CorrectPost[j-39]+1}
  }
}

print(CorrectPost+WrongPost+AbsPost)

#Calcul des taux des réponse item par item au pré
AbsPré<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
APré<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
BPré<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CPré<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
DPré<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
EPré<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 9:38) {
  for (i in 1:nrow(data)) {
    if (is.na(data[i,j])==TRUE){
      AbsPré[j-8]=AbsPré[j-8]+1
    }
    else if (is.na(data[i,j])==FALSE & data[i,j]=="A")
    {APré[j-8]<-APré[j-8]+1}
    else if (is.na(data[i,j])==FALSE  & data[i,j]=="B")
    {BPré[j-8]<-BPré[j-8]+1}
    else if (is.na(data[i,j])==FALSE  & data[i,j]=="C")
    {CPré[j-8]<-CPré[j-8]+1}
    else if (is.na(data[i,j])==FALSE & data[i,j]=="D")
    {DPré[j-8]<-DPré[j-8]+1}
    else if (is.na(data[i,j])==FALSE & data[i,j]=="E")
    {EPré[j-8]<-EPré[j-8]+1}
  }
}
print(APré+BPré+CPré+DPré+EPré+AbsPré)

#Calcul des taux des réponse item par item au post
AbsPost<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
APost<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
BPost<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CPost<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
DPost<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
EPost<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 40:69) {
  for (i in 1:nrow(data)) {
    if (is.na(data[i,j])==TRUE){
      AbsPost[j-39]=AbsPost[j-39]+1
    }
    else if (is.na(data[i,j])==FALSE & data[i,j]=="A")
    {APost[j-39]<-APost[j-39]+1}
    else if (is.na(data[i,j])==FALSE & data[i,j]=="B")
    {BPost[j-39]<-BPost[j-39]+1}
    else if (is.na(data[i,j])==FALSE & data[i,j]=="C")
    {CPost[j-39]<-CPost[j-39]+1}
    else if (is.na(data[i,j])==FALSE & data[i,j]=="D")
    {DPost[j-39]<-DPost[j-39]+1}
    else if (is.na(data[i,j])==FALSE & data[i,j]=="E")
    {EPost[j-39]<-EPost[j-39]+1}
  }
}
AbsPost[25]=2
AbsPost[29]=5
print(APost+BPost+CPost+DPost+EPost+AbsPost)

#Calcul facteur de concentration et de déviation
N<-nrow(data) 
m<-5
#Vecteurs couleurs pour les points en fonction de la dimension (col.dim) et pour la légende (col.dim1)
col.dim<-c("gray45","cornflowerblue","chartreuse2","red2","darkgoldenrod1","palevioletred4","gray1")
col.dim1<-c("darkgoldenrod1","darkgoldenrod1","darkgoldenrod1","red2","darkgoldenrod1","cornflowerblue","cornflowerblue",
           "cornflowerblue","palevioletred4","cornflowerblue","darkgoldenrod1","gray45","darkgoldenrod1","gray45",
           "red2","red2","cornflowerblue","darkgoldenrod1","gray45","gray45","chartreuse2","chartreuse2","cornflowerblue",
           "cornflowerblue","cornflowerblue","chartreuse2","chartreuse2","red2","darkgoldenrod1","darkgoldenrod1")

 ##Calcul du facteur de concentration (attention, APré,...,EPré ne doivent pas être en pourcent)
C.factor.Pré<-c()
C.factor.Post<-c()
for (i in 1:30){
C.factor.Pré[i]<-sqrt(m)/(sqrt(m)-1)*(sqrt((APré[i])^2+(BPré[i])^2+(CPré[i])^2+(DPré[i])^2+(EPré[i])^2)/N-1/sqrt(m))
C.factor.Post[i]<-sqrt(m)/(sqrt(m)-1)*(sqrt((APost[i])^2+(BPost[i])^2+(CPost[i])^2+(DPost[i])^2+(EPost[i])^2)/N-1/sqrt(m))
}
##Calcul du facteur de déviation (attention, APré,...,EPré ne doivent pas être en pourcent)
Dev.factor.Pré<-c()
Dev.factor.Post<-c()
for (i in 1:30){
  Dev.factor.Pré[i]<-sqrt(m-1)/(sqrt(m-1)-1)*(sqrt((APré[i])^2+(BPré[i])^2+(CPré[i])^2+(DPré[i])^2+(EPré[i])^2-CorrectPré[i]^2)/(N-CorrectPré[i])-1/sqrt(m-1))
  Dev.factor.Post[i]<-sqrt(m-1)/(sqrt(m-1)-1)*(sqrt((APost[i])^2+(BPost[i])^2+(CPost[i])^2+(DPost[i])^2+(EPost[i])^2-CorrectPost[i]^2)/(N-CorrectPost[i])-1/sqrt(m-1))
}

Présence.Pré<- nrow(data)-AbsPré 
Présence.Post<- nrow(data)-AbsPost

CorrectPré<-CorrectPré/Présence.Pré
CorrectPost<-CorrectPost/Présence.Post

S.pré<-CorrectPré #compris entre 0 et 1
S.post<-CorrectPost #compris entre 0 et 1

#Numéro des questions ayant un score Low et un Dev élevé(Bao) 
Num.quest.L<-c()

for(i in 1:30){
  if (CorrectPré[i]<0.4 &Dev.factor.Pré[i]>=0.4){
    Num.quest.L[i]=i
  }
  else {
    Num.quest.L[i]=NA
  }
}

#Calculs score et facteur de déviation pour question de type Low

Dev.factor.Pré.L<-Dev.factor.Pré[is.na(Num.quest.L)==FALSE]
Dev.factor.Post.L<-Dev.factor.Post[is.na(Num.quest.L)==FALSE]
CorrectPré.L<-CorrectPré[is.na(Num.quest.L)==FALSE]
CorrectPost.L<-CorrectPost[is.na(Num.quest.L)==FALSE]

#Fonctions permettant de tracer les bornes inférieures et supérieures sur les graphes S-C

Cmax<-function(x){
  f<-(sqrt(m)/(sqrt(m)-1))*(sqrt((1-x)^2+x^2)-1/sqrt(m))
  f
}
Cmin<-function(x){
  f<-(sqrt(m)/(sqrt(m)-1))*(sqrt(4*((1-x)/4)^2+x^2)-1/sqrt(m))
  f
}

##Types (LL,LM,LH, etc) des 30 questions au FCI au prétest
Quest.type<-c()
for (i in 1:30){
  if (CorrectPré[i]<0.4 & C.factor.Pré[i]<0.2){
    Quest.type[i]="LL"
  }
  else if (CorrectPré[i]<0.4 & C.factor.Pré[i]>=0.2& C.factor.Pré[i]<0.5){
    Quest.type[i]="LM"
  }
  else if (CorrectPré[i]<0.4 & C.factor.Pré[i]>=0.2& C.factor.Pré[i]>=0.5){
    Quest.type[i]="LH"
  }
  else if (CorrectPré[i]>=0.4 & CorrectPré[i]<0.7 & C.factor.Pré[i]>=0.2& C.factor.Pré[i]<0.5){
    Quest.type[i]="MM"
  }
  else if (CorrectPré[i]>=0.4 & CorrectPré[i]<0.7& C.factor.Pré[i]<0.2){
    Quest.type[i]="ML"
  }
  else if (CorrectPré[i]>=0.7 & C.factor.Pré[i]>=0.2& C.factor.Pré[i]<0.5){
    Quest.type[i]="HM"
  }
  else if (CorrectPré[i]>=0.7 & C.factor.Pré[i]>=0.5){
    Quest.type[i]="HH"
  }
}
table(Quest.type)
##Types (LL,LM,LH, etc) des 30 questions au FCI au post-test
Quest.type<-c()
for (i in 1:30){
  if (CorrectPré[i]<0.4 & C.factor.Post[i]<0.2){
    Quest.type[i]="LL"
  }
  else if (CorrectPost[i]<0.4 & C.factor.Post[i]>=0.2& C.factor.Post[i]<0.5){
    Quest.type[i]="LM"
  }
  else if (CorrectPost[i]<0.4 & C.factor.Post[i]>=0.2& C.factor.Post[i]>=0.5){
    Quest.type[i]="LH"
  }
  else if (CorrectPost[i]>=0.4 & CorrectPost[i]<0.7 & C.factor.Post[i]>=0.2& C.factor.Post[i]<0.5){
    Quest.type[i]="MM"
  }
  else if (CorrectPost[i]>=0.4 & CorrectPost[i]<0.7& C.factor.Post[i]<0.2){
    Quest.type[i]="ML"
  }
  else if (CorrectPost[i]>=0.7 & C.factor.Post[i]>=0.2& C.factor.Post[i]<0.5){
    Quest.type[i]="HM"
  }
  else if (CorrectPost[i]>=0.7 & C.factor.Post[i]>=0.5){
    Quest.type[i]="HH"
  }
}
table(Quest.type)
  ##graphes concentration (S-C / S-Gamma (mémoire section 4.2))
par(mfrow = c(1,2))
## Graphe S-C
plot(C.factor.Pré~ CorrectPré,xlim=c(0,1),ylim=c(0,1),main="Graphe S-C",col="gray1",
     pch=2,ylab="Facteur de concentration C",xlab="Score par question",lwd=1.8)
#text(x=CorrectPré.H+0.02,y=C.factor.Pré.H,labels=c(1:30))
points(x=CorrectPost,y=C.factor.Post,pch=1,col="gray1",lwd=1.8)
#text(x=CorrectPost.H+0.02,y=C.factor.Post.H,labels=c(1:30))
abline(h=c(0.2,0.5),v=c(0.4,0.7), col="gray45")
lines(seq(from=0,to=1,by=0.001),Cmax(seq(from=0,to=1,by=0.001)),
      type = "l")
lines(seq(from=0,to=1,by=0.001),Cmin(seq(from=0,to=1,by=0.001)),
      type = "l")

points(x=c(mean(CorrectPré),mean(CorrectPost)),y=c(mean(C.factor.Pré),mean(C.factor.Post))
       ,pch=c(17,19),col="gray1",cex=1.5)
arrows(x0=mean(CorrectPré),x1=mean(CorrectPost),y0=mean(C.factor.Pré),y1=mean(C.factor.Post),
       code=2,lwd=1.5,length=0.15)
legend("topleft",c("pré-test","post-test","Moyenne pré-test","Moyenne post-test"),
       cex = 0.9, pch = c(2,1,17,19),col="gray1")

## Graphe S-Gamma 
plot(Dev.factor.Pré~ CorrectPré,xlim=c(0,1),ylim=c(0,1),main="Graphe S-" ~ Gamma  ,col="gray1",
     pch=2,ylab="Facteur de déviation"~ Gamma,xlab="Score par question",lwd=1.8)
text(x=CorrectPré+0.02,y=Dev.factor.Pré,labels=c(1:30))
points(x=CorrectPost,y=Dev.factor.Post,pch=1,col="gray1",lwd=1.8)
text(x=CorrectPost+0.02,y=Dev.factor.Post,labels=c(1:30))

points(x=c(mean(CorrectPré),mean(CorrectPost)),y=c(mean(Dev.factor.Pré),mean(Dev.factor.Post))
       ,pch=c(17,19),col="gray1",cex=1.5)
arrows(x0=mean(CorrectPré),x1=mean(CorrectPost),y0=mean(Dev.factor.Pré),y1=mean(Dev.factor.Post),
       code=2,lwd=1.5,length=0.15)
grid(nx=NULL, ny=NULL, col="gray19")
legend("topleft",c("pré-test","post-test","Moyenne pré-test","Moyenne post-test"),
       cex = 0.9, pch = c(2,1,17,19),col="gray1")

## graphe S-Gamma question de type Low

plot(Dev.factor.Pré.L~ CorrectPré.L,xlim=c(0,1),ylim=c(0,1),main="Graphe S-"* Gamma ~ "",
     col="gray1",pch=2,ylab="Facteur de déviation" ~Gamma,xlab="Score par question",lwd=1.8)
text(x=CorrectPré.L+0.02,y=Dev.factor.Pré.L,labels=Num.quest.L[is.na(Num.quest.L)==FALSE],cex=0.8)
points(x=CorrectPost.L,y=Dev.factor.Post.L,pch=1,col="gray1",lwd=1.8)
text(x=CorrectPost.L+0.02,y=Dev.factor.Post.L,labels=Num.quest.L[is.na(Num.quest.L)==FALSE],cex=0.8)

points(x=c(mean(CorrectPré.L),mean(CorrectPost.L)),y=c(mean(Dev.factor.Pré.L),mean(Dev.factor.Post.L))
       ,pch=c(17,19),col="gray1",cex=1.5)
arrows(x0=mean(CorrectPré.L),x1=mean(CorrectPost.L),y0=mean(Dev.factor.Pré.L),y1=mean(Dev.factor.Post.L),
       code=2,lwd=1.5,length=0.15)
for (i in 1:length(Num.quest.L[is.na(Num.quest.L)==FALSE])) {
  
  arrows(x0=CorrectPré.L[i],x1=CorrectPost.L[i],y0=Dev.factor.Pré.L[i],y1=Dev.factor.Post.L[i],
         code=2,lwd=0.5,length=0.15,lty=3,col="gray35")
}
grid(nx=NULL, ny=NULL, col="gray19")
legend("bottomright",c("pré-test","post-test","Moyenne pré-test","Moyenne post-test"),
       cex = 0.9, pch = c(2,1,17,19),col="gray1")



  ##anciens graphes colorés
#Graphe S-C pour le pré-test

par(mfrow = c(1,2))
plot(C.factor.pré~S.pré,xlim=c(0,1),ylim=c(0,1),main="Facteur de concentration en fonction du score 
obtenu par question au pré-test pour les garçons",col=col.dim1,pch=16,ylab="Facteur de concentration",
     xlab="Score par question au pré-test")
abline(h=c(0.2,0.5),v=c(0.4,0.7), col="gray45")
lines(seq(from=0,to=1,by=0.001),Cmax(seq(from=0,to=1,by=0.001)),
      type = "l")
lines(seq(from=0,to=1,by=0.001),Cmin(seq(from=0,to=1,by=0.001)),
      type = "l")
points(x=mean(S.pré),y=mean(C.factor.pré),pch=18,col="gray1",cex=1.5)
legend("bottomright",c("Cinématique","N1","N2","N3","Forces","Superposition","Moyenne"),
       cex = 0.75, fill = col.dim)

#Graphe S-C au post-test

plot(C.factor.post~S.post,xlim=c(0,1),ylim=c(0,1),main="Facteur de concentration en fonction du score 
obtenu par question au post-test pour les garçons",col=col.dim1,pch=16,ylab="Facteur de concentration",
     xlab="Score par question au post-test")
abline(h=c(0.2,0.5),v=c(0.4,0.7), col="gray45")
lines(seq(from=0,to=1,by=0.001),Cmax(seq(from=0,to=1,by=0.001)),
      type = "l")
lines(seq(from=0,to=1,by=0.001),Cmin(seq(from=0,to=1,by=0.001)),
      type = "l")
points(x=mean(S.post),y=mean(C.factor.post),pch=18,col="gray1",cex=1.5)
legend("bottomright",c("Cinématique","N1","N2","N3","Forces","Superposition","Moyenne"),
       cex = 0.75, fill = col.dim)

#Graphe S-Gamma pour le pré-test

par(mfrow = c(1,2))
plot(Dev.factor.Pré~S.pré,xlim=c(0,1),ylim=c(0,1),main="Concentration des réponses incorrectes en 
fonction du score obtenu par question au pré-test",col=col.dim1,pch=16,ylab="Facteur de concentration des réponses incorrectes",
     xlab="Score par question au pré-test")
grid(nx=NULL, ny=NULL, col="gray19")
points(x=mean(S.pré),y=mean(Dev.factor.Pré),pch=18,col="gray1",cex=1.5)
legend("topleft",c("Cinématique","N1","N2","N3","Forces","Superposition","Moyenne"),
       cex = 0.6, fill = col.dim)
text(x=S.pré+0.02,y=Dev.factor.Pré,labels=c(1:30),cex=0.7)

plot(Dev.factor.Post~S.post,xlim=c(0,1),ylim=c(0,1),main="Concentration des réponses incorrectes en 
fonction du score obtenu par question au post-test pour les garçons",col=col.dim1,pch=16,ylab="Facteur de concentration des réponses incorrectes",
     xlab="Score par question au post-test")
grid(nx=NULL, ny=NULL, col="gray19")
points(x=mean(S.post),y=mean(Dev.factor.Post),pch=18,col="gray1",cex=1.5)
legend("topleft",c("Cinématique","N1","N2","N3","Forces","Superposition","Moyenne"),
       cex = 0.6, fill = col.dim)
text(x=S.post+0.02,y=Dev.factor.Post,labels=c(1:30),cex=0.7)

#Graphe Gain-Gamma
g<-((S.post-S.pré)/(1-S.pré))
plot(Dev.factor.Pré~g,xlim=c(-0.2,0.8),ylim=c(0,1),main="Concentration des réponses incorrectes en 
fonction du gain normalisé",col=col.dim1,pch=16,ylab="Facteur de concentration des réponses incorrectes au pré",
     xlab="gain normalisé par question")
grid(nx=NULL, ny=NULL, col="gray19")
points(x=mean(g),y=mean(Dev.factor.Pré),pch=18,col="gray1",cex=1.5)
legend("topleft",c("Cinématique","N1","N2","N3","Forces","Superposition","Moyenne"),
       cex = 0.6, fill = col.dim)
text(x=g+0.02,y=Dev.factor.Pré,labels=c(1:30),cex=0.7)

Dev.factor.norm<-(Dev.factor.Post-Dev.factor.Pré)/(1-Dev.factor.Pré)
plot(Dev.factor.norm~g,xlim=c(-0.2,0.6),ylim=c(-0.2,0.4),main="Concentration des réponses incorrectes en 
fonction du gain normalisé",col=col.dim1,pch=16,ylab="Facteur de concentration des réponses incorrectes au pré",
     xlab="gain normalisé par question")
grid(nx=NULL, ny=NULL, col="gray19")
points(x=mean(g),y=mean(Dev.factor.norm),pch=18,col="gray1",cex=1.5)
legend("topleft",c("Cinématique","N1","N2","N3","Forces","Superposition","Moyenne"),
       cex = 0.6, fill = col.dim)
text(x=g+0.02,y=Dev.factor.Post-Dev.factor.Pré,labels=c(1:30),cex=0.7)

cor.test(g,Dev.factor.norm,alternative="two.sided",method="kendall")

##Graphe corrélayion facteur g et facteur gamma-g
library(ggplot2)
library("ggpubr")
data.norm<-cbind(g,Dev.factor.norm)
data.norm<-as.data.frame(data.norm)
ggscatter(data.norm, x = "g", y = "Dev.factor.norm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson") + 
  ggtitle("Gain normalisé Gamma-g en fonction du gain normalisé FCI") +
  xlab("gain normalisé g") + ylab("Gamma-g")+grids(linetype="solid")

cor.test(data.norm$g, data.norm$Dev.factor.norm,method="pearson")
cor.test(data.norm$g, data.norm$Dev.factor.norm,method="kendall")
cor.test(data.norm$g, data.norm$Dev.factor.norm,method="spearman")
########################################### Concentration analysis par dimension##############################

#Création des facteur C et gamma par dimension à partir du facteur C et gamma total calculé au-dessus

C.factor.pré.Cin<-c(C.factor.pré[12],C.factor.pré[14],C.factor.pré[19],C.factor.pré[20])
C.factor.post.Cin<-c(C.factor.post[12],C.factor.post[14],C.factor.post[19],C.factor.post[20])

C.factor.pré.N1<-c(C.factor.pré[6],C.factor.pré[7],C.factor.pré[8],C.factor.pré[10],C.factor.pré[17],
                   C.factor.pré[23],C.factor.pré[24],C.factor.pré[25])
C.factor.post.N1<-c(C.factor.post[6],C.factor.post[7],C.factor.post[8],C.factor.post[10],C.factor.post[17],
                   C.factor.post[23],C.factor.post[24],C.factor.post[25])

C.factor.pré.N2<-c(C.factor.pré[21],C.factor.pré[22],C.factor.pré[26],C.factor.pré[27])
C.factor.post.N2<-c(C.factor.post[21],C.factor.post[22],C.factor.post[26],C.factor.post[27])

C.factor.pré.N3<-c(C.factor.pré[4],C.factor.pré[15],C.factor.pré[16],C.factor.pré[28])
C.factor.post.N3<-c(C.factor.post[4],C.factor.post[15],C.factor.post[16],C.factor.post[28])

C.factor.pré.Forces<-c(C.factor.pré[1],C.factor.pré[2],C.factor.pré[3],C.factor.pré[5],C.factor.pré[11],
                       C.factor.pré[13],C.factor.pré[18],C.factor.pré[29],C.factor.pré[30])
C.factor.post.Forces<-c(C.factor.post[1],C.factor.post[2],C.factor.post[3],C.factor.post[5],C.factor.post[11],
                        C.factor.post[13],C.factor.post[18],C.factor.post[29],C.factor.post[30])

##Facteur de déviation par dimension
Dev.factor.pré.Cin<-c(Dev.factor.pré[12],Dev.factor.pré[14],Dev.factor.pré[19],Dev.factor.pré[20])
Dev.factor.Post.Cin<-c(Dev.factor.post[12],Dev.factor.post[14],Dev.factor.post[19],Dev.factor.post[20])

Dev.factor.pré.N1<-c(Dev.factor.pré[6],Dev.factor.pré[7],Dev.factor.pré[8],Dev.factor.pré[10],Dev.factor.pré[17],
                   Dev.factor.pré[23],Dev.factor.pré[24],Dev.factor.pré[25])
Dev.factor.post.N1<-c(Dev.factor.post[6],Dev.factor.post[7],Dev.factor.post[8],Dev.factor.post[10],Dev.factor.post[17],
                    Dev.factor.post[23],Dev.factor.post[24],Dev.factor.post[25])

Dev.factor.pré.N2<-c(Dev.factor.pré[21],Dev.factor.pré[22],Dev.factor.pré[26],Dev.factor.pré[27])
Dev.factor.post.N2<-c(Dev.factor.post[21],Dev.factor.post[22],Dev.factor.post[26],Dev.factor.post[27])

Dev.factor.pré.N3<-c(Dev.factor.pré[4],Dev.factor.pré[15],Dev.factor.pré[16],Dev.factor.pré[28])
Dev.factor.post.N3<-c(Dev.factor.post[4],Dev.factor.post[15],Dev.factor.post[16],Dev.factor.post[28])

Dev.factor.pré.Forces<-c(Dev.factor.pré[1],Dev.factor.pré[2],Dev.factor.pré[3],Dev.factor.pré[5],Dev.factor.pré[11],
                       Dev.factor.pré[13],Dev.factor.pré[18],Dev.factor.pré[29],C.factor.pré[30])
Dev.factor.post.Forces<-c(Dev.factor.post[1],Dev.factor.post[2],Dev.factor.post[3],Dev.factor.post[5],Dev.factor.post[11],
                        Dev.factor.post[13],Dev.factor.post[18],Dev.factor.post[29],Dev.factor.post[30])

#Score par dimension ecprimé en pourcent calculé à partir des des variables calculées à la fin dans la section comparaison pré

S.pré.cin <- Cin.pré/100
S.post.cin<-Cin.post/100
S.pré.N1<-N1.pré/100
S.post.N1<-N1.post/100
S.pré.N2<-N2.pré/100
S.post.N2<-N2.post/100
S.pré.N3<-N3.pré/100
S.post.N3<-N3.post/100
S.pré.Forces<-Forces.pré/100
S.post.Forces<-Forces.post/100

##Dimension Cinématique
   #Graphe Concentration (évolution pré-post)
plot(C.factor.pré.Cin~ S.pré.cin,xlim=c(0,1),ylim=c(0,1),main="Facteur C en fonction du taux de réponses correctes
par question pour la dimension cinématique chez les filles",col="gray65",pch=16,ylab="Facteur de concentration C",
     xlab="Taux de réponses correctes par question")
text(x=S.pré.cin+0.02,y=C.factor.pré.Cin,labels=c(12,14,19,20))
points(x=S.post.cin,y=C.factor.post.Cin,pch=16,col="gray25")
text(x=S.post.cin+0.02,y=C.factor.post.Cin,labels=c(12,14,19,20))
abline(h=c(0.2,0.5),v=c(0.4,0.7), col="gray45")
lines(seq(from=0,to=1,by=0.001),Cmax(seq(from=0,to=1,by=0.001)),
      type = "l")
lines(seq(from=0,to=1,by=0.001),Cmin(seq(from=0,to=1,by=0.001)),
      type = "l")
points(x=c(mean(S.pré.cin),mean(S.post.cin)),y=c(mean(C.factor.pré.Cin),mean(C.factor.post.Cin))
      ,pch=c(17,19),col="gray1",cex=1.5)
arrows(x0=mean(S.pré.cin),x1=mean(S.post.cin),y0=mean(C.factor.pré.Cin),y1=mean(C.factor.post.Cin),
      code=2,lwd=1.5,length=0.15)
legend("topleft",c("pré-test","post-test","Moyenne pré-test","Moyenne post-test"),
       cex = 0.9, pch = c(16,16,17,19),col=c("gray65","gray25","gray1","gray1"))

   #Graphe S-Gamma
plot(Dev.factor.pré.Cin~ S.pré.cin,xlim=c(0,1),ylim=c(0,1),main="Facteur de déviation en fonction du taux de réponses correctes
par question pour la dimension cinématique",col="gray65",pch=16,
     ylab="Facteur de deviation",
     xlab="Taux de réponses correctes par question")
text(x=S.pré.cin+0.02,y=Dev.factor.pré.Cin,labels=c(12,14,19,20))
points(x=S.post.cin,y=Dev.factor.post.Cin,pch=16,col="gray25")
text(x=S.post.cin+0.02,y=Dev.factor.post.Cin,labels=c(12,14,19,20))
points(x=c(mean(S.pré.cin),mean(S.post.cin)),y=c(mean(Dev.factor.pré.Cin),mean(Dev.factor.post.Cin))
       ,pch=c(17,19),col="gray1",cex=1.5)
arrows(x0=mean(S.pré.cin),x1=mean(S.post.cin),y0=mean(Dev.factor.pré.Cin),y1=mean(Dev.factor.post.Cin),
       code=2,lwd=1.5,length=0.15)
grid(nx=NULL, ny=NULL, col="gray19")
legend("topleft",c("pré-test","post-test","Moyenne pré-test","Moyenne post-test"),
       cex = 0.9, pch = c(16,16,17,19),col=c("gray65","gray25","gray1","gray1"))

##Dimension N1
   #Concentration
plot(C.factor.pré.N1~ S.pré.N1,xlim=c(0,1),ylim=c(0,1),main="Facteur de concentration en fonction du taux de réponses
correctes par question pour la dimension N1 chez les garçons",col="skyblue1",pch=16,ylab="Facteur de concentration C",
     xlab="Taux de réponses correctes par question")
text(x=S.pré.N1+0.02,y=C.factor.pré.N1-0.02,labels=c(6,7,8,10,17,23,24,25))
points(x=S.post.N1,y=C.factor.post.N1,pch=16,col="skyblue4")
text(x=S.post.N1-0.03,y=C.factor.post.N1+0.01,labels=c(6,7,8,10,17,23,24,25))
abline(h=c(0.2,0.5),v=c(0.4,0.7), col="gray45")
lines(seq(from=0,to=1,by=0.001),Cmax(seq(from=0,to=1,by=0.001)),
      type = "l")
lines(seq(from=0,to=1,by=0.001),Cmin(seq(from=0,to=1,by=0.001)),
      type = "l")
points(x=c(mean(S.pré.N1),mean(S.post.N1)),y=c(mean(C.factor.pré.N1),mean(C.factor.post.N1))
       ,pch=c(17,19),col="gray1",cex=1.5)
arrows(x0=mean(S.pré.N1),x1=mean(S.post.N1),y0=mean(C.factor.pré.N1),y1=mean(C.factor.post.N1),
       code=2,lwd=1.5,length=0.15)
legend("topleft",c("pré-test","post-test","Moyenne pré-test","Moyenne post-test"),
       cex = 0.9, pch=c(16,16,17,19),col=c("skyblue1","skyblue4","gray1","gray1"))

#Deviation
plot(Dev.factor.pré.N1~ S.pré.N1,xlim=c(0,1),ylim=c(0,1),main="Facteur de déviation en fonction du taux de réponses
correctes par question pour la dimension N1 chez les garçons",col="skyblue1",pch=16,ylab="Facteur de Deviation",
     xlab="Taux de réponses correctes par question")
text(x=S.pré.N1+0.02,y=Dev.factor.pré.N1-0.02,labels=c(6,7,8,10,17,23,24,25))
points(x=S.post.N1,y=Dev.factor.post.N1,pch=16,col="skyblue4")
text(x=S.post.N1-0.03,y=Dev.factor.post.N1+0.01,labels=c(6,7,8,10,17,23,24,25))
grid(nx=NULL, ny=NULL, col="gray19")
points(x=c(mean(S.pré.N1),mean(S.post.N1)),y=c(mean(Dev.factor.pré.N1),mean(Dev.factor.post.N1))
       ,pch=c(17,19),col="gray1",cex=1.5)
arrows(x0=mean(S.pré.N1),x1=mean(S.post.N1),y0=mean(Dev.factor.pré.N1),y1=mean(Dev.factor.post.N1),
       code=2,lwd=1.5,length=0.15)
legend("topleft",c("pré-test","post-test","Moyenne pré-test","Moyenne post-test"),
       cex = 0.9, pch=c(16,16,17,19),col=c("skyblue1","skyblue4","gray1","gray1"))

##Dimension N2
  #Concentration
plot(C.factor.pré.N2~ S.pré.N2,xlim=c(0,1),ylim=c(0,1),main="Facteur de concentration en fonction du taux de réponses
correctes par question pour la dimension N2 chez les garçons",col="chartreuse1",pch=16,ylab="Facteur de concentration C",
     xlab="Taux de réponses correctes par question")
text(x=S.pré.N2+0.02,y=C.factor.pré.N2,labels=c(21,22,26,27))
points(x=S.post.N2,y=C.factor.post.N2,pch=16,col="chartreuse4")
text(x=S.post.N2+0.02,y=C.factor.post.N2,labels=c(21,22,26,27))
abline(h=c(0.2,0.5),v=c(0.4,0.7), col="gray45")
lines(seq(from=0,to=1,by=0.001),Cmax(seq(from=0,to=1,by=0.001)),
      type = "l")
lines(seq(from=0,to=1,by=0.001),Cmin(seq(from=0,to=1,by=0.001)),
      type = "l")
points(x=c(mean(S.pré.N2),mean(S.post.N2)),y=c(mean(C.factor.pré.N2),mean(C.factor.post.N2))
       ,pch=c(17,19),col="gray1",cex=1.5)
lines(x=c(mean(S.pré.N2),mean(S.post.N2)),y=c(mean(C.factor.pré.N2),mean(C.factor.post.N2)),
      lwd=1.5)
legend("topleft",c("pré-test","post-test","Moyenne pré-test","Moyenne post-test"),
       cex = 0.9, pch=c(16,16,17,19),col=c("chartreuse1","chartreuse4","gray1","gray1"))

#Déviation
plot(Dev.factor.pré.N2~ S.pré.N2,xlim=c(0,1),ylim=c(0,1),main="Facteur de déviation en fonction du taux de réponses
correctes par question pour la dimension N2 chez les garçons",col="chartreuse1",pch=16,ylab="Facteur de déviation",
     xlab="Taux de réponses correctes par question")
text(x=S.pré.N2+0.02,y=Dev.factor.pré.N2,labels=c(21,22,26,27))
points(x=S.post.N2,y=Dev.factor.post.N2,pch=16,col="chartreuse4")
text(x=S.post.N2+0.02,y=Dev.factor.post.N2,labels=c(21,22,26,27))
grid(nx=NULL, ny=NULL, col="gray19")
points(x=c(mean(S.pré.N2),mean(S.post.N2)),y=c(mean(Dev.factor.pré.N2),mean(Dev.factor.post.N2))
       ,pch=c(17,19),col="gray1",cex=1.5)
arrows(x0=mean(S.pré.N2),x1=mean(S.post.N2),y0=mean(Dev.factor.pré.N2),y1=mean(Dev.factor.post.N2),
       code=2,lwd=1.5,length=0.15)
legend("topleft",c("pré-test","post-test","Moyenne pré-test","Moyenne post-test"),
       cex = 0.9, pch=c(16,16,17,19),col=c("chartreuse1","chartreuse4","gray1","gray1"))

##Dimension N3
  #Concentration
plot(C.factor.pré.N3~ S.pré.N3,xlim=c(0,1),ylim=c(0,1),main="Facteur de concentration en fonction du taux de réponses
correctes par question pour la dimension N3 chez les garçons",col="red1",pch=16,ylab="Facteur de concentration C",
     xlab="Taux de réponses correctes par question")
text(x=S.pré.N3+0.02,y=C.factor.pré.N3,labels=c(4,15,16,28))
points(x=S.post.N3,y=C.factor.post.N3,pch=16,col="red4")
text(x=S.post.N3+0.02,y=C.factor.post.N3,labels=c(4,15,16,28))
abline(h=c(0.2,0.5),v=c(0.4,0.7), col="gray45")
lines(seq(from=0,to=1,by=0.001),Cmax(seq(from=0,to=1,by=0.001)),
      type = "l")
lines(seq(from=0,to=1,by=0.001),Cmin(seq(from=0,to=1,by=0.001)),
     type = "l")
points(x=c(mean(S.pré.N3),mean(S.post.N3)),y=c(mean(C.factor.pré.N3),mean(C.factor.post.N3))
       ,pch=c(17,19),col="gray1",cex=1.5)
arrows(x0=mean(S.pré.N3),x1=mean(S.post.N3),y0=mean(C.factor.pré.N3),y1=mean(C.factor.post.N3),
code=2,lwd=1.5,length=0.15)
legend("topleft",c("pré-test","post-test","Moyenne pré-test","Moyenne post-test"),
       cex = 0.9, pch=c(16,16,17,19),col=c("red1","red4","gray1","gray1"))

#Déviation
plot(Dev.factor.pré.N3~ S.pré.N3,xlim=c(0,1),ylim=c(0,1),main="Facteur de Deviation en fonction du taux de réponses
correctes par question pour la dimension N3 chez les garçons",col="red1",pch=16,ylab="Facteur de déviation",
     xlab="Taux de réponses correctes par question")
text(x=S.pré.N3+0.02,y=Dev.factor.pré.N3,labels=c(4,15,16,28))
points(x=S.post.N3,y=Dev.factor.post.N3,pch=16,col="red4")
text(x=S.post.N3+0.02,y=Dev.factor.post.N3,labels=c(4,15,16,28))
grid(nx=NULL, ny=NULL, col="gray19")
points(x=c(mean(S.pré.N3),mean(S.post.N3)),y=c(mean(Dev.factor.pré.N3),mean(Dev.factor.post.N3))
       ,pch=c(17,19),col="gray1",cex=1.5)
arrows(x0=mean(S.pré.N3),x1=mean(S.post.N3),y0=mean(Dev.factor.pré.N3),y1=mean(Dev.factor.post.N3),
       code=2,lwd=1.5,length=0.15)
legend("topleft",c("pré-test","post-test","Moyenne pré-test","Moyenne post-test"),
       cex = 0.9, pch=c(16,16,17,19),col=c("red1","red4","gray1","gray1"))

##Dimension Forces
  #Concentration
plot(C.factor.pré.Forces~ S.pré.Forces,xlim=c(0,1),ylim=c(0,1),main="Facteur de concentration en fonction du taux de réponses
correctes par question pour la dimension Forces chez les garçons",col="darkgoldenrod1",pch=16,ylab="Facteur de concentration C",
     xlab="Taux de réponses correctes par question")
text(x=S.pré.Forces+0.02,y=C.factor.pré.Forces+0.01,labels=c(1,2,3,5,11,13,18,29,30))
points(x=S.post.Forces,y=C.factor.post.Forces,pch=16,col="darkgoldenrod4")
text(x=S.post.Forces+0.02,y=C.factor.post.Forces,labels=c(1,2,3,5,11,13,18,29,30))
abline(h=c(0.2,0.5),v=c(0.4,0.7), col="gray45")
lines(seq(from=0,to=1,by=0.001),Cmax(seq(from=0,to=1,by=0.001)),
      type = "l")
lines(seq(from=0,to=1,by=0.001),Cmin(seq(from=0,to=1,by=0.001)),
      type = "l")
points(x=c(mean(S.pré.Forces),mean(S.post.Forces)),y=c(mean(C.factor.pré.Forces),mean(C.factor.post.Forces))
       ,pch=c(17,19),col="gray1",cex=1.5)
arrows(x0=mean(S.pré.Forces),x1=mean(S.post.Forces),y0=mean(C.factor.pré.Forces),y1=mean(C.factor.post.Forces),
       code=2,lwd=1.5,length=0.15)
legend("topleft",c("pré-test","post-test","Moyenne pré-test","Moyenne post-test"),
       cex = 0.9, pch=c(16,16,17,19),col=c("darkgoldenrod1","darkgoldenrod4","gray1","gray1"))

#Déviation
plot(Dev.factor.pré.Forces~ S.pré.Forces,xlim=c(0,1),ylim=c(0,1),main="Facteur de déviation en fonction du taux de réponses
correctes par question pour la dimension Forces chez les garçons",col="darkgoldenrod1",pch=16,ylab="Facteur de déviation",
     xlab="Taux de réponses correctes par question")
text(x=S.pré.Forces+0.02,y=Dev.factor.pré.Forces+0.01,labels=c(1,2,3,5,11,13,18,29,30))
points(x=S.post.Forces,y=Dev.factor.post.Forces,pch=16,col="darkgoldenrod4")
text(x=S.post.Forces+0.02,y=Dev.factor.post.Forces,labels=c(1,2,3,5,11,13,18,29,30))
grid(nx=NULL, ny=NULL, col="gray19")
points(x=c(mean(S.pré.Forces),mean(S.post.Forces)),y=c(mean(Dev.factor.pré.Forces),mean(Dev.factor.post.Forces))
       ,pch=c(17,19),col="gray1",cex=1.5)
arrows(x0=mean(S.pré.Forces),x1=mean(S.post.Forces),y0=mean(Dev.factor.pré.Forces),y1=mean(Dev.factor.post.Forces),
       code=2,lwd=1.5,length=0.15)
legend("topleft",c("pré-test","post-test","Moyenne pré-test","Moyenne post-test"),
       cex = 0.9, pch=c(16,16,17,19),col=c("darkgoldenrod1","darkgoldenrod4","gray1","gray1"))

#######################################Analyse concentration Filles vs Garçons##################

## Calcul des taux de réponses correctes par question + par item + facteur de concentration (code identique adapté en fonction du genre)
AbsPré.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CorrectPré.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WrongPré.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 9:38) {
  for (i in 1:nrow(data_F)) {
    if (is.na(data_F[i,j])==TRUE){
      AbsPré.F[j-8]=AbsPré.F[j-8]+1
    }
    else if (is.na(data_F[i,j])==FALSE & data_F[i,j]!=Items[j-8])
    {WrongPré.F[j-8]<-WrongPré.F[j-8]+1}
    else if (is.na(data_F[i,j])==FALSE & data_F[i,j]==Items[j-8]) 
    {CorrectPré.F[j-8]<-CorrectPré.F[j-8]+1}
  }
}

print(CorrectPré.F+WrongPré.F+AbsPré.F)

AbsPost.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CorrectPost.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WrongPost.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 40:69) {
  for (i in 1:nrow(data_F)) {
    if (is.na(data_F[i,j])==TRUE){
      AbsPost.F[j-39]=AbsPost.F[j-39]+1
    }
    else if (is.na(data_F[i,j])==FALSE & data_F[i,j]!=Items[j-39])
    {WrongPost.F[j-39]<-WrongPost.F[j-39]+1}
    else if (is.na(data_F[i,j])==FALSE & data_F[i,j]==Items[j-39]) 
    {CorrectPost.F[j-39]<-CorrectPost.F[j-39]+1}
  }
}

print(CorrectPost.F+WrongPost.F+AbsPost.F)

#comparaison item par item 
AbsPré.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
APré.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
BPré.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CPré.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
DPré.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
EPré.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 9:38) {
  for (i in 1:nrow(data_F)) {
    if (is.na(data_F[i,j])==TRUE){
      AbsPré.F[j-8]=AbsPré.F[j-8]+1
    }
    else if (is.na(data_F[i,j])==FALSE & data_F[i,j]=="A")
    {APré.F[j-8]<-APré.F[j-8]+1}
    else if (is.na(data_F[i,j])==FALSE  & data_F[i,j]=="B")
    {BPré.F[j-8]<-BPré.F[j-8]+1}
    else if (is.na(data_F[i,j])==FALSE  & data_F[i,j]=="C")
    {CPré.F[j-8]<-CPré.F[j-8]+1}
    else if (is.na(data_F[i,j])==FALSE & data_F[i,j]=="D")
    {DPré.F[j-8]<-DPré.F[j-8]+1}
    else if (is.na(data_F[i,j])==FALSE & data_F[i,j]=="E")
    {EPré.F[j-8]<-EPré.F[j-8]+1}
  }
}
print(APré.F+BPré.F+CPré.F+DPré.F+EPré.F+AbsPré.F)


AbsPost.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
APost.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
BPost.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CPost.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
DPost.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
EPost.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 40:69) {
  for (i in 1:nrow(data_F)) {
    if (is.na(data_F[i,j])==TRUE){
      AbsPost.F[j-39]=AbsPost.F[j-39]+1
    }
    else if (is.na(data_F[i,j])==FALSE & data_F[i,j]=="A")
    {APost.F[j-39]<-APost.F[j-39]+1}
    else if (is.na(data_F[i,j])==FALSE & data_F[i,j]=="B")
    {BPost.F[j-39]<-BPost.F[j-39]+1}
    else if (is.na(data_F[i,j])==FALSE & data_F[i,j]=="C")
    {CPost.F[j-39]<-CPost.F[j-39]+1}
    else if (is.na(data_F[i,j])==FALSE & data_F[i,j]=="D")
    {DPost.F[j-39]<-DPost.F[j-39]+1}
    else if (is.na(data_F[i,j])==FALSE & data_F[i,j]=="E")
    {EPost.F[j-39]<-EPost.F[j-39]+1}
  }
}
AbsPost.F[25]=2
AbsPost.F[29]=4
print(APost.F+BPost.F+CPost.F+DPost.F+EPost.F+AbsPost.F)

N<-nrow(data_F) #changer data en fonction de la population d'intérêt
m<-5
col.dim<-c("gray45","cornflowerblue","chartreuse2","red2","darkgoldenrod1","palevioletred4","gray1")
col.dim1<-c("darkgoldenrod1","darkgoldenrod1","darkgoldenrod1","red2","darkgoldenrod1","cornflowerblue","cornflowerblue",
            "cornflowerblue","palevioletred4","cornflowerblue","darkgoldenrod1","gray45","darkgoldenrod1","gray45",
            "red2","red2","cornflowerblue","darkgoldenrod1","gray45","gray45","chartreuse2","chartreuse2","cornflowerblue",
            "cornflowerblue","cornflowerblue","chartreuse2","chartreuse2","red2","darkgoldenrod1","darkgoldenrod1")
C.factor.Pré.F<-c()
C.factor.Post.F<-c()
for (i in 1:30){
  C.factor.Pré.F[i]<-sqrt(m)/(sqrt(m)-1)*(sqrt((APré.F[i])^2+(BPré.F[i])^2+(CPré.F[i])^2+(DPré.F[i])^2+(EPré.F[i])^2)/N-1/sqrt(m))
  C.factor.Post.F[i]<-sqrt(m)/(sqrt(m)-1)*(sqrt((APost.F[i])^2+(BPost.F[i])^2+(CPost.F[i])^2+(DPost.F[i])^2+(EPost.F[i])^2)/N-1/sqrt(m))
}
#Deviation facteur
Dev.factor.Pré.F<-c()
Dev.factor.Post.F<-c()
for (i in 1:30){
  Dev.factor.Pré.F[i]<-sqrt(m-1)/(sqrt(m-1)-1)*(sqrt((APré.F[i])^2+(BPré.F[i])^2+(CPré.F[i])^2+(DPré.F[i])^2+(EPré.F[i])^2-CorrectPré.F[i]^2)/(N-CorrectPré.F[i])-1/sqrt(m-1))
  Dev.factor.Post.F[i]<-sqrt(m-1)/(sqrt(m-1)-1)*(sqrt((APost.F[i])^2+(BPost.F[i])^2+(CPost.F[i])^2+(DPost.F[i])^2+(EPost.F[i])^2-CorrectPost.F[i]^2)/(N-CorrectPost.F[i])-1/sqrt(m-1))
}

#Facteur déviation pour les questions ayant le niveau "L" pour le score
Dev.factor.Pré.F.L<-Dev.factor.Pré.F[is.na(Num.quest.L)==FALSE]
Dev.factor.Post.F.L<-Dev.factor.Post.F[is.na(Num.quest.L)==FALSE]

Cmax<-function(x){
  f<-(sqrt(m)/(sqrt(m)-1))*(sqrt((1-x)^2+x^2)-1/sqrt(m))
  f
}
Cmin<-function(x){
  f<-(sqrt(m)/(sqrt(m)-1))*(sqrt(4*((1-x)/4)^2+x^2)-1/sqrt(m))
  f
}

Présence.Pré.F<- nrow(data_F)-AbsPré.F 
Présence.Post.F<- nrow(data_F)-AbsPost.F

CorrectPré.F<-CorrectPré.F/Présence.Pré.F
CorrectPost.F<-CorrectPost.F/Présence.Post.F

#Score pour les questions de type LL

CorrectPré.F.L<-CorrectPré.F[is.na(Num.quest.L)==FALSE]
CorrectPost.F.L<-CorrectPost.F[is.na(Num.quest.L)==FALSE]

Cin.Pré.F <- 100*c(CorrectPré.F[12],CorrectPré.F[14],CorrectPré.F[19],CorrectPré.F[20])
Cin.Post.F <-100*c(CorrectPost.F[12],CorrectPost.F[14],CorrectPost.F[19],CorrectPost.F[20])

N1.Pré.F <-100*c(CorrectPré.F[6],CorrectPré.F[7],CorrectPré.F[8],CorrectPré.F[10],CorrectPré.F[17],CorrectPré.F[23],CorrectPré.F[24],CorrectPré.F[25])
N1.Post.F<-100*c(CorrectPost.F[6],CorrectPost.F[7],CorrectPost.F[8],CorrectPost.F[10],CorrectPost.F[17],CorrectPost.F[23],CorrectPost.F[24],CorrectPost.F[25])

N2.Pré.F<-100*c(CorrectPré.F[21],CorrectPré.F[22],CorrectPré.F[26],CorrectPré.F[27])
N2.Post.F<-100*c(CorrectPost.F[21],CorrectPost.F[22],CorrectPost.F[26],CorrectPost.F[27])

N3.Pré.F<-100*c(CorrectPré.F[4],CorrectPré.F[15],CorrectPré.F[16],CorrectPré.F[28])
N3.Post.F<-100*c(CorrectPost.F[4],CorrectPost.F[15],CorrectPost.F[16],CorrectPost.F[28])

Superposition.Pré.F <-100*c(CorrectPré.F[9])
Superposition.Post.F <-100*c(CorrectPost.F[9])

Forces.Pré.F<-100*c(CorrectPré.F[1],CorrectPré.F[2],CorrectPré.F[3],CorrectPré.F[5],CorrectPré.F[11],CorrectPré.F[13],
                  CorrectPré.F[18],CorrectPré.F[29],CorrectPré.F[30])
Forces.Post.F<- 100*c(CorrectPost.F[1],CorrectPost.F[2],CorrectPost.F[3],CorrectPost.F[5],CorrectPost.F[11],
                    CorrectPost.F[13],CorrectPost.F[18],CorrectPost.F[29],CorrectPost.F[30])

#facteur concentration par dimension filles
C.factor.Pré.Cin.F<-c(C.factor.Pré.F[12],C.factor.Pré.F[14],C.factor.Pré.F[19],C.factor.Pré.F[20])
C.factor.Post.Cin.F<-c(C.factor.Post.F[12],C.factor.Post.F[14],C.factor.Post.F[19],C.factor.Post.F[20])

C.factor.Pré.N1.F<-c(C.factor.Pré.F[6],C.factor.Pré.F[7],C.factor.Pré.F[8],C.factor.Pré.F[10],C.factor.Pré.F[17],
                   C.factor.Pré.F[23],C.factor.Pré.F[24],C.factor.Pré.F[25])
C.factor.Post.N1.F<-c(C.factor.Post.F[6],C.factor.Post.F[7],C.factor.Post.F[8],C.factor.Post.F[10],C.factor.Post.F[17],
                    C.factor.Post.F[23],C.factor.Post.F[24],C.factor.Post.F[25])

C.factor.Pré.N2.F<-c(C.factor.Pré.F[21],C.factor.Pré.F[22],C.factor.Pré.F[26],C.factor.Pré.F[27])
C.factor.Post.N2.F<-c(C.factor.Post.F[21],C.factor.Post.F[22],C.factor.Post.F[26],C.factor.Post.F[27])

C.factor.Pré.N3.F<-c(C.factor.Pré.F[4],C.factor.Pré.F[15],C.factor.Pré.F[16],C.factor.Pré.F[28])
C.factor.Post.N3.F<-c(C.factor.Post.F[4],C.factor.Post.F[15],C.factor.Post.F[16],C.factor.Post.F[28])

C.factor.Pré.Forces.F<-c(C.factor.Pré.F[1],C.factor.Pré.F[2],C.factor.Pré.F[3],C.factor.Pré.F[5],C.factor.Pré.F[11],
                       C.factor.Pré.F[13],C.factor.Pré.F[18],C.factor.Pré.F[29],C.factor.Pré.F[30])
C.factor.Post.Forces.F<-c(C.factor.Post.F[1],C.factor.Post.F[2],C.factor.Post.F[3],C.factor.Post.F[5],C.factor.Post.F[11],
                        C.factor.Post.F[13],C.factor.Post.F[18],C.factor.Post.F[29],C.factor.Post.F[30])

##
Dev.factor.Pré.Cin.F<-c(Dev.factor.Pré.F[12],Dev.factor.Pré.F[14],Dev.factor.Pré.F[19],Dev.factor.Pré.F[20])
Dev.factor.Post.Cin.F<-c(Dev.factor.Post.F[12],Dev.factor.Post.F[14],Dev.factor.Post.F[19],Dev.factor.Post.F[20])

Dev.factor.Pré.N1.F<-c(Dev.factor.Pré.F[6],Dev.factor.Pré.F[7],Dev.factor.Pré.F[8],Dev.factor.Pré.F[10],Dev.factor.Pré.F[17],
                     Dev.factor.Pré.F[23],Dev.factor.Pré.F[24],Dev.factor.Pré.F[25])
Dev.factor.Post.N1.F<-c(Dev.factor.Post.F[6],Dev.factor.Post.F[7],Dev.factor.Post.F[8],Dev.factor.Post.F[10],Dev.factor.Post.F[17],
                      Dev.factor.Post.F[23],Dev.factor.Post.F[24],Dev.factor.Post.F[25])

Dev.factor.Pré.N2.F<-c(Dev.factor.Pré.F[21],Dev.factor.Pré.F[22],Dev.factor.Pré.F[26],Dev.factor.Pré.F[27])
Dev.factor.Post.N2.F<-c(Dev.factor.Post.F[21],Dev.factor.Post.F[22],Dev.factor.Post.F[26],Dev.factor.Post.F[27])

Dev.factor.Pré.N3.F<-c(Dev.factor.Pré.F[4],Dev.factor.Pré.F[15],Dev.factor.Pré.F[16],Dev.factor.Pré.F[28])
Dev.factor.Post.N3.F<-c(Dev.factor.Post.F[4],Dev.factor.Post.F[15],Dev.factor.Post.F[16],Dev.factor.Post.F[28])

Dev.factor.Pré.Forces.F<-c(Dev.factor.Pré.F[1],Dev.factor.Pré.F[2],Dev.factor.Pré.F[3],Dev.factor.Pré.F[5],Dev.factor.Pré.F[11],
                         Dev.factor.Pré.F[13],Dev.factor.Pré.F[18],Dev.factor.Pré.F[29],C.factor.Pré.F[30])
Dev.factor.Post.Forces.F<-c(Dev.factor.Post.F[1],Dev.factor.Post.F[2],Dev.factor.Post.F[3],Dev.factor.Post.F[5],Dev.factor.Post.F[11],
                          Dev.factor.Post.F[13],Dev.factor.Post.F[18],Dev.factor.Post.F[29],Dev.factor.Post.F[30])

###Analyse concentration garçons

AbsPré.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CorrectPré.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WrongPré.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 9:38) {
  for (i in 1:nrow(data_H)) {
    if (is.na(data_H[i,j])==TRUE){
      AbsPré.H[j-8]=AbsPré.H[j-8]+1
    }
    else if (is.na(data_H[i,j])==FALSE & data_H[i,j]!=Items[j-8])
    {WrongPré.H[j-8]<-WrongPré.H[j-8]+1}
    else if (is.na(data_H[i,j])==FALSE & data_H[i,j]==Items[j-8]) 
    {CorrectPré.H[j-8]<-CorrectPré.H[j-8]+1}
  }
}

print(CorrectPré.H+WrongPré.H+AbsPré.H)

AbsPost.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CorrectPost.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WrongPost.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 40:69) {
  for (i in 1:nrow(data_H)) {
    if (is.na(data_H[i,j])==TRUE){
      AbsPost.H[j-39]=AbsPost.H[j-39]+1
    }
    else if (is.na(data_H[i,j])==FALSE & data_H[i,j]!=Items[j-39])
    {WrongPost.H[j-39]<-WrongPost.H[j-39]+1}
    else if (is.na(data_H[i,j])==FALSE & data_H[i,j]==Items[j-39]) 
    {CorrectPost.H[j-39]<-CorrectPost.H[j-39]+1}
  }
}

print(CorrectPost.H+WrongPost.H+AbsPost.H)

#comparaison item par item 
AbsPré.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
APré.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
BPré.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CPré.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
DPré.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
EPré.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 9:38) {
  for (i in 1:nrow(data_H)) {
    if (is.na(data_H[i,j])==TRUE){
      AbsPré.H[j-8]=AbsPré.H[j-8]+1
    }
    else if (is.na(data_H[i,j])==FALSE & data_H[i,j]=="A")
    {APré.H[j-8]<-APré.H[j-8]+1}
    else if (is.na(data_H[i,j])==FALSE  & data_H[i,j]=="B")
    {BPré.H[j-8]<-BPré.H[j-8]+1}
    else if (is.na(data_H[i,j])==FALSE  & data_H[i,j]=="C")
    {CPré.H[j-8]<-CPré.H[j-8]+1}
    else if (is.na(data_H[i,j])==FALSE & data_H[i,j]=="D")
    {DPré.H[j-8]<-DPré.H[j-8]+1}
    else if (is.na(data_H[i,j])==FALSE & data_H[i,j]=="E")
    {EPré.H[j-8]<-EPré.H[j-8]+1}
  }
}
print(APré.H+BPré.H+CPré.H+DPré.H+EPré.H+AbsPré.H)


AbsPost.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
APost.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
BPost.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CPost.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
DPost.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
EPost.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 40:69) {
  for (i in 1:nrow(data_H)) {
    if (is.na(data_H[i,j])==TRUE){
      AbsPost.H[j-39]=AbsPost.H[j-39]+1
    }
    else if (is.na(data_H[i,j])==FALSE & data_H[i,j]=="A")
    {APost.H[j-39]<-APost.H[j-39]+1}
    else if (is.na(data_H[i,j])==FALSE & data_H[i,j]=="B")
    {BPost.H[j-39]<-BPost.H[j-39]+1}
    else if (is.na(data_H[i,j])==FALSE & data_H[i,j]=="C")
    {CPost.H[j-39]<-CPost.H[j-39]+1}
    else if (is.na(data_H[i,j])==FALSE & data_H[i,j]=="D")
    {DPost.H[j-39]<-DPost.H[j-39]+1}
    else if (is.na(data_H[i,j])==FALSE & data_H[i,j]=="E")
    {EPost.H[j-39]<-EPost.H[j-39]+1}
  }
}
AbsPost.H[25]=2
AbsPost.H[29]=4
print(APost.H+BPost.H+CPost.H+DPost.H+EPost.H+AbsPost.H)

N<-nrow(data_H) 
m<-5
col.dim<-c("gray45","cornflowerblue","chartreuse2","red2","darkgoldenrod1","palevioletred4","gray1")
col.dim1<-c("darkgoldenrod1","darkgoldenrod1","darkgoldenrod1","red2","darkgoldenrod1","cornflowerblue","cornflowerblue",
            "cornflowerblue","palevioletred4","cornflowerblue","darkgoldenrod1","gray45","darkgoldenrod1","gray45",
            "red2","red2","cornflowerblue","darkgoldenrod1","gray45","gray45","chartreuse2","chartreuse2","cornflowerblue",
            "cornflowerblue","cornflowerblue","chartreuse2","chartreuse2","red2","darkgoldenrod1","darkgoldenrod1")
C.factor.Pré.H<-c()
C.factor.Post.H<-c()
for (i in 1:30){
  C.factor.Pré.H[i]<-sqrt(m)/(sqrt(m)-1)*(sqrt((APré.H[i])^2+(BPré.H[i])^2+(CPré.H[i])^2+(DPré.H[i])^2+(EPré.H[i])^2)/N-1/sqrt(m))
  C.factor.Post.H[i]<-sqrt(m)/(sqrt(m)-1)*(sqrt((APost.H[i])^2+(BPost.H[i])^2+(CPost.H[i])^2+(DPost.H[i])^2+(EPost.H[i])^2)/N-1/sqrt(m))
}
#Deviation facteur
Dev.factor.Pré.H<-c()
Dev.factor.Post.H<-c()
for (i in 1:30){
  Dev.factor.Pré.H[i]<-sqrt(m-1)/(sqrt(m-1)-1)*(sqrt((APré.H[i])^2+(BPré.H[i])^2+(CPré.H[i])^2+(DPré.H[i])^2+(EPré.H[i])^2-CorrectPré.H[i]^2)/(N-CorrectPré.H[i])-1/sqrt(m-1))
  Dev.factor.Post.H[i]<-sqrt(m-1)/(sqrt(m-1)-1)*(sqrt((APost.H[i])^2+(BPost.H[i])^2+(CPost.H[i])^2+(DPost.H[i])^2+(EPost.H[i])^2-CorrectPost.H[i]^2)/(N-CorrectPost.H[i])-1/sqrt(m-1))
}

#Facteur déviation pour les questions ayant le niveau "L" pour le score
Dev.factor.Pré.H.L<-Dev.factor.Pré.H[is.na(Num.quest.L)==FALSE]
Dev.factor.Post.H.L<-Dev.factor.Post.H[is.na(Num.quest.L)==FALSE]

Présence.Pré.H<- nrow(data_H)-AbsPré.H 
Présence.Post.H<- nrow(data_H)-AbsPost.H

CorrectPré.H<-CorrectPré.H/Présence.Pré.H
CorrectPost.H<-CorrectPost.H/Présence.Post.H

#Score pour les questions de type LL

CorrectPré.H.L<-CorrectPré.H[is.na(Num.quest.L)==FALSE]
CorrectPost.H.L<-CorrectPost.H[is.na(Num.quest.L)==FALSE]

Cin.Pré.H <- 100*c(CorrectPré.H[12],CorrectPré.H[14],CorrectPré.H[19],CorrectPré.H[20])
Cin.Post.H <-100*c(CorrectPost.H[12],CorrectPost.H[14],CorrectPost.H[19],CorrectPost.H[20])

N1.Pré.H <-100*c(CorrectPré.H[6],CorrectPré.H[7],CorrectPré.H[8],CorrectPré.H[10],CorrectPré.H[17],CorrectPré.H[23],CorrectPré.H[24],CorrectPré.H[25])
N1.Post.H<-100*c(CorrectPost.H[6],CorrectPost.H[7],CorrectPost.H[8],CorrectPost.H[10],CorrectPost.H[17],CorrectPost.H[23],CorrectPost.H[24],CorrectPost.H[25])

N2.Pré.H<-100*c(CorrectPré.H[21],CorrectPré.H[22],CorrectPré.H[26],CorrectPré.H[27])
N2.Post.H<-100*c(CorrectPost.H[21],CorrectPost.H[22],CorrectPost.H[26],CorrectPost.H[27])

N3.Pré.H<-100*c(CorrectPré.H[4],CorrectPré.H[15],CorrectPré.H[16],CorrectPré.H[28])
N3.Post.H<-100*c(CorrectPost.H[4],CorrectPost.H[15],CorrectPost.H[16],CorrectPost.H[28])

Superposition.Pré.H <-100*c(CorrectPré.H[9])
Superposition.Post.H <-100*c(CorrectPost.H[9])

Forces.Pré.H<-100*c(CorrectPré.H[1],CorrectPré.H[2],CorrectPré.H[3],CorrectPré.H[5],CorrectPré.H[11],CorrectPré.H[13],
                    CorrectPré.H[18],CorrectPré.H[29],CorrectPré.H[30])
Forces.Post.H<- 100*c(CorrectPost.H[1],CorrectPost.H[2],CorrectPost.H[3],CorrectPost.H[5],CorrectPost.H[11],
                      CorrectPost.H[13],CorrectPost.H[18],CorrectPost.H[29],CorrectPost.H[30])

##Graphes S-C filles/garçons

par(mfrow = c(1,2))
## Graphe S-C pour les garçons
plot(C.factor.Pré.H~ CorrectPré.H,xlim=c(0,1),ylim=c(0,1),main="Graphe S-C (Garçons)",col="gray1",
     pch=2,ylab="Facteur de concentration C",xlab="Score par question",lwd=1.8)
#text(x=CorrectPré.H+0.02,y=C.factor.Pré.H,labels=c(1:30))
points(x=CorrectPost.H,y=C.factor.Post.H,pch=1,col="gray1",lwd=1.8)
#text(x=CorrectPost.H+0.02,y=C.factor.Post.H,labels=c(1:30))
abline(h=c(0.2,0.5),v=c(0.4,0.7), col="gray45")
lines(seq(from=0,to=1,by=0.001),Cmax(seq(from=0,to=1,by=0.001)),
      type = "l")
lines(seq(from=0,to=1,by=0.001),Cmin(seq(from=0,to=1,by=0.001)),
      type = "l")

points(x=c(mean(CorrectPré.H),mean(CorrectPost.H)),y=c(mean(C.factor.Pré.H),mean(C.factor.Post.H))
       ,pch=c(17,19),col="gray1",cex=1.5)
arrows(x0=mean(CorrectPré.H),x1=mean(CorrectPost.H),y0=mean(C.factor.Pré.H),y1=mean(C.factor.Post.H),
       code=2,lwd=1.5,length=0.15)
legend("topleft",c("pré-test","post-test","Moyenne pré-test","Moyenne post-test"),
       cex = 0.9, pch = c(2,1,17,19),col="gray1")
text(x=1,y=1.15,xpd=TRUE,labels="a)",cex=1.5)

## Graphe S-C pour les filles
plot(C.factor.Pré.F~ CorrectPré.F,xlim=c(0,1),ylim=c(0,1),main="Graphe S-C (Filles)",
     col="gray1",pch=2,ylab="Facteur de concentration C", xlab="Score par question",lwd=1.8)
#text(x=CorrectPré.F+0.02,y=C.factor.Pré.F,labels=c(1:30))
points(x=CorrectPost.F,y=C.factor.Post.F,pch=1,col="gray1",lwd=1.8)
#text(x=CorrectPost.F+0.02,y=C.factor.Post.F,labels=c(1:30))

abline(h=c(0.2,0.5),v=c(0.4,0.7), col="gray45")
lines(seq(from=0,to=1,by=0.001),Cmax(seq(from=0,to=1,by=0.001)),
      type = "l")
lines(seq(from=0,to=1,by=0.001),Cmin(seq(from=0,to=1,by=0.001)),
      type = "l")

points(x=c(mean(CorrectPré.F),mean(CorrectPost.F)),y=c(mean(C.factor.Pré.F),mean(C.factor.Post.F))
       ,pch=c(17,19),col="gray1",cex=1.5)
arrows(x0=mean(CorrectPré.F),x1=mean(CorrectPost.F),y0=mean(C.factor.Pré.F),y1=mean(C.factor.Post.F),
       code=2,lwd=1.5,length=0.15)
legend("topleft",c("pré-test","post-test","Moyenne pré-test","Moyenne post-test"),
       cex = 0.9, pch = c(2,1,17,19),col="gray1")
text(x=1,y=1.15,xpd=TRUE,labels="b)",cex=1.5)

##S-Gamma garçons/filles

par(mfrow = c(1,2))
 ## Graphe S-Gamma pour les garçons
plot(Dev.factor.Pré.H~ CorrectPré.H,xlim=c(0,1),ylim=c(0,1),main="Graphe S-"* Gamma ~ "(garçons)",col="gray1",
     pch=2,ylab="Facteur de déviation"~ Gamma,xlab="Score par question",lwd=1.8)
#text(x=CorrectPré.H+0.02,y=Dev.factor.Pré.H,labels=c(1:30))
points(x=CorrectPost.H,y=Dev.factor.Post.H,pch=1,col="gray1",lwd=1.8)
#text(x=CorrectPost.H+0.02,y=Dev.factor.Post.H,labels=c(1:30))

points(x=c(mean(CorrectPré.H),mean(CorrectPost.H)),y=c(mean(Dev.factor.Pré.H),mean(Dev.factor.Post.H))
       ,pch=c(17,19),col="gray1",cex=1.5)
arrows(x0=mean(CorrectPré.H),x1=mean(CorrectPost.H),y0=mean(Dev.factor.Pré.H),y1=mean(Dev.factor.Post.H),
       code=2,lwd=1.5,length=0.15)
grid(nx=NULL, ny=NULL, col="gray19")
legend("topleft",c("pré-test","post-test","Moyenne pré-test","Moyenne post-test"),
      cex = 0.9, pch = c(2,1,17,19),col="gray1")
text(x=1,y=1.15,xpd=TRUE,labels="a)",cex=1.5)


## Graphe S-Gamma pour les filles
plot(Dev.factor.Pré.F~ CorrectPré.F,xlim=c(0,1),ylim=c(0,1),main="Graphe S-"* Gamma ~ "(filles)",
     col="gray1",pch=2,ylab="Facteur de déviation" ~ Gamma,xlab="Score par question",lwd=1.8)
#text(x=CorrectPré.F+0.02,y=Dev.factor.Pré.F,labels=c(1:30))
points(x=CorrectPost.F,y=Dev.factor.Post.F,pch=1,col="gray1",lwd=1.8)
#text(x=CorrectPost.F+0.02,y=Dev.factor.Post.F,labels=c(1:30))

points(x=c(mean(CorrectPré.F),mean(CorrectPost.F)),y=c(mean(Dev.factor.Pré.F),mean(Dev.factor.Post.F))
       ,pch=c(17,19),col="gray1",cex=1.5)
arrows(x0=mean(CorrectPré.F),x1=mean(CorrectPost.F),y0=mean(Dev.factor.Pré.F),y1=mean(Dev.factor.Post.F),
       code=2,lwd=1.5,length=0.15)
grid(nx=NULL, ny=NULL, col="gray19")
legend("topleft",c("pré-test","post-test","Moyenne pré-test","Moyenne post-test"),
       cex = 0.9, pch = c(2,1,17,19),col="gray1")
text(x=1,y=1.15,xpd=TRUE,labels="b)",cex=1.5)


##Graphes S-Gamma filles vs garàons pour les questions de type LL

par(mfrow = c(1,2))

## graphe S-Gamma garçons
plot(Dev.factor.Pré.H.L~ CorrectPré.H.L,xlim=c(0,1),ylim=c(0,1),main="Graphe S-"* Gamma ~ "(garçons)",
     col="gray1",pch=2,ylab="Facteur de déviation" ~Gamma,xlab="Score par question",lwd=1.8)
text(x=CorrectPré.H.L+0.02,y=Dev.factor.Pré.H.L,labels=Num.quest.L[is.na(Num.quest.L)==FALSE],cex=0.8)
points(x=CorrectPost.H.L,y=Dev.factor.Post.H.L,pch=1,col="gray1",lwd=1.8)
text(x=CorrectPost.H.L+0.02,y=Dev.factor.Post.H.L,labels=Num.quest.L[is.na(Num.quest.L)==FALSE],cex=0.8)

points(x=c(mean(CorrectPré.H.L),mean(CorrectPost.H.L)),y=c(mean(Dev.factor.Pré.H.L),mean(Dev.factor.Post.H.L))
       ,pch=c(17,19),col="gray1",cex=1.5)
arrows(x0=mean(CorrectPré.H.L),x1=mean(CorrectPost.H.L),y0=mean(Dev.factor.Pré.H.L),y1=mean(Dev.factor.Post.H.L),
       code=2,lwd=1.5,length=0.15)
for (i in 1:length(Num.quest.L[is.na(Num.quest.L)==FALSE])) {
  
  arrows(x0=CorrectPré.H.L[i],x1=CorrectPost.H.L[i],y0=Dev.factor.Pré.H.L[i],y1=Dev.factor.Post.H.L[i],
         code=2,lwd=0.5,length=0.15,lty=3,col="gray35")
}
grid(nx=NULL, ny=NULL, col="gray19")
legend("bottomright",c("pré-test","post-test","Moyenne pré-test","Moyenne post-test"),
       cex = 0.9, pch = c(2,1,17,19),col="gray1")
text(x=1,y=1.15,xpd=TRUE,labels="a)",cex=1.5)

#S-Gamma filles
plot(Dev.factor.Pré.F.L~ CorrectPré.F.L,xlim=c(0,1),ylim=c(0,1),main="Graphe S-"* Gamma ~ "(filles)",
     col="gray1",pch=2,ylab="Facteur de déviation" ~Gamma,xlab="Score par question",lwd=1.8)
text(x=CorrectPré.F.L+0.02,y=Dev.factor.Pré.F.L,labels=Num.quest.L[is.na(Num.quest.L)==FALSE],cex=0.8)
points(x=CorrectPost.F.L,y=Dev.factor.Post.F.L,pch=1,col="gray1",lwd=1.8)
text(x=CorrectPost.F.L+0.02,y=Dev.factor.Post.F.L,labels=Num.quest.L[is.na(Num.quest.L)==FALSE],cex=0.8)

points(x=c(mean(CorrectPré.F.L),mean(CorrectPost.F.L)),y=c(mean(Dev.factor.Pré.F.L),mean(Dev.factor.Post.F.L))
       ,pch=c(17,19),col="gray1",cex=1.5)
arrows(x0=mean(CorrectPré.F.L),x1=mean(CorrectPost.F.L),y0=mean(Dev.factor.Pré.F.L),y1=mean(Dev.factor.Post.F.L),
       code=2,lwd=1.5,length=0.15)
for (i in 1:length(Num.quest.L[is.na(Num.quest.L)==FALSE])) {
  
  arrows(x0=CorrectPré.F.L[i],x1=CorrectPost.F.L[i],y0=Dev.factor.Pré.F.L[i],y1=Dev.factor.Post.F.L[i],
         code=2,lwd=0.5,length=0.15,lty=3,col="gray35")
}
grid(nx=NULL, ny=NULL, col="gray19")
legend("bottomright",c("pré-test","post-test","Moyenne pré-test","Moyenne post-test"),
       cex = 0.9, pch = c(2,1,17,19),col="gray1")
text(x=1,y=1.15,xpd=TRUE,labels="b)",cex=1.5)
#######################################################Comparaison Wcst pas wcst#####################################################
data_H<-data[data$Sexe=="M",
data_F<-data[data$Sexe=="F",]
#data_H<-data[data$Inhibition==0,]
#data_F<-data[data$Inhibition==1,]

AbsPréH<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
APréH<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
BPréH<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CPréH<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
DPréH<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
EPréH<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 9:38) {
  for (i in 1:35) {
    if (is.na(data_H[i,j])==TRUE){
      AbsPréH[j-8]=AbsPréH[j-8]+1
    }
    else if (is.na(data_H[i,j])==FALSE & data_H[i,j]=="A")
    {APréH[j-8]<-APréH[j-8]+1}
    else if (is.na(data_H[i,j])==FALSE  & data_H[i,j]=="B")
    {BPréH[j-8]<-BPréH[j-8]+1}
    else if (is.na(data_H[i,j])==FALSE  & data_H[i,j]=="C")
    {CPréH[j-8]<-CPréH[j-8]+1}
    else if (is.na(data_H[i,j])==FALSE & data_H[i,j]=="D")
    {DPréH[j-8]<-DPréH[j-8]+1}
    else if (is.na(data_H[i,j])==FALSE & data_H[i,j]=="E")
    {EPréH[j-8]<-EPréH[j-8]+1}
  }
}
print(APréH+BPréH+CPréH+DPréH+EPréH+AbsPréH)

AbsPostH<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
APostH<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
BPostH<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CPostH<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
DPostH<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
EPostH<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 40:69) {
  for (i in 1:35) {
    if (is.na(data_H[i,j])==TRUE){
      AbsPostH[j-39]=AbsPostH[j-39]+1
    }
    else if (is.na(data_H[i,j])==FALSE & data_H[i,j]=="A")
    {APostH[j-39]<-APostH[j-39]+1}
    else if (is.na(data_H[i,j])==FALSE & data_H[i,j]=="B")
    {BPostH[j-39]<-BPostH[j-39]+1}
    else if (is.na(data_H[i,j])==FALSE & data_H[i,j]=="C")
    {CPostH[j-39]<-CPostH[j-39]+1}
    else if (is.na(data_H[i,j])==FALSE & data_H[i,j]=="D")
    {DPostH[j-39]<-DPostH[j-39]+1}
    else if (is.na(data_H[i,j])==FALSE & data_H[i,j]=="E")
    {EPostH[j-39]<-EPostH[j-39]+1}
  }
}
print(APostH+BPostH+CPostH+DPostH+EPostH+AbsPostH)

Présence.PréH<- 35-AbsPréH 
Présence.PostH<- 35-AbsPostH

APréH<-APréH/Présence.PréH
BPréH<-BPréH/Présence.PréH
CPréH<-CPréH/Présence.PréH
DPréH<-DPréH/Présence.PréH
EPréH<-EPréH/Présence.PréH
APostH<-APostH/Présence.PostH
BPostH<-BPostH/Présence.PostH
CPostH<-CPostH/Présence.PostH
DPostH<-DPostH/Présence.PostH
EPostH<-EPostH/Présence.PostH

Comp_préH <- rbind(100*APréH,100*BPréH,100*CPréH,100*DPréH,100*EPréH)
Comp_postH<-rbind(100*APostH,100*BPostH,100*CPostH,100*DPostH,100*EPostH)

AbsPréF<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
APréF<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
BPréF<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CPréF<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
DPréF<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
EPréF<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 9:38) {
  for (i in 1:49) {
    if (is.na(data_F[i,j])==TRUE){
      AbsPréF[j-8]=AbsPréF[j-8]+1
    }
    else if (is.na(data_F[i,j])==FALSE & data_F[i,j]=="A")
    {APréF[j-8]<-APréF[j-8]+1}
    else if (is.na(data_F[i,j])==FALSE  & data_F[i,j]=="B")
    {BPréF[j-8]<-BPréF[j-8]+1}
    else if (is.na(data_F[i,j])==FALSE  & data_F[i,j]=="C")
    {CPréF[j-8]<-CPréF[j-8]+1}
    else if (is.na(data_F[i,j])==FALSE & data_F[i,j]=="D")
    {DPréF[j-8]<-DPréF[j-8]+1}
    else if (is.na(data_F[i,j])==FALSE & data_F[i,j]=="E")
    {EPréF[j-8]<-EPréF[j-8]+1}
  }
}
print(APréF+BPréF+CPréF+DPréF+EPréF+AbsPréF)

AbsPostF<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
APostF<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
BPostF<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CPostF<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
DPostF<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
EPostF<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 40:69) {
  for (i in 1:49) {
    if (is.na(data_F[i,j])==TRUE){
      AbsPostF[j-39]=AbsPostF[j-39]+1
    }
    else if (is.na(data_F[i,j])==FALSE & data_F[i,j]=="A")
    {APostF[j-39]<-APostF[j-39]+1}
    else if (is.na(data_F[i,j])==FALSE & data_F[i,j]=="B")
    {BPostF[j-39]<-BPostF[j-39]+1}
    else if (is.na(data_F[i,j])==FALSE & data_F[i,j]=="C")
    {CPostF[j-39]<-CPostF[j-39]+1}
    else if (is.na(data_F[i,j])==FALSE & data_F[i,j]=="D")
    {DPostF[j-39]<-DPostF[j-39]+1}
    else if (is.na(data_F[i,j])==FALSE & data_F[i,j]=="E")
    {EPostF[j-39]<-EPostF[j-39]+1}
  }
}
print(APostF+BPostF+CPostF+DPostF+EPostF+AbsPostF)

Présence.PréF<- 49-AbsPréF 
Présence.PostF<- 49-AbsPostF

APréF<-APréF/Présence.PréF
BPréF<-BPréF/Présence.PréF
CPréF<-CPréF/Présence.PréF
DPréF<-DPréF/Présence.PréF
EPréF<-EPréF/Présence.PréF
APostF<-APostF/Présence.PostF
BPostF<-BPostF/Présence.PostF
CPostF<-CPostF/Présence.PostF
DPostF<-DPostF/Présence.PostF
EPostF<-EPostF/Présence.PostF

Comp_préF <- rbind(100*APréF,100*BPréF,100*CPréF,100*DPréF,100*EPréF)
Comp_postF<-rbind(100*APostF,100*BPostF,100*CPostF,100*DPostF,100*EPostF)

par(mfrow = c(2,1))
Questions<-c(1:30)

mp<-barplot(Comp_préH, main = "Pourcentage des différents types de réponses
  question par question au FCI au pré-test pour les élèves n'ayant pas
  participé au WCST (N=35)", 
            xlab = "Numéro des questions du FCI", ylab = "Pourcentage des différents types de réponses", 
            ylim=c(0,100),beside=TRUE,names.arg=Questions, col=colors,cex.lab=0.85)
grid(nx=NA, ny=NULL, col="gray19")
legend("topleft",c("réponse incorrecte","réponse correcte"),
       cex = 0.6, fill = c("tomato","lawngreen"))

mp<-barplot(Comp_postH, main = "Pourcentage des différents types de réponses
  question par question au FCI au post-test pour les élèves n'ayant pas 
  participé au WCST (N=35)", 
            xlab = "Numéro des questions du FCI", ylab = "Pourcentage des différents types de réponses", 
            ylim=c(0,100),beside=TRUE,names.arg=Questions, col=colors,cex.lab=0.85)
grid(nx=NA, ny=NULL, col="gray19")
legend("topleft",c("réponse incorrecte","réponse correcte"),
       cex = 0.6, fill = c("tomato","lawngreen"))
#######################################################Comparaison pré H/F et wcst#######################################################
data_I1_H<-data[data$Inhibition==1 & data$Sexe=="M",]
data_I1_F<-data[data$Inhibition==1 & data$Sexe=="F",]
data_I0_H<-data[data$Inhibition==0 & data$Sexe=="M",]
data_I0_F<-data[data$Inhibition==0 & data$Sexe=="F",]

AbsPréHI0<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CorrectPréHI0<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WrongPréHI0<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 9:38) {
  for (i in 1:13) {
    if (is.na(data_I0_H[i,j])==TRUE){
      AbsPréHI0[j-8]=AbsPréHI0[j-8]+1
    }
    else if (is.na(data_I0_H[i,j])==FALSE & data_I0_H[i,j]!=Items[j-8])
    {WrongPréHI0[j-8]<-WrongPréHI0[j-8]+1}
    else if (is.na(data_I0_H[i,j])==FALSE & data_I0_H[i,j]==Items[j-8]) 
    {CorrectPréHI0[j-8]<-CorrectPréHI0[j-8]+1}
  }
}
print(AbsPréHI0+CorrectPréHI0+WrongPréHI0)

AbsPréFI0<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CorrectPréFI0<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WrongPréFI0<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 9:38) {
  for (i in 1:22) {
    if (is.na(data_I0_F[i,j])==TRUE){
      AbsPréFI0[j-8]=AbsPréFI0[j-8]+1
    }
    else if (is.na(data_I0_F[i,j])==FALSE & data_I0_F[i,j]!=Items[j-8])
    {WrongPréFI0[j-8]<-WrongPréFI0[j-8]+1}
    else if (is.na(data_I0_F[i,j])==FALSE & data_I0_F[i,j]==Items[j-8]) 
    {CorrectPréFI0[j-8]<-CorrectPréFI0[j-8]+1}
  }
}
print(AbsPréFI0+CorrectPréFI0+WrongPréFI0)

AbsPréHI1<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CorrectPréHI1<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WrongPréHI1<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 9:38) {
  for (i in 1:24) {
    if (is.na(data_I1_H[i,j])==TRUE){
      AbsPréHI1[j-8]=AbsPréHI1[j-8]+1
    }
    else if (is.na(data_I1_H[i,j])==FALSE & data_I1_H[i,j]!=Items[j-8])
    {WrongPréHI1[j-8]<-WrongPréHI1[j-8]+1}
    else if (is.na(data_I1_H[i,j])==FALSE & data_I1_H[i,j]==Items[j-8]) 
    {CorrectPréHI1[j-8]<-CorrectPréHI1[j-8]+1}
  }
}
print(AbsPréHI1+CorrectPréHI1+WrongPréHI1)

AbsPréFI1<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CorrectPréFI1<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WrongPréFI1<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 9:38) {
  for (i in 1:25) {
    if (is.na(data_I1_F[i,j])==TRUE){
      AbsPréFI1[j-8]=AbsPréFI1[j-8]+1
    }
    else if (is.na(data_I1_F[i,j])==FALSE & data_I1_F[i,j]!=Items[j-8])
    {WrongPréFI1[j-8]<-WrongPréFI1[j-8]+1}
    else if (is.na(data_I1_F[i,j])==FALSE & data_I1_F[i,j]==Items[j-8]) 
    {CorrectPréFI1[j-8]<-CorrectPréFI1[j-8]+1}
  }
}
print(AbsPréFI1+CorrectPréFI1+WrongPréFI1)

AbsPostHI0<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CorrectPostHI0<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WrongPostHI0<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 40:69) {
  for (i in 1:13) {
    if (is.na(data_I0_H[i,j])==TRUE){
      AbsPostHI0[j-39]=AbsPostHI0[j-39]+1
    }
    else if (is.na(data_I0_H[i,j])==FALSE & data_I0_H[i,j]!=Items[j-39])
    {WrongPostHI0[j-39]<-WrongPostHI0[j-39]+1}
    else if (is.na(data_I0_H[i,j])==FALSE & data_I0_H[i,j]==Items[j-39]) 
    {CorrectPostHI0[j-39]<-CorrectPostHI0[j-39]+1}
  }
}
print(AbsPostHI0+CorrectPostHI0+WrongPostHI0)

AbsPostHI1<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CorrectPostHI1<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WrongPostHI1<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 40:69) {
  for (i in 1:24) {
    if (is.na(data_I1_H[i,j])==TRUE){
      AbsPostHI1[j-39]=AbsPostHI1[j-39]+1
    }
    else if (is.na(data_I1_H[i,j])==FALSE & data_I1_H[i,j]!=Items[j-39])
    {WrongPostHI1[j-39]<-WrongPostHI1[j-39]+1}
    else if (is.na(data_I1_H[i,j])==FALSE & data_I1_H[i,j]==Items[j-39]) 
    {CorrectPostHI1[j-39]<-CorrectPostHI1[j-39]+1}
  }
}
print(AbsPostHI1+CorrectPostHI1+WrongPostHI1)

AbsPostFI0<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CorrectPostFI0<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WrongPostFI0<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 40:69) {
  for (i in 1:22) {
    if (is.na(data_I0_F[i,j])==TRUE){
      AbsPostFI0[j-39]=AbsPostFI0[j-39]+1
    }
    else if (is.na(data_I0_F[i,j])==FALSE & data_I0_F[i,j]!=Items[j-39])
    {WrongPostFI0[j-39]<-WrongPostFI0[j-39]+1}
    else if (is.na(data_I0_F[i,j])==FALSE & data_I0_F[i,j]==Items[j-39]) 
    {CorrectPostFI0[j-39]<-CorrectPostFI0[j-39]+1}
  }
}
print(AbsPostFI0+CorrectPostFI0+WrongPostFI0)

AbsPostFI1<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CorrectPostFI1<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WrongPostFI1<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 40:69) {
  for (i in 1:25) {
    if (is.na(data_I1_F[i,j])==TRUE){
      AbsPostFI1[j-39]=AbsPostFI1[j-39]+1
    }
    else if (is.na(data_I1_F[i,j])==FALSE & data_I1_F[i,j]!=Items[j-39])
    {WrongPostFI1[j-39]<-WrongPostFI1[j-39]+1}
    else if (is.na(data_I1_F[i,j])==FALSE & data_I1_F[i,j]==Items[j-39]) 
    {CorrectPostFI1[j-39]<-CorrectPostFI1[j-39]+1}
  }
}
print(AbsPostFI1+CorrectPostFI1+WrongPostFI1)

Présence.PréHI0<- 13-AbsPréHI0 
Présence.PréFI0<- 22-AbsPréFI0
Présence.PréHI1<- 24-AbsPréHI1 
Présence.PréFI1<- 25-AbsPréFI1
Présence.PostHI0<- 13-AbsPostHI0 
Présence.PostFI0<- 22-AbsPostFI0
Présence.PostHI1<- 24-AbsPostHI1 
Présence.PostFI1<- 25-AbsPostFI1

CorrectPréHI0<-CorrectPréHI0/Présence.PréHI0
CorrectPréFI0<-CorrectPréFI0/Présence.PréFI0
CorrectPréHI1<-CorrectPréHI1/Présence.PréHI1
CorrectPréFI1<-CorrectPréFI1/Présence.PréFI1
CorrectPostHI0<-CorrectPostHI0/Présence.PostHI0
CorrectPostFI0<-CorrectPostFI0/Présence.PostFI0
CorrectPostHI1<-CorrectPostHI1/Présence.PostHI1
CorrectPostFI1<-CorrectPostFI1/Présence.PostFI1

Comp_H_pré_wcst <- rbind(100*CorrectPréHI0,100*CorrectPréHI1,100*CorrectPostHI0,100*CorrectPostHI1)
Comp_F_pré_wcst <- rbind(100*CorrectPréFI0,100*CorrectPréFI1,100*CorrectPostFI0,100*CorrectPostFI1)
Questions<-c(1:30)
mp<-barplot(Comp_H_pré_wcst, main = "Pourcentage de réponses correctes pour les garçons
  au pré-test et au post-test question par question ", 
            xlab = "Numéro des questions du FCI", ylab = "Pourcentage de réponses correctes", 
            ylim=c(0,110),beside=TRUE,names.arg=Questions, col=c("dodgerblue1","dodgerblue4","coral1","coral4"),cex.lab=0.85)
grid(nx=NA, ny=NULL, col="gray19")
abline(h = mean(100*CorrectPréHI0),
       col = "dodgerblue1",
       lwd = 2)
abline(h = mean(100*CorrectPréHI1),
       col = "dodgerblue4",
       lwd = 2)
abline(h = mean(100*CorrectPostHI0),
       col = "coral1",
       lwd = 2)
abline(h = mean(100*CorrectPostHI1),
       col = "coral4",
       lwd = 2)
legend("topright",c("Pré-test sans WCST (N=13)","Pré-test avec WCST (N=24)","Post-test sans WCST (N=13)","Post-test avec WCST (N=24)"),cex = 0.8, fill = c("dodgerblue1","dodgerblue4","coral1","coral4"))

mp<-barplot(Comp_F_pré_wcst, main = "Pourcentage de réponses correctes pour les filles
  au pré-test et au post-test question par question ", 
            xlab = "Numéro des questions du FCI", ylab = "Pourcentage de réponses correctes", 
            ylim=c(0,110),beside=TRUE,names.arg=Questions, col=c("dodgerblue1","dodgerblue4","coral1","coral4"),cex.lab=0.85)
grid(nx=NA, ny=NULL, col="gray19")
abline(h = mean(100*CorrectPréFI0),
       col = "dodgerblue1",
       lwd = 2)
abline(h = mean(100*CorrectPréFI1),
       col = "dodgerblue4",
       lwd = 2)
abline(h = mean(100*CorrectPostFI0),
       col = "coral1",
       lwd = 2)
abline(h = mean(100*CorrectPostFI1),
       col = "coral4",
       lwd = 2)
legend("top",c("Pré-test sans WCST (N=22)","Pré-test avec WCST (N=25)","Post-test sans WCST (N=22)","Post-test avec WCST (N=25)"),cex = 0.8, fill = c("dodgerblue1","dodgerblue4","coral1","coral4"))
################################################facteur G Hake1998#################################

g.factor<-(S.post-S.pré)/(1-S.pré)
print(mean(g.factor))
print(sd(g.factor))
hist(g.factor,breaks=30)
#############################tests statistiques########################
##Test permutation lorsque distribution pas normal et distribution paramétriques différentes
library(coin)
x<-Dev.factor.post.I0.L
y<-Dev.factor.post.I1.L

x<-data_I1_H$ChangeRule[is.na(data_I1_H$ChangeRule)==FALSE]
y<-data_I1_F$ChangeRule[is.na(data_I1_F$ChangeRule)==FALSE]

x<-g.ave[data$Sexe=="F"]
y<-g.ave[data$Sexe=="M"]
list = c(x,y)
noms = factor(rep(c("A", "B"), c(length(x), length(y))))
boxplot(list~noms)
oneway_test(list ~ noms)

par(mfrow = c(1,2))
hist(x,ylab='densité',breaks=15,prob=TRUE,col="gray85")
lines(density(x[is.na(x)==FALSE],adjust=1),col="red")
hist(y,ylab='densité',breaks=15,prob=TRUE,col="gray85")
lines(density(y[is.na(y)==FALSE],adjust=1),col="red")

#Test wilcoxon pour distribution paramétriques identiques
 ##Paramètre paired=TRUE qd échantillons sont appariés et paired=FALSE lorsque indépendants
wilcox.test(x,y,paired=TRUE)


#test normalité
x<-residuals.lm(step.model2)
x<-data_I1$ChangeRule[data_I1$Sexe=="F" & is.na(data_I1$ChangeRule)==FALSE]
x<-data_potvin$g.pot
library(MASS)
library(fitdistrplus)
fit <- fitdistr(x, "normal")
class(fit)
para <- fit$estimate
par(mfrow = c(1,2))
hist(x,breaks=15,prob=TRUE,main="Histogramme de la variable gain FCI")
curve(dnorm(x, para[1], para[2]), col = 2, add = TRUE)
legend("topleft",c("Fit normal"), cex = 0.6, fill = c("red"))
qqnorm(x,datax=TRUE)
qqline(x, datax=TRUE)

cvm.test(g)
library(nortest)
ad.test(x)
shapiro.test(x)

#fit distribution gamma
x<-data$Score.post.test
fit.gamma <- fitdist(x, distr = "gamma", method = "mle")
plot(fit.gamma)

#homogénéité et variance des résidus
plot(step.model2)

#test corrélation entre les variables

columns <- c(3:15)
vars <- names(data_lm_I1)[columns]

out <-  apply( combn(columns,2),2,function(x){
  chisq.test(table(data_lm_I1[,x[1]],data_lm_I1[,x[2]]),correct=F)$p.value
})

out <- cbind(as.data.frame(t(combn(vars,2))),out)
print(out[out[,3]<0.05,])

###############################Analyse concentration WCST(test) / pas WCST(contrôle)#######################

###groupe avec WCST
AbsPré.I1<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CorrectPré.I1<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WrongPré.I1<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 9:38) {
  for (i in 1:nrow(data_I1)) {
    if (is.na(data_I1[i,j])==TRUE){
      AbsPré.I1[j-8]=AbsPré.I1[j-8]+1
    }
    else if (is.na(data_I1[i,j])==FALSE & data_I1[i,j]!=Items[j-8])
    {WrongPré.I1[j-8]<-WrongPré.I1[j-8]+1}
    else if (is.na(data_I1[i,j])==FALSE & data_I1[i,j]==Items[j-8]) 
    {CorrectPré.I1[j-8]<-CorrectPré.I1[j-8]+1}
  }
}

print(CorrectPré.I1+WrongPré.I1+AbsPré.I1)

AbsPost.I1<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CorrectPost.I1<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WrongPost.I1<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 40:69) {
  for (i in 1:nrow(data_I1)) {
    if (is.na(data_I1[i,j])==TRUE){
      AbsPost.I1[j-39]=AbsPost.I1[j-39]+1
    }
    else if (is.na(data_I1[i,j])==FALSE & data_I1[i,j]!=Items[j-39])
    {WrongPost.I1[j-39]<-WrongPost.I1[j-39]+1}
    else if (is.na(data_I1[i,j])==FALSE & data_I1[i,j]==Items[j-39]) 
    {CorrectPost.I1[j-39]<-CorrectPost.I1[j-39]+1}
  }
}

print(CorrectPost.I1+WrongPost.I1+AbsPost.I1)

#comparaison item par item 
AbsPré.I1<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
APré.I1<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
BPré.I1<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CPré.I1<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
DPré.I1<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
EPré.I1<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 9:38) {
  for (i in 1:nrow(data_I1)) {
    if (is.na(data_I1[i,j])==TRUE){
      AbsPré.I1[j-8]=AbsPré.I1[j-8]+1
    }
    else if (is.na(data_I1[i,j])==FALSE & data_I1[i,j]=="A")
    {APré.I1[j-8]<-APré.I1[j-8]+1}
    else if (is.na(data_I1[i,j])==FALSE  & data_I1[i,j]=="B")
    {BPré.I1[j-8]<-BPré.I1[j-8]+1}
    else if (is.na(data_I1[i,j])==FALSE  & data_I1[i,j]=="C")
    {CPré.I1[j-8]<-CPré.I1[j-8]+1}
    else if (is.na(data_I1[i,j])==FALSE & data_I1[i,j]=="D")
    {DPré.I1[j-8]<-DPré.I1[j-8]+1}
    else if (is.na(data_I1[i,j])==FALSE & data_I1[i,j]=="E")
    {EPré.I1[j-8]<-EPré.I1[j-8]+1}
  }
}
print(APré.I1+BPré.I1+CPré.I1+DPré.I1+EPré.I1+AbsPré.I1)


AbsPost.I1<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
APost.I1<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
BPost.I1<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CPost.I1<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
DPost.I1<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
EPost.I1<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 40:69) {
  for (i in 1:nrow(data_I1)) {
    if (is.na(data_I1[i,j])==TRUE){
      AbsPost.I1[j-39]=AbsPost.I1[j-39]+1
    }
    else if (is.na(data_I1[i,j])==FALSE & data_I1[i,j]=="A")
    {APost.I1[j-39]<-APost.I1[j-39]+1}
    else if (is.na(data_I1[i,j])==FALSE & data_I1[i,j]=="B")
    {BPost.I1[j-39]<-BPost.I1[j-39]+1}
    else if (is.na(data_I1[i,j])==FALSE & data_I1[i,j]=="C")
    {CPost.I1[j-39]<-CPost.I1[j-39]+1}
    else if (is.na(data_I1[i,j])==FALSE & data_I1[i,j]=="D")
    {DPost.I1[j-39]<-DPost.I1[j-39]+1}
    else if (is.na(data_I1[i,j])==FALSE & data_I1[i,j]=="E")
    {EPost.I1[j-39]<-EPost.I1[j-39]+1}
  }
}
print(APost.I1+BPost.I1+CPost.I1+DPost.I1+EPost.I1+AbsPost.I1)

N<-nrow(data_I1)
m<-5

C.factor.pré.I1<-c()
C.factor.post.I1<-c()
for (i in 1:30){
  C.factor.pré.I1[i]<-sqrt(m)/(sqrt(m)-1)*(sqrt((APré.I1[i])^2+(BPré.I1[i])^2+
                          (CPré.I1[i])^2+(DPré.I1[i])^2+(EPré.I1[i])^2)/N-1/sqrt(m))
  C.factor.post.I1[i]<-sqrt(m)/(sqrt(m)-1)*(sqrt((APost.I1[i])^2+(BPost.I1[i])^2+
                        (CPost.I1[i])^2+(DPost.I1[i])^2+(EPost.I1[i])^2)/N-1/sqrt(m))
}
print(C.factor.post.I1)

Dev.factor.pré.I1<-c()
Dev.factor.post.I1<-c()
for (i in 1:30){
  Dev.factor.pré.I1[i]<-sqrt(m-1)/(sqrt(m-1)-1)*(sqrt((APré.I1[i])^2+(BPré.I1[i])^2+(CPré.I1[i])^2
                            +(DPré.I1[i])^2+(EPré.I1[i])^2-CorrectPré.I1[i]^2)/(N-CorrectPré.I1[i])-1/sqrt(m-1))
  Dev.factor.post.I1[i]<-sqrt(m-1)/(sqrt(m-1)-1)*(sqrt((APost.I1[i])^2+(BPost.I1[i])^2+(CPost.I1[i])^2+
                              (DPost.I1[i])^2+(EPost.I1[i])^2-CorrectPost.I1[i]^2)/(N-CorrectPost.I1[i])-1/sqrt(m-1))
}
#Facteur déviation pour les questions ayant le niveau "L" pour le score
Dev.factor.pré.I1.L<-Dev.factor.pré.I1[is.na(Num.quest.L)==FALSE]
Dev.factor.post.I1.L<-Dev.factor.post.I1[is.na(Num.quest.L)==FALSE]

Présence.Pré.I1<- nrow(data_I1)-AbsPré.I1 
Présence.Post.I1<- nrow(data_I1)-AbsPost.I1

CorrectPré.I1<-CorrectPré.I1/Présence.Pré.I1
CorrectPost.I1<-CorrectPost.I1/Présence.Post.I1

S.pré.I1<-CorrectPré.I1
S.post.I1<-CorrectPost.I1

#Score pour les questions ayant le niveau "L" pour le score

S.pré.I1.L<-S.pré.I1[is.na(Num.quest.L)==FALSE]
S.post.I1.L<-S.post.I1[is.na(Num.quest.L)==FALSE]

###groupe contrôle
AbsPré.I0<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CorrectPré.I0<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WrongPré.I0<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 9:38) {
  for (i in 1:nrow(data_I0)) {
    if (is.na(data_I0[i,j])==TRUE){
      AbsPré.I0[j-8]=AbsPré.I0[j-8]+1
    }
    else if (is.na(data_I0[i,j])==FALSE & data_I0[i,j]!=Items[j-8])
    {WrongPré.I0[j-8]<-WrongPré.I0[j-8]+1}
    else if (is.na(data_I0[i,j])==FALSE & data_I0[i,j]==Items[j-8]) 
    {CorrectPré.I0[j-8]<-CorrectPré.I0[j-8]+1}
  }
}

print(CorrectPré.I0+WrongPré.I0+AbsPré.I0)

AbsPost.I0<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CorrectPost.I0<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WrongPost.I0<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 40:69) {
  for (i in 1:nrow(data_I0)) {
    if (is.na(data_I0[i,j])==TRUE){
      AbsPost.I0[j-39]=AbsPost.I0[j-39]+1
    }
    else if (is.na(data_I0[i,j])==FALSE & data_I0[i,j]!=Items[j-39])
    {WrongPost.I0[j-39]<-WrongPost.I0[j-39]+1}
    else if (is.na(data_I0[i,j])==FALSE & data_I0[i,j]==Items[j-39]) 
    {CorrectPost.I0[j-39]<-CorrectPost.I0[j-39]+1}
  }
}

print(CorrectPost.I0+WrongPost.I0+AbsPost.I0)

#comparaison item par item 
AbsPré.I0<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
APré.I0<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
BPré.I0<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CPré.I0<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
DPré.I0<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
EPré.I0<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 9:38) {
  for (i in 1:nrow(data_I0)) {
    if (is.na(data_I0[i,j])==TRUE){
      AbsPré.I0[j-8]=AbsPré.I0[j-8]+1
    }
    else if (is.na(data_I0[i,j])==FALSE & data_I0[i,j]=="A")
    {APré.I0[j-8]<-APré.I0[j-8]+1}
    else if (is.na(data_I0[i,j])==FALSE  & data_I0[i,j]=="B")
    {BPré.I0[j-8]<-BPré.I0[j-8]+1}
    else if (is.na(data_I0[i,j])==FALSE  & data_I0[i,j]=="C")
    {CPré.I0[j-8]<-CPré.I0[j-8]+1}
    else if (is.na(data_I0[i,j])==FALSE & data_I0[i,j]=="D")
    {DPré.I0[j-8]<-DPré.I0[j-8]+1}
    else if (is.na(data_I0[i,j])==FALSE & data_I0[i,j]=="E")
    {EPré.I0[j-8]<-EPré.I0[j-8]+1}
  }
}
print(APré.I0+BPré.I0+CPré.I0+DPré.I0+EPré.I0+AbsPré.I0)


AbsPost.I0<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
APost.I0<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
BPost.I0<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CPost.I0<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
DPost.I0<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
EPost.I0<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 40:69) {
  for (i in 1:nrow(data_I0)) {
    if (is.na(data_I0[i,j])==TRUE){
      AbsPost.I0[j-39]=AbsPost.I0[j-39]+1
    }
    else if (is.na(data_I0[i,j])==FALSE & data_I0[i,j]=="A")
    {APost.I0[j-39]<-APost.I0[j-39]+1}
    else if (is.na(data_I0[i,j])==FALSE & data_I0[i,j]=="B")
    {BPost.I0[j-39]<-BPost.I0[j-39]+1}
    else if (is.na(data_I0[i,j])==FALSE & data_I0[i,j]=="C")
    {CPost.I0[j-39]<-CPost.I0[j-39]+1}
    else if (is.na(data_I0[i,j])==FALSE & data_I0[i,j]=="D")
    {DPost.I0[j-39]<-DPost.I0[j-39]+1}
    else if (is.na(data_I0[i,j])==FALSE & data_I0[i,j]=="E")
    {EPost.I0[j-39]<-EPost.I0[j-39]+1}
  }
}
AbsPost.I0[25]=1
print(APost.I0+BPost.I0+CPost.I0+DPost.I0+EPost.I0+AbsPost.I0)

N<-nrow(data_I0)
m<-5
C.factor.pré.I0<-c()
C.factor.post.I0<-c()
for (i in 1:30){
  C.factor.pré.I0[i]<-sqrt(m)/(sqrt(m)-1)*(sqrt((APré.I0[i])^2+(BPré.I0[i])^2+
                                                       (CPré.I0[i])^2+(DPré.I0[i])^2+(EPré.I0[i])^2)/N-1/sqrt(m))
  C.factor.post.I0[i]<-sqrt(m)/(sqrt(m)-1)*(sqrt((APost.I0[i])^2+(BPost.I0[i])^2+
                                                        (CPost.I0[i])^2+(DPost.I0[i])^2+(EPost.I0[i])^2)/N-1/sqrt(m))
}

Dev.factor.pré.I0<-c()
Dev.factor.post.I0<-c()
for (i in 1:30){
  Dev.factor.pré.I0[i]<-sqrt(m-1)/(sqrt(m-1)-1)*(sqrt((APré.I0[i])^2+(BPré.I0[i])^2+(CPré.I0[i])^2
                                                       +(DPré.I0[i])^2+(EPré.I0[i])^2-CorrectPré.I0[i]^2)/(N-CorrectPré.I0[i])-1/sqrt(m-1))
  Dev.factor.post.I0[i]<-sqrt(m-1)/(sqrt(m-1)-1)*(sqrt((APost.I0[i])^2+(BPost.I0[i])^2+(CPost.I0[i])^2+
                                                          (DPost.I0[i])^2+(EPost.I0[i])^2-CorrectPost.I0[i]^2)/(N-CorrectPost.I0[i])-1/sqrt(m-1))
}
#Facteur déviation pour les questions ayant le niveau "L" pour le score
Dev.factor.pré.I0.L<-Dev.factor.pré.I0[is.na(Num.quest.L)==FALSE]
Dev.factor.post.I0.L<-Dev.factor.post.I0[is.na(Num.quest.L)==FALSE]

Présence.Pré.I0<- nrow(data_I0)-AbsPré.I0 
Présence.Post.I0<- nrow(data_I0)-AbsPost.I0

CorrectPré.I0<-CorrectPré.I0/Présence.Pré.I0
CorrectPost.I0<-CorrectPost.I0/Présence.Post.I0

S.pré.I0<-CorrectPré.I0
S.post.I0<-CorrectPost.I0

#Score pour les questions ayant le niveau "L" pour le score

S.pré.I0.L<-S.pré.I0[is.na(Num.quest.L)==FALSE]
S.post.I0.L<-S.post.I0[is.na(Num.quest.L)==FALSE]

##Plot S-C comparaison groupe WCST fort/faible

par(mfrow = c(1,2))
plot(C.factor.pré.I1~ S.pré.I1,xlim=c(0,1),ylim=c(0,1),main="Graphe S-C groupe test",col="gray1",pch=2,ylab="Facteur de concentration C",
     xlab="Score S",lwd=1.8)
#text(x=S.pré.I1+0.02,y=C.factor.pré.I1,labels=c(1:30))
points(x=S.post.I1,y=C.factor.post.I1,pch=1,col="gray1",lwd=1.8)
#text(x=S.post.I1+0.02,y=C.factor.post.I1,labels=c(1:30))
abline(h=c(0.2,0.5),v=c(0.4,0.7), col="gray45")
lines(seq(from=0,to=1,by=0.001),Cmax(seq(from=0,to=1,by=0.001)),
      type = "l")
lines(seq(from=0,to=1,by=0.001),Cmin(seq(from=0,to=1,by=0.001)),
      type = "l")
points(x=c(mean(S.pré.I1),mean(S.post.I1)),y=c(mean(C.factor.pré.I1),mean(C.factor.post.I1))
       ,pch=c(17,19),col="gray1",cex=1.5)
arrows(x0=mean(S.pré.I1),x1=mean(S.post.I1),y0=mean(C.factor.pré.I1),y1=mean(C.factor.post.I1),
       code=2,lwd=1.5,length=0.15)
legend("topleft",c("pré-test","post-test","Moyenne pré-test","Moyenne post-test"),
       cex = 0.9, pch = c(2,1,17,19),col="gray1")
text(x=1.1,y=1.15,xpd=TRUE,labels="a)",cex=1.5)
## groupe contrôle
plot(C.factor.pré.I0~ S.pré.I0,xlim=c(0,1),ylim=c(0,1),main="Graphe S-C groupe contrôle",col="gray1",pch=2,ylab="Facteur de concentration C",
     xlab="Score S",lwd=1.8)
#text(x=S.pré.I0+0.02,y=C.factor.pré.I0,labels=c(1:30))
points(x=S.post.I0,y=C.factor.post.I0,pch=1,col="gray1",lwd=1.8)
#text(x=S.post.I0+0.02,y=C.factor.post.I0,labels=c(1:30))
abline(h=c(0.2,0.5),v=c(0.4,0.7), col="gray45")
lines(seq(from=0,to=1,by=0.001),Cmax(seq(from=0,to=1,by=0.001)),
      type = "l")
lines(seq(from=0,to=1,by=0.001),Cmin(seq(from=0,to=1,by=0.001)),
      type = "l")
points(x=c(mean(S.pré.I0),mean(S.post.I0)),y=c(mean(C.factor.pré.I0),mean(C.factor.post.I0))
       ,pch=c(17,19),col="gray1",cex=1.5)
arrows(x0=mean(S.pré.I0),x1=mean(S.post.I0),y0=mean(C.factor.pré.I0),y1=mean(C.factor.post.I0),
       code=2,lwd=1.5,length=0.15)
legend("topleft",c("pré-test","post-test","Moyenne pré-test","Moyenne post-test"),
       cex = 0.9, pch = c(2,1,17,19),col="gray1")
text(x=1.1,y=1.15,xpd=TRUE,labels="b)",cex=1.5)

g.I1<-(S.post.I1-S.pré.I1)/(1-S.pré.I1)
mean(g.I1)
sd(g.I1)
g.I0<-(S.post.I0-S.pré.I0)/(1-S.pré.I0)
mean(g.I0)
sd(g.I0)
##Plot Dev-C comparaison groupe test/contrôle
par(mfrow = c(1,2))
plot(Dev.factor.pré.I1~ S.pré.I1,xlim=c(0,1),ylim=c(0,1),main="Graphe S-"* Gamma ~ "groupe test",col="gray1",pch=2,
     ylab="Facteur de déviation" ~ Gamma, xlab="Score par question",lwd=1.8)
text(x=S.pré.I1+0.02,y=Dev.factor.pré.I1,labels=c(1:30),cex=0.6)
points(x=S.post.I1,y=Dev.factor.post.I1,pch=1,col="gray1",lwd=1.8)
text(x=S.post.I1+0.02,y=Dev.factor.post.I1,labels=c(1:30),cex=0.6)

points(x=c(mean(S.pré.I1),mean(S.post.I1)),y=c(mean(Dev.factor.pré.I1),mean(Dev.factor.post.I1))
       ,pch=c(17,19),col="gray1",cex=1.5)
arrows(x0=mean(S.pré.I1),x1=mean(S.post.I1),y0=mean(Dev.factor.pré.I1),y1=mean(Dev.factor.post.I1),
       code=2,lwd=1.5,length=0.15)
grid(nx=NULL, ny=NULL, col="gray19")
legend("topleft",c("pré-test","post-test","Moyenne pré-test","Moyenne post-test"),
       cex = 0.9, pch = c(2,1,17,19),col="gray1")
text(x=1,y=1.15,xpd=TRUE,labels="a)",cex=1.5)

## groupe contrôle
plot(Dev.factor.pré.I0~ S.pré.I0,xlim=c(0,1),ylim=c(0,1),main="Graphe S-"* Gamma ~ "groupe contrôle",
     col="gray1",pch=2,ylab="Facteur de déviation" ~ Gamma ,xlab="Score par question",lwd=1.8)
#text(x=S.pré.I0+0.02,y=Dev.factor.pré.I0,labels=c(1:30),cex=0.6)
points(x=S.post.I0,y=Dev.factor.post.I0,pch=1,col="gray1",lwd=1.8)
#text(x=S.post.I0+0.02,y=Dev.factor.post.I0,labels=c(1:30),cex=0.6)

points(x=c(mean(S.pré.I0),mean(S.post.I0)),y=c(mean(Dev.factor.pré.I0),mean(Dev.factor.post.I0))
       ,pch=c(17,19),col="gray1",cex=1.5)
arrows(x0=mean(S.pré.I0),x1=mean(S.post.I0),y0=mean(Dev.factor.pré.I0),y1=mean(Dev.factor.post.I0),
       code=2,lwd=1.5,length=0.15)
grid(nx=NULL, ny=NULL, col="gray19")
legend("topleft",c("pré-test","post-test","Moyenne pré-test","Moyenne post-test"),
       cex = 0.9, pch = c(2,1,17,19),col="gray1")
text(x=1,y=1.15,xpd=TRUE,labels="b)",cex=1.5)


##Plot Dev-C comparaison groupe test/contrôle pour les questions de type L
par(mfrow = c(1,2))
plot(Dev.factor.pré.I1.L~ S.pré.I1.L,xlim=c(0,1),ylim=c(0,1),main="Graphe S-"* Gamma ~ "groupe test",
     col="gray1",pch=2,ylab="Facteur de déviation" ~ Gamma,xlab="Score par question",lwd=1.8)
text(x=S.pré.I1.L+0.02,y=Dev.factor.pré.I1.L,labels=Num.quest.L[is.na(Num.quest.L)==FALSE],cex=0.8)
points(x=S.post.I1.L,y=Dev.factor.post.I1.L,pch=1,col="gray1",lwd=1.8)
text(x=S.post.I1.L+0.02,y=Dev.factor.post.I1.L,labels=Num.quest.L[is.na(Num.quest.L)==FALSE],cex=0.8)

points(x=c(mean(S.pré.I1.L),mean(S.post.I1.L)),y=c(mean(Dev.factor.pré.I1.L),mean(Dev.factor.post.I1.L))
       ,pch=c(17,19),col="gray1",cex=1.5)
arrows(x0=mean(S.pré.I1.L),x1=mean(S.post.I1.L),y0=mean(Dev.factor.pré.I1.L),y1=mean(Dev.factor.post.I1.L),
       code=2,lwd=1.5,length=0.15)
for (i in 1:length(Num.quest.L[is.na(Num.quest.L)==FALSE])) {
  
  arrows(x0=S.pré.I1.L[i],x1=S.post.I1.L[i],y0=Dev.factor.pré.I1.L[i],y1=Dev.factor.post.I1.L[i],
         code=2,lwd=0.5,length=0.15,lty=3,col="gray35")
}
grid(nx=NULL, ny=NULL, col="gray19")
legend("bottomright",c("pré-test","post-test","Moyenne pré-test","Moyenne post-test"),
       cex = 0.9, pch = c(2,1,17,19),col="gray1")
text(x=1,y=1.15,xpd=TRUE,labels="a)",cex=1.5)
## groupe contrôle
plot(Dev.factor.pré.I0.L~ S.pré.I0.L,xlim=c(0,1),ylim=c(0,1),main="Graphe S-"* Gamma ~ "groupe contrôle",
     col="gray1",pch=2,ylab="Facteur de déviation" ~ Gamma,
     xlab="Score par question",lwd=1.8)
text(x=S.pré.I0.L+0.02,y=Dev.factor.pré.I0.L,labels=Num.quest.L[is.na(Num.quest.L)==FALSE],cex=0.8)
points(x=S.post.I0.L,y=Dev.factor.post.I0.L,pch=1,col="gray1",lwd=1.8)
text(x=S.post.I0.L+0.02,y=Dev.factor.post.I0.L,labels=Num.quest.L[is.na(Num.quest.L)==FALSE],cex=0.8)

points(x=c(mean(S.pré.I0.L),mean(S.post.I0.L)),y=c(mean(Dev.factor.pré.I0.L),mean(Dev.factor.post.I0.L))
       ,pch=c(17,19),col="gray1",cex=1.5)
arrows(x0=mean(S.pré.I0.L),x1=mean(S.post.I0.L),y0=mean(Dev.factor.pré.I0.L),y1=mean(Dev.factor.post.I0.L),
       code=2,lwd=1.5,length=0.15)
for (i in 1:length(Num.quest.L[is.na(Num.quest.L)==FALSE])) {
  
  arrows(x0=S.pré.I0.L[i],x1=S.post.I0.L[i],y0=Dev.factor.pré.I0.L[i],y1=Dev.factor.post.I0.L[i],
         code=2,lwd=0.5,length=0.15,lty=3,col="gray35")
}
grid(nx=NULL, ny=NULL, col="gray19")
legend("bottomright",c("pré-test","post-test","Moyenne pré-test","Moyenne post-test"),
       cex = 0.9, pch = c(2,1,17,19),col="gray1")
text(x=1,y=1.15,xpd=TRUE,labels="b)",cex=1.5)

g.I1.L<-(S.post.I1.L-S.pré.I1.L)/(1-S.pré.I1.L)
mean(g.I1.L)
sd(g.I1.L)

g.I0.L<-(S.post.I0.L-S.pré.I0.L)/(1-S.pré.I0.L)
mean(g.I0.L)
sd(g.I0.L)

################################Analyse concentration WCST F/ pas WCST F#################

###fille avec WCST
AbsPré.I1.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CorrectPré.I1.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WrongPré.I1.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 9:38) {
  for (i in 1:nrow(data_I1_F)) {
    if (is.na(data_I1_F[i,j])==TRUE){
      AbsPré.I1.F[j-8]=AbsPré.I1.F[j-8]+1
    }
    else if (is.na(data_I1_F[i,j])==FALSE & data_I1_F[i,j]!=Items[j-8])
    {WrongPré.I1.F[j-8]<-WrongPré.I1.F[j-8]+1}
    else if (is.na(data_I1_F[i,j])==FALSE & data_I1_F[i,j]==Items[j-8]) 
    {CorrectPré.I1.F[j-8]<-CorrectPré.I1.F[j-8]+1}
  }
}

print(CorrectPré.I1.F+WrongPré.I1.F+AbsPré.I1.F)

AbsPost.I1.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CorrectPost.I1.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WrongPost.I1.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 40:69) {
  for (i in 1:nrow(data_I1_F)) {
    if (is.na(data_I1_F[i,j])==TRUE){
      AbsPost.I1.F[j-39]=AbsPost.I1.F[j-39]+1
    }
    else if (is.na(data_I1_F[i,j])==FALSE & data_I1_F[i,j]!=Items[j-39])
    {WrongPost.I1.F[j-39]<-WrongPost.I1.F[j-39]+1}
    else if (is.na(data_I1_F[i,j])==FALSE & data_I1_F[i,j]==Items[j-39]) 
    {CorrectPost.I1.F[j-39]<-CorrectPost.I1.F[j-39]+1}
  }
}

print(CorrectPost.I1.F+WrongPost.I1.F+AbsPost.I1.F)

#comparaison item par item 
AbsPré.I1.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
APré.I1.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
BPré.I1.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CPré.I1.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
DPré.I1.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
EPré.I1.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 9:38) {
  for (i in 1:nrow(data_I1_F)) {
    if (is.na(data_I1_F[i,j])==TRUE){
      AbsPré.I1.F[j-8]=AbsPré.I1.F[j-8]+1
    }
    else if (is.na(data_I1_F[i,j])==FALSE & data_I1_F[i,j]=="A")
    {APré.I1.F[j-8]<-APré.I1.F[j-8]+1}
    else if (is.na(data_I1_F[i,j])==FALSE  & data_I1_F[i,j]=="B")
    {BPré.I1.F[j-8]<-BPré.I1.F[j-8]+1}
    else if (is.na(data_I1_F[i,j])==FALSE  & data_I1_F[i,j]=="C")
    {CPré.I1.F[j-8]<-CPré.I1.F[j-8]+1}
    else if (is.na(data_I1_F[i,j])==FALSE & data_I1_F[i,j]=="D")
    {DPré.I1.F[j-8]<-DPré.I1.F[j-8]+1}
    else if (is.na(data_I1_F[i,j])==FALSE & data_I1_F[i,j]=="E")
    {EPré.I1.F[j-8]<-EPré.I1.F[j-8]+1}
  }
}
print(APré.I1.F+BPré.I1.F+CPré.I1.F+DPré.I1.F+EPré.I1.F+AbsPré.I1.F)


AbsPost.I1.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
APost.I1.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
BPost.I1.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CPost.I1.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
DPost.I1.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
EPost.I1.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 40:69) {
  for (i in 1:nrow(data_I1_F)) {
    if (is.na(data_I1_F[i,j])==TRUE){
      AbsPost.I1.F[j-39]=AbsPost.I1.F[j-39]+1
    }
    else if (is.na(data_I1_F[i,j])==FALSE & data_I1_F[i,j]=="A")
    {APost.I1.F[j-39]<-APost.I1.F[j-39]+1}
    else if (is.na(data_I1_F[i,j])==FALSE & data_I1_F[i,j]=="B")
    {BPost.I1.F[j-39]<-BPost.I1.F[j-39]+1}
    else if (is.na(data_I1_F[i,j])==FALSE & data_I1_F[i,j]=="C")
    {CPost.I1.F[j-39]<-CPost.I1.F[j-39]+1}
    else if (is.na(data_I1_F[i,j])==FALSE & data_I1_F[i,j]=="D")
    {DPost.I1.F[j-39]<-DPost.I1.F[j-39]+1}
    else if (is.na(data_I1_F[i,j])==FALSE & data_I1_F[i,j]=="E")
    {EPost.I1.F[j-39]<-EPost.I1.F[j-39]+1}
  }
}
AbsPost.I1.F[25]=1
AbsPost.I1.F[29]=3
print(APost.I1.F+BPost.I1.F+CPost.I1.F+DPost.I1.F+EPost.I1.F+AbsPost.I1.F)

N<-nrow(data_I1_F)
m<-5

C.factor.pré.I1.F<-c()
C.factor.post.I1.F<-c()
for (i in 1:30){
  C.factor.pré.I1.F[i]<-sqrt(m)/(sqrt(m)-1)*(sqrt((APré.I1.F[i])^2+(BPré.I1.F[i])^2+
                                                  (CPré.I1.F[i])^2+(DPré.I1.F[i])^2+(EPré.I1.F[i])^2)/N-1/sqrt(m))
  C.factor.post.I1.F[i]<-sqrt(m)/(sqrt(m)-1)*(sqrt((APost.I1.F[i])^2+(BPost.I1.F[i])^2+
                                                   (CPost.I1.F[i])^2+(DPost.I1.F[i])^2+(EPost.I1.F[i])^2)/N-1/sqrt(m))
}
print(C.factor.post.I1.F)

Dev.factor.pré.I1.F<-c()
Dev.factor.post.I1.F<-c()
for (i in 1:30){
  Dev.factor.pré.I1.F[i]<-sqrt(m-1)/(sqrt(m-1)-1)*(sqrt((APré.I1.F[i])^2+(BPré.I1.F[i])^2+(CPré.I1.F[i])^2
                                                      +(DPré.I1.F[i])^2+(EPré.I1.F[i])^2-CorrectPré.I1.F[i]^2)/(N-CorrectPré.I1.F[i])-1/sqrt(m-1))
  Dev.factor.post.I1.F[i]<-sqrt(m-1)/(sqrt(m-1)-1)*(sqrt((APost.I1.F[i])^2+(BPost.I1.F[i])^2+(CPost.I1.F[i])^2+
                                                         (DPost.I1.F[i])^2+(EPost.I1.F[i])^2-CorrectPost.I1.F[i]^2)/(N-CorrectPost.I1.F[i])-1/sqrt(m-1))
}
#Facteur déviation pour les questions ayant le niveau "L" pour le score
Dev.factor.pré.I1.F.L<-Dev.factor.pré.I1.F[is.na(Num.quest.L)==FALSE]
Dev.factor.post.I1.F.L<-Dev.factor.post.I1.F[is.na(Num.quest.L)==FALSE]

Présence.Pré.I1.F<- nrow(data_I1_F)-AbsPré.I1.F 
Présence.Post.I1.F<- nrow(data_I1_F)-AbsPost.I1.F

CorrectPré.I1.F<-CorrectPré.I1.F/Présence.Pré.I1.F
CorrectPost.I1.F<-CorrectPost.I1.F/Présence.Post.I1.F

S.pré.I1.F<-CorrectPré.I1.F
S.post.I1.F<-CorrectPost.I1.F

#Score pour les questions ayant le niveau "L" pour le score

S.pré.I1.F.L<-S.pré.I1.F[is.na(Num.quest.L)==FALSE]
S.post.I1.F.L<-S.post.I1.F[is.na(Num.quest.L)==FALSE]

###Fille pas wcst
AbsPré.I0.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CorrectPré.I0.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WrongPré.I0.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 9:38) {
  for (i in 1:nrow(data_I0_F)) {
    if (is.na(data_I0_F[i,j])==TRUE){
      AbsPré.I0.F[j-8]=AbsPré.I0.F[j-8]+1
    }
    else if (is.na(data_I0_F[i,j])==FALSE & data_I0_F[i,j]!=Items[j-8])
    {WrongPré.I0.F[j-8]<-WrongPré.I0.F[j-8]+1}
    else if (is.na(data_I0_F[i,j])==FALSE & data_I0_F[i,j]==Items[j-8]) 
    {CorrectPré.I0.F[j-8]<-CorrectPré.I0.F[j-8]+1}
  }
}

print(CorrectPré.I0.F+WrongPré.I0.F+AbsPré.I0.F)

AbsPost.I0.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CorrectPost.I0.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WrongPost.I0.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 40:69) {
  for (i in 1:nrow(data_I0_F)) {
    if (is.na(data_I0_F[i,j])==TRUE){
      AbsPost.I0.F[j-39]=AbsPost.I0.F[j-39]+1
    }
    else if (is.na(data_I0_F[i,j])==FALSE & data_I0_F[i,j]!=Items[j-39])
    {WrongPost.I0.F[j-39]<-WrongPost.I0.F[j-39]+1}
    else if (is.na(data_I0_F[i,j])==FALSE & data_I0_F[i,j]==Items[j-39]) 
    {CorrectPost.I0.F[j-39]<-CorrectPost.I0.F[j-39]+1}
  }
}

print(CorrectPost.I0.F+WrongPost.I0.F+AbsPost.I0.F)

#comparaison item par item 
AbsPré.I0.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
APré.I0.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
BPré.I0.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CPré.I0.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
DPré.I0.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
EPré.I0.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 9:38) {
  for (i in 1:nrow(data_I0_F)) {
    if (is.na(data_I0_F[i,j])==TRUE){
      AbsPré.I0.F[j-8]=AbsPré.I0.F[j-8]+1
    }
    else if (is.na(data_I0_F[i,j])==FALSE & data_I0_F[i,j]=="A")
    {APré.I0.F[j-8]<-APré.I0.F[j-8]+1}
    else if (is.na(data_I0_F[i,j])==FALSE  & data_I0_F[i,j]=="B")
    {BPré.I0.F[j-8]<-BPré.I0.F[j-8]+1}
    else if (is.na(data_I0_F[i,j])==FALSE  & data_I0_F[i,j]=="C")
    {CPré.I0.F[j-8]<-CPré.I0.F[j-8]+1}
    else if (is.na(data_I0_F[i,j])==FALSE & data_I0_F[i,j]=="D")
    {DPré.I0.F[j-8]<-DPré.I0.F[j-8]+1}
    else if (is.na(data_I0_F[i,j])==FALSE & data_I0_F[i,j]=="E")
    {EPré.I0.F[j-8]<-EPré.I0.F[j-8]+1}
  }
}
print(APré.I0.F+BPré.I0.F+CPré.I0.F+DPré.I0.F+EPré.I0.F+AbsPré.I0.F)


AbsPost.I0.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
APost.I0.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
BPost.I0.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CPost.I0.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
DPost.I0.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
EPost.I0.F<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 40:69) {
  for (i in 1:nrow(data_I0_F)) {
    if (is.na(data_I0_F[i,j])==TRUE){
      AbsPost.I0.F[j-39]=AbsPost.I0.F[j-39]+1
    }
    else if (is.na(data_I0_F[i,j])==FALSE & data_I0_F[i,j]=="A")
    {APost.I0.F[j-39]<-APost.I0.F[j-39]+1}
    else if (is.na(data_I0_F[i,j])==FALSE & data_I0_F[i,j]=="B")
    {BPost.I0.F[j-39]<-BPost.I0.F[j-39]+1}
    else if (is.na(data_I0_F[i,j])==FALSE & data_I0_F[i,j]=="C")
    {CPost.I0.F[j-39]<-CPost.I0.F[j-39]+1}
    else if (is.na(data_I0_F[i,j])==FALSE & data_I0_F[i,j]=="D")
    {DPost.I0.F[j-39]<-DPost.I0.F[j-39]+1}
    else if (is.na(data_I0_F[i,j])==FALSE & data_I0_F[i,j]=="E")
    {EPost.I0.F[j-39]<-EPost.I0.F[j-39]+1}
  }
}
AbsPost.I0.F[25]=1
print(APost.I0.F+BPost.I0.F+CPost.I0.F+DPost.I0.F+EPost.I0.F+AbsPost.I0.F)

N<-nrow(data_I0_F)
m<-5
C.factor.pré.I0.F<-c()
C.factor.post.I0.F<-c()
for (i in 1:30){
  C.factor.pré.I0.F[i]<-sqrt(m)/(sqrt(m)-1)*(sqrt((APré.I0.F[i])^2+(BPré.I0.F[i])^2+
                                                  (CPré.I0.F[i])^2+(DPré.I0.F[i])^2+(EPré.I0.F[i])^2)/N-1/sqrt(m))
  C.factor.post.I0.F[i]<-sqrt(m)/(sqrt(m)-1)*(sqrt((APost.I0.F[i])^2+(BPost.I0.F[i])^2+
                                                   (CPost.I0.F[i])^2+(DPost.I0.F[i])^2+(EPost.I0.F[i])^2)/N-1/sqrt(m))
}

Dev.factor.pré.I0.F<-c()
Dev.factor.post.I0.F<-c()
for (i in 1:30){
  Dev.factor.pré.I0.F[i]<-sqrt(m-1)/(sqrt(m-1)-1)*(sqrt((APré.I0.F[i])^2+(BPré.I0.F[i])^2+(CPré.I0.F[i])^2
                                                      +(DPré.I0.F[i])^2+(EPré.I0.F[i])^2-CorrectPré.I0.F[i]^2)/(N-CorrectPré.I0.F[i])-1/sqrt(m-1))
  Dev.factor.post.I0.F[i]<-sqrt(m-1)/(sqrt(m-1)-1)*(sqrt((APost.I0.F[i])^2+(BPost.I0.F[i])^2+(CPost.I0.F[i])^2+
                                                         (DPost.I0.F[i])^2+(EPost.I0.F[i])^2-CorrectPost.I0.F[i]^2)/(N-CorrectPost.I0.F[i])-1/sqrt(m-1))
}
#Facteur déviation pour les questions ayant le niveau "L" pour le score
Dev.factor.pré.I0.F.L<-Dev.factor.pré.I0.F[is.na(Num.quest.L)==FALSE]
Dev.factor.post.I0.F.L<-Dev.factor.post.I0.F[is.na(Num.quest.L)==FALSE]

Présence.Pré.I0.F<- nrow(data_I0_F)-AbsPré.I0.F 
Présence.Post.I0.F<- nrow(data_I0_F)-AbsPost.I0.F

CorrectPré.I0.F<-CorrectPré.I0.F/Présence.Pré.I0.F
CorrectPost.I0.F<-CorrectPost.I0.F/Présence.Post.I0.F

S.pré.I0.F<-CorrectPré.I0.F
S.post.I0.F<-CorrectPost.I0.F

#Score pour les questions ayant le niveau "L" pour le score

S.pré.I0.F.L<-S.pré.I0.F[is.na(Num.quest.L)==FALSE]
S.post.I0.F.L<-S.post.I0.F[is.na(Num.quest.L)==FALSE]

##Plot S-C comparaison WCST/non WCST pour les filles

par(mfrow = c(1,2))
plot(C.factor.pré.I1.F~ S.pré.I1.F,xlim=c(0,1),ylim=c(0,1),main="Graphe S-C filles WCST (N=26)",col="gray1",pch=2,ylab="Facteur de concentration C",
     xlab="Taux de réponses correctes par question",lwd=1.8)
#text(x=S.pré.I1.F+0.02,y=C.factor.pré.I1.F,labels=c(1:30))
points(x=S.post.I1.F,y=C.factor.post.I1.F,pch=1,col="gray1",lwd=1.8)
#text(x=S.post.I1.F+0.02,y=C.factor.post.I1.F,labels=c(1:30))
abline(h=c(0.2,0.5),v=c(0.4,0.7), col="gray45")
lines(seq(from=0,to=1,by=0.001),Cmax(seq(from=0,to=1,by=0.001)),
      type = "l")
lines(seq(from=0,to=1,by=0.001),Cmin(seq(from=0,to=1,by=0.001)),
      type = "l")
points(x=c(mean(S.pré.I1.F),mean(S.post.I1.F)),y=c(mean(C.factor.pré.I1.F),mean(C.factor.post.I1.F))
       ,pch=c(17,19),col="gray1",cex=1.5)
arrows(x0=mean(S.pré.I1.F),x1=mean(S.post.I1.F),y0=mean(C.factor.pré.I1.F),y1=mean(C.factor.post.I1.F),
       code=2,lwd=1.5,length=0.15)
legend("topleft",c("pré-test","post-test","Moyenne pré-test","Moyenne post-test"),
       cex = 0.9, pch = c(2,1,17,19),col="gray1")

## filles non WCST
plot(C.factor.pré.I0.F~ S.pré.I0.F,xlim=c(0,1),ylim=c(0,1),main="Graphe S-C filles non WCST (N=22)",col="gray1",pch=2,ylab="Facteur de concentration C",
     xlab="Taux de réponses correctes par question",lwd=1.8)
#text(x=S.pré.I0.F+0.02,y=C.factor.pré.I0.F,labels=c(1:30))
points(x=S.post.I0.F,y=C.factor.post.I0.F,pch=1,col="gray1",lwd=1.8)
#text(x=S.post.I0.F+0.02,y=C.factor.post.I0.F,labels=c(1:30))
abline(h=c(0.2,0.5),v=c(0.4,0.7), col="gray45")
lines(seq(from=0,to=1,by=0.001),Cmax(seq(from=0,to=1,by=0.001)),
      type = "l")
lines(seq(from=0,to=1,by=0.001),Cmin(seq(from=0,to=1,by=0.001)),
      type = "l")
points(x=c(mean(S.pré.I0.F),mean(S.post.I0.F)),y=c(mean(C.factor.pré.I0.F),mean(C.factor.post.I0.F))
       ,pch=c(17,19),col="gray1",cex=1.5)
arrows(x0=mean(S.pré.I0.F),x1=mean(S.post.I0.F),y0=mean(C.factor.pré.I0.F),y1=mean(C.factor.post.I0.F),
       code=2,lwd=1.5,length=0.15)
legend("topleft",c("pré-test","post-test","Moyenne pré-test","Moyenne post-test"),
       cex = 0.9, pch = c(2,1,17,19),col="gray1")

##Plot Dev-C comparaison filles WCST /non WCST
par(mfrow = c(1,2))
plot(Dev.factor.pré.I1.F~ S.pré.I1.F,xlim=c(0,1),ylim=c(0,1),main="Graphe S-Gamma filles WCST (N=26)",col="gray1",pch=2,ylab="Facteur de déviation",
     xlab="Taux de réponses correctes par question",lwd=1.8)
#text(x=S.pré.I1.F+0.02,y=Dev.factor.pré.I1.F,labels=c(1:30),cex=0.6)
points(x=S.post.I1.F,y=Dev.factor.post.I1.F,pch=1,col="gray1",lwd=1.8)
#text(x=S.post.I1.F+0.02,y=Dev.factor.post.I1.F,labels=c(1:30),cex=0.6)

points(x=c(mean(S.pré.I1.F),mean(S.post.I1.F)),y=c(mean(Dev.factor.pré.I1.F),mean(Dev.factor.post.I1.F))
       ,pch=c(17,19),col="gray1",cex=1.5)
arrows(x0=mean(S.pré.I1.F),x1=mean(S.post.I1.F),y0=mean(Dev.factor.pré.I1.F),y1=mean(Dev.factor.post.I1.F),
       code=2,lwd=1.5,length=0.15)
grid(nx=NULL, ny=NULL, col="gray19")
legend("topleft",c("pré-test","post-test","Moyenne pré-test","Moyenne post-test"),
       cex = 0.9, pch = c(2,1,17,19),col="gray1")

## fille non WCST
plot(Dev.factor.pré.I0.F~ S.pré.I0.F,xlim=c(0,1),ylim=c(0,1),main="Graphe S-Gamma filles non WCST (N=22)",col="gray1",pch=2,ylab="Facteur de déviation",
     xlab="Taux de réponses correctes par question",lwd=1.8)
#text(x=S.pré.I0.F+0.02,y=Dev.factor.pré.I0.F,labels=c(1:30),cex=0.6)
points(x=S.post.I0.F,y=Dev.factor.post.I0.F,pch=1,col="gray1",lwd=1.8)
#text(x=S.post.I0.F+0.02,y=Dev.factor.post.I0.F,labels=c(1:30),cex=0.6)

points(x=c(mean(S.pré.I0.F),mean(S.post.I0.F)),y=c(mean(Dev.factor.pré.I0.F),mean(Dev.factor.post.I0.F))
       ,pch=c(17,19),col="gray1",cex=1.5)
arrows(x0=mean(S.pré.I0.F),x1=mean(S.post.I0.F),y0=mean(Dev.factor.pré.I0.F),y1=mean(Dev.factor.post.I0.F),
       code=2,lwd=1.5,length=0.15)
grid(nx=NULL, ny=NULL, col="gray19")
legend("topleft",c("pré-test","post-test","Moyenne pré-test","Moyenne post-test"),
       cex = 0.9, pch = c(2,1,17,19),col="gray1")


##Plot Dev-C comparaison groupe test/contrôle pour les questions de type L
par(mfrow = c(1,2))
plot(Dev.factor.pré.I1.F.L~ S.pré.I1.F.L,xlim=c(0,1),ylim=c(0,1),main="Graphe S-Gamma filles WCST (N=26) 
    questions de type LH",col="gray1",pch=2,ylab="Facteur de déviation",
     xlab="Taux de réponses correctes par question",lwd=1.8)
text(x=S.pré.I1.F.L+0.02,y=Dev.factor.pré.I1.F.L,labels=Num.quest.L[is.na(Num.quest.L)==FALSE],cex=0.8)
points(x=S.post.I1.F.L,y=Dev.factor.post.I1.F.L,pch=1,col="gray1",lwd=1.8)
text(x=S.post.I1.F.L+0.02,y=Dev.factor.post.I1.F.L,labels=Num.quest.L[is.na(Num.quest.L)==FALSE],cex=0.8)

points(x=c(mean(S.pré.I1.F.L),mean(S.post.I1.F.L)),y=c(mean(Dev.factor.pré.I1.F.L),mean(Dev.factor.post.I1.F.L))
       ,pch=c(17,19),col="gray1",cex=1.5)
arrows(x0=mean(S.pré.I1.F.L),x1=mean(S.post.I1.F.L),y0=mean(Dev.factor.pré.I1.F.L),y1=mean(Dev.factor.post.I1.F.L),
       code=2,lwd=1.5,length=0.15)
for (i in 1:length(Num.quest.L[is.na(Num.quest.L)==FALSE])) {
  
  arrows(x0=S.pré.I1.F.L[i],x1=S.post.I1.F.L[i],y0=Dev.factor.pré.I1.F.L[i],y1=Dev.factor.post.I1.F.L[i],
         code=2,lwd=0.5,length=0.15,lty=3,col="gray35")
}
grid(nx=NULL, ny=NULL, col="gray19")
legend("bottomright",c("pré-test","post-test","Moyenne pré-test","Moyenne post-test"),
       cex = 0.9, pch = c(2,1,17,19),col="gray1")

## groupe contrôle
plot(Dev.factor.pré.I0.F.L~ S.pré.I0.F.L,xlim=c(0,1),ylim=c(0,1),main="Graphe S-Gamma filles non WCST (N=22) 
    questions de type LH ",col="gray1",pch=2,ylab="Facteur de déviation",
     xlab="Taux de réponses correctes par question",lwd=1.8)
text(x=S.pré.I0.F.L+0.02,y=Dev.factor.pré.I0.F.L,labels=Num.quest.L[is.na(Num.quest.L)==FALSE],cex=0.8)
points(x=S.post.I0.F.L,y=Dev.factor.post.I0.F.L,pch=1,col="gray1",lwd=1.8)
text(x=S.post.I0.F.L+0.02,y=Dev.factor.post.I0.F.L,labels=Num.quest.L[is.na(Num.quest.L)==FALSE],cex=0.8)

points(x=c(mean(S.pré.I0.F.L),mean(S.post.I0.F.L)),y=c(mean(Dev.factor.pré.I0.F.L),mean(Dev.factor.post.I0.F.L))
       ,pch=c(17,19),col="gray1",cex=1.5)
arrows(x0=mean(S.pré.I0.F.L),x1=mean(S.post.I0.F.L),y0=mean(Dev.factor.pré.I0.F.L),y1=mean(Dev.factor.post.I0.F.L),
       code=2,lwd=1.5,length=0.15)
for (i in 1:length(Num.quest.L[is.na(Num.quest.L)==FALSE])) {
  
  arrows(x0=S.pré.I0.F.L[i],x1=S.post.I0.F.L[i],y0=Dev.factor.pré.I0.F.L[i],y1=Dev.factor.post.I0.F.L[i],
         code=2,lwd=0.5,length=0.15,lty=3,col="gray35")
}
grid(nx=NULL, ny=NULL, col="gray19")
legend("bottomright",c("pré-test","post-test","Moyenne pré-test","Moyenne post-test"),
       cex = 0.9, pch = c(2,1,17,19),col="gray1")

################################Analyse concentration WCST H/ pas WCST H#################

###garçons avec WCST
AbsPré.I1.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CorrectPré.I1.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WrongPré.I1.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 9:38) {
  for (i in 1:nrow(data_I1_H)) {
    if (is.na(data_I1_H[i,j])==TRUE){
      AbsPré.I1.H[j-8]=AbsPré.I1.H[j-8]+1
    }
    else if (is.na(data_I1_H[i,j])==FALSE & data_I1_H[i,j]!=Items[j-8])
    {WrongPré.I1.H[j-8]<-WrongPré.I1.H[j-8]+1}
    else if (is.na(data_I1_H[i,j])==FALSE & data_I1_H[i,j]==Items[j-8]) 
    {CorrectPré.I1.H[j-8]<-CorrectPré.I1.H[j-8]+1}
  }
}

print(CorrectPré.I1.H+WrongPré.I1.H+AbsPré.I1.H)

AbsPost.I1.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CorrectPost.I1.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WrongPost.I1.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 40:69) {
  for (i in 1:nrow(data_I1_H)) {
    if (is.na(data_I1_H[i,j])==TRUE){
      AbsPost.I1.H[j-39]=AbsPost.I1.H[j-39]+1
    }
    else if (is.na(data_I1_H[i,j])==FALSE & data_I1_H[i,j]!=Items[j-39])
    {WrongPost.I1.H[j-39]<-WrongPost.I1.H[j-39]+1}
    else if (is.na(data_I1_H[i,j])==FALSE & data_I1_H[i,j]==Items[j-39]) 
    {CorrectPost.I1.H[j-39]<-CorrectPost.I1.H[j-39]+1}
  }
}

print(CorrectPost.I1.H+WrongPost.I1.H+AbsPost.I1.H)

#comparaison item par item 
AbsPré.I1.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
APré.I1.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
BPré.I1.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CPré.I1.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
DPré.I1.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
EPré.I1.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 9:38) {
  for (i in 1:nrow(data_I1_H)) {
    if (is.na(data_I1_H[i,j])==TRUE){
      AbsPré.I1.H[j-8]=AbsPré.I1.H[j-8]+1
    }
    else if (is.na(data_I1_H[i,j])==FALSE & data_I1_H[i,j]=="A")
    {APré.I1.H[j-8]<-APré.I1.H[j-8]+1}
    else if (is.na(data_I1_H[i,j])==FALSE  & data_I1_H[i,j]=="B")
    {BPré.I1.H[j-8]<-BPré.I1.H[j-8]+1}
    else if (is.na(data_I1_H[i,j])==FALSE  & data_I1_H[i,j]=="C")
    {CPré.I1.H[j-8]<-CPré.I1.H[j-8]+1}
    else if (is.na(data_I1_H[i,j])==FALSE & data_I1_H[i,j]=="D")
    {DPré.I1.H[j-8]<-DPré.I1.H[j-8]+1}
    else if (is.na(data_I1_H[i,j])==FALSE & data_I1_H[i,j]=="E")
    {EPré.I1.H[j-8]<-EPré.I1.H[j-8]+1}
  }
}
print(APré.I1.H+BPré.I1.H+CPré.I1.H+DPré.I1.H+EPré.I1.H+AbsPré.I1.H)


AbsPost.I1.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
APost.I1.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
BPost.I1.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CPost.I1.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
DPost.I1.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
EPost.I1.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 40:69) {
  for (i in 1:nrow(data_I1_H)) {
    if (is.na(data_I1_H[i,j])==TRUE){
      AbsPost.I1.H[j-39]=AbsPost.I1.H[j-39]+1
    }
    else if (is.na(data_I1_H[i,j])==FALSE & data_I1_H[i,j]=="A")
    {APost.I1.H[j-39]<-APost.I1.H[j-39]+1}
    else if (is.na(data_I1_H[i,j])==FALSE & data_I1_H[i,j]=="B")
    {BPost.I1.H[j-39]<-BPost.I1.H[j-39]+1}
    else if (is.na(data_I1_H[i,j])==FALSE & data_I1_H[i,j]=="C")
    {CPost.I1.H[j-39]<-CPost.I1.H[j-39]+1}
    else if (is.na(data_I1_H[i,j])==FALSE & data_I1_H[i,j]=="D")
    {DPost.I1.H[j-39]<-DPost.I1.H[j-39]+1}
    else if (is.na(data_I1_H[i,j])==FALSE & data_I1_H[i,j]=="E")
    {EPost.I1.H[j-39]<-EPost.I1.H[j-39]+1}
  }
}
print(APost.I1.H+BPost.I1.H+CPost.I1.H+DPost.I1.H+EPost.I1.H+AbsPost.I1.H)

N<-nrow(data_I1_H)
m<-5

C.factor.pré.I1.H<-c()
C.factor.post.I1.H<-c()
for (i in 1:30){
  C.factor.pré.I1.H[i]<-sqrt(m)/(sqrt(m)-1)*(sqrt((APré.I1.H[i])^2+(BPré.I1.H[i])^2+
                                                    (CPré.I1.H[i])^2+(DPré.I1.H[i])^2+(EPré.I1.H[i])^2)/N-1/sqrt(m))
  C.factor.post.I1.H[i]<-sqrt(m)/(sqrt(m)-1)*(sqrt((APost.I1.H[i])^2+(BPost.I1.H[i])^2+
                                                     (CPost.I1.H[i])^2+(DPost.I1.H[i])^2+(EPost.I1.H[i])^2)/N-1/sqrt(m))
}
print(C.factor.post.I1.H)

Dev.factor.pré.I1.H<-c()
Dev.factor.post.I1.H<-c()
for (i in 1:30){
  Dev.factor.pré.I1.H[i]<-sqrt(m-1)/(sqrt(m-1)-1)*(sqrt((APré.I1.H[i])^2+(BPré.I1.H[i])^2+(CPré.I1.H[i])^2
                                                        +(DPré.I1.H[i])^2+(EPré.I1.H[i])^2-CorrectPré.I1.H[i]^2)/(N-CorrectPré.I1.H[i])-1/sqrt(m-1))
  Dev.factor.post.I1.H[i]<-sqrt(m-1)/(sqrt(m-1)-1)*(sqrt((APost.I1.H[i])^2+(BPost.I1.H[i])^2+(CPost.I1.H[i])^2+
                                                           (DPost.I1.H[i])^2+(EPost.I1.H[i])^2-CorrectPost.I1.H[i]^2)/(N-CorrectPost.I1.H[i])-1/sqrt(m-1))
}
#Facteur déviation pour les questions ayant le niveau "L" pour le score
Dev.factor.pré.I1.H.L<-Dev.factor.pré.I1.H[is.na(Num.quest.L)==FALSE]
Dev.factor.post.I1.H.L<-Dev.factor.post.I1.H[is.na(Num.quest.L)==FALSE]

Présence.Pré.I1.H<- nrow(data_I1_H)-AbsPré.I1.H 
Présence.Post.I1.H<- nrow(data_I1_H)-AbsPost.I1.H

CorrectPré.I1.H<-CorrectPré.I1.H/Présence.Pré.I1.H
CorrectPost.I1.H<-CorrectPost.I1.H/Présence.Post.I1.H

S.pré.I1.H<-CorrectPré.I1.H
S.post.I1.H<-CorrectPost.I1.H

#Score pour les questions ayant le niveau "L" pour le score

S.pré.I1.H.L<-S.pré.I1.H[is.na(Num.quest.L)==FALSE]
S.post.I1.H.L<-S.post.I1.H[is.na(Num.quest.L)==FALSE]

###Fille pas wcst
AbsPré.I0.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CorrectPré.I0.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WrongPré.I0.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 9:38) {
  for (i in 1:nrow(data_I0_H)) {
    if (is.na(data_I0_H[i,j])==TRUE){
      AbsPré.I0.H[j-8]=AbsPré.I0.H[j-8]+1
    }
    else if (is.na(data_I0_H[i,j])==FALSE & data_I0_H[i,j]!=Items[j-8])
    {WrongPré.I0.H[j-8]<-WrongPré.I0.H[j-8]+1}
    else if (is.na(data_I0_H[i,j])==FALSE & data_I0_H[i,j]==Items[j-8]) 
    {CorrectPré.I0.H[j-8]<-CorrectPré.I0.H[j-8]+1}
  }
}

print(CorrectPré.I0.H+WrongPré.I0.H+AbsPré.I0.H)

AbsPost.I0.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CorrectPost.I0.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
WrongPost.I0.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 40:69) {
  for (i in 1:nrow(data_I0_H)) {
    if (is.na(data_I0_H[i,j])==TRUE){
      AbsPost.I0.H[j-39]=AbsPost.I0.H[j-39]+1
    }
    else if (is.na(data_I0_H[i,j])==FALSE & data_I0_H[i,j]!=Items[j-39])
    {WrongPost.I0.H[j-39]<-WrongPost.I0.H[j-39]+1}
    else if (is.na(data_I0_H[i,j])==FALSE & data_I0_H[i,j]==Items[j-39]) 
    {CorrectPost.I0.H[j-39]<-CorrectPost.I0.H[j-39]+1}
  }
}

print(CorrectPost.I0.H+WrongPost.I0.H+AbsPost.I0.H)

#comparaison item par item 
AbsPré.I0.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
APré.I0.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
BPré.I0.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CPré.I0.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
DPré.I0.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
EPré.I0.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 9:38) {
  for (i in 1:nrow(data_I0_H)) {
    if (is.na(data_I0_H[i,j])==TRUE){
      AbsPré.I0.H[j-8]=AbsPré.I0.H[j-8]+1
    }
    else if (is.na(data_I0_H[i,j])==FALSE & data_I0_H[i,j]=="A")
    {APré.I0.H[j-8]<-APré.I0.H[j-8]+1}
    else if (is.na(data_I0_H[i,j])==FALSE  & data_I0_H[i,j]=="B")
    {BPré.I0.H[j-8]<-BPré.I0.H[j-8]+1}
    else if (is.na(data_I0_H[i,j])==FALSE  & data_I0_H[i,j]=="C")
    {CPré.I0.H[j-8]<-CPré.I0.H[j-8]+1}
    else if (is.na(data_I0_H[i,j])==FALSE & data_I0_H[i,j]=="D")
    {DPré.I0.H[j-8]<-DPré.I0.H[j-8]+1}
    else if (is.na(data_I0_H[i,j])==FALSE & data_I0_H[i,j]=="E")
    {EPré.I0.H[j-8]<-EPré.I0.H[j-8]+1}
  }
}
print(APré.I0.H+BPré.I0.H+CPré.I0.H+DPré.I0.H+EPré.I0.H+AbsPré.I0.H)


AbsPost.I0.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
APost.I0.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
BPost.I0.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
CPost.I0.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
DPost.I0.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
EPost.I0.H<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for(j in 40:69) {
  for (i in 1:nrow(data_I0_H)) {
    if (is.na(data_I0_H[i,j])==TRUE){
      AbsPost.I0.H[j-39]=AbsPost.I0.H[j-39]+1
    }
    else if (is.na(data_I0_H[i,j])==FALSE & data_I0_H[i,j]=="A")
    {APost.I0.H[j-39]<-APost.I0.H[j-39]+1}
    else if (is.na(data_I0_H[i,j])==FALSE & data_I0_H[i,j]=="B")
    {BPost.I0.H[j-39]<-BPost.I0.H[j-39]+1}
    else if (is.na(data_I0_H[i,j])==FALSE & data_I0_H[i,j]=="C")
    {CPost.I0.H[j-39]<-CPost.I0.H[j-39]+1}
    else if (is.na(data_I0_H[i,j])==FALSE & data_I0_H[i,j]=="D")
    {DPost.I0.H[j-39]<-DPost.I0.H[j-39]+1}
    else if (is.na(data_I0_H[i,j])==FALSE & data_I0_H[i,j]=="E")
    {EPost.I0.H[j-39]<-EPost.I0.H[j-39]+1}
  }
}

print(APost.I0.H+BPost.I0.H+CPost.I0.H+DPost.I0.H+EPost.I0.H+AbsPost.I0.H)

N<-nrow(data_I0_H)
m<-5
C.factor.pré.I0.H<-c()
C.factor.post.I0.H<-c()
for (i in 1:30){
  C.factor.pré.I0.H[i]<-sqrt(m)/(sqrt(m)-1)*(sqrt((APré.I0.H[i])^2+(BPré.I0.H[i])^2+
                                                    (CPré.I0.H[i])^2+(DPré.I0.H[i])^2+(EPré.I0.H[i])^2)/N-1/sqrt(m))
  C.factor.post.I0.H[i]<-sqrt(m)/(sqrt(m)-1)*(sqrt((APost.I0.H[i])^2+(BPost.I0.H[i])^2+
                                                     (CPost.I0.H[i])^2+(DPost.I0.H[i])^2+(EPost.I0.H[i])^2)/N-1/sqrt(m))
}

Dev.factor.pré.I0.H<-c()
Dev.factor.post.I0.H<-c()
for (i in 1:30){
  Dev.factor.pré.I0.H[i]<-sqrt(m-1)/(sqrt(m-1)-1)*(sqrt((APré.I0.H[i])^2+(BPré.I0.H[i])^2+(CPré.I0.H[i])^2
                                                        +(DPré.I0.H[i])^2+(EPré.I0.H[i])^2-CorrectPré.I0.H[i]^2)/(N-CorrectPré.I0.H[i])-1/sqrt(m-1))
  Dev.factor.post.I0.H[i]<-sqrt(m-1)/(sqrt(m-1)-1)*(sqrt((APost.I0.H[i])^2+(BPost.I0.H[i])^2+(CPost.I0.H[i])^2+
                                                           (DPost.I0.H[i])^2+(EPost.I0.H[i])^2-CorrectPost.I0.H[i]^2)/(N-CorrectPost.I0.H[i])-1/sqrt(m-1))
}
#Facteur déviation pour les questions ayant le niveau "L" pour le score
Dev.factor.pré.I0.H.L<-Dev.factor.pré.I0.H[is.na(Num.quest.L)==FALSE]
Dev.factor.post.I0.H.L<-Dev.factor.post.I0.H[is.na(Num.quest.L)==FALSE]

Présence.Pré.I0.H<- nrow(data_I0_H)-AbsPré.I0.H 
Présence.Post.I0.H<- nrow(data_I0_H)-AbsPost.I0.H

CorrectPré.I0.H<-CorrectPré.I0.H/Présence.Pré.I0.H
CorrectPost.I0.H<-CorrectPost.I0.H/Présence.Post.I0.H

S.pré.I0.H<-CorrectPré.I0.H
S.post.I0.H<-CorrectPost.I0.H

#Score pour les questions ayant le niveau "L" pour le score

S.pré.I0.H.L<-S.pré.I0.H[is.na(Num.quest.L)==FALSE]
S.post.I0.H.L<-S.post.I0.H[is.na(Num.quest.L)==FALSE]

##Plot S-C comparaison WCST/non WCST pour les garçons

par(mfrow = c(1,2))
plot(C.factor.pré.I1.H~ S.pré.I1.H,xlim=c(0,1),ylim=c(0,1),main="Graphe S-C garçons WCST (N=24)",col="gray1",pch=2,ylab="Facteur de concentration C",
     xlab="Taux de réponses correctes par question",lwd=1.8)
#text(x=S.pré.I1.H+0.02,y=C.factor.pré.I1.H,labels=c(1:30))
points(x=S.post.I1.H,y=C.factor.post.I1.H,pch=1,col="gray1",lwd=1.8)
#text(x=S.post.I1.H+0.02,y=C.factor.post.I1.H,labels=c(1:30))
abline(h=c(0.2,0.5),v=c(0.4,0.7), col="gray45")
lines(seq(from=0,to=1,by=0.001),Cmax(seq(from=0,to=1,by=0.001)),
      type = "l")
lines(seq(from=0,to=1,by=0.001),Cmin(seq(from=0,to=1,by=0.001)),
      type = "l")
points(x=c(mean(S.pré.I1.H),mean(S.post.I1.H)),y=c(mean(C.factor.pré.I1.H),mean(C.factor.post.I1.H))
       ,pch=c(17,19),col="gray1",cex=1.5)
arrows(x0=mean(S.pré.I1.H),x1=mean(S.post.I1.H),y0=mean(C.factor.pré.I1.H),y1=mean(C.factor.post.I1.H),
       code=2,lwd=1.5,length=0.15)
legend("topleft",c("pré-test","post-test","Moyenne pré-test","Moyenne post-test"),
       cex = 0.9, pch = c(2,1,17,19),col="gray1")

## garçons non WCST
plot(C.factor.pré.I0.H~ S.pré.I0.H,xlim=c(0,1),ylim=c(0,1),main="Graphe S-C garçonss non WCST (N=13)",col="gray1",pch=2,ylab="Facteur de concentration C",
     xlab="Taux de réponses correctes par question",lwd=1.8)
#text(x=S.pré.I0.H+0.02,y=C.factor.pré.I0.H,labels=c(1:30))
points(x=S.post.I0.H,y=C.factor.post.I0.H,pch=1,col="gray1",lwd=1.8)
#text(x=S.post.I0.H+0.02,y=C.factor.post.I0.H,labels=c(1:30))
abline(h=c(0.2,0.5),v=c(0.4,0.7), col="gray45")
lines(seq(from=0,to=1,by=0.001),Cmax(seq(from=0,to=1,by=0.001)),
      type = "l")
lines(seq(from=0,to=1,by=0.001),Cmin(seq(from=0,to=1,by=0.001)),
      type = "l")
points(x=c(mean(S.pré.I0.H),mean(S.post.I0.H)),y=c(mean(C.factor.pré.I0.H),mean(C.factor.post.I0.H))
       ,pch=c(17,19),col="gray1",cex=1.5)
arrows(x0=mean(S.pré.I0.H),x1=mean(S.post.I0.H),y0=mean(C.factor.pré.I0.H),y1=mean(C.factor.post.I0.H),
       code=2,lwd=1.5,length=0.15)
legend("topleft",c("pré-test","post-test","Moyenne pré-test","Moyenne post-test"),
       cex = 0.9, pch = c(2,1,17,19),col="gray1")

##Plot Dev-C comparaison garçons WCST /non WCST
par(mfrow = c(1,2))
plot(Dev.factor.pré.I1.H~ S.pré.I1.H,xlim=c(0,1),ylim=c(0,1),main="Graphe S-Gamma garçons WCST (N=24)",col="gray1",pch=2,ylab="Facteur de déviation",
     xlab="Taux de réponses correctes par question",lwd=1.8)
#text(x=S.pré.I1.H+0.02,y=Dev.factor.pré.I1.H,labels=c(1:30),cex=0.6)
points(x=S.post.I1.H,y=Dev.factor.post.I1.H,pch=1,col="gray1",lwd=1.8)
#text(x=S.post.I1.H+0.02,y=Dev.factor.post.I1.H,labels=c(1:30),cex=0.6)

points(x=c(mean(S.pré.I1.H),mean(S.post.I1.H)),y=c(mean(Dev.factor.pré.I1.H),mean(Dev.factor.post.I1.H))
       ,pch=c(17,19),col="gray1",cex=1.5)
arrows(x0=mean(S.pré.I1.H),x1=mean(S.post.I1.H),y0=mean(Dev.factor.pré.I1.H),y1=mean(Dev.factor.post.I1.H),
       code=2,lwd=1.5,length=0.15)
grid(nx=NULL, ny=NULL, col="gray19")
legend("topleft",c("pré-test","post-test","Moyenne pré-test","Moyenne post-test"),
       cex = 0.9, pch = c(2,1,17,19),col="gray1")

## garçons non WCST
plot(Dev.factor.pré.I0.H~ S.pré.I0.H,xlim=c(0,1),ylim=c(0,1),main="Graphe S-Gamma garçons non WCST (N=13)",col="gray1",pch=2,ylab="Facteur de déviation",
     xlab="Taux de réponses correctes par question",lwd=1.8)
#text(x=S.pré.I0.H+0.02,y=Dev.factor.pré.I0.H,labels=c(1:30),cex=0.6)
points(x=S.post.I0.H,y=Dev.factor.post.I0.H,pch=1,col="gray1",lwd=1.8)
#text(x=S.post.I0.H+0.02,y=Dev.factor.post.I0.H,labels=c(1:30),cex=0.6)

points(x=c(mean(S.pré.I0.H),mean(S.post.I0.H)),y=c(mean(Dev.factor.pré.I0.H),mean(Dev.factor.post.I0.H))
       ,pch=c(17,19),col="gray1",cex=1.5)
arrows(x0=mean(S.pré.I0.H),x1=mean(S.post.I0.H),y0=mean(Dev.factor.pré.I0.H),y1=mean(Dev.factor.post.I0.H),
       code=2,lwd=1.5,length=0.15)
grid(nx=NULL, ny=NULL, col="gray19")
legend("topleft",c("pré-test","post-test","Moyenne pré-test","Moyenne post-test"),
     cex = 0.9, pch = c(2,1,17,19),col="gray1")


##Plot Dev-C comparaison groupe test/contrôle pour les questions de type L
par(mfrow = c(1,2))
plot(Dev.factor.pré.I1.H.L~ S.pré.I1.H.L,xlim=c(0,1),ylim=c(0,1),main="Graphe S-Gamma filles WCST (N=26) 
     questions de type LH",col="gray1",pch=2,ylab="Facteur de déviation",
     xlab="Taux de réponses correctes par question",lwd=1.8)
text(x=S.pré.I1.H.L+0.02,y=Dev.factor.pré.I1.H.L,labels=Num.quest.L[is.na(Num.quest.L)==FALSE],cex=0.8)
points(x=S.post.I1.H.L,y=Dev.factor.post.I1.H.L,pch=1,col="gray1",lwd=1.8)
text(x=S.post.I1.H.L+0.02,y=Dev.factor.post.I1.H.L,labels=Num.quest.L[is.na(Num.quest.L)==FALSE],cex=0.8)

points(x=c(mean(S.pré.I1.H.L),mean(S.post.I1.H.L)),y=c(mean(Dev.factor.pré.I1.H.L),mean(Dev.factor.post.I1.H.L))
       ,pch=c(17,19),col="gray1",cex=1.5)
arrows(x0=mean(S.pré.I1.H.L),x1=mean(S.post.I1.H.L),y0=mean(Dev.factor.pré.I1.H.L),y1=mean(Dev.factor.post.I1.H.L),
       code=2,lwd=1.5,length=0.15)
for (i in 1:length(Num.quest.L[is.na(Num.quest.L)==FALSE])) {
  
  arrows(x0=S.pré.I1.H.L[i],x1=S.post.I1.H.L[i],y0=Dev.factor.pré.I1.H.L[i],y1=Dev.factor.post.I1.H.L[i],
         code=2,lwd=0.5,length=0.15,lty=3,col="gray35")
}
grid(nx=NULL, ny=NULL, col="gray19")
legend("bottomright",c("pré-test","post-test","Moyenne pré-test","Moyenne post-test"),
       cex = 0.9, pch = c(2,1,17,19),col="gray1")

## groupe contrôle
plot(Dev.factor.pré.I0.H.L~ S.pré.I0.H.L,xlim=c(0,1),ylim=c(0,1),main="Graphe S-Gamma filles non WCST (N=22) 
     questions de type LH ",col="gray1",pch=2,ylab="Facteur de déviation",
     xlab="Taux de réponses correctes par question",lwd=1.8)
text(x=S.pré.I0.H.L+0.02,y=Dev.factor.pré.I0.H.L,labels=Num.quest.L[is.na(Num.quest.L)==FALSE],cex=0.8)
points(x=S.post.I0.H.L,y=Dev.factor.post.I0.H.L,pch=1,col="gray1",lwd=1.8)
text(x=S.post.I0.H.L+0.02,y=Dev.factor.post.I0.H.L,labels=Num.quest.L[is.na(Num.quest.L)==FALSE],cex=0.8)

points(x=c(mean(S.pré.I0.H.L),mean(S.post.I0.H.L)),y=c(mean(Dev.factor.pré.I0.H.L),mean(Dev.factor.post.I0.H.L))
       ,pch=c(17,19),col="gray1",cex=1.5)
arrows(x0=mean(S.pré.I0.H.L),x1=mean(S.post.I0.H.L),y0=mean(Dev.factor.pré.I0.H.L),y1=mean(Dev.factor.post.I0.H.L),
       code=2,lwd=1.5,length=0.15)
for (i in 1:length(Num.quest.L[is.na(Num.quest.L)==FALSE])) {
  
  arrows(x0=S.pré.I0.H.L[i],x1=S.post.I0.H.L[i],y0=Dev.factor.pré.I0.H.L[i],y1=Dev.factor.post.I0.H.L[i],
         code=2,lwd=0.5,length=0.15,lty=3,col="gray35")
}
grid(nx=NULL, ny=NULL, col="gray19")
legend("bottomright",c("pré-test","post-test","Moyenne pré-test","Moyenne post-test"),
       cex = 0.9, pch = c(2,1,17,19),col="gray1")
#############################Régressions multivariées################################

library(MASS)
library(yhat)

data_lm<-data[is.na(data$Diff.Score)==FALSE,]
data_lm<-data_lm[,-c(6,8,9:38,40:69,73,74,75,76)]
data_lm<-na.omit(data_lm)

data_lm_I1<-data_I1[,-c(8:38,40:69)]
data_lm_I1<-na.omit(data_lm_I1)

#modèle pour l'ensemble des élèves

y<-100*data_lm$Score.post.test/30
x<-100*data_lm$Score.pré.test/30
g<-(y-x)/(100-x)

full.model2 <- lm( g~Score.post.test+Sexe*Inhibition,data = data_lm)
step.model2 <- stepAIC(full.model2, direction = "both", 
                       trace = FALSE)
summary(step.model2)

##modèles pour les élèves ayant participé au WCST

full.model <- glm(Score.pré.test ~Sexe+Age+Etudes+MeanTime+ChangeRule+PersError+Age*ChangeRule
                  +Age*MeanTime+Age*PersError, 
                 Gamma(link = "inverse"),data = data_lm_I1)
step.model <- stepAIC(full.model, direction = "both", 
                       trace = FALSE)
summary(step.model)
test <- gamma.shape(full.model, verbose = TRUE)

y<-100*data_lm_I1$Score.post.test/30
x<-100*data_lm_I1$Score.pré.test/30
g<-(y-x)/(100-x)

full.model5 <- lm(g ~Score.post.test+Sexe+Etudes+MeanTime+ChangeRule
                  +Age*ChangeRule+Sexe*PersError, data = data_lm_I1)
step.model5 <- stepAIC(full.model5, direction = "both", 
                       trace = FALSE)
summary(step.model5)
#residuals.lm(full.model5)

##################################Régressions logistiques#######################
library(MASS)
library(yhat)
 #bases de données sans les données manquantes afin de faire tourner la fct stepAIC 
 #au moins il y a de NA et au moins ont perd d'obeservations)
data_glm.pré<-data[,-c(8,39:70,73:78)]
data_glm.post<-data[,-c(8:39,73:78)]
data_glm.pré<-data_glm.pré[is.na(data_glm.pré$Diff.Score)==FALSE,]
data_glm.post<-data_glm.post[is.na(data_glm.post$Diff.Score)==FALSE,]
data_glm2<-data_I1[,-c(8:39,77)]
data_glm1<-data_I1[,-c(8,39:70,77)]
data_glm2<-data_glm2[is.na(data_glm2$Diff.Score)==FALSE,]
data_glm1<-data_glm1[is.na(data_glm1$Diff.Score)==FALSE,]
data_glm2<-na.omit(data_glm2)
data_glm1<-na.omit(data_glm1)

#ensemble des élèves (N=85)
LogReg1 <- glm(Q21~Sexe+Age+Inhibition+Heures.phys+Sexe*Inhibition+Sexe*Age, 
               family=binomial(link="logit"),data=data_glm.pré) 
LogReg1_Step<-stepAIC(LogReg1, direction = "both", 
                      trace = FALSE)
summary(LogReg1_Step)
summary(LogReg1)

#Groupe test (N=36)
LogReg1 <- glm(Q17~PersError*Sexe+Sexe*Age+ChangeRule*Sexe+Heures.phys+Age+ChangeRule+PersError, 
               family=binomial(link="logit"),data=data_glm1) 
LogReg1_Step<-stepAIC(LogReg1, direction = "both", 
                      trace = FALSE)
summary(LogReg1_Step)
summary(LogReg1)

##########################modèle mixte##################
setwd("C:/Users/charl/OneDrive/Documents/Master 2/Mémoire didactique/code cédric")
dat <- read.csv2("DATABASE8.csv")
library(nlme)

Mixed.model<-nlme()

exp(coefficients(mod.log3))
##################################ggplot2 - boxplot#####################
library(ggplot2)

ggplot(data, aes(x=Sexe, y=Diff.Score)) + 
  geom_boxplot(fill="slateblue", alpha=0.4) + 
  ylab("Gain FCI")+title("df")
data_potvin<-data_I1[data_I1$PersError<27 & data_I1$PersError>0,]
data_potvin$PersError<--1*data_potvin$PersError
library("ggpubr")
#print(data_I1$NOMA[data_I1$PersError>30])
data_potvin$g.pot<-(data_potvin$Score.post.test/30-data_potvin$Score.pré.test/30)/(1-data_potvin$Score.pré.test/30)

ggscatter(data_potvin, x = "ChangeRule", y = "g.pot", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson") + 
  ggtitle("Gain FCI en fonction du nombre d'erreurs de persévérance au WCST") +
  xlab("Erreurs de persévérance WCST") + ylab("Gain FCI")+grids(linetype="solid")


cor.test(data_potvin$PersError, data_potvin$g.pot,method="pearson")
cor.test(data_potvin$PersError,data_potvin$ChangeRule,method="pearson")
data_potvin1<-data_I1[data_I1$ChangeRule>2,]
ggscatter(data_potvin1, x = "ChangeRule", y = "Diff.Score", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "ChangeRule", ylab = "Diff.Score")

## gain FCI en fonction de l'inhibition

x<-100*data$Score.pré.test/30
y<-100*data$Score.post.test/30
g<-(y-x)/(100-x)
par(mfrow = c(1,2))
boundaries4<-boxplot(g~Sexe,data=data,ylab="Gain normalisé",
                     main="Distribution du gain normalisé en fonction du genre", col="skyblue4",
                     ylim=c(-0.2,0.8),names=c("Filles","Garçons"))
nbGroup= nlevels(data$Sexe)
text( 
  x=c(1:nbGroup), 
  y=boundaries4$stats[nrow(boundaries4$stats),]+0.1, 
  paste("n = ",table(data$Sexe),sep="")  
)
text(x=2.5,y=-0.30,xpd=TRUE,labels="a)",cex=1.5)

boundaries4<-boxplot(Diff.Score~Sexe,data=data,ylab="Gain FCI",
                     main="Distribution du gain FCI en fonction du genre", col="skyblue4",
                     ylim=c(-3,12),names=c("Filles","Garçons"))
nbGroup= nlevels(data$Sexe)
text( 
  x=c(1:nbGroup), 
  y=boundaries4$stats[nrow(boundaries4$stats),]+1, 
  paste("n = ",table(data$Sexe),sep="")  
)
text(x=2.5,y=-4.5,xpd=TRUE,labels="b)",cex=1.5)

boundaries4<-boxplot(ChangeRule~Sexe,data=data_I1,ylab="Nombre de changement de règle",
                     main="Nombre de changement de règles en fonction du genre", col="skyblue4",
                     ylim=c(0,15),names=c("Filles","Garçons"))
nbGroup= nlevels(data_I1$Sexe)
text( 
  x=c(1:nbGroup), 
  y=boundaries4$stats[nrow(boundaries4$stats),]+1, 
  paste("n = ",table(data_I1$Sexe),sep="")  
)



g<-(mean(CorrectPost)-mean(CorrectPré))/(1-mean(CorrectPré))
g.ave<-(data$Score.post.test/30-data$Score.pré.test/30)/(1-data$Score.pré.test/30)