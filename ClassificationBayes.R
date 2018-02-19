Bayes <- function(t){
rm(list=ls())#clear
 Separateur <- readline("Separateur De fichier :? > ")#numero attr

Liste<- read.csv(file.choose(),TRUE,sep=Separateur)#import

library(e1071) #naive Bayes library
repeat {
 num <- readline("Num Attribut ? > ")#numero attr
n<-as.numeric(num)

if (n<ncol(Liste)){

break
}
}


nbb <- readline("Num Colonne Classe ? > ")
nb <-as.numeric(nbb)
while(nb > ncol(Liste)) {
nbb <- readline("Num Colonne Classe ? > ")
nb <-as.numeric(nbb)
}



f<-""
for (i in 1:(ncol(Liste)-1)){
f[i]<-readline(names(Liste[i]))
 }




print(Liste[,1:n])
print("------****---hetha 2 v------********---")
print(Liste[,nb])

classifier<-naiveBayes(Liste[,1:n], Liste[,nb])#naive bayes
table(predict(classifier, Liste[,-nb]), Liste[,nb], dnn=list('predicted','actual'))
print("--------------------------------------------------")
print(classifier)

print("--------------------------------------------------")
print(classifier$tables)
print("--------------------------------------------------")
#new data #15
Liste[nrow(Liste)+1, -ncol(Liste)] <- as.factor(f)
result <- predict(classifier, Liste[nrow(Liste),-ncol(Liste)],type="class" )
res <- predict(classifier, Liste[15,-5],type="raw" )
r<-paste("No",res[1])

r1<-paste("Yes",res[2])
r2<-paste(r,r1)
 plot(result,pch=22,col="red", xlab=r2)
return(res)
}


