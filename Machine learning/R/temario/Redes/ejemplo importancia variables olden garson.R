
library(NeuralNetTools)
library(caret)
load("compressbien.Rda")

control<-trainControl(method = "none") 

nnetgrid <-  expand.grid(size=c(15),decay=c(0.01))

red1 <- train(cstrength~ age + water+cement+blast, method = 'nnet',
data = compressbien,tuneGrid=nnetgrid,linout = TRUE,trControl=control)

olden(red1)

importancia<-olden(red1,bar_plot=FALSE)

impor<-importancia[order(-importancia$importance),,drop = FALSE]

# El signo aporta cierta información pero el orden de importancia
# hay que tomarlo en valor absoluto

impor$importance2<-abs(impor$importance)
barplot(impor$importance2,names=rownames(impor),cex.names = 2 )
