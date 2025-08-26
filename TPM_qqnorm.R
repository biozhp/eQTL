gene <- read.table("exp_input_qqnorm.txt",header=T,row.names = 1,sep="\t")
mydata<-list()
for (i in seq_along(gene)){
 mydata[[i]] <- (qqnorm(gene[[i]]))$x
}
write.table(mydata,file="qqnorm.txt",quote=F,row.names=T,col.names=T,sep="\t")
