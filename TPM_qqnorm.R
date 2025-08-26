## The data matrix is assumed to have N rows and G columns, where N is the number of geness, and G is the number of samples.
df <- read.table("TPM.txt",header=T,row.names = 1,sep="\t")
gene <- data.frame(t(df))
mydata<-list()
for (i in seq_along(gene)){
  mydata[[i]] <- (qqnorm(gene[[i]]))$x
}
write.table(mydata,file="qqnorm.txt",quote=F,row.names=F,col.names=F,sep="\t")
