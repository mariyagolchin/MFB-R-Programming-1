library(gridExtra)
library(datasets)
#++++++++++++++++++++create result matrix 10*10 multiplication table +++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
v1<- c(1,2,3,4,5,6,7,8,9,10)
v2<- c(1,2,3,4,5,6,7,8,9,10)

result<- matrix(nrow = 10, ncol =10,dimnames = list(dim1 = v1, dim2 = v2)) # create empty matrix 10*10
print(result)  
for (i in v1) {
  for (j in v2) {
    ij<- i*j
    # print(ij)
    result[i,j]<-ij
  }
}
print(result)

pdf("multiplication10-10.pdf")

plot(c(-4,20),c(-4,20),type = "n",axes="FALSE",ann="FALSE")

colpallet= colorRampPalette(c("red","blue"))
col_vec=colpallet(201)
ylength=nrow(result)
c_loc=c()
for( i in nrow(result):1){
  z=1
  for (j in 1:ncol(result)){
    if (i==nrow(result)){
      text(j+0.5, i+3, colnames(result)[[j]], cex=0.5)
    }
    # text(i-1.5, j-1.5, result[i,j], cex=0.5)
    print(result[i,j])
    text(j+0.5, i-0.5, result[i,j], cex=0.5, srt=360)
    
    loc=result[i,j]
    print(loc)
    c_loc<- append(c_loc,loc)
    x_coord= c(j,j+1,j+1,j)
    y_coord= c(i,i,i-1,i-1)  
    polygon(x_coord,y_coord)
  }
  z=z+1
  
  text(-1, i-0.5, row.names(result)[i], cex=0.5)
}

dev.off()
#++++++++++++++++++++save output in PDF file +++++++++++++++++++++++++++++++++++++

data("result") # Write your dataframe name that you want to print in pdf
pdf("1_result.pdf", height = 11, width = 11)
grid.table(result)
dev.off()
