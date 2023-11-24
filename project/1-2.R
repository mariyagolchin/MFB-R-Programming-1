
############### create pdf file############################################################
pdf("1_Multiplication table.pdf")
#################### Creating a plot and save to pdf ######################################

plot(10:0,10:0,type = "n",xlab = "0 to 10", ylab = "0 to 10") # draw an empty plot
for (i in 0:10) {
  for (j in 0:10) {
    ij<- i*j
    x_coord=c(j,j+1,j+1,j)
    print(paste("x_coord=>",x_coord))
    y_coord=c(i,i,i+1,i+1)
    print(paste("y_coord=>",y_coord))
    polygon(x_coord,y_coord )
    # text(x_coord-0.5,y_coord-0.5, labels=ij)
    text(j-0.5,i-0.5 , labels=ij)
    }
}
# Closing the graphical device
dev.off()
