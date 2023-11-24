pdf("4_box_plot_baseR.pdf")
library(dichromat)
# plot_box nd color it
box_plot <- function(min_q1_q2_q3_max_c){
  plot_color<-c()
  if(min_q1_q2_q3_max_c[6]%%2 !=0){
    plot_color<-"red"
  }else{plot_color<-"blue"}
  
  x_coord=c(min_q1_q2_q3_max_c[6]-0.5,min_q1_q2_q3_max_c[6]+0.5 ,min_q1_q2_q3_max_c[6]+0.5,min_q1_q2_q3_max_c[6]-0.5)  #>> min_q1_q2_q3_max_c[6] counter
  print(paste("x_coord=>",x_coord))
  y_coord=c(min_q1_q2_q3_max_c[2],min_q1_q2_q3_max_c[2],min_q1_q2_q3_max_c[4],min_q1_q2_q3_max_c[4])  #q1 to q3 >>min_q1_q2_q3_max_c[]
  print(paste("y_coord=>",y_coord))
  # polygon(x_coord,y_coord,col=colors[round(min_q1_q2_q3_max_c[6]*100)+1])
  polygon(x_coord,y_coord,col=plot_color)
  # line col=c("slateblue1" , "tomato")
  segments(x0=min_q1_q2_q3_max_c[6]-0.5,y0=min_q1_q2_q3_max_c[3],x1=min_q1_q2_q3_max_c[6]+0.5,y1=min_q1_q2_q3_max_c[3],col="green") #q2 min_q1_q2_q3_max_c[]
  segments(x0=min_q1_q2_q3_max_c[6],y0=min_q1_q2_q3_max_c[1],x1=min_q1_q2_q3_max_c[6],y1=min_q1_q2_q3_max_c[2],col="red") #min to q1  min_q1_q2_q3_max_c[]
  segments(x0=min_q1_q2_q3_max_c[6],y0=min_q1_q2_q3_max_c[4],x1=min_q1_q2_q3_max_c[6],y1=min_q1_q2_q3_max_c[5],col="red") #q3 to max  min_q1_q2_q3_max_c[]
}


 ######## find min q1-q2-q3-max ################################
Find_box_items <- function(data){
  data<- sort(data)
  l_v= c()
  min_num<- data[1]
  # print(min_num)
  l_v<-append(l_v, min_num)
  
  q1<- (length(data)+1)/4
  # print(q1)
  # print(data[q1])
  l_v<-append(l_v, data[q1])
  
  q2<- (length(data)+1)/2
  # print(q2)
  # print(data[q2])
  l_v<-append(l_v, data[q2])
  
  q3<- ((length(data)+1)/4)*3
  # print(q3)
  l_v<-append(l_v, data[q3])
  # print(data[q3])
  # ----------------outliers--------------------------
  # get median
  med = median(data)
  # subtract median from each value of x and get absolute deviation
  abs_dev = abs(data-med)
  # get MAD
  mad = 1.4826 * median(abs_dev)
  
  # get threshold values for outliers
  Tmin = med-(3*mad) 
  Tmax = med+(3*mad) 
  
  # find outlier
  out<- data[which(data < Tmin | data > Tmax)]
  print(paste("+++++++++++++++++++outlier===> ", out))
  # if(out){
  #   l_v<-append(l_v, out)
  # }else{
  #   l_v<-append(l_v, "0")
  # }
  # 
  # -------------------outliers-----------------------
  # outlier_index<-which(data == out)
  # 
  # print(paste("outlier_index==>",outlier_index))
  # data[-outlier_index]
  # max_num<- data[length(data[-outlier_index])]
  max_num<- data[length(data)]
  l_v<-append(l_v, max_num)
  # print(max_num)
  return(l_v)
  # print(l_v)
}

######################## main ##########################################################
col1 = c(1,4,5,9,45,15,21) 
col2 = c(7,3,14,9,7,8,12)
col3 =  c(3,16,17,18,19,20,25) 
col4 =  c(31,13,16,18,19,20,40)
col5 =  c(2,12,21,25,31,34,43)
col6 =  c(31,37,43,48,12,10,16)
col7 = c(1,4,5,9,12,15,21)
col8 =c(7,3,14,9,7,8,12)
col9 =  c(3,16,17,18,19,20,25)
col10 =  c(31,13,16,18,19,20,10)
col11 =  c(2,12,21,25,31,14,13)
col12 =  c(31,30,13,18,13,10,16)
col13 = c(1,4,5,9,12,15,21)
col14=c(7,3,14,9,7,8,12)
# note <- c( rep(c(sample(0:4, 20 , replace=T) , sample(1:6, 20 , replace=T)),2),
#            rep(c(sample(5:7, 20 , replace=T), sample(5:9, 20 , replace=T)),2),
#            c(sample(0:4, 20 , replace=T) , sample(2:5, 20 , replace=T),
#              rep(c(sample(6:8, 20 , replace=T) , sample(7:10, 20 , replace=T)),2) ))
# for(i in note){print(i)}
tframe = data.frame(col1, col2, col3,col4,col5,col6,col7,col8,col9,col10,col11,col12,col13,col14)

####################### draw plot with x and y ##################################
# plot without with x-axis
# x11()
plot(0:50, xlab="groups",boxwex=0.2 , ylab="sickness",
     main="sickness of several wheat lines" , 
     col=c("red", "blue") ,  
     xaxt="n",
     type = "n")

counter=1   
# for(i in aframe)
for(i in tframe)  
{
  print(i)
  
  print("========Find_min_q1_q2_q3_max=========")
  Find_min_q1_q2_q3_max <- Find_box_items(i)
  print(Find_min_q1_q2_q3_max)
  Find_min_q1_q2_q3_max <- append(Find_min_q1_q2_q3_max,counter)
  print(Find_min_q1_q2_q3_max)
  counter<- counter+3
  box_plot(Find_min_q1_q2_q3_max)
}


axis(1, 
     at = seq(2 , 45 ,6.5), 
     # at = seq(0.5 , 14 , 2), 
     labels = c("sold", "silur", "lloy", "pesc", "X45", "Dud", "Clss") , 
     tick=FALSE , cex=0.3)


# Add the grey vertical lines
for(i in seq(0, 46 , 6)){ 
  abline(v=i,lty=1, col="grey")
}
# Add a legend
legend(35, 45, legend= c("High treatment", "Low treatment"),
       col=c("red", "blue"),  pch = 15, bty = "1", pt.cex = 3, cex = 1.2)

# dev.off()


dev.off()


