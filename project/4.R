
library(dichromat)
box_plot <- function(min_q1_q2_q3_max_c){
  # print(m)
  # plot(c(0,20),c(0,20),col="white",xlab="",ylab="")
  colfun= colorRampPalette(c("blue","red"))
  colors=colfun(101)
  x_coord=c(min_q1_q2_q3_max_c[6]-0.5,min_q1_q2_q3_max_c[6]+0.5 ,min_q1_q2_q3_max_c[6]+0.5,min_q1_q2_q3_max_c[6]-0.5)  #>> min_q1_q2_q3_max_c[6] counter
  print(paste("x_coord=>",x_coord))
  y_coord=c(min_q1_q2_q3_max_c[2],min_q1_q2_q3_max_c[2],min_q1_q2_q3_max_c[4],min_q1_q2_q3_max_c[4])  #q1 to q3 >>min_q1_q2_q3_max_c[]
  print(paste("y_coord=>",y_coord))
  polygon(x_coord,y_coord,col=colors[round(min_q1_q2_q3_max_c[6]*100)+1])
  polygon(x_coord,y_coord,col=colors[round(1*100)+1])
  # line
  segments(x0=min_q1_q2_q3_max_c[6]-0.5,y0=min_q1_q2_q3_max_c[3],x1=min_q1_q2_q3_max_c[6]+0.5,y1=min_q1_q2_q3_max_c[3],col="green") #q2 min_q1_q2_q3_max_c[]
  segments(x0=min_q1_q2_q3_max_c[6],y0=min_q1_q2_q3_max_c[1],x1=min_q1_q2_q3_max_c[6],y1=min_q1_q2_q3_max_c[2],col="red") #min to q1  min_q1_q2_q3_max_c[]
  segments(x0=min_q1_q2_q3_max_c[6],y0=min_q1_q2_q3_max_c[4],x1=min_q1_q2_q3_max_c[6],y1=min_q1_q2_q3_max_c[5],col="red") #q3 to max  min_q1_q2_q3_max_c[]
}
# heat_map(ml)


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
  
  max_num<- data[length(data)]
  l_v<-append(l_v, max_num)
  # print(max_num)
  
  # print(l_v)
}



col1 = c(1,4,5,9,12,15,21) 
col2 = c(7,3,14,9,7,8,12)
col3 =  c(3,16,17,18,19,20,25) 
col4 =  c(31,13,16,18,19,20,40)
col5 =  c(2,12,21,25,31,34,43)
col6 =  c(31,37,43,48,50,60,16)

aframe = data.frame(col1, col2, col3,col4,col5,col6)
counter=0
plot(c(0,15),c(0,70),col="white",xlab="sickness of several wheat lines",ylab="sickness")
for(i in aframe){
  print(i)
  counter<- counter+2
  print("========Find_min_q1_q2_q3_max=========")
  Find_min_q1_q2_q3_max <- Find_box_items(i)
  # Find_min_q1_q2_q3_max <- Find_box_items(df[i])
  print(Find_min_q1_q2_q3_max)
  Find_min_q1_q2_q3_max <- append(Find_min_q1_q2_q3_max,counter)
  print(Find_min_q1_q2_q3_max)
  # print(paste("Find_min_q1_q2_q3_max==>",Find_min_q1_q2_q3_max))
  # print(Find_min_q1_q2_q3_max[3])
  box_plot(Find_min_q1_q2_q3_max)
  }




