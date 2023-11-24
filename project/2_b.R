#+++++++++ A prime number (or a prime) is a natural number greater ++++++++++++
#+++++++++ than 1 that is not a product of two smaller natural numbers+++++++++

#+++++++++ prim_checker function ++++++++++++++++++++++++++++++++++++++++++++++ 
prim_checker <-function(number){ 
  isprime<-1  # n%% 1,n is 0 then n is prim   so we check n%% (2:n-1) if %%==0 number is nut prim
  for (i in (3:number-1)) {
    d=number%%i
    if(d==0){
      # print("=====================NOT Prim===============")
      isprime<-0
    }}
  return(isprime)
}

################### prims_between_1to_n__ #######################################
prims_between_1to_n__<-function(n){
# for (i in 2:100000)  prime numbers between 2 to 100,000
    prim_numbers<- c()
    for (i in 2:n){
      print(paste("i=>",i))
      prim_check<- prim_checker(i)
      # print(i)
      if(prim_check==1){
        print("Input number is Prim")
        prim_numbers <- append(prim_numbers, i)
      }else{
        print("Input number is Not Prim")}
      print(paste("len_pim_numbers==>",length(prim_numbers)))
      i<-i+1
    }
    return(prim_numbers)
}    


################### n_prims #####################################################
n__prim_numbers_<-function(n){
    prim_numbers<- c()
    i<-2  
    while (length(prim_numbers)!=n) {
      # print(paste("i=>",i))
      prim_check<- prim_checker(i)
      # print(i)
      if(prim_check==1){
        # print("Input number is Prim")
        prim_numbers <- append(prim_numbers, i)
      }else{
        print("Input number is Not Prim")
        }
      print(paste("len_pim_numbers==>",length(prim_numbers)))
      i<-i+1
    }
    return(prim_numbers)
}
################### n_prims #####################################################
prim_numbers<- n__prim_numbers_(100)
write.csv(prim_numbers, file = "n_prim_numbers_.csv",row.names = FALSE)

################### prims_between_1to_n__ #######################################
# prim_numbers<- prims_between_1to_n__(5)
# write.csv(prim_numbers, file = "prims_1_to_n.csv",row.names = FALSE)
