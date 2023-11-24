
#+++++++++ A prime number (or a prime) is a natural number greater ++++++++++++
#+++++++++ than 1 that is not a product of two smaller natural numbers+++++++++
#+++++++++ prim_checker function ++++++++++++++++++++++++++++++++++++++++++++++ 
prim_checker <-function(number){ 
  isprime<-1  # n%% 1,n is 0 then n is prim   so we check n%% (2:n-1) if %%==0 number is not prim
  for (i in (3:number-1)) {
    print(i)
    d=number%%i
    if(d==0){
      print("=====================NOT Prim===============")
      isprime<-0
    }
  }
  return(isprime)
}

input_number<-72
prim_checker<- prim_checker(input_number)
if(prim_checker==1){
  print("Input number is Prim")}else{
    print("Input number is Not Prim")}


