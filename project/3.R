# 26_b) Find the probability that at least one person will get chosen more than once.
########################################## 3 part 1 ################################################################################### 
# k = 5  # number of people in room
# p(at least 1match)=1-p(nomatch)
p_birthday_match<- function(n_people){
            multipl_i<- 1
            # p(at least n person)=  1-p(no match)=1- (365/365. ...  .(365-n+1)/365)))
            for (i in (365-n_people+1):365) {
                  c<- i/365
                  # print(c)
                  multipl_i<- multipl_i*c
                  }
            p_nomatch_k<-multipl_i
            # p(at least 1match)=1-p(nomatch)
            p_match_k<- 1-p_nomatch_k
            p_match_k<-round(p_match_k, digits = 3)
            return(p_match_k)
}
# np<-c(3,5,23,100)
# for (i in np)
birthday_pro<- function(){
    pall_Match<-c()
    for (i in 1:100){
       print(paste("i>",i))
       PBirthdayMatch<-p_birthday_match(i)  
       print(PBirthdayMatch)
       pall_Match<-append(pall_Match,PBirthdayMatch)
       print(pall_Match)
       }
    plot(pall_Match, main="Probability of birthday match", xlab ="Number of People", ylab = "Probability of match", col="blue")
}


########################################## 3 part2 ################################################################################### 
p_match<- function(n_people){
    multipl_i<- 1
    # p(at least n person)=  1-p(no match)=1- (365/365. ...  .(365-n+1)/365)))
    for (i in (1000000-n_people+1):1000000) {
      c<- i/1000000
      # print(c)
      multipl_i<- multipl_i*c
    }
    p_nomatch_k<-multipl_i
    # p(at least 1match)=1-p(nomatch)
    p_match_k<- 1-p_nomatch_k
    p_match_k<-round(p_match_k, digits = 3)
    return(p_match_k)
}
# np<-c(3,5,23,100)
# for (i in np)
pr_26_b<- function(){
    pall_Match<-c()
    for (i in 1000:1000){
      print(paste("i>",i))
      PBirthdayMatch<-p_match(i)
      print(PBirthdayMatch)
      pall_Match<-append(pall_Match,PBirthdayMatch)
      print(pall_Match)
    }
    plot(pall_Match, main="Probability of birthday match", xlab ="Number of People", ylab = "Probability of match", col="blue")
}


################ 3 part 1########################
birthday_pro()
################ 3 part 2 - 26_b ################
pr_26_b()

