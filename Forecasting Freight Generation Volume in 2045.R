#The following are just copy of the above code.

fut_okinawa<- matrix(0,nrow = 7,ncol =9 )

change=47 #Only insert the desired row no

for (i in 1:7){
  fut_okinawa[i,1]<-fut_pop[change,i]#
  fut_okinawa[i,2]<-dt[141+change,5]#
}

fut_okinawa[1,3]<-fut_okinawa[1,2]
fut_okinawa[1,4]<-fut_okinawa[1,2]

for (i in 1:6){
  fut_okinawa[i+1,3]<-fut_okinawa[i,3] + 2.5*fut_okinawa[i,3]/100
  fut_okinawa[i+1,4]<-fut_okinawa[i,4] + 5*fut_okinawa[i,4]/100
  
}

fut_okinawa[1,5]<-dt[141+change,14]#
fut_okinawa[1,6]<-log(fut_okinawa[1,5])

for (m in 2:4){
  for (n in 1:7){
    fut_okinawa[n,m+5]<-coef(res.gls3)[1]+ coef(res.gls3)[2]*log(fut_okinawa[n,1])+
      coef(res.gls3)[3]*log(fut_okinawa[n,m]) 
    
  }
  
}

z<-abs(fut_okinawa[1,6]-fut_okinawa[1,7])

if (fut_okinawa[1,7]<fut_okinawa[1,6]){
  for (m in 7:9){
    for (n in 1:7){
    fut_okinawa[n,m]<-fut_okinawa[n,m]+z
    }
  }
}   else{ 
    for (m in 7:9){
      for (n in 1:7){
        fut_okinawa[n,m]<-fut_okinawa[n,m]-z
                    }
                  }
      }   

    


fut_okinawa<-data.frame(fut_okinawa)
fut_okinawa<-cbind(fut_okinawa,exp(fut_okinawa$X7))
fut_okinawa<-cbind(fut_okinawa,exp(fut_okinawa$X8))
fut_okinawa<-cbind(fut_okinawa,exp(fut_okinawa$X9))


row.names(fut_okinawa)<-c("2015","2020","2025","2030","2035","2040","2045")
colnames(fut_okinawa)<-c("pop","GRP_0%","GRP_0.5%","GRP_1%","real","log_real","log_est_GRP_0%","log_est_GRP_0.5%","log_est_GRP_1%","est_GRP_0%","est_GRP_0.5%","est_GRP_1%")

View(fut_okinawa)
