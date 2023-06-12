
#Part I
#Displaying the data with model pararmeters 
D <- 15000                 
P <- 80                    
SC <- 220                  
OC <- (18*P)/100          

EOQ <- ((2*D*SC)/OC)^0.5   
AC <- (D/EOQ)*SC          
AHC <- EOQ/2*OC            
TIC <- AC+AHC+(D*P)       
TIC         


#Part II

library(EnvStats)
library(ggplot2)

X <-rtri(1000,13000,17000,15000) 
EOQ2 <- ((2*X*SC)/(OC))^0.5      
AC2 <- (X/EOQ2)*SC               
AHC2 <- EOQ2/2*OC                
TIC2 <- AC2+AHC2                 
                     


# (i)- Estimating the minimal cost
TICmean<- mean(TIC2)
TIC2SD<- sd(TIC2)
TIC2SE<-TIC2SD/sqrt(1000)
Alpha = 0.05
Df= 1000-1
Tscore=qt(p=Alpha/2,df=Df,lower.tail = F)
Merror<-Tscore*TIC2SE
Lowbound<- TICmean-Merror
Upbound<-TICmean+Merror

# Plotting the histogram fir the same
hist(TIC2, 
     col = heat.colors(100, alpha = 0.3),            
     xlab = "minimum cost", main = "Total Inventory Cost")

#(ii)- Estimating the expected order quantity
EOQm<- mean(EOQ2)
EOQ2SD<- sd(EOQ2)
EOQ2SE<-EOQ2SD/sqrt(1000)
Merror1<-Tscore*EOQ2SE
Lowbound1<- EOQm-Merror1
Upbound1<-EOQm+Merror1

#(iii)- Estimating the annual number of orders
AOCm<- mean(AC)
AOCSD<- sd(AC)
AOCSE<-AOCSD/sqrt(1000)
Merror2<-Tscore*AOCSE
Lowbound2<- AOCm-Merror2
Upbound2<-AOCm+Merror2
