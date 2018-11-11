library(ggplot2)
library(dplyr)


setwd("D:/Ilustration-proc-dure")

# Generate Data mu1 & r1-----------------------------------------------------------

Nbre1 <- 50
Nbre2 <- 1000
Nbre3 <- 20
Nbre4 <- 20
Nbre <- Nbre1 + Nbre2 
NT1 <- 120
NT2 <- 500

a <- 0.0004
b <- 0.25
c <- 0.05
sd <- 0.03

Data <- data.frame(N = seq(1:Nbre))
Data$mu1 <- rep(NA,Nbre)

Data$mu1[1:Nbre1] <- 0
Data$mu1[(Nbre1+1):(Nbre1 + Nbre2)] <- a*(Data$N[(Nbre1+1):(Nbre1 + Nbre2)] - Nbre1)
Data$mu1[(NT1+1):(NT1+Nbre3)] <- a*(NT1 - Nbre1) + c
Data$mu1[(NT2+1):(NT2+Nbre4)] <- a*(NT2 - Nbre1) + b

Data$r1 <- Data$mu1 + rnorm(n = Nbre,sd = sd) 
 
ggplot(Data,aes(x = N, y = mu1)) + geom_line()
ggplot(Data,aes(x = N, y = r1))  + geom_point() 
write.table(Data[,c("N","mu1")],"mu1.txt",sep = "\t",row.names = FALSE,col.names = F)

# Generate Data mu2 & r2-----------------------------------------------------------


Nbre1 <- 50
Nbre2 <- 1000
Nbre3 <- 20
Nbre4 <- 20
Nbre <- Nbre1 + Nbre2 
NT1 <- 120
NT2 <- 500

a <- 0.0004
b <- 0.25
c <- 0.05
sd <- 0.03

Data$mu2 <- rep(NA,Nbre)

Data$mu2[1:Nbre1] <- 0
Data$mu2[(Nbre1+1):(Nbre1 + Nbre2)] <- a*(Data$N[(Nbre1+1):(Nbre1 + Nbre2)] - Nbre1)
Data$mu2[(NT2+1):(NT2+Nbre4)] <- a*(NT2 - Nbre1) - b


Data$r2 <- Data$mu2 + rnorm(Nbre,sd = sd) 

ggplot(Data,aes(x = N, y = mu2)) + geom_line()
ggplot(Data,aes(x = N, y = r2))  + geom_point() 
write.table(Data[,c("N","mu2")],"mu2.txt",sep = "\t",row.names = FALSE,col.names = F)



# Labellize the data : 3 trajectory ---------------------------------------

Data$traj[1:(Nbre1 + Nbre2)]  <- "traj1"
Data$traj[(NT1+1):(NT1+Nbre3)]  <- "traj2"
Data$traj[(NT2+1):(NT2+Nbre4)]  <- "traj3"

# Delete a part of data to simulate NSS periods ---------------------------

NSSP <- 0.1
DataBis <- Data[-IndexDelete,]
DataBis <- DataBis[-sample(1:nrow(DataBis),NSSP*nrow(DataBis)),]

IndexDelete <- c((NT2+50): (Nbre2-100))


ggplot(DataBis,aes(x = N, y = r1))  + geom_point() 
ggplot(DataBis,aes(x = N, y = r2))  + geom_point() 
ggplot(DataBis,aes(x = r1, y = r2))  + geom_point() 
 


write.table(DataBis[,c("N","r1")],"r1.txt",sep = "\t",row.names = FALSE,col.names = F)

write.table(DataBis[,c("N","r2")],"r2.txt",sep = "\t",row.names = FALSE,col.names = F)

# Data in the indicators' space -------------------------------------------


ggplot(DataBis,aes(x = r1,y = r2)) + geom_point()
ggplot(DataBis,aes(x = mu1,y = mu2)) + geom_line()


# Save into 3 trajectories ------------------------------------------------

Data %>%
      filter(traj == "traj1") %>%
      select(mu1,mu2)%>%
      write.table(.,"traj1.txt",sep = "\t",row.names = FALSE,col.names = F)

Data %>%
  filter(traj == "traj2") %>%
  select(mu1,mu2)%>%
  write.table(.,"traj2.txt",sep = "\t",row.names = FALSE,col.names = F)

Data %>%
  filter(traj == "traj3") %>%
  select(mu1,mu2)%>%
  write.table(.,"traj3.txt",sep = "\t",row.names = FALSE,col.names = F)


DataBis %>%
  filter(traj == "traj1") %>%
  select(r1,r2)%>%
  write.table(.,"traj1R.txt",sep = "\t",row.names = FALSE,col.names = F)

DataBis %>%
  filter(traj == "traj2") %>%
  select(r1,r2)%>%
  write.table(.,"traj2R.txt",sep = "\t",row.names = FALSE,col.names = F)

DataBis %>%
  filter(traj == "traj3") %>%
  select(r1,r2)%>%
  write.table(.,"traj3R.txt",sep = "\t",row.names = FALSE,col.names = F)


