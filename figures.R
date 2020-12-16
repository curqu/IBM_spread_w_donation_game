### Figures for MATH 564 project


### Import data
setwd("~/Documents/MSC/2020W1/MATH564/MATH564_sims/r_NA")
temp<-list.files(pattern="*.csv")
r0_sims<-lapply(temp,read.csv)

setwd("~/Documents/MSC/2020W1/MATH564/MATH564_sims/r_01")
temp<-list.files(pattern="*.csv")
r01_sims<-lapply(temp,read.csv)

setwd("~/Documents/MSC/2020W1/MATH564/MATH564_sims/r_025")
temp<-list.files(pattern="*.csv")
r025_sims<-lapply(temp,read.csv)

setwd("~/Documents/MSC/2020W1/MATH564/MATH564_sims/r_05")
temp<-list.files(pattern="*.csv")
r05_sims<-lapply(temp,read.csv)


setwd("~/Documents/MSC/2020W1/MATH564/MATH564_sims/r_075")
temp<-list.files(pattern="*.csv")
r075_sims<-lapply(temp,read.csv)


setwd("~/Documents/MSC/2020W1/MATH564/MATH564_sims/r_09")
temp<-list.files(pattern="*.csv")
r09_sims<-lapply(temp,read.csv)

## functions 

leadingedge<-function(x){
  extent<-max(x$x_loc)
  subset(x,x_loc>extent-2)
}

colvect<-function(x){
  out<-vector("numeric",length(x))
  for (i in 1:length(x)){
    ifelse(x[i]<5,
           out[i]<-D_col,
           ifelse(x[i]>95,out[i]<-C_col,out[i]<-mid_col))
  }
  return(out)
}

## colours

C_col<-"#7FB069"
D_col<-"#D36135"
mid_col<-"#ac9130"
main_col<-"#46879D"
edge_col<-"#846487"

## compute distributions of lambda, C/D

temp<-lapply(r0_sims,"[",,5)
r0_C<-unlist(lapply(temp,sum))/unlist(lapply(temp,length))*100

temp<-lapply(r075_sims,"[",,5)
r075_C<-unlist(lapply(temp,sum))/unlist(lapply(temp,length))*100
temp<-lapply(r075_sims,"[",,4)
r075_L<-unlist(lapply(temp,mean))

temp<-lapply(r01_sims,"[",,5)
r01_C<-unlist(lapply(temp,sum))/unlist(lapply(temp,length))*100
temp<-lapply(r01_sims,"[",,4)
r01_L<-unlist(lapply(temp,mean))

temp<-lapply(r025_sims,"[",,5)
r025_C<-unlist(lapply(temp,sum))/unlist(lapply(temp,length))*100
temp<-lapply(r025_sims,"[",,4)
r025_L<-unlist(lapply(temp,mean))

temp<-lapply(r05_sims,"[",,5)
r05_C<-unlist(lapply(temp,sum))/unlist(lapply(temp,length))*100
temp<-lapply(r05_sims,"[",,4)
r05_L<-unlist(lapply(temp,mean))

temp<-lapply(r09_sims,"[",,5)
r09_C<-unlist(lapply(temp,sum))/unlist(lapply(temp,length))*100
temp<-lapply(r09_sims,"[",,4)
r09_L<-unlist(lapply(temp,mean))

## compute range of extents
temp<-lapply(r0_sims,"[",2)
r0_ext<-unlist(lapply(temp,max))

temp<-lapply(r01_sims,"[",2)
r01_ext<-unlist(lapply(temp,max))

temp<-lapply(r025_sims,"[",2)
r025_ext<-unlist(lapply(temp,max))

temp<-lapply(r05_sims,"[",2)
r05_ext<-unlist(lapply(temp,max))

temp<-lapply(r075_sims,"[",2)
r075_ext<-unlist(lapply(temp,max))

temp<-lapply(r09_sims,"[",2)
r09_ext<-unlist(lapply(temp,max))

## compute lambda and C prop for leading edge

edge_r0<-lapply(r0_sims,leadingedge)
temp<-lapply(edge_r0,"[", ,5)
r0edge_C<-unlist(lapply(temp,sum))/unlist(lapply(temp,length))*100

edge_r075<-lapply(r075_sims,leadingedge)
temp<-lapply(edge_r075,"[", ,5)
r075edge_C<-unlist(lapply(temp,sum))/unlist(lapply(temp,length))*100
temp<-lapply(edge_r075,"[",,4)
r075edge_L<-unlist(lapply(temp,mean))

edge_r01<-lapply(r01_sims,leadingedge)
temp<-lapply(edge_r01,"[",,5)
r01edge_C<-unlist(lapply(temp,sum))/unlist(lapply(temp,length))*100
temp<-lapply(edge_r01,"[",,4)
r01edge_L<-unlist(lapply(temp,mean))

edge_r025<-lapply(r025_sims,leadingedge)
temp<-lapply(edge_r025,"[",,5)
r025edge_C<-unlist(lapply(temp,sum))/unlist(lapply(temp,length))*100
temp<-lapply(edge_r025,"[",,4)
r025edge_L<-unlist(lapply(temp,mean))

edge_r05<-lapply(r05_sims,leadingedge)
temp<-lapply(edge_r05,"[",,5)
r05edge_C<-unlist(lapply(temp,sum))/unlist(lapply(temp,length))*100
temp<-lapply(edge_r05,"[",,4)
r05edge_L<-unlist(lapply(temp,mean))

edge_r09<-lapply(r09_sims,leadingedge)
temp<-lapply(edge_r09,"[",,5)
r09edge_C<-unlist(lapply(temp,sum))/unlist(lapply(temp,length))*100
temp<-lapply(edge_r09,"[",,4)
r09edge_L<-unlist(lapply(temp,mean))

## Example extent figure with lambda distributions

rep_r0<-r0_sims[[5]]
rep_r01<-r01_sims[[9]]
rep_r05<-r05_sims[[16]]
rep_r09<-r09_sims[[4]]

par(mfrow=c(4,1),mar=c(2,1,2,1),oma=c(2,1,2,1))
coops<-subset(rep_r0,parent_trait==1)
defs<-subset(rep_r0,parent_trait==0)
plot(coops$y_loc~coops$x_loc
     ,xlim=c(0,80), 
     col=C_col,
     pch=10,
     yaxt='n'
)
points(defs$x_loc,
       defs$y_loc,col=D_col,pch=10)
legend(x="topright",legend=c("C","D"),pch=10,col=c(C_col,D_col),cex=2.5,bty='n')
mtext("No selection",side=3,line=0,cex=1.2,adj=0)

coops<-subset(rep_r01,parent_trait==1)
defs<-subset(rep_r01,parent_trait==0)
plot(coops$y_loc~coops$x_loc
     ,xlim=c(0,80), 
     col=C_col,
     pch=10,
     yaxt='n'
)
points(defs$x_loc,
       defs$y_loc,col=D_col,pch=10)
mtext("r=0.1",side=3,line=0,cex=1.2,adj=0)

coops<-subset(rep_r05,parent_trait==1)
defs<-subset(rep_r05,parent_trait==0)
plot(coops$y_loc~coops$x_loc
     ,xlim=c(0,80), 
     col=C_col,
     pch=10,
     yaxt='n'
)
points(defs$x_loc,
       defs$y_loc,col=D_col,pch=10)
mtext("r=0.5",side=3,line=0,cex=1.2,adj=0)

coops<-subset(rep_r09,parent_trait==1)
defs<-subset(rep_r09,parent_trait==0)
plot(coops$y_loc~coops$x_loc
     ,xlim=c(0,80), 
     col=C_col,
     pch=10,
     yaxt='n'
)
points(defs$x_loc,
       defs$y_loc,col=D_col,pch=10)
mtext("r=0.9",side=3,line=0,cex=1.2,adj=0)
mtext("distance from origin",side=1,line=2)

## Proportion of C/D population vs leading edge
r<-c(0,0.1,0.25,0.5,0.75,0.9)
mean_C<-c(mean(r0_C),mean(r01_C),mean(r025_C),mean(r05_C),mean(r075_C),mean(r09_C))
meanedge_C<-c(mean(r0edge_C),mean(r01edge_C),mean(r025edge_C),mean(r05edge_C),mean(r075edge_C),mean(r09edge_C))

cv_C<-c(sd(r0_C)/mean(r0_C),sd(r01_C)/mean(r01_C),sd(r025_C)/mean(r025_C),sd(r05_C)/mean(r05_C),
          sd(r075_C)/mean(r075_C),sd(r09_C)/mean(r09_C))*100
cvedge_C<-c(sd(r0edge_C)/mean(r0edge_C),sd(r01edge_C)/mean(r01edge_C),sd(r025edge_C)/mean(r025edge_C),
              sd(r05edge_C)/mean(r05edge_C),sd(r075edge_C)/mean(r075edge_C),sd(r09edge_C)/mean(r09edge_C))*100

par(mfrow=c(1,1),mar=c(4,4,2,1),oma=c(1,1,1,1))
plot(mean_C~r,
     pch=16,
     col=main_col,
     cex=2,
     ylim=c(0,100),
     ylab="% cooperators",
     xlab="r",
     cex.lab=1.2)
points(r,meanedge_C,pch=17,
       col=edge_col,
       cex=2)
lines(r,mean_C,
      col=main_col,
      lty=2)
lines(r,meanedge_C,
      lty=3,
      col=edge_col)
legend(x="topright",legend=c("population","leading edge"),
       col=c(main_col,edge_col),
       pch=c(16,17),
       lty=c(2,3),
       bty="n",
       cex=1.5)

plot(cv_C~r,
     pch=16,
     col=main_col,
     cex=2,
     ylim=c(0,150),
     ylab="% cooperators",
     xlab="r")
points(r,cvedge_C,pch=17,
       col=edge_col,
       cex=2)
lines(r,cv_C,
      col=main_col,
      lty=2)
lines(r,cvedge_C,
      lty=3,
      col=edge_col)
## Fitness gradient & waveform

fit_diff_r01<-r01edge_L-r01_L
fit_diff_r025<-r025edge_L-r025_L
fit_diff_r05<-r05edge_L-r05_L
fit_diff_r075<-r075edge_L-r075_L
fit_diff_r09<-r09edge_L-r09_L
colvect_r01<-colvect(r01edge_C)
colvect_r025<-colvect(r025edge_C)
colvect_r05<-colvect(r05edge_C)
colvect_r075<-colvect(r075edge_C)
colvect_r09<-colvect(r09edge_C)
colvect_r0<-colvect(r0edge_C)



par(mfrow=c(2,1), mar=c(4,5,1,1),oma=c(0,0,1,1))
plot(fit_diff_r01~r01edge_C)
boxplot(fit_diff_r01,fit_diff_r025,fit_diff_r05,fit_diff_r075,fit_diff_r09)
plot(jitter(rep(0.1,50),amount=0.03),fit_diff_r01,
     col=colvect_r01,
     pch=10,
     xlim=c(0,1),
     ylim=c(-20,12),
     cex=1.2,
     xlab="r",
     ylab="Fitness difference (edge - population)",
     cex.lab=1.2)
points(jitter(rep(0.25,50),amount=0.03),fit_diff_r025,
       col=colvect_r025,
       pch=10,
       cex=1.2)
points(jitter(rep(0.5,50),amount=0.03),fit_diff_r05,
       col=colvect_r05,
       pch=10,
       cex=1.2)
points(jitter(rep(0.75,50),amount=0.03),fit_diff_r075,
       col=colvect_r075,
       pch=10,
       cex=1.2)
points(jitter(rep(0.9,50),amount=0.03),fit_diff_r09,
       col=colvect_r09,
       pch=10,
       cex=1.2)
legend(x="bottomright",
       legend=c("Defectors dominant","mixed","Cooperators dominant"),
       col=c(D_col,mid_col,C_col),
       pch=10,
       bty="n")
text(x=0.93,y=-12,"Leading edge population")

r0_edge_n<-unlist(lapply(edge_r0,nrow))
r01_edge_n<-unlist(lapply(edge_r01,nrow))
r025_edge_n<-unlist(lapply(edge_r025,nrow))
r05_edge_n<-unlist(lapply(edge_r05,nrow))
r075_edge_n<-unlist(lapply(edge_r075,nrow))
r09_edge_n<-unlist(lapply(edge_r09,nrow))

plot(jitter(rep(0.1,50),amount=0.03),r01_edge_n,
     col=colvect_r01,
     pch=10,
     xlim=c(0,1),
     ylim=c(0,70),
     cex=1.2,
     xlab="r",
     ylab="Leading edge population size",
     cex.lab=1.2)
points(jitter(rep(0.25,50),amount=0.03),r025_edge_n,
       col=colvect_r025,
       pch=10,
       cex=1.2)
points(jitter(rep(0.5,50),amount=0.03),r05_edge_n,
       col=colvect_r05,
       pch=10,
       cex=1.2)
points(jitter(rep(0.75,50),amount=0.03),r075_edge_n,
       col=colvect_r075,
       pch=10,
       cex=1.2)
points(jitter(rep(0.9,50),amount=0.03),r09_edge_n,
       col=colvect_r09,
       pch=10,
       cex=1.2)
points(jitter(rep(0.9,50),amount=0.03),r0_edge_n,
       col=colvect_r0,
       pch=10,
       cex=1.2)
    
  