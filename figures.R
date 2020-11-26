### Figures for MATH 564 project


### Import data
setwd("~/Documents/MSC/2020W1/MATH564/MATH564_sims/r_NA")
temp<-list.files(pattern="*.csv")
r0_sims<-lapply(temp,read.csv)

setwd("~/Documents/MSC/2020W1/MATH564/MATH564_sims/r_005")
temp<-list.files(pattern="*.csv")
r005_sims<-lapply(temp,read.csv)

setwd("~/Documents/MSC/2020W1/MATH564/MATH564_sims/r_01")
temp<-list.files(pattern="*.csv")
r01_sims<-lapply(temp,read.csv)

setwd("~/Documents/MSC/2020W1/MATH564/MATH564_sims/r_025")
temp<-list.files(pattern="*.csv")
r025_sims<-lapply(temp,read.csv)

setwd("~/Documents/MSC/2020W1/MATH564/MATH564_sims/r_05")
temp<-list.files(pattern="*.csv")
r05_sims<-lapply(temp,read.csv)

## functions 

leadingedge<-function(x){
  extent<-max(x$x_loc)
  subset(x,x_loc>extent-2)
}

## colours

C_col<-"#7FB069"
D_col<-"#D36135"

## compute distributions of lambda, C/D

temp<-lapply(r0_sims,"[",,5)
r0_C<-unlist(lapply(temp,sum))/unlist(lapply(temp,length))*100

temp<-lapply(r005_sims,"[",,5)
r005_C<-unlist(lapply(temp,sum))/unlist(lapply(temp,length))*100
temp<-lapply(r005_sims,"[",,4)
r005_L<-unlist(lapply(temp,mean))

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

## compute lambda and C prop for leading edge

edge_r0<-lapply(r0_sims,leadingedge)
temp<-lapply(edge_r0,"[", ,5)
r0edge_C<-unlist(lapply(temp,sum))/unlist(lapply(temp,length))*100

edge_r005<-lapply(r0_sims,leadingedge)
temp<-lapply(edge_r005,"[", ,5)
r005edge_C<-unlist(lapply(temp,sum))/unlist(lapply(temp,length))*100
temp<-lapply(edge_r005,"[",,4)
r005edge_L<-unlist(lapply(temp,mean))

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

## Example extent figure with lambda distributions

rep_r0<-r0_sims[[4]]
rep_r05<-r05_sims[[4]]
plot(rep_r05$x_loc,rep_r05$y_loc,
     col=C_col,
     pch=4)
points(subset(rep_r05,parent_trait==0)[,1],subset(rep_r05,parent_trait==0)[,2],
       col=D_col,
       pch=4)

## Proportion of C/D population vs leading edge


## Extent and mean lambda boxplots
