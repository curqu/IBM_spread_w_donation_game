#### IBM MATH564 v1 #################################################################

#### 04 November 2020

#### Additions from THESIS v0
## set population dynamics parameters constant
## change growth to follow simple logistic model
## add no. of neighbours column to inds matrix
## remove all traits from inds matrix, add C/D
## add mutation rate mu
## add fn to track strategies of neighbours

#### TO DO

## fix boundary problem?

####################################################################################

## environment parameters (don't change)
hood<-1               # maximum distance for interactions between neighbours
                      # used for computing neighbourhood in growth fn
width<-8              # width of landscape (y dimension)
length<-600           # length of landscape (x dim.)

## population parameters (don't change)
n0<-10                # number of starting individuals
lambda_base<-10       # base lambda value
K<-20                 # carrying capacity per neighbourhood
m<-1                  # dispersal (m) value
mu<-0.02              # mutation rate

## change
b<-1                  # fitness benefit
c<-0.9                # fitness cost
r<-c/b               # cost / benefit ratio

## label columns
x_col=1
y_col=2
lambda_col=3
pt_col=4
nbr_col=5
nc_col=6
repr_col=7

#### Functions for dispersal and reproduction

# model dispersal with exponential kernel, random angle
# works with initial step-through but needs more vigorous testing 
dispersal<-function(inds,xmax=length,ymax=width){
  # inds is array of individuals
  # xmax is length of landscape
  # ymax is width of landscape
  # m is coloumn corresponding to m values
  
  total_inds<-dim(inds)[1]; # get number of individuals
  dists<-rexp(total_inds,rate=m)    # define distances
  theta<-runif(total_inds,min=0,max=2*pi)   # define random direction
  delta_x<-dists*cos(theta);
  delta_y<-dists*sin(theta);
  inds[,x_col]<-inds[,x_col]+delta_x;
  inds[,y_col]<-inds[,y_col]+delta_y;
  # add reflecting boundary
  for (i in 1:total_inds){
    if (inds[i,x_col]>xmax){
      inds[i,x_col]<-2*xmax-inds[i,x_col];
    }
    if (inds[i,x_col]<1){
      inds[i,x_col]<-2-inds[i,x_col];
    }
    if (inds[i,y_col]>ymax){ #something weird here?
      inds[i,y_col]<-2*ymax-inds[i,y_col];
    }
    if (inds[i,y_col]<1){
      inds[i,y_col]<-2-inds[i,y_col];
    }
  }
  return(inds)
}

# function for reproduction
# need testing after changes
birth<-function(inds){
  total_inds<-dim(inds)[1]
  ind_cols<-dim(inds)[2]
  # count neighbours & cooperators
  for (i in 1:total_inds){
    nbr<-neighbours(inds[i,],inds)
    inds[i,nbr_col]<-sum(nbr)
    inds[i,nc_col]<-sum(ifelse(nbr,inds[,pt_col],0))
    if (inds[i,pt_col]==1){
      inds[i,nc_col]<-inds[i,nc_col]-1
    }
  }
  # adjust lambdas
  for (i in 1:total_inds){
    ## optional allee effect boost
    # if (inds[i,pt_col]==1 && inds[i,nc_col])
    #   inds[i,lambda_col]<-0
    ##
   # else 
      inds[i,lambda_col]<-max(inds[i,lambda_col]+payoffs(inds[i,]),0)
  }
  # make the babies
  for (i in 1:total_inds){
    if (inds[i,pt_col]==1 && inds[i,nbr_col]==0){
      inds[i,repr_col]==0
    }
    else
  inds[i,repr_col]<-growth(inds[i,])
  }
  total_births<-sum(inds[,repr_col])
  # make them land in the same location as parent, with same traits
 index<-rep(1:total_inds,inds[,repr_col])
 new_inds<-inds[index,]
 new_inds[,repr_col]<-NA
 new_inds[,nbr_col]<-NA
 new_inds[,nc_col]<-NA
 new_inds[,lambda_col]<-lambda_base
 # mutation for C/D
 new_inds[,pt_col]<-mutate(new_inds)
 
  return(new_inds)
}

# function to calculate growth rate
# basic testing done and working as expected
growth<-function(parent){
  # then apply logistic growth model
  det.growth<-(parent[lambda_col]*(1-parent[nbr_col]/K)) 
  det.growth<-max(0,det.growth)
  return(det.growth)
} 

# function to count number of individuals in neighbourhood
# tested, works as expected
neighbours<-function(parent,inds=inds,nhood=hood){
  neighbours_x<-inds[,x_col]>=parent[x_col]-nhood & 
    inds[,x_col]<=parent[x_col]+nhood
  neighbours_y<-inds[,y_col]>=parent[y_col]-nhood & 
    inds[,y_col]<=parent[y_col]+nhood
  k<-neighbours_x & neighbours_y
  return(k)
}

# function to update lambda due to interactions
# needs testing
payoffs<-function(parent){
  
  if (parent[pt_col]==0){
    return(parent[nc_col]*b)
  }
  else
  return(parent[nc_col]*b-parent[nbr_col]*c)
}

# function to add mutations
# tested works as expected
mutate<-function(inds){
  rm<-runif(dim(inds)[1],0,1)
  mutation<-rm<mu
  pt_not<-abs(inds[,pt_col]-1)
  newtraits<-ifelse (mutation, pt_not, inds[,pt_col])
  return(newtraits)
}


#run simulations
x<-1
n<-50
while(x<=n){
## create a table to store info about individuals
inds0 <- array(data =0, dim=c(n0,8))
colnames(inds0)<- c("x_loc","y_loc","lambda","parent_trait","nbr","nbr_c","repr","line")
# set locations
inds0[,1]<-1
inds0[,2]<-runif(n=n0,min=1,max=width)
# set reproduction parameters
inds0[,3]<-lambda_base
# set C/D trait 1: cooperators, 0: defectors
inds0[,4]<-1
# tag gen 1
inds0[,8]<-1:n0
inds<-inds0

# simulate movement over 20 time steps
ts<-0
time_steps<-30
while(ts<time_steps){
  babies<-birth(inds);
  inds<-dispersal(babies)
  ts<-ts+1
}
# adjust lambdas
total_inds<-dim(inds)[1]
ind_cols<-dim(inds)[2]
# count neighbours & cooperators
for (i in 1:total_inds){
  nbr<-neighbours(inds[i,],inds)
  inds[i,nbr_col]<-sum(nbr)
  inds[i,nc_col]<-sum(ifelse(nbr,inds[,pt_col],0))
  if (inds[i,pt_col]==1){
    inds[i,nc_col]<-inds[i,nc_col]-1
  }
}
# adjust lambdas
for (i in 1:total_inds){
  inds[i,lambda_col]<-max(inds[i,lambda_col]+payoffs(inds[i,]),0)
}
write.csv(inds,file=paste0("inds_",r,"_0",x,".csv"))
x<-x+1
}

#for plotting
indsdf<-as.data.frame(inds)
indsdf<-inds_0.9_010
extent<-max(indsdf$x_loc)
leadingedge<-subset(indsdf,x_loc>extent-5)
hist(leadingedge$parent_trait)
hist(indsdf$parent_trait)
hist(indsdf$x_loc)
hist(indsdf$lambda)
hist(leadingedge$lambda)
hist(indsdf$line)
hist(leadingedge$line)
coops<-subset(indsdf,parent_trait==1)
defs<-subset(indsdf,parent_trait==0)
plot(coops$y_loc~coops$x_loc
     ,xlim=c(0,100)
     )
points(defs$x_loc,
       defs$y_loc,col="green")

