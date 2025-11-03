
##############

#streams

N = 700

groups = 7
N.group=c(50,100,250,75,75,100,50)

sum(N.group)

mu.gr = c(1,5,20,50,100,300,500)

y.gr=vector("list",groups)


for(i in 1:groups){
  set.seed(32432+i)
  y.gr[[i]] = rpois(N.group[i],mu.gr[i])
}


y.gr.df = matrix(do.call(c, y.gr),ncol=1,byrow=FALSE)

length(y.gr.df)

strata=rep(c("Strata1","Strata2","Strata3","Strata4",
             "Strata5","Strata6","Strata7"),each=N.group)

y.gr.df= data.frame(count= y.gr.df,
                    strata=strata)


hist(y.gr.df$count)
############

#Get optimal allocation

n = 100

strata.var=aggregate(y.gr.df$count, by=list(y.gr.df$strata),FUN=var)$x

optimal.nh = n*N.group*sqrt(strata.var) / sum(N.group*sqrt(strata.var))

optimal.nh

optimal.nh/n



N.fish= y.gr.df$count
N.fish=sample(N.fish,length(N.fish))

hist(N.fish)

write.csv(N.fish,file="N.fish.csv")

######################

#Now do it as if you don't know the specific strata

dat=read.csv("C:\\Users\\C825033651\\OneDrive - Colostate\\Documents\\GitHub\\bgerber123.github.io\\classfiles552\\ThompsonChapter11\\N.fish.csv")
dat=data.frame(dat)
colnames(dat)=c("Index","Count")


index1=which(dat$Count<=50)
index2=which(dat$Count>50 & dat$Count<=150)
index3=which(dat$Count>150 & dat$Count<=450)
index4=which(dat$Count>450)

dat$strata=NA

dat$strata[index1]="Strata1"
dat$strata[index2]="Strata2"
dat$strata[index3]="Strata3"
dat$strata[index4]="Strata4"

strata.size=unlist(table(dat$strata))
strata.size

700/4

dat$strata

strata.var=aggregate(dat$Count, 
                     by=list(dat$strata),FUN=var)$x

strata.size=aggregate(dat$Count, by=list(dat$strata),FUN=length)$x
strata.size=table(dat$strata)

n = 100

optimal.nh = n*strata.size*sqrt(strata.var) / sum(strata.size*sqrt(strata.var))

optimal.nh

optimal.nh/n

