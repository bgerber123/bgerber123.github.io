library(pwr)
library(ggplot2)

#Consider different mean differences

#THIS IS NEW
  Group1.Mean <- c(40,80,100)
  
#THIS IS THE SAME
  Group1.SD <- 20
  
  Group2.Mean <- 120
  Group2.SD <- 20

  effect.size <- Group1.Mean-Group2.Mean
  group.sd <- sqrt(mean(Group1.SD^2,Group2.SD^2))

  d <- effect.size/group.sd
  
  power = matrix(seq(0.8,0.99,by=0.01))

#THIS IS NEW  
  power.d=expand.grid(power,d)
  power.d$Var1=as.numeric(power.d$Var1)
  
#changed function to allow multiple elements of x
my.func = function(x,x2){                    
                      pwr.t.test(d=x2,power=x,
                                 type="two.sample",
                                 alternative="two.sided"
                                )$n
}


#same but new input
out= mapply(power.d$Var1,power.d$Var2, FUN=my.func)

head(out)

out2=cbind(power.d,out)
colnames(out2)=c("power","d","n")
head(out2)

#prepare for ggplot2
out2$d=as.factor(out2$d)
out2=data.frame(out2)

ggplot(data=out2, aes(x=n, y=power, group=d, color=d)) +
    geom_line(size=2)
