library(equatiomatic)
library(marginaleffects)

#Steps

#fit models

y~1
y~sex
y~age
y~site
y~age+sex
y~age+site
y~age*sex+site
y~age*sex*site

#check diagnostics
#plot predictions

#BDG NOTES
eleph=read.csv("elephant.study.csv")

head(eleph)

hist(eleph$weight)

#constant model
out=glm(y~1,family=gaussian(link = "identity"),
        data=eleph)


out=glm(y~sex,family=gaussian(link = "identity"),
        data=eleph)

summary(out)

par(mfrow=c(2, 2)) # create a 2 x 2 panel plot
plot(out)

library(ggResidpanel)
#resid_panel(out)

summary(out)

library(performance)
check_model(out, check = c("linearity", "homogeneity", "qq", "normality"))

#age*sex model
b=glm(y~age*sex,family=gaussian(link = "identity"),
        data=eleph)

extract_eq(b)

plot(eleph$age,predict(b))

mar=marginaleffects(b)
summary(mar)

comparisons(b)



comparisons(b,varibles=c("age","sex"))



plot.data=data.frame(predict(b),eleph$sex,eleph$age.years)
head(plot.data)

library(ggplot2)
qplot(x = eleph.age.years, y = predict.b., facets = ~eleph.sex,
      data=plot.data, color=eleph.sex) +
  geom_smooth(method = "lm")

#Week 5

Start with linear regression
diagnostics
bootstrapping

#Need to assign work - evaluate sampling distribution?
#create correlation and another assumption violation