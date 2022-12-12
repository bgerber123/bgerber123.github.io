## Winter HSA of 5 Atlantic brant in RI

rm(list = ls())

library(dplyr) ## data formatting
library(ggplot2) ## visualization
library(lme4) ## fitting GLM

# Read in/format data ----
getwd()
brant <- read.csv("C:/Users/tmezebish/Desktop/NRS520_HSA/brant.csv", stringsAsFactors = TRUE) %>% 
  mutate(bird = as.factor(bird), year = as.factor(year), use = as.factor(use)) %>% 
  glimpse()


# Check out trends in data ----
## Distance to development
ggplot(brant, aes(x = dvlp_dist, group = use, fill = use, color = use)) +
  geom_histogram(alpha = 0.5, bins = 50) +
  # geom_density(alpha = 0.5) +
  theme(legend.position = "none") +
  theme_bw() +
  scale_fill_manual(values = c("darkorange", "blue2"), labels = c("Available", "Used"), name = "") +
  scale_color_manual(values = c("darkorange", "blue2"), labels = c("Available", "Used"), name = "") +
  labs(x = "Distance to development (m)", y = "Count") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, face = "bold"), axis.text.y = element_text(face = "bold"), axis.title = element_text(size = 12), legend.text = element_text(face = "bold"), legend.position = "bottom") +
  theme(panel.grid.minor = element_blank())

## Habitat type
ggplot(brant, aes(x = habitat, fill = use))+
  geom_bar(position = position_dodge2(), alpha = 0.75) +
  theme_bw() +
  scale_x_discrete(labels=c("Estuarine \n Wetland", "Estuarine \n Open Water", "Grass")) +
  scale_fill_manual(values = c("darkorange", "blue2"), labels = c("Available", "Used"), name = "") +
  labs(x = "", y = "Count") +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, face = "bold"), axis.text.y = element_text(face = "bold"), axis.title = element_text(size = 12), legend.text = element_text(face = "bold"), legend.position = "bottom") +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())

## Proportion of points in each habitat class
# code from Fieberg et al. (2020)
brant %>%
  group_by(use, habitat) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n), 
         label = paste0(round(prop * 100, 1), "%")) %>% 
  ggplot(aes(habitat, prop, fill = use, group = use, label = label)) + 
  geom_col(position = position_dodge2(), alpha = 0.75) +
  geom_text(size = 4, vjust = -0.25, position = position_dodge(width = 1)) +
  labs(x = "", y = "Proportion", fill = "use")+
  scale_fill_manual(values = c("darkorange", "blue2"), labels = c("Available", "Used"), name = "") +
  scale_x_discrete(labels=c("Estuarine \n Wetland", "Estuarine \n Open Water", "Grass")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, face = "bold"), axis.text.y = element_text(face = "bold"), axis.title = element_text(size = 12), legend.text = element_text(face = "bold"), legend.position = "bottom") +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())


# Scale continuous covariates ----
## mean = 0, SD = 1
hist(brant$dvlp_dist)
brant$dvlp_dist_scale <- scale(brant$dvlp_dist)
hist(scale(brant$dvlp_dist_scale))


# Check for correlations ----
## (if there were more than one continuous covariate)
# cor(brant.continuous.covariates)


# Set reference level for categorical variable ----
## doesn't change anything in this case b/c estuarine emergent wetland was already reference (goes alphanumerically)
## important to think about reference levels in an HSA b/c estimated selection coefficient(s) will be relative to that reference level
levels(brant$habitat)
brant$habitat <- relevel(brant$habitat, ref = "EstEmergWtld")
levels(brant$habitat)


# Add column specifying weight ----
## used points weight = 1
## availabe weight = 1000
## "trick" to increase available:use ratio and improve estimation
## ATBR
brant$weight <- NA
brant$weight[brant$use == 1] <- 1  ## used points weight = 1
brant$weight[brant$use == 0] <- 1000  ## available points weight = 1000


# Fit selection function ----
## logistic regression
brant.mod1 <-glm(use ~ habitat + dvlp_dist_scale, family = binomial(link = "logit"), data = brant, weights = weight)

## output and interpretation
summary(brant.mod1)
  # the intercept in RSFs is meaningless and essentially reflects the ratio of available:used points (-10 = 10:1 ratio)
  # relative probability of selection is significantly different in different habitat type
  # relative probability of selection for estuarine habitat is positive RELATIVE to estuarine emergent wetlands
  # relative probability of selection for grass habitat is positive RELATIVE to estuarine emergent wetlands (but lesser than grass)
  # as distance to development increases, relative probability of selection decreases (i.e., relative probability of selection is greater near development)


# Plot means and 95% CIs ----
## Compile estimates (relative selection strengths) and SEs
brant.summary <- summary(brant.mod1)
brant.ests <- data.frame(brant.summary$coefficients[,1:2])
colnames(brant.ests) <- c("est", "se")

## Drop intercept from output table (meaningless)
brant.ests <- brant.ests[2:nrow(brant.ests),]

## Calculate lower and upper 95% CI
brant.ests$lowCI <- brant.ests$est - 1.96*brant.ests$se
brant.ests$highCI <- brant.ests$est + 1.96*brant.ests$se

## Add column for coefficient name
brant.ests$coeff <- factor(c("estuarine", "grass", "dvlp_dist"))

## Plot
ggplot(brant.ests, aes(x = coeff, y = est)) +
  geom_point(position = position_dodge2(width = 0.5, preserve = "single"), size=2) +
  geom_errorbar(aes(ymin = lowCI, ymax = highCI), position = position_dodge2(width = 0.4, preserve = "single"), width = 0.3, size=0.75) +
  geom_hline(yintercept = 0) +
  xlab(element_blank()) +
  ylab(element_text("Parameter Estimate and 95% CI (logit scale)")) +
  scale_x_discrete(breaks=unique(brant.ests$coeff), labels=c("Estuarine \n Water", "Grass", "Distance to \n Development")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, face = "bold"), axis.text.y = element_text(face = "bold"), axis.title.y = element_text(size = 12, face = "bold")) +
  theme(panel.grid.minor = element_blank()) +
  theme(legend.position = "none")


# Interpretation ----
## Categorical variables
## Relative use of one habitat type vs another
# Assuming two sites are equally available, how much more likely is a brant to use open estuarine water than estuarine wetland habitat (See Fieberg et al. (2022) Appendix A)
exp(brant.ests[1,1])
  # brant are 2.4x more likely to be in open estuarine water than estuarine emergent wetland habitat

# Assuming two sites are equally available, how much more likely is a brant to use grass than estuarine wetland habitat (See Fieberg et al. (2022) Appendix A)
exp(brant.ests[2,1])
  # brant are 1.3x more likely to be in grass than estuarine emergent wetland


## Continuous variables - relative selection strength
## (See Fieberg et al. (2022) Appendix A and Avgar et al. 2017)
# What is the relative selection strength for a location in estuarine open water that is 1m (scaled) from development versus a location in estuarine open water that is 2m (scaled) from development?

# Aka what is the relative selection strength (relative use) for a given location that differs from another location by 1 unit of distance to development, given that they are in the same habitat type, and assuming they are equally available?

## Assume the brant is in estuarine habitat

exp(coef(brant.mod1)["habitatEstuarine"] + 1 * coef(brant.mod1)["dvlp_dist_scale"]) /
  exp(coef(brant.mod1)["habitatEstuarine"] + 2 * coef(brant.mod1)["dvlp_dist_scale"])
  # brant in estuarine habitat is 2.2x more likely to use a location 1m closer to development




