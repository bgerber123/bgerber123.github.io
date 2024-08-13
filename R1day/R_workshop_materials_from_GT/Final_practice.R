### Introduction to R Workshop ###
### August 15th, 2024 ###
### Gerber, Horton, Titcomb ###


## Putting it all together!

# > Objectives:
# 1. Begin to evaluate datasets from a coding perspective - how should data be arranged to communicate relationships?
# 2. Envision intended plotting outcomes and use existing tools to achieve desired plots.
# 3. Compare and evaluate different plots for their effectiveness in communicating data relationships.


### For the last hour of our workshop, we will have the chance to work with real data and to showcase our plotting skills!
### For the next 40 minutes, you and a partner will take the dataset and create:

#### 1. A plot that elegantly displays some chosen relationship in the dataset
#### 2. The plot above, but in the most outrageous color scheme and theme that you can create
#### 3. A multi-panel plot that shows one panel with data that are obscured by a hidden variable, and a second panel showing the effect of the hidden variable.

### Creating these plots may require you to subset, aggregate and summarize, or group the data (especially for task #3). 
### Don't forget your troubleshooting tools if you are struggling!

## You will then paste your plots onto a shared google doc that we will use to showcase and vote on the 'best' plots for each category!



### Introduction to the dataset:

### We will use a camera trapping dataset provided by the Snapshot Safari project, which provides counts of various species spotted 
### in Serengeti National Park (Tanzania). The dataset also has metadata on the camera traps, including trapping effort and environmental covariates.
### These data were collected as part of a large camera trapping project called ‘Snapshot Serengeti’, which you
### can check out on Zooniverse: https://www.zooniverse.org/projects/zooniverse/snapshot-serengeti.

animals = read.csv("sample_camera_data.csv")
camera = read.csv("sample_camera_effort.csv")
covars = read.csv("sample_camera_covariates.csv")

### Counts of animals by site and season, with data on time/date
head(animals)
# subject_id: ID of a photo or short photo burst
# season: one of 8 different seasons lasting ~3 months (S01:S08)
# site: one of 223 different sites where a camera was placed
# roll: within a season and site, an interval of continuous photos
# capture: an ID given to a photo or burst (within a roll)
# datetime: the time and date of the capture
# species: the species seen in the image
# count: the number of individuals seen in the image

### information on when cameras were running
### to make things simple, let's summarize effort as total trap nights
head(camera)

effort = camera %>%
  group_by(site)%>%
  summarize(n = n())

### we also have covariate data
head(covars)

### we can join effort to site covariates:
effort2 = effort %>%
  left_join(covars)

### and now we can append this to our main dataframe (in case you'd like to include any of this information in your plot):
animals = animals %>%
  left_join(effort2)




### Task 1: Using the space below and the 'animals' data frame, create a beautiful plot worthy of publication!










### Task 2: Take the plot that you made above and edit the graphical elements such that it is now outrageous!










### Task 3: Create a multi-panel plot that shows one panel with data that are obscured by a hidden variable/aspect of the data, and a second panel showing the effect of the hidden variable.

## an example:
## zebra activity with and without correcting for site effort:
p1 = animals %>%
  filter(species == "zebra") %>%
  group_by(site, habitat) %>%
  summarize(total_count = sum(count)) %>%
  ggplot(aes(x = habitat, y = total_count))+
  geom_col()

p2 = animals %>%
  filter(species == "zebra") %>%
  group_by(site, habitat) %>%
  summarize(total_count = sum(count),
            total_effort = sum(n)) %>%
  mutate(count_p_tn = total_count / total_effort) %>%
  ggplot(aes(x=habitat, y=count_p_tn))+
  geom_boxplot()+
  scale_y_log10()

gridExtra::grid.arrange(p1, p2)

