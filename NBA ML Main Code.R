library(dplyr)
library(readr)

Combine <- read_csv("~/Blog/NBA Combine ML/Draft_Combine.csv") %>%
  filter(YEAR != 2022) %>%
  mutate(PLAYER = sub('(.+), (.+)','\\2 \\1',PLAYER)) %>%
  rename(Player = PLAYER, CombineYear = YEAR)

Draft <- read_csv("~/Blog/NBA Combine ML/draft-data-20-years.csv") %>%
  filter(DraftYear >= 2014,DraftYear<=2022) %>%
  select(Player,DraftYear) 

Merged <- full_join(Draft,Combine,by='Player') %>%
  mutate(Combine = ifelse(is.na(CombineYear),0,1),
         Drafted = ifelse(is.na(DraftYear),0,1)) %>%
  relocate(Combine,CombineYear,Drafted,DraftYear, .after = Player)

Processed_Set <- Merged %>%
  filter(Combine == 1,
         CombineYear %in% c(2014,2015,2016,2017,2018,2019,2020,2021)) %>%
  mutate(Drafted = as.factor(Drafted),
         CombineYear = as.factor(CombineYear),
         POS = as.factor(POS)) %>%
  mutate(PF = as.integer(grepl('PF',POS)),
         PG = as.integer(grepl('PG',POS)),
         SF = as.integer(grepl('SF',POS)),
         SG = as.integer(grepl('SG',POS)),
         C = as.integer(grepl('C',POS))) %>%
  mutate(PF = as.factor(PF),
         PG = as.factor(PG),
         SF = as.factor(SF),
         SG = as.factor(SG),
         C = as.factor(C)) %>%
  filter(is.na(SHUTTLE_RUN) == FALSE, is.na(THREE_QUARTER_SPRINT) == FALSE) %>%
  select(-c(Combine,DraftYear,BENCH_PRESS,Player,CombineYear,BAR,PAN,PBH,PDT,POS))

View(Processed_Set)

# Feature Engineering
Processed_Set <- Processed_Set %>% 
  mutate(ATHLETICISM = VERTICAL_LEAP/LANE_AGILITY,
         HAND_SIZE = HAND_WIDTH*HAND_LENGTH,
         MAX_HEIGHT = VERTICAL_LEAP+STANDING_REACH,
         EXPLOSIVENESS = VERTICAL_LEAP*WEIGHT
         )


summary(Processed_Set)

# Preprocessing

library(caret)
library(xgboost)

featurePlot(x = Processed_Set$STANDING_VERTICAL,
            y = Processed_Set$Drafted,
            plot = "density")

set.seed(4767)
processor <- preProcess(Processed_Set, method = c('center','scale'))
Model_Set <- predict(processor, newdata = Processed_Set)
View(Model_Set)

# Partition in Training and Test
trainIndex <- createDataPartition(Model_Set$Drafted, p = 0.7, list = FALSE)
trainingSet <- Model_Set[trainIndex,]
testSet <- Model_Set[-trainIndex,]


# Gradient Boosted Model

library(caret)
library(xgboost)

trainControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3)

boost_grid <- expand.grid(eta = .3,
                         nrounds = 100,
                         max_depth = 3,
                         min_child_weight = 1,
                         colsample_bytree = 1,
                         gamma = 3,
                         subsample = 1)


boosted_Model <- train(Drafted ~., 
                       data = trainingSet, 
                       method = "xgbTree",
                       trControl = trainControl,
                       tuneGrid = boost_grid)

boosted_Model

summary(boosted_Model)

varimp_RF <- varImp(boosted_Model)
plot(varimp_RF, main = "Variable Importance")

boost_prediction <- predict(boosted_Model, testSet)

boost_Confusion <- confusionMatrix(boost_prediction, testSet$Drafted, positive = '1')
boost_Confusion

# Digging Deeper

  # Significance Tests

Processed_Set %>%
  t.test(HAND_SIZE~Drafted, data = .)

Processed_Set %>%
  t.test(STANDING_VERTICAL~Drafted, data = .)

Processed_Set %>%
  t.test(HAND_WIDTH~Drafted, data = .)

Processed_Set %>%
  t.test(MAX_HEIGHT~Drafted, data = .)

Processed_Set %>%
  t.test(BODY_FAT~Drafted, data = .)


  # Plots

library(ggplot2)
library(ggthemes)
library(cowplot)

Hand_Size_Plot <- Processed_Set %>%
  ggplot() +
  aes(x= HAND_SIZE, color = Drafted) +
  geom_density()+
  scale_y_continuous(labels = scales::percent)+
  theme_fivethirtyeight() +
  labs(title="NBA Combine Hand Size",
       subtitle = "Drafted vs Undrafted",
       x= 'Hand Size (Square Inches)',
       y= 'Relative Frequency') +
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5))

Standing_Vertical_Plot <- Processed_Set %>%
  ggplot() +
  aes(x= STANDING_VERTICAL, color = Drafted) +
  geom_density()+
  scale_y_continuous(labels = scales::percent)+
  theme_fivethirtyeight() +
  labs(title="NBA Combine Standing Vertical",
       subtitle = "Drafted vs Undrafted",
       x= 'Standing Vertical (Inches)',
       y= 'Relative Frequency') +
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5))

Hand_Width_Plot <- Processed_Set %>%
  ggplot() +
  aes(x= HAND_WIDTH, color = Drafted) +
  geom_density()+
  scale_y_continuous(labels = scales::percent)+
  theme_fivethirtyeight() +
  labs(title="NBA Combine Hand Width",
       subtitle = "Drafted vs Undrafted",
       x= 'Hand Width (Inches)',
       y= 'Relative Frequency') +
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5))

Max_Height_Plot <- Processed_Set %>%
  ggplot() +
  aes(x= MAX_HEIGHT, color = Drafted) +
  geom_density()+
  scale_y_continuous(labels = scales::percent)+
  theme_fivethirtyeight() +
  labs(title="NBA Combine Standing Reach + Vertical Leap",
       subtitle = "Drafted vs Undrafted",
       x= 'Max Height (Inches)',
       y= 'Relative Frequency') +
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5))


Hand_Size_Plot

  # Plot Grid Sample Code
all_plots <- plot_grid(plot_grid(Hand_Size_Plot, Standing_Vertical_Plot),
                       plot_grid(Hand_Width_Plot,Max_Height_Plot),
                       nrow = 2)
all_plots

# Logistic Regression

logistic_model <- glm(Drafted ~ .,
                      data = trainingSet,
                      family = "binomial")
logistic_model

logistic_prediction <- predict(logistic_model, testSet)
logistic_prediction <- ifelse(logistic_prediction >0.5, 1, 0)

summary(logistic_model)

logistic_Confusion <- confusionMatrix(as.factor(logistic_prediction), as.factor(testSet$Drafted), positive = '1')
logistic_Confusion

dim(testSet)
