
# Price of a property prediction ---------------------------------------------------------------------

setwd("C:/My PC Files/Data Science/R Language/Real_State Project")

hd_train <- read.csv("housing_train.csv", stringsAsFactors = FALSE)
hd_test <- read.csv("housing_test.csv", stringsAsFactors = FALSE)

library(dplyr)
library(tidyr)
glimpse(hd_train)
glimpse(hd_test)

hd_test$Price = NA

hd_train$data = 'train'
hd_test$data = 'test'

hd_all <- rbind(hd_train, hd_test)

glimpse(hd_all)



# Data Preparation -------------------------------------------------------------------------------

# CouncilArea Variable
table(hd_all$CouncilArea)
sort(table(hd_all$CouncilArea))
unique(hd_all$CouncilArea)

hd_all <- hd_all %>% 
  mutate(CA_below100 = as.numeric(CouncilArea %in% c("Hume","Kingston")),
         CA_100 = as.numeric(CouncilArea %in% c("Monash","Whitehorse","Manningham")),
         CA_200 = as.numeric(CouncilArea %in% c("Brimbank","Hobsons Bay")),
         CA_300 = as.numeric(CouncilArea %in% c("Bayside","Melbourne","Banyule")),
         CA_400 = as.numeric(CouncilArea %in% c("Port Phillip","Yarra","Maribyrnong")),
         CA_500 = as.numeric(CouncilArea == "Stonnington"),
         CA_600 = as.numeric(CouncilArea %in% c("Glen Eira","Darebin","Moonee Valley")),
         CA_700 = as.numeric(CouncilArea == "Moreland"),
         CA_800 = as.numeric(CouncilArea == "Boroondara")) %>% 
  select(-CouncilArea)

glimpse(hd_all)

# Drop Address Variable 
table(hd_all$Address) # Too many categories

hd_all = hd_all %>%
  select(-Address)

# Type Variable
table(hd_all$Type)
sort(table(hd_all$Type))
unique(hd_all$Type)

hd_all <- hd_all %>% 
  mutate(Type_u = as.numeric(Type == "u"),
         Type_h = as.numeric(Type == "h")) %>%
  select(-Type)

glimpse(hd_all)

# Method Variable
table(hd_all$Method)
sort(table(hd_all$Method))
unique(hd_all$Method)

hd_all <- hd_all %>% 
  mutate(Method_VB = as.numeric(Method == "VB"),
         Method_SP = as.numeric(Method == "SP"),
         Method_PI = as.numeric(Method == "PI"),
         Method_S = as.numeric(Method == "S")) %>%
  select(-Method)

glimpse(hd_all)

# Let's check number of unique values in each column
sort(sapply(hd_all, function(x) length(unique(x))))

# Name of all character columns
names(hd_all)[sapply(hd_all, function(x) is.character(x))]
# Name of all numerical columns
names(hd_all)[sapply(hd_all, function(x) is.numeric(x))]

# Postcode Variable
table(hd_all$Postcode)
sort(table(hd_all$Postcode))
unique(hd_all$Postcode) # 94 Unique Values

hd_all <- hd_all %>% 
  mutate(Postcode_100 = as.numeric(Postcode %in% c(3079,3108,3068,3039,3071,3147,3013,3103,3124, 
                                                   3145,3127,3081,3207,3044,3187,3122,3031,3104, 
                                                   3181,3015,3042,3070,3011,3188,3101,3186,3084,  
                                                   3146,3056,3141,3182,3012,3072,3204,3058)),
         Postcode_200 = as.numeric(Postcode %in% c(3163,3040,3032,3121,3165,3046,3020)),
         Postcode_300 = as.numeric(Postcode == 3073)) %>% 
  select(-Postcode)

glimpse(hd_all)

# Suburb Variable
table(hd_all$Suburb)
sort(table(hd_all$Suburb))
unique(hd_all$Suburb) # 142 Unique Values

# SellerG Variable
table(hd_all$SellerG)
sort(table(hd_all$SellerG))
unique(hd_all$SellerG) # 198 Unique Values

# To Create Dummy Variables For Suburb and SellerG

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

hd_all = CreateDummies(hd_all,"Suburb", 100)
hd_all = CreateDummies(hd_all,"SellerG", 200)

glimpse(hd_all)

# Let's check number of unique values in each column
sort(sapply(hd_all, function(x) length(unique(x))))

# Name of all character columns
names(hd_all)[sapply(hd_all, function(x) is.character(x))]
# Name of all numerical columns
names(hd_all)[sapply(hd_all, function(x) is.numeric(x))]

# Missing Value Treatment ---------------------------------------------------------------------------------------

# NA values in all the columns of hd_all
sum(is.na(hd_all))
sort(sapply(hd_all, function(x) sum(is.na(x))))

# Sum of missing values in Bedroom2 variable
sum(is.na(hd_all$Bedroom2))

# Imputing all missing values by mean function
for(col in names(hd_all)){
  
  if(sum(is.na(hd_all[,col])) > 0 & !(col %in% c("data","Price"))) {
    
    hd_all[is.na(hd_all[,col]),col] = mean(hd_all[hd_all$data =='train',col],na.rm = T)
  }
  
}

# Price have 1885 missing values that we have own create
sum(is.na(hd_all)) # 1885 - These are missing value of Price from Test Data

# Separate train and test
hd_train = hd_all %>% filter(data == 'train') %>% select(-data)
hd_test = hd_all %>% filter(data =='test') %>% select (-data,-Price)

# # Export Training and Test dataset for future use
write.csv(hd_train, "hd_train_clean.csv", row.names = F)
write.csv(hd_test, "hd_test_clean.csv", row.names = F)

# -------------------------------------------------------------------------------------------------------------------------------------

# Model Building on 80% of Training data ----------------------------------------------------------------------------------------------

# Import Training and Test dataset for Model Building
# hd_train = read.csv("hd_train_clean.csv") # Import Training data
# hd_test = read.csv("hd_test_clean.csv") # Import Test data

set.seed(2)
s = sample(1:nrow(hd_train),0.8*nrow(hd_train))
hd_train1 = hd_train[s,] # Training data
hd_train2 = hd_train[-s,] # Holdout/Validation data

# Remove Multicoliniarity on 80% of Training data -------------------------------------------------------------------------------------------

library(car)

# Fitting a linear model on the data using all variables
fit <- lm(Price ~., data = hd_train1)
summary(fit)
formula(fit)

# We'll take vif cutoff as 5
vif(fit)
sort(vif(fit), decreasing = T)[1:5] # all vif values less than 5

# Remove Method_S which have high Multicolinarity 
fit <- lm(Price ~.-Suburb_Reservoir -Method_S, data = hd_train1)
sort(vif(fit), decreasing = T)[1:5]

# Set Formula for Linear Model
# Now let's create linear regression model
fit <- lm(Price ~.-Suburb_Reservoir -Method_S, data = hd_train1)

fit$coefficients
summary(fit)$coefficients
summary(fit)$coefficients[, 4]
sort(summary(fit)$coefficients[, 4])
round(sort(summary(fit)$coefficients[, 4]), 2)
names(fit$coefficients) 

names(summary(fit))
summary(fit)$call
summary(fit)$r.squared
summary(fit)$adj.r.squared

# Step-wise regression for variable selection based on AIC score -------------------------------------
step_fit <- step(fit)

# Based on AIC score _ Lower the better
summary(step_fit) # Not all variables are significant
names(step_fit$coefficients) 
formula(step_fit)

fit_new = lm(formula = Price ~ Rooms + Distance + Bedroom2 + Bathroom + 
               Car + Landsize + BuildingArea + YearBuilt + CA_100 + CA_200 + 
               CA_300 + CA_400 + CA_500 + CA_700 + CA_800 + Type_u + Type_h + 
               Method_VB + Method_SP + Method_PI + Postcode_100 + Postcode_200 + 
               Postcode_300 + Suburb_Doncaster + Suburb_AscotVale + Suburb_Footscray + 
               Suburb_Hampton + Suburb_Yarraville + Suburb_Balwyn + Suburb_MalvernEast + 
               Suburb_Camberwell + Suburb_Carnegie + Suburb_Bentleigh + 
               Suburb_BrightonEast + Suburb_Hawthorn + Suburb_BalwynNorth + 
               Suburb_Kew + Suburb_Brighton + Suburb_GlenIris + Suburb_Essendon + 
               Suburb_StKilda + Suburb_Preston + Suburb_BentleighEast + 
               SellerG_Brad + SellerG_Biggin + SellerG_Buxton + SellerG_Marshall + 
               SellerG_Barry + SellerG_hockingstuart + SellerG_Jellis + 
               SellerG_Nelson, data = hd_train1)

summary(fit_new)
sort((summary(fit_new)$coefficients)[,4], decreasing = T)[1:5]

# Remove SellerG_Brad
fit_new = lm(formula = Price ~ Rooms + Distance + Bedroom2 + Bathroom + 
               Car + Landsize + BuildingArea + YearBuilt + CA_100 + CA_200 + 
               CA_300 + CA_400 + CA_500 + CA_700 + CA_800 + Type_u + Type_h + 
               Method_VB + Method_SP + Method_PI + Postcode_100 + Postcode_200 + 
               Postcode_300 + Suburb_Doncaster + Suburb_AscotVale + Suburb_Footscray + 
               Suburb_Hampton + Suburb_Yarraville + Suburb_Balwyn + Suburb_MalvernEast + 
               Suburb_Camberwell + Suburb_Carnegie + Suburb_Bentleigh + 
               Suburb_BrightonEast + Suburb_Hawthorn + Suburb_BalwynNorth + 
               Suburb_Kew + Suburb_Brighton + Suburb_GlenIris + Suburb_Essendon + 
               Suburb_StKilda + Suburb_Preston + Suburb_BentleighEast +
               SellerG_Biggin + SellerG_Buxton + SellerG_Marshall + 
               SellerG_Barry + SellerG_hockingstuart + SellerG_Jellis + 
               SellerG_Nelson, data = hd_train1)

summary(fit_new)
round(sort(summary(fit_new)$coefficients[, 4]), 2)
sort((summary(fit_new)$coefficients)[,4], decreasing = T)[1:5]

# Remove SellerG_hockingstuart
fit_new = lm(formula = Price ~ Rooms + Distance + Bedroom2 + Bathroom + 
               Car + Landsize + BuildingArea + YearBuilt + CA_100 + CA_200 + 
               CA_300 + CA_400 + CA_500 + CA_700 + CA_800 + Type_u + Type_h + 
               Method_VB + Method_SP + Method_PI + Postcode_100 + Postcode_200 + 
               Postcode_300 + Suburb_Doncaster + Suburb_AscotVale + Suburb_Footscray + 
               Suburb_Hampton + Suburb_Yarraville + Suburb_Balwyn + Suburb_MalvernEast + 
               Suburb_Camberwell + Suburb_Carnegie + Suburb_Bentleigh + 
               Suburb_BrightonEast + Suburb_Hawthorn + Suburb_BalwynNorth + 
               Suburb_Kew + Suburb_Brighton + Suburb_GlenIris + Suburb_Essendon + 
               Suburb_StKilda + Suburb_Preston + Suburb_BentleighEast +
               SellerG_Biggin + SellerG_Buxton + SellerG_Marshall + 
               SellerG_Barry + SellerG_Jellis + 
               SellerG_Nelson, data = hd_train1)

summary(fit_new)
round(sort(summary(fit_new)$coefficients[, 4]), 2)
sort((summary(fit_new)$coefficients)[,4], decreasing = T)[1:5]
formula(fit_new)

# Making Prediction and Measuring Errors ---------------------------------------------------------

# To make predictions on validation set using this model (Error Checking)
val.pred <- predict(fit, newdata = hd_train2)
val.pred[1:5]
hd_train2$Price[1:5]

val.errors <- hd_train2$Price - val.pred
val.errors[1:5]

# Root mean square error (RMSE)
val.errors**2 %>% mean() %>% sqrt()
# Validation error  407087

# Training Error
train.pred <- predict(fit, newdata = hd_train1)
train.pred[1:5]
hd_train1$Price[1:5]

train_error <- hd_train1$Price - train.pred
train_error[1:5]

# Root mean square error (RMSE Value)
train_error**2 %>% mean() %>% sqrt() # 394569.6

# Training set error 394569.6 less than validation set error 407087 as expected

# ---------------------------------------------------------------------------------------------------------

# Final Model Building For Entire Training Data ------------------------

fit.final = lm(Price ~ . -Suburb_Reservoir, data = hd_train)
summary(fit.final)
formula(fit.final)

# Multicolinarity Check For Entire Training Data

# We'll take vif cutoff as 5
vif(fit.final)
sort(vif(fit.final), decreasing = T)[1:5] # all vif values less than 5

# Remove Method_S which have high Multicolinarity
fit.final = lm(Price ~ .-Suburb_Reservoir -Method_S, data = hd_train)
sort(vif(fit.final), decreasing = T)[1:5]

# Set Formula for Linear Model
# Now let's create linear regression model
fit.final = lm(Price ~ .-Suburb_Reservoir -Method_S, data = hd_train)

# Step-wise regression for variable selection based on AIC score
fit.final = step(fit.final)

# Based on AIC score _ Lower the better
summary(fit.final) # Not all variables are significant
names(fit.final$coefficients) 
formula(fit.final)

# Based on summary of fit.final remove variables step by step which have highest p-values
fit.final = lm(formula = Price ~ Rooms + Distance + Bedroom2 + Bathroom + 
                 Car + Landsize + BuildingArea + YearBuilt + CA_100 + CA_200 + 
                 CA_300 + CA_400 + CA_500 + CA_700 + CA_800 + Type_u + Type_h + 
                 Method_VB + Method_SP + Method_PI + Postcode_100 + Postcode_200 + 
                 Postcode_300 + Suburb_Doncaster + Suburb_AscotVale + Suburb_Footscray + 
                 Suburb_Thornbury + Suburb_Hampton + Suburb_Yarraville + Suburb_Balwyn + 
                 Suburb_MalvernEast + Suburb_Camberwell + Suburb_Carnegie + 
                 Suburb_PortMelbourne + Suburb_Bentleigh + Suburb_BrightonEast + 
                 Suburb_Hawthorn + Suburb_BalwynNorth + Suburb_Northcote + 
                 Suburb_Kew + Suburb_Brighton + Suburb_GlenIris + Suburb_Essendon + 
                 Suburb_StKilda + Suburb_Preston + Suburb_BentleighEast + 
                 SellerG_Brad + SellerG_Biggin + SellerG_Ray + SellerG_Buxton + 
                 SellerG_Marshall + SellerG_Barry + SellerG_hockingstuart + 
                 SellerG_Jellis + SellerG_Nelson, data = hd_train)

summary(fit.final)
sort((summary(fit.final)$coefficients)[,4], decreasing = T)[1:5]

# Remove Suburb_Thornbury
fit.final = lm(formula = Price ~ Rooms + Distance + Bedroom2 + Bathroom + 
                 Car + Landsize + BuildingArea + YearBuilt + CA_100 + CA_200 + 
                 CA_300 + CA_400 + CA_500 + CA_700 + CA_800 + Type_u + Type_h + 
                 Method_VB + Method_SP + Method_PI + Postcode_100 + Postcode_200 + 
                 Postcode_300 + Suburb_Doncaster + Suburb_AscotVale + Suburb_Footscray +
                 Suburb_Hampton + Suburb_Yarraville + Suburb_Balwyn + 
                 Suburb_MalvernEast + Suburb_Camberwell + Suburb_Carnegie + 
                 Suburb_PortMelbourne + Suburb_Bentleigh + Suburb_BrightonEast + 
                 Suburb_Hawthorn + Suburb_BalwynNorth + Suburb_Northcote + 
                 Suburb_Kew + Suburb_Brighton + Suburb_GlenIris + Suburb_Essendon + 
                 Suburb_StKilda + Suburb_Preston + Suburb_BentleighEast + 
                 SellerG_Brad + SellerG_Biggin + SellerG_Ray + SellerG_Buxton + 
                 SellerG_Marshall + SellerG_Barry + SellerG_hockingstuart + 
                 SellerG_Jellis + SellerG_Nelson, data = hd_train)

summary(fit.final)
sort((summary(fit.final)$coefficients)[,4], decreasing = T)[1:5]

# Remove Suburb_Northcote
fit.final = lm(formula = Price ~ Rooms + Distance + Bedroom2 + Bathroom + 
                 Car + Landsize + BuildingArea + YearBuilt + CA_100 + CA_200 + 
                 CA_300 + CA_400 + CA_500 + CA_700 + CA_800 + Type_u + Type_h + 
                 Method_VB + Method_SP + Method_PI + Postcode_100 + Postcode_200 + 
                 Postcode_300 + Suburb_Doncaster + Suburb_AscotVale + Suburb_Footscray +
                 Suburb_Hampton + Suburb_Yarraville + Suburb_Balwyn + 
                 Suburb_MalvernEast + Suburb_Camberwell + Suburb_Carnegie + 
                 Suburb_PortMelbourne + Suburb_Bentleigh + Suburb_BrightonEast + 
                 Suburb_Hawthorn + Suburb_BalwynNorth + 
                 Suburb_Kew + Suburb_Brighton + Suburb_GlenIris + Suburb_Essendon + 
                 Suburb_StKilda + Suburb_Preston + Suburb_BentleighEast + 
                 SellerG_Brad + SellerG_Biggin + SellerG_Ray + SellerG_Buxton + 
                 SellerG_Marshall + SellerG_Barry + SellerG_hockingstuart + 
                 SellerG_Jellis + SellerG_Nelson, data = hd_train)

summary(fit.final)
sort((summary(fit.final)$coefficients)[,4], decreasing = T)[1:5]

# Remove SellerG_Ray
fit.final = lm(formula = Price ~ Rooms + Distance + Bedroom2 + Bathroom + 
                 Car + Landsize + BuildingArea + YearBuilt + CA_100 + CA_200 + 
                 CA_300 + CA_400 + CA_500 + CA_700 + CA_800 + Type_u + Type_h + 
                 Method_VB + Method_SP + Method_PI + Postcode_100 + Postcode_200 + 
                 Postcode_300 + Suburb_Doncaster + Suburb_AscotVale + Suburb_Footscray +
                 Suburb_Hampton + Suburb_Yarraville + Suburb_Balwyn + 
                 Suburb_MalvernEast + Suburb_Camberwell + Suburb_Carnegie + 
                 Suburb_PortMelbourne + Suburb_Bentleigh + Suburb_BrightonEast + 
                 Suburb_Hawthorn + Suburb_BalwynNorth + 
                 Suburb_Kew + Suburb_Brighton + Suburb_GlenIris + Suburb_Essendon + 
                 Suburb_StKilda + Suburb_Preston + Suburb_BentleighEast + 
                 SellerG_Brad + SellerG_Biggin + SellerG_Buxton + 
                 SellerG_Marshall + SellerG_Barry + SellerG_hockingstuart + 
                 SellerG_Jellis + SellerG_Nelson, data = hd_train)

summary(fit.final)
sort((summary(fit.final)$coefficients)[,4], decreasing = T)[1:5]
round(sort(summary(fit.final)$coefficients[, 4]), 2)

fit.final = fit = lm(formula = Price ~ Rooms + Distance + Bedroom2 + Bathroom + 
                       Car + Landsize + BuildingArea + YearBuilt + CA_100 + CA_200 + 
                       CA_300 + CA_400 + CA_500 + CA_700 + CA_800 + Type_u + Type_h + 
                       Method_VB + Method_SP + Method_PI + Postcode_100 + Postcode_200 + 
                       Postcode_300 + Suburb_Doncaster + Suburb_AscotVale + Suburb_Footscray +
                       Suburb_Hampton + Suburb_Yarraville + Suburb_Balwyn + 
                       Suburb_MalvernEast + Suburb_Camberwell + Suburb_Carnegie + 
                       Suburb_PortMelbourne + Suburb_Bentleigh + Suburb_BrightonEast + 
                       Suburb_Hawthorn + Suburb_BalwynNorth + 
                       Suburb_Kew + Suburb_Brighton + Suburb_GlenIris + Suburb_Essendon + 
                       Suburb_StKilda + Suburb_Preston + Suburb_BentleighEast + 
                       SellerG_Brad + SellerG_Biggin + SellerG_Buxton + 
                       SellerG_Marshall + SellerG_Barry + SellerG_hockingstuart + 
                       SellerG_Jellis + SellerG_Nelson, data = hd_train)

summary(fit.final)
summary(fit)

# Prediction on Test Data Set
test.pred <- predict(fit.final, newdata = hd_test)
test.pred[1:5] # Probability Score of Final Training Data

# Export Prediction for submission
write.csv(test.pred, "submisionLR.csv", row.names = F)