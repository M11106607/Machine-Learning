Due to the intricate structure of an electric traction drive, direct measurement
with thermal sensors is not possible for rotor temperatures, and even in case of
the stator temperatures, sensor outage or even just deterioration can't be ad-
ministered properly without redundant modeling. In addition, precise thermal
modeling gets more and more important with the rising relevance of functional
safety.
I have design a model with appropriate feature engineering, that esti-
mates one target temperature rotor temperature ("pm") in a causal manner.
pmsm_temperature_data_A1_2021

Data Set: The data set contains 15,147 instances, each of which have 13 columns: the
rst 8 columns corresponding to the attributes (e.g., ambient, coolant, u d, u q,
motor speed, torque, i_d, i_q) and the 9th column "pm" containing the rotor
temperature, i.e., the variable that we will predict. The details of the
data set can be found in the original Kaggle competition (https://www.kaggle.com/wkirgsn/electric-motor-temperature).

Install following libraries:
- dplyr
- glmnet
- randomForest
- psych
- ggplot2
- reshape2
- gridExtra
- car
- e1071

