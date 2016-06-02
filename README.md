# Kaggle-Titanic-Machine-Learning-from-Disaster

1. Data is loaded
2. Data exploration & pre-processing
  
  a. check the proportions of survivors based on port of entry

  b. check the proportion of survival in different classes
  
  c. check the proportion of survival in diffent gender
  
  d. survials in different age groups: looks like a normal distribution, lda maybe?
  
  e. fare distributions in diffrent classes, its weird how a few people have abnormal high fare price while a few have zero fare price. initial guess was maybe a family member purchased tickets for everyone, but there seems to be no last name resemblance between high price ticket buyer and the zero price passangers just to account for the outliers in the fare prices i replaced the high fares in each class with the average fare price in the respective class in both train and test dataset
  
  f. impute missing ages using the titles in the names predictor 
  for female- miss., mrs.
  for male -  mr., rev., master, Dr.
  imputing missing age values with average age for each title from both the train and the test set
  first lets seperate the data based on titles, the titles were obtained from eye balling the data
  
  g.checking the embarked predictor shows that two of the observations have no data, lets discard these observations
  
  h. i ran through a combination of interaction terms in variables, and kept editing and rediting the same interaction terms code and the variable below gave a good improvement in predition since we know that women have a high proportion of survival and occupants of first class have high proportions of survival lets combine these two variables into a varibale called papi
  
  i. lets create a variable called tic which has the alphabet initial in the cabin variable

3. MODEL FITTING AND SELECTION
 
  a.logistic regression
    lets use validation set approach to select the best threshold for logistic regg. this logistic regg tells us that Pclass, sex, age, sibsp, and (sex & parch), papa play an important role in classification.
    the best prediction accuracy of logistic regression is 81.5 percent on the validation set using Pclass+Parch*Sex+Age+SibSp+tic+papa

    b. Linear Discriminant Analysis
    lda gives a 80.5 percent predicition accuracy using Pclass+Sex*Parch+Age+SibSp+Fare+papa+tic+Embarked, the idea behind choosing lda was that the age variable seems to have a normal distribution,but oh well it dint seem to do any better
    
    c. random forest
    we get an 84.5% accuracy building a regression tree on Pclass+ Sex+ Age+ SibSp+Parch+Fare+Embarked+papa+tic with 3 predictors at every step
    
    d. svm
    prediction accuracy in 81% at best with polynomial kernel in svms using Pclass+ Sex+ Age+ SibSp+Parch+Fare+Embarked on the validation set
    
4. Prediction
  
  the random forest showed the best prediction accuracy so lets use that formthe final predition, using Pclass+Sex+Age+SibSp+Parch+Fare+papa+Embarked, with mtry=3
    
