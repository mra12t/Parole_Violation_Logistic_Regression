Predicting Parole Violation
================
2022-12-18

### About the Study and the Data Set

In the criminal justice system, parole is a system in which inmates who
are not considered a threat to society are released from prison before
their sentence is completed. They are still considered to be serving
their sentence while on parole and can be returned to prison if they
break the rules of their parole. Parole boards are responsible for
determining which inmates are suitable candidates for parole and aim to
release those who are unlikely to commit more crimes after being
released. In this problem, we will create and validate a model that
predicts whether an inmate will violate the terms of their parole. Such
a model could be useful to parole boards when deciding whether to grant
or deny parole. We will use data from the 2004 United States National
Corrections Reporting Program, a nationwide census of parole releases in
2004, to train and test our model. The data includes information about
parolees who either completed their parole successfully or violated the
terms of their parole in 2004. The dataset includes variables such as
the parolee’s gender, race, age, state, time served in prison, maximum
sentence length, and main crime leading to incarceration. The goal of
the model is to predict whether a parolee will violate the terms of
their parole based on these variables.

### EDA

``` r
parole = read.csv("parole.csv")
nrow(parole)
```

    ## [1] 675

``` r
library(knitr)
kable(table(parole$violator))
```

| Var1 | Freq |
|:-----|-----:|
| 0    |  597 |
| 1    |   78 |

``` r
str(parole)
```

    ## 'data.frame':    675 obs. of  9 variables:
    ##  $ male             : int  1 0 1 1 1 1 1 0 0 1 ...
    ##  $ race             : int  1 1 2 1 2 2 1 1 1 2 ...
    ##  $ age              : num  33.2 39.7 29.5 22.4 21.6 46.7 31 24.6 32.6 29.1 ...
    ##  $ state            : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ time.served      : num  5.5 5.4 5.6 5.7 5.4 6 6 4.8 4.5 4.7 ...
    ##  $ max.sentence     : int  18 12 12 18 12 18 18 12 13 12 ...
    ##  $ multiple.offenses: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ crime            : int  4 3 3 1 1 4 3 1 3 2 ...
    ##  $ violator         : int  0 0 0 0 0 0 0 0 0 0 ...

we have two variables that are unordered factors with more than 3 levels
in the data set, these being: crime and state. From the description of
the data set:

- “Crime” is a variable in the dataset that represents the main crime
  that led to the parolee’s incarceration. The variable is coded, with a
  value of 2 indicating that the crime was larceny, a value of 3
  indicating that the crime was drug-related, a value of 4 indicating
  that the crime was driving-related, and a value of 1 indicating that
  the crime was any other type of crime.

- “State” is a variable in the dataset that represents the state where
  the parolee was released. The variable is coded, with a value of 2
  indicating that the state is Kentucky, a value of 3 indicating that
  the state is Louisiana, a value of 4 indicating that the state is
  Virginia, and a value of 1 indicating that the state is any other
  state. The three states of Kentucky, Louisiana, and Virginia were
  chosen for inclusion in the dataset because they had a high number of
  parolees represented in the data.

``` r
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)
# lets check the result
summary(parole$crime)
```

    ##   1   2   3   4 
    ## 315 106 153 101

``` r
summary(parole$state)
```

    ##   1   2   3   4 
    ## 143 120  82 330

``` r
library(caTools)
#choosing random seeds
set.seed(123)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)
```

### Logistic Regression Model

``` r
model = glm(violator ~ . , family = binomial, data = train)
summary(model)
```

    ## 
    ## Call:
    ## glm(formula = violator ~ ., family = binomial, data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.5774  -0.4056  -0.2914  -0.1857   2.7506  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)       -4.644032   1.318639  -3.522 0.000429 ***
    ## male               0.535110   0.480299   1.114 0.265228    
    ## race               0.833942   0.397351   2.099 0.035839 *  
    ## age                0.006136   0.015915   0.386 0.699833    
    ## state2             0.052284   0.563240   0.093 0.926041    
    ## state3             0.609359   0.533849   1.141 0.253685    
    ## state4            -3.241737   0.595107  -5.447 5.11e-08 ***
    ## time.served       -0.070543   0.119984  -0.588 0.556574    
    ## max.sentence       0.064190   0.051494   1.247 0.212558    
    ## multiple.offenses  1.756543   0.422035   4.162 3.15e-05 ***
    ## crime2             0.763284   0.468803   1.628 0.103492    
    ## crime3            -0.188224   0.444510  -0.423 0.671973    
    ## crime4            -0.013951   0.546209  -0.026 0.979624    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 340.04  on 472  degrees of freedom
    ## Residual deviance: 250.66  on 460  degrees of freedom
    ## AIC: 276.66
    ## 
    ## Number of Fisher Scoring iterations: 6

As we can see from the summary of the model above, there are three main
significant variables that affect whether or not a parolee will violate
their parole. These are state4, multiple.offence, and race.  
Furthermore, if we have to parolee A and B that have all identical
variables but A has higher multiple offences than B. Then, A is 4 times
more likely to violate their parole since the factor of multiple
offences variable is 1.403, the odds must be calculated as
$exp(1.403)=4.06$.

### Testing the Model on the Testing Set

``` r
predection = predict(model, newdata = test, type = "response")
summary(predection)
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## 0.001464 0.020751 0.049480 0.114416 0.106383 0.803560

``` r
x = table(test$violator, as.numeric(predection >= 0.5))
kable(x)
```

|     |   0 |   1 |
|:----|----:|----:|
| 0   | 176 |   3 |
| 1   |  15 |   8 |

``` r
# Calculating the Sensitivity of the Model
x[4]/(x[4]+x[2])
```

    ## [1] 0.3478261

``` r
#Calculating the Specificity of the Model
x[1]/(x[3]+x[1])
```

    ## [1] 0.9832402

``` r
#Calculating the Accuracy of the Model 
sum(diag(x))/sum(x)
```

    ## [1] 0.9108911

Now, let’s compare it to the accuracy of a base line model.

``` r
kable(table(test$violator))
```

| Var1 | Freq |
|:-----|-----:|
| 0    |  179 |
| 1    |   23 |

A base line model on the test set will predict a parolee will not
violate their parole all the time. Let’s find the accuracy of such
model.

``` r
table(test$violator)[1]/nrow(test)
```

    ##         0 
    ## 0.8861386

***Important Observations***

- ***The parole board would be more likely to regret releasing a
  prisoner who goes on to violate parole (a false negative prediction)
  than denying parole to a prisoner who would not have violated parole
  (a false positive prediction). Using the model for parole decisions, a
  negative prediction would result in parole being granted, while a
  positive prediction would lead to parole being denied. To minimize the
  number of false negatives, the parole board should lower the cutoff
  for positive predictions, which would increase false positives but
  decrease false negatives. On the other hand, raising the cutoff for
  positive predictions would increase false negatives and decrease false
  positives. Since the parole board places a high value on avoiding
  false negatives, they should choose to lower the cutoff(\<0.5).***

- ***The model with a threshold of 0.5 has 3 false positives and 15
  false negatives, while the baseline model has 0 false positives and 23
  false negatives. Given that false negatives are more costly for the
  board, the model with a threshold of 0.5 appears to be more useful.***

``` r
#Calculating AUC
library(ROCR)
pred = prediction(predection, test$violator)
as.numeric(performance(pred, "auc")@y.values)
```

    ## [1] 0.8651931

### Notes on AUC:

The AUC, or area under the curve, is a metric used to evaluate the
performance of a binary classification model. It measures the model’s
ability to distinguish between positive and negative examples.

To calculate the AUC, a curve is plotted with the true positive rate on
the y-axis and the false positive rate on the x-axis. The true positive
rate is the proportion of positive examples that are correctly
classified as positive, while the false positive rate is the proportion
of negative examples that are incorrectly classified as positive.

The AUC is calculated by finding the area under this curve. A model with
a high AUC is able to correctly classify a larger proportion of positive
and negative examples, while a model with a low AUC has difficulty
distinguishing between the two classes.

One important aspect of the AUC is that it is independent of the cutoff
used to classify an example as positive or negative. This means that the
AUC is not affected by the specific threshold used to make a prediction.
This makes it a useful metric for comparing models, since it is not
influenced by the decision boundary chosen by the user.
