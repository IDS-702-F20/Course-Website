
###########################################################################
###########################################################################
################## Multiple Regression of Diamonds Data ###################
###########################################################################
###########################################################################


###### Clear environment and load libraries
rm(list = ls())
library(knitr)
library(ggplot2)
library(kableExtra)
library(lattice)
#library(dplyr)
library(rms)




###### Data
diamonds <- read.csv("data/diamonds.csv", header= T,
                     colClasses = c("numeric","factor","factor","factor","numeric"))
dim(diamonds)
head(diamonds)
summary(diamonds)




###### EDA
#Is the distribution of the response variable normal?
hist(diamonds$Price,xlab="Price",main="Distribution of price",col=rainbow(10))

#Next, explore the relationship between `price` and `carats`
plot(diamonds$Price~diamonds$Carats,xlab="Carats",ylab="Price",col='cyan4')
abline(lm(Price~Carats,data=diamonds),col='red3',lty=10,lwd=2)
#What do you think of this plot?

# Since the distribution of `carats` is not really normal, let's transform the variable. 
#This is the natural log, you can try other bases on your own.
hist(log(diamonds$Price),xlab="Log Price",main="Distribution of log price",col=rainbow(10))
#Is this distribution more normal?
  
#Now the relationship between log(price) and carats
plot(log(diamonds$Price)~diamonds$Carats,xlab="Carats",ylab="Log Price",col='cyan4')
abline(lm(log(Price)~Carats,data=diamonds),col='red3',lty=10,lwd=2)
#Is this more "linear" than what we had before?
  
#Next, `price` and `color`.
boxplot(Price~Color,data=diamonds,ylab="Price",xlab="Color",col=rainbow(15))
#What do you think of this plot?
  
  
  ---
  ## Multiple regression of diamonds data
  
  How about, `log(price)` and `color` instead

boxplot(log(Price)~Color,data=diamonds,ylab="Log Price",xlab="Color",col=rainbow(15))
#What do you think of this plot?
  
  
  ---
  ## Multiple regression of diamonds data
  
  Next, `price` and `clarity`.
```{r fig.height=3.4}
boxplot(Price~Clarity,data=diamonds,ylab="Price",xlab="Clarity",col=rainbow(15))
#What do you think of this plot?
  
  
  ---
  ## Multiple regression of diamonds data
  
  How about, `log(price)` and `clarity` instead
```{r fig.height=3.4}
boxplot(log(Price)~Clarity,data=diamonds,ylab="Log Price",xlab="Clarity",col=rainbow(15))
#What do you think of this plot?
  
  
  ---
  ## Multiple regression of diamonds data
  
  Next, `price` and `Certification`.
```{r fig.height=3.4}
#What do you think of this plot?
  
  
  ---
  ## Multiple regression of diamonds data
  
  How about, `log(price)` and `Certification` instead
```{r fig.height=3.4}
boxplot(log(Price)~Certification,data=diamonds,ylab="Log Price",xlab="Certification",
        col=rainbow(15))
#What do you think of this plot?
  
  
  
  ---
  ## Multiple regression of diamonds data
  
  Let's make some plots to explore interactions. First, `log(price)` and `Carats` by `Color`
```{r fig.height=3.6}
xyplot(log(Price)~Carats|Color,data=diamonds,group=Color,type=c("p","r"),col.line="red4")
#Is there an interaction effect?


---
## Multiple regression of diamonds data

Next, `log(price)` and `Carats` by `Clarity`
```{r fig.height=3.6}
xyplot(log(Price)~Carats|Clarity,data=diamonds,group=Clarity,type=c("p","r"),col.line="red4")
#Is there an interaction effect?

---
## Multiple regression of diamonds data

Next, `log(price)` and `Carats` by `Certification`
```{r fig.height=3.6}
xyplot(log(Price)~Carats|Certification,data=diamonds,group=Certification,type=c("p","r"),
       col.line="red4")
#Is there an interaction effect?


---
## Multiple regression of diamonds data

We also can examine interactions among categorical variables
```{r fig.height=3.6}
bwplot(log(Price)~Clarity|Color,data = diamonds)
#Is there an interaction effect?



---
## Multiple regression of diamonds data

We also can examine interactions among categorical variables
```{r fig.height=3.6}
bwplot(log(Price)~Certification|Color,data = diamonds)
#Is there an interaction effect?


---
## Multiple regression of diamonds data

We also can examine interactions among categorical variables
```{r fig.height=3.6}
bwplot(log(Price)~Certification|Clarity,data = diamonds)
```

--
<div class="question">
Is there an interaction effect?
</div>


---
## Multiple regression of diamonds data

- We see some evidence of non-normality, non-linearity (with `Carats`) and non-constant variance overall. 

- Taking the log transformation helped with those. 

- We might need a quadratic term for `Carats`. Maybe?

- We might also consider interactions between `Clarity` and `Color`.

- Let's mean center the numerical predictors (just `Carat`)  to avoid multicollinearity.
```{r}
diamonds$CaratsCent <- diamonds$Carats - mean(diamonds$Carats)
diamonds$CaratsCent2 <- diamonds$CaratsCent^2
```

- Based on our EDA, our candidate model is
.block[
  .small[
    $$\boldsymbol{y} = \boldsymbol{X}\boldsymbol{\beta} + \boldsymbol{\epsilon}; \ \ \boldsymbol{\epsilon} \sim N(0, \sigma^2 \boldsymbol{I}).$$
      ]
  ]

where $\boldsymbol{X}$ contains Carats, $\textrm{Carats}^2$, Color, Clarity, Certification, and Color:Clarity.


---
  ## Diamonds data: basic MLR
  
  First, a MLR model with only main effects
.large[
  ```{r}
  Model1 <- lm(Price~CaratsCent+Color+Clarity+Certification,data=diamonds);summary(Model1)
  ```
  ]


---
  ## Multiple regression of diamonds data
  
  By the way, we can change the baseline levels for the categorical variables.
.large[
  ```{r}
  Model1 <- lm(Price~CaratsCent+relevel(Color,ref="E")+Clarity+Certification,data=diamonds);summary(Model1)
  ```
  ]


---
  ## Diamonds data: model assessment
  
  Now some model assessment
```{r fig.height=3.3}
plot(Model1$residual~diamonds$CaratsCent,ylab="Residual",col='navy')
abline(0,0,col="red4")
```

--
  .block[
    Clearly, some problems!
      ]


---
  ## Diamonds data: model assessment
  
  ```{r fig.height=3.3}
boxplot(Model1$residual~diamonds$Color,ylab="Residual",col='cyan')
abline(0,0,col="red4")
```

--
  <div class="question">
  What do you think?
  </div>
  
  
  ---
  ## Diamonds data: model assessment
  
  ```{r fig.height=3.3}
boxplot(Model1$residual~diamonds$Clarity,ylab="Residual",col='cyan')
abline(0,0,col="red4")
```

--
  <div class="question">
  What do you think?
  </div>
  
  
  ---
  ## Diamonds data: model assessment
  
  ```{r fig.height=3.3}
boxplot(Model1$residual~diamonds$Certification,ylab="Residual",col='cyan')
abline(0,0,col="red4")
```

--
  <div class="question">
  What do you think?
  </div>
  
  
  ---
  ## Diamonds data: better MLR
  
  Let's fit our EDA suggested model instead but without the interactions.
.small[
```{r}
Model2 <- lm(log(Price)~CaratsCent+CaratsCent2+Clarity+Color+Certification,data=diamonds);summary(Model2)
```
]


---
## Diamonds data: model assessment

Now some model assessment on the new model.
```{r fig.height=3.3}
plot(Model2$residual~diamonds$CaratsCent,ylab="Residual",col='navy')
abline(0,0,col="red4")
```

--
<div class="question">
What do you think?
</div>


---
## Diamonds data: model assessment

```{r fig.height=3.3}
boxplot(Model2$residual~diamonds$Color,ylab="Residual",col='cyan')
abline(0,0,col="red4")
```

--
<div class="question">
What do you think?
</div>


---
## Diamonds data: model assessment

```{r fig.height=3.3}
boxplot(Model2$residual~diamonds$Clarity,ylab="Residual",col='cyan')
abline(0,0,col="red4")
```

--
<div class="question">
What do you think?
</div>

---
## Diamonds data: model assessment

```{r fig.height=3.3}
boxplot(Model2$residual~diamonds$Certification,ylab="Residual",col='cyan')
abline(0,0,col="red4")
```

--
<div class="question">
What do you think?
</div>


---
## Checking independence and equal variance

```{r fig.height=3.3}
plot(Model2,which=1)
```

--
<div class="question">
Do you see any clear violations of the independence and equal variance assumptions?
</div>


---
## Checking independence and equal variance

```{r fig.height=3.3}
plot(Model2,which=3)
```

--
<div class="question">
Do you see any clear violations of the independence and equal variance assumptions?
</div>



---
## Checking normality

```{r fig.height=3.6}
plot(Model2,which=2)
```

--
<div class="question">
Do you see any clear violations of the normality assumption?
</div>


---
## Outliers and influential points

```{r fig.height=3.6}
plot(Model2,which=5)
```

--
<div class="question">
Are there any potential outliers or influential points?
</div>





---
## Diamonds data: nested F test

- Let's do a nested $F$-test to see if all the interaction terms between `Clarity` and `Color` are important.

--
  
  - I won't print the results because there are too many interactions but you should run `summary(Model2_inter)` yourself to see.
  ```{r}
Model2_inter <- lm(log(Price)~CaratsCent+CaratsCent2+Clarity*Color+Certification,
                   data=diamonds);
anova(Model2,Model2_inter)
```

--
  <div class="question">
What do we conclude?
</div>

--

- This confirms our EDA.


---
## Diamonds data: nested F test

How about a nested $F$-test for interaction terms between `Color` and `Certification`?
```{r}
Model2_inter3 <- lm(log(Price)~CaratsCent+CaratsCent2+Color+Clarity*Certification,
                   data=diamonds);
anova(Model2,Model2_inter3)
```

--
<div class="question">
What do we conclude?
</div>

--

This also confirms our EDA.


---
## Diamonds data: multicollinearity

Should we be worried about multicollinearity for the model with main effects? Use the .hlight[vif] function in the .hlight[rms] package.
```{r}
Model2 <- lm(log(Price)~CaratsCent+CaratsCent2+Clarity+Color+Certification,data=diamonds)
vif(Model2)
```

--
<div class="question">
Should we be worried?
</div>

