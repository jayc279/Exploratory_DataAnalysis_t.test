## 1. Load the ToothGrowth data and perform some basic exploratory data analyses
## Load Data
library(datasets)
data(ToothGrowth)
tg <- ToothGrowth

## 2. Provide a basic summary of the data.
dim(tg)	## 60 3
names(tg)	## "len"  "supp" "dose"
str(tg)
table(tg$dose, tg$supp)
summary(tg$len)

library(ggplot2)
g <- ggplot(tg, aes(x=factor(dose), y=len, fill=supp))
g + geom_boxplot() + 
  ggtitle("Tooth Growth based of Dosage of Type of Supplements used") +
  ylab("Lenght of tooth") + xlab("Dosage") +
  theme(legend.position = "bottom") +
  scale_fill_discrete("Supplement")


# From the boxplot we can observe that the dosage for 0.5 and 1.0 have bigger differences in length
# compared to the other dosages. As dosage increases, length of tooth increases. Of the two supplements
# OJ seems to have more effect on tooth growth than VC
# look at the mean for the length of tooth growth for each dose and supp.

# load libraries 
library(dplyr)
library(tidyr)

tg %>%
    group_by(supp, dose) %>%
    summarize(mean = mean(len), .groups='drop') %>%
    spread(supp, mean) %>%
    mutate(diff = abs(VC - OJ))


# Observing the mean, we can see 2mg/day doses has very small differences as compared to dose of 0.5 and 2. This means it's harder to compare the effectiveness between OJ and VC for dose 2. 

# To formally test the effectiveness between the two supplement types, we will use t.test to find the p-values and confidence intervals to perform hypothesis testing.

# t.test Hypothesis Testing
#######################################################################
# # * Our null hypothesis would is there is no difference between using OJ and VC
# * Our alternative hypothesis is there is a difference between sign OJ and VC
# * alpha rate is set at 0.05 as standard  

# Even though dose 2 has the smallest differences, I will still be performing hypothesis testing for all three dose types. 
# First we filter the data based on dose. This makes it easier and cleaner for t.test.

dosage_half <- filter(tg, dose == 0.5)
dosage_one <- filter(tg, dose == 1)
dosage_two <- filter(tg, dose == 2)

## Get the differences in means for the 3 'dosage' groups
OJ_half <- dosage_half %>% filter(supp == "OJ") %>% summarize_at(c("len"), mean)
VC_half <- dosage_half %>% filter(supp == "VC") %>% summarize_at(c("len"), mean)

OJ_one <- dosage_one %>% filter(supp == "OJ") %>% summarize_at(c("len"), mean)
VC_one <- dosage_one %>% filter(supp == "VC") %>% summarize_at(c("len"), mean)

OJ_two <- dosage_two %>% filter(supp == "OJ") %>% summarize_at(c("len"), mean)
VC_two <- dosage_two %>% filter(supp == "VC") %>% summarize_at(c("len"), mean)

# When dosage is '1' when using supplement 'OJ' tooth growth was biggest
# When dosage is '2', the trend starts to reverse - supplement 'VC' seem to gain
# 'OJ" but result is conclusive since we don't have data past two dosages

### t-test for 0.5 mg/day dose
t.test(len ~ supp, dosage_half)

###  t-test for 1 mg/day dose
t.test(len ~ supp, dosage_one)

###  t-test for 2 mg/day dose
t.test(len ~ supp, dosage_two)

## # Sort by group then ID
## sleep <- sleep[order(sleep$group, sleep$ID), ]

## Again, the t-test function can be used on a data frame with a grouping variable, 
## or on two vectors. It relies the relative position to determine the pairing. 
### If you are using long-format data with a grouping variable, the first row with 
## group=1 is paired with the first row with group=2. It is important to make sure 
## that the data is sorted and there are not missing observations; otherwise the 
## pairing can be thrown off. In this case, we can sort 
## by the group and ID variables to ensure that the order is the same.
dosage_one <- dosage_one[order(dosage_one$dose, dosage_one$supp), ]


## By default, t.test does not assume equal variances; instead of Student's t-test, 
## it uses the Welch t-test by default. Note that in the Welch t-test, df=17.776, 
## because of the adjustment for unequal variances. To use Student's t-test, 
## set var.equal=TRUE.

## The paired t-test is equivalent to testing whether difference between each pair 
## of observations has a population mean of 0. (See below for comparing a single 
## group to a population mean.)

# table to summarize the t.tests results 
dose <- c(0.5, 1.0, 2.0)
p_value <- c(0.0064, 0.0010, 0.9639)
conf.int <- c("1.72, 8.78", "2.80, 9.06", "-3.80, 3.64")
decision <- c("Reject null", "Reject null", "Do not reject null")
data.frame(dose, conf.int, p_value, decision)

# As expected, the p-values for dose 0.5 and 1.0 will be very small because of the big differences in mean between them. 
# Thus, for dose 0.5 and 1.0, since p-values are smaller than 0.5, we reject the null hypotheses that the supplement types don't have a difference on tooth growth. But for dose 2.0 mg/day, we can reject the null as the p-value is greater than 0.5.
