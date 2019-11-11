

https://www.kaggle.com/chrisbow/an-introduction-to-facebook-ad-analysis-using-r


#STEP1) Setup
----------------

packages.install("tidyverse")

library(tidyverse)

#import data
data <- read_csv("./KAG_conversion_data.csv")

# take a quick look at the data
glimpse(data)

Observations: 1,143
Variables: 11
$ ad_id               <dbl> 708746, 708749, 708771, 708815, 708818, 708820, 7…
$ xyz_campaign_id     <dbl> 916, 916, 916, 916, 916, 916, 916, 916, 916, 916,…
$ fb_campaign_id      <dbl> 103916, 103917, 103920, 103928, 103928, 103929, 1…
$ age                 <chr> "30-34", "30-34", "30-34", "30-34", "30-34", "30-…
$ gender              <chr> "M", "M", "M", "M", "M", "M", "M", "M", "M", "M",…
$ interest            <dbl> 15, 16, 20, 28, 28, 29, 15, 16, 27, 28, 31, 7, 16…
$ Impressions         <dbl> 7350, 17861, 693, 4259, 4133, 1915, 15615, 10951,…
$ Clicks              <dbl> 1, 2, 0, 1, 1, 0, 3, 1, 1, 3, 0, 0, 0, 0, 7, 0, 1…
$ Spent               <dbl> 1.43, 1.82, 0.00, 1.25, 1.29, 0.00, 4.77, 1.27, 1…
$ Total_Conversion    <dbl> 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
$ Approved_Conversion <dbl> 1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0…


The documenation describes the columns in the data as follows:
1.) ad_id: unique ID for each ad.
2.) xyz_campaign_id: an ID associated with each ad campaign of XYZ company.
3.) fb_campaign_id: an ID associated with how Facebook tracks each campaign.
4.) age: age of the person to whom the ad is shown.
5.) gender: gender of the person to whom the add is shown
6.) interest: a code specifying the category to which the person’s interest belongs (interests are as mentioned in the person’s Facebook public profile).
7.) Impressions: the number of times the ad was shown.
8.) Clicks: number of clicks on for that ad.
9.) Spent: Amount paid by company xyz to Facebook, to show that ad.
10.) Total conversion: Total number of people who enquired about the product after seeing the ad.
11.) Approved conversion: Total number of people who bought the product after seeing the ad.

# look for unique values in 'age' column
unique(data$age)

# create copy of data for editing
dataTf <- data

# replace character string age ranges with number
dataTf$age[dataTf$age == '30-34'] <- 32
dataTf$age[dataTf$age == '35-39'] <- 37
dataTf$age[dataTf$age == '40-44'] <- 42
dataTf$age[dataTf$age == '45-49'] <- 47

# convert variable to integer
dataTf$age <- as.integer(dataTf$age)


# let's just check that age variable now
unique(dataTf$age)
str(dataTf$age)


# convert gender variable to integer
dataTf$gender[dataTf$gender == 'M'] <- 0
dataTf$gender[dataTf$gender == 'F'] <- 1
dataTf$gender <- as.integer(dataTf$gender)


# abbreviate some variable names
dataTf <- dataTf %>%
  rename(xyzCampId = xyz_campaign_id, fbCampId = fb_campaign_id, impr = Impressions,
        conv = Total_Conversion, appConv = Approved_Conversion)

glimpse(dataTf)

install.packages("heatmaply")
library(heatmaply)
dataMatNorm <- as.matrix(normalize(dataTf, method = "standardize"))
heatmap(dataMatNorm)

#STEP2) Creating New Features
-------------------------------

Creating additional features
While we have the main 'building blocks' of our KPIs in the original dataset, there are some standard metrics missing, so let's take the opportunity to add them here.

Click-through-rate (CTR). This is the percentage of how many of our impressions became clicks. A high CTR is often seen as a sign of good creative being presented to a relevant audience. A low click through rate is suggestive of less-than-engaging adverts (design and / or messaging) and / or presentation of adverts to an inappropriate audience. What is seen as a good CTR will depend on the type of advert (website banner, Google Shopping ad, search network test ad etc.) and can vary across sectors, but 2% would be a reasonable benchmark.
Conversion Rate (CR). This is the percentage of clicks that result in a 'conversion'. What a conversion is will be determined by the objectives of the campaign. It could be a sale, someone completing a contact form on a landing page, downloading an e-book, watching a video, or simply spending more than a particular amount of time or viewing over a target number of pages on a website.
Cost Per Click (CPC). Self-explanatory this one: how much (on average) did each click cost. While it can often be seen as desirable to reduce the cost per click, the CPC needs to be considered along with other variables. For example, a campaign with an average CPC of £0.5 and a CR of 5% is likely achieving more with its budget than one with a CPC of £0.2 and a CR of 1% (assuming the conversion value is the same.
Cost Per Conversion. Another simple metric, this figure is often more relevant than the CPC, as it combines the CPC and CR metrics, giving us an easy way to quickly get a feel for campaign effectiveness.
There are other values that are also very useful in assessing the performance of a marketing campaign. One of these is the conversion value: how much each conversion is worth. For example, our conversion could be a signup form on a landing page to receive information about a new car. If we know that, on average, 1% of people end up purchasing a car for £10,000, we can use that figure in calculating what our target cost per conversion should be.

For an e-commerce site, we could implement conversion tracking to tie-up the value of specific transactions to particular campaigns, this would allow us to assign the actual amount of revenue generated by each campaign / ad creative.

Knowing the conversion value would allow us to calculate other KPIs such as the Return on Advertising Spend (ROAS). While advertising campaigns have other benefits (such as increased brand awareness and future purchases based on customer lifetime value) that may factor into the over return on investment (ROI), ROAS can quickly tell us how a campaign is paying for itself. It is simply the revenue as a percentage of the advertising spend. If a campaign costs £100 and leads to £400 sales, the ROAS is 400% (or 4).

Create the additional KPIs with dplyr
With the variables we have in the dataset, we can easily create the CTR and CPC figures using the mutate function from the dplyr package:

dataTf <- dataTf %>%
  mutate(CTR = ((Clicks / impr) * 100), CPC = Spent / Clicks)
dataTf$CTR <- round(dataTf$CTR, 4)
dataTf$CPC <- round(dataTf$CPC, 2)

> glimpse(dataTf)
Observations: 1,143
Variables: 13
$ ad_id     <dbl> 708746, 708749, 708771, 708815, 708818, 708820, 708889, 708…
$ xyzCampId <dbl> 916, 916, 916, 916, 916, 916, 916, 916, 916, 916, 916, 916,…
$ fbCampId  <dbl> 103916, 103917, 103920, 103928, 103928, 103929, 103940, 103…
$ age       <int> 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,…
$ gender    <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
$ interest  <dbl> 15, 16, 20, 28, 28, 29, 15, 16, 27, 28, 31, 7, 16, 16, 20, …
$ impr      <dbl> 7350, 17861, 693, 4259, 4133, 1915, 15615, 10951, 2355, 950…
$ Clicks    <dbl> 1, 2, 0, 1, 1, 0, 3, 1, 1, 3, 0, 0, 0, 0, 7, 0, 1, 0, 1, 4,…
$ Spent     <dbl> 1.43, 1.82, 0.00, 1.25, 1.29, 0.00, 4.77, 1.27, 1.50, 3.16,…
$ conv      <dbl> 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2,…
$ appConv   <dbl> 1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1,…
$ CTR       <dbl> 0.0136, 0.0112, 0.0000, 0.0235, 0.0242, 0.0000, 0.0192, 0.0…
$ CPC       <dbl> 1.43, 0.91, NaN, 1.25, 1.29, NaN, 1.59, 1.27, 1.50, 1.05, N…

# create trimmed dataset
dataTfTrim <- dataTf %>%
  select(CTR, CPC, appConv, conv, impr, Spent, Clicks)
# omit missing values, normalise data, calculate correlations and plot heatmap
heatmap(cor(normalize(na.omit(dataTfTrim))))


#STEP3) Look at a Campaign in more detail
--------------------------------------------

For our next stage in the analysis, we'll look at a specific campaign in more detail and see what we can pull out of it. First of all, let's choose a campaign, the one on which we regularly spend the most money and regularly get the most conversions (and for which we have the most data!) might be a good place to start.

#there are only 3 campaigns
unique(dataTf$xyzCampId)
[1]  916  936 1178

# set plot size options
options(repr.plot.width=4, repr.plot.height=3)

ggplot(dataTf, aes(as.factor(xyzCampId), Spent)) + geom_boxplot() 
  + labs(x = "Campaign", y = "Advertising Spend")


ggplot(dataTf, aes(as.factor(xyzCampId), conv)) + geom_boxplot() 
  + labs(x = "Campaign", y = "Conversions")


data1178 <- data %>%
  rename(xyzCampId = xyz_campaign_id, fbCampId = fb_campaign_id, impr = Impressions,
        conv = Total_Conversion, appConv = Approved_Conversion) %>%
  filter(xyzCampId == 1178)

> glimpse(data1178)
Observations: 625
Variables: 11
$ ad_id     <dbl> 1121091, 1121092, 1121094, 1121095, 1121096, 1121097, 11210…
$ xyzCampId <dbl> 1178, 1178, 1178, 1178, 1178, 1178, 1178, 1178, 1178, 1178,…
$ fbCampId  <dbl> 144531, 144531, 144531, 144531, 144531, 144532, 144532, 144…
$ age       <chr> "30-34", "30-34", "30-34", "30-34", "30-34", "30-34", "30-3…
$ gender    <chr> "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M",…
$ interest  <dbl> 10, 10, 10, 10, 10, 15, 15, 15, 15, 15, 16, 16, 16, 16, 18,…
$ impr      <dbl> 1194718, 637648, 24362, 459690, 750060, 30068, 1267550, 305…
$ Clicks    <dbl> 141, 67, 0, 50, 86, 1, 123, 340, 1, 30, 202, 9, 1, 95, 123,…
$ Spent     <dbl> 254.05, 122.40, 0.00, 86.33, 161.91, 1.82, 236.77, 639.95, …
$ conv      <dbl> 28, 13, 1, 5, 11, 1, 24, 60, 2, 7, 40, 5, 2, 26, 6, 4, 7, 1…
$ appConv   <dbl> 14, 5, 1, 2, 2, 0, 10, 17, 1, 3, 21, 2, 0, 14, 2, 1, 4, 0, …


install.packages("DataExplorer")
library(DataExplorer)

# look for missing data
plot_missing(data1178)

options(repr.plot.width=4, repr.plot.height=4)
plot_bar(data1178)

options(repr.plot.width=8, repr.plot.height=4)
plot_histogram(data1178)

# and we'll revisit our correlation matrix for the 1178 campaign
plot_correlation(data1178")

data1178 <- data1178 %>%
  mutate(totConv = conv + appConv,
        conVal = conv * 5,
        appConVal = appConv * 100) %>%
  mutate(totConVal = conVal + appConVal) %>%
  mutate(costPerCon = round(Spent / totConv, 2),
        ROAS = round(totConVal / Spent, 2))

Let's also introduce another KPI that we haven't talked about yet: Cost Per Mille (CPM). This number is the cost of one thousand impressions. If your objective is ad exposure to increase brand awareness, this might be an important KPI for you to measure.

data1178 <- data1178 %>%
  mutate(CPM = round((Spent / impr) * 1000, 2))
# take a look at our new variables
head(data1178)

Preliminary analysis of campaign 1178
The types of analyses you would want to perform on your own data will depend very much on your campaign objectives, what sort of data you have and what decisions you want to be able to have the insight to support. As we don't know the context for this dataset, we'll take a look through it from the point of view of exploratory data analysis, using the types of tools you could use on your own data.

For our purposes, we'll assume that this is an e-commerce business that is purely focussed on maximising revenue.

We'll start by looking at what happens to the number of conversions and the value of our conversions when we spend more money on our campaign. If we spend more, do we get more back?

options(repr.plot.width=6, repr.plot.height=3)
ggplot(data1178, aes(Spent, totConv)) + geom_point() + geom_smooth(method = "lm") +
  labs(x = "Amount spent on campaign", y = "Total number of conersions")
ggplot(data1178, aes(Spent, totConVal)) + geom_point() + geom_smooth(method = "lm") +
  labs(x = "Amount spent on campaign", y = "Total value of conversions")

With Facebook, we have a lot of demographic information we can use, so we'll use that to break apart our dataset, we'll start by splitting the data by gender:

options(repr.plot.width=4, repr.plot.height=3)
ggplot(data1178, aes(gender, ROAS)) + geom_boxplot() + scale_y_log10()


The data look quite symmetrical with a log-transformed axis, but without the log-transformation, it doesn't fit the normal distribution, so we'll look to see if this difference is significant using a non-parametric test:

wilcox.test(ROAS ~ gender, data=data1178)

And let's look for the median and the mean of these data:


data1178 %>%
  select(gender, ROAS) %>%
  group_by(gender) %>%
  filter(ROAS != 'Inf') %>%
  summarise(medianROAS = median(ROAS), meanROAS = mean(ROAS))

Analysis by interest
It looks like our ROAS is a higher for males than females and that this difference is statistically significant (p < 0.01).

In this case, while the median does give us a more accurate estimation of what our ROAS would be for a particular adID, there are a lot of points that pull the data towards the right. Over time, the ROAS is more likely to tend towards the mean. Using that figure, we can see that the ROAS differences by gender are quite striking and, depending on the profit margins involved, could make the difference between the campaign being profitable or not.

However, we have the data to go a lot more granular than this, so let's see how else we can break the dataset apart. We'll look at interests next:



Final thoughts
Hopefully this notebook has been of some use to if you're new to pay-per-click advertising, or if you've been looking for new ways to try and improve ROI from your digital campaigns.

This notebook is just a quick glimpse into the sort of analyses you can do with your digital advertising datasets, but it really is only a starting point: the correct types of analysis and measures of success will be driven by your own business model and your underlying marketing objectives.

For example, if you have a physical business as well as an online presence, how do you factor in people becoming aware of your business, product or promotion online, but converting in store in person? What about products with long buying cycles, where the resulting conversion could be months after the initial

















