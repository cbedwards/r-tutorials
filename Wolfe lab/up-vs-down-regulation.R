## Script by Dr. Collin Edwards, ORCID: 0000-0002-4937-5159, eco-eco.org

## NOTE: Wordpress (at least my version) doesn't allow the upload of 

## ###############################################################
## Setup -------------------
## ###############################################################

## we're going to use three packages: 
##   lme4 has our model-fitting tools
##   car has the appropriate Anova() function.
##   emmeans has appropriate tools for post-hoc tests to say which taxa differ

#install package lme4 if needed by uncommenting the next line:
# install.packages("lme4")
library(lme4)

#install package car if needed by uncommenting the next line:
# install.packages("car")
library(car)

#install package emmeans if needed by uncommenting the next line:
# install.packages("emmeans")
library(emmeans)

## ###############################################################
## Step 1: basic example --------------------
## ###############################################################

## let's create example data for a single taxa, counting the number of up and down-regulated
## genes that were significant.
## We'll create this data as a matrix with 1 row and two columns for now.
## We're basicallly treating up and down regulation (of the significantly changed genes)
## as if they were "wins" and "losses" or "heads" and "tails" for logistic regression
dat.example = cbind(upregulated = 350, downregulated = 250)
#fit the model
fit = glm(dat.example ~ 1, family = "binomial")
coef(fit)
## note that the coefficients will be on the LOGIT scale. As quick reference,
## 0 on the logit scale means a 50:50 chance, 
##   -infinity means a 0% chance, +infinity means a 100% chance.
## we can backtransform to actual probabilities with the plogis() function
plogis(coef(fit))
## if we choose a significantly changed gene at random, 58.3% of the time it'll be UPregulated
## (since the first column of dat.example is the upregulated)

## more importantly, we can look at the 95% confidence interval. If it doesn't span 50:50, 
## then the changed genes are significantly more likely to be upregulated than down.
## confint() is our friend here, but note that by default it ALSO spits out results on the logit
## scale. So when we run it:
confint(fit)
## that's our 95% CI on the logit scale, and we can see that it DOESN'T span 0, 
## which means that the 95% CI doesn't span 50:50 odds. But we can get this on 
## the scale of probabilities with
plogis(confint(fit))
## there's our confidence interval for the probability of a (signifcant) gene chosen
## at random being upregulated


## ###############################################################
## Step 2: more realistic example ---------------
## ###############################################################
## 
## What if we have multiple taxa, and we want to compare across taxa as well as 
## between up and down regulation? Now we're going to have to think about the tests for that
## as well as more complicated data.
## 
## I think the easiest approach is to have a data frame with a column of 
## taxa names, a column of the # of singificantly up-regulated genes, and a 
## column of the # of significantly down-regulated genes. It's probably best/easiest
## to make that in excel, but I'll code it by hand here for my example. 
## If you read in from a  file, make sure it looks like mine. I'll label the columns
## "taxa", "upregulated", and "downregulated" -- if you choose different
## column names, you'll need to similarly update the code referencing those columns
## 
## NOTES ON USING EXCEL FOR R: 
## (a) you want no extraneous details on your data page: it should be a rectangle 
##   of entries with a single row for column names, then rows for the entries. 
## (b) R doesn't like column names to start with numbers, or to include
##   spaces or symbols. It will modify those names when it reads in the files, and your life is easier
##   if you just R-friendly names in excel. Periods are fine, so I tend to use them in the place
##   of dashes or spaces in column names. Also, since you will have to type column
##   names into your R code I'd suggest avoiding uppercase letters/names for columns 
##   - it's just more work to use shift/capslock, and if NOTHING is uppercase, you 
##   don't have to worry about accidentally mixing up the cases in your code.
## (c) The cleanest way to read things in (minimal chance of mistakenly reading things in wrong)
##   s to save your data as a .csv, and then use read.csv() in R to read it in. 
##   
dat.examp = data.frame(taxa = c("turtle", "unicorn", "axolotl", "milkweed"),
                       upregulated = c(350, 310, 400, 200),
                       downregulated = c(250, 290, 100, 50))
## let's look at my example data:
dat.examp

## First: fit our model
## Since we're working with a data-frame and we want the response to be a matrix, 
## the code is slightly fiddly, and we have to use cbind.
## Note that we're also including `0 +` in our equation so that R estimates the 
## probability of each taxa, instead of using the probability of "axolotl" as the baseline
## and reporting the other coefficients as the difference between them and axolotl.
fit = glm(cbind(upregulated, downregulated) ~ 0 + taxa, 
          family = "binomial",
          data = dat.examp)

## okay, our model is fit! Here's our estimated probabilies/proportions for each taxa:
plogis(coef(fit))
## each of these is the proportion of the significantly changed genes being upregulated
## Admittedly, we could also get those numbers by dividing the number of up-regulated
## genes by the number of up AND downregulated genes. BUT we can get confidence intervals!

## Confidence intervals:
plogis(confint(fit))

## Are some of the taxa significantly different from one another?
## This is where we want Anova() from the car package
Anova(fit)

## significantly different! If taxa were statistically indistinguishable, we would stop here.
## But we know that some of the taxa are different from others, but not which ones.
##   In the example case, axolotyl and milkweed have identical proportions, so even
##   though Anova said some taxa are different, we know that this doesn't mean all of
##   them are different from one another.
## Now we need to use emmeans to do what's called a "post-hoc" test!
## emmeans is a little fiddly because it's designed to allow you to ask questions
## from really complicated models with lots of predictors, where maybe you only want to 
## compare across one of them. So we have to tell it the specifications, which I'm doing
## in the function call
em = emmeans(fit, specs = ~ taxa)
em
## note that by default emmeans reports everything on the logit scale again. (and has a message for that)
## But we can actually tell it to report things on the "response" (ie natural) scale instead
em = emmeans(fit, specs = ~ taxa, type = "response")
em

#if we compare the asymp.LCL and asymp.UCL ("asymptotic lower confidence limit" 
#and "asymptotic upper confdience limit", we can see that these are the same as
#the plogis(confint(fit)) - they're calculating the same things using minimally different algorithms. 
#So the emmeans object gives us everything we need to make a barplot of the probabilites
#of up or down regulation: prob, lower and upper confidence limits, and we don't need to worry
#about df. 
#
#NERDY SIDE NOTE: Because the model is fit on a logit scale, reporting the SEs doesn't really mean
#anything. The SE of a fitted model is only meaningful on the scale it was fit on -
# which means it's totally fine for linear regression which assumes a normal distribution of
# errors (ie lm() ) but isn't helpful here.

## But which taxa are different form each other?
pairs(em)
## There we go! the P values here tell us which pairs of taxa are significantly
## different from one another.


## ###############################################################
## minimal code without explanations: -------------
## ###############################################################
## presuming we have the data already. might read it in from a csv with the following line:
#dat.examp = read.csv("SOMEFILE NAME) #with columns "taxa", "upregulated", and "downregulated
fit =  glm(cbind(upregulated, downregulated) ~ 0 + taxa, 
           family = "binomial",
           data = dat.examp)
em = emmeans(fit, specs = ~ taxa, type = "response")
em ## our key results
pairs(em) ## our other key results


## ###############################################################
## quick ggplot of proportion up and down-regulation ----------
## ###############################################################
# install.package(ggplot2)
library(ggplot2)
## first, emmeans objects aren't data frames. Let's fix that.
dat.plot = summary(em)
ggplot(data = dat.plot, aes(x = taxa))+
  geom_segment(aes(xend = taxa, y = asymp.LCL, yend = asymp.UCL),
               col = 'blue')+ ## make blue 95% confidence intervals
  geom_point(aes(y = prob))+
  geom_hline(yintercept = .5, linetype = 2)+ ## dashed line for 50:50 up and down-regulation
  ylim(c(0,1))+ #plot from 0 to 1 on the y axis
  xlab("")+ #turn off x axis label
  ylab("Proportion up-regulated")+ #give y axis label
  ggtitle("Comparing up- and down-regulation across taxa")+
  theme_bw() #use the bw theme, which is maybe a bit more publication friendly. 
## you can remove theme_bw() and the plus before it to use the default theme (gray background, etc)
## 
## QUICK NERDY ASIDE: comparing overlap of 95% confidence intervals from a plot like this
## isn't quite the same as our pairs() call above. The pairwise contrasts from pairs()
## is the thing to trust in terms of taxa being significantly different or not.


## ###############################################################
## Talking about this approach in the methods section of a publication --------
## ###############################################################
## If you use this approach for publication, your explanation should touch on these key facts:
## - you used logistic regression to identify when upregulation was significantly more
## common than downregulation (or vice versa), using only genes that were identified as 
## significantly up-regulated or down-regulated when using [whatever regulation analysis 
## pipeline thing is]
## - You identified significant tendencies for up or down regulation within taxa based
## on the 95% confidence interval for each taxa
## - You identified (or found a lack of support for) significant variation between taxa
## using Anova(). You then carried out post-hoc analysis with Tukey's test to identify 
## significant differences between individual pairs of taxa.

## In terms of citing software and packages, I've taken to including a single paragraph
## at the end of my methods sections that quickly summarizes the key pieces. Here's what that looks
## like for my recent ecology pub:
## (YOUR SECTION WILL LOOK DIFFERENT SINCE YOU ARE USING DIFFERENT PACKAGES)
## 
## "Software and Packages
##     All analyses and simulations were carried out in the programming language 
##     R (R Core Team 2021). We used the following key packages: mgcv (Wood 2011) 
##     to fit mixed effects models; randomForest (Liaw and Wiener 2002) to fit 
##     Random Forest models; car (Fox and Weisberg 2019) for marginal hypothesis 
##     testing and calculating sum of squares; the tidyverse package suite 
##     (Wickham et al. 2019) for data cleaning and manipulation; ggplot2 (Wickham 2016),
##     ggpubr (Kassambara 2020), and cowplot (Wilke 2020) for creating figures. 
##     The scripts used to carry out all our analyses are available on Figshare 
##     (doi:10.6084/m9.figshare.20421633).
##     
##     From: Edwards, Ellner and Agrawal 2022. 
##     "Plant defense synergies and antagonisms affect performance of specialist 
##     herbivores of common milkweed." Ecology. https://doi.org/10.1002/ecy.3915


## ###############################################################
## in terms of your reference section: ---------------
## ###############################################################

## For citation information for R:
citation()
## For citation information on the packages:
citation("lme4")
citation("car")
citation("emmeans")

