#    Highland Statistics Ltd.
#    www.highstat.com
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.

## This code is an adjusted version of code provided by Highland Statistics during one of their courses. 
## The code was adjusted by Inger van den Bosch for her PhD project on harbour poproise ecology. 

#set the working directory & read the data
#On a Windows computer: adjust blah blah blah
#setwd("c:/blah blah blah")

setwd("C:/R PhD/Habitat use modelling")


##Hier lijtk al iets fout te gaan met het inladen van de database en dan voor irt het aantal kolommen (volgende stap)...

#Import the data from a tab delimited ascii file
HPN <- read.csv(file = "Output09082018.csv",
                   header = TRUE, sep = ";", dec = ",")

#dec = ','   means that the comma is used for decimals. 
#Change to   dec = "."   if required.
########################################################################


########################################################################
#Housekeeping for data exploration
#Load packages from R and support functions that we wrote

library(lattice)  #For fancy multipanel graphs



source("HighstatLibV10.R")   #<------
#For fancy graphs


#House keeping option for GAM later on
#Load packages from R and support functions
install.packages("lattice")
install.packages("ggplot2")
install.packages("plyr")
install.packages("mgcv")
install.packages("nlme") # the mgcv package is asking for this one too...?
install.packages("MASS")

library(lattice)  
library(ggplot2)
library(plyr)
library(nlme)
library(mgcv) 
library(MASS)
source("HighstatLibV10.R")
########################################################################

#Inspect the file
#What do we have?
names(HPN)

str(HPN)  #Make sure you have num and not factors for the numerical variables!
## FLIGHT and DATE are Factors. Should and if how to change this??


########################################################################
# CHECK BEFORE YOU CONTINUE....
#  - Did you execute the library(lattice) command? 
#  - Did you source the HighstatLibV10.R file without errors?
#  - Does the output of str look like yours?

# If 3x yes..then continue


########################################################################
#Data exploration next steps will be to check for:
# A Outliers in Y / Outliers in X
# B Collinearity X
# C Relationships Y vs X
# D Spatial/temporal aspects of sampling design (not relevant here)
# E Interactions (is the quality of the data good enough to include them?)
# F Zero inflation Y
# G Are categorical covariates balanced?



###################################################
#First some elementary R commands.
# How do you acces variables in an object like HP19912013?
## ABUND en ARE aanpassen naar
HPN             #All data
head(HPN)         #First 6 rows
head(HPN, 10)     #First 10 rows
HPN$DEPTH         #Depth variable
HPN[  ,1]         #First column
HPN[,2]           #Second coloumn
HPN[ , "DEPTH"]   #Depth variable 
HPN[1,"DEPTH"]    #First row of depth variable
HPN[1:10,"DEPTH"] #First 10 rows of depth
c("DEPTHinM", "SLOPEMAP_1")  #Two characters concatenated
HPN[ , c("DEPTH", "SLOPEMAP_1")] #Depth and slope variables

MyVar <- c("DEPTH",  "SLOPEMAP_1")  #Same as last two steps
HPN[, MyVar]

##############################################
#A Outliers
#MAYBE: Copy from here...
par(mfrow = c(1, 2))    #Two graphs next to each other
boxplot(HPN$DEPTH, 
        main = "Depth")

dotchart(HPN$DEPTH, 
         xlab = "Depth", 
         ylab = "Order of data")
#...to here and paste into R

par(mfrow = c(1, 1)) # lingle graphs in one
par(mfrow = c(1, 2)) # two graphs in one line
par(mfrow = c(2, 2)) # two graphs in two lines 
## etc
###################################################
# A Outliers in X and Y
##DO: basic outlier check, could add more later on

par(mfrow = c(2, 3))
MyVar1 <- c("DEPTH", "SLOPEMAP_1", "DISTCOAST", "SEIZ", "XUTM", "YUTM")
Mydotplot(HPN[, MyVar1])

par(mfrow = c(2, 2))
MyVar2 <- c("HP", "NEAR_DIST", "NATURETYPE", "PERIOD")
Mydotplot(HPN[, MyVar2])

#Why we don't like boxplots....
par(mfrow = c(1, 2))
boxplot(HPN$DEPTH)
dotchart(HPN$DEPTH)

par(mfrow = c(1, 2))
boxplot(HPN$SLOPEMAP_1)
dotchart(HPN$SLOPEMAP_1)

par(mfrow = c(1, 2))
boxplot(HPN$DISTCOAST)
dotchart(HPN$DISTCOAST)

par(mfrow = c(1, 2))
boxplot(HPN$SEIZ)
dotchart(HPN$SEIZ)




##We know there are spatial outliers, make them vissible with: 
#Is there an outlier in the spatial sampling positions? 
xyplot(YUTM ~ XUTM, 
       aspect = "iso", #x en y as same units
       data = HPN,
       col = 1,
       pch = 16)

xyplot(YUTM ~ XUTM, 
       aspect = "iso", 
       data = HPN,
       col = HP$SEIZ, #1 = black, 2 = red
       pch = 16)

#Identify the outliers 
#Copy and paste from here...
par(mfrow = c(1, 1))
plot(x = HPN$XUTM, 
     y = HPN$YUTM)
identify(x = HPN$XUTM, 
         y =HPN$YUTM)
#..to here. Click close to a point
## No spatial outliers here (everything is where it should be based on the surveys)
#Press ESCAPE to quit


#If there are however spatial outliers in a next dataset then --> 
## Remove the spatial outlier one at a time (based on clicked points and provided numbers)
## Eample function
HPN2 <- HPN[-c(170, 361, 762, 907, 1346, 1350, 1351, 1668, 1726, 1982, 2059, 2109, 2110, 2169, 3254), ]
dim(HPN2)

## Outliers manually identified and removed? but thats not all? Check later!


#Are the missing values?
sum(is.na(HPN$XUTM))
colSums(is.na(HPN))
##No missing vallues in the XUTM
## Also do for YUTM???
sum(is.na(HPN$YUTM))
colSums(is.na(HPN))
## neither for YUTM
###############################################################
###############################################################
#Remove missing values if needed, not needed now:
#Not preferable: Option 1:
HPNx <- na.exclude(HPNx) #Be careful! Every row where there is somewhere an NA will be removed!
## Check of this is OK for this dataset!!!

##DO: Option 2 if needed, not needed now:
I1 <- is.na(HPNx$XUTM) | is.na(HPNx$YUTM)  
# |   or   
# &   and   
# ==  equals
# !=  not equal


I1
HPN2 <- HPNx[!I1, ]
dim(HPN)
dim(HPN22)
dim(HPN3)

#########################################################
#########################################################


#Other option:
#B Outliers in the X other options to run
#Copy from here...
par(mfrow = c(2, 3), mar = c(4, 3, 3, 2)) #mar is white space: botton/left/top/right
dotchart(HPN$SLOPEMAP_1, main = "Slope")
dotchart(HPN$DISTCOAST, main = "DistanceToShore")
dotchart(HPN$SEIZ, main = "Year")
dotchart(HPN$PERIOD, main = "Period")
dotchart(HPN$HP, main = "Count")
#to here and paste into R


#Make a multi-panel dotplot use
##Without "Associated" because is Factor
#the following code.
#First make a vector of names
MyVar <- c("DEPTH", "SLOPEMAP_1", "DISTCOAST", "SEIZ", 
           "HP", "Year")
MyVar


#Zuur et al a wrapper around the dotplot to make it look a bit better
#function. It is in HighstatlibV10.R
#Make sure that you typed library(lattice) and sourced 
#the HighstatLibV10.R file 
##If the mydotplot function doesnt work, add lattice library again from source
Mydotplot(HPN[ ,MyVar])




##Log transformation needed for this data?
##Based on previous results check! 
## I dont think it is needed for this particular set now
#Apply transformations
HPN$LogDEPTH  <- log(HPN$DEPTHinM)
HPN$LogSLOPEMAP_1 <- log(HPN$SLOPEMAP_1)
HPN$LogDISTCOAST <- log(HPN$DISTCOAST)
## If log fixed then use following, otherwise skip to #B
## Then plot again with log adjusted covariates if needed:
MyVar <- c("DEPTHin<", "SLOPEMAP_1", "DISTCOAST", "SEIZ", 
            "PERIOD", "LogDEPTH", "LogSLOPEMAP_1", "LogDISTCOAST")

Mydotplot(HPN[ ,MyVar])
##############################################
##############################################



##############################################
#B Collinearity X (or LOg if needed from previous section)
## With less variables at the same time?
##DO ## Only gives figures without correltaion value
MyVar <- c("XUTM", "YUTM", "DEPTH", "SLOPEMAP_1", "DISTCOAST", "SEIZ", 
           "HP", "PERIOD")
pairs(HPN[, MyVar])



#You need HighstatlibV10.R for this. No need to source
#it again though.
pairs(HPN[,c("DEPTH", "SLOPEMAP_1", "DISTCOAST", "SEIZ", 
               "HP", "PERIOD")],
      lower.panel = panel.cor)


#This will work as well. It is a wrapper around the pairs
#function. 
## reload lattice to prevent error
library(lattice)
source("HighstatLibV10.R") 
MyVar <- c("XUTM", "YUTM", "DEPTH", "SLOPEMAP_1", "DISTCOAST", "SEIZ", 
           "HP", "PERIOD")
Mypairs(HPN[,MyVar])

#B. Collinearity and relationships other option--> example for season:
par(mfrow = c(1, 1))
pairs(HPN[,MyVar], 
      lower.panel = panel.cor)

boxplot(DEPTH ~ SEIZ, 
        data = HPN,
        xlab = "Season",
        ylab = "Depth")

##or Month
boxplot(DEPTHinM ~ Month, 
        data = HPoud,
        xlab = "Month",
        ylab = "Depth")

##Large VIF = larger variance of regression parameters, VIF>3 is too high. Remove higherst VIF and colinearity will go down, 
## repeat removal until all VIF <3
#corvif(HPoud[,c("XUTM", "YUTM", "DEPTHinM", "SLOPEdegrees", "DISTtoSHOREinM", "Season", 
#                 "Month", "Year")

corvif(HPoud[,c("XUTM", "YUTM", "DEPTHinM", "SLOPEdegrees", "DISTtoSHOREinM", "Season", "Month", "Year")])
## Variance inflation factors

#GVIF
#XUTM             7.811697
#YUTM            13.388601
#DEPTHinM         1.461847
#SLOPEdegrees     1.253541
#DISTtoSHOREinM  18.984752
#Season         598.958172
#Month            4.103577
#Year           608.755758
## So remove "Year"first and run again
corvif(HPoud[,c("XUTM", "YUTM", "DEPTHinM", "SLOPEdegrees", "DISTtoSHOREinM", "Season", "Month")])
## Better alreadym now remove DISTtoSHORE
corvif(HPoud[,c("XUTM", "YUTM", "DEPTHinM", "SLOPEdegrees", "Season", "Month")])
## YAY!!! :)




#############################################################################
###############################################################################
#Option: How do you detect collinearity between a continuous covariate
#and a categorical? Make a conditional boxplot
par(mar = c(5,5,2,2))
boxplot(DEPTHinM ~ factor(SLOPEdegrees), ## SLOPE is not a factor here so skip this bit for now
        data = HPoud,
        ylab = "Depth in meters",
        xlab = "Slope in Degrees",
        cex.lab = 1.5)

## Trouble?
##############################################
####################################################


##############################################
#C. Relationships Y vs X
MyVar <- c("DEPTHinM", "SLOPEdegrees", "DISTtoSHOREinM", "Season", 
           "Month", "Year")
pairs(HPoud[, MyVar],
      lower.panel = panel.cor)


boxplot(DEPTHinM ~ factor(SLOPEdegrees), ## Slope not a factor, so skip this bit for now
        data = HPoud,
        varwidth = TRUE,
        ylab = "Depth in meters",
        xlab = "Slope in degrees",
        main = "")

boxplot(DEPTHinM ~ factor(DISTtoSHOREinM), ## same
        data = HPoud,
        varwidth = TRUE,
        ylab = "Depth in meters",
        xlab = "Distance to shore in meters",
        main = "")

boxplot(SLOPEdegrees ~ factor(DISTtoSHOREinM), ## same
        data = HPoud,
        varwidth = TRUE,
        ylab = "Slope in degrees",
        xlab = "Distance to shore in meters",
        main = "")

# or
#C. Relationships between Count and Depth
plot(x = HPoud$DEPTHinM, 
     y = HPoud$Month,
     col = HPoud$Season,
     pch = 16)

boxplot(DEPTHinM ~ Season, 
        data = HPoud,
        xlab = "Seaosn",
        ylab = "Depth")



## What is our Y? Fix script here (not count, all 1 now)
#Plot every covariate versus Y, Y is count? (count all 1 because of split)
MyX  <- c("DEPTHinM", "SLOPEdegrees", "DISTtoSHOREinM", "Season", 
          "Month", "Year")
Myxyplot(HPoud, MyX, "Count", MyYlab = "Count")

#The code for this yourself is rather ugly!
#See 'Beginner's Guide to R' (2009). Zuur, Ieno, Meesters
##############################################
##############################################




##############################################
#D. Spatial/temporal aspects of sampling

#We have already done part of this:
xyplot(YUTM ~ XUTM, 
       aspect = "iso", 
       data = HPoud,
       col = HPoud$Year, #1 = black, 2 = red
       pch = 16)

xyplot(YUTM ~ XUTM | factor(Year), 
       aspect = "iso", 
       data = HPoud,
       col = 1,
       pch = 16)

##Makes no sense with current Y (all count is 1)
#Or if you would like more control
xyplot(Count ~ DEPTHinM | factor(Year),
       data = HPoud, 
       ylab = "Count",
       xlab = "Depth")
##############################################



##############################################
#E Interactions
## This bit does not work yet!! Fix!!
# Take a deep breath
# There is no need to understand all the R code!!!
xyplot(Count ~ DEPTHinM | factor(SLOPEdegrees),
       data = HPoud, 
       layout = c(3,2),
       xlab = "Depth in m",
       ylab = "Count",
       panel=function(x,y){
         panel.points(x, y, col = 1)  #Add points
         panel.abline(lm(y~x))        #Add regression line
       })


#############################################################
#############################################################
## This bit is for other dataset, change to this one? Needed?
#other option still need to adjust script: 
#Why we don't like transformations
#It can remove interactions!

#Code below visualizes the interaction between MeanDepth and Period for
#Density and also for log transformed density

Fish3$LogDens <- log(Fish3$Dens)
coplot(Dens ~ MeanDepth | factor(Period), data = Fish3,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

#win.graph()   #Use quartz() for a MAC
quartz()
coplot(LogDens ~ MeanDepth | factor(Period), data = Fish3,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })
##############################################
###############################################





##############################################
## Does this also coutn for presence only data???
#F. Zero inflation
sum(HPoud$Count == 0)
sum(HPoud$Count == 0) / nrow(HPoud3)

##############################################



##############################################
#G. Are categorical covariates balanced? --> This data has no catergorials (yet)
##table(HPoud$cat var)
#No levels with 0, 1, 2, 3, 4 observations?
##############################################



##############################################
#DON'T MAKE HISTOGRAMS OF COVARIATES!!!!!!!!!!!!!!
#DON'T MAKE HISTOGRAMS OF THE RESPONSE VARIABLE!!!!!!!!!!!!!!
##############################################













########################################################


#Data exploration done? Nearly

dotchart(HPoud$DEPTHinM)
plot(x = HPoud$SLOPEdegrees,
     y = HPoud$DEPTHinM,
     xlab = "Depth",
     ylab = "Slope",
     main = "Collinearity?",
     cex.lab = 1.5)



###################################################


###################################################
#Start analysis

##Optional id density ipv count data... 
# Offset in GLM --> count used as density (count per area) not adjusted for this dataset yet! Could do...
# This step is needed for the offset. Note that we must use the natural log.
HPoud$LSA <- log(HPoud$SweptArea) ## still need to adjust covariate

# Using lm we found:
#  -negative fitted values:    Solution: Poisson/NB 
#  -heterogeneity              Solution: Poisson/NB
#  -non-linear patterns        Solution: GAM
#  -outlier                    Solution: NB

# Problem: Do we have density?
# Solution: Offset + Poisson/NB

# Example: E(Total Abundance ) = exp(Covariates + offset(LSA))
#

# Starting point of the analysis:
# Model 1:
# TotAbund_i ~ Poisson(mu_i)
# E[TotAbund_i] = mu_i
# mu_i = exp(MeanDepth_i * fPeriod_i + offset(LSA_i))

HPoud$fYear <- factor(HPoud$Year)
M0 <- glm(Count ~ DEPTHinM * fYear + offset(Count),
          family = poisson,
          data = HPoud)

# If you get the error: Error in offset(LSA) : object 'LSA' not found
# Then you didn't execute the command: Fish3$LSA <- log(Fish3$SweptArea)

# Check for overdispersion
E0 <- resid(M0, type = "pearson")
Overdispersion <- sum(E0^2) / M0$df.res  #df.res = N - p
Overdispersion


# Plot residuals versus fitted values
plot( x = fitted(M0),
      y = E0,
      xlab = "Fitted values",
      ylab = "Pearson residuals",
      cex.lab = 1.5)
abline(h = 0, lty = 2)
# OK?


# One of the model validation graphs is this one:
plot( x = HPoud$DEPTHinM,
      y = E0,
      xlab = "Depth",
      ylab = "Pearson residuals",
      cex.lab = 1.5)
abline(h = 0, lty = 2)

# assess here if:  Based on what we have seen for this data set, we decided
# to continue with a GAM. And we expect a different pattern
# in each period.
library(ggplot2)

HPoud$E0 <- resid(M0, type = "pearson")
p <- ggplot()
p <- p + geom_point(data = HPoud,
                    aes(x = DEPTHinM, y = E0))
p <- p + geom_smooth(data = HPoud,
                     aes(x = DEPTHinM, y = E0))
p <- p + xlab("Depth") + ylab("Pearson residuals")
p <- p + facet_grid(.~fYear)
p
# Is there a non-linear residual pattern in the first period?


# We could do a GAM with an interaction and see whether:
# 1. We indeed need a smoother.
# 2. We need a smoother with an interaction.
############################################





############################################
# GAM 1:
#  Count_i ~ Poisson(mu_i)
#  E(Count_i) = mu_i
#  var(Count_i) = mu_i
#  log(mu_i) = beta_1 + beta_2 * Seaon_i + s(DEPTHinM_i) + 1 * Density or Count?
M1 <- gam(Count ~ s(DEPTHinM) + fSeason + offset(LSA),
          family = poisson,
          data = HPoud)

par(mfrow = c(1,1))
plot(M1)  #CIs are rather small!
summary(M1)

E1 <- resid(M1, type = "pearson")
Overdispersion1 <- sum(E1^2) / M1$df.res  #df.res = N - p
Overdispersion1
#How does it go?
##Othersise try....

# GAM 2:
# Allow the smoother to change over the seaosns
# log(mu_i) = beta_1 + beta_2 * Season_i + s_j(DEPTHinM_i) + 1 * Density or Count?
# j = 1, 2
M2 <- gam(Count ~ s(DEPTHinM, by = fSeason) + fSeason +
            offset(LSA),
          family = poisson,
          data = HPoud)

par(mfrow = c(1, 2))
plot(M2)
summary(M2)
AIC(M1, M2)  # Which model M1 or M2 is better?

#Check for overdispersion in chosen model (change M2 to M1 if needed)
E2 <- resid(M2, type = "pearson")
Overdispersion2 <- sum(E2^2) / M2$df.res  #df.res = N - p
Overdispersion2
#How does this go?

# Is one or noth of the models overdispersed?


# If so: Apply model validation to figure out why there is overdispersion
Fit2 <- fitted(M2)
plot(x = Fit2,
     y = E2,
     xlab = "Fitted values",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)
# Still a little bit of heterogeneity?




# One option to solve overdispersion is an  NB
#  Count_i ~ NB(mu_i, k)
#  E(Count_i) = mu_i
#  var(Count_i) = mu_i + mu_i^2 / k
#  log(mu_i) = beta_1 + beta_2 * Season_i + s(DEPTHinM_i) + 1 * Density of Count?

M3 <- gam(Count ~ s(DEPTHinM) + fSeason +
            offset(LSA),
          family = nb(),
          data = HPoud3)

#Also apply the model with the Detph x Season interaction for this smoother
M4 <- gam(Count ~ s(DEPTHinM, by = fSeason) + fSeason +
            offset(LSA),
          family = nb(),
          data = HPoud3)

E4 <- resid(M4, type = "pearson")
Overdispersion4 <- sum(E4^2) / M4$df.res
Overdispersion4
#Perfect?


# Compare models
AIC(M3, M4)
# The model with interaction slightly better?
#################################################






#################################################
# Model validation model M4

# Plot residuals versus fitted values
F4 <- fitted(M4)
plot(x = F4, 
     y = E4,
     xlab = "Fitted values",
     ylab = "Pearson residuals")


# Plot residuals vs Depth
plot(x = HOpud3$DEPTHinM, 
     y = E4)
abline(h = 0, lty = 2)

# Plot residials vs Season
boxplot(E4 ~ HPoud3$Season)

# And this one:
p <- ggplot()
p <- p + geom_point(data = HPoud3,
                    aes(x = DEPTHinM, y = E4))
p <- p + geom_smooth(data = HPoud3,
                     aes(x = DEPTHinM, y = E4))
p <- p + xlab("Depth") + ylab("Pearson residuals")
p <- p + facet_grid(.~fSeason)
p
# Is 0 always with the 95% CIs???

################################################




################################################
# Model interpretation
summary(M4)
par(mfrow = c(1, 2))
plot(M4)


# The fitted model is as follows.
# COunt_i ~ NB(mu_i, 2.037)
#
# E(Count_i)  = mu_i
# var(Count_i) = mu_i + mu_i^2 / 2.037
#
# Season 1?
#        ? + f_1(DEPTHinM) + LSA_i
# mu_i = e
#
# Season 2:
#         ? - ? + f_2(DEPTHinM) + LSA_i
#mu_i = e
#
################################################




################################################
# Sketch fitted values



M4 <- gam(Count ~ s(DEPTHinM, by = fSeason) + fSeason +
            offset(LSA),
          family = nb(),
          data = HPoud3)

MyData <- ddply(HPoud3, 
                .(fSeason), 
                summarize,
                DEPTHinM = seq(min(DEPTHinM), 
                                max(DEPTHinM),
                                length = 100))

# Sketch the fitted values for average sampling effort \
MyData$LSA <- log(mean(HPoud3$SweptArea)) ### Adjust Swept Area for our value herefor?
head(MyData)

P4 <- predict(M4, 
              newdata = MyData, 
              se = TRUE, 
              link = "predictor")
MyData$mu   <- exp(P4$fit)
MyData$SeUp <- exp(P4$fit + 1.96 * P4$se.fit)
MyData$SeLo <- exp(P4$fit - 1.96 * P4$se.fit)
head(MyData)


p <- ggplot()
p <- p + geom_point(data = HPoud3, 
                    aes(y = COunt,  
                        x = DEPTHinM,
                        col = fSeason),
                    shape = 20, 
                    size = 2)
p <- p + xlab("Depth") + ylab("Total porpoise counts")## is this right?
p <- p + theme(text = element_text(size=20))
p <- p + geom_line(data = MyData, 
                   aes(x = DEPTHinM, 
                       y = mu,
                       group = fSeason,
                       colour = fSeason))

p <- p + geom_ribbon(data = MyData, 
                     aes(x = DEPTHinM, 
                         ymax = SeUp, ## Adjust to what?
                         ymin = SeLo, ## adjust to what?
                         group = fSeason,
                         fill = fSeason),
                     alpha = 0.2)
p

# How can you criticise this figure?
# What were the fitted values?



# Out of curiosity...did we really needed the GAM?
# Let's compare it with a NB GLM
M5 <- glm.nb(Count ~ DEPTHinM * fSeason + offset(LSA),
             data = HPoud3)

AIC(M3, M4, M5) # GAM justifiable?
# The only question  is...which M are we sticking with?


# You can also do a simulation study like we did for the red squirrel data.
# The code would be identical.

####################################################


# One last question: Do we actually need the offset,
# or should we use sampling effort/count as a covariate?

# We have as options:
M4 <- gam(COunt ~ s(DEPTHinM, by = fSeason) + fSeason +
            offset(LSA),
          family = nb(),
          data = HPoud3)

M6 <- gam(Count ~ s(DEPTHinM, by = fSeason) + fSeason + LSA,
          family = nb(),
          data = HPoud3)

M7 <- gam(Count ~ s(DEPTHinM, by = fSeason) + fSeason + SweptArea, ## What is our offset parameter?
          family = nb(),
          data = HPoud3)

M8 <- gam(Count ~ s(DEPTHinM, by = fSeason) + fSeason + s(SweptArea), ## What is our offset parameter?
          family = nb(),
          data = HPoud3)

AIC(M4, M6, M7, M8)
# Which M is better?


summary(M6) ##Or whichever M is better.






#######################################################
#######################################################
##OR LINEAR REGRESSION MODELLING?
#######################################################
########################################################
#Start analysis
#Apply the linear regression model

M1 <- lm(Count ~ DEPTHinM + factor(Season) + 
           DEPTHinM : factor(Season),
         data = HPoud3)

#Everything signficant?
summary(M1)
drop1(M1, test = "F")  #Same info as there are no factors with > 2 levels
##########################################################





###################################################
# Model validation
# Look at homogeneity: plot fitted values vs residuals
# Look at influential values: Cook
# Look at independence: 
#      plot residuals vs each covariate in the model
#      plot residuals vs each covariate not in the model
#      Common sense (smelly dependence?)
# Look at normality: histogram


E1 <- resid(M1)
F1 <- fitted(M1)

par(mfrow = c(1, 1))
plot(x = F1, 
     y = E1, 
     xlab = "Fitted values",
     ylab = "Residuals")
abline(v = 0, lwd = 2, col = 2)
abline(h = 0, lty = 2, col = 1)
#Negative vallues?? Not good

#Normality
par(mfrow = c(1, 1))
hist(E1, breaks = 10)


#Independence
plot(x = HPoud3$DEPTHinM, 
     y = E1, 
     main = "Independence")
abline(h = 0,  lty = 2)
#Do we have heterogeneity and non-linear patterns?

boxplot(E1 ~ HPoud3$Season)

#Spatial independence?
HPoud3$MyCex <- abs(E1) / max(abs(E1))
HPoud3$MyCol <- E1
HPoud3$MyCol[E1 >= 0 ] <- 1
HPoud3$MyCol[E1 < 0 ]  <- 2

# Are the large residuals all close to each other,
# and are the small residuals all close to
# each other? Are positive residuals all
# close to each other? Are negative residuals all
# close to each other? 
xyplot(YUTM ~ XUTM,
       cex = 4 * sqrt(HPoud3$MyCex),
       pch = 16,
       col = HPoud3$MyCol,
       data = HPoud3,
       main = "Spatial plot of residuals")

# If you are not sure, then read the R-INLA book.
##########################################################





##########################################################
#Model interpretation (though we are wasting our time as
#we should be doing something else)
summary(M1)


##########################################################
#Sketch fitted values

library(plyr)
library(ggplot2)

# This is data for which we will predict density data
MyData <- ddply(HPoud3, 
                .(Season), 
                summarize,
                DEPTHinM = seq(min(DDEPTHinM),
                                length = 25))  ## or other length (100?)?
head(MyData)


P <- predict(M1, newdata = MyData, se = TRUE)

# Add fitted values and 95% CIs  ## use same numbers???? Check!!!
MyData$mu    <- P$fit                    #Fitted values
MyData$selow <- P$fit - 1.96 * P$se.fit  #lower bound
MyData$seup  <- P$fit + 1.96 * P$se.fit  #upper bound
head(MyData)

# And plot the whole thing.
p <- ggplot()
p <- p + geom_point(data = HPoud3, 
                    aes(y = COunt, x = DEPTHinM),
                    shape = 16, 
                    size = 3)
p <- p + xlab("DEPTH (m)") + ylab("Count")
p <- p + theme(text = element_text(size=15))
p <- p + geom_line(data = MyData, 
                   aes(x = DEPTHinM, y = mu), 
                   colour = "black")

p <- p + geom_ribbon(data = MyData, 
                     aes(x = DEPTHinM, 
                         ymax = seup, 
                         ymin = selow ),
                     alpha = 0.5)

p <- p + facet_grid(. ~ Season, scales = "fixed")
p  




# Now the same thing, but wit thwo lines in one panel

MyData$fSeason <- factor(MyData$Season)
Fish3$fSeason  <- factor(Fish3$Season)

p <- ggplot()
p <- p + geom_point(data =HPoud3, 
                    aes(y = Count, 
                        x = DEPTHinM, 
                        col = fSeason),
                    shape = 16, 
                    size = 3)
p <- p + xlab("DEPTH (m)") + ylab("Count in ..")
p <- p + theme(text = element_text(size=15))
p <- p + geom_line(data = MyData, 
                   aes(x = DEPTHinM, 
                       y = mu, 
                       group = fSeason, 
                       col = fSeason))

p <- p + geom_ribbon(data = MyData, 
                     aes(x = DEPTHinM, 
                         ymax = seup, 
                         ymin = selow,
                         fill = fSeason,
                         col = fSeason,
                         group = fSeason),
                     alpha = 0.5,
                     show.legend = FALSE)
p  
# Google is your friend in case you want to change colours, or the heading
# for the label text.





#######################################################
# And this is some old code in case ggplot2 does not work.
# We will not run it.
range(HPoud3$DEPYHinM[HPoud3$Season == 1])
range(HPoud3$DEPTHinM[HPoud3$Season == 2])
tapply(HPoud3$DEPTHinM, INDEX = HPoud3$Season, FUN = range)
MyData1 <- data.frame(DEPTHinM = seq(from = 0 , to = 4865 , ## or whatever range suits our dataset?
                                      length = 25),
                      Period = 1)

MyData2 <- data.frame(DEPTHinM = seq(from = 808 , to = 4840 , ## Or whatever range suits our dataset?
                                      length = 25),
                      Period = 2)

P1 <- predict(M1, newdata = MyData1)
P2 <- predict(M1, newdata = MyData2)

plot(x = HPoud3$DEPTHinM, 
     y = HPoud3$COunt, 
     col = HPoud3$Season,
     xlab = "Depth",
     ylab = "Count", 
     main = "But it is all rubbish!", ### Is it?
     pch = 16)

lines(x = MyData1$DEPTHinM, y = P1, col = 1, lwd = 5)
lines(x = MyData2$DEPTHinM, y = P2, col = 2, lwd = 5)

legend("topright",
       legend = c("Period 1", "Period 2"),
       col = c(1,2),
       lty = c(1,1),
       lwd = c(5,5))
#########################################################
# This is an edit to show how git works
# git diff Klaar\ voor\ R/R\ producten/HP20132017_29082018.R
# git add Klaar\ voor\ R/R\ producten/HP20132017_29082018.R
# git commit -m "Your message"
# git push origin master



