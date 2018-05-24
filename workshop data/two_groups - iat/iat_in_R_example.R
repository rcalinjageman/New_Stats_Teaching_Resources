# Example of a two-independent groups analysis in R

# This script is just a demo of the New Stats approach to analyzing the difference between two ind. groups
# This script is designed to analyze data from the ManyLabs 1 gender/math IAT
# The user will be prompted to select a file; select one of the IAT data files prepped for the workshop


#First, make sure required packages are installed.
if (!is.element("multicon", installed.packages()[,1]))  install.packages("multicon", dep = TRUE)
if (!is.element("MBESS", installed.packages()[,1])) install.packages("MBESS", dep = TRUE)

#Next load the required libraries
library("MBESS")
library("multicon")

#Next, open the file with the IAT data.  We'll let the user decide what lab to analyze
filename = file.choose(new = FALSE)
cdata = read.csv(filename, header=TRUE, sep=",")

#set the order for the sex factor; this is done just to make the plot come out males then famales
cdata$sex = factor(cdata$sex, levels= c("m", "f"))


#do a basic t-test, then obtain cohen's d
result = t.test(cdata[cdata$sex == "f", ]$d_art, cdata[cdata$sex == "m", ]$d_art)
male_n = nrow(cdata[cdata$sex == "m", ])
female_n = nrow(cdata[cdata$sex == "f", ])
cohend = ci.smd(result$statistic, n.1=female_n, n.2=male_n, conf.level = 0.95)
result
cohend

#The rest of this is just to make nice labels for a graph
  means = result$estimate
  malel = paste("Males: M=", format(means[2],digits=2), " N=", male_n)
  femalel = paste("Females: M=", format(means[1], digits=2), "N=", female_n)
  difference = means[1] - means[2]
  ci = result$conf.int
  cilabel  = paste("Mdiff=", format(difference, digits=2), " CI[", format(ci[1], digits=2), ",", format(ci[2], digits=2), "]")
  cohendlabel = paste("d=", format(cohend$smd, digits = 2), "CI[", format(cohend$Lower.Conf.Limit.smd, digits = 2), ", ", format(cohend$Upper.Conf.Limit.smd, digits = 2), "]")
  overall_label = paste(cilabel, " ", cohendlabel)
  lab = trimws(sub(".csv", "", basename(filename)))

plot = diffPlot(cdata$sex, cdata$d_art, paired=FALSE, conf=0.95, sub=overall_label, grp.names = c(malel, femalel), main=paste("Manylabs IAT data: ", lab), ylab="IAT Score: Negative Math Association")
