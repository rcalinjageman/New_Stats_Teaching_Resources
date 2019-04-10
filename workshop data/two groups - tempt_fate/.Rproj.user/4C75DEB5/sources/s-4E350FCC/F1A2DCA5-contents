# This script analyzes the tempting fate replication from ManyLabs 2
# Klein, R. A., Vianello, M., Hasselman, F., Adams, B. G., Adams, R. B., Alper, S., … Nosek, 
#   B. A. (2018). Many Labs 2: Investigating Variation in Replicability Across 
#   Samples and Settings. Advances in Methods and Practices in Psychological Science, 1(4), 443–490. 
#   http://doi.org/10.1177/2515245918810225
# The original study is Risen & Gilovich, 2008
# In that study participants were asked to estimate how likely they were to be 
#   called on in class on a scale of 1-10.  Before estimating, participants were asked
#   to imagine they were prepared for class that day or not prepared.
#   The finding is that those imagining being unprepared estimate a higher chance of being
#   called on.  In the original study this was a moderate but uncertain effect 
#   d = 0.39 95% CI[0.03, 0.75].
# In the ManyLabs2 project, the key aspects of the procedure in Risen & Gilovich was repeated.
#   It was found that imagining being unprepared did increase estimated likelihood to be called on
#   Though with a somewhat weaker effect: 0.18 in all participants, 0.22 in just college students
# This analysis script takes public data from ManyLabs2 and then splits it by site and
#   Creates summary estimation plots for each site
# The data was pulled from the OSF page for ManyLabs 2: https://osf.io/8cd4r/
# Specifically, it analyzes this file: Risen_1_study_global_include_all_CLEAN_CASE.csv
# Which was obtained from this zip file: https://osf.io/ag2pd/
# From this subdirectory: Tempting Fate (Risen & Gilovich, 2008)\Risen.1\Global\Data
# Looks like it is also available directly from: https://osf.io/z4m6x/
# The data structure posted to OSF is a bit confusing!  
# This data file seems to be the undergrads only, as reported on p. 461, second para of 2nd column
# Analyzing this data file with a simple t-test of being called on by condition gives:
#  t(4588) = 7.57, p = 4.44e-14
#  and in the text it is reported: t(4597) = 7.57, p = 4.4e-14... soooo...close enough!

# This script uses a function I kludged together to create estimation plots.
# That function in turn uses the flat_violin.R file from the daBest package by Adam Clardige-Chang's lab
# I wrote my own version because daBest uses bootstrapped intervals and I wanted the classic CI

# This script expects subdirectories called "figures" and "databysite" to exist in the project space

# This script throws a bunch of warnings.  
#   First, the estimation plot function throws warnings about the x scale being replaced.
#     That's annoying, and probably due to my ignorance of how to properly use ggplot, but can be ignored
#   Second, the cohen.d.ci function in psych throws a bunch of warnings because it tries
#     to calculate d for *each* column in the dataframe...annoying, but didn't spend time trying
#     to figure out how to specify just one dv of interest.

# Lots of stuff in this script is hard-coded in stupid ways, but I just wanted to get the job done.

# Anyways, this seems to work out ok.


library(ggplot2)
library(magrittr)
library(ggbeeswarm)
library(psych)
library(dplyr)
library(metafor)
source("flat_violin.R")


diffplot <- function(plotdata, g1, g2, conflevel=0.95, xaxislabel="", filename="") {
  #Function to create a different plot
  #Just a start; lots of stuff still hard-coded
  
  # Set the order of the conditions and then split the data into each group
  plotdata$condition <- factor(plotdata$factor, levels = c(g1, g2, "Difference"))
  plotdata$dv = plotdata$variable
  conflevel <-  conflevel + (1-conflevel)/2
  
  #Calculate summary data for each group and the difference between the groups
  #First, group and summarize the raw data
  plotdata <- group_by(plotdata,condition)
  summarydata <- summarize(plotdata,n=n(),m=mean(dv,na.rm=T),s=sd(dv,na.rm=T))
  summarydata <- mutate(summarydata, df = n-1)
  #Now calculate group means, pooledsd *2, totalN, and difference betwen means
  g1mean <-  summarydata$m[1]
  g2mean <-  summarydata$m[2]
  #Note pooled sd is multiplied by 2 here... kind of a kludge, should probably have a group number and use this in the SEM calculations
  pooledsd <-  2*sqrt ( sum(summarydata$s^2 * summarydata$df) / sum(summarydata$df)  )
  totalN <-  sum(summarydata$n)
  diffmean <-  summarydata$m[1] - summarydata$m[2]
  #Now add a row for the difference data
  summarydata <- rbind(summarydata, data.frame(condition = "Difference", n = totalN, m = g2mean, s=pooledsd, df=totalN-2))  
  #Now do some common calculations: SEM, MOE, and boundaries for error bars for the SEM or CI
  summarydata <- mutate(summarydata, sem=s/sqrt(n))
  summarydata <- mutate(summarydata, moe=sem*abs(qt(conflevel,df)))
  summarydata <- mutate(summarydata, semlow=m-sem, semhigh=m+sem, cilow=m-moe,cihigh=m+moe)  
  summarydata$dv = summarydata$m
  summarydata$condition <- factor(summarydata$condition, levels = c(g1, g2, "Difference"))
 
  
  #now make difference data for the estimation plot
  diffdata <-  data.frame(t = rt(10000, totalN-2))
  diffdata$dv <-  (summarydata$sem[3] * diffdata$t) + g2mean
  diffdata$condition <-  "Difference"
  
  #Now set labels
  g1label <- paste(g1, "\nN = ", summarydata$n[1])
  g2label <- paste(g2, "\nN = ", summarydata$n[2])
  xlabels <- c(g1label, g2label, "Difference")
  
  # Now create an estimation plot of the data--one that shows each group and the estimated difference between the groups
  # This is still a bit kludgey/custom to this specific data set, but it is a starting point for a general estimation plot function
  #basic plot
  myplot <- ggplot(data=summarydata, aes(x=condition, y=dv, color=condition, shape=condition, fill=condition))
  myplot <- myplot + xlim(0,3.7)
  #difference axis
  myplot <- myplot + scale_y_continuous(breaks = seq(1, 10, by=1), limits = c(1, 10), sec.axis = sec_axis(trans= ~.-g1mean, breaks=seq(-2, 2, by=1)))   #Difference axis
  #Style options
  myplot <- myplot + theme_classic()  # Classic theme
  myplot <- myplot + scale_color_manual(values = c("black", "black", "black"))  #Outlines
  myplot <- myplot + scale_fill_manual(values = c("white", "gray75", "gray75"))  #Fills
  myplot <- myplot + scale_shape_manual(values=c(21, 21, 17)) #shapes
  myplot <- myplot + theme(legend.position="none")
  #Now build up plot -- CIs first, then raw data, then error distribution
  myplot <- myplot + geom_errorbar(data=summarydata, aes(ymin=summarydata$cilow, ymax = summarydata$cihigh), size=1, width = 0, position=position_nudge(x=c(0.35, 0.35, 0)))
  myplot <- myplot + geom_beeswarm(data=plotdata, aes(x=condition, y=dv, fill=condition), cex=1, size=2)
  myplot <- myplot + geom_flat_violin(data=diffdata, aes(x=condition, y=dv), width=.8,adjust=3,size=0, alpha=.5)
  #Now reference lines
  myplot <- myplot + geom_segment(color = "black", linetype = "dashed", aes(x=1.35, xend=3.7,y = g1mean, yend=g1mean))
  myplot <- myplot + geom_segment(color = "black", linetype = "dotted", aes(x=2.35, xend=3,y = g2mean, yend=g2mean))
  #Now fix up the difference axis
  myplot <- myplot + theme( axis.line.y.right = element_blank())
  myplot <- myplot + geom_segment(color="black", linetype="solid", aes(x=3.7,xend=3.7,y=g1mean-2, yend=g1mean+2))
  #Now means--they go last so they'll show up on top
  myplot <- myplot + geom_point(data=summarydata, size = 3, stroke=1, position= position_nudge(x=c(0.35, 0.35, 0)))
  #Finally, labels and axis scale to match original figure
  myplot <- myplot + ylab("Estimated likelihood of being called on (1-10)") + xlab(xaxislabel)
  myplot <- myplot + scale_x_discrete(labels = xlabels)
  #show the plot in RStudio then save
  myplot
  
  if(filename != "") {
    ggsave(filename, width=5, height=7, units="in")
  }
  
  
  myplot
  
}


#Read in the tempting fate data set from ManyLabs2 and then conduct overall t-test
  tfate <- read.csv("Risen_1_study_global_include_all_CLEAN_CASE.csv")
  #This t-test mostly matches the text for the analysis in college students only
  #But the df are slightly different...oh well for now
  t.test(variable ~ factor, data=tfate)
  
#Prep an object to store results from each site
  sitecount = length(levels(tfate$source))
  res <- vector("list", sitecount+1)

#Store result from overall t-test
  res[[1]] <- t.test(variable ~ factor, data=tfate)
  names(res) <- c("All")

#Now cycle through each site, storing t-test and saving individual data file
  step = 2
  i = levels(tfate$source)[[1]]
  
  for (i in levels(tfate$source)) {
    #get data for that site only
    tsite <- tfate[tfate$source == i, ]

    #conduct and store a t-test for that site; store the name of the site 
    res[[step]] = t.test(variable ~ factor, data=tsite)
    names(res)[[step]] <- i

    #print the site name and p value
    print(i)
    print(res[[step]]$p.value)
  
    #get cohen's d and its 95% CI, then make a label for the graph with site name, d, and p value
    sited <- cohen.d(tsite, "factor")$cohen.d["variable", ]
    dtext <- paste("d = ", round(sited["effect"],2), " 95% CI [", round(sited["lower"],2), ", ", round(sited["upper"],2), "]")
    sxaxislabel<- paste("Site:", i, "\n", dtext, "\np = ", round(res[[step]]$p.value,3))
    gfilename <- paste("figures/tfate - ", i, ".png")
    
    #Now create a difference plot
    siteplot <- diffplot(tsite, "Prepared", "Unprepared", xaxislabel=sxaxislabel, filename = gfilename)
    siteplot
    
    #Write the data for each site
    fname <- paste("databysite/tfate - ", i, ".csv")
    write.csv(tsite, fname)
    
    #finally, increment this kludgey counter
    step = step + 1
    

  }




