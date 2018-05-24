#Script to read the IAT data from the ManyLabs 1 project
# and then produce a difference plot for each lab

#Load libraries
library(foreign)
library(multicon)
library(MBESS)
library(metafor)

#Get the data from the Many Labs 1 OSF site
#download.file("https://osf.io/nqg97/download", "Datasets.zip", cacheOK = FALSE)
#unzip(zipfile = "Datasets.zip", file="Data/CleanedDataset.sav", overwrite = TRUE)

#Read the data, get rid of NAs, trim whitespace
iat = read.spss("Data/CleanedDataset.sav", to.data.frame = TRUE)
levels(iat$referrer) = trimws(levels(iat$referrer))
levels(iat$sex) = trimws(levels(iat$sex))
levels(iat$iat_exclude) = trimws(levels(iat$iat_exclude))
iat = iat[!is.na(iat$d_art), ]
iat = iat[iat$sex == "f" | iat$sex == "m", ]
keeps = c("session_id", "referrer", "sex", "d_art", "iat_exclude")
iat = iat[keeps]

#Set order so it is males, then females
iat$sex = factor(iat$sex, levels= c("m", "f"))

#Some participants are marked exclude for the IAT experiment; delete those.
iat = iat[iat$iat_exclude %in% "Include", ]

labresults = data.frame(lab=factor(), 
                        m_males=double(),
                        s_males=double(),
                        n_males=double(),
                        m_females=double(),
                        s_females=double(),
                        n_females=double(),
                        sdifference=double(),
                        scilow=double(),
                        scihigh=double(),
                        sd=double(),
                        sdcilow=double(),
                        sdcihigh=double(),
                        odifference=double(),
                        od=double()
)


#make a plot for each location and also save a csv file with the data
for (lab in levels(iat$referrer)) {
  print(lab)
  
  #reduce down to just the data for the lab, then save a csv file of it
  cdata = iat[iat$referrer == lab, ]
  cdata = cdata[!is.na(cdata$d_art), ]
  write.table(cdata, file = paste("/iat_data_csv/", trimws(lab), ".csv"), sep=",", col.names = TRUE, row.names=FALSE)
  
  #now get the data that is *not* from that lab and do an overall t-test (a meta-analysis is probably better but probably not that different)
  odata = iat[iat$referrer != lab, ]
  odata = odata[!is.na(odata$d_art), ]
  oresult = t.test(odata[odata$sex == "f", ]$d_art, odata[odata$sex == "m", ]$d_art)
  odifference = oresult$estimate[1] - oresult$estimate[2]
  
      matable = data.frame(
        lab=factor(),
        m1i=double(),
        sd1i=double(),
        n1i=integer(),
        m2i=double(),
        sd2i=double(),
        n2i=integer()
      )
      #Now collect all the descriptive site-by-site to do the leave-one-out meta-analysis
        for (olab in levels(odata$referrer)) {
          om = odata[odata$referrer == olab & odata$sex == "m", ]
          om = om[!is.na(om$d_art), ]   #why would I need to keep doing this?
          of = odata[odata$referrer == olab & odata$sex == "f" & !is.na(odata$d_art), ]
          of = of[!is.na(of$d_art), ]   #again, makes no sense that this might be needed
          if(olab != lab) {
            matable = rbind(matable, data.frame(lab=olab,m1i=mean(of$d_art), sd1i=sd(of$d_art), n1i=nrow(of), m2i=mean(om$d_art), sd2i=sd(om$d_art), n2i=nrow(om) ) )
          }
        }
      matable = escalc(measure="SMD", m1i=m1i, sd1i=sd1i, n1i = n1i, m2i = m2i, sd2i = sd2i, n2i = n2i, data=matable)
      maresult = rma(yi, vi, data=matable, knha=TRUE)  
      doverall = maresult$b[1]
  
  #do a basic t-test, format the results for adding to the graph
  result = t.test(cdata[cdata$sex == "f", ]$d_art, cdata[cdata$sex == "m", ]$d_art)
  means = result$estimate
  malel = paste("Males: ", format(means[2],digits=2))
  femalel = paste("Females: ", format(means[1], digits=2))
  male_n = nrow(cdata[cdata$sex == "m" & !is.na(cdata$d_art), ])
  female_n = nrow(cdata[cdata$sex == "f" & !is.na(cdata$d_art), ])
  male_s = sd(cdata[cdata$sex == "m", ]$d_art, na.rm=TRUE)
  female_s = sd(cdata[cdata$sex == "f", ]$d_art, na.rm=TRUE)
  difference = means[1] - means[2]
  ci = result$conf.int
  cilabel  = paste("Mdiff=", format(difference, digits=2), " CI[", format(ci[1], digits=2), ",", format(ci[2], digits=2), "]")
  
  #check to see if the CI in this sample covers the mean difference found across all the other data
  if (ci[1] < odifference & ci[2] > odifference)  {
    cilabel = paste(cilabel, "*")
  }
  cilabel = paste(cilabel, " (", format(odifference, digits=2), ")")
  

  #obtain cohen's d and it's ci
  cohend = ci.smd(result$statistic, n.1=female_n, n.2=male_n, conf.level = 0.95)
  cohendlabel = paste("d=", format(cohend$smd, digits = 2), "CI[", format(cohend$Lower.Conf.Limit.smd, digits = 2), ", ", format(cohend$Upper.Conf.Limit.smd, digits = 2), "]")
  
  #ManyLabs reports an overall effect size of 0.53; see if this falls within the CI for this sample
  if (cohend$Lower.Conf.Limit.smd < doverall & cohend$Upper.Conf.Limit.smd > doverall) {
    cohendlabel = paste(cohendlabel, "*")
  }
  cohendlabel = paste(cohendlabel, " (", format(doverall, digits=2), ")")
  
  overall_label = paste(cilabel, " ", cohendlabel)
  
  labresults = rbind(labresults, data.frame(lab=lab, 
                                            m_males=means[2], 
                                            s_males=male_s, 
                                            n_males=male_n, 
                                            m_females=means[1], 
                                            s_females=female_s, 
                                            n_females=female_n, 
                                            sdifference=difference, 
                                            scilow = ci[1], 
                                            scihigh = ci[2], 
                                            sd = cohend$smd, 
                                            sdcilow = cohend$Lower.Conf.Limit.smd, 
                                            sdcihigh = cohend$Upper.Conf.Limit.smd, 
                                            odifference=odifference, 
                                            od=doverall)
                     )
  
  plot = diffPlot(cdata$sex, cdata$d_art, paired=FALSE, conf=0.95, sub=overall_label, grp.names = c(malel, femalel), main=paste("Manylabs IAT data: ", lab), ylab="IAT Score: Negative Math Association")
  dev.copy(png, filename=paste("output/", lab, ".png"))
  
}

labresults$capture = FALSE
labresults[labresults$sdcilow < labresults$od & labresults$sdcihigh > labresults$od, ]$capture = TRUE

capture = nrow(labresults[labresults$capture == TRUE, ])
tstudies = nrow(labresults)

paste("Captured: ", capture, " out of", tstudies, " for a capture rate of ", format(capture/tstudies, digits=2))

labresults$repcapturerate = 0

for (lab in levels(labresults$lab)) {
  replications_captured = 0
  total_replications = 0
  scilow = labresults[labresults$lab == lab, ]$scilow
  scihigh = labresults[labresults$lab == lab, ]$scihigh
    for (rep in levels(labresults$lab)) {
      if (lab != rep) {
        total_replications = total_replications + 1
        repdif = labresults[labresults$lab == rep, ]$sdifference
        if (repdif > scilow & repdif < scihigh) {
          replications_captured = replications_captured + 1
        }
      }
      
    }
  labresults[labresults$lab == lab, ]$repcapturerate = replications_captured/total_replications
  print(paste(lab, " captured other replications: ", replications_captured, " out of ", total_replications, " for a rate of ", format(replications_captured/total_replications, digits=2)))
}

print(paste("overall capture rate = ", format(mean(labresults$repcapturerate, digits = 2))))

write.table(labresults, file = "iat_summary_data.csv", sep=",", col.names = TRUE, row.names=FALSE)
