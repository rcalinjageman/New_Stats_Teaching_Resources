#This is an example of analyzing two independent proprtions with R with a New Stats approach
#The example is power and perspective-taking data from Galinsky et al and several direct and/or conceptual replications

# This source file has 3 helper functions that do all the calculaitons
source("proportions.R")


#let the user select a file
filename = file.choose(new = FALSE)

#Read the file and also record the base name of the file
pdata = read.csv(filename, header=TRUE, sep=",")
lab = trimws(sub(".csv", "", basename(filename)))

#calculate the difference between two proportions
estimate.diffproportions.rawdata(pdata, "Condition", "Response", 1, main=lab)
