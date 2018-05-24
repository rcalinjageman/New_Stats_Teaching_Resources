#Analyze the power and perspective-taking data and save plots
source("../proportions.R")

#This analyzes from summary data
estimate.diffproportions(name1 = "High Power", r1 = 8, n1 = 24, name2 = "Low Power", r2=4, n2=33, measure="Proportion Ego-Centric", plot=TRUE, main="Galinsky et al., 2006")
dev.copy(png, filename=paste("output/Galinskey", ".png"))
dev.off()
estimate.diffproportions(name1 = "High Power", r1 = 17, n1 = 80, name2 = "Low Power", r2=19, n2=71, measure="Proportion Ego-Centric", plot=TRUE, main="In-Person Replication")
dev.copy(png, filename=paste("output/InPerson", ".png"))
dev.off()
estimate.diffproportions(name1 = "High Power", r1 = 48, n1 = 105, name2 = "Low Power", r2=33, n2=101, measure="Proportion Ego-Centric", plot=TRUE, main="MTurk Replication")
dev.copy(png, filename=paste("output/MTurk", ".png"))
dev.off()
estimate.diffproportions(name1 = "High Power", r1 = 44, n1 = 122, name2 = "Low Power", r2=44, n2=133, measure="Proportion Ego-Centric", plot=TRUE, main="Prolific Academic Replication")
dev.copy(png, filename=paste("output/ProlificAcademic", ".png"))
dev.off()
estimate.diffproportions(name1 = "High Power", r1 = 57, n1 = 78, name2 = "Low Power", r2=46, n2=79, measure="Proportion Ego-Centric", plot=TRUE, main="Blader et al., 2016, Study 4")
dev.copy(png, filename=paste("output/Blader", ".png"))
dev.off()
estimate.diffproportions(name1 = "High Power", r1 = 18, n1 = 71, name2 = "Low Power", r2=22, n2=70, measure="Proportion Ego-Centric", plot=TRUE, main="Laurent & Lester, unpublished replication")
dev.copy(png, filename=paste("output/Lester", ".png"))
dev.off()

#This is how you would do it from raw data
pdata = read.csv("../iat_data_csv/mturk.csv", header=TRUE)
estimate.diffproportions.rawdata(pdata, "Condition", "Response", 1, main="MTurk")
