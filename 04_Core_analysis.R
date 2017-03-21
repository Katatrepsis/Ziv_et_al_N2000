############################################################################
### Purpose of this script module 08 is to:
###
### 08.1 Clean the N2000 impact data
### 08.2 Subsetting data 
### 08.3 Associations of services
### 08.4 Bar plots of services by biogeographical region
### 08.5 ESS displayed by core habitat
### 08.6 Site-centred conservation and IUCN indices
### 08.7 Site-centred conservation and IUCN indices by Biogeographical regions
###
### Authors: CH, AC, MB, AK
###
### Run script modules 00, 01, and 02 before
############################################################################

############################################################################
### 08.1 Clean the N2000 impact data
###
### Create N2000Impact data frame as a base for further data analysis, and
### fix a large number of typos and missing data
############################################################################

# set wd to path2temp where files have been downloaded and extracted
setwd(path2temp %+% "/") 

# Load data
N2000Impact<-read.csv("IMPACT.csv")

#### DATA CLEANING ####
# Change column name from "ï..SITECODE" to "SITECODE"
colnames(N2000Impact)[1] <- "SITECODE"

# Convert lower case to upper case
N2000Impact$IMPACTCODE<-gsub("j", "J", N2000Impact$IMPACTCODE)
N2000Impact$IMPACTCODE<-gsub("k", "K", N2000Impact$IMPACTCODE)

# Convert letter "O" to number "0"
N2000Impact$IMPACTCODE<-gsub("O", "0", N2000Impact$IMPACTCODE)

# Replace comma with period
N2000Impact$IMPACTCODE<-gsub(",", ".", N2000Impact$IMPACTCODE)

# Remove spaces
N2000Impact$IMPACTCODE<-gsub(" ", "", N2000Impact$IMPACTCODE)

# Some impact codes had a period as the final character, which is also invalid
for(x in 1:nrow(N2000Impact)){
  if(substr(N2000Impact$IMPACTCODE[x],nchar(N2000Impact$IMPACTCODE[x]),nchar(N2000Impact$IMPACTCODE[x]))==".")
  {N2000Impact$IMPACTCODE[x]<-substr(N2000Impact$IMPACTCODE[x],1,nchar(N2000Impact$IMPACTCODE[x])-1)}
}

# Remove codes that do not exist in definitions, i.e. beginning with 0, 6, 8, O and P (n=102)
FirstChar<-substr(N2000Impact$IMPACTCODE,1,1)
N2000Impact<-subset(N2000Impact,is.na(match(FirstChar,c("0","6", "8", "O","P"))))

# Remove NULL impact codes (n=5494)
N2000Impact<-subset(N2000Impact,N2000Impact$IMPACTCODE!="NULL")

# And some very specific mistakes
N2000Impact<-subset(N2000Impact,N2000Impact$IMPACTCODE!="D014.01") # Not possible to establish whether D01.01 or D04.01, so delete
N2000Impact$IMPACTCODE[which(N2000Impact$IMPACTCODE=="D2.01")]<-"D02.01" 
N2000Impact$IMPACTCODE[which(N2000Impact$IMPACTCODE=="F.03.01.01")]<-"F03.01.01" 
N2000Impact$IMPACTCODE[which(N2000Impact$IMPACTCODE=="K.02.01")]<-"K02.01" 
N2000Impact$IMPACTCODE[which(N2000Impact$IMPACTCODE=="G.01.04.03")]<-"G01.04.03" 
N2000Impact$IMPACTCODE[which(N2000Impact$IMPACTCODE=="F3.01.01")]<-"F03.01.01" 
N2000Impact$IMPACTCODE[which(N2000Impact$IMPACTCODE=="C3.03")]<-"C03.03"

# Remove duplicate lines
N2000Impact<-N2000Impact[!duplicated(N2000Impact),]

# Convert to factor
N2000Impact$IMPACTCODE<-as.factor(N2000Impact$IMPACTCODE)


###############################################
### 08.2 Subsetting data 
### Just working with subset of N2000Impact with the following characteristics:
###     (i) SITE_TYPE = A or C (SPA sites only)
###     (ii) INTENSITY = MEDIUM or HIGH
###     (iii) OCCURRENCE = IN or BOTH
###############################################

# First, subset N2000Impact by intensity and occurrence
N2000Impact<-subset(N2000Impact,N2000Impact$INTENSITY=="MEDIUM" | N2000Impact$INTENSITY=="HIGH")
N2000Impact<-subset(N2000Impact,N2000Impact$OCCURRENCE=="IN" | N2000Impact$OCCURRENCE=="BOTH")

# Assign site type
# Load data
N2000Sites <- read.csv("NATURA2000SITES.csv")
N2000Sites[,4] <- as.character(N2000Sites[,4])
N2000Impact$SITE_TYPE <- NA

# Add site type to N2000Impact (running time 117 seconds)
for(x in 1:nrow(N2000Impact)){
  N2000Impact$SITE_TYPE[x] <- N2000Sites[match(N2000Impact$SITECODE[x],N2000Sites$SITECODE),4]
}

# Now subset to exclude SITE_TYPE="B"
N2000Impact<-subset(N2000Impact,N2000Impact$SITE_TYPE %in% c("A","C"))


############################################################################
### 08.3 Associations of services
###
### Association the threats from the N2000Impact data with the mapped
### ecosystem services from the Google Doc
############################################################################

# Bind ES mapping to the threats table
N2000Impact<-cbind(N2000Impact,MappingData[match(N2000Impact$IMPACTCODE,MappingData$ACT_Code),])

# Create a list of services based on ES mapping 
ServiceList<-names(MappingData[,c(3:11)])

ServiceBySite<-matrix(ncol=length(ServiceList)*4,nrow=length(unique(N2000Impact$SITECODE)))
rownames(ServiceBySite)<-unique(N2000Impact$SITECODE)
colnames(ServiceBySite)<-c(paste(ServiceList,"POS"),paste(ServiceList,"NEG"),paste(ServiceList,"BOTH"),paste(ServiceList,"NET"))

# Run through mapping and tally the positive (in the first 9 columns) and negative (second 9 columns)
# ESS associated with each site. Then calculate the difference between the two to give a net score for each
# ESS on each site
ptm <- proc.time()
for(x in 1:nrow(ServiceBySite)){
  # For each unique site ID code, extract a list of threats that have corresponding services
  SiteServices<-subset(N2000Impact,N2000Impact$SITECODE==rownames(ServiceBySite)[x])
  # For each service group, sum the number of times it was positive or negative
  for(y in 1:length(ServiceList)){
    if(nrow(subset(SiteServices,SiteServices[,10+y] %in% c("c","x") & SiteServices$IMPACT_TYPE=="P"))>0) {ServiceBySite[x,y] <- 1} else {ServiceBySite[x,y] <- 0}
    if(nrow(subset(SiteServices,SiteServices[,10+y] %in% c("c","x") & SiteServices$IMPACT_TYPE=="N"))>0) {ServiceBySite[x,y+9] <- 1} else {ServiceBySite[x,y+9] <- 0}
    if("P"%in%subset(SiteServices,SiteServices[,10+y] %in% c("c","x"))$IMPACT_TYPE & "N"%in%subset(SiteServices,SiteServices[,10+y] %in% c("c","x"))$IMPACT_TYPE) {ServiceBySite[x,y+18]<-1;ServiceBySite[x,y] <- 0;ServiceBySite[x,y+9] <- 0} else {ServiceBySite[x,y+18]<-0}
    ServiceBySite[x,y+27]<-ServiceBySite[x,y]-ServiceBySite[x,y+9]
  }
  # Timer to track progress of loop
  if(x %% 100 == 0) {print(x/nrow(ServiceBySite));flush.console()}
}
proc.time() - ptm

# Final "net" value for all ESS across each site
NetESS<-rowSums(ServiceBySite[,c(28:36)])
NetESSwt<-rowSums(ServiceBySite[,c(28:34)])/7+ServiceBySite[,35]+ServiceBySite[,36]
ServiceBySite<-cbind(ServiceBySite,NetESS,NetESSwt)

# Add Bioregion (note that sometimes the BIOREGION$SITECODE field is called "i..SITECODE" which
# causes problems with matching the datasets - it may depend on operating system)
BIOREGION<-read.csv("BIOREGION.csv")
# Change column name from "ï..SITECODE" to "SITECODE"
colnames(BIOREGION)[1] <- "SITECODE"
ServiceBySite<-data.frame(ServiceBySite,Biogeog=as.factor(BIOREGION[match(rownames(ServiceBySite),BIOREGION$SITECODE),2]))


############################################################################
### 08.4 Bar plots of services by biogeographical region
###
### Creates bar plots of types of ESS by biogeographical region
############################################################################

# 1: STACKED BARS SHOWING PROPORTIONS
png(filename="Figure 2.png")
par(mfrow=c(3,2),mar=c(3,4,3,2))
ESLabels<-c("CR","FD","FB","LS","WF","AQ","WA","RG","RC")
# Figure 2A (all SPAs)
All_BarData<-cbind(colSums(ServiceBySite[,c(1:9)]),colSums(ServiceBySite[,c(19:27)]),colSums(ServiceBySite[,c(10:18)]))
rownames(All_BarData) <- ServiceList
colnames(All_BarData) <- c("POS","BOTH","NEG")
barplot(t(All_BarData/rowSums(All_BarData)),las=2, legend=F, main="(A) All SPAs",names.arg=ESLabels)

# Figure 2B (Atlantic)
Atlantic_ServiceBySite<-subset(ServiceBySite,ServiceBySite$Biogeog=="Atlantic")
Atlantic_BarData<-cbind(colSums(Atlantic_ServiceBySite[,c(1:9)]),colSums(Atlantic_ServiceBySite[,c(19:27)]),colSums(Atlantic_ServiceBySite[,c(10:18)]))
rownames(Atlantic_BarData) <- ServiceList
colnames(Atlantic_BarData) <- c("POS","BOTH","NEG")
barplot(t(Atlantic_BarData/rowSums(Atlantic_BarData)),las=2, legend=F, main="(B) Atlantic SPAs",names.arg=ESLabels)

# Figure 2C (Continental)
Continental_ServiceBySite<-subset(ServiceBySite,ServiceBySite$Biogeog=="Continental")
Continental_BarData<-cbind(colSums(Continental_ServiceBySite[,c(1:9)]),colSums(Continental_ServiceBySite[,c(19:27)]),colSums(Continental_ServiceBySite[,c(10:18)]))
rownames(Continental_BarData) <- ServiceList
colnames(Continental_BarData) <- c("POS","BOTH","NEG")
barplot(t(Continental_BarData/rowSums(Continental_BarData)),las=2, legend=F, main="(C) Continental SPAs",names.arg=ESLabels)

# Figure 2D (Mediterranean)
Mediterranean_ServiceBySite<-subset(ServiceBySite,ServiceBySite$Biogeog=="Mediterranean")
Mediterranean_BarData<-cbind(colSums(Mediterranean_ServiceBySite[,c(1:9)]),colSums(Mediterranean_ServiceBySite[,c(19:27)]),colSums(Mediterranean_ServiceBySite[,c(10:18)]))
rownames(Mediterranean_BarData) <- ServiceList
colnames(Mediterranean_BarData) <- c("POS","BOTH","NEG")
barplot(t(Mediterranean_BarData/rowSums(Mediterranean_BarData)),las=2, legend=F, main="(D) Mediterranean SPAs",names.arg=ESLabels)

# Figure 2E (Boreal)
Boreal_ServiceBySite<-subset(ServiceBySite,ServiceBySite$Biogeog=="Boreal")
Boreal_BarData<-cbind(colSums(Boreal_ServiceBySite[,c(1:9)]),colSums(Boreal_ServiceBySite[,c(19:27)]),colSums(Boreal_ServiceBySite[,c(10:18)]))
rownames(Boreal_BarData) <- ServiceList
colnames(Boreal_BarData) <- c("POS","BOTH","NEG")
barplot(t(Boreal_BarData/rowSums(Boreal_BarData)),las=2, legend=F, main="(E) Boreal SPAs",names.arg=ESLabels)

# Figure 2F (Alpine)
Alpine_ServiceBySite<-subset(ServiceBySite,ServiceBySite$Biogeog=="Alpine")
Alpine_BarData<-cbind(colSums(Alpine_ServiceBySite[,c(1:9)]),colSums(Alpine_ServiceBySite[,c(19:27)]),colSums(Alpine_ServiceBySite[,c(10:18)]))
rownames(Alpine_BarData) <- ServiceList
colnames(Alpine_BarData) <- c("POS","BOTH","NEG")
barplot(t(Alpine_BarData/rowSums(Alpine_BarData)),las=2, legend=F, main="(F) Alpine SPAs",names.arg=ESLabels)

dev.off()

# Export data
BiogeogOrigin<-cbind(rbind(All_BarData,Atlantic_BarData,Continental_BarData,Mediterranean_BarData,Boreal_BarData,Alpine_BarData),
      rep(c("All","Atlantic","Continental","Mediterranean","Boreal","Alpine"),each=9))
write.table(BiogeogOrigin,"BiogeogOrigin.txt")

############################################################################
### 08.5 ESS displayed by core habitat
############################################################################

# Load HABITATCLASS
HABITATCLASS<-read.csv("HABITATCLASS.csv")
# Change column name from "ï..SITECODE" to "SITECODE"
colnames(HABITATCLASS)[1] <- "SITECODE"
# Convert % cover to a numeric variable
HABITATCLASS$PERCENTAGECOVER<-as.numeric(as.vector(HABITATCLASS$PERCENTAGECOVER))
# Extract a subset where the % cover is >=50%
HABITATCLASS<-subset(HABITATCLASS,HABITATCLASS$PERCENTAGECOVER>=50)
# Add that dominant habitat to the main table
ServiceBySite<-cbind(ServiceBySite,DomHab=HABITATCLASS$HABITATCODE[match(rownames(ServiceBySite),HABITATCLASS$SITECODE)])


# Subset the data to just include those sites with a dominant class
DomHabData<-subset(ServiceBySite,ServiceBySite$DomHab%in%names(which(table(ServiceBySite$DomHab)>30)))
# Create a matrix where any mention of a service is counted
AnyMention<-matrix(nrow=nrow(DomHabData),ncol=9)
for(x in 1:nrow(DomHabData)){
  for(y in 1:9){
    if(sum(DomHabData[x,c(y,y+9,y+18)])>0) {AnyMention[x,y]<-1} else {AnyMention[x,y]<-0}
  }
}

# Find the proportions of positive and negative mentions of each ES
PosNegDF<-cbind(colSums(ServiceBySite[,c(1:9)]),
      colSums(ServiceBySite[,c(1:9)])/(colSums(ServiceBySite[,c(1:9)])+colSums(ServiceBySite[,c(10:18)])),
      colSums(ServiceBySite[,c(10:18)]),
      colSums(ServiceBySite[,c(10:18)])/(colSums(ServiceBySite[,c(1:9)])+colSums(ServiceBySite[,c(10:18)])),
      colSums(ServiceBySite[,c(1:9)])+colSums(ServiceBySite[,c(10:18)]))
colnames(PosNegDF)<-c("Pos","Prop Pos","Neg","Prop Neg","Total")

# Find the average number of ESS per dominant habitat
aggregate(rowSums(AnyMention), list(DomHabData$DomHab), mean)

# Find the proportion of SPAs in each dominant habitat that mention each ESS
xtabs(DomHabData$DomHab~AnyMention[,1])
ESSByHab<-aggregate(AnyMention, list(DomHabData$DomHab), mean)

# Tables of each habitat, with positive, negative, and both
# First, N01
N01_Data<-subset(DomHabData,DomHabData$DomHab=="N01")
N02_Data<-subset(DomHabData,DomHabData$DomHab=="N02")
N05_Data<-subset(DomHabData,DomHabData$DomHab=="N05")
N06_Data<-subset(DomHabData,DomHabData$DomHab=="N06")
N07_Data<-subset(DomHabData,DomHabData$DomHab=="N07")
N08_Data<-subset(DomHabData,DomHabData$DomHab=="N08")
N10_Data<-subset(DomHabData,DomHabData$DomHab=="N10")
N12_Data<-subset(DomHabData,DomHabData$DomHab=="N12")
N14_Data<-subset(DomHabData,DomHabData$DomHab=="N14")
N15_Data<-subset(DomHabData,DomHabData$DomHab=="N15")
N16_Data<-subset(DomHabData,DomHabData$DomHab=="N16")
N17_Data<-subset(DomHabData,DomHabData$DomHab=="N17")
N19_Data<-subset(DomHabData,DomHabData$DomHab=="N19")
N23_Data<-subset(DomHabData,DomHabData$DomHab=="N23")

# Find proportions
N01_ESS<-data.frame(Pos=t(unname(aggregate(N01_Data[,c(1:9)], list(N01_Data$DomHab), mean))),
                   Neg=t(unname(aggregate(N01_Data[,c(10:18)], list(N01_Data$DomHab), mean))),
                   Both=t(unname(aggregate(N01_Data[,c(19:27)], list(N01_Data$DomHab), mean))))[-1,]
N02_ESS<-data.frame(Pos=t(unname(aggregate(N02_Data[,c(1:9)], list(N02_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N02_Data[,c(10:18)], list(N02_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N02_Data[,c(19:27)], list(N02_Data$DomHab), mean))))[-1,]
N05_ESS<-data.frame(Pos=t(unname(aggregate(N05_Data[,c(1:9)], list(N05_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N05_Data[,c(10:18)], list(N05_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N05_Data[,c(19:27)], list(N05_Data$DomHab), mean))))[-1,]
N06_ESS<-data.frame(Pos=t(unname(aggregate(N06_Data[,c(1:9)], list(N06_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N06_Data[,c(10:18)], list(N06_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N06_Data[,c(19:27)], list(N06_Data$DomHab), mean))))[-1,]
N07_ESS<-data.frame(Pos=t(unname(aggregate(N07_Data[,c(1:9)], list(N07_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N07_Data[,c(10:18)], list(N07_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N07_Data[,c(19:27)], list(N07_Data$DomHab), mean))))[-1,]
N08_ESS<-data.frame(Pos=t(unname(aggregate(N08_Data[,c(1:9)], list(N08_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N08_Data[,c(10:18)], list(N08_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N08_Data[,c(19:27)], list(N08_Data$DomHab), mean))))[-1,]
N10_ESS<-data.frame(Pos=t(unname(aggregate(N10_Data[,c(1:9)], list(N10_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N10_Data[,c(10:18)], list(N10_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N10_Data[,c(19:27)], list(N10_Data$DomHab), mean))))[-1,]
N12_ESS<-data.frame(Pos=t(unname(aggregate(N12_Data[,c(1:9)], list(N12_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N12_Data[,c(10:18)], list(N12_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N12_Data[,c(19:27)], list(N12_Data$DomHab), mean))))[-1,]
N14_ESS<-data.frame(Pos=t(unname(aggregate(N14_Data[,c(1:9)], list(N14_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N14_Data[,c(10:18)], list(N14_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N14_Data[,c(19:27)], list(N14_Data$DomHab), mean))))[-1,]
N15_ESS<-data.frame(Pos=t(unname(aggregate(N15_Data[,c(1:9)], list(N15_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N15_Data[,c(10:18)], list(N15_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N15_Data[,c(19:27)], list(N15_Data$DomHab), mean))))[-1,]
N16_ESS<-data.frame(Pos=t(unname(aggregate(N16_Data[,c(1:9)], list(N16_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N16_Data[,c(10:18)], list(N16_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N16_Data[,c(19:27)], list(N16_Data$DomHab), mean))))[-1,]
N17_ESS<-data.frame(Pos=t(unname(aggregate(N17_Data[,c(1:9)], list(N17_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N17_Data[,c(10:18)], list(N17_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N17_Data[,c(19:27)], list(N17_Data$DomHab), mean))))[-1,]
N19_ESS<-data.frame(Pos=t(unname(aggregate(N19_Data[,c(1:9)], list(N19_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N19_Data[,c(10:18)], list(N19_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N19_Data[,c(19:27)], list(N19_Data$DomHab), mean))))[-1,]
N23_ESS<-data.frame(Pos=t(unname(aggregate(N23_Data[,c(1:9)], list(N23_Data$DomHab), mean))),
                    Neg=t(unname(aggregate(N23_Data[,c(10:18)], list(N23_Data$DomHab), mean))),
                    Both=t(unname(aggregate(N23_Data[,c(19:27)], list(N23_Data$DomHab), mean))))[-1,]

# Convert to numeric
N01_ESS<-transform(N01_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N02_ESS<-transform(N02_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N05_ESS<-transform(N05_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N06_ESS<-transform(N06_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N07_ESS<-transform(N07_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N08_ESS<-transform(N08_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N10_ESS<-transform(N10_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N12_ESS<-transform(N12_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N14_ESS<-transform(N14_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N15_ESS<-transform(N15_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N16_ESS<-transform(N16_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N17_ESS<-transform(N17_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N19_ESS<-transform(N19_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))
N23_ESS<-transform(N23_ESS, Pos = as.numeric(as.vector(Pos)),Neg = as.numeric(as.vector(Neg)),Both = as.numeric(as.vector(Both)))

# Plot barcharts for each ESS across the different core habitats
AllESS<-as.data.frame(rbind(N01_ESS,N02_ESS,N05_ESS,N06_ESS,N07_ESS,N08_ESS,N10_ESS,N12_ESS,N14_ESS,N15_ESS,N16_ESS,N17_ESS,N19_ESS,N23_ESS))
AllESS$ESS<-rep(ServiceList,14)
Habitats<-c("N01","N02","N05","N06","N07","N08","N10","N12","N14","N15","N16","N17","N19","N23")
AllESS$Habitat<-rep(Habitats,each=9)
AllESS<-AllESS[,c(1,3,2,4,5)]

OnlyMarineESS<-OnlyWaterESS<-OnlyHeathGrassESS<-OnlyFarmESS<-OnlyWoodESS<-OnlyOtherESS<-AllESS

OnlyMarineESS[OnlyMarineESS$Habitat%in%c("N06","N07","N08","N10","N12","N14","N15","N16","N17","N19","N23"),c(1:3)]<-0
OnlyWaterESS[OnlyWaterESS$Habitat%in%c("N01","N02","N05","N08","N10","N12","N14","N15","N16","N17","N19","N23"),c(1:3)]<-0
OnlyHeathGrassESS[OnlyHeathGrassESS$Habitat%in%c("N01","N02","N05","N06","N07","N12","N14","N15","N16","N17","N19","N23"),c(1:3)]<-0
OnlyFarmESS[OnlyFarmESS$Habitat%in%c("N01","N02","N05","N06","N07","N08","N10","N16","N17","N19","N23"),c(1:3)]<-0
OnlyWoodESS[OnlyWoodESS$Habitat%in%c("N01","N02","N05","N06","N07","N08","N10","N12","N14","N15","N23"),c(1:3)]<-0
OnlyOtherESS[OnlyOtherESS$Habitat%in%c("N01","N02","N05","N06","N07","N08","N10","N12","N14","N15","N16","N17","N19"),c(1:3)]<-0

png(filename="Figure 3.png")
par(mfrow=c(3,3))

barplot(t(as.matrix(subset(AllESS,AllESS$ESS=="Aquaculture")))[c(1:3),rev(c(1:14))],main="Aquaculture",names.arg=rev(Habitats),horiz=TRUE,las=1,xlim=c(0,1))
barplot(t(as.matrix(subset(OnlyMarineESS,OnlyMarineESS$ESS=="Aquaculture")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("darkblue","blue","lightblue"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyWaterESS,OnlyWaterESS$ESS=="Aquaculture")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("darkblue","blue","lightblue"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyHeathGrassESS,OnlyHeathGrassESS$ESS=="Aquaculture")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("mediumpurple","mediumorchid4","violet"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyFarmESS,OnlyFarmESS$ESS=="Aquaculture")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("goldenrod4","goldenrod3","goldenrod2"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyWoodESS,OnlyWoodESS$ESS=="Aquaculture")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("darkgreen","green","lightgreen"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)

barplot(t(as.matrix(subset(AllESS,AllESS$ESS=="Crop")))[c(1:3),rev(c(1:14))],main="Crop",names.arg=rev(Habitats),horiz=TRUE,las=1,xlim=c(0,1))
barplot(t(as.matrix(subset(OnlyMarineESS,OnlyMarineESS$ESS=="Crop")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("darkblue","blue","lightblue"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyWaterESS,OnlyWaterESS$ESS=="Crop")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("darkblue","blue","lightblue"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyHeathGrassESS,OnlyHeathGrassESS$ESS=="Crop")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("mediumpurple","mediumorchid4","violet"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyFarmESS,OnlyFarmESS$ESS=="Crop")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("goldenrod4","goldenrod3","goldenrod2"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyWoodESS,OnlyWoodESS$ESS=="Crop")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("darkgreen","green","lightgreen"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)

barplot(t(as.matrix(subset(AllESS,AllESS$ESS=="Fibre")))[c(1:3),rev(c(1:14))],main="Fibre",names.arg=rev(Habitats),horiz=TRUE,las=1,xlim=c(0,1))
barplot(t(as.matrix(subset(OnlyMarineESS,OnlyMarineESS$ESS=="Fibre")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("darkblue","blue","lightblue"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyWaterESS,OnlyWaterESS$ESS=="Fibre")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("darkblue","blue","lightblue"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyHeathGrassESS,OnlyHeathGrassESS$ESS=="Fibre")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("mediumpurple","mediumorchid4","violet"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyFarmESS,OnlyFarmESS$ESS=="Fibre")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("goldenrod4","goldenrod3","goldenrod2"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyWoodESS,OnlyWoodESS$ESS=="Fibre")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("darkgreen","green","lightgreen"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)

barplot(t(as.matrix(subset(AllESS,AllESS$ESS=="Fodder")))[c(1:3),rev(c(1:14))],main="Fodder",names.arg=rev(Habitats),horiz=TRUE,las=1,xlim=c(0,1))
barplot(t(as.matrix(subset(OnlyMarineESS,OnlyMarineESS$ESS=="Fodder")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("darkblue","blue","lightblue"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyWaterESS,OnlyWaterESS$ESS=="Fodder")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("darkblue","blue","lightblue"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyHeathGrassESS,OnlyHeathGrassESS$ESS=="Fodder")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("mediumpurple","mediumorchid4","violet"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyFarmESS,OnlyFarmESS$ESS=="Fodder")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("goldenrod4","goldenrod3","goldenrod2"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyWoodESS,OnlyWoodESS$ESS=="Fodder")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("darkgreen","green","lightgreen"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)

barplot(t(as.matrix(subset(AllESS,AllESS$ESS=="Livestock")))[c(1:3),rev(c(1:14))],main="Livestock",names.arg=Habitats,horiz=TRUE,las=1,xlim=c(0,1))
barplot(t(as.matrix(subset(OnlyMarineESS,OnlyMarineESS$ESS=="Livestock")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("darkblue","blue","lightblue"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyWaterESS,OnlyWaterESS$ESS=="Livestock")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("darkblue","blue","lightblue"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyHeathGrassESS,OnlyHeathGrassESS$ESS=="Livestock")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("mediumpurple","mediumorchid4","violet"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyFarmESS,OnlyFarmESS$ESS=="Livestock")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("goldenrod4","goldenrod3","goldenrod2"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyWoodESS,OnlyWoodESS$ESS=="Livestock")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("darkgreen","green","lightgreen"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)

barplot(t(as.matrix(subset(AllESS,AllESS$ESS=="Water")))[c(1:3),rev(c(1:14))],main="Water",names.arg=rev(Habitats),horiz=TRUE,las=1,xlim=c(0,1))
barplot(t(as.matrix(subset(OnlyMarineESS,OnlyMarineESS$ESS=="Water")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("darkblue","blue","lightblue"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyWaterESS,OnlyWaterESS$ESS=="Water")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("darkblue","blue","lightblue"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyHeathGrassESS,OnlyHeathGrassESS$ESS=="Water")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("mediumpurple","mediumorchid4","violet"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyFarmESS,OnlyFarmESS$ESS=="Water")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("goldenrod4","goldenrod3","goldenrod2"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyWoodESS,OnlyWoodESS$ESS=="Water")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("darkgreen","green","lightgreen"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)

barplot(t(as.matrix(subset(AllESS,AllESS$ESS=="Wild.food")))[c(1:3),rev(c(1:14))],main="Wild food",names.arg=rev(Habitats),horiz=TRUE,las=1,xlim=c(0,1))
barplot(t(as.matrix(subset(OnlyMarineESS,OnlyMarineESS$ESS=="Wild.food")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("darkblue","blue","lightblue"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyWaterESS,OnlyWaterESS$ESS=="Wild.food")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("darkblue","blue","lightblue"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyHeathGrassESS,OnlyHeathGrassESS$ESS=="Wild.food")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("mediumpurple","mediumorchid4","violet"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyFarmESS,OnlyFarmESS$ESS=="Wild.food")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("goldenrod4","goldenrod3","goldenrod2"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyWoodESS,OnlyWoodESS$ESS=="Wild.food")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("darkgreen","green","lightgreen"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)

barplot(t(as.matrix(subset(AllESS,AllESS$ESS=="Regulating")))[c(1:3),rev(c(1:14))],main="Regulating",names.arg=rev(Habitats),horiz=TRUE,las=1,xlim=c(0,1))
barplot(t(as.matrix(subset(OnlyMarineESS,OnlyMarineESS$ESS=="Regulating")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("darkblue","blue","lightblue"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyWaterESS,OnlyWaterESS$ESS=="Regulating")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("darkblue","blue","lightblue"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyHeathGrassESS,OnlyHeathGrassESS$ESS=="Regulating")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("mediumpurple","mediumorchid4","violet"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyFarmESS,OnlyFarmESS$ESS=="Regulating")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("goldenrod4","goldenrod3","goldenrod2"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyWoodESS,OnlyWoodESS$ESS=="Regulating")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("darkgreen","green","lightgreen"),horiz=TRUE,las=1,xlim=c(0,1),axisnames=FALSE)

barplot(t(as.matrix(subset(AllESS,AllESS$ESS=="Recreation")))[c(1:3),rev(c(1:14))],main="Recreation",names.arg=rev(Habitats),horiz=TRUE,las=1,xlim=c(0,1))
barplot(t(as.matrix(subset(OnlyMarineESS,OnlyMarineESS$ESS=="Recreation")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("darkblue","blue","lightblue"),horiz=TRUE,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyWaterESS,OnlyWaterESS$ESS=="Recreation")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("darkblue","blue","lightblue"),horiz=TRUE,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyHeathGrassESS,OnlyHeathGrassESS$ESS=="Recreation")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("mediumpurple","mediumorchid4","violet"),horiz=TRUE,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyFarmESS,OnlyFarmESS$ESS=="Recreation")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("goldenrod4","goldenrod3","goldenrod2"),horiz=TRUE,xlim=c(0,1),axisnames=FALSE)
barplot(t(as.matrix(subset(OnlyWoodESS,OnlyWoodESS$ESS=="Recreation")))[c(1:3),rev(c(1:14))],add=TRUE,col=c("darkgreen","green","lightgreen"),horiz=TRUE,xlim=c(0,1),axisnames=FALSE)

dev.off()

############################################################################
### 08.6 Site-centred conservation and IUCN indices
###
### This script takes the site as the unit of replication and calculates
### the "conservation index" (the mean habitat quality for the community
### of bird species living at the site) and the "IUCN index" (the mean
### IUCN trend for the species living at the site)
############################################################################

# Convert the A, B, C CONSERVATION code to a numeric score
SPECIES<-read.csv("SPECIES.csv")
ConvertToNumber<-data.frame(Letters=c("A","B","C"),Numbers=c(2,1,0))
SpeciesIndex<-ConvertToNumber[match(SPECIES$CONSERVATION,ConvertToNumber[,1]),2]
SPECIES$SpeciesIndex<-SpeciesIndex

BIRDSPECIES<-subset(SPECIES,SPECIES$SPGROUP=="Birds" & SPECIES$GLOBAL %in% c("A","B","C"))

# Add bird scores to the sites
IUCNIndex<-BirdIndex<-IUCNNumber<-BirdNumber<-numeric(length=nrow(ServiceBySite))
for(x in 1:nrow(ServiceBySite)){
  BirdIndex[x]<-mean(subset(BIRDSPECIES$SpeciesIndex,BIRDSPECIES$SITECODE==rownames(ServiceBySite)[x]))
  BirdNumber[x]<-length(subset(BIRDSPECIES$SpeciesIndex,BIRDSPECIES$SITECODE==rownames(ServiceBySite)[x]))
  IUCNIndex[x]<-mean(c(rep(2,subset(SiteTrends$Inc,SiteTrends$Site==rownames(ServiceBySite)[x])),
                       rep(1,subset(SiteTrends$Stable,SiteTrends$Site==rownames(ServiceBySite)[x])),
                       rep(0,subset(SiteTrends$Dec,SiteTrends$Site==rownames(ServiceBySite)[x]))))
  IUCNNumber[x]<-sum(subset(SiteTrends$Inc,SiteTrends$Site==rownames(ServiceBySite)[x]),
                     subset(SiteTrends$Stable,SiteTrends$Site==rownames(ServiceBySite)[x]),
                     subset(SiteTrends$Dec,SiteTrends$Site==rownames(ServiceBySite)[x]))
  if(x%%100==0) {print(x)}
  flush.console()
}

SiteData<-cbind(IUCNIndex,BirdIndex,IUCNNumber,BirdNumber,NetESS)
SiteData<-as.data.frame(SiteData[complete.cases(SiteData),])
png(filename="Figure 4A.png")
par(mfrow=c(1,1),mar=c(5,4,4,2))

SummaryBirdData<-matrix(ncol=4,nrow=13)
colnames(SummaryBirdData)<-c("NetESS","MeanBirdStatus","SE","N")
for(x in 1:13){
  SummaryBirdData[x,1]<-x-9
  SummaryBirdData[x,2]<-mean(subset(BirdIndex,NetESS==x-9),na.rm=TRUE)
  SummaryBirdData[x,3]<-sd(subset(BirdIndex,NetESS==x-9),na.rm=TRUE)/sqrt(length(subset(BirdIndex,NetESS==x-9)))
  SummaryBirdData[x,4]<-length(subset(BirdIndex,NetESS==x-9))
}

plot(SummaryBirdData[,1],SummaryBirdData[,2],ylim=c(0,2),xlim=c(-9,6),ylab="Conservation index",xlab="Net ESS")
text(-7,1.75,"A",cex=2)
points(jitter(NetESS),BirdIndex,col="lightgrey",cex=0.5)
points(SummaryBirdData[,1],SummaryBirdData[,2],pch=19)

# Calculate and plot confidence interval
mod1<-lm(BirdIndex~NetESS,data=SiteData)
newx <- seq(min(NetESS), max(NetESS), length.out=100)
preds <- predict(mod1, newdata = data.frame(NetESS=newx), interval = 'confidence')
# add the shaded confidence intervals
polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = rgb(0.1,0.1,0.1,0.2), border = NA)
# add the fitted curve
lines(newx,preds[,1],type="l")
# add the boundaries to the confidence intervals
lines(newx, preds[ ,3], lty = 'dashed', col = 'red');lines(newx, preds[ ,2], lty = 'dashed', col = 'red')
dev.off()
# Spearman's rank correlation
cor.test(NetESS,BirdIndex,method="spearman")

# Net ESS versus IUCN status
SummaryIUCNData<-matrix(ncol=4,nrow=13)
colnames(SummaryIUCNData)<-c("NetESS","MeanBirdStatus","SE","N")
for(x in 1:13){
  SummaryIUCNData[x,1]<-x-9
  SummaryIUCNData[x,2]<-mean(subset(IUCNIndex,NetESS==x-9),na.rm=TRUE)
  SummaryIUCNData[x,3]<-sd(subset(IUCNIndex,NetESS==x-9),na.rm=TRUE)/sqrt(length(subset(IUCNIndex,NetESS==x-9)))
  SummaryIUCNData[x,4]<-length(subset(IUCNIndex,NetESS==x-9))
}

png(filename="Figure S2.png")
par(mfrow=c(1,1),mar=c(5,4,4,2))
plot(SummaryIUCNData[,1],SummaryIUCNData[,2],ylim=c(0,2),xlim=c(-9,6),ylab="IUCN trends index",xlab="Net ESS")
text(-7,1.8,"B",cex=2)
points(jitter(NetESS),IUCNIndex,col="lightgrey",cex=0.5)
points(SummaryIUCNData[,1],SummaryIUCNData[,2],pch=19)

# Calculate and plot confidence interval
mod2<-lm(IUCNIndex~NetESS,data=SiteData)
newx <- seq(min(NetESS), max(NetESS), length.out=100)
preds <- predict(mod2, newdata = data.frame(NetESS=newx), interval = 'confidence')
# add the shaded confidence intervals
polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = rgb(0.1,0.1,0.1,0.2), border = NA)
# add the fitted curve
lines(newx,preds[,1],type="l")
# add the boundaries to the confidence intervals
lines(newx, preds[ ,3], lty = 'dashed', col = 'red');lines(newx, preds[ ,2], lty = 'dashed', col = 'red')
dev.off()
# Spearman's rank correlation
cor.test(NetESS,IUCNIndex,method="spearman")

############################################################################
### 08.7 Site-centred conservation and IUCN indices - by Biogeographical regions
###
### This script takes the site as the unit of replication and calculates
### the "conservation index" (the mean habitat quality for the community
### of bird species living at the site) and the "IUCN index" (the mean
### IUCN trend for the species living at the site)
### in addition, this script runs the analysis by Biogeographical regions
############################################################################

# Convert the A, B, C CONSERVATION code to a numeric score
SPECIES<-read.csv("SPECIES.csv")
ConvertToNumber<-data.frame(Letters=c("A","B","C"),Numbers=c(2,1,0))
SpeciesIndex<-ConvertToNumber[match(SPECIES$CONSERVATION,ConvertToNumber[,1]),2]
SPECIES$SpeciesIndex<-SpeciesIndex

# add Bioregions
SPECIES$Biogeog<-BIOREGION$BIOGEFRAPHICREG[match(SPECIES$SITECODE,BIOREGION$SITECODE)]
SiteTrends$Biogeog<-BIOREGION$BIOGEFRAPHICREG[match(SiteTrends$Site,BIOREGION$SITECODE)]

regions<-c("Boreal","Atlantic","Alpine","Continental","Mediterranean")
png(filename="Figure 4B-F.png")
par(mfrow=c(2,3),mar=c(5,4,4,2))
for(a in 1:length(regions)){
  sub_SPECIES<-subset(SPECIES,SPECIES$Biogeog==paste(regions[a]))
  sub_ServiceBySite<-subset(ServiceBySite,ServiceBySite$Biogeog==paste(regions[a]))
  sub_SiteTrends<-subset(SiteTrends,SiteTrends$Biogeog==paste(regions[a]))
  sub_NetESS<-NetESS[match(rownames(sub_ServiceBySite),names(NetESS))]
  
  BIRDSPECIES<-subset(sub_SPECIES,sub_SPECIES$SPGROUP=="Birds" & sub_SPECIES$GLOBAL %in% c("A","B","C"))

  # Add bird scores to the sites
  IUCNIndex<-BirdIndex<-IUCNNumber<-BirdNumber<-numeric(length=nrow(sub_ServiceBySite))
  for(x in 1:nrow(sub_ServiceBySite)){
    BirdIndex[x]<-mean(subset(BIRDSPECIES$SpeciesIndex,BIRDSPECIES$SITECODE==rownames(sub_ServiceBySite)[x]))
    BirdNumber[x]<-length(subset(BIRDSPECIES$SpeciesIndex,BIRDSPECIES$SITECODE==rownames(sub_ServiceBySite)[x]))
    IUCNIndex[x]<-mean(c(rep(2,subset(sub_SiteTrends$Inc,sub_SiteTrends$Site==rownames(sub_ServiceBySite)[x])),
                         rep(1,subset(sub_SiteTrends$Stable,sub_SiteTrends$Site==rownames(sub_ServiceBySite)[x])),
                         rep(0,subset(sub_SiteTrends$Dec,sub_SiteTrends$Site==rownames(sub_ServiceBySite)[x]))))
    IUCNNumber[x]<-sum(subset(sub_SiteTrends$Inc,sub_SiteTrends$Site==rownames(sub_ServiceBySite)[x]),
                       subset(sub_SiteTrends$Stable,sub_SiteTrends$Site==rownames(sub_ServiceBySite)[x]),
                       subset(sub_SiteTrends$Dec,sub_SiteTrends$Site==rownames(sub_ServiceBySite)[x]))
    if(x%%100==0) {print(x)}
    flush.console()
  }

  SiteData<-cbind(IUCNIndex,BirdIndex,IUCNNumber,BirdNumber,sub_NetESS)
  
  # add Bioregions
  SiteData<-data.frame(SiteData, Biogeog=as.factor(BIOREGION[match(rownames(SiteData),BIOREGION$SITECODE),2]))
  SiteData<-as.data.frame(SiteData[complete.cases(SiteData),])
  
  
  SummaryBirdData<-matrix(ncol=4,nrow=13)
  colnames(SummaryBirdData)<-c("sub_NetESS","MeanBirdStatus","SE","N")
  for(x in 1:13){
    SummaryBirdData[x,1]<-x-9
    SummaryBirdData[x,2]<-mean(subset(BirdIndex,sub_NetESS==x-9),na.rm=TRUE)
    SummaryBirdData[x,3]<-sd(subset(BirdIndex,sub_NetESS==x-9),na.rm=TRUE)/sqrt(length(subset(BirdIndex,sub_NetESS==x-9)))
    SummaryBirdData[x,4]<-length(subset(BirdIndex,sub_NetESS==x-9))
  }
  plot(SummaryBirdData[,1],SummaryBirdData[,2],ylim=c(0,2),xlim=c(-9,6),ylab="Conservation index",xlab="Net ESS", main=paste(regions[a]))
  res<-cor.test(sub_NetESS,BirdIndex,method="spearman")
  points(jitter(sub_NetESS),BirdIndex,col="lightgrey",cex=0.5)
  points(SummaryBirdData[,1],SummaryBirdData[,2],pch=19)
  summary(lm(BirdIndex~sub_NetESS))
  # Calculate and plot confidence interval
  mod1<-lm(BirdIndex~sub_NetESS,data=SiteData)
  newx <- seq(min(sub_NetESS), max(sub_NetESS), length.out=100)
  preds <- predict(mod1, newdata = data.frame(sub_NetESS=newx), interval = 'confidence')
  # add the shaded confidence intervals
  polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = rgb(0.1,0.1,0.1,0.2), border = NA)
  # add the fitted curve
  lines(newx,preds[,1],type="l")
  # add the boundaries to the confidence intervals
  lines(newx, preds[ ,3], lty = 'dashed', col = 'red');lines(newx, preds[ ,2], lty = 'dashed', col = 'red')
  
  }
dev.off()
