# ColonoThicknessandDecoration.R
# Comparing relationship between thickness and surface treatment 
# AND compares residue on burnished and unburnished sherds
# Created by:  JG in SAS translated by EAB 2.18.2016
# Last update: EAB 12.27.2018     

# load the libraries
require(RPostgreSQL)
require(ggplot2)
require(plyr)
require(dplyr)
require(reshape2)
require(plotrix)
require(maptools)
require(tidyverse)
require(viridis)

####Create dataframe of all colono sherds that are burnished####

#Link to file with database password
source("credentials.R")

#some sherds have decoration recorded in CEW table, pull those out
AllColonowDec<- dbGetQuery(DRCcon,'
                           SELECT          
                           "a"."State",
                           "b"."ProjectName" as "ProjectName",
                           "c"."ProjectID" as "ProjectID",
                           "i"."ArtifactID" as "ArtifactID",
                           "i"."Quantity" as "Quantity",
                           "i"."SherdThickness" as "Thickness",
                           "i"."RimDiameter" as "Diameter",
                           "j"."Ware" as "Ware",
                           string_agg(distinct COALESCE("l"."InteriorExterior")||\',\'||("m"."CeramicDecTechType"), \'; \') as "DecTech",  
                           "n"."CeramicCEWDecMode" as "CEWDecTech",
                           "v"."editor_id" as "Editor",
                           "w"."OxidizedVersusReduced" as "Reduction",
                           string_agg(distinct COALESCE("y"."UseWearLocation")||\', \'||COALESCE("z"."UseWearPattern"), \'; \') as "WearLocation_Pattern",
                           "q"."PasteInclusionDensity" as "CoarseEarthenwarePasteInclusionDensity",
                           string_agg(distinct COALESCE("r"."PasteInclusion"), \'; \') as "PasteInclusions",
                           "o"."CeramicCompleteness" as "Completeness"
                           
                           FROM
                           "tblProject" as  "a"
                           left join "tblProjectName" as "b" on "a"."ProjectNameID" = "b"."ProjectNameID"
                           left join "tblContext" as "c" on "a"."ProjectID" = "c"."ProjectID"  
                           left join "tblContextSample" as "g" on "c"."ContextID"="g"."ContextID"
                           left join "tblGenerateContextArtifactID" as "h" on "g"."ContextSampleID" = "h"."ContextSampleID"
                           left join "tblCeramic" as "i" on "h"."ArtifactID" = "i"."ArtifactID"
                           left join "tblCeramicWare" as  "j"  on "i"."WareID" = "j"."WareID"
                           left JOIN "tblCeramicDecTech" as "k" ON "i"."GenerateContextArtifactID" = "k"."GenerateContextArtifactID"
                           left join "tblCeramicDecTechInteriorExterior" as "l" on "k"."InteriorExteriorID" = "l"."InteriorExteriorID"
                           left JOIN "tblCeramicDecTechType" as "m" ON "k"."CeramicDecTechTypeID" = "m"."CeramicDecTechTypeID"    
                           left join "tblCeramicCEWDecMode" as "n" on "i"."CeramicCEWDecModeID"="n"."CeramicCEWDecModeID"
                           left join "users" as "v" on "i"."EditorIDN" = "v"."editor_id"   
                           left join "tblCeramicOxidized" as "w" on "i"."OxidizedVersusReducedID" = "w"."OxidizedVersusReducedID"
                           left join "tblCeramicUseWear" as "x" on "i"."GenerateContextArtifactID" = "x"."GenerateContextArtifactID"
                           left join "tblCeramicUseWearLocation" as "y" on "x"."UseWearLocationID" = "y"."UseWearLocationID"
                           left join "tblCeramicUseWearPattern" as "z" on "x"."UseWearPatternID" = "z"."UseWearPatternID"
                           left join "tblCeramicCompleteness" as "o" on "i"."CeramicCompletenessID" = "o"."CeramicCompletenessID"
                           left join "tblCeramicPasteInclusion" as "p" on "i"."ArtifactID" = "p"."ArtifactID"
                           left join "tblCeramicPasteInclusionDensity" AS "q" on "i"."PasteInclusionDensityID" = "q"."PasteInclusionDensityID"
                           left join "tblPasteInclusion" as "r" on "p"."PasteInclusionID" = "r"."PasteInclusionID"        
                           
                           WHERE
                           
                           ("j"."Ware" = \'Colonoware\') AND
                           
                           ("a"."ProjectID" in (\'1005\', \'1007\', \'1008\', \'1009\', 
                           \'1011\', \'1012\', \'1013\', \'1014\', \'1017\',
                           \'1020\', \'1025\', \'1026\', \'1027\', \'1031\', 
                           \'1152\', \'1153\', \'1154\', 
                           \'1171\', \'1300\', \'1302\',\'1304\',\'1305\', \'1306\')) 
                           
                           
                           GROUP BY
                           "a"."State",
                           "b"."ProjectName",
                           "c"."ProjectID",
                           "o"."CeramicCompleteness", 
                           "i"."ArtifactID",
                           "i"."Quantity",    
                           "j"."Ware",  
                           "i"."SherdThickness", 
                           "i"."RimDiameter",
                           "n"."CeramicCEWDecMode",
                           "w"."OxidizedVersusReduced",
                           "i"."Notes",
                           "v"."editor_id",
                           "q"."PasteInclusionDensity"
                           
                           ')
#Removed projects 1155, 1159, 1160, 1161, 1162, 1163, 1164, 1165, 1167, 1170, 1172 because these are aggregate totals for AI purposes, no thickness or decorative info

#Summarize to see total counts by State
summaryAll<-aggregate(AllColonowDec$Quantity, by=list(AllColonowDec$State), FUN=sum)
summaryAll

#Change project names for sites for sites with 'Colono Study' because this causes problems later on
AllColonowDec$ProjectName <- as.character(AllColonowDec$ProjectName)
AllColonowDec$ProjectName <- ifelse((grepl('Colono', AllColonowDec$ProjectName)),
                                    gsub('Colono Study: ', '', AllColonowDec$ProjectName),
                                    AllColonowDec$ProjectName)

#Change project names to more familiar terms
AllColonowDec$ProjectName[AllColonowDec$ProjectName =='Old Dorchester, SC'] <-"Old Dorchester"
AllColonowDec$ProjectName[AllColonowDec$ProjectName =='38BK75'] <-"Yaughan 75"
AllColonowDec$ProjectName[AllColonowDec$ProjectName =='38BK76'] <-"Yaughan 76"
AllColonowDec$ProjectName[AllColonowDec$ProjectName =='38BK245'] <-"Curriboo"


####Remove records with questionable slip IDs from SC sites####
ColonowDec2<-subset(AllColonowDec, !((AllColonowDec$Editor == '3' & AllColonowDec$ProjectID == '1300') | 
                                       (AllColonowDec$Editor == '3' & AllColonowDec$ProjectID == '1302') |
                                       (AllColonowDec$Editor == '3' & AllColonowDec$ProjectID == '1304') | 
                                       (AllColonowDec$Editor == '3' & AllColonowDec$ProjectID == '1305') | 
                                       (AllColonowDec$Editor == '3' & AllColonowDec$ProjectID == '1306')))

EditorCheck<-ddply(ColonowDec2, .(Editor, ProjectID), summarise, Count=sum(Quantity))

ColonowDecT<- tapply(ColonowDec2$Quantity, ColonowDec2$State, FUN=sum)  

#write out csv file to check if necessary
#write.csv(ColonowDec, file='ColonowDecT.csv')

####Create Thickness Dataset####
#include only sherds with completeness that is not NA
SherdsNACompleteness1 <- ColonowDec2$ArtifactID[(is.na(ColonowDec2$Completeness))]
paste(SherdsNACompleteness1)

#Subset dataframe by removing all Artifact IDs that are in the list of SherdsNoThickness
ColonowDec3 <- ColonowDec2[!ColonowDec2$ArtifactID %in% SherdsNACompleteness1,]
#check to make sure no NAs remain
Colono3 <- ddply(ColonowDec3, .(ProjectName, ProjectID, Completeness), summarise, Count=sum(Quantity))

#Remove all sherds where handle is part of completeness (i.e. Handle, Body, Handle, Rim, etc.) 
#because this will impact average thickness 
ColonowDec4<-ColonowDec3[!grepl("^.*Handle", ColonowDec3$Completeness), ]
ColonobyProject <- ddply(ColonowDec4, .(ProjectName, ProjectID, Completeness), summarise, Count=sum(Quantity))

#Create a new column that captures whether sherd is burnished or unburnished
#Look in DecTech column to see if burnishing is recorded, if so, put "Burnished" in new
#column, if not, paste "Unburnished"
ColonowDec4$Burnishing<-ifelse(grepl("^.*Burnished", ColonowDec4$DecTech),
                               paste("Burnished"), paste("Unburnished"))

#include only sherds with thickness measurements
SherdsNoThickness <- ColonowDec4$ArtifactID[(is.na(ColonowDec4$Thickness))]
#Subset dataframe by removing all Artifact IDs that are in the list of SherdsNoThickness
AllColonowThickness <- ColonowDec4[!ColonowDec4$ArtifactID %in% SherdsNoThickness,]

#Subset dataframe to get colono sherds that are burnished just to see total amount
#AllColonoBur1 <- ColonowDec[grepl("^.*Burnished", ColonowDec$DecTech) | grepl("^.*Burnished", ColonowDec$CEWDecTech), ]


#summarize colono by site to see totals using aggregate which splits data into subsets and computes summary stats for each
sumSite<-aggregate(AllColonowThickness$Quantity, by=list(AllColonowThickness$ProjectID), FUN=sum)
sumSite

#remove sites with assemblage sizes less than 10
AllColonowThickness<-subset(AllColonowThickness, (AllColonowThickness$ProjectID != '1005' & AllColonowThickness$ProjectID != '1007'
                                                  & AllColonowThickness$ProjectID != '1011' & AllColonowThickness$ProjectID != '1031'))
summary_rmv<-aggregate(AllColonowThickness$Quantity, by=list(AllColonowThickness$ProjectID), FUN=sum)
summary_rmv

####Histograms to Explore Thickness Data####

#create histogram of thickness and burnishing comparing regions with ggplot
ThicknessByRegion<-ggplot(AllColonowThickness, aes(x=AllColonowThickness$Thickness))+
  geom_histogram(aes(y=..density..), binwidth=1, color="black", fill="white")+
  geom_density(alpha=.2, fill="light blue")+
  labs(title="Colonoware Thickness, By Region",x="Thickness(mm)", y = "Density")+
  facet_grid(State~.)
ThicknessByRegion

#create histogram of thickness and burnishing comparing regions with ggplot
ThicknessByBurnishing<-ggplot(AllColonowThickness, aes(x=AllColonowThickness$Thickness))+
  geom_histogram(aes(y=..density..), binwidth=1, color="black", fill="white")+
  geom_density(alpha=.2, fill="light blue")+
  labs(title="Colonoware Thickness, By Region",x="Thickness(mm)", y = "Density")+
  facet_grid(Burnishing ~ State)
ThicknessByBurnishing

#right skewed need to take log
AllColonowThickness$LogThickness<-log(AllColonowThickness$Thickness)

#create histogram of thickness and burnishing comparing regions with ggplot
ThicknessByRegion2<-ggplot(AllColonowThickness, aes(x=AllColonowThickness$LogThickness))+
  geom_histogram(aes(y=..density..), binwidth=.1, color="black", fill="white")+
  geom_density(alpha=.2, fill="light blue")+
  labs(title="Colonoware Thickness, By Region",x="Log Thickness(mm)", y = "Density")+
  facet_grid(State~.)
ThicknessByRegion2

ThicknessByBurnishing2<-ggplot(AllColonowThickness, aes(x=AllColonowThickness$LogThickness))+
  geom_histogram(aes(y=..density..), binwidth=.1, color="black", fill="white")+
  geom_density(alpha=.2, fill="light blue")+
  labs(title="Colonoware Thickness and Decoration, By Region",x="Log Thickness(mm)", y = "Density")+
  facet_grid(Burnishing ~ State)

ThicknessByBurnishing2

#Read in Mean Occupation dates
Dates<-read.csv('CA_MCD.csv')

AllColonowThickness2<-left_join(AllColonowThickness, Dates, by="ProjectName")
#ggplot boxplot of thickness by burnished and unburnished by site and order projects by their mean
#occupation date
burn<-ggplot(AllColonowThickness2, aes(x=reorder(ProjectName, MeanOccupationDate), y=LogThickness, fill=Burnishing)) + geom_boxplot(notch=F) +
  labs(title="Comparison of Thickness and Burnishing by Site Assemblage") +
  theme_classic() +
  labs(x="Project Name", y="Log Thickness") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.2)) +
  theme(panel.background = element_rect(fill = NA, color = "black")) +
  facet_grid(. ~ State.x, scales="free")

burn


####Plot burnished/unburnished for sites and compare by average thickness####

#calculate average log thickness by ProjectID
Colono_avgthick <-aggregate(AllColonowThickness$Thickness, by=list(AllColonowThickness$ProjectName), FUN=mean)
Colono_avgLogthick <-aggregate(AllColonowThickness$LogThickness, by=list(AllColonowThickness$ProjectName), FUN=mean)
#calculate average thickness by ProjectID

colnames(Colono_avgthick)<-c("ProjectName", "Average")
colnames(Colono_avgLogthick)<-c("ProjectName", "Average")

#calculate average log thickness for burnished sherds by ProjectID
AllColonowThicknessBur<-filter(AllColonowThickness2, Burnishing=='Burnished')

Colono_ThickLogBur<- aggregate(AllColonowThicknessBur$LogThickness, by=list(AllColonowThicknessBur$ProjectName), FUN=mean) 
Colono_ThickBur <- aggregate(AllColonowThicknessBur$Thickness, by=list(AllColonowThicknessBur$ProjectName), FUN=mean)

colnames(Colono_ThickLogBur)<-c("ProjectName", "LogAverage")
colnames(Colono_ThickBur)<-c("ProjectName", "Average")

#calculate sd thickness for regular values and log transformed values
Csdthick<- tapply(AllColonowThickness2$LogThickness, AllColonowThickness2$ProjectName, FUN=sd)   
Csdthick2<- tapply(AllColonowThickness2$Thickness, AllColonowThickness2$ProjectName, FUN=sd)

#calculate n=projectid assemblage size
CThickN<- tapply(AllColonowThickness2$LogThickness, AllColonowThickness2$ProjectName, FUN=length)

AvgSDN<-data.frame(Colono_avgLogthick, Csdthick, CThickN)
#MCDValues<-read.csv('AbundanceIndexTable.csv',header=T, sep=',')

#MCDOrder <- MCDValues[order(MCDValues$ProjectID),]
#MCDOrder3 <-MCDOrder[-c(1:2,5,13,14:16,20:29),]

ThicknessAvgMCD <-merge(AvgSDN, Dates, by.x=c("ProjectName"), by.y=c("ProjectName"))
ThicknessAvgMCD <-ThicknessAvgMCD[-c(5),]

####Plot log body thickness over time####
#--------------CI
alpha <- .05
LogThickLowerCL <- ThicknessAvgMCD$Average + ThicknessAvgMCD$Csdthick/sqrt(ThicknessAvgMCD$CThickN) * qt(alpha/2,ThicknessAvgMCD$CThickN-1)   
LogThickUpperCL <- ThicknessAvgMCD$Average + ThicknessAvgMCD$Csdthick/sqrt(ThicknessAvgMCD$CThickN) * qt(1-(alpha/2), ThicknessAvgMCD$CThickN-1)   


plot(ThicknessAvgMCD$Average~ThicknessAvgMCD$MeanOccupationDate, pch=19, cex=2, ylim=c(1,2.5),col=ThicknessAvgMCD$State, 
     main="Average Log Thickness Compared with Mean Occupation Date", xlab="Mean Occupation Date", ylab="Average Log Thickness")

arrows(ThicknessAvgMCD$Average,LogThickLowerCL,ThicknessAvgMCD$Average,LogThickUpperCL,
       angle=90,code=3,length=.1, col="grey", lwd=2)


plotCI(ThicknessAvgMCD$Average, ThicknessAvgMCD$MeanOccupationDate, ui=LogThickUpperCL, li=LogThickLowerCL, add=T)
pointLabel(ThicknessAvgMCD$Average, ThicknessAvgMCD$MeanOccupationDate, ThicknessAvgMCD$ProjectName, offset=0, cex=1, col="black")
legend("topleft", # places a legend at the appropriate place 
       c("VA","SC"), # puts text in the legend
       pch=c(19,19), # gives the legend appropriate symbols (lines)
       col=c("red","black")) # gives the legend lines the correct color and width



plot(ThicknessAvgMCD$Average ~ ThicknessAvgMCD$MeanOccupationDate, pch=19, cex=2, ylim=c(1,2.5),col=ThicknessAvgMCD$State, 
     main="Average Thickness Over Time", xlab="Mean Occupation Date", ylab="Average Log Thickness")
plotCI(ThicknessAvgMCD$MeanOccupationDate, ThicknessAvgMCD$Average, ui=LogThickUpperCL, li=LogThickLowerCL, add=T)
pointLabel(ThicknessAvgMCD$blueMCD, ThicknessAvgMCD$Average, ThicknessAvgMCD$ProjectName, offset=0, cex=1, col="black")
legend("topleft", # places a legend at the appropriate place 
       c("VA","SC"), # puts text in the legend
       pch=c(19,19), # gives the legend appropriate symbols (lines)
       col=c("red","black")) # gives the legend lines the correct color and width