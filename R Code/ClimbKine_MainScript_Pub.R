# R script for "Limb kinematics and morphology improve salamander climbing performance"
# Code written by Jonathan M. Huie

rm(list = ls())
source("All_Functions.R") # all additional functions
library(kraken)
library(ggplot2)
library(patchwork)
library(abind)
library(MASS)
library(plyr)
library(dplyr)
library(emmeans)

col.sp <- setNames(c("darkgrey","#8c510a","#35978f","purple"),c("Aneides aeneus","Aneides hardii","Aneides lugubris","Plethodon glutinosus"))
shape.sp <- setNames(c(21,22,23,24), c("Aneides aeneus","Aneides hardii","Aneides lugubris","Plethodon glutinosus"))
fill.sp <- setNames(c(c("darkgrey","#8c510a","white","purple")), c("Aneides aeneus","Aneides hardii","Aneides lugubris","Plethodon glutinosus"))

# Run Once -----------------------------------------------------------------
# do not need to run if the whole 'R Code' folder was downloaded from GitHub
# jump ahead to where it says 'Start Here'

## Load Raw 3D pts ----------------------------------------------------

# unhash the following code to run

# # read in 3D pts and create lists
# combined_fore <- list()
# combined_hind <- list()
# 
# threeD_path <- "../Landmarks/3Dpoints"
# 
# files <- list.files(path =threeD_path, full.names = TRUE)
# KineMeta <- read.csv("ClimbKineMeta.csv") 
# KineData <- KineMeta[,1:11]
# for (i in 1:length(files)) {
#   name <- list.files(path =threeD_path, full.names = FALSE)[i]
#   name <- gsub(".csv","",name)
#   land <- read.csv(files[i], na.strings = "NaN")
#   if (any(colnames(land) == "Frame")) {
#     land <- land[,-1]
#   }
#   
#   # prep forelimb points; trim to just the stride
#   fore <- land[,c(1:21,43:45)]
#   fore <- fore[,c(grep("Pec_A",colnames(fore)),grep("Pec_P",colnames(fore)),grep("Shoulder",colnames(fore)),
#                   grep("Elbow",colnames(fore)),grep("Wrist",colnames(fore)),
#                   grep("Metacarpal",colnames(fore)),grep("Finger",colnames(fore)),grep("Snout",colnames(fore)))]
#   fore <- na.omit(fore)
#   fore[,c(grep("X",colnames(fore),grep("Y",colnames(fore))))] <- -1*fore[,c(grep("X",colnames(fore),grep("Y",colnames(fore))))]
#   start <- KineMeta[which(KineMeta$Trial == name),"Fore_Stance_Start"]
#   end <- KineMeta[which(KineMeta$Trial == name),"Fore_Swing_End"]
#   fore <- fore[which.min(abs(as.numeric(rownames(fore)) - start)):which.min(abs(as.numeric(rownames(fore)) - end)),]
#   
#   # prep hindlimb points; trim to just the stride
#   hind <- land[,c(22:42,43:45)]
#   hind <- hind[,c(grep("Pel_A",colnames(hind)),grep("Pel_P",colnames(hind)),grep("Hip",colnames(hind)),
#                   grep("Knee",colnames(hind)),grep("Ankle",colnames(hind)),
#                   grep("Metatarsal",colnames(hind)),grep("Toe",colnames(hind)),grep("Snout",colnames(hind)))]
#   hind <- na.omit(hind)
#   hind[,c(grep("X",colnames(hind),grep("Y",colnames(hind))))] <- -1*hind[,c(grep("X",colnames(hind),grep("Y",colnames(hind))))]
#   start <- KineMeta[which(KineMeta$Trial == name),"Hind_Stance_Start"]
#   end <- KineMeta[which(KineMeta$Trial == name),"Hind_Swing_End"]
#   hind <- hind[which.min(abs(as.numeric(rownames(hind)) - start)):which.min(abs(as.numeric(rownames(hind)) - end)),]
#   
#   # prep snout landmark
#   snout <- land[,c(43:45)]
#   snout <- na.omit(snout)
#   snout[,c(grep("X",colnames(snout),grep("Y",colnames(snout))))] <- -1*snout[,c(grep("X",colnames(snout),grep("Y",colnames(snout))))]
#   start <- KineMeta[which(KineMeta$Trial == name),"Hind_Stance_Start"]
#   end <- KineMeta[which(KineMeta$Trial == name),"Fore_Swing_End"]
#   snout <- snout[which.min(abs(as.numeric(rownames(snout)) - start)):which.min(abs(as.numeric(rownames(snout)) - end)),]
#   
#   # calc body speed
#   fps <- KineMeta[which(KineMeta$Trial == name),"Frame_Rate"]/KineMeta[which(KineMeta$Trial == name),"Decimation"]
#   KineData[which(KineData$Trial == name),"Speed"] <- speedKine(snout,fps)$speed
#   # calc stride length
#   KineData[which(KineData$Trial == name),"Hind_Stride_Length"] <- speedKine(hind[,13:15],fps)$dist
#   KineData[which(KineData$Trial == name),"Fore_Stride_Length"] <- speedKine(fore[,13:15],fps)$dist
#   # calc limb length
#   KineData[which(KineData$Trial == name),"Arm_Length"] <- limb_length(fore[,7:9],fore[,10:12],fore[,13:15])
#   KineData[which(KineData$Trial == name),"Leg_Length"] <- limb_length(hind[,7:9],hind[,10:12],hind[,13:15])
#   
#   if (all(is.na(fore)) != TRUE) {
#     combined_fore[[length(combined_fore) + 1]] <- fore
#   }
#   if (all(is.na(hind)) != TRUE) {
#     combined_hind[[length(combined_hind) + 1]] <- hind
#   }
#   
#   remove(land)
#   remove(fore)
#   remove(hind)
#   remove(snout)
# }
# 
# for(i in unique(KineData$ID)) {
#   KineData[grep(i,KineData$ID),"Arm_Length"] <- tapply(KineData$Arm_Length,KineData$ID,median)[i]
#   KineData[grep(i,KineData$ID),"Leg_Length"] <- tapply(KineData$Leg_Length,KineData$ID,median)[i]
# }
# 
# # some spatiotemporal calc
# KineData$Fore_Duty_Factor <- (KineMeta$Fore_Stance_End+1 - KineMeta$Fore_Stance_Start)/(KineMeta$Fore_Swing_End - KineMeta$Fore_Stance_Start)
# KineData$Hind_Duty_Factor <- (KineMeta$Hind_Stance_End+1 - KineMeta$Hind_Stance_Start)/(KineMeta$Hind_Swing_End - KineMeta$Hind_Stance_Start)
# KineData$Fore_Stride_Dur <- (KineMeta$Fore_Swing_End - KineMeta$Fore_Stance_Start)/(KineMeta$Frame_Rate/KineMeta$Decimation)
# KineData$Hind_Stride_Dur <- (KineMeta$Hind_Swing_End - KineMeta$Hind_Stance_Start)/(KineMeta$Frame_Rate/KineMeta$Decimation)
# KineData$Fore_Stride_Freq <- 1/KineData$Fore_Stride_Dur
# KineData$Hind_Stride_Freq <- 1/KineData$Hind_Stride_Dur
# 
# # save the kine data so far
# write.csv(KineData,"ClimbKineData.csv",row.names = F,quote = F)
# 
# names(combined_fore) <- gsub(".csv","",basename(files))
# names(combined_hind) <- gsub(".csv","",basename(files))
# 
# #interpolate to 101 frames so each column = 1% of stance
# combined_fore <- lapply(combined_fore, FUN = function(x) interpolateR(x, 101))
# combined_hind <- lapply(combined_hind, FUN = function(x) interpolateR(x, 101))
# 
# # save lists as RDS so that you don't have to run this whole chunk again
# saveRDS(combined_fore, "fore3Dpts_all.rds")
# saveRDS(combined_hind, "hind3Dpts_all.rds")
# 
# ## Calculate Angles --------------------------------------------------------
# 
# # calculate the 3D joint angles
# KineMeta <- read.csv("ClimbKineMeta.csv") 
# combined_fore <- readRDS("fore3Dpts_all.rds")
# combined_hind <- readRDS("hind3Dpts_all.rds")
# All_fore <- list()
# All_hind <- list()
# CalibPlanes <- read.csv("../CameraCalibs/CalibPlanes.csv") 
# rownames(CalibPlanes) <- CalibPlanes$Calibration
# 
# # forelimbs
# for (i in names(combined_fore)){
#   calib <- KineMeta$Calibration[which(KineMeta$Trial == i)]
#   All_fore[i] <- lapply(combined_fore[i], anglecalc,
#                         planeP1 = as.numeric(CalibPlanes[calib,2:4]),
#                         planeP2 = as.numeric(CalibPlanes[calib,5:7]),
#                         planeP3 = as.numeric(CalibPlanes[calib,8:10]))
# }
# 
# # hindlimbs
# for (i in names(combined_hind)){
#   calib <- KineMeta$Calibration[which(KineMeta$Trial == i)]
#   All_hind[i] <- lapply(combined_hind[i], anglecalc,
#                         planeP1 = as.numeric(CalibPlanes[calib,2:4]),
#                         planeP2 = as.numeric(CalibPlanes[calib,5:7]),
#                         planeP3 = as.numeric(CalibPlanes[calib,8:10]))
# }
# 
# #check that there were no errors or NAs
# which(sapply(All_fore, anyNA))
# which(sapply(All_hind, anyNA))
# 
# #save RDS
# saveRDS(All_fore, "foreAngles_all.rds")
# saveRDS(All_hind, "hindAngles_all.rds")


# Start Here --------------------------------------------------------------
## Load Data --------------------------------------------------------

# load data back in
KineMeta <- read.csv("ClimbKineMeta.csv") 
KineData <- read.csv("ClimbKineData.csv") 
rownames(KineData) <- KineData$Trial

# correct speed and morphology by SVL
KineData$Speed <- KineData$Speed/KineData$SVL
KineData$Fore_Stride_Length <- KineData$Fore_Stride_Length/KineData$SVL
KineData$Hind_Stride_Length <- KineData$Hind_Stride_Length/KineData$SVL
FootData <- read.csv("ClimbKineFootSize.csv") 
KineData$HandArea <- NA
KineData$FootArea <- NA
for (i in unique(KineData$ID)) {
  KineData[which(KineData$ID == i), "ManusArea"] <- FootData[which(FootData$ID == i), "ManusArea"]
  KineData[which(KineData$ID == i), "PesArea"] <- FootData[which(FootData$ID == i), "PesArea"]
}

# correct lateral limb spread by SVL
foreAngles <-readRDS("foreAngles_all.rds")
hindAngles <- readRDS("hindAngles_all.rds")
for (i in 1:nrow(KineMeta)) {
  foreAngles[[KineMeta$Trial[i]]]$spread <- foreAngles[[KineData$Trial[i]]]$spread/(KineData$SVL[i])
  hindAngles[[KineMeta$Trial[i]]]$spread <- hindAngles[[KineData$Trial[i]]]$spread/(KineData$SVL[i])
}

# subset kinematics for stance phase 
foreAngles_stance <- foreAngles
hindAngles_stance <- hindAngles
for (i in 1:nrow(KineData)) {
  foreAngles_stance[[i]] <- foreAngles[[i]][1:round(KineData$Fore_Duty_Factor[i]*100,0),]
  hindAngles_stance[[i]] <- hindAngles[[i]][1:round(KineData$Hind_Duty_Factor[i]*100,0),]
}

# calculate the mean autopod angle and limb spread during stance
KineData$Hand_Angle <- 0
KineData$Foot_Angle <- 0
KineData$Hand_Spread <- 0
KineData$Foot_Spread <- 0
for ( i in 1:nrow(KineData)) {
  trial <- KineData$Trial[i]
  KineData$Hand_Angle[i] <- mean(foreAngles_stance[[i]]$foot)
  KineData$Foot_Angle[i] <- mean(hindAngles_stance[[i]]$foot)
  KineData$Hand_Spread[i] <- mean(foreAngles_stance[[i]]$spread)
  KineData$Foot_Spread[i] <- mean(hindAngles_stance[[i]]$spread)
}


# Plot Profiles -----------------------------------------------------------

# calculate the mean joint angle and standard error for each joint for each % of the stride
Aaen_fore <- foreAngles[grep("Aaen",names(foreAngles))]
Aaen_hind <- hindAngles[grep("Aaen",names(hindAngles))]

Alug_fore <- foreAngles[grep("Alug",names(foreAngles))]
Alug_hind <- hindAngles[grep("Alug",names(hindAngles))]

Ahar_fore <- foreAngles[grep("Ahar",names(foreAngles))]
Ahar_hind <- hindAngles[grep("Ahar",names(hindAngles))]

Pglu_fore <- foreAngles[grep("Pglu",names(foreAngles))]
Pglu_hind <- hindAngles[grep("Pglu",names(hindAngles))]


Aaen_fore_0_MeanSE <- SummKine(Aaen_fore[grep("Flat",names(Aaen_fore))],"Aaen","fore","0")
Aaen_hind_0_MeanSE <- SummKine(Aaen_hind[grep("Flat",names(Aaen_hind))],"Aaen","hind","0")
Aaen_fore_90_MeanSE <- SummKine(Aaen_fore[grep("Vert",names(Aaen_fore))],"Aaen","fore","90")
Aaen_hind_90_MeanSE <- SummKine(Aaen_hind[grep("Vert",names(Aaen_hind))],"Aaen","hind","90")

Alug_fore_0_MeanSE <- SummKine(Alug_fore[grep("Flat",names(Alug_fore))],"Alug","fore","0")
Alug_hind_0_MeanSE <- SummKine(Alug_hind[grep("Flat",names(Alug_hind))],"Alug","hind","0")
Alug_fore_90_MeanSE <- SummKine(Alug_fore[grep("Vert",names(Alug_fore))],"Alug","fore","90")
Alug_hind_90_MeanSE <- SummKine(Alug_hind[grep("Vert",names(Alug_hind))],"Alug","hind","90")

Ahar_fore_0_MeanSE <- SummKine(Ahar_fore[grep("Flat",names(Ahar_fore))],"Ahar","fore","0")
Ahar_hind_0_MeanSE <- SummKine(Ahar_hind[grep("Flat",names(Ahar_hind))],"Ahar","hind","0")
Ahar_fore_80_MeanSE <- SummKine(Ahar_fore[grep("80deg",names(Ahar_fore))],"Ahar","fore","80")
Ahar_hind_80_MeanSE <- SummKine(Ahar_hind[grep("80deg",names(Ahar_hind))],"Ahar","hind","80")

Pglu_fore_0_MeanSE <- SummKine(Pglu_fore[grep("Flat",names(Pglu_fore))],"Pglu","fore","0")
Pglu_hind_0_MeanSE <- SummKine(Pglu_hind[grep("Flat",names(Pglu_hind))],"Pglu","hind","0")
Pglu_fore_90_MeanSE <- SummKine(Pglu_fore[grep("Vert",names(Pglu_fore))],"Pglu","fore","90")
Pglu_hind_90_MeanSE <- SummKine(Pglu_hind[grep("Vert",names(Pglu_hind))],"Pglu","hind","90")

# For Figure 3: plot walking and climbing hindlimb profiles
# walking
AbAd_Walk <- PlotKine(rbind(Aaen_hind_0_MeanSE$abad,Alug_hind_0_MeanSE$abad,Ahar_hind_0_MeanSE$abad,Pglu_hind_0_MeanSE$abad), var = "Species", ylab = "Abd vs Add (°)", rect = T, ylim =c(-20,60))+
  annotate("text", x=90, y=-18, label= "Adduction", size = 3)+annotate("text", x=90, y=59, label= "Abduction", size = 3)
ProRet_Walk <- PlotKine(rbind(Aaen_hind_0_MeanSE$proret,Alug_hind_0_MeanSE$proret,Ahar_hind_0_MeanSE$proret,Pglu_hind_0_MeanSE$proret), var = "Species", ylab = "Pro vs Ret (°)", rect = T, ylim = c(-70,70)) +
  annotate("text", x=90, y=-68, label= "Retraction", size = 3)+annotate("text", x=89, y=69, label= "Protraction", size = 3)
Elbow_Walk <- PlotKine(rbind(Aaen_hind_0_MeanSE$elbow,Alug_hind_0_MeanSE$elbow,Ahar_hind_0_MeanSE$elbow,Pglu_hind_0_MeanSE$elbow), var = "Species", ylab = "Knee (°)", ylim = c(50, 180)) +
  annotate("text", x=93, y=53, label= "Flexion", size = 3)+annotate("text", x=90, y=179, label= "Extension", size = 3)
Wrist_Walk <- PlotKine(rbind(Aaen_hind_0_MeanSE$wrist,Alug_hind_0_MeanSE$wrist,Ahar_hind_0_MeanSE$wrist,Pglu_hind_0_MeanSE$wrist), var = "Species", ylab = "Ankle (°)", ylim = c(50, 180)) +
  annotate("text", x=93, y=53, label= "Flexion", size = 3)+annotate("text", x=90, y=179, label= "Extension", size = 3)

# climbing
AbAd_Climb <- PlotKine(rbind(Aaen_hind_90_MeanSE$abad,Alug_hind_90_MeanSE$abad,Ahar_hind_80_MeanSE$abad,Pglu_hind_90_MeanSE$abad), var = "Species", ylab = "Abd vs Add (°)", rect = T, ylim =c(-20,60))+
  annotate("text", x=90, y=-18, label= "Adduction", size = 3)+annotate("text", x=90, y=59, label= "Abduction", size = 3)
ProRet_Climb <- PlotKine(rbind(Aaen_hind_90_MeanSE$proret,Alug_hind_90_MeanSE$proret,Ahar_hind_80_MeanSE$proret,Pglu_hind_90_MeanSE$proret), var = "Species", ylab = "Pro vs Ret (°)", rect = T, ylim = c(-70,70)) +
  annotate("text", x=90, y=-67, label= "Retraction", size = 3)+annotate("text", x=89, y=69, label= "Protraction", size = 3)
Elbow_Climb <- PlotKine(rbind(Aaen_hind_90_MeanSE$elbow,Alug_hind_90_MeanSE$elbow,Ahar_hind_80_MeanSE$elbow,Pglu_hind_90_MeanSE$elbow), var = "Species", ylab = "Knee (°)", ylim = c(50, 180)) +
  annotate("text", x=93, y=53, label= "Flexion", size = 3)+annotate("text", x=90, y=179, label= "Extension", size = 3)
Wrist_Climb <- PlotKine(rbind(Aaen_hind_90_MeanSE$wrist,Alug_hind_90_MeanSE$wrist,Ahar_hind_80_MeanSE$wrist,Pglu_hind_90_MeanSE$wrist), var = "Species", ylab = "Ankle (°)", ylim = c(50, 180)) +
  annotate("text", x=93, y=53, label= "Flexion", size = 3)+annotate("text", x=90, y=179, label= "Extension", size = 3)

plot_list <- list(AbAd_Walk,AbAd_Climb,ProRet_Walk,ProRet_Climb,Elbow_Walk,Elbow_Climb,Wrist_Walk,Wrist_Climb)
wrap_plots(plot_list, ncol =2, guides = "collect") +  plot_annotation(tag_levels = "A") & theme(legend.position = 'bottom') 
# ggsave("Figure 3.pdf", width = 7.25, height = 7.6, units = "in")

# DFA --------------------------------------------------------------------
## Prep Data ---------------------------------------------------------------
# determine whether stride kinematics can be distinguished based on species and incline
ForeKine <- KineData[,c("Species","ID","Trial","Incline","SVL")]
ForeKine$Incline <- factor(ForeKine$Incline)
levels(ForeKine$Incline) <- c("0","45","80","90")
ForeKine$Limb <- "fore"
ForeKine$Group <- paste0(ForeKine$Species,"_",ForeKine$Incline)
ForeKine <- cbind(ForeKine, KineData[,c("Hand_Angle","Hand_Spread")])
ForeKine$Hand_Angle <- ForeKine$Hand_Angle + 90
ForeKine$Duty_Factor <- KineData$Fore_Duty_Factor
ForeKine$Stride_Length <- KineData$Fore_Stride_Length
ForeKine$Stride_Freq <- KineData$Fore_Stride_Freq
for ( i in ForeKine$Trial) {
  ForeKine[i,"ab_max"] <- abs(max(foreAngles[[i]]$abad))
  ForeKine[i,"ad_max"] <- abs(max(foreAngles[[i]]$abad*-1))
  ForeKine[i,"abad_exc"] <- max(foreAngles[[i]]$abad) - min(foreAngles[[i]]$abad) 
  ForeKine[i,"pro_max"] <- abs(max(foreAngles[[i]]$proret) )
  ForeKine[i,"ret_max"] <- abs(max(foreAngles[[i]]$proret*-1))
  ForeKine[i,"proret_exc"] <- max(foreAngles[[i]]$proret) - min(foreAngles[[i]]$proret) 
  ForeKine[i,"elbow_ext_max"] <- abs(max(foreAngles[[i]]$elbow-90))
  ForeKine[i,"elbow_flex_max"] <- abs(max((foreAngles[[i]]$elbow-90)*-1))
  ForeKine[i,"elbow_exc"] <- max(foreAngles[[i]]$elbow) - min(foreAngles[[i]]$elbow) 
  ForeKine[i,"wrist_ext_max"] <- abs(max(foreAngles[[i]]$wrist-90))
  ForeKine[i,"wrist_flex_max"] <- abs(max((foreAngles[[i]]$wrist-90)*-1))
  ForeKine[i,"wrist_exc"] <- max(foreAngles[[i]]$wrist) - min(foreAngles[[i]]$wrist) 
  ForeKine[i,"yaw_ips_max"] <- abs(max(foreAngles[[i]]$yaw))
  ForeKine[i,"yaw_cont_max"] <- abs(max(foreAngles[[i]]$yaw*-1))
  ForeKine[i,"yaw_exc"] <- max(foreAngles[[i]]$yaw)-min(foreAngles[[i]]$yaw) 
}

HindKine <- KineData[,c("Species","ID","Trial","Incline","SVL")]
HindKine$Incline <- factor(HindKine$Incline)
levels(HindKine$Incline) <- c("0","45","80","90")
HindKine$Limb <- "hind"
HindKine$Group <- paste0(HindKine$Species,"_",HindKine$Incline)
HindKine <- cbind(HindKine, KineData[,c("Foot_Angle","Foot_Spread")])
HindKine$Foot_Angle <- HindKine$Foot_Angle + 90
HindKine$Duty_Factor <- KineData$Hind_Duty_Factor
HindKine$Stride_Length <- KineData$Hind_Stride_Length
HindKine$Stride_Freq <- KineData$Hind_Stride_Freq
for ( i in HindKine$Trial) {
  HindKine[i,"ab_max"] <- abs(max(hindAngles[[i]]$abad))
  HindKine[i,"ad_max"] <- abs(max(hindAngles[[i]]$abad*-1))
  HindKine[i,"abad_exc"] <- max(hindAngles[[i]]$abad) - min(hindAngles[[i]]$abad) 
  HindKine[i,"pro_max"] <- abs(max(hindAngles[[i]]$proret))
  HindKine[i,"ret_max"] <- abs(max(hindAngles[[i]]$proret*-1))
  HindKine[i,"proret_exc"] <- max(hindAngles[[i]]$proret) - min(hindAngles[[i]]$proret) 
  HindKine[i,"knee_ext_max"] <- abs(max(hindAngles[[i]]$elbow-90))
  HindKine[i,"knee_flex_max"] <- abs(max((hindAngles[[i]]$elbow-90)*-1))
  HindKine[i,"knee_exc"] <- max(hindAngles[[i]]$elbow) - min(hindAngles[[i]]$elbow) 
  HindKine[i,"ankle_ext_max"] <- abs(max(hindAngles[[i]]$wrist)-90)
  HindKine[i,"ankle_flex_max"] <- abs(max((hindAngles[[i]]$wrist-90)*-1))
  HindKine[i,"ankle_exc"] <- max(hindAngles[[i]]$wrist) - min(hindAngles[[i]]$wrist) 
  HindKine[i,"yaw_ips_max"] <- abs(max(hindAngles[[i]]$yaw))
  HindKine[i,"yaw_cont_max"] <- abs(max(hindAngles[[i]]$yaw*-1))
  HindKine[i,"yaw_exc"] <- max(hindAngles[[i]]$yaw)-min(hindAngles[[i]]$yaw) 
}

## Perfom DFA --------------------------------------------------------------
# now visualize the forelimbs and hindlimbs in the same analysis
colnames(ForeKine) <- colnames(HindKine)

BothKine <- rbind(HindKine,ForeKine)
BothKine$Group[1:840] <- paste0(HindKine$Group,"_hind")
BothKine$Group[841:1680] <- paste0(ForeKine$Group,"_fore")


dfa.data <- BothKine[,-c(1:6,10:12)]
dfa.data[,-1] <- log(dfa.data[,-1])

dfa <- lda(Group ~ ., dfa.data[,])
dfa$scaling[,2] <- dfa$scaling[,2]*-1
predictions <- predict(dfa, dfa.data)

ldascores <- data.frame(predictions$x)
ldascores <- cbind(Species = as.factor(BothKine$Species),Incline = as.factor(BothKine$Incline), Group = as.factor(BothKine$Group), ldascores)
ldascores$Limb[1:840] <- "hind"
ldascores$Limb[841:1680] <- "fore"

# plot biplot; df 1 vs df 2
plot(predictions$x[,2]~predictions$x[,1], 
     pch =c(19,17,7,3)[as.factor(BothKine$Incline)], 
     col = "transparent")
lda.arrows(dfa, myscale = .5,tex = 0.5)

# plot biplot; df 2 vs df 3
plot(predictions$x[,3]~predictions$x[,2],
     pch =c(19,17,7,3)[as.factor(BothKine$Incline)],
     col = "transparent")
lda.arrows(dfa, myscale = .5,tex = 0.5, choices= c(2,3))

## Plot w/ Arrows ----------------------------------------------------------

group <- c(paste0(KineData$ID,"_",KineData$Incline,"_hind"),paste0(KineData$ID,"_",KineData$Incline,"_fore"))
ld1 <- tapply(ldascores$LD1,group,mean)
ld2 <- tapply(ldascores$LD2,group,mean)
names <- names(ld1)
species <- as.factor(substr(names,start=1,stop =4))
levels(species) <- c("Aneides aeneus", "Aneides hardii", "Aneides lugubris", "Plethodon glutinosus")
id <- as.factor(substr(names,start=1,stop =6))
incline <- substr(names,start=7,stop =9)
incline <- gsub("_","",incline)

dat <- data.frame(cbind(species,id,incline,ld1,ld2))
dat$species <- species
dat$id <- id

# calculate the mean point for each species on each incline
# forelimb
dat2 <- dat[grep("fore",rownames(dat)),]
ld1_0 <- tapply(as.numeric(dat2$ld1[which(dat2$incline == "0")]), dat2$species[which(dat2$incline == "0")], mean)
ld1_45 <- tapply(as.numeric(dat2$ld1[which(dat2$incline == "45")]), dat2$species[which(dat2$incline == "45")], mean)
ld1_80 <- tapply(as.numeric(dat2$ld1[which(dat2$incline == "80")]), dat2$species[which(dat2$incline == "80")], mean)
ld1_90 <- tapply(as.numeric(dat2$ld1[which(dat2$incline == "90")]), dat2$species[which(dat2$incline == "90")], mean)

ld2_0 <- tapply(as.numeric(dat2$ld2[which(dat2$incline == "0")]), dat2$species[which(dat2$incline == "0")], mean)
ld2_45 <- tapply(as.numeric(dat2$ld2[which(dat2$incline == "45")]), dat2$species[which(dat2$incline == "45")], mean)
ld2_80 <- tapply(as.numeric(dat2$ld2[which(dat2$incline == "80")]), dat2$species[which(dat2$incline == "80")], mean)
ld2_90 <- tapply(as.numeric(dat2$ld2[which(dat2$incline == "90")]), dat2$species[which(dat2$incline == "90")], mean)
mean_fore <- data.frame("Species" = names(ld1_0),"Incline" = c(0,0,0,0,90,90,90,90,45,45,45,45,80,80,80,80), 
                        "LD1" = c(ld1_0,ld1_90,ld1_45,ld1_80) ,"LD2" = c(ld2_0,ld2_90,ld2_45,ld2_80))

# hindlimb
dat3 <- dat[grep("hind",rownames(dat)),]
ld1_0 <- tapply(as.numeric(dat3$ld1[which(dat3$incline == "0")]), dat3$species[which(dat3$incline == "0")], mean)
ld1_45 <- tapply(as.numeric(dat3$ld1[which(dat3$incline == "45")]), dat3$species[which(dat3$incline == "45")], mean)
ld1_80 <- tapply(as.numeric(dat3$ld1[which(dat3$incline == "80")]), dat3$species[which(dat3$incline == "80")], mean)
ld1_90 <- tapply(as.numeric(dat3$ld1[which(dat3$incline == "90")]), dat3$species[which(dat3$incline == "90")], mean)

ld2_0 <- tapply(as.numeric(dat3$ld2[which(dat3$incline == "0")]), dat3$species[which(dat3$incline == "0")], mean)
ld2_45 <- tapply(as.numeric(dat3$ld2[which(dat3$incline == "45")]), dat3$species[which(dat3$incline == "45")], mean)
ld2_80 <- tapply(as.numeric(dat3$ld2[which(dat3$incline == "80")]), dat3$species[which(dat3$incline == "80")], mean)
ld2_90 <- tapply(as.numeric(dat3$ld2[which(dat3$incline == "90")]), dat3$species[which(dat3$incline == "90")], mean)
mean_hind <- data.frame("Species" = names(ld1_0),"Incline" = c(0,0,0,0,90,90,90,90,45,45,45,45,80,80,80,80), 
                        "LD1" = c(ld1_0,ld1_90,ld1_45,ld1_80) ,"LD2" = c(ld2_0,ld2_90,ld2_45,ld2_80))

# for Figure 5
# plot the DFA with the limb, species, incline means
ggplot() + 
  geom_point(data = ldascores, mapping = aes(x = LD1, y = LD2, colour = Species, fill = Species, shape = Incline), 
             show.legend = FALSE, cex = 2, alpha = 0.4)+
  stat_ellipse(data = ldascores, mapping = aes(x = LD1, y = LD2, group=Limb), colour = "black", fill = "transparent",geom="polygon", 
               show.legend =F,level = 0.975, size = 1, linetype = 5) +
  geom_point(data = mean_fore[1:4,], mapping = aes(x = LD1, y =LD2, fill =Species, group = Species), pch = 21,
             show.legend = FALSE, size = 5) +
  geom_point(data = mean_fore[c(5,7,8),], mapping = aes(x = LD1, y =LD2, fill =Species, group = Species), pch = 24,
             show.legend = FALSE, size = 5) +  
  geom_point(data = mean_fore[c(14),], mapping = aes(x = LD1, y =LD2, fill =Species, group = Species), pch = 25,
             show.legend = FALSE, size = 5) +  
  geom_point(data = mean_hind[1:4,], mapping = aes(x = LD1, y =LD2, fill =Species, group = Species), pch = 21,
             show.legend = FALSE, size = 5) +
  geom_point(data = mean_hind[c(5,7,8),], mapping = aes(x = LD1, y =LD2, fill =Species, group = Species), pch = 24,
             show.legend = FALSE, size = 5) +  
  geom_point(data = mean_hind[c(14),], mapping = aes(x = LD1, y =LD2, fill =Species, group = Species), pch = 25,
             show.legend = FALSE, size = 5) +  
  scale_shape_manual(values = c(21,22,25,24))+
  scale_colour_manual(values = col.sp)+
  scale_fill_manual(values = col.sp)+
  labs(x= "LD1: 49.8%", y = "LD2: 16.7%")+
  xlim(-6.5,6.5) + ylim(-5.17,5.17)+
  guides(colour=guide_legend(title="Species"),
         shape=guide_legend(title="Incline"))+
  theme_classic() +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 13))

# ggsave("Figure 5.pdf", width = 5.39, height = 4.89, units = "in")

# LMER --------------------------------------------------------------------
# perform all of the different LMMs

## speed --------------------------------------------------------------------
lm <- nlme::lme(Speed~as.factor(Incline)*Species*log(SVL), random =  ~1|ID, data = KineData)
performance::r2_nakagawa(lm)
speed_sum <- emmeans(lm, ~ Species | Incline | SVL)
speed_sum <- data.frame(speed_sum[1:16])
speed_sum$Species <- factor(speed_sum$Species, levels = c("Aneides aeneus", "Aneides lugubris","Aneides hardii", "Plethodon glutinosus"))

## duty factor --------------------------------------------------------------
duty_comb <- rbind.fill(KineData[,c("Species","ID","Trial","Incline","SVL")],
                        KineData[,c("Species","ID","Trial","Incline","SVL")])
duty_comb$Limb <- c(rep("fore",nrow(KineData)),rep("hind",nrow(KineData)))
duty_comb$Duty_Factor <- c(KineData$Fore_Duty_Factor,KineData$Hind_Duty_Factor)

lm <- nlme::lme(Duty_Factor~as.factor(Incline)*Limb*Species*log(SVL), random =  ~1|ID, data = duty_comb)
performance::r2_nakagawa(lm)
duty_sum <- emmeans(lm, ~ Species | Incline | Limb | SVL)
duty_sum <- data.frame(duty_sum[1:32])
duty_sum$Species <- factor(duty_sum$Species, levels = c("Aneides aeneus", "Aneides lugubris","Aneides hardii", "Plethodon glutinosus"))

## stride length ------------------------------------------------------------
slength_comb <- rbind.fill(KineData[,c("Species","ID","Trial","Incline","SVL")],
                           KineData[,c("Species","ID","Trial","Incline","SVL")])
slength_comb$Limb <- c(rep("fore",nrow(KineData)),rep("hind",nrow(KineData)))
slength_comb$Stride_Length <- c(KineData$Fore_Stride_Length,KineData$Hind_Stride_Length)

lm <- nlme::lme(Stride_Length~as.factor(Incline)*Limb*Species*log(SVL), random =  ~1|ID, data = slength_comb)
performance::r2_nakagawa(lm)
slength_sum <- emmeans(lm, ~ Species | Incline | Limb | SVL)
slength_sum <- data.frame(slength_sum[1:32])
slength_sum$Species <- factor(slength_sum$Species, levels = c("Aneides aeneus", "Aneides lugubris","Aneides hardii", "Plethodon glutinosus"))


## stride freq --------------------------------------------------------------
sfreq_comb <- rbind.fill(KineData[,c("Species","ID","Trial","Incline","SVL")],
                         KineData[,c("Species","ID","Trial","Incline","SVL")])
sfreq_comb$Limb <- c(rep("fore",nrow(KineData)),rep("hind",nrow(KineData)))
sfreq_comb$Stride_Freq <- c(KineData$Fore_Stride_Freq,KineData$Hind_Stride_Freq)

lm <- nlme::lme(Stride_Freq~as.factor(Incline)*Limb*Species*log(SVL), random =  ~1|ID, data = sfreq_comb)
performance::r2_nakagawa(lm)
sfreq_sum <- emmeans(lm, ~ Species | Incline | Limb | SVL)
sfreq_sum <- data.frame(sfreq_sum[1:32])
sfreq_sum$Species <- factor(sfreq_sum$Species, levels = c("Aneides aeneus", "Aneides lugubris","Aneides hardii", "Plethodon glutinosus"))


## abad --------------------------------------------------------------------
abad_comb <- rbind(LMER_Prep(foreAngles,"fore")$abad,LMER_Prep(hindAngles,"hind")$abad)
abad_comb$SVL <- rep(KineData[,"SVL"],2)
abad_comb$Species <- as.factor(abad_comb$Species)
levels(abad_comb$Species) <- c("Aneides aeneus", "Aneides hardii", "Aneides lugubris", "Plethodon glutinosus")

lm <- nlme::lme(Max~Incline*Limb*Species*log(SVL), random =  ~1|ID, data = abad_comb)
performance::r2_nakagawa(lm)
em1 <- emmeans(lm, ~ Species | Incline | Limb | SVL)
em1 <- data.frame(em1[1:32])
colnames(em1)[5:6] <- c("max_mean","max_se")

lm <- nlme::lme(Min~Incline*Limb*Species*log(SVL), random =  ~1|ID, data = abad_comb)
performance::r2_nakagawa(lm)
em2 <- emmeans(lm, ~ Species | Incline | Limb | SVL)
em2 <- data.frame(em2[1:32])
colnames(em2)[5:6] <- c("min_mean","min_se")

lm <- nlme::lme(Exc~Incline*Limb*Species*log(SVL), random =  ~1|ID, data = abad_comb)
performance::r2_nakagawa(lm)
em3 <- emmeans(lm, ~ Species | Incline | Limb | SVL)
em3 <- data.frame(em3[1:32])
colnames(em3)[5:6] <- c("exc_mean","exc_se")

abad_sum <- cbind(em1[,c(1:3,5:6)],em2[,5:6],em3[,5:6])
abad_sum$Species <- factor(abad_sum$Species, levels = c("Aneides aeneus", "Aneides lugubris","Aneides hardii", "Plethodon glutinosus"))

## proret --------------------------------------------------------------------
proret_comb <- rbind(LMER_Prep(foreAngles,"fore")$proret,LMER_Prep(hindAngles,"hind")$proret)
proret_comb$SVL <- rep(KineData[,"SVL"],2)
proret_comb$Species <- as.factor(proret_comb$Species)
levels(proret_comb$Species) <- c("Aneides aeneus", "Aneides hardii", "Aneides lugubris", "Plethodon glutinosus")

lm <- nlme::lme(Max~Incline*Limb*Species*log(SVL), random =  ~1|ID, data = proret_comb)
performance::r2_nakagawa(lm)
em1 <- emmeans(lm, ~ Species | Incline | Limb | SVL)
em1 <- data.frame(em1[1:32])
colnames(em1)[5:6] <- c("max_mean","max_se")

lm <- nlme::lme(Min~Incline*Limb*Species*log(SVL), random =  ~1|ID, data = proret_comb)
performance::r2_nakagawa(lm)
em2 <- emmeans(lm, ~ Species | Incline | Limb | SVL)
em2 <- data.frame(em2[1:32])
colnames(em2)[5:6] <- c("min_mean","min_se")

lm <- nlme::lme(Exc~Incline*Limb*Species*log(SVL), random =  ~1|ID, data = proret_comb)
performance::r2_nakagawa(lm)
em3 <- emmeans(lm, ~ Species | Incline | Limb | SVL)
em3 <- data.frame(em3[1:32])
colnames(em3)[5:6] <- c("exc_mean","exc_se")

proret_sum <- cbind(em1[,c(1:3,5:6)],em2[,5:6],em3[,5:6])
proret_sum$Species <- factor(proret_sum$Species, levels = c("Aneides aeneus", "Aneides lugubris","Aneides hardii", "Plethodon glutinosus"))

## elbow/knee --------------------------------------------------------------------
elbow_comb <- rbind(LMER_Prep(foreAngles,"fore")$elbow,LMER_Prep(hindAngles,"hind")$elbow)
elbow_comb$SVL <- rep(KineData[,"SVL"],2)
elbow_comb$Species <- as.factor(elbow_comb$Species)
levels(elbow_comb$Species) <- c("Aneides aeneus", "Aneides hardii", "Aneides lugubris", "Plethodon glutinosus")

lm <- nlme::lme(Max~Incline*Limb*Species*log(SVL), random =  ~1|ID, data = elbow_comb)
performance::r2_nakagawa(lm)
em1 <- emmeans(lm, ~ Species | Incline | Limb | SVL)
em1 <- data.frame(em1[1:32])
colnames(em1)[5:6] <- c("max_mean","max_se")

lm <- nlme::lme(Min~Incline*Limb*Species*log(SVL), random =  ~1|ID, data = elbow_comb)
performance::r2_nakagawa(lm)
em2 <- emmeans(lm, ~ Species | Incline | Limb | SVL)
em2 <- data.frame(em2[1:32])
colnames(em2)[5:6] <- c("min_mean","min_se")

lm <- nlme::lme(Exc~Incline*Limb*Species*log(SVL), random =  ~1|ID, data = elbow_comb)
performance::r2_nakagawa(lm)
em3 <- emmeans(lm, ~ Species | Incline | Limb | SVL)
em3 <- data.frame(em3[1:32])
colnames(em3)[5:6] <- c("exc_mean","exc_se")

elbow_sum <- cbind(em1[,c(1:3,5:6)],em2[,5:6],em3[,5:6])
elbow_sum$Species <- factor(elbow_sum$Species, levels = c("Aneides aeneus", "Aneides lugubris","Aneides hardii", "Plethodon glutinosus"))


## wrist/ankle --------------------------------------------------------------------
wrist_comb <- rbind(LMER_Prep(foreAngles,"fore")$wrist,LMER_Prep(hindAngles,"hind")$wrist)
wrist_comb$SVL <- rep(KineData[,"SVL"],2)
wrist_comb$Species <- as.factor(wrist_comb$Species)
levels(wrist_comb$Species) <- c("Aneides aeneus", "Aneides hardii", "Aneides lugubris", "Plethodon glutinosus")

lm <- nlme::lme(Max~Incline*Limb*Species*log(SVL), random =  ~1|ID, data = wrist_comb)
performance::r2_nakagawa(lm)
em1 <- emmeans(lm, ~ Species | Incline | Limb | SVL)
em1 <- data.frame(em1[1:32])
colnames(em1)[5:6] <- c("max_mean","max_se")

lm <- nlme::lme(Min~Incline*Limb*Species*log(SVL), random =  ~1|ID, data = wrist_comb)
performance::r2_nakagawa(lm)
em2 <- emmeans(lm, ~ Species | Incline | Limb | SVL)
em2 <- data.frame(em2[1:32])
colnames(em2)[5:6] <- c("min_mean","min_se")

lm <- nlme::lme(Exc~Incline*Limb*Species*log(SVL), random =  ~1|ID, data = wrist_comb)
performance::r2_nakagawa(lm)
em3 <- emmeans(lm, ~ Species | Incline | Limb | SVL)
em3 <- data.frame(em3[1:32])
colnames(em3)[5:6] <- c("exc_mean","exc_se")

wrist_sum <- cbind(em1[,c(1:3,5:6)],em2[,5:6],em3[,5:6])
wrist_sum$Species <- factor(wrist_sum$Species, levels = c("Aneides aeneus", "Aneides lugubris","Aneides hardii", "Plethodon glutinosus"))

## yaw --------------------------------------------------------------------
yaw_comb <- rbind(LMER_Prep(foreAngles,"fore")$yaw,LMER_Prep(hindAngles,"hind")$yaw)
yaw_comb$SVL <- rep(KineData[,"SVL"],2)
yaw_comb$Species <- as.factor(yaw_comb$Species)
levels(yaw_comb$Species) <- c("Aneides aeneus", "Aneides hardii", "Aneides lugubris", "Plethodon glutinosus")

lm <- nlme::lme(Max~Incline*Limb*Species*log(SVL), random =  ~1|ID, data = yaw_comb)
performance::r2_nakagawa(lm)
em1 <- emmeans(lm, ~ Species | Incline | Limb | SVL)
em1 <- data.frame(em1[1:32])
colnames(em1)[5:6] <- c("max_mean","max_se")

lm <- nlme::lme(Min~Incline*Limb*Species*log(SVL), random =  ~1|ID, data = yaw_comb)
performance::r2_nakagawa(lm)
em2 <- emmeans(lm, ~ Species | Incline | Limb | SVL)
em2 <- data.frame(em2[1:32])
colnames(em2)[5:6] <- c("min_mean","min_se")

lm <- nlme::lme(Exc~Incline*Limb*Species*log(SVL), random =  ~1|ID, data = yaw_comb)
performance::r2_nakagawa(lm)
em3 <- emmeans(lm, ~ Species | Incline | Limb | SVL)
em3 <- data.frame(em3[1:32])
colnames(em3)[5:6] <- c("exc_mean","exc_se")

yaw_sum <- cbind(em1[,c(1:3,5:6)],em2[,5:6],em3[,5:6])
yaw_sum$Species <- factor(yaw_sum$Species, levels = c("Aneides aeneus", "Aneides lugubris","Aneides hardii", "Plethodon glutinosus"))

## foot angle --------------------------------------------------------------------
foot_comb <- rbind.fill(KineData[,c("Species","ID","Trial","Incline","SVL")],
                        KineData[,c("Species","ID","Trial","Incline","SVL")])
foot_comb$Limb <- c(rep("fore",nrow(KineData)),rep("hind",nrow(KineData)))
foot_comb$Foot_Angle <- c(KineData$Hand_Angle,KineData$Foot_Angle)

lm <- nlme::lme(Foot_Angle~as.factor(Incline)*Limb*Species*log(SVL), random =  ~1|ID, data = foot_comb)
performance::r2_nakagawa(lm)
foot_sum <- emmeans(lm, ~ Species | Incline | Limb | SVL)
foot_sum <- data.frame(foot_sum[1:32])
colnames(foot_sum)[5:6] <- c("mean","se")
foot_sum$Species <- factor(foot_sum$Species, levels = c("Aneides aeneus", "Aneides lugubris","Aneides hardii", "Plethodon glutinosus"))

## spread --------------------------------------------------------------------
spread_comb <- rbind.fill(KineData[,c("Species","ID","Trial","Incline","SVL")],
                          KineData[,c("Species","ID","Trial","Incline","SVL")])
spread_comb$Limb <- c(rep("fore",nrow(KineData)),rep("hind",nrow(KineData)))
spread_comb$Spread <- c(KineData$Hand_Spread,KineData$Foot_Spread)

lm <- nlme::lme(Spread~as.factor(Incline)*Limb*Species*log(SVL), random =  ~1|ID, data = spread_comb)
performance::r2_nakagawa(lm)
spread_sum <- emmeans(lm, ~ Species | Incline | Limb | SVL)
spread_sum <- data.frame(spread_sum[1:32])
colnames(spread_sum)[5:6] <- c("mean","se")
spread_sum$Species <- factor(spread_sum$Species, levels = c("Aneides aeneus", "Aneides lugubris","Aneides hardii", "Plethodon glutinosus"))


# Gait Boxplots -----------------------------------------------------------

# For Figure 4 plot morphology and gait parameters

a <- ggplot(speed_sum,aes(x = as.factor(Incline), y= emmean))+
  geom_pointrange(aes(ymin = emmean-SE, ymax = emmean+SE, 
                      color = Species, fill = Species, shape = Species),
                  position = position_dodge(0.6), size = 0.4,
                  show.legend = FALSE) +
  xlab("Incline (°)") + ylab("Speed (SVL/s)") +
  scale_colour_manual(values = c(col.sp)) + 
  scale_fill_manual(values = fill.sp) + 
  scale_shape_manual(values = shape.sp)+
  theme_classic()+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.title =element_text(size = 10),
        axis.text =element_text(size = 8))

b <- ggplot(duty_sum %>% filter(Limb == "hind") ,aes(x = as.factor(Incline), y= emmean))+
  geom_pointrange(aes(ymin = emmean-SE, ymax = emmean+SE, 
                      color = Species, fill = Species, shape = Species),
                  position = position_dodge(0.6), size = 0.4,
                  show.legend = FALSE) +
  xlab("Incline (°)") + ylab("Duty Factor") +
  scale_colour_manual(values = c(col.sp)) + 
  scale_fill_manual(values = fill.sp) + 
  scale_shape_manual(values = shape.sp)+
  theme_classic()+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.title =element_text(size = 10),
        axis.text =element_text(size = 8))

c <- ggplot(slength_sum %>% filter(Limb == "hind") ,aes(x = as.factor(Incline), y= emmean))+
  geom_pointrange(aes(ymin = emmean-SE, ymax = emmean+SE, 
                      color = Species, fill = Species, shape = Species),
                  position = position_dodge(0.6), size = 0.4,
                  show.legend = FALSE) +
  xlab("Incline (°)") + ylab("Stride Length (SVL)") +
  scale_colour_manual(values = c(col.sp)) + 
  scale_fill_manual(values = fill.sp) + 
  scale_shape_manual(values = shape.sp)+
  theme_classic()+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.title =element_text(size = 10),
        axis.text =element_text(size = 8))

d <- ggplot(sfreq_sum %>% filter(Limb == "hind") ,aes(x = as.factor(Incline), y= emmean))+
  geom_pointrange(aes(ymin = emmean-SE, ymax = emmean+SE, 
                      color = Species, fill = Species, shape = Species),
                  position = position_dodge(0.6), size = 0.4,
                  show.legend = T) +
  xlab("Incline (°)") + ylab("Stride Frequency (stride/s)") +
  scale_colour_manual(values = c(col.sp)) + 
  scale_fill_manual(values = fill.sp) + 
  scale_shape_manual(values = shape.sp)+
  theme_classic()+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.title =element_text(size = 10),
        axis.text =element_text(size = 8))

sub <- data.frame()
for (i in unique(KineData$ID)) {
  tmp <- KineData[which(KineData$ID == i),]
  max.climb <- max(tmp[which(tmp$Incline == 80 | tmp$Incline == 90),"Speed"])
  max.walk <- max(tmp[which(tmp$Incline == 0),"Speed"])
  tmp[which(tmp$Speed == max.climb),]
  tmp$Perf <- max.climb/max.walk
  sub <- rbind(sub,tmp[which(tmp$Speed == max.climb),])
}
sub$Species <- factor(sub$Species, levels = c("Aneides aeneus", "Aneides lugubris", "Aneides hardii", "Plethodon glutinosus"))


g <- ggplot(sub,aes(x = Species, y=(FootArea/SVL^2), fill = Species))+ 
  geom_boxplot(show.legend = FALSE)+
  xlab("Species") + ylab("Pes Area (SVL2)") +
  scale_x_discrete(labels = c('A. aen','A. lug','A. har', 'P. glu'))+
  scale_fill_manual(values = col.sp) + 
  theme_classic()+
  theme(#axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    axis.title =element_text(size = 10),
    axis.text =element_text(size = 8))

h <- ggplot(sub,aes(x = Species, y= Leg_Length/SVL, fill = Species))+ 
  geom_boxplot(show.legend = FALSE)+
  xlab("Species") + ylab("Hindlimb Length (SVL)") +
  scale_x_discrete(labels = c('A. aen','A. lug','A. har', 'P. glu'))+
  scale_fill_manual(values = col.sp) + 
  theme_classic()+
  theme(#axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    axis.title =element_text(size = 10),
    axis.text =element_text(size = 8))

h + g  + a + b + c +d + plot_layout(ncol = 2, guides = "collect")+ plot_annotation(tag_levels = "A") & theme(legend.position = 'bottom') 
ggsave("Figure 4.pdf", width = 6.77, height = 6.22, units = "in")


# Main DFA Boxplots -------------------------------------------------------

# for Figure 6 plot the main kinematic variables

#### abd max ####
a <- ggplot(abad_sum %>% filter(Limb == "fore"),aes(x = Incline, y= max_mean))+
  geom_pointrange(aes(ymin = max_mean-max_se, ymax = max_mean+max_se,                       
                      color = Species, shape = Species, fill = Species), 
                  position = position_dodge(0.6), size = 0.4) +
  xlab("Incline (°)") + ylab("") + labs(title ="Max Forelimb Abduction (°)") +
  ylim(32,54)+
  scale_colour_manual(values = c(col.sp)) + 
  scale_fill_manual(values = fill.sp) + 
  scale_shape_manual(values = shape.sp)+
  theme_classic()+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.title =element_text(size = 10),
        axis.text =element_text(size = 8),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 10))

b <- ggplot(abad_sum %>% filter(Limb == "hind"),aes(x = Incline, y= max_mean))+
  geom_pointrange(aes(ymin = max_mean-max_se, ymax = max_mean+max_se,                       
                      color = Species, shape = Species, fill = Species), 
                  position = position_dodge(0.6), size = 0.4) +
  xlab("Incline (°)") + ylab("") + labs(title ="Max Hindlimb Abduction (°)") +
  ylim(32,54)+
  scale_colour_manual(values = c(col.sp)) + 
  scale_fill_manual(values = fill.sp) + 
  scale_shape_manual(values = shape.sp)+
  theme_classic()+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.title =element_text(size = 10),
        axis.text =element_text(size = 8),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 10))

#### abad exc ####
c <- ggplot(abad_sum %>% filter(Limb == "fore"),aes(x = Incline, y= exc_mean))+
  geom_pointrange(aes(ymin = exc_mean-exc_se, ymax = exc_mean+exc_se,                       
                      color = Species, shape = Species, fill = Species), 
                  position = position_dodge(0.6), size = 0.4) +
  xlab("Incline (°)") + ylab("") + labs(title ="Forelimb Abd/Add Excursion (°)") +
  ylim(30,57)+
  scale_colour_manual(values = c(col.sp)) + 
  scale_fill_manual(values = fill.sp) + 
  scale_shape_manual(values = shape.sp)+
  theme_classic()+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.title =element_text(size = 10),
        axis.text =element_text(size = 8),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 10))

d <- ggplot(abad_sum %>% filter(Limb == "hind"),aes(x = Incline, y= exc_mean))+
  geom_pointrange(aes(ymin = exc_mean-exc_se, ymax = exc_mean+exc_se,                       
                      color = Species, shape = Species, fill = Species), 
                  position = position_dodge(0.6), size = 0.4) +
  xlab("Incline (°)") + ylab("") + labs(title ="Hindlimb Abd/Add Excursion (°)") +
  ylim(30,57)+
  scale_colour_manual(values = c(col.sp)) + 
  scale_fill_manual(values = fill.sp) + 
  scale_shape_manual(values = shape.sp)+
  theme_classic()+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.title =element_text(size = 10),
        axis.text =element_text(size = 8),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 10))

#### ret max ####
e <- ggplot(proret_sum %>% filter(Limb == "fore"),aes(x = Incline, y= min_mean))+
  geom_pointrange(aes(ymin = min_mean-min_se, ymax = min_mean+min_se, 
                      color = Species, shape = Species, fill = Species), 
                  position = position_dodge(0.6), size = 0.4) +
  xlab("Incline (°)") + ylab("") + labs(title ="Max Forelimb Retraction (°)")+
  ylim(-65,-35)+
  scale_colour_manual(values = c(col.sp)) + 
  scale_fill_manual(values = fill.sp) + 
  scale_shape_manual(values = shape.sp)+
  theme_classic()+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.title =element_text(size = 10),
        axis.text =element_text(size = 8),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 10))

f <- ggplot(proret_sum %>% filter(Limb == "hind"),aes(x = Incline, y= min_mean))+
  geom_pointrange(aes(ymin = min_mean-min_se, ymax = min_mean+min_se, 
                      color = Species, shape = Species, fill = Species), 
                  position = position_dodge(0.6), size = 0.4) +
  xlab("Incline (°)") + ylab("") + labs(title ="Max Hindlimb Retraction (°)") +
  ylim(-65,-35)+
  scale_colour_manual(values = c(col.sp)) + 
  scale_fill_manual(values = fill.sp) + 
  scale_shape_manual(values = shape.sp)+
  theme_classic()+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.title =element_text(size = 10),
        axis.text =element_text(size = 8),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 10))

#### proret exc ####
g <- ggplot(proret_sum %>% filter(Limb == "fore"),aes(x = Incline, y= exc_mean))+
  geom_pointrange(aes(ymin = exc_mean-exc_se, ymax = exc_mean+exc_se, 
                      color = Species, shape = Species, fill = Species), 
                  position = position_dodge(0.6), size = 0.4) +
  xlab("Incline (°)") + ylab("") + labs(title ="Forelimb Pro/Ret Excursion (°)")+
  ylim(70,120)+
  scale_colour_manual(values = c(col.sp)) + 
  scale_fill_manual(values = fill.sp) + 
  scale_shape_manual(values = shape.sp)+
  theme_classic()+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.title =element_text(size = 10),
        axis.text =element_text(size = 8),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 10))

h <- ggplot(proret_sum %>% filter(Limb == "hind"),aes(x = Incline, y= exc_mean))+
  geom_pointrange(aes(ymin = exc_mean-exc_se, ymax = exc_mean+exc_se, 
                      color = Species, shape = Species, fill = Species), 
                  position = position_dodge(0.6), size = 0.4) +
  xlab("Incline (°)") + ylab("") + labs(title ="Hindlimb Pro/Ret Excursion (°)") +
  ylim(70,120)+
  scale_colour_manual(values = c(col.sp)) + 
  scale_fill_manual(values = fill.sp) + 
  scale_shape_manual(values = shape.sp)+
  theme_classic()+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.title =element_text(size = 10),
        axis.text =element_text(size = 8),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 10))

#### max wrist/ankle ####
i <- ggplot(wrist_sum %>% filter(Limb == "fore"),aes(x = Incline, y= max_mean))+
  geom_pointrange(aes(ymin = max_mean-max_se, ymax = max_mean+max_se, 
                      color = Species, shape = Species, fill = Species), 
                  position = position_dodge(0.6), size = 0.4) +
  xlab("Incline (°)") + ylab("") + labs(title = "Max Wrist Extension (°)") +
  ylim(159,177)+
  scale_colour_manual(values = c(col.sp)) + 
  scale_fill_manual(values = fill.sp) + 
  scale_shape_manual(values = shape.sp)+
  theme_classic()+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.title =element_text(size = 10),
        axis.text =element_text(size = 8),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 10))

j <- ggplot(wrist_sum %>% filter(Limb == "hind"),aes(x = Incline, y= max_mean))+
  geom_pointrange(aes(ymin = max_mean-max_se, ymax = max_mean+max_se, 
                  color = Species, shape = Species, fill = Species), 
                  position = position_dodge(0.6), size = 0.4) +
  xlab("Incline (°)") + ylab("") + labs(title ="Max Ankle Extension (°)") +
  ylim(159,177)+
  scale_colour_manual(values = c(col.sp)) + 
  scale_fill_manual(values = fill.sp) + 
  scale_shape_manual(values = shape.sp)+
  theme_classic()+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.title =element_text(size = 10),
        axis.text =element_text(size = 8),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 10))

#### girdle exc ####
k <- ggplot(yaw_sum %>% filter(Limb == "fore"),aes(x = Incline, y= exc_mean))+
  geom_pointrange(aes(ymin = exc_mean-exc_se, ymax = exc_mean+exc_se, 
                      color = Species, shape = Species, fill = Species), 
                  position = position_dodge(0.6), size = 0.4) +
  xlab("Incline (°)") + ylab("") + labs(title ="Pectoral Girdle Excursion (°)") +
  ylim(18,49)+
  scale_colour_manual(values = c(col.sp)) + 
  scale_fill_manual(values = fill.sp) + 
  scale_shape_manual(values = shape.sp)+
  theme_classic()+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.title =element_text(size = 10),
        axis.text =element_text(size = 8),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 10))

l <- ggplot(yaw_sum %>% filter(Limb == "hind"),aes(x = Incline, y= exc_mean))+
  geom_pointrange(aes(ymin = exc_mean-exc_se, ymax = exc_mean+exc_se, 
                      color = Species, shape = Species, fill = Species), 
                  position = position_dodge(0.6), size = 0.4) +
  
  xlab("Incline (°)") + ylab("") + labs(title ="Pelvic Girdle Excursion (°)") +
  ylim(18,49)+
  scale_colour_manual(values = c(col.sp)) + 
  scale_fill_manual(values = fill.sp) + 
  scale_shape_manual(values = shape.sp)+
  theme_classic()+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.title =element_text(size = 10),
        axis.text =element_text(size = 8),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 10))


a + b + c + d +e +f +g +h + i + j + k + l + plot_layout(ncol = 4, guides = "collect")+ plot_annotation(tag_levels = "A") & theme(legend.position = 'bottom') 
# ggsave("Figure 6.pdf", width = 9.52, height = 7.99, units = "in")

#c + d + g+  h  + e +  f + a  +b +i + j +plot_layout(ncol = 5, guides = "collect")+ plot_annotation(tag_levels = "A") & theme(legend.position = 'bottom') 

# Foot Angle and Spread ---------------------------------------------------

# For Figure 7 plot foot orientation and limb spread 

a <- ggplot(foot_sum %>% filter(Limb == "fore"),aes(x = as.factor(Incline), y= mean))+
  geom_pointrange(aes(ymin = mean-se, ymax = mean+se, 
                      color = Species, fill = Species, shape = Species),
                  position = position_dodge(0.6), size = 0.4) +
  xlab("Incline (°)") + ylab("Manus Orientation (°)") +
  scale_colour_manual(values = c(col.sp)) + 
  ylim(-14,24)+
  scale_fill_manual(values = fill.sp) + 
  scale_shape_manual(values = shape.sp)+
  theme_classic()+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.title =element_text(size = 10),
        axis.text =element_text(size = 8),
        plot.title = element_text(size = 10))

b <- ggplot(foot_sum %>% filter(Limb == "hind"),aes(x = as.factor(Incline), y= mean))+
  geom_pointrange(aes(ymin = mean-se, ymax = mean+se, 
                      color = Species, fill = Species, shape = Species),
                  position = position_dodge(0.6), size = 0.4) +
  xlab("Incline (°)") + ylab("Pes Orientation (°)") +
  scale_colour_manual(values = c(col.sp)) + 
  ylim(-14,24)+
  scale_fill_manual(values = fill.sp) + 
  scale_shape_manual(values = shape.sp)+
  theme_classic()+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.title =element_text(size = 10),
        axis.text =element_text(size = 8),
        plot.title = element_text(size = 10))

c <- ggplot(spread_sum %>% filter(Limb == "fore"),aes(x = as.factor(Incline), y= mean))+
  geom_pointrange(aes(ymin = mean-se, ymax = mean+se, 
                      color = Species, fill = Species, shape = Species),
                  position = position_dodge(0.6), size = 0.4) +
  xlab("Incline (°)") + ylab("Forelimb Spread (SVL)") +
  scale_colour_manual(values = c(col.sp)) + 
  ylim(0.057,0.165)+
  scale_fill_manual(values = fill.sp) + 
  scale_shape_manual(values = shape.sp)+
  theme_classic()+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.title =element_text(size = 10),
        axis.text =element_text(size = 8),
        plot.title = element_text(size = 10))

d <- ggplot(spread_sum %>% filter(Limb == "hind"),aes(x = as.factor(Incline), y= mean))+
  geom_pointrange(aes(ymin = mean-se, ymax = mean+se, 
                      color = Species, fill = Species, shape = Species),
                  position = position_dodge(0.6), size = 0.4) +
  xlab("Incline (°)") + ylab("Hindlimb Spread (SVL)") +
  scale_colour_manual(values = c(col.sp)) + 
  ylim(0.057,0.165)+
  scale_fill_manual(values = fill.sp) + 
  scale_shape_manual(values = shape.sp)+
  theme_classic()+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.title =element_text(size = 10),
        axis.text =element_text(size = 8),
        plot.title = element_text(size = 10))

a + b + c +d +plot_layout(ncol = 2, guides = "collect")+ plot_annotation(tag_levels = "A") & theme(legend.position = 'bottom') 
ggsave("Figure 7.pdf", width = 6.77, height = 5, units = "in")
#ggsave("Figure 7v2.pdf", width = 9.52, height = 3, units = "in")

