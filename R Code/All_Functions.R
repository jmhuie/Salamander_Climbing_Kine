# Main Function -----------------------------------------------------

anglecalc <- function(trial,planeP1 = c(0,0,0), 
                      planeP2 = c(.1,0,0), 
                      planeP3 = c(0,.1,0)){
  #Single function to calculate angles for limb joints.
  #Trial data must have landmark XYZ coordinates for columns and frame numbers for rows
  #(col1 = 1x, col2 = 1y, col3= 1z, col4 = 2x ... col21 = 7z)
  #(row1 = frame 1, row2 = frame 2 ... row101 = frame 101)
  #Landmarks should be as follows:
  #1-3 = Anterior Midline Point
  #4-6 = Posterior Midline Point
  #7-9 = Shoulder/Hip
  #10-12 = Elbow/Knee
  #13-15 = Wrist/Ankle
  #16-18 = Metacarpo/Metatarsophalangeal joint
  #19-21 = Finger/Toe
  
  #abad - Abduction/Adduction
  abad <- abadAngle(prox = trial[, 7:9], dist = trial[,10:12], 
                    planeP1 = planeP1, planeP2 = planeP2, planeP3 = planeP3)
  
  #proret - Protraction/Retraction
  proret <- proretAngle(prox = trial[, 7:9], dist = trial[, 10:12], 
                        post = trial[, 4:6], ant = trial[, 1:3])
  
  #lar - Long-Axis Rotation
  lar <- larAngle(trial[,7:9],trial[,10:12],trial[,13:15], planeP1 = planeP1,  planeP2 = planeP2, planeP3 = planeP3)
  
  #elbow - Elbow/Knee joint angle
  elbow <- jointAngle(trial[,  13:15], trial[, 10:12], trial[, 7:9])
  
  #wrist - Wrist/Ankle joint angle
  wrist <- jointAngle(trial[, 16:18], trial[, 13:15], trial[, 10:12])
  
  #yaw - Yaw angle
  # anterior girdle point first then posterior girdle point
  yaw <- yawAngle(trial[, 1:3], trial[, 4:6], 
                  planeP1 = planeP1,  planeP2 = planeP2, planeP3 = planeP3)
  
  #foot - Foot orientation
  foot <- footAngle(trial[, 19:21], trial[, 13:15], trial[, 1:3], trial[, 4:6], 
                   planeP1 = planeP1, planeP2 = planeP2, planeP3 = planeP3)
  
  #spread - lateral spread of the limb based on the metacarpal/tarsal point
  spread <- limb_spread(trial[, 13:15],trial[, 1:3], trial[, 4:6], trial[,7:9],
                        planeP1 = planeP1, planeP2 = planeP2, planeP3 = planeP3)
  
  #Hip - height of the hip/shoulder relative to the horizontal plane
  hip <- hip_height(trial[,7:9], planeP1 = planeP1,  planeP2 = planeP2, planeP3 = planeP3)
  
  
  return(data.frame(abad, proret, lar, elbow, wrist, yaw, foot, spread, hip))
}


# Extra Math Fx -----------------------------------------------------------

## dot product between two row vectors
wdot <- function(a, b) {
  y <- a*b
  y <- t(sum(t(y)))
  return(y)
}

## Cross product between two row vectors 
vec_cross <- function(ab,ac){
  abci = ab[2] * ac[3] - ac[2] * ab[3];
  abcj = ac[1] * ab[3] - ab[1] * ac[3];
  abck = ab[1] * ac[2] - ac[1] * ab[2];
  return (c(abci, abcj, abck))
}

## calculate vector length
vlength <- function(x) {
  v <- sqrt(wdot(x, x))
  return(v)
}

distbetween3Dpts <-  function(point1, point2) {
  # Calculate the differences in coordinates
  dx <- point2[,1] - point1[,1]
  dy <- point2[,2] - point1[,2]
  dz <- point2[,3] - point1[,3]
  
  # Calculate the Euclidean distance
  distance <- sqrt(dx^2 + dy^2 + dz^2)
  
  return(distance)
}

# Abduction/Adduction -----------------------------------------------------

abadAngle <- function(prox, dist, planeP1 = c(0,0,0), 
                      planeP2 = c(.1,0,0), 
                      planeP3 = c(0,.1,0)) { 
  
  # requires the proximal joint (shoulder/ hip), distal joint (elbow/knee)
  # and three points to define the horizontal plane 
  
  FemurVector <- dist - prox 
  
  #Generate horizontal plane and calculate vector normal to it
  ## Setting up horizontal Plane
  planeVec1 <- planeP2 - planeP1
  planeVec2 <- planeP3 - planeP1
  PlaneNorm <- vec_cross(planeVec1,planeVec2)#vector normal to plane
  PlaneNorm <- abs(PlaneNorm)
  
  FemFRAngInit <- matrix()
  for (i in 1:nrow(FemurVector)) {
    FemurVectorTrans1AA <- FemurVector[i,]
    dotFemurVectorFR <- wdot(FemurVectorTrans1AA,PlaneNorm) # find dot prod between femur and normal vectors
    MagFemurVectorFR <- vlength(FemurVectorTrans1AA) # find magnitude of femur vector
    MagPlaneNorm <- vlength(PlaneNorm) # find magnitude of femur
    MagFemurVectorPlaneNorm <- MagFemurVectorFR*MagPlaneNorm
    CosdotFemurVectorFR <- dotFemurVectorFR/MagFemurVectorPlaneNorm
    FemFRAngA <- (acos(CosdotFemurVectorFR))*(180/pi) #converting radians to degree
    # %makes a one column matrix of angles of the abduction/adduction angle
    FemFRAngInit[i] <- FemFRAngA
  }
  
  FemFRAng <- (FemFRAngInit)
  #setting the zero of the angles to be perpendicular to x axis
  FemFRAng <- -(90-FemFRAng)
  
  return(FemFRAng)
}

# Protraction/Retraction --------------------------------------------------

proretAngle <- function(prox, dist, post, ant) {
  # requires the proximal joint (shoulder/ hip), distal joint (elbow/knee)
  # and two points on the midline above the girdle
  
  FemurVector <- dist - prox
  MidlineVector <- ant - post
  
  FemTVAngInit <- matrix()
  for (i in 1:nrow(FemurVector)) {
    FemurVectorTrans1AA <- FemurVector[i,]
    dotFemurVectorTV <- wdot(FemurVectorTrans1AA,MidlineVector[i,])
    MagFemurVectorTV <- vlength(FemurVectorTrans1AA)
    MagTVNorm <- vlength(MidlineVector[i,])
    MagFemurVectorTVNorm <- MagFemurVectorTV*MagTVNorm
    CosdotFemurVectorTV <- dotFemurVectorTV/MagFemurVectorTVNorm
    FemTVAngA <- (acos(CosdotFemurVectorTV))*(180/pi) #converting radians to degree
    # makes a one column matrix of angles of the protraction/retraction angle
    FemTVAngInit[i] <- FemTVAngA
  }
  
  FemTVAng <- FemTVAngInit
  FemTVAng <- (90-FemTVAng)
  
  return(FemTVAng)
}

# Long Axis ---------------------------------------------------------------
larAngle <- function(hip, knee, ankle, planeP1 = c(0,0,0), planeP2 = c(.1,0,0), planeP3 = c(0,.1,0)) {
  
  # upperlimb vector
  FemurVector <- knee - hip 
  # lowerlimb vector
  TibiaVec <- ankle - knee
  
  #Generate horizontal plane and calculate vector normal to it
  ## Setting up horizontal Plane
  planeVec1 <- planeP2 - planeP1
  planeVec2 <- planeP3 - planeP1
  PlaneNorm <- vec_cross(planeVec1,planeVec2)#vector normal to plane
  PlaneNorm <- abs(PlaneNorm)
  
  # empty data matrix for output
  LAR <- matrix(NA, nrow(hip), 1)
  for (i in 1:nrow(hip)) {
    # Arm plane normal
    LegNorm <- vec_cross(TibiaVec[i,], FemurVector[i,])
    # Remove elevation
    dot1 <- wdot(LegNorm, FemurVector[i,])
    dot2 <- wdot(FemurVector[i,], FemurVector[i,])
    LegProj <- LegNorm - c(dot1/dot2)*FemurVector[i,]
    # Project vertical same way
    dot3 <- wdot(PlaneNorm, FemurVector[i,])
    VertProj <- PlaneNorm - c(dot3/dot2)*FemurVector[i,]
    # Normalize 
    LegProj <- LegProj / c(vlength(LegProj))
    VertProj <- VertProj / c(vlength(VertProj))
    
    # Unsigned angle 
    cosang <- wdot(VertProj, LegProj) /
      (vlength(VertProj)*vlength(LegProj))
    cosang <- max(-1, min(1, cosang))
    ang <- acos(cosang)
    
    # minus 90 so that vertical is 0
    LAR[i] <- ang * (180/pi) - 90
  }
  
  return(LAR)
}



# Joint Angles ------------------------------------------------------------

jointAngle <- function(P1, P2, P3) {
  # Assume that P2 is the vertex and P1 and P3 are the other two points that form the angle
  
  P32 <- matrix(0, nrow(P2), 3)
  P12 <- matrix(0, nrow(P2), 3)
  angle_degrees <- numeric(length = nrow(P1))
  
  for (i in 1:nrow(P1)) {
    if (isTRUE(is.vector(P1))) {
      # Create vectors of the P3-P2 and P1-P2 segments
      P32 <- as.numeric(P3 - P2)
      P12 <- as.numeric(P1 - P2)
      
      angle_degrees <- atan2(vlength(vec_cross(P32, P12)), sum(P32 * P12))*(180/pi)
      
    } else {
      # Create vectors of the P3-P2 and P1-P2 segments
      P32[i,] <- as.numeric(P3[i,] - P2[i,])
      P12[i,] <- as.numeric(P1[i,] - P2[i,])
      
      angle_degrees[i] <- atan2(vlength(vec_cross(P32[i,], P12[i,])), sum(P32[i,] * P12[i,]))*(180/pi)
    }
  }
  
  return(angle_degrees)
}


# Yaw ---------------------------------------------------------------------

yawAngle<- function(P1, P2, planeP1, planeP2, planeP3) {
  # requires two points on the midline above the girdle
  # calculates yaw relative to the heading of the posterior gridle point
  
  # calculate the point inbetween the two girdle points
  P3 <- P1
  P3[,1] <- rowMeans(cbind(P1[,1],P2[,1]))
  P3[,2] <- rowMeans(cbind(P1[,2],P2[,2]))
  P3[,3] <- rowMeans(cbind(P1[,3],P2[,3]))
  # calculate the direction of the girdle point to define the sagittal plane
  StartPoint <- P3[1,]
  EndPoint <- P3[nrow(P3),]
  MidlineVector <- EndPoint - StartPoint
  
  GirdleVector <- P1 - P2
  
  #Generate horizontal plane and calculate vector normal to it
  ## Setting up frontal Plane
  planeVec1 <- planeP2 - planeP1
  planeVec2 <- planeP3 - planeP1
  PlaneNorm <- vec_cross(planeVec1,planeVec2)#vector normal to plane
  PlaneNorm <- abs(PlaneNorm)
  
  # calculate a vector perpendicular to the normal vector of horizontal plane and the direction of the animal
  newVector <- vec_cross(PlaneNorm,MidlineVector)
  
  GirdleTVAngInit <- matrix()
  for (i in 1:nrow(GirdleVector)) {
    GirdleVectorTrans1AA <- GirdleVector[i,]
    dotGirdleVectorTV <- wdot(GirdleVectorTrans1AA,newVector)
    MagGirdleVectorTV <- vlength(GirdleVectorTrans1AA)
    MagTVNorm <- vlength(newVector)
    MagGirdleVectorTVNorm <- MagGirdleVectorTV*MagTVNorm
    CosdotGirdleVectorTV <- dotGirdleVectorTV/MagGirdleVectorTVNorm
    GirdleTVAngA <- (acos(CosdotGirdleVectorTV))*(180/pi) #converting radians to degree
    GirdleTVAngInit[i] <- GirdleTVAngA
  }
  
  yaw <- (90-GirdleTVAngInit)
  
  return(yaw)
}

# Foot Angle ---------------------------------------------------------------

footAngle <- function(dist, prox, P1, P2, planeP1, planeP2, planeP3) {
  # requires toe and metacarpa/tarsal point and two points on the midline above the girdle
  # calculates angle of the toe relative to the heading of the posterior gridle point
  
  # calculate the point inbetween the two girdle points
  P3 <- P1
  P3[,1] <- rowMeans(cbind(P1[,1],P2[,1]))
  P3[,2] <- rowMeans(cbind(P1[,2],P2[,2]))
  P3[,3] <- rowMeans(cbind(P1[,3],P2[,3]))
  # calculate the direction of the girdle point to define the sagittal plane
  StartPoint <- P3[1,]
  EndPoint <- P3[nrow(P3),]
  MidlineVector <- EndPoint - StartPoint
  
  ToeVector <- dist - prox
  
  #Generate horizontal plane and calculate vector normal to it
  ## Setting up frontal Plane
  planeVec1 <- planeP2 - planeP1
  planeVec2 <- planeP3 - planeP1
  PlaneNorm <- vec_cross(planeVec1,planeVec2)#vector normal to plane
  PlaneNorm <- abs(PlaneNorm)
  
  # calculate a vector perpendicular to the normal vector of horizontal plane and the direction of the animal
  newVector <- vec_cross(PlaneNorm,MidlineVector)
  
  ToeTVAngInit <- matrix()
  for (i in 1:nrow(ToeVector)) {
    ToeVectorTrans1AA <- ToeVector[i,]
    dotToeVectorTV <- wdot(ToeVectorTrans1AA,newVector)
    MagToeVectorTV <- vlength(ToeVectorTrans1AA)
    MagTVNorm <- vlength(newVector)
    MagToeVectorTVNorm <- MagToeVectorTV*MagTVNorm
    CosdotToeVectorTV <- dotToeVectorTV/MagToeVectorTVNorm
    ToeTVAngA <- (acos(CosdotToeVectorTV))*(180/pi) #converting radians to degree
    ToeTVAngInit[i] <- ToeTVAngA
  }
  
  yaw <- (90-ToeTVAngInit)
  
  return(yaw)
}

# Limb Spread -------------------------------------------------------------

limb_spread <- function(point, P1, P2, P4, planeP1 = c(0,0,0), 
                        planeP2 = c(.1,0,0), 
                        planeP3 = c(0,.1,0)) {
  # calculate the horizontal distance betweem the foot and a sagittal plane
  
  # calculate the point inbetween the two girdle points
  P3 <- P1
  P3[,1] <- rowMeans(cbind(P1[,1],P2[,1]))
  P3[,2] <- rowMeans(cbind(P1[,2],P2[,2]))
  P3[,3] <- rowMeans(cbind(P1[,3],P2[,3]))
  # calculate the direction of the girdle point to define the sagittal plane
  StartPoint <- P4[1,]
  EndPoint <- P4[nrow(P4),]
  MidlineVector <- EndPoint - StartPoint
  
  # calculate horizontal plane norm
  planeVec1 <- planeP2 - planeP1
  planeVec2 <- planeP3 - planeP1
  HorizPlaneNorm <- vec_cross(planeVec1,planeVec2)#vector normal to plane
  
  #calculate the normal of the sagittal plane
  SagPlaneNorm <- vec_cross(HorizPlaneNorm, MidlineVector)
  
  #calculate the sagittal plane coefficients
  A <- SagPlaneNorm[1]
  B <- SagPlaneNorm[2]
  C <- SagPlaneNorm[3]
  D <- -(A * StartPoint[1] + B * StartPoint[2] + C * StartPoint[3])
  
  spread <- c()
  for (i in 1:nrow(point)){
    numerator <- abs(A * point[i,1] + B * point[i,2] + C * point[i,3] + D)
    denominator <- sqrt(A^2 + B^2 + C^2)
    spread <-c(spread,c(numerator / denominator))
  }
  names(spread) <- NULL
  
  # Output the distance
  return(spread)
}

# Hip Height --------------------------------------------------------------

hip_height <- function(point, planeP1 = c(0,0,0), 
                       planeP2 = c(.1,0,0), 
                       planeP3 = c(0,.1,0)) {
  # calculate the vertical height of a point relative to the horizontal plane
  
  ## Setting up horizontal Plane
  planeVec1 <- planeP2 - planeP1
  planeVec2 <- planeP3 - planeP1
  PlaneNorm <- vec_cross(planeVec1,planeVec2)#vector normal to plane
  
  # Coefficients of the plane equation (A, B, C)
  A <- PlaneNorm[1]
  B <- PlaneNorm[2]
  C <- PlaneNorm[3]
  
  # Find D using point P1
  D <- -(A * planeP1[1] + B * planeP1[2] + C * planeP1[3])
  
  # Calculate the distance from the point to the plane
  distance <- c()
  for (i in 1:nrow(point)){
    numerator <- abs(A * point[i,1] + B * point[i,2] + C * point[i,3] + D)
    denominator <- sqrt(A^2 + B^2 + C^2)
    distance <-c(distance,c(numerator / denominator))
  }
  names(distance) <- NULL
  
  # Output the distance
  return(distance)
}



# Speed -------------------------------------------------------------------

speedKine <- function(data,fps) {
  crop_data <- data[c(1,nrow(data)), 1:3]
  dist <- (sqrt((crop_data[1,1]-crop_data[2,1])^2+(crop_data[1,2]-crop_data[2,2])^2+(crop_data[1,3]-crop_data[2,3])^2))
  frames <- as.numeric(rownames(crop_data))[nrow(crop_data)] - as.numeric(rownames(crop_data))[1]
  time <- frames/fps
  speed <- dist/time
  return(list("speed" = as.numeric(speed), 
              "dist" =  as.numeric(dist), 
              "time" = as.numeric(time)))
}

# Limb Length -------------------------------------------------------------

limb_length <- function(P1, P2, P3, P4 = NULL, foot = FALSE) {
  # requires points for the hip, elbow, wrist, and toe
  # calculates the length of each segment and sums it
  # have the option to ignore the foot length and only use upper and lower limb length
  
  upper <- distbetween3Dpts(P1,P2)
  lower <- distbetween3Dpts(P2,P3)
  
  if (foot == TRUE & !is.null(P4)) {
    foot <- distbetween3Dpts(P3,P4)
    limblength <- upper + lower + foot
  } else {
    limblength <- upper + lower
  }
  
  limblength <- mean(limblength)
  
  return(limblength)
}

# General Plotting --------------------------------------------------------

PlotKine <- function(data, ylab, rect = F, ylim = NULL, var = "Trial", col = c(1:3)) {
  # function for plotting the kinematic profiles
  plot <- ggplot(data=data, aes(x=rep(0:100,round(nrow(data)/100)), y=Mean, colour=data[,var], linetype = data[,var]))
  if (rect == T) {
    plot <- plot + geom_rect(xmin=-Inf, xmax=Inf,ymin=-Inf, ymax=0, alpha=.3,fill="grey90",colour = "transparent")
  }
  plot <- plot +
    geom_ribbon(aes(ymax=Max, ymin=Min,fill=data[,var]), alpha=0.7, colour = "transparent")+
    geom_line(linewidth=1.4, alpha=1)+
    labs( x = "% of Stride", y = paste0(ylab)) +
    scale_fill_manual(label =c("Aneides aeneus","Aneides hardii","Aneides lugubris","Plethodon glutinosus"),values = c("darkgrey","#8c510a","#35978f","purple"))+
    scale_colour_manual(label =c("Aneides aeneus","Aneides hardii","Aneides lugubris","Plethodon glutinosus"),values = c("darkgrey","#8c510a","#35978f","purple"))+
    scale_x_continuous(limits = c(0,100), expand = c(0, 0))+
    guides(colour=guide_legend(title=var),
           linetype="none",
           fill =guide_legend(title=var))
  if (is.null(ylim) == F) {
    plot <- plot + ylim(ylim)
  }
  plot <- plot +  
    theme(plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"))+
    theme(axis.title.x=element_text(colour="black"))+ # vjust=0 puts a little more spacing btwn the axis text and label
    theme(axis.title.y=element_text(colour='black'))+
    theme(axis.title = element_text(size = 10))+
    theme(axis.text.x=element_text(colour='black'))+
    theme(axis.text.y=element_text(colour='black'))+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+ # get rid of gridlines
    theme(panel.background=element_blank())+ # make background white
    theme(axis.line=element_line(colour="black", linetype="solid")) # put black lines for axes
  return(plot)
}

# Prep for LME ------------------------------------------------------------
# prepare tthe 
LMER_Prep <- function(list,  limb) {
  summary <- list()
  for (y in colnames(list[[1]])) {
    tmp <- data.frame(matrix(nrow = length(list), ncol = 9))
    colnames(tmp) <- c("Species","ID","Trial","Incline","Limb","Max","Mean","Min","Exc")
    tmp$ID <- sapply(strsplit(names(list), "_"),"[[",1)
    tmp$Species <- gsub('[0-9]+', '', tmp$ID)
    tmp$Trial <- names(list)
    tmp$Incline <- sapply(strsplit(names(list), "_"),"[[",2)
    tmp$Incline[grep("Flat",tmp$Incline)] <- 0
    tmp$Incline[grep("45deg",tmp$Incline)] <- 45
    tmp$Incline[grep("80deg",tmp$Incline)] <- 80
    tmp$Incline[grep("Vert",tmp$Incline)] <- 90
    tmp$Limb <- limb
    for (i in 1:length(list)) {
      tmp$Max[i] <- max(list[[i]][,y])
      tmp$Min[i] <- min(list[[i]][,y])
      tmp$Mean[i] <- mean(list[[i]][,y])
      tmp$Exc[i] <- tmp$Max[i]-tmp$Min[i]
    }
    summary <- append(summary,list(tmp))
  }
  names(summary) <- colnames(list[[1]])
  return(summary)
}


# DFA Biplot --------------------------------------------------------------

lda.arrows <- function(x, myscale = 1, tex = 0.75, choices = c(1,2), ...){
  ## adds `biplot` arrows to an lda using the discriminant function values
  heads <- coef(x)
  arrows(x0 = 0, y0 = 0,
         x1 = myscale * heads[,choices[1]],
         y1 = myscale * heads[,choices[2]], length = .1,...)
  text(myscale * heads[,choices], labels = row.names(heads),
       cex = tex, pos = c(2,4,3,4,1))
}
