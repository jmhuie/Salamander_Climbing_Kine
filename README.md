# Salamander Climbing Kinematics

### About
This repository contains the data and scripts for "Limb kinematics and morphology improve salamander climbing performance"

### Content
- .../CameraCalibs/
    - /MayaCams/
		- A directory containing the camera calibrations from each filming session. Files were extracted from XMALab files in MayaCam format with the <code>extract_mayacam</code> function in xma2dlc Python script.
	- /CalibPlanes.csv
		- A .csv file containing coordinates to define a horizontal plane for each filming session.

- .../Landmarks/
	- /2Dpoints/
		- A directory of 2D coordinate files predicted by the DLC model for each trial.
	- /3Dpoints/
		- A directory of 3D coordinate files converted from 2D with the <code>convert_2d_to_3d</code> function in xma2dlc Python script.
		
- .../Python Code/
	- /xma2dlc.py
		- A Python script file containing functions for creating a DLC network with training data from XMALab, analyzing new videos, extracting camera calibrations,
		converting 2D coordinates to 3D, and more. Some functions were modified from the [XROMM Tools](https://github.com/jdlaurence/XROMM_DLCTools) as part of [Laurence-Chasen et al. 2020](https://doi.org/10.1242/jeb.226720).
		
- .../R Code/
	- /All_Functions.R
		- A R script file containing auxiliary functions including, but not limited to, functions used to calculate kinematic angles and measurements using the 3D coordinates.
	- /ClimbKine_Mainscript_Pub.R
		- The main R script file used to conduct the analyses performed for this study and plot data figures.
	- /ClimbKineMeta.csv
		- A .csv file containing the metadata for each trial including the species, ID, mass (g), snout-vent length (SVL; cm), total body length (BL; cm), hand and foot area (cm^2), trial number, date filmed,
		calibration filename, camera frame rate, video decimation prior to analysis, trial incline, the frame numbers indicating the start and end of stance and swing phase for the hindlimb
		and ipsilateral forelimb strides, and measurements of digital spread.	
	- /ClimbKineData.csv
		- A .csv file containing similar content as ClimbKineMeta.csv as well as the calculated limb lengths and raw spatiotemporal gait parameters calculated in R. Variables include speed (cm/s), stride lengths (cm), limb lengths (cm), duty factor,
		stride duration (s), and stride frequencies (stride/s). Made and saved in <code>ClimbKine_Mainscript_Pub.R</code>.
	- /Code.Rproj
		- A R project file. Should be used to launch R studio.
	- /fore3Dpts_all.rds
		- A R .rds file containing the 3D coordinates for all forelimb strides after interpolating to 101 frames. Saved from <code>ClimbKine_Mainscript_Pub.R</code> to save time re-running code.
	- /foreAngles_all.rds
		- A R .rds file containing the angular and linear kinematic variables calculated for all forelimb strides. Saved from <code>ClimbKine_Mainscript_Pub.R</code> to save time re-running code.
	- /hind3Dpts_all.rds
		- A R .rds file containing the 3D coordinates for all hindlimb strides after interpolating to 101 frames. Saved from <code>ClimbKine_Mainscript_Pub.R</code> to save time re-running code.
	- /hindAngles_all.rds
		- A R .rds file containing the angular and linear kinematic variables calculated for all hindlimb strides. Saved from <code>ClimbKine_Mainscript_Pub.R</code> to save time re-running code.

