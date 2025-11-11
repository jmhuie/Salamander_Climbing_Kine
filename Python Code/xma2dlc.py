"""
XMALab to DeepLabCut
Developed by Jonathan M. Huie

Functions modified "XROMM Tools for DeepLabCut" developed by J.D. Laurence-Chasen:
- xma_to_dlc: create DeepLabCut training dataset from data tracked in XMALab
- dlc_to_xma: convert output of DeepLabCut to XMALab format 2D points file
- analyze_xromm_videos: Predict 2D points for novel trials
- add_frames: Add new frames corrected/tracked in XMALab to an existing training dataset

Original Functions
- extract_error_videos: identify video with highest proportion of low likelihood points
- extract_error_frames: identify frames from videos with the lowest prediction likelihoods
- extract_mayacam: export mayacam calibration files from XMALab files
- IterativeLS_triangulate: triangulates 3D points from 3D with error, copies XMALab
- read_maya_cam: reads the mayacam calibration files 
- convert_2d_to_3d: converts 2D DLC points into 3D points

"""
import os
import pandas as pd
import numpy as np
import cv2
import shutil
import zipfile
import glob

#from deeplabcut.gui.tabs import analyze_videos
from deeplabcut.pose_estimation_tensorflow.predict_videos import analyze_videos
from deeplabcut.utils.auxiliaryfunctions import read_config
# from deeplabcut.create_project.add import add_new_videos


def xma_to_dlc(config_path, trials_path, points_path):
    cfg = read_config(config_path)
    scorer = cfg['scorer']
    config = config_path[:-12]
    cameras = [1, 2]
    #picked_frames = []
    dfs = []
    pnames = []
    subs = [["c01", "c1", "C01", "C1", "Cam1", "cam1", "Cam01", "cam01", "Camera1", "camera1", "Dor"],
            ["c02", "c2", "C02", "C2", "Cam2", "cam2", "Cam02", "cam02", "Camera2", "camera2", "Lat"]]
    pts = ["2Dpts", "2dpts", "2DPts", "2dPts", "pts2D", "Pts2D", "pts2d", "points2D", "Points2d", "points2d",
           "2Dpoints", "2dpoints", "2DPoints"]
    train = ["train", "Train", "training", "Training"]

    ### PART 1: Pick frames for dataset

    data = []
    for file in sorted(os.listdir(points_path)):
        if '.csv' in file:
            file_path = points_path + "/" + file
            df = pd.read_csv(file_path, na_values="NaN")
            frames = df.dropna().index.astype(str).tolist()
            frames = [str(int(f) + 1) for f in frames]
            name = file.replace('_2Dpts_train.csv', '')
            row = [name] + frames
            data.append(row)

    # Make all rows the same length by padding with NaN
    max_length = max(len(row) for row in data)
    data_padded = [row + [np.nan] * (max_length - len(row)) for row in data]

    # Convert to DataFrame
    f = pd.DataFrame(data_padded)
    trialnames = list(f.iloc[:, 0])  # first column of frames file must be trialnames
    picked_frames = []
    # this is disgusting code
    for row in range(f.shape[0]):
        picked_frames.append(list(f.loc[row, 1:]))
    for count, row in enumerate(picked_frames):
        picked_frames[count] = [x for x in row if str(x) != 'nan']  # remove nans
    for count, row in enumerate(picked_frames):
        picked_frames[count] = [int(x) for x in row]  # convert to int

    for trial in trialnames:
        # Read 2D points file
        contents = os.listdir(points_path)
        filename = [x for x in contents if ".csv" in x and trial in x]  # csv filename
        df1 = pd.read_csv(points_path + "/" + filename[0], sep=',', header=None)

        # read pointnames from header row
        pointnames = df1.loc[0, ::4].astype(str).str[:-7].tolist()
        pnames.append(pointnames)

        df1 = df1.loc[1:, ].reset_index(drop=True)  # remove header row

        ncol = df1.shape[1]

        dfs.append(df1)

    # a couple errors
    # if pointnames aren't the same across trials
    if any(pnames[0] != x for x in pnames):
        raise ValueError('Make sure body part names are consistent across trials')


    ### Part 2: Extract images and 2D point data

    for trialnum, trial in enumerate(trialnames):
        relnames = []
        data = pd.DataFrame()

        # new training dataset folder
        newpath = config + "/labeled-data/" + trial
        h5_save_path = newpath + "/CollectedData_" + scorer + ".h5"
        csv_save_path = newpath + "/CollectedData_" + scorer + ".csv"

        if not os.path.exists(newpath):
            os.makedirs(newpath)  # make new folder

        for camera in cameras:

            # get video file
            file = []
            for filename in os.listdir(trials_path + "/" + trial):
                if '.DS_Store' in filename:
                    os.remove(trials_path + "/" + trial +"/"+filename)
            contents = os.listdir(trials_path + "/" + trial)
            
            for name in contents:
                if any(x in name for x in subs[camera - 1]):
                    file = name
                    trialcam = os.path.splitext(file)[0]
            if not file:
                raise ValueError('Cannot locate %s video file or image folder' % trialcam)

            # add video the video set
            # if trialcam not in trialnames:
                # add_new_videos(config_path, video, copy_videos = False)

            # file is actually a file
            # extract frames from video and convert to png
            print("Extracting images and 2D points from %s..." % (trialcam))
            video = trials_path + "/" + trial + "/" + file
            relpath = "labeled-data/" + trial + "/"
            frames = picked_frames[trialnum]
            frames.sort()
            cap = cv2.VideoCapture(video)
            success, image = cap.read()
            count = 0
            while success:
                if count + 1 in frames:
                    imgname = trialcam + "_%s.png" % str(count + 1).zfill(4)
                    relname = relpath + imgname
                    relnames = relnames + [relname]
                    contents = os.listdir(newpath)
                    if imgname in contents:
                        print('Point data for %s already extracted.' % imgname)
                    else:
                        cv2.imwrite(newpath + "/"  + imgname,
                                image)  # save frame
                success, image = cap.read()
                count += 1
            cap.release()

            # extract 2D points data
            contents = os.listdir(points_path)
            pointsfile = [x for x in contents if '.csv' in x and trial in x]

            if not pointsfile:
                raise ValueError('Cannot locate %s 2D points file' % trial)

            # if multiple csv files, look for "2Dpoints" in the name
            if len(pointsfile) > 1:
                t = []
                for q in pointsfile:
                    if any(x in q for x in pts):
                        t = t + [q]
                # if there are multiple 2D points files, look for "train" in the name
                if len(t) > 1:
                    for r in pointsfile:
                        if any(x in r for x in train):
                            pointsfile = r
            else:
                pointsfile = pointsfile[0]
            if isinstance(pointsfile, str) != True:
                raise ValueError('Please check the points files in training points folder')

            df = pd.read_csv(points_path + '/' + pointsfile, sep=',', header=None)
            df = df.loc[1:, ].reset_index(drop=True)
            frames = [x - 1 for x in frames]  # account for zero index in python
            xpos = df.iloc[frames, 0 + (camera - 1) * 2::4]
            ypos = df.iloc[frames, 1 + (camera - 1) * 2::4]
            temp_data = pd.concat([xpos, ypos], axis=1).sort_index(axis=1)
            temp_data.columns = range(temp_data.shape[1])
            data = pd.concat([data, temp_data])


            ### Part 3: Complete final structure of datafiles
            dataFrame = pd.DataFrame()
            temp = np.empty((data.shape[0], 2,))
            temp[:] = np.nan
            for i, bodypart in enumerate(pointnames):
                index = pd.MultiIndex.from_product([[scorer], [bodypart], ['x', 'y']],
                                                    names=['scorer', 'bodyparts', 'coords'])
                frame = pd.DataFrame(temp, columns=index, index=relnames)
                frame.iloc[:, 0:2] = data.iloc[:, 2 * i:2 * i + 2].values.astype(float)
                dataFrame = pd.concat([dataFrame, frame], axis=1)
            dataFrame.replace('', np.nan, inplace=True)
            dataFrame.replace(' NaN', np.nan, inplace=True)
            dataFrame.replace(' NaN ', np.nan, inplace=True)
            dataFrame.replace('NaN ', np.nan, inplace=True)
            dataFrame.apply(pd.to_numeric)
            dataFrame.to_hdf(h5_save_path, key="df_with_missing", mode="w")
            dataFrame.to_csv(csv_save_path, na_rep='NaN')
    print("...done.")
    print("Training data extracted to projectpath/labeled-data. Now use deeplabcut.create_training_dataset")

def dlc_to_xma(cam1data, cam2data, trialname, savepath):
    h5_save_path = savepath + "/" + trialname + "_2Dpts_predicted.h5"
    csv_save_path = savepath + "/" + trialname + "_2Dpts_predicted.csv"

    if isinstance(cam1data, str):  # is string
        if ".csv" in cam1data:

            cam1data = pd.read_csv(cam1data, sep=',', header=None)
            cam2data = pd.read_csv(cam2data, sep=',', header=None)
            pointnames = list(cam1data.loc[1, 1:].unique())

            # reformat CSV / get rid of headers
            cam1data = cam1data.loc[3:, 1:]
            cam1data.columns = range(cam1data.shape[1])
            cam1data.index = range(cam1data.shape[0])
            cam2data = cam2data.loc[3:, 1:]
            cam2data.columns = range(cam2data.shape[1])
            cam2data.index = range(cam2data.shape[0])

        elif ".h5" in cam1data:  # is .h5 file
            cam1data = pd.read_hdf(cam1data)
            cam2data = pd.read_hdf(cam2data)
            pointnames = list(cam1data.columns.get_level_values('bodyparts').unique())

        else:
            raise ValueError('2D point input is not in correct format')
    else:

        pointnames = list(cam1data.columns.get_level_values('bodyparts').unique())

    # make new column names
    nvar = len(pointnames)
    pointnames = [item for item in pointnames for repetitions in range(4)]
    post = ["_cam1_X", "_cam1_Y", "_cam2_X", "_cam2_Y"] * nvar
    cols = [m + str(n) for m, n in zip(pointnames, post)]

    # remove likelihood columns
    cam1data = cam1data.drop(cam1data.columns[2::3], axis=1)
    cam2data = cam2data.drop(cam2data.columns[2::3], axis=1)

    # replace col names with new indices
    c1cols = list(range(0, cam1data.shape[1] * 2, 4)) + list(range(1, cam1data.shape[1] * 2, 4))
    c2cols = list(range(2, cam1data.shape[1] * 2, 4)) + list(range(3, cam1data.shape[1] * 2, 4))
    c1cols.sort()
    c2cols.sort()
    cam1data.columns = c1cols
    cam2data.columns = c2cols

    df = pd.concat([cam1data, cam2data], axis=1).sort_index(axis=1)
    df.columns = cols
    df.to_hdf(h5_save_path, key="df_with_missing", mode="w")
    df.to_csv(csv_save_path, na_rep='NaN', index=False)

def analyze_xromm_videos(config_path, save_path, trials_path, iteration, gputouse=0):
    # assumes you have cam1 and cam2 videos as .avi in their own separate trial folders
    # assumes all folders w/i trials_path are trial folders
    # convert jpg stacks?

    # analyze videos
    cameras = [1, 2]
    config = config_path
    subs = [["c01", "c1", "C01", "C1", "Cam1", "cam1", "Cam01", "cam01", "Camera1", "camera1", "Dor"],
            ["c02", "c2", "C02", "C2", "Cam2", "cam2", "Cam02", "cam02", "Camera2", "camera2", "Lat"]]
    trialnames = os.listdir(trials_path)

    for trialnum, trial in enumerate(trialnames):
        trialpath = trials_path + "/" + trial
        for filename in os.listdir(trialpath):
            if '.DS_Store' in filename:
                os.remove(trialpath+"/"+filename)
        contents = os.listdir(trialpath)
        savepath = save_path + "/Analyzed_Trials/" + trial + "/" + "it%d" % iteration
        # savepath = trialpath + "/" + "it%d" % iteration
        if os.path.exists(savepath):
            temp = os.listdir(savepath)
            if temp:
                # raise ValueError('There are already predicted points in iteration %d subfolders' %iteration)
                print('There are already predicted points in iteration %d subfolders' % iteration)
        else:
            os.makedirs(savepath)  # make new folder
            # get video file
            for camera in cameras:
                file = []
                for name in contents:
                    if any(x in name for x in subs[camera - 1]):
                        file = name
                if not file:
                    raise ValueError('Cannot locate %s video file or image folder' % trial)

                video = trialpath + "/" + file
                # analyze video
                analyze_videos(config, [video], destfolder=savepath, save_as_csv=True, gputouse=gputouse)

            # get filenames and read analyzed data
            contents = os.listdir(savepath)
            datafiles = [s for s in contents if '.h5' in s]
            if not datafiles:
                raise ValueError('Cannot find predicted points. Some wrong with DeepLabCut?')
            cam1data = pd.read_hdf(savepath + "/" + datafiles[0])
            cam2data = pd.read_hdf(savepath + "/" + datafiles[1])
            dlc_to_xma(cam1data, cam2data, trial, savepath)

def extract_error_videos(analyzed_path, iteration = 0, pcut = 0.6, ntrials = 10):

    subs = [["c01", "c1", "C01", "C1", "Cam1", "cam1", "Cam01", "cam01", "Camera1", "camera1", "Dor"],
            ["c02", "c2", "C02", "C2", "Cam2", "cam2", "Cam02", "cam02", "Camera2", "camera2", "Lat"]]
    data = []

    trials = os.listdir(analyzed_path)

    for i in trials:
        it_path = os.path.join(analyzed_path, i, f"it{iteration}")
        if not os.path.exists(it_path):
            continue

        files = os.listdir(it_path)

        # Find DorDLC .csv file
        cam1_files = [f for f in files if any(tag in f for tag in subs[0]) and f.endswith(".csv")]
        for f in files:
            if f.endswith(".csv"):
                for tag in subs[0]:
                    if tag in f:
                        cam1_match = tag
                        break
        if not cam1_files:
            continue
        cam1data_path = os.path.join(it_path, cam1_files[0])
        cam1data = pd.read_csv(cam1data_path, skiprows=2)
        cam1_like = cam1data.filter(like="likelihood")
        cam1_count = np.sum(cam1_like.min(axis=1) < pcut)
        cam1_percent = round(cam1_count / len(cam1data), 3) * 100

        # Find LatDLC .csv file
        cam2_files = [f for f in files if any(tag in f for tag in subs[1]) and f.endswith(".csv")]
        for f in files:
            if f.endswith(".csv"):
                for tag in subs[1]:
                    if tag in f:
                        cam2_match = tag
                        break
        if not cam2_files:
            continue
        cam2data_path = os.path.join(it_path, cam2_files[0])
        cam2data = pd.read_csv(cam2data_path, skiprows=2)
        cam2_like = cam2data.filter(like="likelihood")
        cam2_count = np.sum(cam2_like.min(axis=1) < pcut)
        cam2_percent = round(cam2_count / len(cam2data), 3) * 100

        # Combine both
        comb_like = pd.concat([cam1_like, cam2_like], axis=1)
        comb_count = np.sum(comb_like.min(axis=1) < pcut)
        comb_percent = round(comb_count / len(comb_like), 3) * 100

        data.append([i, cam1_percent, cam2_percent, comb_percent, cam1_count, cam2_count, comb_count])

    # Create and process DataFrame
    df = pd.DataFrame(data, columns=[
        "Trial", cam1_match + "_percent", cam2_match + "_percent", "total_percent",
        cam1_match + "_count", cam2_match + "_count", "total_count"
    ])

    df[[cam1_match + "_count", cam2_match + "_count", "total_count", "total_percent"]] = df[[
        cam1_match + "_count", cam2_match + "_count", "total_count", "total_percent"
    ]].apply(pd.to_numeric)

    df = df.sort_values(by="total_percent", ascending=False)

    return df.head(ntrials)

def extract_error_frames(analyzed_path, iteration = 0, pcut = 0.6, ntrials = 10, save = False):

    subs = [["c01", "c1", "C01", "C1", "Cam1", "cam1", "Cam01", "cam01", "Camera1", "camera1", "Dor"],
            ["c02", "c2", "C02", "C2", "Cam2", "cam2", "Cam02", "cam02", "Camera2", "camera2", "Lat"]]

    toperror = extract_error_videos(analyzed_path, iteration, pcut, ntrials)

    correct = []

    for trial_name in toperror["Trial"]:
        it_path = os.path.join(analyzed_path, trial_name, f"it{iteration}")
        if not os.path.exists(it_path):
            continue

        files = os.listdir(it_path)

        # Find Dor (cam1) file
        cam1_files = [f for f in files if any(tag in f for tag in subs[0]) and f.endswith(".csv")]
        if not cam1_files:
            continue
        cam1data_path = os.path.join(it_path, cam1_files[0])
        cam1data = pd.read_csv(cam1data_path, skiprows=2)
        cam1_like = cam1data.filter(like="likelihood")

        # Find Lat (cam2) file
        cam2_files = [f for f in files if any(tag in f for tag in subs[1]) and f.endswith(".csv")]
        if not cam2_files:
            continue
        cam2data_path = os.path.join(it_path, cam2_files[0])
        cam2data = pd.read_csv(cam2data_path, skiprows=2)
        cam2_like = cam2data.filter(like="likelihood")

        # Combine and find 10 worst frames (lowest min-likelihood across all points)
        comb_like = pd.concat([cam1_like, cam2_like], axis=1)
        comb_min = comb_like.min(axis=1)
        comb_bad_indices = (comb_min.nsmallest(10).index + 1).tolist()

        # Store the trial name + frame indices
        correct.append([trial_name] + comb_bad_indices)

    # Create DataFrame (empty cells will be NaN if fewer than 10 frames)
    correct_df = pd.DataFrame(correct)

    # Save to CSV if requested
    if save:
        parent_dir = os.path.abspath(os.path.join(analyzed_path, "..")) + "/Corrections"
        if not os.path.exists(parent_dir):
            os.makedirs(parent_dir)
        save_path = os.path.join(parent_dir, "it" +str(iteration) + "_frames_to_correct.csv")
        correct_df.to_csv(save_path, index=False, header=False)
        correct_dir = os.path.abspath(os.path.join(analyzed_path, "..")) + "/Corrections/it" +str(iteration) + "_corrected"
        if not os.path.exists(correct_dir):
            os.makedirs(correct_dir)

    return correct_df

def add_frames(config_path, trials_path, corrected_path, frames_path):
    cfg = read_config(config_path)
    scorer = cfg['scorer']
    config = config_path[:-12]
    cameras = [1, 2]
    #picked_frames = []
    dfs = []
    pnames = []
    subs = [["c01", "c1", "C01", "C1", "Cam1", "cam1", "Cam01", "cam01", "Camera1", "camera1", "Dor"],
            ["c02", "c2", "C02", "C2", "Cam2", "cam2", "Cam02", "cam02", "Camera2", "camera2", "Lat"]]
    pts = ["2Dpts", "2dpts", "2DPts", "2dPts", "pts2D", "Pts2D", "pts2d", "points2D", "Points2d", "points2d",
           "2Dpoints", "2dpoints", "2DPoints"]
    corr = ["correct","Correct","corrected","Corrected"]

    ### PART 1: Read frames for dataset
    # read frames from csv
    if '.csv' in frames_path:
        f = pd.read_csv(frames_path,header=None)
        trialnames = list(f.iloc[:,0]) # first row of frames file must be trialnames
        picked_frames = []
        # this is disgusting code
        for row in range(f.shape[0]):
            picked_frames.append(list(f.loc[row,1:]))
        for count,row in enumerate(picked_frames):
            picked_frames[count] = [x for x in row if str(x) != 'nan'] # remove nans
        for count,row in enumerate(picked_frames):
            picked_frames[count] = [int(x) for x in row] # convert to int
    else:
        raise ValueError('frames must be a .csv file with trialnames and frame numbers')

    for trial in trialnames:
        # Read 2D points file
        contents = os.listdir(corrected_path)
        filename = [x for x in contents if ".csv" in x and trial in x]  # csv filename
        df1 = pd.read_csv(corrected_path + "/" + filename[0], sep=',', header=None)

        # read pointnames from header row
        pointnames = df1.loc[0, ::4].astype(str).str[:-7].tolist()
        pnames.append(pointnames)

        df1 = df1.loc[1:, ].reset_index(drop=True)  # remove header row

        ncol = df1.shape[1]

        dfs.append(df1)

    # a couple errors
    # if pointnames aren't the same across trials
    if any(pnames[0] != x for x in pnames):
        raise ValueError('Make sure body part names are consistent across trials')


    ### Part 2: Extract images and 2D point data

    for trialnum, trial in enumerate(trialnames):
        relnames = []
        data = pd.DataFrame()

        # new training dataset folder
        newpath = config + "/labeled-data/" + trial
        h5_save_path = newpath + "/CollectedData_" + scorer + ".h5"
        csv_save_path = newpath + "/CollectedData_" + scorer + ".csv"

        if not os.path.exists(newpath):
            os.makedirs(newpath)  # make new folder

        for camera in cameras:

            # get video file
            file = []
            for filename in os.listdir(trials_path + "/" + trial):
                if '.DS_Store' in filename:
                    os.remove(trials_path + "/" + trial +"/"+filename)
            contents = os.listdir(trials_path + "/" + trial)
            for name in contents:
                if any(x in name for x in subs[camera - 1]):
                    file = name
                    trialcam = os.path.splitext(file)[0]
            if not file:
                raise ValueError('Cannot locate %s video file or image folder' % trialcam)

            # add video the video set
            # if trialcam not in trialnames:
                # add_new_videos(config_path, video, copy_videos = False)

            # file is actually a file
            # extract frames from video and convert to png
            print("Extracting images and 2D points from %s..." % (trialcam))
            video = trials_path + "/" + trial + "/" + file
            relpath = "labeled-data/" + trial + "/"
            frames = picked_frames[trialnum]
            frames.sort()
            cap = cv2.VideoCapture(video)
            success, image = cap.read()
            count = 0
            while success:
                if count + 1 in frames:
                    imgname = trialcam + "_%s.png" % str(count + 1).zfill(4)
                    relname = relpath + imgname
                    relnames = relnames + [relname]
                    contents = os.listdir(newpath)
                    if imgname in contents:
                        print('Point data for %s already extracted.' % imgname)
                    else:
                        cv2.imwrite(newpath + "/"  + imgname,
                                image)  # save frame
                success, image = cap.read()
                count += 1
            cap.release()

            # extract 2D points data
            contents = os.listdir(corrected_path)
            pointsfile = [x for x in contents if '.csv' in x and trial in x]

            if not pointsfile:
                raise ValueError('Cannot locate %s 2D points file' % trial)

            # if multiple csv files, look for "2Dpoints" in the name
            if len(pointsfile) > 1:
                t = []
                for q in pointsfile:
                    if any(x in q for x in pts):
                        t = t + [q]
                # if there are multiple 2D points files, look for "train" in the name
                if len(t) > 1:
                    for r in pointsfile:
                        if any(x in r for x in corr):
                            pointsfile = r
            else:
                pointsfile = pointsfile[0]
            if isinstance(pointsfile, str) != True:
                raise ValueError('Please check the points files in training points folder')

            df = pd.read_csv(corrected_path + '/' + pointsfile, sep=',', header=None)
            df = df.loc[1:, ].reset_index(drop=True)
            frames = [x - 1 for x in frames]  # account for zero index in python
            xpos = df.iloc[frames, 0 + (camera - 1) * 2::4]
            ypos = df.iloc[frames, 1 + (camera - 1) * 2::4]
            temp_data = pd.concat([xpos, ypos], axis=1).sort_index(axis=1)
            temp_data.columns = range(temp_data.shape[1])
            data = pd.concat([data, temp_data])


        ### Part 3: Complete final structure of datafiles
        dataFrame = pd.DataFrame()
        temp = np.empty((data.shape[0], 2,))
        temp[:] = np.nan
        for i, bodypart in enumerate(pointnames):
            index = pd.MultiIndex.from_product([[scorer], [bodypart], ['x', 'y']],
                                                names=['scorer', 'bodyparts', 'coords'])
            frame = pd.DataFrame(temp, columns=index, index=relnames)
            frame.iloc[:, 0:2] = data.iloc[:, 2 * i:2 * i + 2].values.astype(float)
            dataFrame = pd.concat([dataFrame, frame], axis=1)
        dataFrame.replace('', np.nan, inplace=True)
        dataFrame.replace(' NaN', np.nan, inplace=True)
        dataFrame.replace(' NaN ', np.nan, inplace=True)
        dataFrame.replace('NaN ', np.nan, inplace=True)
        dataFrame.apply(pd.to_numeric)

        # add to existing data sheet if exists
        contents = os.listdir(newpath)
        h5file = [x for x in contents if '.h5' in x]
        if h5file:
            ogdata = pd.read_hdf(newpath+'/'+h5file[0]) # read old point labels
            dataFrame = pd.concat([ogdata, dataFrame])
            dataFrame = dataFrame[~dataFrame.index.duplicated(keep='last')] # remove duplicates. keep last instance

        dataFrame.to_hdf(h5_save_path, key="df_with_missing", mode="w")
        dataFrame.to_csv(csv_save_path, na_rep='NaN')

    print('Frames from %d trials successfully added to training dataset'%len(trialnames))
    
def extract_mayacam(calib_path, imagesize = [1920,1080]):
    maya_path = os.path.dirname(calib_path) + "/MayaCams"
    if not os.path.exists(maya_path): 
       os.makedirs(maya_path)
    for filename in os.listdir(calib_path):
      if '.DS_Store' in filename:
         os.remove(calib_path+"/"+filename)
    calibs = os.listdir(calib_path)
    for calib in calibs:
       if not any(x.startswith(calib) for x in os.listdir(maya_path)):
          print("Exporting MayaCams for " + str(calib)) 
          calib_folder = calib_path +"/"+ calib
          xma_path = calib_folder +"/"+ calib +".xma"
          tmp_path = calib_folder  +"/tmp"
          if not os.path.exists(tmp_path ): 
             os.makedirs(tmp_path )
          xma_zip_path = tmp_path +"/"+ calib +".zip"
          shutil.copy(xma_path, xma_zip_path)
          z = zipfile.ZipFile(xma_zip_path)
          z.extractall(path = tmp_path)
          z.close()
          cameras = [1,2]
          it = 0
          image_size = imagesize # dependent on resolution of the videos
          for camera in cameras:
             cam_path = tmp_path +"/Camera "+ str(camera)
             cam_mat_path = cam_path +"/data/" + "Camera " + str(camera) + "_CameraMatrix.csv"
             cam_rot_path = glob.glob(cam_path +"/data/*_RotationMatrix.csv")[it]
             cam_tran_path = glob.glob(cam_path +"/data/*_TranslationVector.csv")[it]
             cam_mat = pd.read_csv(cam_mat_path,header=None)
             cam_mat_txt = 'camera matrix\n'+str(cam_mat.iloc[0,0])+',0,'+str(cam_mat.iloc[0,2])+'\n0,'+str(cam_mat.iloc[1,1])+','+str(cam_mat.iloc[1,2])+'\n0,0,1\n'
             cam_rot = pd.read_csv(cam_rot_path,header=None)
             cam_rot_txt  = 'rotation\n'+cam_rot.to_csv(header= False, index = False).replace('\r','')
             cam_tran = pd.read_csv(cam_tran_path,header=None)
             cam_tran_txt = 'translation\n'+cam_tran.to_csv(header= False, index = False).replace('\r','')
             image_size_txt = 'image size\n'+pd.DataFrame(image_size).transpose().to_csv(header = False,index = False).replace('\r','')
             frame = os.path.basename(cam_rot_path).split('_')[0]
             textfile = maya_path +"/"+ calib + "_" + frame +"_MayaCam"+ str(camera)+".txt"
             if not os.path.exists(textfile):
                with open(textfile, 'w') as file:
                   file.write(image_size_txt + '\n'+ cam_mat_txt + '\n'+ cam_rot_txt + '\n'+ cam_tran_txt) 
          shutil.rmtree(tmp_path)
       else:
          print("MayaCams already exported for " + str(calib))
          
def IterativeLS_triangulate(P1, P2, pts1, pts2):
    # Iteratively triangulate 3D points from 2D correspondences using SVD.
	# Parameters:
    # - P1, P2: 3x4 camera projection matrices
    # - pts1, pts2: Nx2 arrays of corresponding 2D points in each camera

    # Returns:
    # - Nx3 array of triangulated 3D points
    
    EPS = 1e-7
    pts1 = np.asarray(pts1)
    pts2 = np.asarray(pts2)
    n_points = pts1.shape[0]
    pts3D = np.zeros((n_points, 3))

    for i in range(n_points):
        w1, w2 = 1.0, 1.0
        x = np.zeros(4)  # initialize

        for _ in range(10):
            A = np.zeros((4, 4))
            A[0] = (pts1[i, 0] * P1[2] - P1[0]) / w1
            A[1] = (pts1[i, 1] * P1[2] - P1[1]) / w1
            A[2] = (pts2[i, 0] * P2[2] - P2[0]) / w2
            A[3] = (pts2[i, 1] * P2[2] - P2[1]) / w2

            _, _, Vt = np.linalg.svd(A)
            x = Vt[-1]

            new_w1 = P1[2] @ x
            new_w2 = P2[2] @ x

            if abs(w1 - new_w1) <= EPS and abs(w2 - new_w2) <= EPS:
                break

            w1 = new_w1
            w2 = new_w2

        pts3D[i] = x[:3] / x[3]

    return pts3D
    
def read_maya_cam(filepath):
    if filepath is None:
        raise ValueError("Provide MayaCam file path")

    # Read the file line by line
    with open(filepath, 'r') as f:
        lines = f.readlines()

    # Split sections by blank lines
    groups = []
    current = []
    for line in lines:
        if line.strip() == "":
            if current:
                groups.append(current)
                current = []
        else:
            current.append(line)
    if current:
        groups.append(current)

    if len(groups) < 4:
        raise ValueError("File must contain at least 4 non-empty sections")

    def parse_csv_section(section):
        return np.loadtxt(section[1:], delimiter=",")

    image = parse_csv_section(groups[0])
    camera = parse_csv_section(groups[1])
    rotation = parse_csv_section(groups[2])
    transform = parse_csv_section(groups[3])

    # Concatenate rotation (3x3) and transform (3x1) to make a 3x4 matrix
    if transform.ndim == 1:
        transform = transform.reshape(-1, 1)
    P = camera @ np.hstack((rotation, transform))
    
    return P
    
def convert_2d_to_3d(points2D_path, calibmeta_path, maya_path):
    # Load calibration metadata
    calib_meta = pd.read_csv(calibmeta_path)

    # Set 3D output folder
    points3D_path = points2D_path.replace("2Dpoints_it", "3Dpoints_it")
    os.makedirs(points3D_path, exist_ok=True)

    for trial_file in os.listdir(points2D_path):
        if not trial_file.endswith("_2Dpts_predicted.csv"):
            continue

        trialname = trial_file.replace("_2Dpts_predicted.csv", "")
        points2D = pd.read_csv(os.path.join(points2D_path, trial_file), na_values="NaN")
        
        contents = os.listdir(points3D_path)
        if trialname in contents:
        	continue

        if points2D.columns[0] == "Frame":
            points2D = points2D.iloc[:, 1:]

        markers = [col.replace("_cam1_X", "") for col in points2D.columns if "cam1_X" in col]

        colnames_3D = [f"{m}_{axis}" for m in markers for axis in ["X", "Y", "Z"]]
        points3D = pd.DataFrame(np.nan, index=points2D.index, columns=colnames_3D)

        # Match calibration files
        trial_calib_row = calib_meta[calib_meta["Trial"].str.contains(trialname, na=False)]
        if trial_calib_row.empty:
            print(f"Warning: No calibration entry found for {trialname}")
            continue

        trial_calib_name = trial_calib_row["Calibration"].values[0]

        calib_files = os.listdir(maya_path)
        matched_files = [f for f in calib_files if trial_calib_name in f]
        cam1_file = next((f for f in matched_files if "Cam1" in f), None)
        cam2_file = next((f for f in matched_files if "Cam2" in f), None)

        if not cam1_file or not cam2_file:
            print(f"Warning: Cam1 or Cam2 calibration files not found for {trialname}")
            continue

        P1 = read_maya_cam(os.path.join(maya_path, cam1_file))
        P2 = read_maya_cam(os.path.join(maya_path, cam2_file))

        # Triangulate each marker
        print(f"Converting 2D points for {trialname} to 3D")
        for marker in markers:
            marker_cols = [col for col in points2D.columns if marker in col]
            points2D_temp = points2D[marker_cols].dropna()

            if points2D_temp.empty:
                continue

            frame_indices = points2D_temp.index
            pts1 = points2D_temp.iloc[:, :2].values
            pts2 = points2D_temp.iloc[:, 2:4].values

            pts3D = IterativeLS_triangulate(P1, P2, pts1, pts2)

            points3D.loc[frame_indices, [f"{marker}_X", f"{marker}_Y", f"{marker}_Z"]] = pts3D

        # Save result
        output_file = os.path.join(points3D_path, f"{trialname}.csv")
        points3D.to_csv(output_file, index=False)
    
    

