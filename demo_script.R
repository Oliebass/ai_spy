#################
## AI-Spy Demo ##
#################

#' This demo is setup to showcase the flow of the AI-Spy protocol. To do this,
#' please run the below code to showcase the steps. Note, in practice all of 
#' the steps are run using the "start()" function, and using the workflow, will
#' not need renaming of the central file system. This is merely needed to allow
#' for the 

# Step 1 - Setup the project
## 1. Select 1
## 2. Set project name to "tester" for purposes of demonstration
## 3. Survey design: test_data/survey_design.csv
source("ai_workflow_functions.R")
start()

# Step 2 - Simulate AI-Processing and output saving
## 1. Copy images to "Incoming Images" folder (code below)
## 2. Copy classifier outputs to "Incoming Images" folder
## 3. Shift absolute path name to the path linked to "Incoming Images" structure
ai_output_files <- dir_ls("test_data", type = "file")[1:4]
image_files <- dir_ls("test_data/NH7801/", type = "file")

file_copy(ai_output_files, sub("test_data", "tester/Incoming Images", ai_output_files))
file_copy(image_files, sub("test_data", "tester/Incoming Images", image_files))

detections <- read.csv("tester/Incoming Images/results_files.csv", header = TRUE)
detections$absolute_path <- paste0(getwd(), '/tester/Incoming Images')
write.csv(detections, "tester/Incoming Images/results_files.csv", row.names = FALSE)
classifications <- read.csv("tester/Incoming Images/results_detections.csv", header = TRUE)
classifications$absolute_path <- paste0(getwd(), '/tester/Incoming Images')
write.csv(classifications, "tester/Incoming Images/results_detections.csv", row.names = FALSE)

