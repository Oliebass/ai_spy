#################################################
## AI Spy Human-In-The-Loop workflow for humans##
#################################################
source('/Users/oliverhartley/Desktop/RScripts/ai_workflow_functions.R')
start()

# demo renaming
dta_files <- read.csv("Incoming Images/results_files.csv", header = TRUE)
dta_files$absolute_path <- paste0(getwd(),'/', 'Incoming Images')
write.csv(dta_files, "Incoming Images/results_files.csv", row.names = FALSE)

dta_det <- read.csv("Incoming Images/results_detections.csv", header = TRUE)
dta_det$absolute_path <- paste0(getwd(),'/', 'Incoming Images')
write.csv(dta_det, "Incoming Images/results_detections.csv", row.names = FALSE)



# Step 1: ---------------------------------------------------------------------
#'  - Sets up project folder to follow layout used by full system
#'  - Ensures iterative processing possible
#'    -> Designated Incoming Images Folder for iterative AI classification
source("N:/Conservation/R Scripts/ai_workflow_functions.R")
setwd("N:/Conservation/AI_PipeLine_Tester_Folder/Demo/") # Set root directory where your projects are to be kept
setup("Lou_tester") # enter name for project

# Step 2: ----------------------------------------------------------------------
#'  - Load images into Incoming Images Folder
#'    -> If survey design provided, can load into site file
#'  - Select 'Incoming Images' folder for AI processing in AddaxAI
#'  - Save results from AddaxAI in 'Incoming Images'
 
# Step 3: ----------------------------------------------------------------------
#'  - Set Working Directory to project folder
#'  - Run below function:
#'    -> Moves AI output files to designated folder
#'    -> Loads AI output for pipeline
#'    -> Copies images used for validation to separate folder
#'    -> Ensures validation folders equipped with standardised validation sheets
#'    -> Moves images to central archive
#'    -> Logs all image movement and AI-processing in dedicated log sheets
source("N:/Conservation/R Scripts/ai_workflow_functions.R")
setwd("N:/Conservation/Winter Camera Trap Survey 2025-26/") # Set to project directory

### TUT FIX:
#addax_all <- read.csv('Incoming Images/results_files.csv', header = TRUE)
#addax_all$absolute_path <- sub('demo_project', 'Lou_tester', addax_all$absolute_path)
#write.csv(addax_all, 'Incoming Images/results_files.csv', row.names = FALSE)
#
#addax_detections <- read.csv('Incoming Images/results_detections.csv', header = TRUE)
#addax_detections$absolute_path <- sub('demo_project', 'Lou_tester', addax_detections$absolute_path)
#write.csv(addax_detections, 'Incoming Images/results_detections.csv', row.names = FALSE)
#
detections_to_validate(ai_to_data()) # run single command

# Step 4: ----------------------------------------------------------------------
#' - Validate the images manually in the "To Validate" folder

# Step 5: ----------------------------------------------------------------------
#'  - Set Working Directory to project folder
#'  - Run below function: 
#'    -> Finalises a validation cycle
#'    -> Collates all validations
#'    -> Ensures unvalidated images are documented correctly
#'    -> Compiles Master File for the project
#'      -> If exists, updates with new validations
#'    -> Only makes updates permanent after all processes complete
source("N:/Conservation/R Scripts/ai_workflow_functions.R")
setwd("N:/Conservation/Winter Camera Trap Survey 2025-26/") # Set to project directory
validation_wrapup()

#-------
## Step 2a: Set up custom AI-archiving and processing (can be run directly inside detections_to_validation(): 
## Step 2b: Archive images into image repository, set up validation protocol and copy image for validation process
##          based off cleaned and processed AI data outputs from ai_to_data()
## archive and process image data already processed by an AI
## ensure in the correct project folder for system to run
#setwd('N:/Conservation/AI_PipeLine_Tester_Folder/Addax_Trial/')
  ## prefix: the prefix to be used for your ai_results archive once ai processing completed.
    ## Default: 'processed_' + process date
  ## event_level: how you would like to process the images for validation (image or sequence);
    ## both are calculated, but the specified level will be what what will be recorded for validation
    ## Default: 'image'
  ## event_independence: the duration of time to pass to assume independent events for your project
    ## Default: 1 min
#detections_for_validation <- ai_to_data()




