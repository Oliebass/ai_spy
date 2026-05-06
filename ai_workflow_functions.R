if(!'pacman' %in% rownames(installed.packages())){
  install.packages('pacman')
}

pacman::p_load(dplyr, fs, lubridate, tidyr) # using pacman to install and load packages (so good)

## Gateway function -----------------------------------------------------------
## Allow user to enter functions iteratively if wanted
start <- function(){
  message('Welcome to the AI-Spy protocol. Please select what you would like to do:')
  options <- c('1' = 'Set up a new project\n',
               '2' = 'Log new AI-processed images into your project\n',
               '3' = 'Wrap up a round of image validations\n',
               '4' = 'Cancel\n')
  input <- NA 
  counter <- 0
  
  while(!input %in% names(options) & counter < 3){
    if(counter > 1){
      message('Choice not recognised.\nPlease try again')
    }
    
    for(option in options){
      message(names(options)[options == option], '.\t', option)
    }
    
    input <- readline()   
    counter <- counter + 1
  }
  
  if(!input %in% names(options) & counter == 3){ 
    message('Input not recognised 3 times.')
    input <- '4'
  }
  
  if(input == '1'){
    setup()
  }
  
  if(input == '2'){
    outcome <- dir_setup()
    if(!is.na(outcome)){
      detections_to_validate(ai_to_data())
    } else {
      input <- '4'
    }
  }
  
  if(input == '3'){
    outcome <- dir_setup()
    if(!is.na(outcome)){
      validation_wrapup()
    } else {
      input <- '4'
    }
  }
  
  if(input == '4'){
    message('AI-Spy protocol cancelled.\n\nProgram closing.')
  }
}

## checks and controls working directory needed for functions
dir_setup <- function(){
  message('The current project directory is:\n\n', getwd(), '\n\nIs this the project directory you wish to work in?\ny/n')
  answer <- tolower(readline())
  
  if(answer == 'no' | answer == 'n'){
    file_path_switch <- FALSE
    counter <- 0
  
    while(file_path_switch != TRUE & counter < 3){
      counter <- counter + 1
      if(counter>1){
        message('"', file_path, '" does not exist. Please try again.')
      }
      
      message('Please enter the full file path to the project directory that you wish to work in:\n')
      file_path <- gsub('(\")', '', readline())
      if(dir.exists(file_path)){
        message(file_path, ' detected.\n Setting to project directory.\n\n')
        setwd(file_path)
        return(TRUE)
        break
      }
    }
    message('Unable to detect and set project directory.')
    return(NA)
  } else {
    return(TRUE)
  }
}

## Phase 0: Set up File System for year -----------------------------------------
setup <- function(folder_name = NA){
  ## Function Confirmation
  message('Are you sure you want to create a new project?\ny/n')
  safety_checker <- tolower(readline())
  
  if(safety_checker == 'y' || safety_checker == 'yes'){
    dir_setup()
    ## read.csv of sites
    if(is.na(folder_name)){
      message('Please enter the name for the new project:')
      folder_name <- readline()
    }
    message('Enter filepath for survey design.\ne.g. "C:/Users/user/Documents/Wildcat AI Pipeline/survey.csv"\n\n If none at this time, hit ENTER.\n')
    file_path <- gsub('(\")', '', readline())
    
    if(file_path != ''){
      ## code for future dynamic col selection to save into config.txt
      #survey_file <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE)
      #message('Select column containing your trapping sites by entering the corresponding number:\n')
      #for(i in seq_along(colnames(survey_file))){
      #  message(i,'.\t', colnames(survey_file)[i])
      #}
      #site_column <- as.numeric(readline())
      #
      #message('Using ', colnames(survey_file)[site_column],' for trapping sites.')
      #sites <- unique(survey_file[,site_column])
      
      sites <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE)$Camera_grid_number
      for(site in sites){
        dir_create(paste0(getwd(), '/', folder_name,'/data/images/', site))
        dir_create(paste0(getwd(), '/', folder_name,'/Incoming Images/', site))
      }
      
      file_copy(file_path, paste(getwd(), folder_name, 'data', 'survey_design.csv', sep = '/'))
    } else {
      message('No survey design file detected. Directories will be created without including
              site folders\n')
      dir_create(paste0(getwd(), '/', folder_name,'/data/images/'))
      dir_create(paste0(getwd(), '/', folder_name,'/Incoming Images/'))
    }
    
    dir_create(paste0(getwd(), '/', folder_name,'/data/logs/ai_files/'))
    dir_create(paste0(getwd(), '/', folder_name,'/data/logs/'))
    dir_create(paste0(getwd(), '/', folder_name,'/misc/'))
    
    message('Setup complete for project:\n', paste0(getwd(), '/', folder_name))
  } else {
    message('Project setup cancelled.\n')
  }
}

## safety measures:
## lockfile to ensure 
#create_lock <- 

## Phase 1 - AI to Data Pipe ---------------------------------------------------
ai_to_data <- function(prefix = 'processed_', event_level = 'image', event_independence = 1){
  ## Step 1: Archive AI Detection Data
  ## move and create AI output repository
  message('Moving AI classification files to data/logs/ai_files directory\n')
  folder_name <- ai_outputs_to_file(prefix) 
  
  if(folder_name != 'None'){
    ## Step 2: Data loading
    ## loading ai_detections
    message('Loading AI classification data\n')
    ai_classifications <- ai_detections(folder_name) 
  
    ## Step 3: Data prep
    ## Preparing and processing AI classification data for validation
    message('Preparing AI classification data for processing\n')
    ai_cleaned <- ai_data_prep(ai_classifications, event_independence) 
    
    message('Processing AI classification data\n')
    dta_processed <- top_guess(ai_cleaned, event_level) # works as intended
  
    ## Image Logger update
    ai_logger(dta_processed, 'entry') # works as intended
    
    image_logger('initial log', dta_processed)
    
    return(dta_processed)
  
  } else {
    message('Process terminated.\n')
    return(NULL)
  } # works as intended
} # works as intended

## Step 1: Archive AI Detection Data
ai_outputs_to_file <- function(prefix){
  ## debugging code
    # prefix <- paste0('tester_')
  ## incase using without structure in place
  if(!file_exists('data/logs/ai_files/')){
    message('data/logs/ai_files/ not detected. Creating ai_files')
    dir_create('data/logs/ai_files/')
  }
  
  ## set folder destination to move ai_classified files to ai_files archive
  prefix <- paste0('data/logs/ai_files/', prefix)
  ai_classification_files <- dir_ls('Incoming Images/', recurse = TRUE, regexp = '\\.(csv|json)$', type = 'file')
  
  ## checking to make sure files there to move to unique folder, and can safely break function
  if(length(ai_classification_files) > 0){
    
    ## folder name creation
    folder_name <- paste0(prefix,ymd(today()))
    similar_folder_names <- sum(grepl(folder_name, dir_ls("data/logs/ai_files/")))
    
    ## existing folder checking to ensure safe file moving
    ## checking folder name for processing
    if(similar_folder_names > 0){
      ## adds extra files on if multiple already exist
      if(similar_folder_names != 1){
        folder_name <- paste(folder_name, letters[similar_folder_names+1], sep = '_')
      } else {
        ## renames first occurrence of file and sets new folder name to be next cronologically
          ## will not affect down stream processes as ai_files isolated from further processes
        success <- file.rename(folder_name, paste(folder_name, letters[similar_folder_names], sep = '_'))
        if(!all(success)) stop('Multiple files of same name exist, but renaming protocol failed. Copy AI output files into suitably named folder in data/logs/ai_files and rerun process.')
        folder_name <- paste(folder_name, letters[similar_folder_names+1], sep = '_')
      }
    }
    
    ## creates directory to move ai_files into
    dir_create(folder_name)
    
    ## moves AI classification data to new file
    success <- file.rename(ai_classification_files, sub('Incoming Images/', paste0(folder_name, '/'), ai_classification_files))
    if(!all(success)) stop('Some of the ai classification files could not be moved. Please move them to:\n', folder_name)
    
  } else {
    message('No new AI classification files to be moved. Which classifications would you like to load?')
    folder_list <- dir_ls('data/logs/ai_files/', type = 'directory')
    
    for(i in seq_along(folder_list)){
      message(i,'.\t', folder_list[i])
    }
    message(length(folder_list)+1, '.\t', 'None\n')
    
    input <- as.numeric(readline())
    error_counter <- 1
    
    while((is.na(input) || !(input %in% seq_len(length(folder_list) + 1))) && error_counter < 3){
      error_counter <- error_counter + 1
      
      message('Input not recognised. Please try again\n')
      for(i in seq_along(folder_list)){
        message(i,'.\t', folder_list[i])
      }
      message(length(folder_list)+1, '.\t', 'None')
      input <- as.numeric(readline())
    }
    
    if(error_counter == 3) {
      message('Selection not recognised. Please check "/data/logs/ai_files" is available in project folder\n')
      folder_name <- 'None'
    }
    if(input == length(folder_list)+1){
      folder_name <- 'None'
    }
    if(input %in% seq_along(folder_list)){
      folder_name <- folder_list[input]
    }
    
  }
  return(folder_name) 
  
} ## Works as intended

## Step 2: Data loading
#! need to make dynamic to suit different ai_systems. 
ai_detections <- function(folder_name){
  ai_all <- select(read.csv(paste0(folder_name,'/results_files.csv'), header = TRUE, stringsAsFactors = FALSE), 
                   absolute_path, relative_path, n_detections, DateTime)
  ai_tags <- select(read.csv(paste0(folder_name,'/results_detections.csv'), header = TRUE, stringsAsFactors = FALSE),
                    relative_path, label, confidence)
  
  ai_comb <- left_join(ai_all, ai_tags, 'relative_path')
  
  return(ai_comb)
} #works as intended; can make more robust to 1. differing file names, 2. differing number of files, 3. different colnames

## Step 3: Data prep
ai_data_prep <- function(dta, event_independence){
  ## debuggin code
    # dta <- ai_classifications
    # event_independence <- 1
  
  ## handle blank detections
  dta$label[is.na(dta$label)] <- 'blank'
  dta$confidence[dta$label == 'blank'] <- 0
  
  ## generate universal joinkey
  dta$joinkey <- paste(dta$absolute_path, dta$relative_path, sep = '/')
  dta$datetime <- dmy_hms(dta$DateTime)
  
  ## remove duplicate entries
  dta <- dta[!duplicated(dta),]
  
  ## determine events for isolation
  dta <- dta %>% 
    arrange(datetime, joinkey) %>% 
    mutate(joinkey = paste(absolute_path, relative_path, sep = '/'),
           datetime = dmy_hms(DateTime),
           time_diff = as.numeric(datetime-lag(datetime), units = 'mins'),
           detection_id = cumsum(ifelse(is.na(time_diff), 1, time_diff >= 0)), # each detection assigned unique ID
           sequence_id = cumsum(ifelse(is.na(time_diff), 1, time_diff > event_independence)), # each sequence above the independence level treated as an event
           independent_sequence_duration = event_independence # in case multiple event time-scales used
    )
  
  ## find last event for each event level to use later_ensures unique ID mapping
  max_previous_events <- ai_logger(dta, 'event_checker')
    
  dta$detection_id <- dta$detection_id + max_previous_events[1]
  dta$sequence_id <- dta$sequence_id + max_previous_events[2]

  return(dta)
} # works as intended

ai_logger <- function(dta, process){
  ## debugging code
    # dta <-  
    # process <- 
  
  ## Adding entries to the AI database
  if(process == 'entry'){
    ## saves all image detections into central space for later joining
    if(file_exists('data/logs/ai_database.csv')){
      old_image_master <- read.csv('data/logs/ai_database.csv', header = TRUE, stringsAsFactors = FALSE)
      new_images <- dta[!dta$joinkey %in% old_image_master$joinkey, ]
      if(nrow(new_images)>0){
        message('Updating data/logs/ai_database.csv\n')
        new_image_master <- rbind(old_image_master[!old_image_master$joinkey %in% dta$joinkey,], dta)
        write.csv(new_image_master, 'data/logs/ai_database.csv', row.names = FALSE)
        message(nrow(new_images),' new detections added to data/logs/ai_database.csv\n')
      } else{
        message('No new detections to add to data/logs/ai_database.csv\n')
      }
    } else {
      message('Creating data/logs/ai_database.csv\n')
      write.csv(dta, 'data/logs/ai_database.csv', row.names = FALSE)
    }
  } # works as intended
  
  ## Checking to see what last ID's were for event and detection to create unique mapping
  if(process == 'event_checker'){
      ## find last event for each event level to use later_ensures unique ID mapping
    if(file_exists('data/logs/ai_database.csv')){
      old_image_master <- read.csv('data/logs/ai_database.csv', header = TRUE, stringsAsFactors = FALSE)
      # ensure mapping to unique images in case validation not completed before next upload and processing
      max_image <- max(old_image_master$detection_id, na.rm = TRUE)
      max_event <- max(old_image_master$sequence_id, na.rm = TRUE)
      
      return(c(max_image, max_event))
    } else {
      return(c(0, 0))
    }
  } # works as intended
} # works as intended

image_logger <- function(logging_event, images_to_log = NA){
  ## image log to archive image movements within the pipeline
  
  
  if(logging_event == 'initial log'){
    image_log <- data.frame(
      initial_location = images_to_log$joinkey[!duplicated(images_to_log$joinkey)],
      archive_location = NA,
      date_created = format(images_to_log$datetime[!duplicated(images_to_log$joinkey)]),
      date_archived = NA,
      date_validated = NA,
      stringsAsFactors = FALSE
    )
    
    if(file_exists('data/logs/image_log.csv')){
      message('Updating data/logs/image_log.csv\n')
      image_log_old <- read.csv('data/logs/image_log.csv', header = TRUE, stringsAsFactors = FALSE)
      new_images <- image_log[!image_log$initial_location %in% image_log_old$initial_location, ]
      
      updated_image_log <- rbind(image_log_old, new_images)
      
      message(nrow(new_images), ' entries added to data/logs/image_log.csv\n')
      write.csv(updated_image_log, 'data/logs/image_log.csv', row.names = FALSE)
      
    } else {
      message('Creating data/logs/image_log.csv\n')
      write.csv(image_log, 'data/logs/image_log.csv', row.names = FALSE)
      message(nrow(image_log), ' records added to data/logs/image_log.csv\n')
    }
    
  }
  if(logging_event == 'archive'){
    image_log <- read.csv('data/logs/image_log.csv', header = TRUE, stringsAsFactors = FALSE)
    image_log_to_update <- image_log$initial_location %in% images_to_log & is.na(image_log$date_archived)
    image_log$archive_location[image_log_to_update] <- sub('Incoming Images', 'data/images', image_log$initial_location[image_log_to_update])
    image_log$date_archived[image_log_to_update] <- format(as_datetime(now()))
    write.csv(image_log, 'data/logs/image_log.csv', row.names = FALSE)
    message(sum(image_log_to_update), ' records updated in data/logs/image_log.csv\n')
  }
  if(logging_event == 'validate'){
    image_log <- read.csv('data/logs/image_log.csv', header = TRUE, stringsAsFactors = FALSE)
    image_log_to_update <- is.na(image_log$date_validated)
    image_log$date_validated[image_log_to_update] <- format(as_datetime(now()))
    write.csv(image_log, 'data/temp/image_log.csv', row.names = FALSE)
    message(nrow(image_log), ' records updated in data/logs/image_log.csv\n')
  }
  
}

## Step 4: classification detection
top_guess <- function(dta, event_level){
  ## debugging code
    # dta <- ai_cleaned
    # event_level = 'sequence'
  
  ## compress to single detection per species per image based off the ai dta
  dta <- dta %>% 
    arrange(joinkey) %>% 
    group_by(joinkey, label) %>% 
    summarise(n_obs_ai = n(),
              confidence = max(confidence),
              detection_id = detection_id[which.max(confidence)],
              sequence_id = sequence_id[which.max(confidence)],
              datetime = datetime[which.max(confidence)],
              .groups = "drop") %>% 
    select(joinkey, label, confidence, n_obs_ai, detection_id, sequence_id, datetime)
  
  ## find top detection per event 
  ## selects top guess per event based off descending scores
  dta_top_image <- dta %>% 
    group_by(joinkey) %>% 
    filter(confidence == max(confidence)) %>% 
    slice(1) %>% 
    ungroup()
  
  dta_top_sequence <- dta %>% 
    group_by(sequence_id) %>% 
    filter(confidence == max(confidence)) %>% 
    slice(1) %>% 
    ungroup()
  
  ## store top detection per image into initial dataset
  dta$top_id_for_image <- ifelse(dta$detection_id %in% dta_top_image$detection_id, 1, 0)
  dta$top_image_for_sequence <- ifelse(dta$joinkey %in% dta_top_sequence$joinkey, 1, 0)
  dta$validation_level <- event_level
  
  return(dta)
} # works as intended

## Phase 2 - Data to Validation Pipe -------------------------------------------
detections_to_validate <- function(dta_processed){ 
  ## copy images into species specific validation folders, tracks image copying to
  ## original file path and writes species-specific validation file to be collated
  ## post validation
  ## dta_processed <- ai_to_data()
  # dta <- dta_processed
  # top guess images to folder for human validation 
  
  ## SAFETY: preventing duplicate images being admitted to validation if already in archive
  safety_out <- image_safety(dta_processed)
  if(safety_out$verdict == 'STOP'){
    return(message('Image to validation process terminated.'))
  }
  
  if(safety_out$verdict == 'SAFE'){
  image_rename_tracker <- images_to_validation(safety_out$dta)
  
  # set and find absolute path
  folder_checker('validation')

  # joining existing image copying map with new results IF it exists (i.e. validation not yet complete on previous data)
  if(is.null(image_rename_tracker)){
    message('No updates for validation batch logger')
  } else {
    if(file_exists('data/logs/current_validation_batch_tracker.csv')){
      message('Updating current validation batch logger\n')
      validation_image_tracker <- read.csv('data/logs/current_validation_batch_tracker.csv', header = TRUE, stringsAsFactors = FALSE)
      image_rename_tracker <- arrange(rbind(validation_image_tracker, image_rename_tracker), joinkey)
      write.csv(image_rename_tracker, 'data/logs/current_validation_batch_tracker.csv', row.names = FALSE)
    } else {
      message('Creating batch logger for current validation cycle\n')
      write.csv(image_rename_tracker, 'data/logs/current_validation_batch_tracker.csv', row.names = FALSE)
    }
  }
  # moving all images to image database
  message('Moving images to archive')
  images_to_archive()
  }
  
}

## Step 1: Copy images for human validation
images_to_validation <- function(processed_dta){
  # track where images are copied to
  
  #processed_dta <- dta_processed
  #detection <- unique(processed_dta$label)[5]
  
  ## Checking incase no images detected for validation
  if(nrow(processed_dta) == 0){
    message('No images detected for valdation.')
    return(NULL)
  }
  
  ## safety check: ensure only 1 level of processing is being used (should be fine already but as an incase)
  if(length(unique(processed_dta$validation_level)) > 1){
    stop('Multiple validation levels trying to be processed. Please ensure a single validation level is used for the committed dataframe.\n')
  } else {
    event_level <- unique(processed_dta$validation_level)[1]
  }

  ## validation filtering based on event_level
  image_rename_tracker <- NULL
  if(event_level == 'sequence'){
    processed_dta <- processed_dta[processed_dta$top_image_for_sequence == 1,] 
  }
  if(event_level == 'image'){
    processed_dta <- processed_dta[processed_dta$top_id_for_image == 1,]
  }
  
  images_for_validation <- processed_dta$joinkey
  images_in_incoming_directory <- dir_ls(paste0(getwd(), '/Incoming Images'), recurse = TRUE, type = 'file')
  
  ## safety check: make sure all images present before moving to make auditing easier
  if(!all(images_for_validation %in% images_in_incoming_directory)){
    
    images_missing <- paste0(' - ', images_for_validation[!images_for_validation %in% images_in_incoming_directory] , collapse = '\n')
    stop('Not all images to be used for validation are in the Incoming Images directory. Please ensure the following images are there before reattempting this process.',
         'Missing images: \n', images_missing)
  }
  
  ## safety check: filter out images already in validation folder
  if(file_exists('data/logs/current_validation_batch_tracker.csv')){
    existing_validations <- read.csv('data/logs/current_validation_batch_tracker.csv', header = TRUE, stringsAsFactors = FALSE)
    processed_dta <- processed_dta[!processed_dta$joinkey %in% existing_validations$joinkey,]
  }
  
  ## 
  if(nrow(processed_dta) == 0){
    message('No new images required to be copied to validation folder.\n')
    return(NULL)
  }
  
  ## track distribution of images for current
  for(detection in unique(na.omit(processed_dta$label))){
    # set relative path
    destination_folder <- folder_checker(detection)
    det_temp <- processed_dta[processed_dta$label == detection,]
    
    # makes sure images are not going to be overwritten
    last_image <- last_image_checker(destination_folder)
    
    # name creation for images to be copied
    det_temp$validation_file_path <- paste0(destination_folder, sprintf(fmt = '/IMG_%04d.JPG', 
                                                                 seq(from = last_image + 1,
                                                                     to = (nrow(det_temp)+last_image))))
    
    det_temp$validation_file_path <- ifelse(grepl('*MP4', det_temp$joinkey),
                                              sub('JPG', 'MP4', det_temp$validation_file_path), det_temp$validation_file_path)
    det_temp$validation_file_path <- ifelse(grepl('*MP4', det_temp$joinkey),
                                            sub('IMG', 'VID', det_temp$validation_file_path), det_temp$validation_file_path)
                                           
    det_temp$validation_move_date <- as.Date(now())
    
    message('Copying top detections for "', detection, '" to validation folder\n')
    # validation file set up
    validation_file_template <- det_temp %>% 
      mutate(image = basename(validation_file_path),
             sp_a = ifelse(label == 'mustelid', 'pine marten', label),
             n_obs_a = 1,
             sp_b = NA,
             n_obs_b = NA,
             sp_c = NA,
             n_obs_c = NA,
             phenotype = NA,
             unique_cat_id = NA,
             human_validated = FALSE,
             validated_by = NA) %>% 
      select(image, sp_a, n_obs_a, sp_b, n_obs_b, sp_c, n_obs_c, phenotype, unique_cat_id, human_validated, validated_by)
    
    # joining existing validation results with new results IF it exists
    if(file_exists(paste0(destination_folder, '/human_validation.csv'))){
      existing_validation_file <- read.csv(paste0(destination_folder, '/human_validation.csv'), header = TRUE, stringsAsFactors = FALSE)
      validation_file_template <- rbind(existing_validation_file, validation_file_template)
    }
    
    write.csv(validation_file_template, file = paste0(destination_folder, '/human_validation.csv'), row.names = FALSE)
    
    # image copies created off mapping between original and validation folder
    file_copy(det_temp$joinkey, det_temp$validation_file_path)
    
    # updating image tracking database
    image_rename_tracker <- rbind(image_rename_tracker, det_temp)
  }
  
  image_rename_tracker <- select(image_rename_tracker, 
                                 joinkey, 
                                 validation_file_path,
                                 detection_id,
                                 sequence_id,
                                 validation_level) %>% 
                          arrange(joinkey)
  
  return(image_rename_tracker)
} 

## Step 2: Move images to image archive
images_to_archive <- function(){

    ## find images to be relocated from image log
    unarchived_images <- read.csv('data/logs/image_log.csv', header = TRUE, stringsAsFactors = FALSE)
    unarchived_images <- unarchived_images[is.na(unarchived_images$date_archived),]
    
    ## differing directories
    new_dirs <- unique(dirname(unarchived_images$initial_location))
    existing_dirs <- dir_ls(paste0(getwd(), '/data/images/'), recurse = TRUE, type = 'directory')
    directory_diffs <- setdiff(new_dirs, sub('data/images', 'Incoming Images', existing_dirs))
    
    ## writes missing directories to be copied to
    dir_create(sub('Incoming Images', 'data/images', directory_diffs))
    
    ## moves only new images to archive
    incoming_images <- unique(unarchived_images$initial_location)
    existing_images <- dir_ls(paste0(getwd(), '/data/images/'), recurse = TRUE, type = 'file')
    images_to_move <- setdiff(incoming_images, sub('data/images', 'Incoming Images', existing_images))
    image_dir_list <- dir_ls(paste0(getwd(), '/Incoming Images/'), recurse = TRUE, type = 'file')
    
    if(length(images_to_move)>0){
      ## saftey check to make sure all images to be moved are present (prevents breaking loop)
      if(!all(images_to_move %in% image_dir_list)){
        images_missing <- paste0(' - ', images_to_move[!images_to_move %in% image_dir_list] , collapse = '\n')
        stop('Not all images to be moved are in the Incoming Images directory. Please ensure the following images are there before reattempting this process.',
             'Missing images: \n', images_missing)
      } else{
        ## moves images across,
        file_move(images_to_move, sub('Incoming Images', 'data/images', images_to_move))
        message('All images moved to data/images\n')
        
        image_logger('archive', images_to_move)
      }
    } else {
      message('No new classified images to move.\n')
    }
    
}

image_safety <- function(dta_processed){
  ## safety function to prevent duplicate data and accidental deletes from being revalidated and archived
  ## loading images in directory and finding images supposedly in archive
  if(!is.null(dta_processed)){
    
    ## datasets used for checks
    images_in_dir <- dir_ls(paste0(getwd(), '/Incoming Images/'), recurse = TRUE, type = 'file', glob = '*.JPG|*.MP4')
    images_in_archive <- dir_ls(paste0(getwd(), '/data/images/'), recurse = TRUE, type = 'file')
    images_as_archived <- read.csv('data/logs/image_log.csv', header = TRUE, stringsAsFactors = FALSE)
    images_as_archived <- images_as_archived[!is.na(images_as_archived$date_archived),]
    #images_in_validation <- read.csv('data/logs/current_validation_batch_tracker.csv', header = TRUE, stringsAsFactors = FALSE)
    
    ## finding images are both archived but being reentered to archive
    archived_and_incoming <- intersect(images_in_dir, sub('data/images', 'Incoming Images', images_as_archived$archive_location))
    
    ## finding images that are possible duplicates or missing
    duplicated_images <- intersect(sub('data/images', 'Incoming Images', images_in_archive), archived_and_incoming)
    missing_archived_images <- setdiff(sub('Incoming Images', 'data/images', archived_and_incoming), images_in_archive)
    
    
    ## safety check: are all the images to be copied in the archive
    ## finds images missing from 'incoming images
    message('Checking that all images for validation are in "Incoming Images"')
    missing_images <- setdiff(dta_processed$joinkey, images_in_dir)
    if(length(missing_images)>0){
      message(length(missing_images), ' missing from "Incoming Images":\n',
              paste('-', missing_images, collapse = '\n'))
      message('Please locate these before proceeding.')
      message('Process will now terminate.')
      return(list(verdict = 'STOP',
                  dta = NULL))
    } else {
      message('All images to be validated present.\n')
    }
    
    ## safety check: images already in archive AND in 'Incoming Images'
    ## stops if duplicated images present in both folders to ask what to do
    if(length(duplicated_images) > 0){
      message(length(duplicated_images), ' shared images between "Incoming Images" and "data/images/" detected. If not duplicates, this will overwrite preexisting images. Are you sure the following images are in the correct locations?')
      message(paste('-', duplicated_images, collapse = '\n'))
      message('\n\ny/n\n')
      message('Warning:\nIf "y", the duplicate images will be deleted from the "Incoming Images" \n')
      readline()
      
      message('Please enter once more to confirm choice\n\ny/n')
      safetyChecker <- tolower(readline())
      
      if(safetyChecker %in% c('y', 'yes')){
        message('Removing duplicate image submissions.')
        file_delete(duplicated_images)
        
        images_in_dir <- dir_ls(paste0(getwd(), '/Incoming Images/'), recurse = TRUE, type = 'file')
        images_in_archive <- dir_ls(paste0(getwd(), '/data/images/'), recurse = TRUE, type = 'file')
        images_as_archived <- read.csv('data/logs/image_log.csv', header = TRUE, stringsAsFactors = FALSE)
        images_as_archived <- images_as_archived[!is.na(images_as_archived$date_archived),]
        
        ## comparing to see which images are both archived but being reentered to archive
        archived_and_incoming <- intersect(images_in_dir, sub('data/images', 'Incoming Images', images_as_archived$archive_location))
        
        ##finding images that are possible duplicates or missing
        missing_archived_images <- setdiff(sub('Incoming Images', 'data/images', archived_and_incoming), images_in_archive)
        
      } else {
        message('Image moving process terminated. Please move files to correct folder and rerun process.
                Simply select the relevant AI folder when asked during rerun and process will continue.')
        return(list(verdict = 'STOP',
                    dta = NULL))
      }
    }
    
    
    ## safety check: images already recorded as archived BUT not in image archive
    ## moves images already archived but in incoming folder back to archive
    if(length(missing_archived_images) > 0){
      
      message(length(missing_archived_images), ' images recorded as archived, are trying to be rearchived but missing from the image archive.')
      message(paste('-', missing_archived_images, collapse = '\n'))
      message('Are you sure these images have been placed in the correct folder?\n\ny/n\n')
      message('Warning:\nIf "y", these will be moved to the archive and may overwrite existing images \n')
      readline()
      
      message('Please enter once more to confirm choice\n\ny/n')
      safetyChecker <- tolower(readline())
      
      if(safetyChecker %in% c('y', 'yes')){
        message('Moving previously archived images to archive.')
        dir_create(missing_archived_images)
        file_move(sub('data/images', 'Incoming Images', missing_archived_images), missing_archived_images)
      } else {
        message('Image moving process terminated. Please move files to correct folder and rerun process.
                Simply select the relevant AI folder when asked during rerun and process will continue.')
        return(list(verdict = 'STOP',
                    dta = NULL))
      }
    }
    
    ### safety check: what images are to be validated but not in validation batch (in case error after copying to validation folder)
    #message('Checking images already present in validation folder.')
    #images_already_recoreded_to_be_validated <- unique(intersect(dta_processed$joinkey, images_in_validation$joinkey)) # > 0 means not first try to add to validation
    #
    #if(length(images_already_recoreded_to_be_validated) > 0){
    #  message(length(images_already_recoreded_to_be_validated), ' of the ', length(unique(dta_processed$joinkey)),' have already been added to the current validation file previously.\n')
    #  message('Note:\nOnly the outstanding ', length(unique(dta_processed$joinkey)) - length(images_already_recoreded_to_be_validated), ' outstanding images will be processed.')
    #  dta_processed <- dta_processed[!dta_processed$joinkey %in% images_already_recoreded_to_be_validated, ]
    #}
    
    images_in_dir <- dir_ls(paste0(getwd(), '/Incoming Images/'), recurse = TRUE, type = 'file')
    dta_updated <- dta_processed[dta_processed$joinkey %in% images_in_dir,] # multi-detections not yet removed, so will be different to number outputs for joinkeys.
    
    return(list(verdict = 'SAFE', 
                dta = dta_updated))
  } else {
    return(list(verdict = 'STOP',
                dta = NULL))
  }
}

## Step 3: Check folder structure present for moving images to correct files
folder_checker <- function(detection){
  ## check to see if required file structure in place before copying
  ## validation folder check
  validation_folder_path <- paste0(getwd(), '/To Validate/')
  if(detection == 'validation'){
    if(!dir_exists(validation_folder_path)){
      message('Creating validation folder:\n', validation_folder_path)
      dir_create(validation_folder_path)
    }
    return(validation_folder_path)
  }
  
  ## species specific validation folder check
  if(detection != 'validation'){
    animal_folder_path <- paste0(validation_folder_path, detection)
    if(!dir_exists(animal_folder_path)){
      message('Creating species validation folder:\n', animal_folder_path)
      dir_create(animal_folder_path)
    }
    return(animal_folder_path)
  }
}

## Step 4: Ensure images are not overwritten with new file names during iterative processing
last_image_checker <- function(folder){
  image_list <- sort(dir_ls(folder, regexp = 'IMG_[0-9]+\\.JPG$'))
  if(length(image_list)==0){ 
    return(0)
  } else {
    return(max(as.integer(sub('IMG_([0-9]+)\\.JPG$', '\\1', basename(image_list))), na.rm = TRUE))
  }
}

## Phase 3 - Validation to Data Pipe -------------------------------------------
## Step 1: Validation Wrap-Up
validation_wrapup <- function(){
  
  message('Are you sure that you are ready to complete this round of validations?\n y/n')
  safety_checker <- tolower(readline())
  
  if(tolower(safety_checker) == 'y'){
    message('Detection Validation Process Confirmed\n')
    dir_create('data/temp')
    
    ## compile all required logs and files to commit to database
    master_file_compiler()
    
    ## batch_validation_confirmation to validated_image_log for long-term auditing
    batch_tracker_update()
    
    ## image log update
    image_logger('validate')
    
    ## validation commit or terminate
    validation_commit()
  } else {
    message('Validation Wrap Up Aborted\n')
  }
} ## Put the joining key here or in validation wrap up? What does the csv look like here and where do we need it to be?

master_file_compiler <- function(){
  ## collating human_validation.csv files
  #validated_images <- validated_images
  validated_images <- validation_file_compiler()
  validated_images$human_validated <- tolower(trimws(as.character(validated_images$human_validated), which = 'both')) == 'true'
  
  ## loading required csvs
  # ai_files
  ai_master <- read.csv('data/logs/ai_database.csv', header = TRUE, stringsAsFactors = FALSE) %>% 
    select(joinkey, label, confidence, detection_id, sequence_id, datetime)
  ai_master$species <- ai_master$label
  
  # batch to be processed information
  current_batch_tracker <- read.csv('data/logs/current_validation_batch_tracker.csv', header = TRUE, stringsAsFactors = FALSE)
  
  ## joining to form central table
  master.temp <- left_join(current_batch_tracker, validated_images, 'validation_file_path') %>% 
    mutate(image_validated = ifelse(validation_level == 'image' & human_validated == TRUE, TRUE, FALSE),
           sequence_validated = ifelse(validation_level == 'sequence' & human_validated == TRUE, TRUE, FALSE))%>% 
    mutate(across(c(sp_a, sp_b, sp_c), as.character))%>% 
    pivot_longer(cols = c(sp_a, n_obs_a, sp_b, n_obs_b, sp_c, n_obs_c), 
                 names_to = c(".value", "group"),
                 names_pattern = "(sp|n_obs)_(a|b|c)",
                 values_drop_na = TRUE) %>% 
    mutate(group_id = ifelse(validation_level == 'sequence', sequence_id, joinkey)) %>% 
    group_by(group_id) %>% 
    mutate(n_sp = n()) %>% 
    ungroup() %>% 
    select(-group_id)
  
  master.temp$sp[master.temp$sp == 'human'] <- 'person'
  master.temp$sp_ai <- label_standardiser(master.temp$sp)
  
  ## multi-species join keep
  master.temp <- full_join(ai_master[ai_master$joinkey %in% master.temp$joinkey,], master.temp, by = c('joinkey' = 'joinkey', 'species' = 'sp_ai')) %>% 
    rename(detection_id = detection_id.x,
           sequence_id = sequence_id.y,
           max_confidence = confidence) %>% 
    mutate(species = ifelse(is.na(n_obs) & is.na(n_sp), NA, species),
           site = sub(' -.*', '', sub('.*/Incoming Images/([^/]+)/.*', '\\1', joinkey)), # ensures only the number is kept. All data entered past the '-' are removed from being called site (fit jamie workflow)
           species = sp) %>% 
    select(joinkey, site, species, label, max_confidence, n_obs, n_sp,
           phenotype, unique_cat_id, detection_id, sequence_id, datetime, image_validated,
           sequence_validated, validated_by) %>% 
    arrange(joinkey)
  
  ## saving to temporary folder before committing
  if(file_exists('data/logs/validation_archive.csv')){
    message('Updating data/logs/validation_archive.csv\n')
    old_archive <- read.csv('data/logs/validation_archive.csv', header = TRUE, stringsAsFactors = FALSE)
    updated_archive <- rbind(old_archive, master.temp)
    
    write.csv(updated_archive, 'data/temp/validation_archive.csv', row.names = FALSE)
  } else {
    message('Creating data/logs/validation_archive.csv\n')
    write.csv(master.temp, 'data/temp/validation_archive.csv', row.names = FALSE)
  }

  
  ## adding required metadata ##make optional
  if(file_exists('data/survey_design.csv')){
    site_info <- read.csv('data/survey_design.csv', header = TRUE, stringsAsFactors = FALSE) %>% 
      rename(Date_of_deployment = Date_of_deployment_.dd.mm.yy.,
             Time_of_deployment = Time_of_deployment_.hh.mm.,
             Easting = Easting_.x.,
             Northing = Northing_.y.)
    master.temp <- left_join(master.temp, site_info, by = c('site' = 'Camera_grid_number'))
    
  } else {
    message('No data/survey_design.csv file found. No metadata added to master file.\n')
  }
  
  
  if(file_exists(paste0('data/', sub('.*?([^/]+)$','\\1', getwd()), '_Master.csv'))){
    message('Detected ', paste0('data/', sub('.*?([^/]+)$','\\1', getwd()), '_Master.csv'))
    message('Updating ', paste0('data/', sub('.*?([^/]+)$','\\1', getwd()), '_Master.csv\n'))
    
    old_master <- read.csv(paste0('data/', sub('.*?([^/]+)$','\\1', getwd()), '_Master.csv'), header = TRUE, stringsAsFactors = FALSE)
    old_master <- old_master[!old_master$joinkey %in% master.temp$joinkey, ]
    new_master <- rbind(old_master, master.temp)
    
    write.csv(new_master, paste0('data/temp/', sub('.*?([^/]+)$','\\1', getwd()), '_Master.csv'), row.names = FALSE)
    
  } else {
    message(paste0('\"data/', sub('.*?([^/]+)$','\\1', getwd()), '_Master.csv\" not detected.
                     \nIs this correct? (y/n).'))
    safety_checker <- tolower(readline())
    if(safety_checker == 'y' || safety_checker == 'yes'){
      message(paste0('Creating data/', sub('.*?([^/]+)$','\\1', getwd()), '_Master.csv'))
      write.csv(master.temp, paste0('data/temp/', sub('.*?([^/]+)$','\\1', getwd()), '_Master.csv'), row.names = FALSE)
    } else {
      stop('Aborting Process')
    }
  }
}

label_standardiser <- function(dta){
  label_dictionary <- c('human' = 'person',
                        'mink' = 'mustelid',
                        'stoat' = 'mustelid',
                        'pine marten' = 'mustelid',
                        'pole cat' = 'mustelid',
                        'weasel' = 'mustelid',
                        'wild cat' = 'cat',
                        'brown hare' = 'lagomorph',
                        'mountain hare' = 'lagomorph',
                        'rabbit' = 'lagomorph',
                        'grey squirrel' = 'squirrel',
                        'gray squirrel' = 'squirrel',
                        'red squirrel' = 'squirrel',
                        'capercaillie' = 'bird',
                        'pheasant' = 'bird',
                        'red-legged partridge' = 'bird'
                        )
  
  dta_corrected <- ifelse(dta %in% names(label_dictionary), label_dictionary[dta], dta)
  return(dta_corrected)
}

validation_file_compiler <- function(){
  ## collates validation folders (ONLY RUN IF DONE WITH ALL VALIDATIONS)
  ## load needed databases
  message('Combining validation files')
  csv_list <- dir_ls('To Validate/', recurse = TRUE, glob = '*.csv')
  file_list <- csv_list[grepl('.*?/human_validation.csv', csv_list)]
  
  validated_images <- NULL
  
  ## combining detection files
  for(file in file_list){
    ## reads and binds validated folders for all detections iteratively
    file.temp <- read.csv(file, header = TRUE, stringsAsFactors = FALSE) %>% 
      mutate(validation_file_path = paste(dirname(file), image, sep = '/'))
    
    validated_images <- rbind(validated_images, file.temp)
  }
  
  validated_images$validation_file_path <- path_abs(validated_images$validation_file_path)
  validated_images <- validated_images[!duplicated(validated_images),]
  
  return(validated_images)
}

batch_tracker_update <- function(){
  current_batch_tracker <- read.csv('data/logs/current_validation_batch_tracker.csv', header = TRUE, stringsAsFactors = FALSE)
  current_batch_tracker$validation_confirmation_date <- format(as_datetime(now()))
  
  if(file_exists('data/validated_images_tracker.csv')){
    message('Updating data/validated_images_tracker.csv\n')
    image_tracker_master <- read.csv('data/validated_images_tracker.csv', header = TRUE, stringsAsFactors = FALSE)
    current_batch_tracker <- rbind(image_tracker_master, current_batch_tracker)
    
    write.csv(current_batch_tracker, 'data/temp/validated_images_tracker.csv', row.names = FALSE)
  } else {
    message('Creating data/validated_images_tracker.csv\n')
    write.csv(current_batch_tracker, 'data/temp/validated_images_tracker.csv', row.names = FALSE)
  }
}

validation_commit <- function(){
  
  files_in_temp <- dir_ls('data/temp', type = 'file')
  files <- paste0(' - ', files_in_temp, collapse = '\n')
  
  message('All files are compiled and updated with new validation commits. Would you like to commit these changes to the database?\n\n',
          'NOTE\n',
          'Commiting changes will:\n',
          'Remove all temporary files and folders\n',
          ' - To Validate\n',
          ' - logs/current_validation_batch_tracker.csv\n',
          ' - data/temp\n\n',
          'Make updates to the following files permanent:\n', 
          files)

  message('Would you like to continue?\n
            y/n')
  response <- tolower(readline())
  
  
  if(response == 'y'|| response == 'yes'){
    message('Commit confirmed.\n')
    # Writing changes to files
    for(file in dir_ls('data/temp')){
      message('Saving changes to ', file)
      logs <- c('data/temp/image_log.csv', 'data/temp/validation_archive.csv')
      if(file %in% logs){
        path <- sub('/temp', '/logs', file)
      } else {
        path <- sub('/temp', '', file)
      }
      write.csv(read.csv(file, header = TRUE, stringsAsFactors = FALSE), path, row.names = FALSE)
      message('Commits to ', path, ' confirmed.\n')
    }
    
    message('All commits completed. Removing temporary data.\n')
    
    # Removing Temporary Files
    temp_list <- c('data/logs/current_validation_batch_tracker.csv', 'To Validate/', 'data/temp/')
    for(file in temp_list){
      message('Deleting ', file)
      if(is_dir(file)){
        dir_delete(file)
      } else {
        file_delete(file)
      }
    }
    
    message('Validation commit process completed.')
    
  } else {
    message('Commits aborted.\nRemoving data/temp.')
    dir_delete('data/temp')
  }
}


## Retrospective Calling
#revalidate_data <- function(species = 'all'){
  ## read in existing dataset
  
  ## filter based on current species requests
  
  ## copy images to validation folder
  
  ## update master sheet
 # master_data <- 
#}
