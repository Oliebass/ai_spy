# Load Libraries
source('ai_workflow_functions.R')
setwd('C:/Users/oh57/Desktop/ai_spy/demo/')

# Step 1: Create Backups of validation files in case things go wrong:
message('Rollback function initiated\n')

if(file_exists('data/logs/current_validation_batch_tracker.csv')){
  current_validations <- read.csv('data/logs/current_validation_batch_tracker.csv', header = TRUE, stringsAsFactors = FALSE)
  
validation_files_backup <- NULL # back up of all validation not to be deleted incase
for(species in dir_ls('To Validate', type = 'directory')){
  message('Backing up ', paste(species, 'human_validation.csv', sep = '/'))
  species_name <- basename(species)
  val_file <- read.csv(paste(species, 'human_validation.csv', sep = '/'), header = TRUE, stringsAsFactors = FALSE)
  
  validation_files_backup <- rbind(validation_files_backup, val_file |> mutate(image = paste(path_abs(species), image, sep = '/')))
  
  if(!dir_exists(paste('data/backups/temp_rollback', species, sep = '/'))){
    message('Creating ', paste('data/backups/temp_rollback', species_name, sep = '/'), '\t')
    dir_create(paste('data/backups/temp_rollback', species_name, sep = '/'))
  }
  write.csv(val_file, paste('data/backups/temp_rollback', species_name, 'human_validation.csv', sep = '/'), row.names = FALSE)
}
message('Creating master validation backup')
write.csv(validation_files_backup, 'data/backups/human_validations_backup.csv', row.names = FALSE)

message('Creating current_validation_batch_tracker.csv backup')

write.csv(current_validations, 'data/backups/current_validation_batch_tracker.csv', row.names = FALSE)

message('Backup process completed.\n')


# Step 2: Load required database ####
message('Rollback initiated.\n')

recorded_image <- current_validations$validation_file_path
images <- path_abs(dir_ls('To Validate', recurse = TRUE, type = 'file', glob = '*.JPG|*.MP4'))
uncatalogged_images <- images[!images %in% recorded_image]

if(length(uncatalogged_images) > 0){
  message(length(uncatalogged_images), ' uncatalogged images detected. Would you like to proceed with rollback?\n\n NOTE: You will be asked at each species to confirm if what is to be deleted appears to be correct.\nn/Y')
  input <- tolower(readline())
  if(input == 'y' | input == 'yes'){
    ## Script to remove directories
    confirmation_tracker <- NULL
    for(species_dir in dir_ls('To Validate', type = 'directory')){
      # Fix 1: update the csv's
      message('Removing additional detections in: \n\t', paste(species_dir, 'human_validation.csv', sep = '/'))
      
      incorrect_table <- read.csv(paste(species_dir, 'human_validation.csv', sep = '/'), header = TRUE, stringsAsFactors = FALSE)
      incorrect_table$file_path <- paste(path_abs(species_dir), incorrect_table$image, sep = '/')
      corrected_table <- incorrect_table[incorrect_table$file_path %in% current_validations$validation_file_path,]
      
      message('Does the following seem correct? If so, rollback will happen for this species.\n n/Y')
      message(basename(species_dir), '\nTracked entries: ', nrow(corrected_table), '\nUntracked entries: ', nrow(incorrect_table)-nrow(corrected_table))
      
      del_confirm <- tolower(readline())
      if(del_confirm == 'y' | del_confirm == 'yes'){
        message(basename(species_dir), ' rollback confirmed')
          write.csv(corrected_table %>% 
                      select(!file_path), paste(species_dir, 'human_validation.csv', sep = '/'), row.names = FALSE)
          message('\t', paste(species_dir, 'human_validation.csv', sep = '/'), ' updated and corrected.')
          
          # Fix 2: remove the excess images
          message('\tRemoving unlogged images from ', species_dir)
          image_list <- path_abs(dir_ls(species_dir, recurse = TRUE, type = 'file', glob = '*.JPG|*.MP4'))
          extra_images <- image_list[!image_list %in% corrected_table$file_path]
          
          #corrections <- append(corrections, extra_images)
          
          file_delete(extra_images)
          
          message('\t', length(extra_images), ' images removed.')
      } else if(del_confirm == 'n' | del_confirm == 'no') {
        message('Incorrect rollback numbers confirmed. "', basename(species_dir), '" rollback will be skipped.\nSkipped file record will be saved in "data/backups/skipped_confirmations.csv"')
        skipped_confirmation <- data.frame(untracked_skipped_images =  incorrect_table$file_path[!incorrect_table$file_path %in% current_validations$validation_file_path])
        confirmation_tracker <- rbind(confirmation_tracker, skipped_confirmation)
      } else {
        message('Input not recognised. "', basename(species_dir), '" rollback will be skipped.\nSkipped file record will be saved in "data/backups/skipped_confirmations.csv"')
        skipped_confirmation <- data.frame(untracked_skipped_images =  incorrect_table$file_path[!incorrect_table$file_path %in% current_validations$validation_file_path])
        confirmation_tracker <- rbind(confirmation_tracker, skipped_confirmation)
      }
    }
      
      if(is.null(confirmation_tracker)){
        message('Rollback successful.')
      } else {
        message('Rollback partially successful.')
        message('Saving "data/backups/skipped_confirmations.csv"')
        write.csv(confirmation_tracker, 'data/backups/skipped_confirmations.csv', row.names = FALSE)
      }
      
      message('Removing temporary rollback backup files.')
      dir_delete('data/backups/temp_rollback')
      
      message('Backup removed successfully.')
      message('Rollback process completed')
      
  } else if(input == 'n' | input == 'no'){
      message('Rollback cancelled.')
      message('Removing rollback backup files.')
      dir_delete('data/backups/temp_rollback')
      message('Backup removed successfully.')
      message('Rollback process cancelled')
  } else {
      message('Input not recognised. Rollback terminating')
      message('Removing rollback backup files.')
      dir_delete('data/backups/temp_rollback')
      message('Backup removed successfully.')
      message('Rollback process terminated')
    }
    
  } else {
  message('No unrecorded images in "To Validate" are recorded in data/logs/current_validation_batch_tracker.csv.\nRollback not needed.\n\nTerminating rollback.')
  message('Removing rollback backup files.')
  dir_delete('data/backups/temp_rollback')
  message('Backup removed successfully.')
  message('Rollback process completed')
}
} else {
  message('Critical file not found:\t
          "data/logs/current_validation_batch_tracker.csv"')
  message('Please ensure you are in the correct directory and that the file is present before restarting rollback.')
}
