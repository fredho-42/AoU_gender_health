# =========================================================================
# Script: End-of-Day Bucket Sync
# Purpose: Backs up selected local PD folders to the Workspace Bucket
# =========================================================================

my_bucket <- "gs://archive-wb-shiny-melon-1268"

folders_to_sync <- c("data", "scripts", "results", "reports")

message("Starting backup to: ", my_bucket)

for (folder in folders_to_sync) {
  message("--> Syncing ~/", folder, " ...")
  
  cmd <- sprintf("gcloud storage rsync ~/analysis/%s/ %s/%s/ --recursive", folder, my_bucket, folder)
  
  system(cmd)
}

message("✅ All selected folders successfully backed up to the bucket!")