# =========================================================================
# Script: Start-of-Day Bucket Sync (Pull)
# Purpose: Pulls the latest files from the Workspace Bucket to local PD
# =========================================================================

my_bucket <- "gs://archive-wb-shiny-melon-1268"

folders_to_sync <- c("data", "scripts", "results", "reports")

message("Starting workspace sync from: ", my_bucket)

for (folder in folders_to_sync) {
  message("--> Pulling ", folder, " to local disk (", folder, ") ...")
  
  cmd <- sprintf("gcloud storage rsync %s/%s/ ~/analysis/%s/ --recursive", my_bucket, folder, folder)
  
  system(cmd)
}

message("✅ Workspace successfully synced! You are ready to code.")