# Remove documentation cache
if (file.exists("doc")) {
  message("Removing doc/ directory...")
  unlink("doc", recursive = TRUE)
}

# Remove meta directory
if (file.exists("Meta")) {
  message("Removing Meta/ directory...")
  unlink("Meta", recursive = TRUE)
}

# Remove vignette caches
vig_cache_dirs <- list.files("vignettes", pattern = "_cache$", full.names = TRUE)
if (length(vig_cache_dirs) > 0) {
  message("Removing vignette cache directories...")
  invisible(sapply(vig_cache_dirs, unlink, recursive = TRUE))
}

# Remove vignette files directories
vig_files_dirs <- list.files("vignettes", pattern = "_files$", full.names = TRUE)
if (length(vig_files_dirs) > 0) {
  message("Removing vignette files directories...")
  invisible(sapply(vig_files_dirs, unlink, recursive = TRUE))
}

# Clean any temporary files
temp_files <- list.files(".", pattern = "^\\~|\\.(o|so)$", full.names = TRUE, recursive = TRUE)
if (length(temp_files) > 0) {
  message("Removing temporary files...")
  invisible(sapply(temp_files, unlink))
}

# Remove backup directories (but keep the original backups)
if (FALSE) { # Set to TRUE if you want to remove backup directories too
  backup_dirs <- c(
    "man/figures/backup",
    "vignettes/figures/backup"
  )

  for (dir in backup_dirs) {
    if (dir.exists(dir)) {
      message("Removing backup directory: ", dir)
      unlink(dir, recursive = TRUE)
    }
  }
}

message("Cleanup complete!")
