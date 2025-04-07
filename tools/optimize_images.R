#' Optimize PNG images in the package
#'
#' @param pkg_dir Package root directory, default is current directory
#' @param target_dirs Directories to process
#' @param quality Quality setting for JPEG (1-100), lower means smaller files
#' @param compression_level PNG compression level (0-9), higher means more compression
#' @param resize Percentage to resize images (e.g., 70 means resize to 70% of original)
#' @param backup Create backup of original images
#' @param verbose Print detailed information
#' @return Invisible NULL
optimize_package_images <- function(pkg_dir = ".",
                                    target_dirs = c("man/figures", "vignettes/figures"),
                                    quality = 75,
                                    compression_level = 9,
                                    resize = 70,
                                    backup = TRUE,
                                    verbose = TRUE) {

  # Check if required packages are available
  if (!requireNamespace("magick", quietly = TRUE)) {
    message("Please install the 'magick' package: install.packages('magick')")
    return(invisible(NULL))
  }

  # Process each target directory
  for (target_dir in target_dirs) {
    full_dir <- file.path(pkg_dir, target_dir)

    if (!dir.exists(full_dir)) {
      if (verbose) message("Directory not found: ", full_dir)
      next
    }

    # Get all image files in the directory
    img_files <- list.files(full_dir,
                            pattern = "\\.(png|jpe?g|gif)$",
                            full.names = TRUE,
                            ignore.case = TRUE)

    if (length(img_files) == 0) {
      if (verbose) message("No image files found in ", full_dir)
      next
    }

    if (verbose) message("Found ", length(img_files), " image files in ", full_dir)

    # Create backup directory if needed
    if (backup) {
      backup_dir <- file.path(full_dir, "backup")
      if (!dir.exists(backup_dir)) {
        dir.create(backup_dir)
      }
    }

    # Track total size reduction
    original_total <- 0
    optimized_total <- 0

    # Process each file
    for (img_file in img_files) {
      # Get file info before optimization
      original_size <- file.info(img_file)$size
      original_total <- original_total + original_size

      # Create backup if requested
      if (backup) {
        backup_file <- file.path(backup_dir, basename(img_file))
        file.copy(img_file, backup_file, overwrite = TRUE)
      }

      # Optimize the image
      if (verbose) message("Optimizing ", basename(img_file), "...")

      # Read the image
      img <- magick::image_read(img_file)

      # Get original dimensions
      img_info <- magick::image_info(img)
      original_width <- img_info$width
      original_height <- img_info$height

      # Resize if requested
      if (resize < 100) {
        new_width <- as.integer(original_width * resize / 100)
        new_height <- as.integer(original_height * resize / 100)
        img <- magick::image_resize(img, paste0(new_width, "x", new_height))

        if (verbose) {
          message(sprintf("  Resized from %dx%d to %dx%d",
                          original_width, original_height,
                          new_width, new_height))
        }
      }

      # Adjust quality and compression based on file type
      file_ext <- tolower(tools::file_ext(img_file))

      if (file_ext %in% c("jpg", "jpeg")) {
        img <- magick::image_write(img, img_file, format = "jpeg", quality = quality)
      } else if (file_ext == "png") {
        img <- magick::image_write(img, img_file, format = "png", quality = quality)

        # Additional PNG compression using magick
        system2("magick", c("convert", img_file,
                            "-quality", "90%",
                            "-define", paste0("png:compression-level=", compression_level),
                            img_file))
      } else {
        img <- magick::image_write(img, img_file)
      }

      # Get file info after optimization
      optimized_size <- file.info(img_file)$size
      optimized_total <- optimized_total + optimized_size

      # Calculate size reduction
      size_reduction <- original_size - optimized_size
      percent_reduction <- (size_reduction / original_size) * 100

      if (verbose) {
        message(sprintf("  Reduced from %s to %s (%.1f%% reduction)",
                        format_bytes(original_size),
                        format_bytes(optimized_size),
                        percent_reduction))
      }
    }

    # Report overall reduction for this directory
    if (verbose && length(img_files) > 0) {
      total_reduction <- original_total - optimized_total
      total_percent <- (total_reduction / original_total) * 100

      message(sprintf("\nTotal reduction for %s: %s to %s (%.1f%% reduction)",
                      target_dir,
                      format_bytes(original_total),
                      format_bytes(optimized_total),
                      total_percent))
    }
  }

  # Final message
  if (verbose) {
    message("\nImage optimization complete!")
    if (backup) {
      message("Original images backed up in respective backup/ directories.")
    }
  }

  return(invisible(NULL))
}

#' Format bytes into human-readable format
#'
#' @param bytes Number of bytes
#' @return Character string with formatted size
format_bytes <- function(bytes) {
  units <- c("B", "KB", "MB", "GB")
  i <- 1

  while (bytes >= 1024 && i < length(units)) {
    bytes <- bytes / 1024
    i <- i + 1
  }

  return(sprintf("%.2f %s", bytes, units[i]))
}

# Add a function to optimize a specific image
optimize_single_image <- function(image_path,
                                  quality = 75,
                                  compression_level = 9,
                                  resize = 70,
                                  backup = TRUE) {
  if (!file.exists(image_path)) {
    message("Image file not found: ", image_path)
    return(invisible(NULL))
  }

  # Create backup
  if (backup) {
    backup_dir <- file.path(dirname(image_path), "backup")
    if (!dir.exists(backup_dir)) {
      dir.create(backup_dir)
    }
    backup_file <- file.path(backup_dir, basename(image_path))
    file.copy(image_path, backup_file, overwrite = TRUE)
  }

  # Get original size
  original_size <- file.info(image_path)$size

  # Read and optimize the image
  img <- magick::image_read(image_path)

  # Get original dimensions
  img_info <- magick::image_info(img)
  original_width <- img_info$width
  original_height <- img_info$height

  # Resize if requested
  if (resize < 100) {
    new_width <- as.integer(original_width * resize / 100)
    new_height <- as.integer(original_height * resize / 100)
    img <- magick::image_resize(img, paste0(new_width, "x", new_height))
  }

  # Adjust quality and compression based on file type
  file_ext <- tolower(tools::file_ext(image_path))

  if (file_ext %in% c("jpg", "jpeg")) {
    img <- magick::image_write(img, image_path, format = "jpeg", quality = quality)
  } else if (file_ext == "png") {
    img <- magick::image_write(img, image_path, format = "png", quality = quality)

    # Additional PNG compression using magick
    system2("magick", c("convert", image_path,
                        "-quality", "90%",
                        "-define", paste0("png:compression-level=", compression_level),
                        image_path))
  } else {
    img <- magick::image_write(img, image_path)
  }

  # Get optimized size
  optimized_size <- file.info(image_path)$size

  # Calculate reduction
  size_reduction <- original_size - optimized_size
  percent_reduction <- (size_reduction / original_size) * 100

  message(sprintf("Optimized %s: %s to %s (%.1f%% reduction)",
                  basename(image_path),
                  format_bytes(original_size),
                  format_bytes(optimized_size),
                  percent_reduction))

  return(invisible(NULL))
}

# Function to optimize all PNG files in man/figures
optimize_man_figures <- function() {
  man_dir <- "man/figures"
  if (!dir.exists(man_dir)) {
    message("man/figures directory not found")
    return(invisible(NULL))
  }

  optimize_package_images(target_dirs = man_dir, resize = 60, quality = 75)
}

# Function to optimize all PNG files in vignettes/figures
optimize_vignettes_figures <- function() {
  vig_dir <- "vignettes/figures"
  if (!dir.exists(vig_dir)) {
    message("vignettes/figures directory not found")
    return(invisible(NULL))
  }

  optimize_package_images(target_dirs = vig_dir, resize = 60, quality = 75)
}

# Run optimization on specific large images
optimize_large_images <- function() {
  large_images <- c(
    "man/figures/annotate1.png",
    "man/figures/annotate2.png",
    "man/figures/annotating_process.png",
    "man/figures/code_and_theme_hierarchy.png"
  )

  for (img in large_images) {
    if (file.exists(img)) {
      message("Optimizing large image: ", img)
      optimize_single_image(img, resize = 60, quality = 70, compression_level = 9)
    }
  }
}

# Enable direct execution of this script
if (!interactive()) {
  message("Running image optimization script...")

  # Install magick if needed
  if (!requireNamespace("magick", quietly = TRUE)) {
    message("Installing magick package...")
    install.packages("magick")
  }

  # Run optimization
  optimize_package_images()
}

# If running interactively, provide a message
if (interactive()) {
  message("To run the optimization, use one of these functions:")
  message("  - optimize_package_images() - optimize all images")
  message("  - optimize_man_figures() - optimize only man/figures")
  message("  - optimize_vignettes_figures() - optimize only vignettes/figures")
  message("  - optimize_large_images() - optimize specific large images")
}
