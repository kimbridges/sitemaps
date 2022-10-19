#' Reads EXIF data to get coordinates and other info from all the JPG files in a folder.
#'
#' @description
#' You can get most digital phone cameras to record location information in the
#' EXIF data that is part of a JPG image. This function extract that data.
#' 
#' There are several useful fields that are retrieved along with the geographic
#' coordinates. These include the contents of the Description field, the date,
#' and the time of the photo.
#' 
#' A reference number (the file order in the folder) and the file name are also
#' returned.
#'
#' @param folder The name of the folder with a set of JPG images
#'
#' @return table_info with file number, name, description, lat, lon, direction, 
#' view width, distance, date, time
#' @import lubridate exifr stats
#' @export
#'

site_photos <- function(folder){
  
  ## Build a list of all the JPG files in a folder
  files <- list.files(path=folder,
                      pattern="*.jpg",
                      full.names=TRUE,
                      recursive=FALSE)
  
  ## Count the photos in the folder
  no_files <- length(files)
  
  ## Prepare a place to store the photo data
  table_info <- NULL
  table_info <- data.frame(matrix(ncol=10, nrow=0))
  table_info <- stats::setNames(table_info,
                        c("number",
                          "file",
                          "description",
                          "lat",
                          "lon",
                          "dir",
                          "fov",
                          "dist",
                          "date",
                          "time"))
  
  ## It is possible to specify the EXIF tags which have useful information.
  ## Without this, all (about 150) tags are downloaded.
  ## tags <- c("Description", "GPSLatitude", "GPSLongitude", 
  ##           "GPSImgDirection", "FOV", 
  ###          "GPSDestDistance", "DateTimeOriginal")
  
  ## Process each of the photos, one at a time
  for (i in 1:no_files){
    
    ## Get the EXIF info (here, without limiting with tags=tags)
    photo_exif <- exifr::read_exif(path=files[i],recursive = FALSE)

    ## Decode the EXIF data and put in easy-to-use variables
    ## Several tags are not always found in the EXIF data.
    ## Make sure these don't cause problems by their absence.
    
    ## If the geographic coordinates are missing, make them NA
    if(!"GPSLongitude" %in% names(photo_exif)){
      in_lat <- NA
      in_lon <- NA} else {in_lat <- photo_exif$GPSLatitude
                          in_lon <- photo_exif$GPSLongitude}
    # ## iPhone variant 1
    # if("Longitude" %in% names(photo_exif)){
    #   in_lat <- photo_exif$Latitude
    #   in_lon <- photo_exif$Longitude}
    # 
    # ## iPhone variant 2
    # if("longitude" %in% names(photo_exif)){
    #   in_lat <- photo_exif$latitude
    #   in_lon <- photo_exif$longitude}

    ## Check for direction and fov tags and include if they exist.
    if("GPSImgDirection" %in% names(photo_exif)){
        in_dir <- photo_exif$GPSImgDirection} else {in_dir <- 0}

    if("FOV" %in% names(photo_exif)){
        in_fov <- photo_exif$FOV} else {in_fov <- 0}

    ## Check for description tag and include if it exists.
    if("Description" %in% names(photo_exif)){
        in_description <- photo_exif$Description} else {in_description <- "none"}
    
    ## 0=NA, 1=macro, 2=close, 3=distant
    if("SubjectDistanceRange" %in% names(photo_exif)){
      in_dist <- photo_exif$SubjectDistanceRange} else {in_dist <- 0}

    ## Process the time, date and description.
    lub_str   <- ymd_hms(photo_exif$DateTimeOriginal)
    in_hour   <- hour(lub_str)
    in_minute <- minute(lub_str)
    in_minute <- sprintf("%02d",in_minute)
    in_time   <- paste0(in_hour,":",in_minute)

    in_date <- lubridate::fast_strptime(substring(photo_exif$DateTimeOriginal,1,10),
                                        "%Y:%m:%d")
    
    ## Get the filename.
    f_name <- basename(files[i])
    
    ## Build a row entry and then add it to the table.
    table_row  <- data.frame(number      = i,
                             file        = f_name,
                             description = in_description,
                             lat         = in_lat,
                             lon         = in_lon,
                             dir         = in_dir,
                             fov         = in_fov,
                             dist        = in_dist,
                             date        = in_date,
                             time        = in_time)
    
    table_info <- rbind(table_info, table_row)
    
  } ## End photo loop
  
  ## Return EXIF information
  return(table_info)
}