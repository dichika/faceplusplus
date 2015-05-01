#' @export
getDetect <- function(file, secret=options()$FACEPP_SECRET, key=options()$FACEPP_KEY){
  require("httr")
  if(grepl("^http|^https", file)){
    u <- sprintf("https://apius.faceplusplus.com/v2/detection/detect?url=%s&api_secret=%s&api_key=%s&attribute=glass,pose,gender,age,race,smiling",
                 file,secret,key
    )
    res <- GET(u)
  } else{
    u <- sprintf("https://apius.faceplusplus.com/v2/detection/detect?api_secret=%s&api_key=%s&attribute=glass,pose,gender,age,race,smiling",
                 secret, key
    )
    res <- POST(u, body=list(img=upload_file(file)))    
  }
  res <- content(res, as="text")
  res <- jsonlite::fromJSON(res)
  return(res$face)
}

#' @export
photo2AgefromMac <- function(){
  old <- setwd("~/Pictures/iPhoto Library.photolibrary/Masters/")
  f <- file.choose()
  setwd(old)
  res <- getDetect(f)
}