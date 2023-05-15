
rootDir <- "docDB"
rootPath <- ""

verbose <- FALSE;

verbose_message <- function(msg) {
  if (verbose) {
    message(msg)
  }
}

configDB <- function(root, path = "") {
  

  if (path != "") {
    path_dir <- file.path(path, root)
    if (!dir.exists(path_dir)) {
      dir.create(path_dir)
    }
    
    rootPath <<- path
  } else {
    if(!dir.exists(root)){
      dir.create(root)
    }
    
  }
}



getExtn<- function (fileName){
  
  
  file_ext = "";  
  
  if(grepl(".jpg" , fileName , fixed=TRUE)){
    file_ext = ".jpg"
  }
  
  if(grepl(".mp3" , fileName , fixed=TRUE)){
    file_ext = ".mp3"
  }
  
  if(grepl(".tiff" , fileName , fixed=TRUE)){
    file_ext = ".tiff"
  }
  

  
  
  return(file_ext)
  
}


getTags <- function(fileName) {
  # Find tags in the file name
  
  
 file_ext = getExtn(fileName);
 

 
 if(file_ext == ""){
   stop(sprintf("[ERROR] file has unsupported extension %s\n",fileName))
 } 
 
 processed_filename = gsub(file_ext,"",fileName)
 tags <- unlist(strsplit(processed_filename, " "))[-1]
 
 return_v <- unlist(lapply(tags, trimws))
 
 
 return(return_v)

}


getFileName <- function(fileName) {
  
  file_ext = getExtn(fileName);

  if(file_ext == ""){
    stop(sprintf("[ERROR] file has unsupported extension %s\n",fileName))
  } 
  
  processed_filename = gsub(file_ext,"",fileName)
  fileName <- unlist(strsplit(processed_filename, " "))[1]
  
  trimmed_filename = gsub("^\\s+", "", fileName)
  
  return( paste0(trimmed_filename , file_ext))
  
}


genObjPath <- function (root, tag){
  tag<- gsub("#","",tag)
  
  if(rootPath != ""){
    objPath = file.path(rootPath,root,tag)
    return(objPath)
  } else {
    objPath = file.path(root,tag)
    return(objPath)
  }
  
} 



storeObjs<- function (folder, root , verbose = FALSE){
  if(!dir.exists(folder)){
    stop(sprintf("[ERROR] Input folder does not exists %s",folder))
  }
  
  if(verbose){
    verbose <<- TRUE
  }
  
  file_list <- list.files(folder)
  
  for(file_name in file_list){
    file_path = file.path(folder,file_name)
    
    
    tags <- getTags(file_name)
    
    for( tag in tags ){
      dest_dir <- genObjPath(root,tag)
      if(!dir.exists(dest_dir)){
        dir.create(dest_dir)
      }
      
      dest_path <- file.path(dest_dir , getFileName(file_name))
      
      
      verbose_message(sprintf("[INFO] copying %s to %s... \n",file_path,dest_path))
      file.copy(file_path , dest_path)
    }
    
    
  }
  
  
  
  
}

main <- function()
  
{
  
  configDB(rootDir , "")
  
  storeObjs("testData",rootDir,TRUE)
  
  
}





main()

