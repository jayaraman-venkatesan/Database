
rootDir <- "docDB"
rootPath <- ""

verbose <- FALSE;

verbose_message <- function(msg) {
  if (verbose) {
    message(msg)
  }
}

#' ConfigDB
#' 
#' Configure initial folders for document database
#'
#' @param root name of the root folder for database
#' @param path path where the root folder should be present
#'
#'
#' @examples
#' configDB(rootDir,"")
#' configDB(rootDir,"/path/to/rootDir")
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



#' getExtn
#' 
#' Finds the extension of a file or returns empty string if not found
#'
#' @param fileName name of the file to get extension from
#'
#' @return extention of the file
#'
#' @examples
#' 
#' getExtn(test.jpg) returns jpg
#' getExtn(test.abc) returns ""
getExtn<- function (fileName){
  
  
  file_ext = "";  
  
  file_ext = tools::file_ext(fileName)
  
  return(paste0(".",file_ext))
  
}


#' getTags
#' 
#' Finds the tags from file name and returns the tags as vector
#'
#' @param fileName name of the file to extract tags from
#'
#' @return vector of tags associated with file
#'
#' @examples
#' 
#' getTags(test #abc #def.jpg)
#' returns #abc , #def as vectors
#' 
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


#' getFileName
#' 
#' Extracts and returns fileName from the filenames with tags
#'
#' @param fileName filenames containing tags associated with them
#'
#' @return filename without tags
#' 
#' @examples
#' #getFileName(test #abc #def.jpg) returns test.jpg
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


#' genObjPath
#' 
#' Generates destination path of where the tagged file should be stored 
#'
#' @param root Denotes the root directory of database
#' @param tag tag associated with the file
#'
#' @return destination path of the directory where the file should be stored in
#' @export
#'
#' @examples
#' genObjPath(rootDir,test) 
#' 
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



#' storeObjs
#' 
#' Copies all files in the specified in the folder argument to their correct 
#' folders underneath the root folder speficied to the tags associated with 
#' each files
#'
#' @param folder The folder in which files are stored
#' @param root root folder of the database
#' @param verbose modify the code for the function so that it prints a message
#'  for every file that is copied
#'
#' @return
#' @export
#'
#' @examples
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
    
    verbose_message(sprintf("Copying %s to %s \n",getFileName(file_name),toString(tags)))
    
    for( tag in tags ){
      dest_dir <- genObjPath(root,tag)
      if(!dir.exists(dest_dir)){
        dir.create(dest_dir)
      }
      
      dest_path <- file.path(dest_dir , getFileName(file_name))
      
      file.copy(file_path , dest_path)
    }
    
  }
  verbose <<- FALSE
  
}


#' clearDB
#' 
#' removes all folders and files in the folder specified by root but not the 
#' folder for root itself
#'
#' @param root Base folder which is to be cleared
#'
#' @return
#' @export
#'
#' @examples
clearDB <- function (root){
  
  if(rootPath!=""){
    
    list_of_files = list.dirs(file.path(rootPath,rootDir))[-1]
    print(list_of_files)
    
    for (subdir in list_of_files) {
      print(subdir)
      unlink(subdir, recursive = TRUE)
    }
 
    
  } else {
    list_of_files = list.dirs(rootDir)[-1]
    print(list_of_files)
    for (subdir in list_of_files) {
      print(subdir)
      unlink(subdir, recursive = TRUE)
    }
    
  }
  
  
  
}

main <- function()
  
{
  
  configDB(rootDir , "")
  
  storeObjs("testData",rootDir,TRUE)
  
 #clearDB(rootDir)
  
}





main()

