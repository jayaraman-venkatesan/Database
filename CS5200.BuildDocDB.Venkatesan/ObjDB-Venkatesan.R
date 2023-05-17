# R Script Title : Document database
#
# Description: This script performs basic operation for a document database
#
# Author: Jayaraman Venkatesan
# Date: 2023-05-17


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
    if(!dir.exists(path)){
      stop("[ERROR]Given path is not present")
    }
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
  
  print("a")
  print(fileName)
  print(file_ext)
  print("b")
  
  return(paste0(".",file_ext))
  
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
getFileName<- function(fileName){
  fileName <- trimws(fileName)
  
  # Remove hashtags and their subsequent content
  fileName <- gsub(" #.*?(?=\\.[^.]*$)| #.*$", "", fileName, perl = TRUE)
  
  # Remove leading/trailing spaces resulting from the removal of hashtags
  fileName <- trimws(fileName)
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
 processed_filename = tools::file_path_sans_ext(basename(fileName))
 
 tags <- unlist(strsplit(processed_filename, " "))[-1]
 
 return_v <- unlist(lapply(tags, trimws))
  

 
 return(unique(return_v))

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

#' testConfigDB
#' 
#' Performs manual unit testing on configDB function
#'
testConfigDB <- function(){
  
  print("-- Testing configDB()")
  # valid scenario 1 - path empty
  configDB(rootDir,"")
  if(!dir.exists(rootDir)){
    print("[FAILED] test case 1/3")
  } else {
    print("[PASSED] test case 1/3")
  }
  
  
  # valid scenario 2 - path present
  curr_dir = getwd();
  input = file.path(curr_dir)
  
  result <- tryCatch(
    {
      configDB(rootDir, input)
      print("[PASSED] test case 2/3")
    }, 
    error = function(e) {
      # Assert that the error message matches our expectation
      print("[FAILED] test case 2/3")
    }
  )
  
  # invalid scenario - path not present
  curr_dir = getwd();
  input = file.path(curr_dir,"test-case1")

  
  result <- tryCatch(
    {
      configDB(rootDir, input)
      print("[FAILED] test case 3/3")
    }, 
    error = function(e) {
      # Assert that the error message matches our expectation
      print("[PASSES] test case 3/3")
    }
  )
  
}


#' testGetFileName
#' 
#' Performs manual unit testing for getFileName function
#'
testGetFileName <- function (){
  
  print("-- Testing getFileName()")
  
  # Test case 1:hashtags at the end
  result <- getFileName("CampusAtNight.jpg #Northeastern #ISEC")
  # Expected: "CampusAtNight.jpg"
  stopifnot(result == "CampusAtNight.jpg")
  
  print("[PASSED] test case 1/4")
  
  
  # Test case 2: hashtags in the middle
  result <- getFileName("CampusAtNight #Northeastern #ISEC.jpg")
  # Expected: "CampusAtNight.jpg"
 stopifnot(result == "CampusAtNight.jpg")
 
 print("[PASSED] test case 2/4")
 
  
  # Test case 3: No hashtags
  result <- getFileName("CampusAtNight.jpg")
  # Expected: "CampusAtNight.jpg"
  stopifnot(result == "CampusAtNight.jpg")
  
  print("[PASSED] test case 3/4")
  
  
  # Test case 4: File name with leading/trailing spaces and multiple hashtags
  result <- getFileName("  Image 1.jpg  #Nature #Scenery  ")
  # Expected: "Image 1.jpg"
  stopifnot(result == "Image 1.jpg")
  
  print("[PASSED] test case 4/4")
  
}



#' testGetTags
#' 
#' Performs manual unit testing for getTags function
testGetTags <- function (){
  
  print("-- Testing getTags()")
  # tests for extensions at the middle
  # 2 tags
  result <- getTags("CampusAtNight.jpg #Northeastern #ISEC")
  stopifnot(toString(result) == "#Northeastern, #ISEC")
  
  print("[PASSED] test case 1/8")
  
  # 1 tag
  result <- getTags("CampusAtNight.jpg #ISEC")
  stopifnot(toString(result) == "#ISEC")
  
  print("[PASSED] test case 2/8")
  
  # no tag
  result <- getTags("CampusAtNight.jpg")
  stopifnot(toString(result) == "")
  
  print("[PASSED] test case 3/8")
  
  #duplicate tags
  result <- getTags("CampusAtNight.jpg #Northeastern #Northeastern")
  stopifnot(toString(result) == "#Northeastern")
  
  print("[PASSED] test case 4/8")
  
  # tests for extentions at the end
  # 2 tags
  result <- getTags("CampusAtNight #Northeastern #ISEC.jpg")
  stopifnot(toString(result) == "#Northeastern, #ISEC")
  
  print("[PASSED] test case 5/8")
  
  # 1 tag
  result <- getTags("CampusAtNight #ISEC.jpg")
  stopifnot(toString(result) == "#ISEC")
  
  print("[PASSED] test case 6/8")
  
  # no tag
  result <- getTags("CampusAtNight.jpg")
  stopifnot(toString(result) == "")
  
  print("[PASSED] test case 7/8")
  
  #duplicate tags
  result <- getTags("CampusAtNight #Northeastern #Northeastern.jpg")
  stopifnot(toString(result) == "#Northeastern")
  
  print("[PASSED] test case 8/8")
  
}



#' testStoreObjs
#' 
#' Performs manual unit testing for storeObjs function
testStoreObjs <- function (){
  
  
  
}


#' unittest
#' 
#' Driver function for unit testing all the components of document database
unittest(){
  

  # unit test configDB ------------------------------------------------------
  testConfigDB()

  # unit test getFilename ---------------------------------------------------
  testGetFileName()
  
  # unit test getTags -------------------------------------------------------
  testGetTags()
  
  # unit test storeObjs -----------------------------------------------------
  testStoreObjs()
  
}

main <- function() {
  
  unittest()
  
  #configDB(rootDir , "")
  
  #storeObjs("testData",rootDir,TRUE)
  
  #clearDB(rootDir)
  
}

main()

