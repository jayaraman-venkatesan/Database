# CS5200.BuildDocDB.Venkatesan

This R project and script are designed to set up a document database and perform various operations on the documents based on their tags.


## Project Structure
- Project Name: CS5200.BuildDocDB.Venkatesan
- R Script: ObjDB-Venkatesan.R

## Getting Started
1. Launch R Studio.
2. Open the CS5200.BuildDocDB.Venkatesan R Project.
3. Open the R script ObjDB-Venkatesan.R.

## Global Variable
- rootDir: A global variable that represents the root directory of the document database. It is initialized with the value "docDB".
- rootPath : Directory where rootDir should be present

## Functions

1. <b>configDB(root, path):</b> This function sets up the folder structure for the database. If the path argument is empty, it creates the "docDB" folder in the project folder; otherwise, it creates the "docDB" folder under the provided path.

2. getTags(fileName): This function returns a vector of tags extracted from the fileName argument. It handles file names with tags in different positions and formats, such as "CampusAtNight.jpg #Northeastern #ISEC" or "CampusAtNight #Northeastern #ISEC.jpg". The function correctly handles file extensions on Windows.

3. <b>getFileName(fileName):</b> This function returns the file name portion from the fileName argument. It removes any tags or extensions, returning just the file name.


4. <b>genObjPath(root, tag):</b> This function returns the correctly generated path to a tag folder by concatenating the root and tag arguments. The '#' character is stripped from the tag.

5. <b>storeObjs(folder, root, verbose):</b> This function copies all files from the specified folder argument to their respective tag folders under the root directory. It creates tag folders as needed. If the verbose argument is true, it prints a message for every file that is copied, displaying the file name and the tags separated by commas.


## Running the program

1. Ensure that the necessary functions and global variables are defined.
2. Call the main() function.
3. Modify the code inside the main() function to perform testing or any other desired operations.
