#Connecting R To The GitHub Repository

#Author: Laura Briggs

#Date: 29 June 2024

#I. Initial Set-up: Install and Load Required Packages with Basic Feedback

#list of required packages
pkg2 <- c("usethis", "config", "here")

#function to install missing packages with all dependencies
#dependencies = TRUE ensures that all required dependencies are installed
#installing all dependencies minimizes potential issues with missing packages
#adding messages provides feedback on the installation and loading status of each package

install_if_missing <- function(package) {
  tryCatch({
    if (!package %in% rownames(installed.packages())) { #check if package is installed
      install.packages(package, dependencies = TRUE) #install the package with all dependencies
      Sys.sleep(2) #add a brief delay to ensure the installation process completes properly before attempting to load the package
      .libPaths(.libPaths()) #reload the library paths to ensure that the newly installed package is recognized by the current R session
      if (!require(package, character.only = TRUE)) { #try to load the package
        return(paste("Failed to install or load package:", package)) #return message if loading fails
      } else {
        return(paste(package, "was installed and loaded successfully.")) #return message if installation and loading succeed
      }
    } else {
      library(package, character.only = TRUE) #load the package if it is already installed
      return(paste(package, "was already installed.")) #return message if the package was already installed
    }
  }, error = function(e) {
    #this function is called if an error occurs during the try block
    #e is the error object that contains details about the error
    return(paste("Error installing or loading package:", package, "-", e$message)) #extracts and returns the error message
  })
}

#install and load packages
install_results <- lapply(pkg2, install_if_missing) #apply the install_if_missing function to each package in pkg2

#print installation and loading results with a title
cat("Summary:\n", unlist(install_results), sep = "\n")

#II. Subsequent Sessions: Load Required Packages

#list of required packages that were previously installed
#pkg2 <- c("usethis", "config", "here")

#load packages that were previously installed
#lapply(pkg2, require, character.only = TRUE)

#III. Set-up The Git Environment

#personal access token (PAT} to connect R with gitbub

#create a new PAT token to connect R with GitHub
usethis::create_github_token()

#create a text file and save the PAT in a config.yml file
#add this file to .gitignore
#ignore config.yml to prevent uploading sensitive information to github

#read the configuration settings
config <- config::get()

#set the GITHUB_PAT environment variable
Sys.setenv(GITHUB_PAT = config$GITHUB_PAT)

#check the status of the connection of R with the github repository
usethis::git_sitrep()

#set up git configuration with your name and email for committing changes
#be sure to use your own name and email address
#usethis::use_git_config(user.name = "your name", user.email = "your email")

