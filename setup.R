#Connecting R To The GitHub Repository

#Author: Laura Briggs

#Date: 30 June 2024

# I. Initial Set-up: Install and Load Required Packages with Basic Feedback

#this script installs and loads required packages with basic feedback
#separating the installation check from the loading step ensures a more robust process

#list of required packages
pkg2 <- c("usethis", "config", "here")

#create a function to install and load packages with feedback
#installing all dependencies minimizes potential issues with missing packages
#adding messages provides feedback on the installation and loading status of each package

install_and_load <- function(package) {
  tryCatch({
    if (!package %in% rownames(installed.packages())) { #check if package is installed
      install.packages(package, dependencies = TRUE) #install the package with all dependencies
      Sys.sleep(2) #ensure the installation process completes properly
      .libPaths(.libPaths()) #reload the library paths
      if (!require(package, character.only = TRUE)) { #try to load the package again
        return(paste("failed to install or load package:", package)) #return message if loading fails
      } else {
        return(paste(package, "was installed and loaded successfully.")) #return message if successful
      }
    } else {
      if (!require(package, character.only = TRUE)) { #try to load the package
        return(paste("failed to load package:", package)) #return message if loading fails
      } else {
        return(paste(package, "was already installed and loaded.")) #return message if already installed and loaded
      }
    }
  }, error = function(e) {
    return(paste("error installing or loading package:", package, "-", e$message)) #extract and return the error message
  })
}

#install and load packages
install_results <- lapply(pkg2, install_and_load)

#print installation and loading results with a title
cat("summary:\n", unlist(install_results), sep = "\n")

# II. Subsequent Session(s): Load Required Packages

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

