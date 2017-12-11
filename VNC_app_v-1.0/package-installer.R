# ------------------------------------------------
# This script installs all missing packages
# that are required to succesfully launch
# the VNC analysis application.
# Press CTRL + A to select the complete script, 
# followed by CTRL + ENTER to run it.
#
# Once the installation is finished (as indicated
# in the console below), you can close this script. 
# ------------------------------------------------

need <- c("shiny","reshape2","ggplot2","markdown") # needed packages
ins <- installed.packages()[,1] # currently installed packages

(Get <- need[which(is.na(match(need,ins)))]) # check if the needed packages are installed
if(length(Get)>0){install.packages(Get)} # install the needed packages if they are not-installed

# eval(parse(text=paste("library(",need,")"))) #load the needed packages

paste("Installation completed!")