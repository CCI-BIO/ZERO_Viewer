# ZERO Viewer

## Requirements 

### Standalone version

- R 4.0.x 
- Compatible web browser (eg. Chrome, Firefox, Edge)

### Additional requirements for running from source

- Rstudio
- The following R packages (available through CRAN):
  - shinythemes
  - shinyjs
  - DT
  - Rtsne
  - ggplot2
  - plotly
  - tidyr
  - RColorBrewer
  
## Installation and running the application

### Standalone version (windows only)

A compiled standalone executable is available in the current release section of the git. The executable has been compiled with Inno Setup (v5.6.1(a)) via the RInno package. R version 4.0.2 is also included in the installation. 

Run the executable and follow the prompts. The installation directory will be located in your user documents folder. Once installed, the first run of the application may take several minutes as the R packages are installed. A browser window will automatically open with the application GUI when ready. Succesive runs of the application will have a much quicker startup time as the R packages will already be installed. 

### Source code

The source code is available to download through the current release or the git repository. To run the application from the source you will require a qorking installation of R (4.0.x) and Rstudio as well as the required R packages (see additional requirements for running from source). Open the app.R file in R studio and click run the Shiny application. Once the main window pops up click run in browser window. The application must be run in a browser window as the plot download functionality is only availble via browsers.

## Troubleshooting and common issues

**When the application opens in the browser it is greyed out**

This event indicates that there has been a crash in the shiny application script. If you come across this issue please navigate to <user>/Documents/ZERO Viewer/log/ and create an issue on the git with the contents of the *error.log* file.

**The application will not open at all when I try to click on the launch icon**

Chances are there was a crash in the previous instance of the application running. If so, please check your error.log file and post the issue on the git page. Sometimes there may be an unexpected closure of the application and a instance of R for window front end will be still running in the background. Please open task manager (Ctrl + Shift + Esc) and find any instance of R for windows front end and end those tasks. Once that is done, try and re-open the application. 

**An error saying a packages version is not compatiable shows up when launching the application**

If you see this error, there may have been an signficant update in the packages. Please submit an issue on the git page and we will update the application as soon as possible. 
