webpage and documentation at https://cbedu2.github.io/CS424/Projects/Project2/

# Every Breath You Take
# CS424 Project2

Project Link: http://shiny.evl.uic.edu:3838/g5/g5_project2/

Git Link: https://github.com/wilt00/424_project2

The work described here is a result of the collaboration of the following:

    Dylan Vo - dvo7 - https://dylanvo21.github.io/CS424
    Will Toher - wtoher2 - https://willtoher.com/424
    Will Bedu - cbedu2 - https://cbedu2.github.io

# Status of our project:
* 03/18/2019 	Submitted Final Solution
* 03/04/2019 	Submitted Alpha Version
* 02/20/2019 	Group met to discuss best viz choices and data mutation
* 02/15/2019 	Group approved
* 02/14/2019 	Awaiting group approval

# Steps To Reproduce

1. Clone/Download git repo
2. Download/Install the following R libraries

        leaflet
        shinydashboard
        plotly
        ggplot2
        dplyr
        purrr
        leaflet
        magrittr
        feather
        reshape2
        ggrepel
        DT
        reshape
        ggmap
        rgdal
2. Download the following file

        source: https://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_050_00_20m.json
        This is used in map charts
3. run preprocess.R

        This downloads any missing files that you need and converts them into a feather file
        source: aws.epa.goc
        ex: hourly Ozone, SO2, CO, NO2, PM2.5 FRM/FEM, PM10 mass files, as well as the hourly_WIND files, and the hourly_TEMP files for 2018
4. Run mockServer.R

        This runs a local instance of a shiny server
5. (Optional) move files to Shiny Server

        copy all feather files, .R files, and JSON to your shiny server home
