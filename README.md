OpenCPU-based coding app: mlcodage
====================
  
The purpose of this OpenCPU app is to help automating the task of code assignment to discharge summaries using machine learning. 

Functions for learning and prediction are available. Four models built on French surgery reports to predict procedure codes are given for the example.

Use locally
-----------

To run the app in your local R session:

    library(mlcodage)
	mlcodage::launchListener(port=80)

then, to start coding, go to url: %ip-address%:%port%/ocpu/library/mlcodage/www/index.html

Author(s)

ERIC Lab (University Lyon 1 & 2), 
ECSTRA Team INSERM 1153 (Paris Diderot University), 
Departement of medical informatics AP-HP
