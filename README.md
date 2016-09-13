# mlcodage
mlcodage is an R package for medical text annotation, based on machine learning. It uses a Web application based on OpenCPU.

The purpose of this package is to help automating the task of code assignment to discharge summaries using machine learning.  

Use locally
-----------

To run the app in your local R session:

  library(mlcodage)
	mlcodage::launchListener(port=80) # can change port nuumber

then, to start coding, go to url: %ip-address%:%port%/ocpu/library/mlcodage/www/index.html

