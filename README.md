## Behavior Change Among HIV-Negative Men Who Have Sex with Men Not Using PrEP in the United States
### Replication instructions

**Full citation**: Goodreau SM, Barry MP, Hamilton DT, Williams AM, Wang L, Sanchez TH, Katz DA, Delaney KP, Behavior Change Among HIV-Negative Men Who Have Sex with Men Not Using PrEP in the United States. *AIDS and Behavior* 28, 1766–1780 (2024).

**Step 1. Obtain data.** This manuscript uses data from the *American Men's Internet Survey*, years 2014-2019. These data are the property of Emory University, who is solely authorized to share them. The project website is [here](https://emoryamis.org), and the data request page is [here](https://emoryamis.org/data-requests/). These web addresses are accurate as of the time of the publication of the manuscript. Note that there may be a fee associated with the data request, and it may vary based on the number of variables requested.  Variables that were part of the data request for this paper can be found at the start of the project file *01_Initial_submission_data_prep_and_exploratory_analyses.R*. Please note that because the data are managed by another party, we cannot place any direct guarantees on the data extraction process and thus ensure that the subsequent analyses will be exactly replicable.

**Step 2. Situate data.** Create a folder in your project directory called ```data```, and place the .csv file received from AMIS staff there.

**Step 3. Add datafile name.** Edit line 27 of  *01_Initial_submission_data_prep_and_exploratory_analyses.R* to reflect the name of your .csv datafile.

**Step 4. Download R packages.** The R packages required by the project are lsited in the file *00_master_script.R*. If you do not have any of the required packages in your library, download them from CRAN with the ```install.packages("pkgname")``` command.

**Step 5. Source the master script** with the command ```source("00_master_script.R")```. This will clear your environment, load the required packages, and source each of the other project scripts in turn.

Questions? please email Steven Goodreau at goodreau@uw.edu.



