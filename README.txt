Step 1) Define the parameter file from template parameters_CuTEx_template.txt 
Step 2) Provide the image FITS file in the directory where the software will run
Step 3) launch the shell script launch_script_CuTEx.sh DIRNAME, where DIRNAME is the name of the directory where the FITS is provided together to the parameter file
Step 4) Wait run....
Step 5) Get results in DIRNAME as tarred file: resultsMain.tar and resultsAux.tar


Example Provided:

FITS file		:  	'G351.07-0.55_PSW.fits'
parameters file :	'parameters_CuTEx.txt'

copy these two files in directory 'out_cutex' (default directory)

on terminal type:

./launch_script_CuTEx.sh out_cutex

Wait... on successful run the output will be in out_cutex.