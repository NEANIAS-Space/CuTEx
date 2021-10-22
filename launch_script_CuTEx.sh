#/bin/bash
if	[ -n "$1" ];
	then 
	dirRun=''$(echo $1)''
	echo "Running on Directory "$dirRun
else
	dirRun='out_cutex'
	echo "Running on Directory "$dirRun
fi	

currDir=$(pwd)
cat ./Code_CuTEx/CuTEx_100/phot_package_compile.lis | sed "s:DUMMYDIR:$currDir:"  > ./Code_CuTEx/CuTEx_100/phot_package.lis
cat launch_script_template_GDL_CuTEx.pro | sed "s:DUMMYDIR:$currDir:" | sed "s:RUNDIR:$dirRun:" > launch_script_GDL_CuTEx.pro

/usr/local/bin/gdl launch_script_GDL_CuTEx.pro 

pushd $dirRun
mainFiles=$(cat list_mainOutputs.txt)
jj=''
for ii in ${mainFiles[*]}; do jj=$jj' '$ii;done
echo 'tar -cvf resultsMain.tar '$jj
tar -cvf resultsMain.tar $jj
echo 'Storing Main Outputs : '$jj

auxFiles=$(cat list_auxiliaryOutputs.txt)
jj=''
for ii in ${auxFiles[*]}; do jj=$jj' '$ii;done
echo 'tar -cvf resultsAux.tar '$jj
tar -cvf resultsAux.tar $jj
echo 'Storing Auxiliary Outputs :'$jj

echo 'Data are store and available at '$dirRun
echo ' 			- Main Outputs: resultsMain.tar'
echo ' 			- Auxiliary Outputs resultsAux.tar'

popd


