journal, 'RUNDIR/journal_Run.log'

dirSoftware='DUMMYDIR/Code_CuTEx/CuTEx_100/'
dirData='DUMMYDIR/'
dirRun=dirData+'/RUNDIR/'

optionFile = 'parameters_CuTEx.txt'

!PATH=EXPAND_PATH('+'+dirSoftware)+':'+dirData+'/libs/'

print, 'Compiling Routines for CuTEx'
@DUMMYDIR/Code_CuTEx/CuTEx_100/phot_package.lis

out = make_options(dirRun+optionFile)
detFile = "sources_"+strtrim(string(out.thr,format='(F9.5)'),2)+"_"+strmid(out.image,1, strpos(out.image, '.fits')-1)+".dat"
print, 'DET FILE ', detFile

strDet_ = "outCutex = detection('"+dirRun+"',"+out.image+","+strtrim(string(out.thr,format='(F9.5)'),2)+",'"+dirRun+"'"+out.parstrdet+")"
print, '------------------------------------------------------'
print, 'Running DETECTION algorithm with this call'
print, strDet_
print, ' '
void = execute(strDet_)
print, '----- DONE Detection step -----'

strExt_  = "extract_photo,'"+dirRun+"',"+out.image+",'"+dirRun+"','"+detFile+"'"+out.parstrextr
print, '------------------------------------------------------'
print, 'Running EXTRACTION algorithm with this call'
print, strExt_
print, ' '
void = execute(strExt_)

print, '--- DONE Extraction step ----'

print, '------------------------------------------------------'
print, 'Listing Data to be saved
openw, unit, dirRun+'list_mainOutputs.txt', /GET_LUN

listmainOutputs = [file_search(dirRun+'*photall.dat'), file_search(dirRun+'*allmemb*.reg')]
for k=0, n_elements(listmainOutputs)-1 do listmainOutputs[k] = strmid(listmainOutputs[k], strpos(listmainOutputs[k], dirRun)+strlen(dirRun))
for k=0, n_elements(listmainOutputs)-1 do printf, unit, listmainOutputs[k]

free_lun,unit

openw, unit, dirRun+'list_auxiliaryOutputs.txt', /GET_LUN

listancOutputs = [file_search(dirRun+'*photall_err.dat'), file_search(dirRun+'*parameters.dat'), file_search(dirRun+'*backpar.dat'), file_search(dirRun+'sources_'+strtrim((out.thr),2)+'*.dat'), file_search(dirRun+'*_memb_*.reg')]
for k=0, n_elements(listancOutputs)-1 do listancOutputs[k] = strmid(listancOutputs[k], strpos(listancOutputs[k], dirRun)+strlen(dirRun))
for k=0, n_elements(listancOutputs)-1 do printf, unit, listancOutputs[k]

free_lun,unit
journal

exit
