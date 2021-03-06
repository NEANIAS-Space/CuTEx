
PRO read_line_ids, fname, wvl, outstr, range=range, shift=shift

;+
; NAME
;
;    READ_LINE_IDS
;
; EXPLANATION
;
;    Reads the file specified by FNAME
;    and outputs a structure containing all transitions close to the input 
;    wavelength WVL. The file contains for each transition an approximate 
;    wavelength (to 0.1 angstrom), an ion identifier of the form, e.g., 
;    ca_10, and the CHIANTI level indices of the transition. READ_LINE_IDS 
;    then reads data from the user's version of CHIANTI to determine the 
;    current CHIANTI wavelength of the transition.
;
; INPUTS
;
;    FNAME  The name of the file containing the wavelengths and level 
;           identifiers.
;
;    WVL    A wavelength in angstroms. READ_LINE_IDS will search for 
;           transitions in FNAME that lie within in +/- 1 
;           angstrom of this wavelength. This limit can be varied with the 
;           optional input RANGE.
;
; OUTPUTS
;
;    OUTSTR A structure containing information about the transitions lying 
;           close to WVL. The tags are:
;            .name  Name of the ion, e.g., 'Ca X'
;            .wvl   The wavelength of the transition (float).
;            .str   String containing ion and wavelength, e.g., 
;                   'Fe XVI 360.76'.
;
; OPTIONAL INPUTS
;
;    RANGE  By default the routine searches for lines within +/- 1 angstrom 
;           of WVL. RANGE allows this to be varied, e.g., RANGE=0.5.
;
;    SHIFT  When checking to see which lines satisfy RANGE, a velocity SHIFT
;           is included. The units are km/s, and +ve corresponds to a redshift
;           and -ve corresponds to blueshift.
;
; CALLS
;
;    V2LAMB, ION2SPECTROSCOPIC, ION2FILENAME, READ_WGFA, LEVEL_STRING
;
; RESTRICTIONS
;
;    The line IDs file must have the particular format '(f7.0,a6,2i4)'.
;
; HISTORY
;
;    Ver.1, 6-Nov-2002, Peter Young
;    Ver.2, 17-Jul-2003, Peter Young
;         added FNAME input and removed reference to $CDS_LINE_IDS
;    Ver.3, 11-Dec-2003, Peter Young
;         changed findfile() call to file_search() as findfile was failing
;         to find a file on unix machine.
;    Ver.4, 12-Oct-2004, Peter Young
;         added extra tags to output structure
;    Ver.5, 8-Dec-2006, Peter Young
;         changed maximum size of OUTSTR to 500 (from 20)
;-

IF n_params() LT 3 THEN BEGIN
  print,'Use: IDL> read_line_ids, fname, wvl, outstr, range=, shift='
  return
ENDIF


IF n_elements(range) EQ 0 THEN range=1.
IF n_elements(shift) EQ 0 THEN shift=0.

result=file_search(fname)
IF result[0] EQ '' THEN BEGIN
  print,'%READ_LINE_IDS: the specified file does not exist. Returning...'
  return
ENDIF

openr,lun,fname,/get_lun

str={wvl: 0., ion: '', l1: 0, l2: 0}
data=replicate(str,300)

i=0
w=0.
ion=''
l1=0
l2=0
WHILE eof(lun) NE 1 DO BEGIN
  readf,lun,format='(f7.0,a6,2i4)',w,ion,l1,l2
  data[i].wvl=w
  data[i].ion=ion
  data[i].l1=l1
  data[i].l2=l2
  i=i+1
ENDWHILE

data=data[0:i-1]

str={name: '', wvl: 0., str: '', gf: 0., aval: 0., trans: ''}
outstr=replicate(str,500)

wvlshift=v2lamb(shift,wvl)

ind=where(abs(wvl-data.wvl-wvlshift) LT range)

IF ind[0] NE -1 THEN BEGIN
  n=n_elements(ind)
  FOR i=0,n-1 DO BEGIN
    l1=data[ind[i]].l1
    l2=data[ind[i]].l2
    ion=trim(data[ind[i]].ion)
    ion2filename,data[ind[i]].ion,file
    ion2spectroscopic,data[ind[i]].ion,iname
    file=strtrim(file,2)+'.wgfa'
    read_wgfa,file,lvl1,lvl2,ww,gf,a_value,ref
    outstr[i].name=strtrim(iname,2)
    outstr[i].wvl=ww[l1-1,l2-1]
    outstr[i].gf=gf[l1-1,l2-1]
    outstr[i].aval=a_value[l1-1,l2-1]
    outstr[i].trans=level_string(ion,l1)+' - '+ $
           level_string(ion,l2)
    IF abs(ww[l1-1,l2-1]*2.-wvl) LT 2.*range THEN addstr='x2' ELSE $
         addstr=''
    outstr[i].str=strtrim(iname,2)+' '+ $
         strtrim(string(format='(f12.2)',ww[l1-1,l2-1]),2)+addstr
  ENDFOR
  outstr=outstr[0:n-1]
 ;
  chck=abs(outstr.wvl-wvl)
  ii=sort(chck)
  outstr=outstr[ii]
ENDIF ELSE outstr=-1


free_lun,lun

END
  
