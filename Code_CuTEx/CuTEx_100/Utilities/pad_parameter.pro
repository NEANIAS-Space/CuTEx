function pad_parameter,par,pad_char,length
; this function returns the input parameter converted to string and padded with the required number of blanks to get it to the required LENGTH. it is used to pack parameters to be written in IPAAC table format

padded_par=strcompress(string(par),/remove_all)
n=strlen(padded_par)
for i=0,length-n-1 do padded_par=padded_par+pad_char


return,padded_par
end