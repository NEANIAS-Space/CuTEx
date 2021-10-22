

;c_statistic.pro

function c_statistic,f_obs,f_mod,            $
                     F_MOD_MIN=f_mod_min,    $
                     CHISQ=chisq,            $
                     EXPECTATION=expectation
;+
;
; PURPOSE:
;      To provide a goodness-of-fit statistic valid for very low countrates
;      e.g., counts/bin < 10  (Webster Cash, AP.J., 1979)
;
; INPUTS:
;      f_obs = array of integer counts
;      f_mod = same size array of positive model counts (possibly
;      non-integer)
;
; OPTIONAL INPUTS:
;      If f_mod is not positive everywhere, one can set a minimum
;      allowed value using the f_mod_min keyword.
;      IF CHISQ is set, the chi-squared statistic for Poisson
;      distributed f_obs will be returned.
;
;      If EXPECTATION is set to an existing variable, the expected
;      C-statistic will be returned in it.
;
; NOTES:
;      If f_mod and f_obs form the best possible match, cash_statistic
;      will have a local minimum (which may be <1 or > 1).  This is
;      not true for chi-squared when counts/bin < 10.
;      If counts/bin >> 10, cash_statistic = Chisquared
;
; HISTORY:
;      EJS April 19, 2000
;      W. Cash Ap.J., 1979
;      EJS May 1, 2000 -- FLOATED f_mod to prevent f_obs/f_mod
;                         becoming zero if f_obs and f_mod both INTEGER.
;      EJS Jun 6, 2000 -- Added option to get expectation of C-statistic
;      Kim, Mar 4, 2005 -- Extracted c_expected file into a separate file
;-

f_mod=float(f_mod)
if keyword_set(f_mod_min) then   f_mod=f_mod>(f_mod_min)

if (min(f_mod) LE 0) then message,'f_mod is not positive everywhere!'

 NN=n_elements(f_mod)

if keyword_set(chisq) then begin
 z_chi2=(1./NN)*(f_obs-f_mod)^2/f_mod
 statistic=total(z_chi2)
endif else begin
 z_cash=(2./NN)*f_mod
 w=where(f_obs NE 0)

 z_cash(w)=(2./NN)*(f_obs(w)*alog(f_obs(w)/f_mod(w))+f_mod(w)-f_obs(w))
 statistic=total(z_cash)
endelse

if n_elements(expectation) GT 0 then expectation=c_expected(f_mod)

return,statistic

end

