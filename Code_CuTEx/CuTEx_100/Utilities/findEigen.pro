;
; This is a function designed to diagonalize 2x2 real Matrices for the usage of CuTEx and VLFF
;
; Assuming the matrix to be 
; Matrix [[A1, A2], [B1, B2]]
;
;  [A1, A2]
;  [B1, B2]
;
; To diagonalize the matrix we have to solve the following equation  Matrix - l * 1 = 0
; that translate into the Characteristic equation 
;
;  l^2 - (A1 + B2)*l +A1*B2 - A2*B2
; 
; whose solutions are the eigenvalues l1 and l2



function findEigen, Matrix

ON_ERROR, 2   ; Return to caller

check = size(Matrix)
if check[1] ne 2 then MESSAGE, 'Error Wrong Input - Function defined to work only on 2x2 matrices '

; I rewrite the Characteristic Equation as a*l^2 + b*l + c = 0 with solutions
; l1 = -b/2. + SQRT( b^2 - a*c ) and l2 = -b/2. - SQRT( b^2 - a*c )
;
; where the b and c values are the following ( Inverted Minus Sign since b is defined as - ( -(A1 + B2) ) )
 
b =  ( Matrix[0,0] + Matrix[1,1] ) 
c =  ( Matrix[0,0]*Matrix[1,1] - Matrix[0,1]*Matrix[1,0])

; EigenValues solutions

l1 = (b - SQRT( b^2. - 4.*c ))/2.
l2 = (b + SQRT( b^2. - 4.*c ))/2.

eigenValues = [l1, l2]

; EigenVectors are obtained by replacing the values for l1 and l2 in the equation  Matrix - l * 1 = 0
; 

Iarr = [[l1,0], [0,l2]]

EigenVectors = Matrix - Iarr 

return, eigenValues

end
