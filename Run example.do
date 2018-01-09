clear all

////////////////////////////////////////////////////////////////////////////////
//
// This is a data-example briefly illustrating how to run pscore2
// * The function atts is borrowd from Becker and Ichino 
// * type findit atts
////////////////////////////////////////////////////////////////////////////////

use DWNSW.dta
glo X "age educ black hisp marr nodegr re75 re74 u74 u75"

// Basic pscore2 algorithm

cap drop id pscore
pscore2 treat $X, blockid(id) pscore(pscore) ksm det
atts re78 treat, pscore(pscore) blockid(id) 

cap drop id pscore
pscore2 treat $X, blockid(id) pscore(pscore) ksm level(0.3) rescale by(0.95) det
atts re78 treat, pscore(pscore) blockid(id) 

cap drop id pscore
pscore2 treat $X, blockid(id) pscore(pscore) tenf det
atts re78 treat, pscore(pscore) blockid(id) 


help(pscore2)
