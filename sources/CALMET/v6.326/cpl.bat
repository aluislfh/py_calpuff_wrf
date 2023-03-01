REM Compiling and linking with LF95

del params.met
copy paramss.met params.met
lf95 calmet.for -o0 -trap doi -out calmets.exe >cpl_s

del params.met
copy paramsm.met params.met
lf95 calmet.for -o0 -trap doi -out calmetm.exe >cpl_m

del params.met
copy paramsl.met params.met
lf95 calmet.for -o0 -trap doi -out calmetl.exe >cpl_l
