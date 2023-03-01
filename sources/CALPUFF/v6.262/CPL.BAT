REM Compiling and linking with LF95

del params.puf
copy paramss.puf params.puf
lf95 calpuff.for -o0 -trap doi -out calpuffs.exe >cpl_s

del params.puf
copy paramsm.puf params.puf
lf95 calpuff.for -o0 -trap doi -out calpuffm.exe >cpl_m

del params.puf
copy paramsl.puf params.puf
lf95 calpuff.for -o0 -trap doi -out calpuffl.exe >cpl_l
