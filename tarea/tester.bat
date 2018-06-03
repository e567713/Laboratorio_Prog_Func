Compiler.exe ejemplo1
fc ejemplo1.c ejemplo1SAL.c
if ERRORLEVEL 0 echo ejemplo1 OK > test.txt
if ERRORLEVEL 1 echo ejemplo1 FAIL > test.txt
Compiler.exe ejemplo2
fc ejemplo2.c ejemplo2SAL.c
if ERRORLEVEL 0 echo ejemplo2 OK >> test.txt
if ERRORLEVEL 1 echo ejemplo2 FAIL >> test.txt
Compiler.exe ejemplo3
fc ejemplo3.c ejemplo3SAL.c
if ERRORLEVEL 0 echo ejemplo3 OK >> test.txt
if ERRORLEVEL 1 echo ejemplo3 FAIL >> test.txt
Compiler.exe ejemplo4
fc ejemplo4.c ejemplo4SAL.c
if ERRORLEVEL 0 echo ejemplo4 OK >> test.txt
if ERRORLEVEL 1 echo ejemplo4 FAIL >> test.txt
Compiler.exe ejemplo5
fc ejemplo5.c ejemplo5SAL.c
if ERRORLEVEL 0 echo ejemplo5 OK >> test.txt
if ERRORLEVEL 1 echo ejemplo5 FAIL >> test.txt
Compiler.exe ejemplo6
fc ejemplo6.err ejemplo6SAL.err
if ERRORLEVEL 0 echo ejemplo6 OK >> test.txt
if ERRORLEVEL 1 echo ejemplo6 FAIL >> test.txt
Compiler.exe ejemplo7
fc ejemplo7.err ejemplo7SAL.err
if ERRORLEVEL 0 echo ejemplo7 OK >> test.txt
if ERRORLEVEL 1 echo ejemplo7 FAIL >> test.txt
Compiler.exe ejemplo8
fc ejemplo8.err ejemplo8SAL.err
if ERRORLEVEL 0 echo ejemplo8 OK >> test.txt
if ERRORLEVEL 1 echo ejemplo8 FAIL >> test.txt
Compiler.exe ejemplo9
fc ejemplo9.err ejemplo9SAL.err
if ERRORLEVEL 0 echo ejemplo9 OK >> test.txt
if ERRORLEVEL 1 echo ejemplo9 FAIL >> test.txt
Compiler.exe test1
fc test1.c test1SAL.c
if ERRORLEVEL 0 echo test1    OK >> test.txt
if ERRORLEVEL 1 echo test1    FAIL >> test.txt
Compiler.exe test1err
fc test1err.err test1errSAL.err
if ERRORLEVEL 0 echo test1err OK >> test.txt
if ERRORLEVEL 1 echo test1err FAIL >> test.txt
Compiler.exe test2
fc test2.c test2SAL.c
if ERRORLEVEL 0 echo test2    OK >> test.txt
if ERRORLEVEL 1 echo test2    FAIL >> test.txt
Compiler.exe test2err
fc test2err.err test2errSAL.err
if ERRORLEVEL 0 echo test2err OK >> test.txt
if ERRORLEVEL 1 echo test2err FAIL >> test.txt
Compiler.exe test3
fc test3.c test3SAL.c
if ERRORLEVEL 0 echo test3    OK >> test.txt
if ERRORLEVEL 1 echo test3    FAIL >> test.txt
Compiler.exe test3err
fc test3err.err test3errSAL.err
if ERRORLEVEL 0 echo test3err OK >> test.txt
if ERRORLEVEL 1 echo test3err FAIL >> test.txt
Compiler.exe test4
fc test4.c test4SAL.c
if ERRORLEVEL 0 echo test4    OK >> test.txt
if ERRORLEVEL 1 echo test4    FAIL >> test.txt
Compiler.exe test4err
fc test4err.err test4errSAL.err
if ERRORLEVEL 0 echo test4err OK >> test.txt
if ERRORLEVEL 1 echo test4err FAIL >> test.txt
Compiler.exe test5
fc test5.c test5SAL.c
if ERRORLEVEL 0 echo test5    OK >> test.txt
if ERRORLEVEL 1 echo test5    FAIL >> test.txt
Compiler.exe test5err
fc test5err.err test5errSAL.err
if ERRORLEVEL 0 echo test5err OK >> test.txt
if ERRORLEVEL 1 echo test5err FAIL >> test.txt
cls
