SET OLDCURDRIVE=%CD:~,2%
SET OLDCURDIR=%CD%

SET COMPILER=%BUILDDIR%\pp\bin\i386-win32\ppc386.exe

%BUILDDRIVE%
cd %BUILDDIR%
%MAKEEXE% clean PP=%COMPILER% >> %LOGFILE%
%MAKEEXE% lcl OPT="-gl -Xs -XX -Ur" PP=%COMPILER% >> %LOGFILE%
%MAKEEXE% all OPT="-gl -Xs -XX" PP=%COMPILER% >> %LOGFILE%
%FPCBINDIR%\strip.exe lazarus.exe

%FPCBINDIR%\strip.exe startlazarus.exe

%OLDCURDRIVE%
cd %OLDCURDIR%