@REM Wrapper for booting ciao_builder from Windows CMD.EXE using MSYS2

@REM Switch to the script directory
@cd %~dp0
@REM Inhibit directory change (cygwin/msys2 option in /etc/profile)
@set CHERE_INVOKING=1

@REM Use MSYSTEM=MINGW32 for MinGW32
@REM Use MSYSTEM=MINGW64 for MinGW64
@REM Use MSYSTEM=MSYS for MSYS2 (POSIX emulation)
@set MSYSTEM=MINGW32
@"c:\msys32\usr\bin\bash.exe" -l -c "./ciao-boot.sh %*"
