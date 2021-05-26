@echo off
set project=Concepts
where rsvars.bat /q
if %errorlevel% neq 0 (
  echo Add the Embarcadero Studio bin folder to your system path.
  echo e.g. "c:\Program Files (x86)\Embarcadero\Studio\19.0\bin"
) else (
  call rsvars
  if exist .\Bin\Win32\%project%.exe del .\Bin\Win32\%project%.*
  msbuild %project%.dproj /t:make /p:config=Debug /p:platform=Win32
  ::msbuild %project%.dproj /t:make /p:config=Release /p:platform=Win32
  if exist .\Bin\Win64\%project%.exe del .\Bin\Win64\%project%.*
  msbuild %project%.dproj /t:make /p:config=Debug /p:platform=Win64
  ::msbuild %project%.dproj /t:make /p:config=Release /p:platform=Win64
  where upx.exe /q
  if %errorlevel% neq 0 (
    echo upx not found
  ) else (
    upx .\Bin\Win32\%project%.exe
  )
)
pause
