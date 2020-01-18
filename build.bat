set rsvars="d:\Program Files (x86)\Embarcadero\Studio\19.0\bin\rsvars.bat"
call %rsvars%                     
msbuild Concepts.dproj /t:make /p:config=Debug /p:platform=Win32
msbuild Concepts.dproj /t:make /p:config=Release /p:platform=Win32
msbuild Concepts.dproj /t:make /p:config=Debug /p:platform=Win64
msbuild Concepts.dproj /t:make /p:config=Release /p:platform=Win64
pause
