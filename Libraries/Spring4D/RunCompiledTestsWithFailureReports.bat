@echo off
  for /f %%s in ('dir /s /b Spring.Tests.exe') do if exist %%s call :run %%s
  grep -wd error *MSBuildLog.txt
  grep -wd failures *Spring.Tests.Reports.xml
  goto :eof
:run
  echo %1
  pushd %~dp1
  %1
  popd
  goto :eof
