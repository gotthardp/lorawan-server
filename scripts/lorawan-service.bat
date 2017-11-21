@echo off
setlocal EnableDelayedExpansion

for /D %%A in ("%PROGRAMFILES%\erl*" "%PROGRAMFILES(x86)%\erl*") do (
    for /D %%B in ("%%A\erts*") do (
        for /D %%C in ("%%B\bin\erlsrv.exe") do (
            set "ERLSRV=%%C"
        )
    )
)

if [%1] == [] goto MISSING
if [%1] == [add] goto ADD

%ERLSRV% %1 "LoRaWAN Server"
goto END

:ADD
set "SCRIPT_DIR=%~dp0"
for %%A in ("%SCRIPT_DIR%\..") do (
    set "ROOT_DIR=%%~fA"
)

for /D %%A in ("%ROOT_DIR%\lib\*") do (
    set FILES=!FILES! "%%A\ebin"
)

%ERLSRV% add "LoRaWAN Server" -w "%ROOT_DIR%" -sname lorawan -ar "+Bd -pa !FILES! -s lorawan_app -config releases\{{release_version}}\sys.config" -c "Handles packets received by LoRaWAN gateways"
goto END

:MISSING
echo "Use %~nx0 {add|remove|start|stop|disable|enable|list}"

:END
