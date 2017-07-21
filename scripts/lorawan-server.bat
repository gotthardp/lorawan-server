@echo off
setlocal EnableDelayedExpansion

for /D %%A in ("%PROGRAMFILES%\erl*" "%PROGRAMFILES(x86)%\erl*") do (
    for /D %%B in ("%%A\erts*") do (
        for /D %%C in ("%%B\bin\erl.exe") do (
            set "ERLDIR=%%C"
        )
    )
)

set "SCRIPT_DIR=%~dp0"
for %%A in ("%SCRIPT_DIR%\..") do (
    set "ROOT_DIR=%%~fA"
)

for /D %%A in ("%ROOT_DIR%\lib\*") do (
    set FILES=!FILES! "%%A\ebin"
)

cd %ROOT_DIR% && %ERLDIR% -noinput +Bd -sname lorawan -pa !FILES! -s lorawan_app -config releases/{{release_version}}/sys.config
