@echo off
setlocal EnableDelayedExpansion

for /D %%A in ("%PROGRAMFILES%\erl*" "%PROGRAMFILES(x86)%\erl*") do (
    for /D %%B in ("%%A\erts*") do (
        for /D %%C in ("%%B\bin\erl.exe") do (
            set "ERL_DIR=%%C"
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

set ERL_ARGS="-lager log_root "log""
cd %ROOT_DIR% && %ERL_DIR% -noinput +Bd -sname lorawan -pa !FILES! -s lorawan_app %ERL_ARGS% -config releases/{{release_version}}/sys.config
