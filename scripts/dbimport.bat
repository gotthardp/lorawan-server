@echo OFF

set SERVER=localhost:8080
set AUTH=--user=admin --password=admin
set DIR=%1

for %%F in (%DIR%\*.json) do (
    echo wget -nv %AUTH% --header=Content-Type:application/json --post-file=%%F http://%SERVER%/api/%%~nF
)
