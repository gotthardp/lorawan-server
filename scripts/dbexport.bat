@echo OFF
for /f "tokens=1-3 delims=/-. " %%a in ('date/t') do (
    set AA=%%c
    set BB=%%b
    set CC=%%a
)

set SERVER=localhost:8080
set AUTH=--user=admin --password=admin
set DIR=backup-%AA%-%BB%-%CC%

mkdir %DIR%
for %%G in (user,area,gateway,multicast_channel,network,group,profile,device,node,ignored_node,handler,connector) do (
    echo Processing db %%G
    wget -nv %AUTH% --output-document=%DIR%/%%G.json http://%SERVER%/api/%%G
)
