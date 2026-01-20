@echo off
REM Persistent worker client call - avoids repeated heavy imports
REM This batch uses worker_client.py which auto-starts worker_server.py if needed

REM Get the directory where this batch file is located
cd /d "%~dp0\..\app_py"

set PYTHONPATH=%CD%
set PYTHONIOENCODING=utf-8

REM Call the worker client (prints JSON with delimiters); output is redirected by the DLL
"%CD%\venv\Scripts\python.exe" -O "%CD%\worker_client.py" "%~1" > "%~2" 2>&1