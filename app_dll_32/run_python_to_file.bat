@echo off
REM Simple Point Cloud Shape Reader - Unified implementation
REM Call the new simplified main.py with import profiling

REM Get the directory where this batch file is located
cd /d "%~dp0\..\app_py"

set PYTHONPATH=%CD%
set PYTHONIOENCODING=utf-8

REM Call the simplified main.py that returns shape and import timing
"%CD%\venv\Scripts\python.exe" -O "%CD%\main.py" "%~1" > "%~2" 2>&1