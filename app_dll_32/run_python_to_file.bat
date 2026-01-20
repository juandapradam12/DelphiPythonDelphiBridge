@echo off
REM Clean 32-bit bridge - using proven algorithms
cd /d "C:\CylinderCenterlineApp\app_py"
set PYTHONPATH=C:\CylinderCenterlineApp\app_py
set PYTHONIOENCODING=utf-8
REM Use main.py with proven cylinder fitting algorithms
"C:\CylinderCenterlineApp\app_py\venv\Scripts\python.exe" -O "C:\CylinderCenterlineApp\app_py\main.py" "%~1" > "%~2" 2>&1