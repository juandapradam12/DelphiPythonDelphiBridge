@echo off
echo Starting CylinderEngine64...
echo.

cd /d "c:\CylinderCenterlineApp\app_py"

echo Checking Python environment...
python --version
echo.

echo Starting 64-bit compute engine...
python CylinderEngine64.py

pause