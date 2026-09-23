@echo off
setlocal enabledelayedexpansion
cd /d "%~dp0"

if exist "%~dp0R-Portable\bin\x64\Rscript.exe" (
    set "RSCRIPT=%~dp0R-Portable\bin\x64\Rscript.exe"
) else if exist "%~dp0..\R-Portable\bin\x64\Rscript.exe" (
    set "RSCRIPT=%~dp0..\R-Portable\bin\x64\Rscript.exe"
) else (
    where Rscript >nul 2>nul
    if !ERRORLEVEL! equ 0 (
        set "RSCRIPT=Rscript"
        echo [INFO] R-Portable not found. Falling back to system R runtime...
    ) else (
        set "RSCRIPT="
    )
)

if "%RSCRIPT%"=="" (
    echo ==============================================================================
    echo [ERROR] R runtime not found!
    echo Please make sure the R-Portable folder is present, or R is installed on PATH.
    echo ==============================================================================
    pause
    exit /b 1
)

echo Starting SciDataView Desktop Application...
if exist "%~dp0run_app.R" (
    "%RSCRIPT%" "%~dp0run_app.R"
) else (
    "%RSCRIPT%" "%~dp0app\run_app.R"
)

if %ERRORLEVEL% NEQ 0 (
    echo.
    echo ==============================================================================
    echo [ERROR] SciDataView exited with error code %ERRORLEVEL%.
    echo ==============================================================================
    pause
)
