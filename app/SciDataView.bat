@echo off
setlocal enabledelayedexpansion
cd /d "%~dp0"
title SciDataView Launcher

:: ==============================================================================
:: 1. Search for R Runtime (Portable -> PATH -> Program Files)
:: ==============================================================================
set "RSCRIPT="

:: Check local R-Portable
if exist "%~dp0R-Portable\bin\x64\Rscript.exe" set "RSCRIPT=%~dp0R-Portable\bin\x64\Rscript.exe"
if not defined RSCRIPT if exist "%~dp0..\R-Portable\bin\x64\Rscript.exe" set "RSCRIPT=%~dp0..\R-Portable\bin\x64\Rscript.exe"
if not defined RSCRIPT if exist "%~dp0R-Portable\bin\Rscript.exe" set "RSCRIPT=%~dp0R-Portable\bin\Rscript.exe"
if not defined RSCRIPT if exist "%~dp0..\R-Portable\bin\Rscript.exe" set "RSCRIPT=%~dp0..\R-Portable\bin\Rscript.exe"

:: Check PATH via where
if not defined RSCRIPT (
    where Rscript.exe >nul 2>nul
    if not errorlevel 1 (
        for /f "delims=" %%I in ('where Rscript.exe') do (
            if not defined RSCRIPT set "RSCRIPT=%%I"
        )
    )
)

:: Check Program Files fallback directories
if not defined RSCRIPT (
    for /d %%D in ("%ProgramFiles%\R\R-*") do (
        if exist "%%D\bin\x64\Rscript.exe" set "RSCRIPT=%%D\bin\x64\Rscript.exe"
        if not defined RSCRIPT if exist "%%D\bin\Rscript.exe" set "RSCRIPT=%%D\bin\Rscript.exe"
    )
)

:: ==============================================================================
:: 2. Fallback when R is not found
:: ==============================================================================
if not defined RSCRIPT (
    echo ==============================================================================
    echo [ERROR] R runtime was not found on your system!
    echo ==============================================================================
    echo SciDataView requires R to run.
    echo.
    echo If you want a standalone version that runs without installing R:
    echo   - Download "SciDataView-Windows.zip" from GitHub Releases.
    echo.
    echo If you want to install official R on your system:
    echo   - Download from https://cloud.r-project.org
    echo ==============================================================================
    echo.
    set /p "OPEN_WEB=Would you like to open CRAN website to download R? [Y/N, Default: Y]: "
    if "!OPEN_WEB!"=="" set "OPEN_WEB=Y"
    if /i "!OPEN_WEB!"=="Y" start https://cloud.r-project.org/bin/windows/base/
    pause
    exit /b 1
)

if not defined RSCRIPT (
    echo [ERROR] R runtime is still not available. Exiting...
    pause
    exit /b 1
)

:: ==============================================================================
:: 3. Launch SciDataView
:: ==============================================================================
echo [INFO] Using R runtime: !RSCRIPT!
echo Starting SciDataView Desktop Application...

if exist "%~dp0run_app.R" (
    "!RSCRIPT!" "%~dp0run_app.R"
) else (
    "!RSCRIPT!" "%~dp0app\run_app.R"
)

if %ERRORLEVEL% NEQ 0 (
    echo.
    echo ==============================================================================
    echo [ERROR] SciDataView exited with error code %ERRORLEVEL%.
    echo ==============================================================================
    pause
)
