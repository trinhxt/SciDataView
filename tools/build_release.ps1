# ==============================================================================
# SciDataView - Local Release Packaging Script
# ==============================================================================
[CmdletBinding()]
param(
    [string]$Version = "1.0"
)

$ErrorActionPreference = "Stop"
$rootDir = Split-Path -Parent $PSScriptRoot

Write-Host "================================================================================" -ForegroundColor Cyan
Write-Host "             SciDataView - Packaging SciDataView-$Version.zip                   " -ForegroundColor Cyan
Write-Host "================================================================================" -ForegroundColor Cyan

$zipName = "SciDataView-$Version.zip"
$zipPath = Join-Path $rootDir $zipName
if (Test-Path $zipPath) { Remove-Item $zipPath -Force }

# 1. Ensure app/ has all scripts
Copy-Item (Join-Path $rootDir "install_deps.R") (Join-Path $rootDir "app\install_deps.R") -Force

# 2. Update shortcut (.lnk) at root
$wsh = New-Object -ComObject WScript.Shell
$shortcut = $wsh.CreateShortcut((Join-Path $rootDir "SciDataView.lnk"))
$shortcut.TargetPath = "%windir%\system32\cmd.exe"
$shortcut.Arguments = "/c call app\SciDataView.bat"
$shortcut.WorkingDirectory = ""
$shortcut.IconLocation = "app\icon\app.ico,0"
$shortcut.Description = "SciDataView - Universal Scientific Data Profiler"
$shortcut.Save()

# 3. Create staging directory
$stage = Join-Path $env:TEMP "SciDataView_Stage"
if (Test-Path $stage) { Remove-Item $stage -Recurse -Force }
$packageDir = Join-Path $stage "SciDataView"
New-Item -ItemType Directory -Path $packageDir -Force | Out-Null

# Copy root files: README, LICENSE, and OS click shortcuts
Copy-Item (Join-Path $rootDir "README.md") $packageDir -Force
Copy-Item (Join-Path $rootDir "LICENSE") $packageDir -Force
Copy-Item (Join-Path $rootDir "SciDataView.lnk") $packageDir -Force
Copy-Item (Join-Path $rootDir "SciDataView.command") $packageDir -Force
Copy-Item (Join-Path $rootDir "SciDataView.sh") $packageDir -Force

# Copy app/ folder (containing app.R, run_app.R, SciDataView.bat, SciDataView.sh, install_deps.R, icon/)
Copy-Item (Join-Path $rootDir "app") $packageDir -Recurse -Force

# Compress to zip archive
Compress-Archive -Path $packageDir -DestinationPath $zipPath -CompressionLevel Optimal
Remove-Item $stage -Recurse -Force

Write-Host "[SUCCESS] Created: $zipPath ($((Get-Item $zipPath).Length / 1KB | ForEach-Object { '{0:N1} KB' -f $_ }))" -ForegroundColor Green
