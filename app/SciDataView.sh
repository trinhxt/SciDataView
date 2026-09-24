#!/usr/bin/env bash
# ==============================================================================
# SciDataView - Desktop Application Launcher (macOS / Linux)
# ==============================================================================
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

echo "=============================================================================="
echo "SciDataView - Universal Scientific Data Profiler"
echo "=============================================================================="

# 1. Search for Rscript in PATH and standard locations
RSCRIPT=""
if command -v Rscript >/dev/null 2>&1; then
    RSCRIPT="$(command -v Rscript)"
elif [ -x "/usr/local/bin/Rscript" ]; then
    RSCRIPT="/usr/local/bin/Rscript"
elif [ -x "/usr/bin/Rscript" ]; then
    RSCRIPT="/usr/bin/Rscript"
elif [ -x "/opt/R/bin/Rscript" ]; then
    RSCRIPT="/opt/R/bin/Rscript"
elif [ -x "/Library/Frameworks/R.framework/Resources/bin/Rscript" ]; then
    RSCRIPT="/Library/Frameworks/R.framework/Resources/bin/Rscript"
fi

if [ -z "$RSCRIPT" ]; then
    echo "[ERROR] R runtime (Rscript) was not found on your system!"
    echo "SciDataView requires R to run."
    echo ""
    echo "Please install R for your operating system:"
    echo "  - macOS: brew install r  OR  download from https://cloud.r-project.org/bin/macosx/"
    echo "  - Ubuntu/Debian: sudo apt install r-base"
    echo "  - Fedora: sudo dnf install R"
    echo "=============================================================================="
    exit 1
fi

echo "[INFO] Using R runtime: $RSCRIPT"
echo "Starting SciDataView Desktop Application..."

if [ -f "run_app.R" ]; then
    "$RSCRIPT" run_app.R
elif [ -f "app/run_app.R" ]; then
    "$RSCRIPT" app/run_app.R
else
    echo "[ERROR] Could not find run_app.R!"
    exit 1
fi
