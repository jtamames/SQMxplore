#!/bin/bash
# Watermelon launcher
# Usage:
#   ./watermelon.sh                 (uses default port 3838)
#   ./watermelon.sh 8080            (custom port)
#   ./watermelon.sh 3838 nobrowser  (do not auto-open the browser)

set -e

PORT="${1:-3838}"
HOST="127.0.0.1"
NOBROWSER="${2:-}"

APP_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

if ! command -v Rscript &> /dev/null; then
  echo "ERROR: Rscript not found."
  echo "Activate your SqueezeMeta conda environment first:"
  echo "  conda activate SqueezeMeta"
  exit 1
fi

# Detect a browser-opening command
BROWSER_CMD=""
if [ "$NOBROWSER" != "nobrowser" ]; then
  for cmd in xdg-open open firefox google-chrome chromium-browser sensible-browser; do
    if command -v "$cmd" &> /dev/null; then
      BROWSER_CMD="$cmd"
      break
    fi
  done
fi

URL="http://${HOST}:${PORT}"

echo ""
echo "============================================================"
echo " Watermelon"
echo "------------------------------------------------------------"
if [ -n "$BROWSER_CMD" ]; then
  echo " Starting up — your browser will open once the app is ready."
  echo " If it does not, open this URL manually:"
else
  echo " Please open your browser and enter the following URL:"
fi
echo "   ${URL}"
echo "============================================================"
echo ""

# Open the browser only AFTER the server actually accepts connections.
# Poll the port instead of using a fixed sleep, so we never open too early.
if [ -n "$BROWSER_CMD" ]; then
  (
    for i in $(seq 1 60); do      # wait up to ~60s
      if (exec 3<>"/dev/tcp/${HOST}/${PORT}") 2>/dev/null; then
        exec 3>&- 3<&-
        sleep 1                   # tiny grace period for Shiny to be fully ready
        "$BROWSER_CMD" "$URL" >/dev/null 2>&1 || true
        exit 0
      fi
      sleep 1
    done
    echo "WARNING: server did not come up within 60s — open ${URL} manually."
  ) &
fi

# launch.browser = FALSE: bash handles the browser (above).
# quiet = TRUE suppresses Shiny's default "Listening on ..." line.
exec Rscript -e "shiny::runApp('${APP_DIR}', host='${HOST}', port=${PORT}, launch.browser=FALSE, quiet=TRUE)"
