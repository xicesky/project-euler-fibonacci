#!/bin/sh
set -e

REPORT="benchmark-report.html"

echo "Building everything"
stack build --test --haddock

echo "Benchmarking"
stack run -- -o "$REPORT"

echo "Report generated:"
ls -la "$REPORT"

