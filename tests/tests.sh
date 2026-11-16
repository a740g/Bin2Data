#!/bin/bash
set -e

# Bin2Data test runner for Linux/macOS

if [ -f "./Bin2Data.exe" ]; then
    EXE="./Bin2Data.exe"
elif [ -f "./Bin2Data" ]; then
    EXE="./Bin2Data"
else
    echo "Error: Bin2Data executable not found. Please run 'make' first."
    exit 1
fi

SAMPLE_FILE="tests/sample.txt"
if [ ! -f "$SAMPLE_FILE" ]; then
    echo "Error: Sample file $SAMPLE_FILE not found."
    exit 1
fi

FAILED=0

echo "Running Bin2Data tests..."

# Test 1: Help output
echo -n "Test 1: Help output... "
if $EXE -? | grep -q "Bin2Data: Converts binary files to QB64-PE data"; then
    echo "PASSED"
else
    echo "FAILED"
    FAILED=$((FAILED + 1))
fi

BI_FILE="${SAMPLE_FILE}.bi"
H_FILE="${SAMPLE_FILE}.h"
DEFLATE_FILE="${SAMPLE_FILE}.deflate"

# Test 2: DATA mode (.bi)
echo -n "Test 2: DATA mode... "
rm -f "$BI_FILE"
$EXE -o -d "$SAMPLE_FILE" > /dev/null
if [ -f "$BI_FILE" ] && grep -q "DATA " "$BI_FILE"; then
    echo "PASSED"
    rm -f "$BI_FILE"
else
    echo "FAILED"
    FAILED=$((FAILED + 1))
fi

# Test 3: CONST mode (.bi)
echo -n "Test 3: CONST mode... "
rm -f "$BI_FILE"
$EXE -o -c "$SAMPLE_FILE" > /dev/null
if [ -f "$BI_FILE" ] && grep -q "CONST " "$BI_FILE"; then
    echo "PASSED"
    rm -f "$BI_FILE"
else
    echo "FAILED"
    FAILED=$((FAILED + 1))
fi

# Test 4: C array mode (.h)
echo -n "Test 4: C array mode... "
rm -f "$H_FILE"
$EXE -o -p "$SAMPLE_FILE" > /dev/null
if [ -f "$H_FILE" ] && grep -q "const uint8_t " "$H_FILE"; then
    echo "PASSED"
    rm -f "$H_FILE"
else
    echo "FAILED"
    FAILED=$((FAILED + 1))
fi

# Test 5: Raw deflate mode (.deflate)
echo -n "Test 5: Raw deflate mode... "
rm -f "$DEFLATE_FILE"
$EXE -o -r "$SAMPLE_FILE" > /dev/null
if [ -f "$DEFLATE_FILE" ]; then
    echo "PASSED"
    rm -f "$DEFLATE_FILE"
else
    echo "FAILED"
    FAILED=$((FAILED + 1))
fi

if [ $FAILED -gt 0 ]; then
    echo "$FAILED test(s) failed."
    exit 1
fi

echo "All tests passed successfully!"
exit 0
