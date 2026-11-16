# Bin2Data test runner for Windows (PowerShell)
$ErrorActionPreference = "Stop"

if (Test-Path ".\Bin2Data.exe") {
    $exe = ".\Bin2Data.exe"
} elseif (Test-Path ".\Bin2Data") {
    $exe = ".\Bin2Data"
} else {
    Write-Error "Error: Bin2Data executable not found. Please run 'make' first."
    exit 1
}

$sampleFile = "tests/sample.txt"
if (-not (Test-Path $sampleFile)) {
    Write-Error "Error: Sample file $sampleFile not found."
    exit 1
}

$failed = 0

Write-Host "Running Bin2Data tests..."

# Test 1: Help output
Write-Host "Test 1: Help output (-? and -h)... " -NoNewline
$helpOutput1 = & $exe -? | Out-String
$helpOutput2 = & $exe -h | Out-String
if (($helpOutput1 -match "Bin2Data: Converts binary files to QB64-PE data") -and `
    ($helpOutput2 -match "Bin2Data: Converts binary files to QB64-PE data")) {
    Write-Host "PASSED" -ForegroundColor Green
} else {
    Write-Host "FAILED" -ForegroundColor Red
    $failed++
}

# Test 2: DATA mode (.bi, compressed)
Write-Host "Test 2: DATA mode (compressed)... " -NoNewline
$biFile = "$sampleFile.bi"
if (Test-Path $biFile) { Remove-Item $biFile -Force }
& $exe -o -d $sampleFile | Out-Null
if ((Test-Path $biFile) -and ((Get-Content $biFile -Raw) -match "DATA \d+,\d+,-1")) {
    Write-Host "PASSED" -ForegroundColor Green
    Remove-Item $biFile -Force
} else {
    Write-Host "FAILED" -ForegroundColor Red
    $failed++
}

# Test 3: DATA mode (.bi, stored/uncompressed)
Write-Host "Test 3: DATA mode (stored/uncompressed)... " -NoNewline
if (Test-Path $biFile) { Remove-Item $biFile -Force }
& $exe -o -s -d $sampleFile | Out-Null
if ((Test-Path $biFile) -and ((Get-Content $biFile -Raw) -match "DATA \d+,\d+,0")) {
    Write-Host "PASSED" -ForegroundColor Green
    Remove-Item $biFile -Force
} else {
    Write-Host "FAILED" -ForegroundColor Red
    $failed++
}

# Test 4: CONST mode (.bi)
Write-Host "Test 4: CONST mode... " -NoNewline
if (Test-Path $biFile) { Remove-Item $biFile -Force }
& $exe -o -c $sampleFile | Out-Null
if ((Test-Path $biFile) -and ((Get-Content $biFile -Raw) -match "CONST DATA_")) {
    Write-Host "PASSED" -ForegroundColor Green
    Remove-Item $biFile -Force
} else {
    Write-Host "FAILED" -ForegroundColor Red
    $failed++
}

# Test 5: C array mode (.h)
Write-Host "Test 5: C array mode... " -NoNewline
$hFile = "$sampleFile.h"
if (Test-Path $hFile) { Remove-Item $hFile -Force }
& $exe -o -p $sampleFile | Out-Null
if ((Test-Path $hFile) -and ((Get-Content $hFile -Raw) -match "const uint8_t ")) {
    Write-Host "PASSED" -ForegroundColor Green
    Remove-Item $hFile -Force
} else {
    Write-Host "FAILED" -ForegroundColor Red
    $failed++
}

# Test 6: Raw deflate mode (.deflate)
Write-Host "Test 6: Raw deflate mode... " -NoNewline
$deflateFile = "$sampleFile.deflate"
if (Test-Path $deflateFile) { Remove-Item $deflateFile -Force }
& $exe -o -r $sampleFile | Out-Null
if (Test-Path $deflateFile) {
    Write-Host "PASSED" -ForegroundColor Green
    Remove-Item $deflateFile -Force
} else {
    Write-Host "FAILED" -ForegroundColor Red
    $failed++
}

# Test 7: Unknown option warning
Write-Host "Test 7: Unknown option check... " -NoNewline
$warnOutput = & $exe -unknown_option_test | Out-String
if ($warnOutput -match "Unknown option: -unknown_option_test") {
    Write-Host "PASSED" -ForegroundColor Green
} else {
    Write-Host "FAILED" -ForegroundColor Red
    $failed++
}

if ($failed -gt 0) {
    Write-Error "$failed test(s) failed."
    exit 1
}

Write-Host "All tests passed successfully!" -ForegroundColor Green
exit 0
