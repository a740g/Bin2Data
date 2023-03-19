'---------------------------------------------------------------------------------------------------------------------------------------------------------------
' Binary to Data converter using Base64 library
' Copyright (c) 2023 Samuel Gomes
'---------------------------------------------------------------------------------------------------------------------------------------------------------------

'---------------------------------------------------------------------------------------------------------------------------------------------------------------
' HEADER FILES
'---------------------------------------------------------------------------------------------------------------------------------------------------------------
'$Include:'./include/Base64.bi'
'---------------------------------------------------------------------------------------------------------------------------------------------------------------

'---------------------------------------------------------------------------------------------------------------------------------------------------------------
' METACOMMANDS
'---------------------------------------------------------------------------------------------------------------------------------------------------------------
$Console:Only
$ExeIcon:'./Bin2Data.ico'
$VersionInfo:ProductName=Bin2Data
$VersionInfo:CompanyName=Samuel Gomes
$VersionInfo:LegalCopyright=Copyright (c) 2023 Samuel Gomes
$VersionInfo:LegalTrademarks=All trademarks are property of their respective owners
$VersionInfo:Web=https://github.com/a740g
$VersionInfo:Comments=https://github.com/a740g
$VersionInfo:InternalName=Bin2Data
$VersionInfo:OriginalFilename=Bin2Data.exe
$VersionInfo:FileDescription=Bin2Data executable
$VersionInfo:FILEVERSION#=1,0,0,4
$VersionInfo:PRODUCTVERSION#=1,0,0,0
'---------------------------------------------------------------------------------------------------------------------------------------------------------------

'---------------------------------------------------------------------------------------------------------------------------------------------------------------
' CONSTANTS
'---------------------------------------------------------------------------------------------------------------------------------------------------------------
Const BASE64_CHARACTERS_PER_LINE = 20 * 4
'---------------------------------------------------------------------------------------------------------------------------------------------------------------


'---------------------------------------------------------------------------------------------------------------------------------------------------------------
' PROGRAM ENTRY POINT
'---------------------------------------------------------------------------------------------------------------------------------------------------------------
' Change to the directory specified by the environment
ChDir StartDir$

' If there are no command line parameters just show some info and exit
If CommandCount < 1 Then
    Color 7
    Print
    Print "Bin2Data: Converts binary files to QB64 Data"
    Print
    Print "Copyright (c) 2023 Samuel Gomes"
    Print
    Print "https://github.com/a740g"
    Print
    Print "Usage: bin2data [filespec]"
    Print
    Print "Note:"
    Print " * This will create filespec.bi"
    Print " * Bulk convert files using wildcards"
    Print " * If filespec.bi already exists, then it will not be overwritten"
    Print
    Print "Usage:"
    Print " 1. Encode the binary file using Bin2Data"
    Print " 2. Include the file or it's contents"
    Print " 3. Include Base64.bi at the top of your source"
    Print " 4. Include Base64.bas at the bottom of your source"
    Print " 5. Load the file:"
    Color 14
    Print "     Restore label_generated_by_bin2data"
    Print "     Dim buffer As String"
    Print "     buffer = LoadResource"
    Color 7
    Print
    System
End If

Print

' Convert all files requested
Dim As Unsigned Long i
For i = 1 To CommandCount
    MakeResource Command$(i)
Next

System
'---------------------------------------------------------------------------------------------------------------------------------------------------------------

'---------------------------------------------------------------------------------------------------------------------------------------------------------------
' FUNCTIONS & SUBROUTINES
'---------------------------------------------------------------------------------------------------------------------------------------------------------------
' Gets the filename portion from a file path
Function GetFileNameFromPath$ (pathName As String)
    Dim i As Unsigned Long

    ' Retrieve the position of the first / or \ in the parameter from the
    For i = Len(pathName) To 1 Step -1
        If Asc(pathName, i) = KEY_SLASH Or Asc(pathName, i) = KEY_BACKSLASH Then Exit For
    Next

    ' Return the full string if pathsep was not found
    If i = 0 Then
        GetFileNameFromPath = pathName
    Else
        GetFileNameFromPath = Right$(pathName, Len(pathName) - i)
    End If
End Function


' Removes invalid characters from filenames and makes a valid QB64 line label
Function MakeLegalLabel$ (fileName As String, fileSize As Unsigned Long)
    Dim As Unsigned Long i
    Dim As String label: label = fileName

    For i = 1 To Len(label)
        Select Case Asc(label, i)
            Case KEY_0 To KEY_9, KEY_UPPER_A To KEY_UPPER_Z, KEY_LOWER_A To KEY_LOWER_Z, KEY_UNDERSCORE ' legal characters
                ' NOP
            Case Else
                Asc(label, i) = KEY_UNDERSCORE ' replace with underscore
        End Select
    Next

    MakeLegalLabel = "Data_" + label + "_" + Trim$(Str$(fileSize)) + ":"
End Function


' This reads in fileNames and converts it to Base64 after compressing it (if needed)
' The file is then written as a QB64 include file
Sub MakeResource (fileName As String)
    If Not FileExists(fileName) Then
        Print fileName; " does not exist!"
        Exit Sub
    End If

    Dim As String biFileName: biFileName = fileName + ".bi"

    If FileExists(biFileName) Then
        Print biFileName; " already exists!"
        Exit Sub
    End If

    Print "Processing "; fileName

    Dim As Long fh: fh = FreeFile
    Open fileName For Binary Access Read As fh

    Dim As Unsigned Long ogSize: ogSize = LOF(fh)

    If ogSize < 1 Then
        Print fileName; " is empty!"
        Close fh
        Exit Sub
    End If

    ' Read in the whole file
    Dim As String buffer: buffer = Input$(ogSize, fh)
    Close fh


    Print "File size is"; ogSize; "bytes"

    Print "Writing to "; biFileName

    ' Open the output file and write the label
    Open biFileName For Output As fh
    Print #fh, MakeLegalLabel(GetFileNameFromPath$(fileName), ogSize) ' write the label

    ' Attempt to compress and see if we get any goodness
    Dim As String compBuf: compBuf = Deflate$(buffer)

    If Len(compBuf) < ogSize Then ' we got goodness
        buffer = EncodeBase64(compBuf) ' we do not need the original buffer contents
        Print #fh, "Data "; LTrim$(Str$(ogSize)); ","; LTrim$(Str$(Len(buffer))); ","; LTrim$(Str$(TRUE))

        Print Using "Compressed###.##%"; 100 - 100! * Len(compBuf) / ogSize

        compBuf = NULLSTRING
    Else ' no goodness
        buffer = EncodeBase64(buffer) ' we do not need the original buffer contents
        Print #fh, "Data "; LTrim$(Str$(ogSize)); ","; LTrim$(Str$(Len(buffer))); ","; LTrim$(Str$(FALSE))
        compBuf = NULLSTRING

        Print "Stored"
    End If

    Dim As Unsigned Long i
    For i = 1 To Len(buffer)
        If (i - 1) Mod BASE64_CHARACTERS_PER_LINE = 0 Then
            If i > 1 Then Print #fh, NULLSTRING
            Print #fh, "Data ";
        End If

        Print #fh, Chr$(Asc(buffer, i));
    Next

    Close fh

    Print "Done"
End Sub
'---------------------------------------------------------------------------------------------------------------------------------------------------------------

'---------------------------------------------------------------------------------------------------------------------------------------------------------------
' MODULE FILES
'---------------------------------------------------------------------------------------------------------------------------------------------------------------
'$Include:'./include/Base64.bas'
'---------------------------------------------------------------------------------------------------------------------------------------------------------------
'---------------------------------------------------------------------------------------------------------------------------------------------------------------

