'-----------------------------------------------------------------------------------------------------------------------
' Binary to Data converter using Base64 library
' Copyright (c) 2023 Samuel Gomes
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' HEADER FILES
'-----------------------------------------------------------------------------------------------------------------------
'$Include:'include/CRTLib.bi'
'$Include:'include/FileOps.bi'
'$Include:'include/Base64.bi'
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' METACOMMANDS
'-----------------------------------------------------------------------------------------------------------------------
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
$VersionInfo:FILEVERSION#=1,1,0,0
$VersionInfo:PRODUCTVERSION#=1,1,0,0
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' CONSTANTS
'-----------------------------------------------------------------------------------------------------------------------
Const BASE64_CHARACTERS_PER_LINE_MIN = 1
Const BASE64_CHARACTERS_PER_LINE_DEFAULT = 20 * 4
Const BASE64_CHARACTERS_PER_LINE_MAX = 4096
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' GLOBAL VARIABLES
'-----------------------------------------------------------------------------------------------------------------------
Dim Shared dataCPL As Long: dataCPL = BASE64_CHARACTERS_PER_LINE_DEFAULT ' characters per data line
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' PROGRAM ENTRY POINT
'-----------------------------------------------------------------------------------------------------------------------
' Change to the directory specified by the environment
ChDir StartDir$

' If there are no command line parameters just show some info and exit
If CommandCount < 1 Or GetProgramArgumentIndex(KEY_QUESTION_MARK) > 0 Then
    Color 7
    Print
    Print "Bin2Data: Converts binary files to QB64 Data"
    Print
    Print "Copyright (c) 2023 Samuel Gomes"
    Print
    Print "https://github.com/a740g"
    Print
    Print "Usage: Bin2Data [-w characters_per_data_line] [filespec]"
    Print "   -w: A number specifying the number of characters per line. Default"; dataCPL
    Print
    Print "Note:"
    Print " * This will create filespec.bi"
    Print " * Bulk convert files using wildcards"
    Print " * filespec can be a URL"
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
Dim argName As Integer
Dim argIndex As Long: argIndex = 1 ' start with the first argument

Do
    argName = ToLower(GetProgramArgument("w", argIndex))

    Select Case argName
        Case -1 ' ' no more arguments
            Exit Do

        Case KEY_LOWER_W ' w
            argIndex = argIndex + 1 ' value at next index
            dataCPL = ClampLong(Val(Command$(argIndex)), BASE64_CHARACTERS_PER_LINE_MIN, BASE64_CHARACTERS_PER_LINE_MAX)
            Print "Characters per data line set to"; dataCPL

        Case Else ' probably a file name
            MakeResource Command$(argIndex)
    End Select

    argIndex = argIndex + 1 ' move to the next index
Loop Until argName = -1

System
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' FUNCTIONS & SUBROUTINES
'-----------------------------------------------------------------------------------------------------------------------
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
    Dim As String biFileName: biFileName = fileName + ".bi"

    If FileExists(biFileName) Then
        Print biFileName; " already exists!"
        Exit Sub
    End If

    Print "Processing "; fileName


    ' Read in the whole file
    Dim As String buffer: buffer = LoadFile(fileName)
    Dim As Unsigned Long ogSize: ogSize = Len(buffer)

    If ogSize < 1 Then
        Print fileName; " is empty or could not be read!"
        Exit Sub
    End If


    Print "File size is"; ogSize; "bytes"

    Print "Writing to "; biFileName

    ' Open the output file and write the label
    Dim fh As Long: fh = FreeFile
    Open biFileName For Output As fh
    Print #fh, MakeLegalLabel(GetFileNameFromPathOrURL$(fileName), ogSize) ' write the label

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
        If (i - 1) Mod dataCPL = 0 Then
            If i > 1 Then Print #fh, NULLSTRING
            Print #fh, "Data ";
        End If

        Print #fh, Chr$(Asc(buffer, i));
    Next

    Close fh

    Print "Done"
End Sub
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' MODULE FILES
'-----------------------------------------------------------------------------------------------------------------------
'$Include:'include/ProgramArgs.bas'
'$Include:'include/FileOps.bas'
'$Include:'include/Base64.bas'
'-----------------------------------------------------------------------------------------------------------------------
'-----------------------------------------------------------------------------------------------------------------------

