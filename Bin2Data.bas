'-----------------------------------------------------------------------------------------------------------------------
' QB64-PE Binary to DATA converter
' Copyright (c) 2024 Samuel Gomes
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' HEADER FILES
'-----------------------------------------------------------------------------------------------------------------------
'$INCLUDE:'include/StringOps.bi'
'$INCLUDE:'include/MathOps.bi'
'$INCLUDE:'include/FileOps.bi'
'$INCLUDE:'include/Base64.bi'
'$INCLUDE:'include/Deflate.bi'
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' METACOMMANDS
'-----------------------------------------------------------------------------------------------------------------------
$CONSOLE:ONLY
$EXEICON:'./Bin2Data.ico'
$VERSIONINFO:ProductName='Bin2Data'
$VERSIONINFO:CompanyName='Samuel Gomes'
$VERSIONINFO:LegalCopyright='Copyright (c) 2024 Samuel Gomes'
$VERSIONINFO:LegalTrademarks='All trademarks are property of their respective owners'
$VERSIONINFO:Web='https://github.com/a740g'
$VERSIONINFO:Comments='https://github.com/a740g'
$VERSIONINFO:InternalName='Bin2Data'
$VERSIONINFO:OriginalFilename='Bin2Data.exe'
$VERSIONINFO:FileDescription='Bin2Data executable'
$VERSIONINFO:FILEVERSION#=2,3,0,0
$VERSIONINFO:PRODUCTVERSION#=2,3,0,0
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' CONSTANTS
'-----------------------------------------------------------------------------------------------------------------------
CONST BASE64_CHAR_PER_LINE_MIN = SIZE_OF_BYTE
CONST BASE64_CHAR_PER_LINE_DEFAULT = 28 * SIZE_OF_LONG
CONST BASE64_CHAR_PER_LINE_MAX = 4096
CONST COMP_LEVEL = 0 ' whatever is the default for the library
CONST INDENT_SPACES = 4
CONST GENERATE_DATA = 0 ' BASIC DATA
CONST GENERATE_CONST = 1 ' BASIC CONST
CONST GENERATE_CARR = 2 ' C array
CONST GENERATE_RAW = 3 ' raw file dump (only if file is compressed)
CONST FILE_EXT_BI = ".bi"
CONST FILE_EXT_H = ".h"
CONST FILE_EXT_DEFLATE = ".deflate"
CONST ID_NAME_LENGTH_MAX = 40
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' TYPES
'-----------------------------------------------------------------------------------------------------------------------
TYPE AppOtionType
    mode AS _UNSIGNED _BYTE ' see GENERATE_* CONSTs above
    charPerLine AS _UNSIGNED LONG ' characters / line
    compLevel AS _UNSIGNED INTEGER ' compression level
    overwrite AS _BYTE ' overwrite output?
    store AS _BYTE ' bypass compression?
END TYPE
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' GLOBAL VARIABLES
'-----------------------------------------------------------------------------------------------------------------------
DIM SHARED appOption AS AppOtionType ' global options
REDIM qb64peKeyword(0 TO 0) AS STRING ' QB64-PE keywords array
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' PROGRAM ENTRY POINT
'-----------------------------------------------------------------------------------------------------------------------
SetDefaultAppOptions

' Change to the directory specified by the environment
CHDIR _STARTDIR$

' If there are no command line parameters just show some info and exit
IF _COMMANDCOUNT < 1 _ORELSE GetProgramArgumentIndex(KEY_QUESTION_MARK) > 0 THEN
    COLOR 7
    PRINT "Bin2Data: Converts binary files to QB64-PE data"
    PRINT "Copyright (c) 2024 Samuel Gomes"
    PRINT "https://github.com/a740g"
    PRINT
    PRINT "Usage: Bin2Data [-w characters_per_data_line] [-i compression_level] [-d] [-c] [-p] [-r] [-s] [-o] [filespec]"
    PRINT "   -w: A number specifying the number of characters per data line."; BASE64_CHAR_PER_LINE_MIN; "-"; BASE64_CHAR_PER_LINE_MAX; "(default"; STR$(appOption.charPerLine); ")"
    PRINT "   -i: A number specifying the compression level (anything more than 15 will be too slow). 1 -"; UINTEGER_MAX; "(default 15)"
    PRINT "   -d: Generate DATA (.bas; default)"
    PRINT "   -c: Generate a CONST (.bas; suitable for small files)"
    PRINT "   -p: Generate a C array (.h)"
    PRINT "   -r: Dump the raw compressed file (.deflate)"
    PRINT "   -s: Disable compression and store the file instead"
    PRINT "   -o: Overwrite output file if it exists"
    PRINT
    PRINT "Note:"
    PRINT " * Will create filespec.bi/.h/.deflate (based on the switches)"
    PRINT " * Can bulk convert files using wildcards"
    PRINT " * filespec can be a URL"
    PRINT " * If filespec.bi/.h/.deflate exists, then it will not be overwritten (unless -o is specified)"
    PRINT " * Character per line may be changed in CONST mode due to QB64's 500 line continuation limit"
    PRINT " * C output is barebones. Use sizeof to get the array size"
    PRINT
    PRINT "Usage:"
    PRINT " 1. Encode the binary file using Bin2Data"
    PRINT " 2. Include the file or it's contents"
    PRINT " 3. Include Base64.bi at the top of your source (if using the Toolbox64 version)"
    PRINT " 4. Include Base64.bas at the bottom of your source"
    PRINT " 5. Load the file:"
    COLOR 11
    PRINT "     DATA mode:"
    COLOR 14
    PRINT "         RESTORE label_generated_by_bin2data"
    PRINT "         DIM buffer AS STRING"
    PRINT "         buffer = Base64_LoadResourceData"
    PRINT
    COLOR 11
    PRINT "     CONST mode:"
    COLOR 14
    PRINT "         DIM buffer AS STRING"
    PRINT "         buffer = Base64_LoadResourceString(STRING_DATA_CONST, ORIGINAL_SIZE_CONST, IS_COMPRESSED_CONST)"
    COLOR 7
    PRINT
    PRINT "Get Base64.bas lite from: https://github.com/a740g/InForm-PE/blob/master/InForm/extensions/Base64.bas"
    SYSTEM
END IF

PRINT

' Convert all files requested
DIM argName AS INTEGER
DIM argIndex AS LONG: argIndex = 1 ' start with the first argument

DO
    argName = String_ToLowerCase(GetProgramArgument("widcprso", argIndex))

    SELECT CASE argName
        CASE -1 ' ' no more arguments
            EXIT DO

        CASE KEY_LOWER_W
            argIndex = argIndex + 1 ' value at next index
            appOption.charPerLine = Math_ClampLong(VAL(COMMAND$(argIndex)), BASE64_CHAR_PER_LINE_MIN, BASE64_CHAR_PER_LINE_MAX)
            PRINT "Characters per data line set to"; appOption.charPerLine

        CASE KEY_LOWER_I
            argIndex = argIndex + 1 ' value at next index
            appOption.compLevel = Math_ClampLong(VAL(COMMAND$(argIndex)), 1, UINTEGER_MAX)
            PRINT "Compression level set to"; appOption.compLevel

        CASE KEY_LOWER_D
            appOption.mode = GENERATE_DATA
            PRINT "DATA generation enabled"

        CASE KEY_LOWER_C
            appOption.mode = GENERATE_CONST
            PRINT "CONST generation enabled"

        CASE KEY_LOWER_P
            appOption.mode = GENERATE_CARR
            PRINT "C array generation enabled"

        CASE KEY_LOWER_R
            appOption.mode = GENERATE_RAW
            PRINT "Raw dump enabled"

            IF appOption.store THEN
                appOption.store = FALSE
                PRINT "Store mode disabled"
            END IF

        CASE KEY_LOWER_S
            IF appOption.mode = GENERATE_RAW THEN
                appOption.store = FALSE
                PRINT "Cannot enable store mode when raw dump is enabled"
            ELSE
                appOption.store = TRUE
                PRINT "Store mode enabled"
            END IF

        CASE KEY_LOWER_O
            appOption.overwrite = TRUE
            PRINT "Overwrite mode enabled"

        CASE ELSE ' probably a file name
            MakeResource COMMAND$(argIndex)
    END SELECT

    argIndex = argIndex + 1 ' move to the next index
LOOP UNTIL argName = -1

SYSTEM
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' FUNCTIONS & SUBROUTINES
'-----------------------------------------------------------------------------------------------------------------------
SUB SetDefaultAppOptions
    appOption.mode = GENERATE_DATA
    appOption.charPerLine = BASE64_CHAR_PER_LINE_DEFAULT
    appOption.compLevel = COMP_LEVEL
    appOption.overwrite = FALSE ' do not overwrite
    appOption.store = FALSE ' do not just store
END SUB


SUB MakeResource (fileName AS STRING)
    ' Get the base file name that we'll use to prefix before the generated extension
    DIM outputFileName AS STRING

    IF LEN(GetDriveOrSchemeFromPathOrURL(fileName)) > 2 THEN
        outputFileName = GetLegalFileName(GetFileNameFromPathOrURL(fileName))
    ELSE
        outputFileName = fileName
    END IF

    ' Get the output file name
    SELECT CASE appOption.mode
        CASE GENERATE_CARR
            outputFileName = outputFileName + FILE_EXT_H

        CASE GENERATE_RAW
            outputFileName = outputFileName + FILE_EXT_DEFLATE

        CASE ELSE ' GENERATE_DATA, GENERATE_CONST
            outputFileName = outputFileName + FILE_EXT_BI
    END SELECT

    ' Quickly check if the output file exists and if show a message and exit
    IF _NEGATE appOption.overwrite _ANDALSO _FILEEXISTS(outputFileName) THEN
        PRINT outputFileName; " already exists!"
        EXIT SUB
    END IF

    ' Load and compress the file (if needed)
    ' We are doing this here since this step is common for all modes
    DIM buffer AS STRING: buffer = _READFILE$(fileName)
    DIM ogBufSize AS _UNSIGNED LONG: ogBufSize = LEN(buffer) ' we'll need this later

    IF LEN(buffer) < 1 THEN
        PRINT fileName; " is empty or could not be read!"
        EXIT SUB
    END IF

    PRINT "File size is"; LEN(buffer); "bytes"

    ' Attempt to compress and see if we get any goodness
    IF appOption.store THEN
        PRINT "Stored"
    ELSE
        PRINT "Compressing data (this may take some time) ... ";
        DIM compBuf AS STRING: compBuf = DeflatePro(buffer, appOption.compLevel)
        PRINT "done"

        IF LEN(compBuf) < LEN(buffer) THEN
            PRINT USING "Compressed###.##%"; 100 - 100! * LEN(compBuf) / LEN(buffer)
            buffer = compBuf ' replace the original data
        ELSE
            IF appOption.mode = GENERATE_RAW THEN
                PRINT "Compressed file size is more than the original size!"
                EXIT SUB
            ELSE
                appOption.store = TRUE ' switch to store mode
            END IF
        END IF
    END IF

    PRINT "Writing to "; outputFileName

    ' Now we have the output filename and the buffer
    ' Call the appropriate resource generation function
    SELECT CASE appOption.mode
        CASE GENERATE_CONST
            MakeConst buffer, outputFileName, ogBufSize

        CASE GENERATE_CARR
            MakeCArray buffer, outputFileName, ogBufSize

        CASE GENERATE_RAW
            _WRITEFILE outputFileName, buffer

        CASE ELSE ' GENERATE_DATA
            MakeData buffer, outputFileName, ogBufSize
    END SELECT
END SUB


' Returns true if idName is a QB64PE keyword
FUNCTION IsQB64Keyword%% (idName AS STRING)
    ' This was plucked straight out of QB64-PE's 'syntax_highlighter_list.bas'
    ' We'll use this to build our QB64-PE keyword data table
    CONST QB64PE_KEYWORDS = "@?@$CHECKING@$ERROR@$CONSOLE@ONLY@$DYNAMIC@$ELSE@$ELSEIF@$END@$ENDIF@$EXEICON@$IF@$INCLUDE@$LET@$RESIZE@$SCREENHIDE@$SCREENSHOW@$STATIC@$VERSIONINFO@$VIRTUALKEYBOARD@ABS@ABSOLUTE@ACCESS@ALIAS@AND@APPEND@AS@ASC@ATN@BASE@BEEP@BINARY@BLOAD@BSAVE@BYVAL@CALL@CALLS@CASE@IS@CDBL@CDECL@CHAIN@CHDIR@CHR$@CINT@CIRCLE@CLEAR@CLNG@CLOSE@CLS@COLOR@COM@COMMAND$@COMMON@CONST@COS@CSNG@CSRLIN@CUSTOMTYPE@CVD@CVDMBF@CVI@CVL@CVS@CVSMBF@DATA@DATE$@DECLARE@DEF@DEFDBL@DEFINT@DEFLNG@DEFSNG@DEFSTR@DIM@DO@DOUBLE@DRAW@DYNAMIC@ELSE@ELSEIF@END@ENDIF@ENVIRON@ENVIRON$@EOF@EQV@ERASE@ERDEV@ERDEV$@ERL@ERR@ERROR@EVERYCASE@EXIT@EXP@FIELD@FILEATTR@FILES@FIX@FN@FOR@FRE@FREE@FREEFILE@FUNCTION@GET@GOSUB@GOTO@HEX$@IF@IMP@INKEY$@INP@INPUT@INPUT$@INSTR@INT@INTEGER@INTERRUPT@INTERRUPTX@IOCTL@IOCTL$@KEY@KILL@LBOUND@LCASE$@LEFT$@LEN@LET@LIBRARY@LINE@LIST@LOC@LOCATE@LOCK@LOF@LOG@LONG@LOOP@LPOS@LPRINT@LSET@LTRIM$@MID$@MKD$@MKDIR@MKDMBF$@MKI$@MKL$@MKS$@MKSMBF$@MOD@NAME@NEXT@NOT@OCT$@OFF@ON@OPEN@OPTION@OR@OUT@OUTPUT@PAINT@PALETTE@PCOPY@PEEK@PEN@PLAY@PMAP@POINT@POKE@POS@PRESET@PRINT@PSET@PUT@RANDOM@RANDOMIZE@READ@REDIM@REM@RESET@RESTORE@RESUME@RETURN@RIGHT$@RMDIR@RND@RSET@RTRIM$@RUN@SADD@SCREEN@SEEK@SEG@SELECT@SETMEM@SGN@SHARED@SHELL@SIGNAL@SIN@SINGLE@SLEEP@SOUND@SPACE$@SPC@SQR@STATIC@STEP@STICK@STOP@STR$@STRIG@STRING@STRING$@SUB@SWAP@SYSTEM@TAB@TAN@THEN@TIME$@TIMER@TO@TROFF@TRON@TYPE@UBOUND@UCASE$@UEVENT@UNLOCK@UNTIL@USING@VAL@VARPTR@VARPTR$@VARSEG@VIEW@WAIT@WEND@WHILE@WIDTH@WINDOW@WRITE@XOR@_ACOS@_ACOSH@_ALPHA@_ALPHA32@_ARCCOT@_ARCCSC@_ARCSEC@_ASIN@_ASINH@_ATAN2@_ATANH@_AUTODISPLAY@_AXIS@_BACKGROUNDCOLOR@_BIN$@_BIT@_BLEND@_BLINK@_BLUE@_BLUE32@_BUTTON@_BUTTONCHANGE@_BYTE@_CEIL@_CLEARCOLOR@_CLIP@_CLIPBOARD$@_CLIPBOARDIMAGE@_COMMANDCOUNT@_CONNECTED@_CONNECTIONADDRESS$@_CONNECTIONADDRESS@_CONSOLE@_CONSOLETITLE@_CONTINUE@_CONTROLCHR@_COPYIMAGE@_COPYPALETTE@_COSH@_COT@_COTH@_CSC@_CSCH@_CV@_CWD$@_D2G@_D2R@_DEFAULTCOLOR@_DEFINE@_DELAY@_DEPTHBUFFER@_DESKTOPHEIGHT@_DESKTOPWIDTH@_DEST@_DEVICE$@_DEVICEINPUT@_DEVICES@_DIR$@_DIREXISTS@_DISPLAY@_DISPLAYORDER@_DONTBLEND@_DONTWAIT@" _
        + "_ERRORLINE@_ERRORMESSAGE$@_EXIT@_EXPLICIT@_EXPLICITARRAY@_FILEEXISTS@_FLOAT@_FONT@_FONTHEIGHT@_FONTWIDTH@_FREEFONT@_FREEIMAGE@_FREETIMER@_FULLSCREEN@_G2D@_G2R@_GLRENDER@_GREEN@_GREEN32@_HEIGHT@_HIDE@_HYPOT@_ICON@_INCLERRORFILE$@_INCLERRORLINE@_INTEGER64@_KEYCLEAR@_KEYDOWN@_KEYHIT@_LASTAXIS@_LASTBUTTON@_LASTWHEEL@_LIMIT@_LOADFONT@_LOADIMAGE@_MAPTRIANGLE@_MAPUNICODE@_MEM@_MEMCOPY@_MEMELEMENT@_MEMEXISTS@_MEMFILL@_MEMFREE@_MEMGET@_MEMIMAGE@_MEMSOUND@_MEMNEW@_MEMPUT@_MIDDLE@_MK$@_MOUSEBUTTON@_MOUSEHIDE@_MOUSEINPUT@_MOUSEMOVE@_MOUSEMOVEMENTX@_MOUSEMOVEMENTY@_MOUSEPIPEOPEN@_MOUSESHOW@_MOUSEWHEEL@_MOUSEX@_MOUSEY@_NEWIMAGE@_OFFSET@_OPENCLIENT@_OPENCONNECTION@_OPENHOST@_OS$@_PALETTECOLOR@_PI@_PIXELSIZE@_PRESERVE@_PRINTIMAGE@_PRINTMODE@_PRINTSTRING@_PRINTWIDTH@_PUTIMAGE@_R2D@_R2G@_RED@_RED32@_RESIZE@_RESIZEHEIGHT@_RESIZEWIDTH@_RGB@_RGB32@_RGBA@_RGBA32@_ROUND@_SCREENCLICK@_SCREENEXISTS@_SCREENHIDE@_SCREENICON@_SCREENIMAGE@_SCREENMOVE@_SCREENPRINT@_SCREENSHOW@_SCREENX@_SCREENY@_SEC@_SECH@_SETALPHA@_SHELLHIDE@_SINH@_SNDBAL@_SNDCLOSE@_SNDCOPY@_SNDGETPOS@_SNDLEN@_SNDLIMIT@_SNDLOOP@_SNDOPEN@_SNDOPENRAW@_SNDPAUSE@_SNDPAUSED@_SNDPLAY@_SNDPLAYCOPY@_SNDPLAYFILE@_SNDPLAYING@_SNDRATE@_SNDRAW@_SNDRAWDONE@_SNDRAWLEN@_SNDSETPOS@_SNDSTOP@_SNDVOL@_SOURCE@_STARTDIR$@_STRCMP@_STRICMP@_TANH@_TITLE@_TITLE$@_UNSIGNED@_WHEEL@_WIDTH@_WINDOWHANDLE@_WINDOWHASFOCUS@_GLACCUM@_GLALPHAFUNC@_GLARETEXTURESRESIDENT@_GLARRAYELEMENT@_GLBEGIN@_GLBINDTEXTURE@_GLBITMAP@_GLBLENDFUNC@_GLCALLLIST@_GLCALLLISTS@_GLCLEAR@_GLCLEARACCUM@_GLCLEARCOLOR@_GLCLEARDEPTH@_GLCLEARINDEX@_GLCLEARSTENCIL@_GLCLIPPLANE@_GLCOLOR3B@_GLCOLOR3BV@_GLCOLOR3D@_GLCOLOR3DV@_GLCOLOR3F@_GLCOLOR3FV@_GLCOLOR3I@_GLCOLOR3IV@_GLCOLOR3S@_GLCOLOR3SV@_GLCOLOR3UB@_GLCOLOR3UBV@_GLCOLOR3UI@_GLCOLOR3UIV@_GLCOLOR3US@_GLCOLOR3USV@_GLCOLOR4B@_GLCOLOR4BV@_GLCOLOR4D@_GLCOLOR4DV@_GLCOLOR4F@_GLCOLOR4FV@_GLCOLOR4I@_GLCOLOR4IV@_GLCOLOR4S@_GLCOLOR4SV@_GLCOLOR4UB@_GLCOLOR4UBV@_GLCOLOR4UI@_GLCOLOR4UIV@_GLCOLOR4US@_GLCOLOR4USV@_GLCOLORMASK@_GLCOLORMATERIAL@_GLCOLORPOINTER@_GLCOPYPIXELS@_GLCOPYTEXIMAGE1D@_GLCOPYTEXIMAGE2D@_GLCOPYTEXSUBIMAGE1D@" _
        + "_GLCOPYTEXSUBIMAGE2D@_GLCULLFACE@_GLDELETELISTS@_GLDELETETEXTURES@_GLDEPTHFUNC@_GLDEPTHMASK@_GLDEPTHRANGE@_GLDISABLE@_GLDISABLECLIENTSTATE@_GLDRAWARRAYS@_GLDRAWBUFFER@_GLDRAWELEMENTS@_GLDRAWPIXELS@_GLEDGEFLAG@_GLEDGEFLAGPOINTER@_GLEDGEFLAGV@_GLENABLE@_GLENABLECLIENTSTATE@_GLEND@_GLENDLIST@_GLEVALCOORD1D@_GLEVALCOORD1DV@_GLEVALCOORD1F@_GLEVALCOORD1FV@_GLEVALCOORD2D@_GLEVALCOORD2DV@_GLEVALCOORD2F@_GLEVALCOORD2FV@_GLEVALMESH1@_GLEVALMESH2@_GLEVALPOINT1@_GLEVALPOINT2@_GLFEEDBACKBUFFER@_GLFINISH@_GLFLUSH@_GLFOGF@_GLFOGFV@_GLFOGI@_GLFOGIV@_GLFRONTFACE@_GLFRUSTUM@_GLGENLISTS@_GLGENTEXTURES@_GLGETBOOLEANV@_GLGETCLIPPLANE@_GLGETDOUBLEV@_GLGETERROR@_GLGETFLOATV@_GLGETINTEGERV@_GLGETLIGHTFV@_GLGETLIGHTIV@_GLGETMAPDV@_GLGETMAPFV@_GLGETMAPIV@_GLGETMATERIALFV@_GLGETMATERIALIV@_GLGETPIXELMAPFV@_GLGETPIXELMAPUIV@_GLGETPIXELMAPUSV@_GLGETPOINTERV@_GLGETPOLYGONSTIPPLE@_GLGETSTRING@_GLGETTEXENVFV@_GLGETTEXENVIV@_GLGETTEXGENDV@_GLGETTEXGENFV@_GLGETTEXGENIV@_GLGETTEXIMAGE@_GLGETTEXLEVELPARAMETERFV@_GLGETTEXLEVELPARAMETERIV@_GLGETTEXPARAMETERFV@_GLGETTEXPARAMETERIV@_GLHINT@_GLINDEXMASK@_GLINDEXPOINTER@_GLINDEXD@_GLINDEXDV@_GLINDEXF@_GLINDEXFV@_GLINDEXI@_GLINDEXIV@_GLINDEXS@_GLINDEXSV@_GLINDEXUB@_GLINDEXUBV@_GLINITNAMES@_GLINTERLEAVEDARRAYS@_GLISENABLED@_GLISLIST@_GLISTEXTURE@_GLLIGHTMODELF@_GLLIGHTMODELFV@_GLLIGHTMODELI@_GLLIGHTMODELIV@_GLLIGHTF@_GLLIGHTFV@_GLLIGHTI@_GLLIGHTIV@_GLLINESTIPPLE@_GLLINEWIDTH@_GLLISTBASE@_GLLOADIDENTITY@_GLLOADMATRIXD@_GLLOADMATRIXF@_GLLOADNAME@_GLLOGICOP@_GLMAP1D@_GLMAP1F@_GLMAP2D@_GLMAP2F@_GLMAPGRID1D@_GLMAPGRID1F@_GLMAPGRID2D@_GLMAPGRID2F@_GLMATERIALF@_GLMATERIALFV@_GLMATERIALI@_GLMATERIALIV@_GLMATRIXMODE@_GLMULTMATRIXD@_GLMULTMATRIXF@_GLNEWLIST@_GLNORMAL3B@_GLNORMAL3BV@_GLNORMAL3D@_GLNORMAL3DV@_GLNORMAL3F@_GLNORMAL3FV@_GLNORMAL3I@_GLNORMAL3IV@_GLNORMAL3S@_GLNORMAL3SV@_GLNORMALPOINTER@_GLORTHO@_GLPASSTHROUGH@_GLPIXELMAPFV@_GLPIXELMAPUIV@_GLPIXELMAPUSV@_GLPIXELSTOREF@_GLPIXELSTOREI@_GLPIXELTRANSFERF@_GLPIXELTRANSFERI@_GLPIXELZOOM@_GLPOINTSIZE@_GLPOLYGONMODE@_GLPOLYGONOFFSET@_GLPOLYGONSTIPPLE@" _
        + "_GLPOPATTRIB@_GLPOPCLIENTATTRIB@_GLPOPMATRIX@_GLPOPNAME@_GLPRIORITIZETEXTURES@_GLPUSHATTRIB@_GLPUSHCLIENTATTRIB@_GLPUSHMATRIX@_GLPUSHNAME@_GLRASTERPOS2D@_GLRASTERPOS2DV@_GLRASTERPOS2F@_GLRASTERPOS2FV@_GLRASTERPOS2I@_GLRASTERPOS2IV@_GLRASTERPOS2S@_GLRASTERPOS2SV@_GLRASTERPOS3D@_GLRASTERPOS3DV@_GLRASTERPOS3F@_GLRASTERPOS3FV@_GLRASTERPOS3I@_GLRASTERPOS3IV@_GLRASTERPOS3S@_GLRASTERPOS3SV@_GLRASTERPOS4D@_GLRASTERPOS4DV@_GLRASTERPOS4F@_GLRASTERPOS4FV@_GLRASTERPOS4I@_GLRASTERPOS4IV@_GLRASTERPOS4S@_GLRASTERPOS4SV@_GLREADBUFFER@_GLREADPIXELS@_GLRECTD@_GLRECTDV@_GLRECTF@_GLRECTFV@_GLRECTI@_GLRECTIV@_GLRECTS@_GLRECTSV@_GLRENDERMODE@_GLROTATED@_GLROTATEF@_GLSCALED@_GLSCALEF@_GLSCISSOR@_GLSELECTBUFFER@_GLSHADEMODEL@_GLSTENCILFUNC@_GLSTENCILMASK@_GLSTENCILOP@_GLTEXCOORD1D@_GLTEXCOORD1DV@_GLTEXCOORD1F@_GLTEXCOORD1FV@_GLTEXCOORD1I@_GLTEXCOORD1IV@_GLTEXCOORD1S@_GLTEXCOORD1SV@_GLTEXCOORD2D@_GLTEXCOORD2DV@_GLTEXCOORD2F@_GLTEXCOORD2FV@_GLTEXCOORD2I@_GLTEXCOORD2IV@_GLTEXCOORD2S@_GLTEXCOORD2SV@_GLTEXCOORD3D@_GLTEXCOORD3DV@_GLTEXCOORD3F@_GLTEXCOORD3FV@_GLTEXCOORD3I@_GLTEXCOORD3IV@_GLTEXCOORD3S@_GLTEXCOORD3SV@_GLTEXCOORD4D@_GLTEXCOORD4DV@_GLTEXCOORD4F@_GLTEXCOORD4FV@_GLTEXCOORD4I@_GLTEXCOORD4IV@_GLTEXCOORD4S@_GLTEXCOORD4SV@_GLTEXCOORDPOINTER@_GLTEXENVF@_GLTEXENVFV@_GLTEXENVI@_GLTEXENVIV@_GLTEXGEND@_GLTEXGENDV@_GLTEXGENF@_GLTEXGENFV@_GLTEXGENI@_GLTEXGENIV@_GLTEXIMAGE1D@_GLTEXIMAGE2D@_GLTEXPARAMETERF@_GLTEXPARAMETERFV@_GLTEXPARAMETERI@_GLTEXPARAMETERIV@_GLTEXSUBIMAGE1D@_GLTEXSUBIMAGE2D@_GLTRANSLATED@_GLTRANSLATEF@_GLVERTEX2D@_GLVERTEX2DV@_GLVERTEX2F@_GLVERTEX2FV@_GLVERTEX2I@_GLVERTEX2IV@_GLVERTEX2S@_GLVERTEX2SV@_GLVERTEX3D@_GLVERTEX3DV@_GLVERTEX3F@_GLVERTEX3FV@_GLVERTEX3I@_GLVERTEX3IV@_GLVERTEX3S@_GLVERTEX3SV@_GLVERTEX4D@_GLVERTEX4DV@_GLVERTEX4F@_GLVERTEX4FV@_GLVERTEX4I@_GLVERTEX4IV@_GLVERTEX4S@_GLVERTEX4SV@_GLVERTEXPOINTER@_GLVIEWPORT@SMOOTH@STRETCH@_ANTICLOCKWISE@_BEHIND@_CLEAR@_FILLBACKGROUND@_GLUPERSPECTIVE@_HARDWARE@_HARDWARE1@_KEEPBACKGROUND@_NONE@_OFF@_ONLY@_ONLYBACKGROUND@_ONTOP@_SEAMLESS@_SMOOTH@_SMOOTHSHRUNK@_SMOOTHSTRETCHED@" _
        + "_SOFTWARE@_SQUAREPIXELS@_STRETCH@_ALLOWFULLSCREEN@_ALL@_ECHO@_READFILE$@_WRITEFILE@_INSTRREV@_TRIM$@_ACCEPTFILEDROP@_FINISHDROP@_TOTALDROPPEDFILES@_DROPPEDFILE@_DROPPEDFILE$@_SHR@_SHL@_ROR@_ROL@" _
        + "_ADLER32@_CRC32@_MD5$@_DEFLATE$@_INFLATE$@_READBIT@_RESETBIT@_SETBIT@_TOGGLEBIT@$INCLUDEONCE@$ASSERTS@CONSOLE@_ASSERT@_CAPSLOCK@_NUMLOCK@_SCROLLLOCK@_TOGGLE@_CONSOLEFONT@_CONSOLECURSOR@_CONSOLEINPUT@_CINP@$NOPREFIX@$COLOR@$DEBUG@$EMBED@_EMBEDDED$@_ENVIRONCOUNT@$UNSTABLE@$MIDISOUNDFONT@" _
        + "_NOTIFYPOPUP@_MESSAGEBOX@_INPUTBOX$@_SELECTFOLDERDIALOG$@_COLORCHOOSERDIALOG@_OPENFILEDIALOG$@_SAVEFILEDIALOG$@_SAVEIMAGE@_FILES$@_FULLPATH$@" _
        + "_STATUSCODE@_SNDNEW@_SCALEDWIDTH@_SCALEDHEIGHT@_UFONTHEIGHT@_UPRINTWIDTH@_ULINESPACING@_UPRINTSTRING@_UCHARPOS@"

    SHARED qb64peKeyword() AS STRING

    DIM text AS STRING: text = UCASE$(_TRIM$(idName))

    ' Check for empty string and strings that are too big to be allowed
    IF LEN(text) = 0 OR LEN(text) > ID_NAME_LENGTH_MAX THEN EXIT FUNCTION

    ' Load the keyword table if it was not loaded
    IF LEN(qb64peKeyword(0)) = 0 THEN
        IF String_Tokenize(QB64PE_KEYWORDS, "@", STRING_EMPTY, FALSE, qb64peKeyword()) < 1 THEN
            ERROR ERROR_INTERNAL_ERROR
        END IF
    END IF

    ' Check if this is a QB64-PE keyword
    DIM i AS LONG: FOR i = 0 TO UBOUND(qb64peKeyword)
        ' Simple compare
        IF text = qb64peKeyword(i) THEN
            IsQB64Keyword = TRUE
            EXIT FUNCTION
        END IF

        ' Compare without leading `_` for $NOPREFIX cases
        IF ASC(qb64peKeyword(i), 1) = KEY_UNDERSCORE THEN
            IF RIGHT$(qb64peKeyword(i), LEN(qb64peKeyword(i)) - 1) = text THEN
                IsQB64Keyword = TRUE
                EXIT FUNCTION
            END IF
        END IF
    NEXT i
END FUNCTION


FUNCTION MakeQB64LegalId$ (idName AS STRING)
    CONST ID_CONFLICT_RESOLUTION_CHAR = "d"

    ' Remove leading and trailing spaces
    DIM text AS STRING: text = _TRIM$(idName)

    DIM startingLegal AS _BYTE

    DIM i AS _UNSIGNED LONG: FOR i = 1 TO LEN(text)
        SELECT CASE ASC(text, i)
            CASE KEY_0 TO KEY_9
                IF NOT startingLegal THEN ASC(text, i) = ASC(ID_CONFLICT_RESOLUTION_CHAR)

            CASE KEY_UPPER_A TO KEY_UPPER_Z, KEY_LOWER_A TO KEY_LOWER_Z
                startingLegal = TRUE

            CASE KEY_UNDERSCORE
                ' NOP

            CASE ELSE
                ASC(text, i) = KEY_UNDERSCORE ' replace with underscore
        END SELECT
    NEXT

    ' Check if the identifier begins with a single underscore
    IF ASC(text, 1) = KEY_UNDERSCORE THEN
        ' Check if there's only one underscore at the beginning
        IF LEN(text) > 1 AND ASC(text, 2) <> KEY_UNDERSCORE THEN
            text = "_" + text ' add another underscore to make it legal
        END IF
    END IF

    ' Check if the identifier ends with an underscore
    WHILE ASC(text, LEN(text)) = KEY_UNDERSCORE
        text = LEFT$(text, LEN(text) - 1) ' remove trailing underscore
    WEND

    ' Prefix with 'd' if it is a QB64-PE keyword
    WHILE IsQB64Keyword(text)
        text = ID_CONFLICT_RESOLUTION_CHAR + text
    WEND

    ' Check if the identifier exceeds maximum length
    IF LEN(text) > ID_NAME_LENGTH_MAX THEN
        text = LEFT$(text, ID_NAME_LENGTH_MAX) ' truncate the identifier to the maximum length
    END IF

    ' Return the sanitized identifier
    MakeQB64LegalId = text
END FUNCTION


FUNCTION MakeIdentifier$ (fileName AS STRING, size AS _UNSIGNED LONG)
    DIM sizeText AS STRING: sizeText = LTRIM$(STR$(size))
    DIM i AS _UNSIGNED LONG: i = ID_NAME_LENGTH_MAX - LEN(sizeText) - 6 ' ID_NAME_LENGTH_MAX - text size of file size - len("data_" + "_")
    DIM nameText AS STRING: nameText = MakeQB64LegalId(GetFileNameFromPathOrURL(fileName))
    IF LEN(nameText) > i THEN nameText = LEFT$(nameText, i) ' chop the name if everything is adding up to be more than LABEL_LENGTH_MAX chars
    MakeIdentifier = nameText + "_" + sizeText
END FUNCTION


SUB MakeData (buffer AS STRING, outputfileName AS STRING, ogSize AS _UNSIGNED LONG)
    CONST DATA_STATEMENT = "DATA "

    ' Open the output file
    DIM fh AS LONG: fh = FREEFILE
    OPEN outputfileName FOR OUTPUT AS fh

    PRINT #fh, "$INCLUDEONCE"
    PRINT #fh, STRING_EMPTY

    ' Write the DATA lable
    PRINT #fh, "data_"; MakeIdentifier(outputfileName, ogSize); ":"

    PRINT "Encoding data (this may take some time) ... ";
    buffer = Base64_Encode(buffer) ' we do not need the original buffer contents
    PRINT "done"

    ' Write the DATA header
    PRINT #fh, DATA_STATEMENT; LTRIM$(STR$(ogSize)); ","; LTRIM$(STR$(LEN(buffer))); ","; LTRIM$(STR$(LEN(buffer) <> ogSize))

    DIM srcSizeRem AS _UNSIGNED LONG: srcSizeRem = LEN(buffer) MOD appOption.charPerLine ' remainder
    DIM srcSizeMul AS _UNSIGNED LONG: srcSizeMul = LEN(buffer) - srcSizeRem ' exact multiple

    DIM i AS _UNSIGNED LONG: FOR i = 1 TO srcSizeMul STEP appOption.charPerLine
        PRINT #fh, DATA_STATEMENT; MID$(buffer, i, appOption.charPerLine) ' a worth line of DATA
    NEXT i

    IF srcSizeRem > 0 THEN
        PRINT #fh, DATA_STATEMENT; MID$(buffer, i, appOption.charPerLine) ' a worth line of DATA
    END IF

    CLOSE fh

    PRINT "Done"
END SUB


SUB MakeConst (buffer AS STRING, outputfileName AS STRING, ogSize AS _UNSIGNED LONG)
    CONST CONST_KEYWORD = "CONST "
    CONST ASSIGNMENT_STRING = " ="
    CONST LINE_CONTINUATION = " _"
    CONST LINE_CONTINUATION_MAX = 500

    ' Open the output file
    DIM fh AS LONG: fh = FREEFILE
    OPEN outputfileName FOR OUTPUT AS fh

    DIM id AS STRING: id = UCASE$(MakeIdentifier(outputfileName, ogSize))

    PRINT #fh, "$INCLUDEONCE"
    PRINT #fh, STRING_EMPTY

    PRINT #fh, CONST_KEYWORD; "SIZE_"; id; ASSIGNMENT_STRING; STR$(ogSize) + "~&" ' write the size const
    PRINT #fh, CONST_KEYWORD; "COMP_"; id; ASSIGNMENT_STRING; LTRIM$(STR$(LEN(buffer) <> ogSize)); "%%" ' write if file is compressed

    PRINT "Encoding data (this may take some time) ... ";
    buffer = Base64_Encode(buffer) ' we do not need the original buffer contents
    PRINT "done"

    PRINT #fh, CONST_KEYWORD; "DATA_"; id; ASSIGNMENT_STRING; LINE_CONTINUATION ' write the const name

    ' Adjust character per line to work around QB64 limit if needed
    IF LEN(buffer) \ appOption.charPerLine >= LINE_CONTINUATION_MAX - 2 THEN
        appOption.charPerLine = LEN(buffer) \ (LINE_CONTINUATION_MAX - 2)
        PRINT "Characters per data line auto-changed to"; appOption.charPerLine
    END IF

    DIM srcSizeRem AS _UNSIGNED LONG: srcSizeRem = LEN(buffer) MOD appOption.charPerLine ' remainder
    DIM srcSizeMul AS _UNSIGNED LONG: srcSizeMul = LEN(buffer) - srcSizeRem ' exact multiple

    DIM i AS _UNSIGNED LONG: FOR i = 1 TO srcSizeMul STEP appOption.charPerLine
        PRINT #fh, SPACE$(INDENT_SPACES); CHR$(KEY_QUOTATION_MARK); MID$(buffer, i, appOption.charPerLine); CHR$(KEY_QUOTATION_MARK);

        ' Now check if we need to write the line continuation combo for CONST string
        IF srcSizeRem > 0 OR i < srcSizeMul - appOption.charPerLine THEN
            PRINT #fh, " +"; LINE_CONTINUATION ' write a string concat op and then a line continuation
        ELSE
            PRINT #fh, STRING_EMPTY ' move to the next line
        END IF
    NEXT i

    IF srcSizeRem > 0 THEN
        PRINT #fh, SPACE$(INDENT_SPACES); CHR$(KEY_QUOTATION_MARK); MID$(buffer, i, appOption.charPerLine); CHR$(KEY_QUOTATION_MARK)
    END IF

    PRINT #fh, STRING_EMPTY ' put a newline (required for CONST!)

    CLOSE fh

    PRINT "Done"
END SUB


SUB MakeCArray (buffer AS STRING, outputfileName AS STRING, ogSize AS _UNSIGNED LONG)
    DIM f AS LONG: f = FREEFILE
    OPEN outputfileName FOR OUTPUT AS f

    DIM id AS STRING: id = MakeIdentifier(outputfileName, ogSize)

    PRINT #f, "#pragma once"
    PRINT #f, STRING_EMPTY
    PRINT #f, "#include <stdint.h>"
    PRINT #f, STRING_EMPTY
    PRINT #f, "SIZE_"; UCASE$(id); "()"; ogSize
    PRINT #f, "COMP_"; UCASE$(id); "() "; LTRIM$(STR$(LEN(buffer) <> ogSize))
    PRINT #f, "DATA_"; UCASE$(id); "() "; "((uintptr_t)(&"; id; "[0]))"
    PRINT #f, STRING_EMPTY
    PRINT #f, "const uint8_t * " + id + "[] = {"

    DIM xPos AS _UNSIGNED LONG: xPos = 1

    DIM i AS _UNSIGNED LONG: FOR i = 1 TO LEN(buffer)
        IF xPos = 1 THEN PRINT #f, SPACE$(INDENT_SPACES);

        DIM char AS STRING: char = LTRIM$(STR$(ASC(buffer, i)))
        PRINT #f, char;

        IF i < LEN(buffer) THEN PRINT #f, ",";

        xPos = xPos + LEN(char) + 1 ' +1 for comma

        IF xPos >= appOption.charPerLine THEN
            IF i < LEN(buffer) THEN PRINT #f, STRING_EMPTY
            xPos = 1
        END IF
    NEXT i

    PRINT #f, STRING_EMPTY
    PRINT #f, "};"

    CLOSE #f
END SUB
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' MODULE FILES
'-----------------------------------------------------------------------------------------------------------------------
'$INCLUDE:'include/ProgramArgs.bas'
'$INCLUDE:'include/StringOps.bas'
'$INCLUDE:'include/FileOps.bas'
'$INCLUDE:'include/Base64.bas'
'$INCLUDE:'include/Deflate.bas'
'-----------------------------------------------------------------------------------------------------------------------
'-----------------------------------------------------------------------------------------------------------------------
