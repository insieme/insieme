# Microsoft Visual C++ Generated NMAKE File, Format Version 2.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

!IF "$(CFG)" == ""
CFG=Win32 Debug
!MESSAGE No configuration specified.  Defaulting to Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Win32 Release" && "$(CFG)" != "Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "tmndec.mak" CFG="Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

################################################################################
# Begin Project
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "WinRel"
# PROP BASE Intermediate_Dir "WinRel"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "WinRel"
# PROP Intermediate_Dir "WinRel"
OUTDIR=.\WinRel
INTDIR=.\WinRel

ALL : $(OUTDIR)/tmndec.exe $(OUTDIR)/tmndec.bsc

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

# ADD BASE CPP /nologo /ML /W3 /GX /YX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /FR /c
# ADD CPP /nologo /ML /W3 /GX /YX /O2 /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D "WINDOWS" /D "USE_TIME" /FR /c
CPP_PROJ=/nologo /ML /W3 /GX /YX /O2 /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "WINDOWS" /D "USE_TIME" /FR$(INTDIR)/ /Fp$(OUTDIR)/"tmndec.pch" /Fo$(INTDIR)/\
 /c 
CPP_OBJS=.\WinRel/
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"tmndec.bsc" 
BSC32_SBRS= \
	$(INTDIR)/getpic.sbr \
	$(INTDIR)/gethdr.sbr \
	$(INTDIR)/store.sbr \
	$(INTDIR)/yuvrgb24.sbr \
	$(INTDIR)/recon.sbr \
	$(INTDIR)/display.sbr \
	$(INTDIR)/dither.sbr \
	$(INTDIR)/win.sbr \
	$(INTDIR)/getvlc.sbr \
	$(INTDIR)/idct.sbr \
	$(INTDIR)/yuv2rgb.sbr \
	$(INTDIR)/getblk.sbr \
	$(INTDIR)/tmndec.sbr \
	$(INTDIR)/idctref.sbr \
	$(INTDIR)/getbits.sbr \
	$(INTDIR)/sac.sbr

$(OUTDIR)/tmndec.bsc : $(OUTDIR)  $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /NOLOGO /SUBSYSTEM:console /MACHINE:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib vfw32.lib winmm.lib /NOLOGO /SUBSYSTEM:console /MACHINE:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib vfw32.lib winmm.lib\
 /NOLOGO /SUBSYSTEM:console /INCREMENTAL:no /PDB:$(OUTDIR)/"tmndec.pdb"\
 /MACHINE:I386 /OUT:$(OUTDIR)/"tmndec.exe" 
DEF_FILE=
LINK32_OBJS= \
	$(INTDIR)/getpic.obj \
	$(INTDIR)/gethdr.obj \
	$(INTDIR)/store.obj \
	$(INTDIR)/yuvrgb24.obj \
	$(INTDIR)/recon.obj \
	$(INTDIR)/display.obj \
	$(INTDIR)/dither.obj \
	$(INTDIR)/win.obj \
	$(INTDIR)/getvlc.obj \
	$(INTDIR)/idct.obj \
	$(INTDIR)/yuv2rgb.obj \
	$(INTDIR)/getblk.obj \
	$(INTDIR)/tmndec.obj \
	$(INTDIR)/idctref.obj \
	$(INTDIR)/getbits.obj \
	$(INTDIR)/sac.obj

$(OUTDIR)/tmndec.exe : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "WinDebug"
# PROP BASE Intermediate_Dir "WinDebug"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "WinDebug"
# PROP Intermediate_Dir "WinDebug"
OUTDIR=.\WinDebug
INTDIR=.\WinDebug

ALL : $(OUTDIR)/tmndec.exe $(OUTDIR)/tmndec.bsc

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

# ADD BASE CPP /nologo /ML /W3 /GX /Zi /YX /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /FR /c
# ADD CPP /nologo /ML /W3 /GX /Zi /YX /Od /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D "WINDOWS" /D "USE_TIME" /FR /c
CPP_PROJ=/nologo /ML /W3 /GX /Zi /YX /Od /D "_DEBUG" /D "WIN32" /D "_CONSOLE"\
 /D "WINDOWS" /D "USE_TIME" /FR$(INTDIR)/ /Fp$(OUTDIR)/"tmndec.pch"\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"tmndec.pdb" /c 
CPP_OBJS=.\WinDebug/
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"tmndec.bsc" 
BSC32_SBRS= \
	$(INTDIR)/getpic.sbr \
	$(INTDIR)/gethdr.sbr \
	$(INTDIR)/store.sbr \
	$(INTDIR)/yuvrgb24.sbr \
	$(INTDIR)/recon.sbr \
	$(INTDIR)/display.sbr \
	$(INTDIR)/dither.sbr \
	$(INTDIR)/win.sbr \
	$(INTDIR)/getvlc.sbr \
	$(INTDIR)/idct.sbr \
	$(INTDIR)/yuv2rgb.sbr \
	$(INTDIR)/getblk.sbr \
	$(INTDIR)/tmndec.sbr \
	$(INTDIR)/idctref.sbr \
	$(INTDIR)/getbits.sbr \
	$(INTDIR)/sac.sbr

$(OUTDIR)/tmndec.bsc : $(OUTDIR)  $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /NOLOGO /SUBSYSTEM:console /DEBUG /MACHINE:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib vfw32.lib winmm.lib /NOLOGO /SUBSYSTEM:console /DEBUG /MACHINE:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib vfw32.lib winmm.lib\
 /NOLOGO /SUBSYSTEM:console /INCREMENTAL:yes /PDB:$(OUTDIR)/"tmndec.pdb" /DEBUG\
 /MACHINE:I386 /OUT:$(OUTDIR)/"tmndec.exe" 
DEF_FILE=
LINK32_OBJS= \
	$(INTDIR)/getpic.obj \
	$(INTDIR)/gethdr.obj \
	$(INTDIR)/store.obj \
	$(INTDIR)/yuvrgb24.obj \
	$(INTDIR)/recon.obj \
	$(INTDIR)/display.obj \
	$(INTDIR)/dither.obj \
	$(INTDIR)/win.obj \
	$(INTDIR)/getvlc.obj \
	$(INTDIR)/idct.obj \
	$(INTDIR)/yuv2rgb.obj \
	$(INTDIR)/getblk.obj \
	$(INTDIR)/tmndec.obj \
	$(INTDIR)/idctref.obj \
	$(INTDIR)/getbits.obj \
	$(INTDIR)/sac.obj

$(OUTDIR)/tmndec.exe : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.c{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

################################################################################
# Begin Group "Source Files"

################################################################################
# Begin Source File

SOURCE=.\getpic.c

$(INTDIR)/getpic.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\gethdr.c

$(INTDIR)/gethdr.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\store.c

$(INTDIR)/store.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\yuvrgb24.c

$(INTDIR)/yuvrgb24.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\recon.c

$(INTDIR)/recon.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\display.c

$(INTDIR)/display.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dither.c

$(INTDIR)/dither.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\win.c

$(INTDIR)/win.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\getvlc.c

$(INTDIR)/getvlc.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\idct.c

$(INTDIR)/idct.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\yuv2rgb.c

$(INTDIR)/yuv2rgb.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\getblk.c

$(INTDIR)/getblk.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\tmndec.c

$(INTDIR)/tmndec.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\idctref.c

$(INTDIR)/idctref.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\getbits.c

$(INTDIR)/getbits.obj :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\sac.c

$(INTDIR)/sac.obj :  $(SOURCE)  $(INTDIR)

# End Source File
# End Group
# End Project
################################################################################
