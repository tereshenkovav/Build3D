Unicode True
RequestExecutionLevel admin
SetCompressor /SOLID zlib
LoadLanguageFile "${NSISDIR}\Contrib\Language files\Russian.nlf"
AutoCloseWindow true
Icon Build3D.ico
XPStyle on

!include "FileFunc.nsh"
!insertmacro GetTime

!define TEMP1 $R0 ;Temp variable

ReserveFile /plugin InstallOptions.dll
ReserveFile "runapp.ini"

; �������������� ����
!ifdef updatemode
OutFile "M:\Build3D-1.1.0.2-Win32-update.exe"
!else
OutFile "M:\Build3D-1.1.0.2-Win32.exe"
!endif

var is_update

Page directory
!ifndef updatemode
Page components
!endif
Page instfiles
Page custom SetRunApp ValidateRunApp ": �������" 

UninstPage uninstConfirm
UninstPage instfiles

Name "���������� ��������"

; ��������

Function .onInit

  InitPluginsDir
  File /oname=$PLUGINSDIR\runapp.ini "runapp.ini"

  StrCpy $INSTDIR $PROGRAMFILES\Build3D

  IfFileExists $INSTDIR\Build3D.exe +3
  StrCpy $is_update "0"
  Goto +2
  StrCpy $is_update "1"

FunctionEnd

Function .onInstSuccess
  StrCmp $is_update "1" SkipAll

  ReadINIStr ${TEMP1} "$PLUGINSDIR\runapp.ini" "Field 1" "State"
  StrCmp ${TEMP1} "0" SkipDesktop

  SetOutPath $INSTDIR
  CreateShortCut "$DESKTOP\���������� ��������.lnk" "$INSTDIR\Build3D.exe" "" "$INSTDIR\Build3D.ico" 

SkipDesktop:

  ReadINIStr ${TEMP1} "$PLUGINSDIR\runapp.ini" "Field 2" "State"
  StrCmp ${TEMP1} "0" SkipRun

  Exec $INSTDIR\Build3D.exe

  SkipRun:
  SkipAll:

FunctionEnd

Function un.onUninstSuccess
  MessageBox MB_OK "���������� �������� ������� ������!"
FunctionEnd

Function un.onUninstFailed
  MessageBox MB_OK "������ �������� ����������� ���������!"
FunctionEnd

Function .onInstFailed
  MessageBox MB_OK "������ ��������� ����������� ���������!"
FunctionEnd

Section "������� ����������"
  SectionIn RO

  StrCmp $is_update "0" SkipSleep
  Sleep 3000
  SkipSleep:

  SetOutPath $INSTDIR
  File ..\bin\Build3D.exe
  File Build3D.ico

  StrCmp $is_update "1" Skip2

  CreateDirectory $LOCALAPPDATA\Build3D
  CreateDirectory $LOCALAPPDATA\Build3D\textures
  
  WriteUninstaller $INSTDIR\Uninst.exe

  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Build3D" \
                 "DisplayName" "���������� ��������"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Build3D" \
                 "UninstallString" "$\"$INSTDIR\Uninst.exe$\""
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Build3D" \
                 "EstimatedSize" 0x00001770
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Build3D" \
                 "DisplayIcon" $INSTDIR\Build3D.exe

  ${GetTime} "" "L" $0 $1 $2 $3 $4 $5 $6
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Build3D" \
                 "InstallDate"  "$2$1$0"

  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Build3D" \
                 "Publisher"  "���������� �.�."
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Build3D" \
                 "DisplayVersion"  "1.1.0.2"

  ; �������� �������
  ;SetOutPath $INSTDIR
  ;CreateShortCut "���������� ��������.lnk" "$INSTDIR\Build3D.exe" "" "$INSTDIR\Build3D.ico" 
  ;CopyFiles "$INSTDIR\���������� ��������.lnk" $DESKTOP
  ;Delete "$INSTDIR\*.lnk"

  SetOutPath $INSTDIR
  CreateDirectory "$SMPROGRAMS\���������� ��������"
  CreateShortCut "$SMPROGRAMS\���������� ��������\���������� ��������.lnk" "$INSTDIR\Build3D.exe" "" "$INSTDIR\Build3D.ico" 

Skip2:

SectionEnd

Section "����� �������"
  SetOutPath $LOCALAPPDATA\Build3D\textures
  File ..\bin\textures\*
SectionEnd

Section "Uninstall"
  RMDir /r $INSTDIR
  RMDir /r $LOCALAPPDATA\Build3D
  RMDir /r "$SMPROGRAMS\���������� ��������"
  Delete "$DESKTOP\���������� ��������.lnk"

  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Build3D"
SectionEnd

Function SetRunApp

  Push ${TEMP1}

  InstallOptions::dialog "$PLUGINSDIR\runapp.ini"
    Pop ${TEMP1}
  
  Pop ${TEMP1}

FunctionEnd

Function ValidateRunApp

FunctionEnd
