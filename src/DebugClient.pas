unit DebugClient;

interface

function WriteToServer(procname, msg: string):Boolean;

implementation
uses Windows ;

type
  TPipeMessage = record
    appname:string[40] ;
    procname:string[40] ;
    msg:string[40] ;
  end;

function OnlyFile (PathAndFile:string):string ;
var n,Poz,Max:word ;
begin
   Max:=length(PathAndFile) ;
   Poz:=1 ; // �� ������, ���� PathAndFile �� �������� ����
   for n:=1 to Max do
     if PathAndFile[n]='\' then Poz:=n+1 ;

   OnlyFile:=copy(PathAndFile,Poz,Max-Poz+1) ;
end ;

function WriteToServer(procname, msg: string):Boolean;
var
  hPipe: THandle;
  bytesWritten: DWORD;
  pm:TPipeMessage ;
begin
  pm.appname:=OnlyFile(ParamStr(0)) ;
  pm.procname:=procname ;
  pm.msg:=msg ;

  try

  hPipe := CreateFile('\\.\PIPE\DebugServerPipe',
   GENERIC_WRITE, //������ ������
   FILE_SHARE_READ or // ������������ ������\�������
   FILE_SHARE_WRITE,
   nil, //��������� ������������
   OPEN_EXISTING,   // ����� ������ ���� ������
   0, 0);

  if hPipe = INVALID_HANDLE_VALUE then Exit(False);
  Result:=WriteFile(hPipe, pm, SizeOf(TPipeMessage), bytesWritten,nil) ;

  finally
    DisconnectNamedPipe(hpipe);
  end;
end;


end.
