unit CommonProc;

interface
uses CommonClasses ;

type
TColor = -$7FFFFFFF-1..$7FFFFFFF;

function getGrayColor(C:TColor; level:Integer):TColor ;
procedure unRGBOpenGL(C:TColor; var oglR,oglG,oglB:Single) ;
function AppDataPath():string ;

function getX(b:TBlock):Integer ;
function getY(b:TBlock):Integer ;
function getZ(b:TBlock):Integer ;
function isValueBetween(x,a,b:Integer):Boolean ;
function isBlockEqTexAndType(const b1:TBlock; const b2:TBlock):Boolean ;
function isZoneReadyForFillRot(const zone:TZone3I):Boolean ;

implementation
uses SysUtils, IOUtils ;

function AppDataPath():string ;
begin
  if Pos('Build3D.exe',ParamStr(0))<>0 then
    Result:=GetEnvironmentVariable('LOCALAPPDATA')+'\Build3D'
  else
    Result:=ExtractFileDir(ParamStr(0)) ;
end;

procedure unRGBOpenGL(C:TColor; var oglR,oglG,oglB:Single) ;
var r,g,b:byte ;
begin
  asm
    mov eax,c
    mov r,al
    mov g,ah
    shr eax,8
    mov b,ah
  end ;
  oglR:=r/255 ;
  oglG:=g/255 ;
  oglB:=b/255 ;
end ;

function getGrayColor(C:TColor; level:Integer):TColor ;
var r,g,b,avg:byte ;
begin
  asm
    mov eax,c
    mov r,al
    mov g,ah
    shr eax,8
    mov b,ah
  end ;
  avg:=(r+g+b) div 3 ;
  avg:=Round(avg*(level/100)) ;
  asm
    xor eax,eax
    mov ah,avg
    shl eax,8
    mov al,avg
    mov ah,avg
    mov @Result,eax
  end ;
end;

function getX(b:TBlock):Integer ;
begin  Result:=b.x ; end;

function getY(b:TBlock):Integer ;
begin  Result:=b.y ; end;

function getZ(b:TBlock):Integer ;
begin  Result:=b.z ; end;

function isValueBetween(x,a,b:Integer):Boolean ;
begin
  Result:=((a<=x)and(x<=b)) or ((b<=x)and(x<=a)) ;
end;

function isBlockEqTexAndType(const b1:TBlock; const b2:TBlock):Boolean ;
begin
  Result:=(b1.texcode=b2.texcode)and(b1.bt=b2.bt) ;
end;

function isZoneReadyForFillRot(const zone:TZone3I):Boolean ;
begin
  Result:=False ;
  if (zone.dX()=1)and(zone.dY()>1)and(zone.dZ()>1) then Exit(True) ;
  if (zone.dY()=1)and(zone.dX()>1)and(zone.dZ()>1) then Exit(True) ;
  if (zone.dZ()=1)and(zone.dX()>1)and(zone.dY()>1) then Exit(True) ;
end;

end.
