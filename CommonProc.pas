unit CommonProc;

interface
uses Graphics, CommonClasses ;

function getGrayColor(C:TColor; level:Integer):TColor ;
procedure unRGBOpenGL(C:TColor; var oglR,oglG,oglB:Single) ;
function AppPath():string ;

function getX(b:TBlock):Integer ;
function getY(b:TBlock):Integer ;
function getZ(b:TBlock):Integer ;

implementation

function AppPath():string ;
var n,Poz,Max:word ;
begin
   Max:=length(ParamStr(0)) ;
   Poz:=1 ; // На случай, если PathAndFile не содержит путь
   for n:=1 to Max do
     if ParamStr(0)[n]='\' then Poz:=n+1 ;

   Result:=copy(ParamStr(0),1,Poz-1) ;
end ;

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

end.
