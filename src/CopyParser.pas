unit CopyParser;

interface
uses CommonClasses ;

type
  TCopyParser = class
  private
    isok:Boolean ;
    target:TPoint3I ;
    mirrors:TAxisSet ;
  public
    constructor Create(str:string) ;
    function isCommandOk():Boolean ;
    function getTarget():TPoint3I ;
    function getMirrors():TAxisSet ;
  end;

implementation
uses Classes,SysUtils,RegularExpressions ;

{ TCopyParser }

constructor TCopyParser.Create(str: string);
var
  r:TRegEx ;
  m:TMatchCollection ;
begin
  isok:=False ;
  mirrors:=[] ;
  r:=TRegEx.Create('([+-]?\d+)\x20+([+-]?\d+)\x20+([+-]?\d+)');
  m := r.Matches(str.Trim());
  if m.Count=1 then begin
    target.x:=StrToInt(m[0].Groups[1].Value) ;
    target.y:=StrToInt(m[0].Groups[2].Value) ;
    target.z:=StrToInt(m[0].Groups[3].Value) ;
    if m[0].Groups[3].Value.IndexOf('x')<>-1 then mirrors:=mirrors+[AxisX] ;
    if m[0].Groups[3].Value.IndexOf('y')<>-1 then mirrors:=mirrors+[AxisY] ;
    if m[0].Groups[3].Value.IndexOf('z')<>-1 then mirrors:=mirrors+[AxisZ] ;
    isok:=True ;
  end ;

  r:=TRegEx.Create('.*?(\x20+[xyz]+)');
  m := r.Matches(str.Trim());
  if m.Count=1 then begin
    if m[0].Groups[1].Value.IndexOf('x')<>-1 then mirrors:=mirrors+[AxisX] ;
    if m[0].Groups[1].Value.IndexOf('y')<>-1 then mirrors:=mirrors+[AxisY] ;
    if m[0].Groups[1].Value.IndexOf('z')<>-1 then mirrors:=mirrors+[AxisZ] ;
  end ;

end;

function TCopyParser.getMirrors: TAxisSet;
begin
  Result:=mirrors ;
end;

function TCopyParser.getTarget: TPoint3I;
begin
  Result:=target ;
end;

function TCopyParser.isCommandOk: Boolean;
begin
  Result:=isok ;
end;

end.
