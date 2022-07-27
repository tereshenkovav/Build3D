unit ModelMap;

interface
uses Generics.Collections,
  CommonClasses ;

type
  TModelMap = class
  private
    cube:array of array of array of Integer ;
    blocks:TList<TBlock> ;
    xs,ys,zs:Integer;
    xl,yl,zl:Integer;
    isempty:Boolean ;
  public
    constructor Create(list:TList<TBlock>) ;
    destructor Destroy ; override ;
    function isBlockAt(x,y,z:Integer):Boolean ;
    function isFullBlockAt(x,y,z:Integer):Boolean ;
    function getBlockAt(x,y,z:Integer; out block:TBlock):Boolean ;
    function getCountBlocksAround6(b:TBlock):Integer ; overload ;
    function getMaxTexNameAround6(x,y,z:Integer): string;
    function getCountBlocksAround6(x,y,z:Integer):Integer ; overload ;
    function getCountFullBlocksAround6(b:TBlock):Integer ; overload ;
    function getCountFullBlocksAround6(x,y,z:Integer):Integer ; overload ;
    function getCountBlocksAround26(x,y,z:Integer): Integer;
  end;

implementation
uses SysUtils,
  CommonProc, BlockListHelper, DebugClient ;

{ TModelMap }

constructor TModelMap.Create(list: TList<TBlock>);
var xf,yf,zf,x,y,z:Integer;
    i:Integer ;
begin
  blocks:=list ;
  isempty:=list.Count=0 ;

  if isempty then Exit ;  

  xs:=list.Min(getX) ;
  ys:=list.Min(getY) ;
  zs:=list.Min(getZ) ;
  xf:=list.Max(getX) ;
  yf:=list.Max(getY) ;
  zf:=list.Max(getZ) ;

  xl:=xf-xs+1 ;
  yl:=yf-ys+1 ;
  zl:=zf-zs+1 ;

  SetLength(cube,xl,yl,zl) ;
  for x := 0 to xl-1 do
    for y := 0 to yl-1 do
      for z := 0 to zl-1 do
        cube[x,y,z]:=-1 ;

  for i := 0 to list.Count-1 do
    cube[list[i].x-xs,list[i].y-ys,list[i].z-zs]:=i ;
end;

destructor TModelMap.Destroy;
begin
  SetLength(cube,0,0,0) ;
  inherited Destroy;
end;

function TModelMap.isBlockAt(x,y,z:Integer):Boolean ;
begin
  if isempty then Exit(False) ;

  if x-xs<0 then Exit(False) ;
  if x-xs>=xl then Exit(False) ;
  if y-ys<0 then Exit(False) ;
  if y-ys>=yl then Exit(False) ;
  if z-zs<0 then Exit(False) ;
  if z-zs>=zl then Exit(False) ;

  //DebugClient.WriteToServer('TModelMap.isBlockAt',
  //  Format('test[%d %d %d]',[x-xs,y-ys,z-zs])) ;
  Result:=cube[x-xs,y-ys,z-zs]>=0 ;
end;

function TModelMap.isFullBlockAt(x,y,z:Integer):Boolean ;
begin
  if isBlockAt(x,y,z) then
    Result:=blocks[cube[x-xs,y-ys,z-zs]].bt=btFull
  else
    Result:=False ;
end;

function TModelMap.getBlockAt(x,y,z:Integer; out block:TBlock):Boolean ;
begin
  if isempty then Exit(False) ;

  if not isBlockAt(x,y,z) then Result:=False else begin
    block:=blocks[cube[x-xs,y-ys,z-zs]] ;
    Result:=True ;
  end;
end;

function TModelMap.getCountBlocksAround26(x,y,z:Integer): Integer;
var i,j,k:Integer ;
begin
  Result:=0 ;
  for i := -1 to 1 do
    for j := -1 to 1 do
      for k := -1 to 1 do
        if not((i=0)and(j=0)and(k=0)) then
          if isBlockAt(x+i,y+j,z+k) then Inc(Result) ;
end;

function TModelMap.getCountBlocksAround6(x,y,z:Integer): Integer;
begin
  Result:=0 ;
  if isBlockAt(x-1,y,z) then Inc(Result) ;
  if isBlockAt(x+1,y,z) then Inc(Result) ;
  if isBlockAt(x,y-1,z) then Inc(Result) ;
  if isBlockAt(x,y+1,z) then Inc(Result) ;
  if isBlockAt(x,y,z-1) then Inc(Result) ;
  if isBlockAt(x,y,z+1) then Inc(Result) ;
end;

function TModelMap.getMaxTexNameAround6(x,y,z:Integer): string;
var texs:TDictionary<string,integer> ;

procedure IncTex(texcode:string) ;
begin
  if texs.ContainsKey(texcode) then
    texs[texcode]:=texs[texcode]+1
  else
    texs.Add(texcode,1);
end;

var key:string ;
    max:Integer ;
begin
  texs:=TDictionary<string,integer>.Create() ;
  if isBlockAt(x-1,y,z) then IncTex(blocks[cube[x-xs-1,y-ys,z-zs]].texcode);
  if isBlockAt(x+1,y,z) then IncTex(blocks[cube[x-xs+1,y-ys,z-zs]].texcode);
  if isBlockAt(x,y-1,z) then IncTex(blocks[cube[x-xs,y-ys-1,z-zs]].texcode);
  if isBlockAt(x,y+1,z) then IncTex(blocks[cube[x-xs,y-ys+1,z-zs]].texcode);
  if isBlockAt(x,y,z-1) then IncTex(blocks[cube[x-xs,y-ys,z-zs-1]].texcode);
  if isBlockAt(x,y,z+1) then IncTex(blocks[cube[x-xs,y-ys,z-zs+1]].texcode);

  max:=0 ;
  Result:='' ;
  for key in texs.Keys do
    if texs[key]>max then begin
      Result:=key ;
      max:=texs[key] ;
    end ;

  texs.Free ;
end;

function TModelMap.getCountFullBlocksAround6(x,y,z:Integer): Integer;
begin
  Result:=0 ;
  if isFullBlockAt(x-1,y,z) then Inc(Result) ;
  if isFullBlockAt(x+1,y,z) then Inc(Result) ;
  if isFullBlockAt(x,y-1,z) then Inc(Result) ;
  if isFullBlockAt(x,y+1,z) then Inc(Result) ;
  if isFullBlockAt(x,y,z-1) then Inc(Result) ;
  if isFullBlockAt(x,y,z+1) then Inc(Result) ;
end;

function TModelMap.getCountBlocksAround6(b: TBlock): Integer;
begin
  Result:=getCountBlocksAround6(b.x,b.y,b.z) ;
end;

function TModelMap.getCountFullBlocksAround6(b: TBlock): Integer;
begin
  Result:=getCountFullBlocksAround6(b.x,b.y,b.z) ;
end;

end.
