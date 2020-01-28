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
    function getBlockAt(x,y,z:Integer; out block:TBlock):Boolean ;
    function getCountBlocksAround6(b:TBlock):Integer ;
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

function TModelMap.getCountBlocksAround6(b: TBlock): Integer;
begin
  Result:=0 ;
  if isBlockAt(b.x-1,b.y,b.z) then Inc(Result) ;
  if isBlockAt(b.x+1,b.y,b.z) then Inc(Result) ;
  if isBlockAt(b.x,b.y-1,b.z) then Inc(Result) ;
  if isBlockAt(b.x,b.y+1,b.z) then Inc(Result) ;
  if isBlockAt(b.x,b.y,b.z-1) then Inc(Result) ;
  if isBlockAt(b.x,b.y,b.z+1) then Inc(Result) ;
end;

end.
