unit BlockListHelper;

interface
uses Generics.Collections,
  Model, CommonClasses ;

type
  TBlockListHelper = class helper for TList<TBlock>
  strict private type
    TFindBlockRef = reference to function(block:TBlock):Boolean ;
    TSizeBlockRef = reference to function(block:TBlock):Integer ;
  public
    function Find(Lambda: TFindBlockRef; out block:TBlock):Boolean;
    function Min(Lambda: TSizeBlockRef):Integer ;
    function Max(Lambda: TSizeBlockRef):Integer ;
    procedure fillHiddenGIDs(dic:TDictionary<String,Boolean>) overload ;
    procedure fillHiddenGIDs(dic:TDictionary<String,Boolean>;
      planes:TDictionary<String,TBlockDirSet>) overload ;
  end;

implementation
uses SysUtils, CommonProc, ModelMap ;

function TBlockListHelper.Find(Lambda: TFindBlockRef; out block:TBlock):Boolean;
var b:TBlock ;
begin
  Result:=False ;
  for b in self do
    if Lambda(b) then begin
      block:=b ;
      Result:=True ;
      break ;
    end;
end;

function TBlockListHelper.Max(Lambda: TSizeBlockRef):Integer;
var b:TBlock ;
    v:Integer ;
begin
  if self.Count=0 then raise Exception.Create('Empty list');

  Result:=Lambda(self[0]) ;
  for b in self do begin
    v:=Lambda(b) ;
    if v>Result then Result:=v ;
  end;
end;

function TBlockListHelper.Min(Lambda: TSizeBlockRef):Integer;
var b:TBlock ;
    v:Integer ;
begin
  if self.Count=0 then raise Exception.Create('Empty list');

  Result:=Lambda(self[0]) ;
  for b in self do begin
    v:=Lambda(b) ;
    if v<Result then Result:=v ;
  end;
end;

procedure TBlockListHelper.fillHiddenGIDs(dic:TDictionary<String,Boolean>) ;
var b:TBlock ;
    mm:TModelMap ;
begin
  mm:=TModelMap.Create(self) ;

  // Дубликат
  dic.Clear() ;
  for b in self do
    if mm.getCountFullBlocksAround6(b)=6 then begin
      if not dic.ContainsKey(b.gid) then dic.Add(b.gid,True) ;
    end;

  mm.Free ;
end ;

procedure TBlockListHelper.fillHiddenGIDs(dic:TDictionary<String,Boolean>;
  planes:TDictionary<String,TBlockDirSet>) ;
var b,bn:TBlock ;
    mm:TModelMap ;
    blockdirset:TBlockDirSet ;
begin
  mm:=TModelMap.Create(self) ;

  // Дубликат
  dic.Clear() ;
  planes.Clear() ;
  for b in self do
    if mm.getCountFullBlocksAround6(b)=6 then begin
      if not dic.ContainsKey(b.gid) then dic.Add(b.gid,True) ;
    end
    else begin
      blockdirset:=[] ;
      if mm.getBlockAt(b.x+1,b.y,b.z,bn) then
        if isBlockEqTexAndType(b,bn) then Include(blockdirset,dirXgr) ;
      if mm.getBlockAt(b.x-1,b.y,b.z,bn) then
        if isBlockEqTexAndType(b,bn) then Include(blockdirset,dirXle) ;
      if mm.getBlockAt(b.x,b.y+1,b.z,bn) then
        if isBlockEqTexAndType(b,bn) then Include(blockdirset,dirYgr) ;
      if mm.getBlockAt(b.x,b.y-1,b.z,bn) then
        if isBlockEqTexAndType(b,bn) then Include(blockdirset,dirYle) ;
      if mm.getBlockAt(b.x,b.y,b.z+1,bn) then
        if isBlockEqTexAndType(b,bn) then Include(blockdirset,dirZgr) ;
      if mm.getBlockAt(b.x,b.y,b.z-1,bn) then
        if isBlockEqTexAndType(b,bn) then Include(blockdirset,dirZle) ;
      if not planes.ContainsKey(b.gid) then planes.Add(b.gid,blockdirset) ;
    end;

  mm.Free ;
end ;

end.
