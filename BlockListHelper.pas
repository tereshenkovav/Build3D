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
    procedure fillHiddenGIDs(dic:TDictionary<String,Boolean>) ;
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

  dic.Clear() ;
  for b in self do
    if mm.getCountFullBlocksAround6(b)=6 then begin
      if not dic.ContainsKey(b.gid) then dic.Add(b.gid,True) ;
    end;

  mm.Free ;
end ;

end.
