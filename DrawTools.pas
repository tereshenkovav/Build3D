unit DrawTools;

interface
uses Generics.Collections, Generics.Defaults, rtti,
  Model, CommonClasses ;

type
  TDrawTool = class(TObject)
  private
    class var cleartool:TDrawTool ;
    class var toollist:TList<TDrawTool> ;
    class procedure setClearTool(drawtool:TDrawTool) ;
  public
    tmpTexName:string ;
    tmpCubeSize:Integer ;
    tmpParX:Integer ;
    tmpParY:Integer ;
    tmpParZ:Integer ;
    tmpBlockType:TBlockType ;
    tmpSetToEdge:Boolean ;
    function Caption():string ; virtual ; abstract ;
    function Order():Integer ; virtual ; abstract ;
    function Apply(model:TModel; block:TBlock; dir:TBlockDir):Boolean ; virtual ;
    function PregetNewBlocks(block:TBlock; dir:TBlockDir):TList<TBlock> ; virtual ;
    class function getToolList():TList<TDrawTool> ;
    class function getClearTool():TDrawTool ;
  end;

// Не удалять! Это нужно для rtti
procedure ForceRef(C: TClass);

implementation
uses SysUtils, DrawToolClear ;

// Не удалять! Это нужно для rtti
procedure ForceRef(C: TClass);
begin
end;

{ TDrawTool }

function TDrawTool.Apply(model: TModel; block:TBlock; dir: TBlockDir): Boolean;
begin
  model.PushBlocks() ;
end;

class function TDrawTool.getClearTool: TDrawTool;
begin
  Result:=cleartool ;
end;

class function TDrawTool.getToolList: TList<TDrawTool>;
var c : TRttiContext;
    a: TArray<TRttiType>;
    t: TRttiType ;
    drawtool:TDrawTool ;
begin
  if toollist=nil then begin
    toollist:=TList<TDrawTool>.Create() ;
    c := TRttiContext.Create;
    a := c.GetTypes;
    for t in a do
      if t.BaseType<>nil then
        if t.BaseType.Name=TDrawTool.ClassName then begin
           drawtool:=TDrawTool(t.GetMethod('Create').Invoke(
             t.AsInstance.MetaclassType,[]).AsObject) ;
           toollist.Add(drawtool) ;
           if drawtool is TDrawToolClear then SetClearTool(drawtool) ;
        end;
    c.Free ;

    toollist.Sort(TComparer<TDrawTool>.Construct(
      function (const L, R: TDrawTool): integer
      begin
        Result := L.Order() - R.Order;
      end
      )) ;
  end;
  Result:=toollist ;
end;

function TDrawTool.PregetNewBlocks(block: TBlock;
  dir: TBlockDir): TList<TBlock>;
begin
  Result:=TList<TBlock>.Create() ;
end;

class procedure TDrawTool.setClearTool(drawtool: TDrawTool);
begin
  cleartool:=drawtool ;
end;

end.
