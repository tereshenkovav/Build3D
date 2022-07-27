unit DrawToolClear;

interface
uses DrawTools, Model, CommonClasses ;

type
  TDrawToolClear = class(TDrawTool)
  public
     function Caption():string; override ;
     function Order():Integer ; override ;
     function Apply(model:TModel; block:TBlock; dir:TBlockDir):Boolean ; override ;
  end;

implementation

{ TDrawToolClear }

function TDrawToolClear.Apply(model: TModel; block:TBlock;
  dir: TBlockDir): Boolean;
begin
  inherited Apply(model,block,dir) ;
  model.DeleteBlock(block.x,block.y,block.z);
  model.RebuildSkippedBlocks() ;
end;

function TDrawToolClear.Caption: string;
begin
  Result:='Стёрка' ;
end;

function TDrawToolClear.Order: Integer;
begin
  Result:=999 ;
end;

initialization
  ForceRef(TDrawToolClear) ;

end.
