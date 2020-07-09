unit DrawToolCube;

interface
uses Model, DrawTools, CommonClasses, Generics.Collections, Generics.Defaults ;

type
  TDrawToolCube = class(TDrawTool)
  public
     function Caption():string; override ;
     function Order():Integer ; override ;
     function Apply(model:TModel; block:TBlock; dir:TBlockDir):Boolean ; override ;
     function PregetNewBlocks(block:TBlock; dir:TBlockDir):TList<TBlock> ; override ;
  end;

implementation

{ TDrawToolCube }

function TDrawToolCube.Apply(model: TModel; block: TBlock;
  dir: TBlockDir): Boolean;
var x,y,z:Integer ;
begin
  inherited Apply(model,block,dir) ;
  x:=block.x ; y:=block.y ; z:=block.z ;
  updateXYZByDir(dir,x,y,z) ;
  model.AddTypedBlock(x,y,z,tmpTexName,tmpBlockType) ;
  model.RebuildSkippedBlocks() ;
end;

function TDrawToolCube.Caption: string;
begin
  Result:='ќдин куб' ;
end;

function TDrawToolCube.Order: Integer;
begin
  Result:=1 ;
end;

function TDrawToolCube.PregetNewBlocks(block: TBlock;
  dir: TBlockDir): TList<TBlock>;
var x,y,z:Integer ;
    b:TBlock ;
begin
  Result:=inherited PregetNewBlocks(block,dir) ;

  x:=block.x ; y:=block.y ; z:=block.z ;
  updateXYZByDir(dir,x,y,z) ;

  Result.Add(TModel.newBlock(x,y,z,tmpTexName,tmpBlockType)) ;
end;

initialization
  ForceRef(TDrawToolCube) ;

end.
