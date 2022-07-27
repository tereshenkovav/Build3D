unit DrawToolCubeCube;

interface
uses DrawTools, Model, CommonClasses, Generics.Collections, Generics.Defaults ;

type
  TDrawToolCubeCube = class(TDrawTool)
  private
     n1:Integer ;
     n2:Integer ;
     function isBlockInZone(const b:TBlock; const x,y,z:Integer):Boolean ;
  public
     function Caption():string; override ;
     function Order():Integer ; override ;
     function Apply(model: TModel; block: TBlock;
       dir: TBlockDir): Boolean; override ;
     function PregetNewBlocks(block:TBlock; dir:TBlockDir):TList<TBlock> ; override ;
  end;

implementation
uses BlockListHelper ;

{ TDrawToolCubeCube }

function TDrawToolCubeCube.isBlockInZone(const b:TBlock; const x,y,z:Integer):Boolean ;
begin
  Result:=(b.x-x>=n1) and (b.x-x<=n2) and
          (b.y-y>=n1) and (b.y-y<=n2) and
          (b.z-z>=n1) and (b.z-z<=n2) ;
end;

function TDrawToolCubeCube.Apply(model: TModel; block: TBlock;
  dir: TBlockDir): Boolean;
var x,y,z:Integer ;
    i,j,k:integer ;
    fordel:TList<TBlock> ;
    b:TBlock ;
begin
  inherited Apply(model,block,dir) ;

  x:=block.x ; y:=block.y ; z:=block.z ;
  updateXYZByDir(dir,x,y,z) ;

  n1:=-tmpCubeSize div 2 ;
  n2:=n1+tmpCubeSize-1 ;

  fordel:=TList<TBlock>.Create() ;
  for b in model.getBlocks() do
    if isBlockInZone(b,x,y,z) then fordel.Add(b) ;
  for b in fordel do
     model.getBlocks().Remove(b) ;
  fordel.Free ;

  for i :=n1 to n2 do
    for j :=n1 to n2 do
      for k :=n1 to n2 do
        model.AddBlock(x+i,y+j,z+k,tmpTexName,False) ;

  model.RebuildSkippedBlocks() ;
end;

function TDrawToolCubeCube.Caption: string;
begin
  Result:=' уб из кубов' ;
end;

function TDrawToolCubeCube.Order: Integer;
begin
  Result:=2 ;
end;

function TDrawToolCubeCube.PregetNewBlocks(block: TBlock;
  dir: TBlockDir): TList<TBlock>;
var x,y,z:Integer ;
    i,j,k,n1,n2:integer ;
begin
  Result:=inherited PregetNewBlocks(block,dir) ;

  x:=block.x ; y:=block.y ; z:=block.z ;
  updateXYZByDir(dir,x,y,z) ;

  n1:=-tmpCubeSize div 2 ;
  n2:=n1+tmpCubeSize-1 ;
  for i :=n1 to n2 do
    for j :=n1 to n2 do
      for k :=n1 to n2 do
        if (i=n1)or(i=n2)or(j=n1)or(j=n2)or(k=n1)or(k=n2) then
          Result.Add(TModel.newBlock(x+i,y+j,z+k,tmpTexName)) ;

end;

initialization
  ForceRef(TDrawToolCubeCube) ;

end.
