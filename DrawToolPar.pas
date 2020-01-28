unit DrawToolPar;

interface
uses DrawTools, Model, CommonClasses, Generics.Collections, Generics.Defaults ;

type
  TDrawToolPar = class(TDrawTool)
  private
     i1,i2,j1,j2,k1,k2:Integer ;
     function isBlockInZone(const b:TBlock; const x,y,z:Integer):Boolean ;
  public
     function Caption():string; override ;
     function Order():Integer ; override ;
     function Apply(model: TModel; block: TBlock;
       dir: TBlockDir): Boolean; override ;
     function PregetNewBlocks(block:TBlock; dir:TBlockDir):TList<TBlock> ; override ;
  end;

implementation

{ TDrawToolPar }

function TDrawToolPar.isBlockInZone(const b:TBlock; const x,y,z:Integer):Boolean ;
begin
  Result:=(b.x-x>=i1) and (b.x-x<=i2) and
          (b.y-y>=j1) and (b.y-y<=j2) and
          (b.z-z>=k1) and (b.z-z<=k2) ;
end;

function TDrawToolPar.Apply(model: TModel; block: TBlock;
  dir: TBlockDir): Boolean;
var x,y,z:Integer ;
    i,j,k:integer ;
    fordel:TList<TBlock> ;
    b:TBlock ;
begin
  inherited Apply(model,block,dir) ;

  x:=block.x ; y:=block.y ; z:=block.z ;
  updateXYZByDir(dir,x,y,z) ;

  i1:=-tmpParX div 2 ;
  i2:=i1+tmpParX-1 ;
  j1:=-tmpParY div 2 ;
  j2:=j1+tmpParY-1 ;
  k1:=-tmpParZ div 2 ;
  k2:=k1+tmpParZ-1 ;

  fordel:=TList<TBlock>.Create() ;
  for b in model.getBlocks() do
    if isBlockInZone(b,x,y,z) then fordel.Add(b) ;
  for b in fordel do
     model.getBlocks().Remove(b) ;
  fordel.Free ;

  for i :=i1 to i2  do
    for j :=j1 to j2 do
      for k :=k1 to k2 do
        model.AddBlock(x+i,y+j,z+k,tmpTexName) ;

  model.RebuildSkippedBlocks() ;
end;

function TDrawToolPar.Caption: string;
begin
  Result:='Параллелепипед' ;
end;

function TDrawToolPar.Order: Integer;
begin
  Result:=3 ;
end;

function TDrawToolPar.PregetNewBlocks(block: TBlock;
  dir: TBlockDir): TList<TBlock>;
var x,y,z:Integer ;
    i,j,k,i1,i2,j1,j2,k1,k2:integer ;
begin
  Result:=inherited PregetNewBlocks(block,dir) ;

  x:=block.x ; y:=block.y ; z:=block.z ;
  updateXYZByDir(dir,x,y,z) ;

  i1:=-tmpParX div 2 ;
  i2:=i1+tmpParX-1 ;
  j1:=-tmpParY div 2 ;
  j2:=j1+tmpParY-1 ;
  k1:=-tmpParZ div 2 ;
  k2:=k1+tmpParZ-1 ;

  for i :=i1 to i2  do
    for j :=j1 to j2 do
      for k :=k1 to k2 do
        if (i=i1)or(i=i2)or(j=j1)or(j=j2)or(k=k1)or(k=k2) then
          Result.Add(TModel.newBlock(x+i,y+j,z+k,tmpTexName)) ;
end;

initialization
  ForceRef(TDrawToolPar) ;
end.
