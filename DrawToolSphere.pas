unit DrawToolSphere;

interface
uses DrawTools, Model, CommonClasses, Generics.Collections, Generics.Defaults ;

type
  TDrawToolSphere = class(TDrawTool)
  private
     function isBlockInZone(const b:TBlock; const x,y,z:Integer):Boolean ;
  public
     function Caption():string; override ;
     function Order():Integer ; override ;
     function Apply(model: TModel; block: TBlock;
       dir: TBlockDir): Boolean; override ;
     function PregetNewBlocks(block:TBlock; dir:TBlockDir):TList<TBlock> ; override ;
  end;

implementation
uses BlockListHelper, ModelMap, IOUtils, SysUtils ;

{ TDrawToolSphere }

function TDrawToolSphere.isBlockInZone(const b:TBlock; const x,y,z:Integer):Boolean ;
begin
  Result:=(b.x-x)*(b.x-x)+(b.y-y)*(b.y-y)+(b.z-z)*(b.z-z)<tmpCubeSize*tmpCubeSize ;
end;

function TDrawToolSphere.Apply(model: TModel; block: TBlock;
  dir: TBlockDir): Boolean;
var x,y,z:Integer ;
    i,j,k,n1,n2:integer ;
    mm:TModelMap ;
    fordel:TList<TBlock> ;
    b:TBlock ;
begin
  inherited Apply(model,block,dir) ;

  x:=block.x ; y:=block.y ; z:=block.z ;
  if tmpSetToEdge then
    updateXYZByDir(dir,x,y,z,tmpCubeSize)
  else
    updateXYZByDir(dir,x,y,z) ;

  fordel:=TList<TBlock>.Create() ;

  n1:=-tmpCubeSize ;
  n2:=+tmpCubeSize ;

  for b in model.getBlocks() do
    if isBlockInZone(b,x,y,z) then fordel.Add(b) ;

  for b in fordel do
     model.getBlocks().Remove(b) ;

  for i :=n1 to n2 do
    for j :=n1 to n2 do
      for k :=n1 to n2 do
        if (i*i+j*j+k*k<tmpCubeSize*tmpCubeSize) then
          model.AddBlock(x+i,y+j,z+k,tmpTexName,False) ;

  fordel.Free ;

  model.RebuildSkippedBlocks() ;
end;

function TDrawToolSphere.Caption: string;
begin
  Result:='Сфера' ;
end;

function TDrawToolSphere.Order: Integer;
begin
  Result:=4 ;
end;

function TDrawToolSphere.PregetNewBlocks(block: TBlock;
  dir: TBlockDir): TList<TBlock>;
var x,y,z:Integer ;
    i,j,k,n1,n2:integer ;
    list:TList<TBlock> ;
    skipped:TDictionary<String,Boolean> ;
    b:TBlock ;
begin
  Result:=inherited PregetNewBlocks(block,dir) ;

  x:=block.x ; y:=block.y ; z:=block.z ;
  if tmpSetToEdge then
    updateXYZByDir(dir,x,y,z,tmpCubeSize)
  else
    updateXYZByDir(dir,x,y,z) ;

  list:=TList<TBlock>.Create() ;

  n1:=-tmpCubeSize ;
  n2:=+tmpCubeSize ;
  for i :=n1 to n2 do
    for j :=n1 to n2 do
      for k :=n1 to n2 do
        if (i*i+j*j+k*k<tmpCubeSize*tmpCubeSize) then
          list.Add(TModel.newBlock(x+i,y+j,z+k,tmpTexName)) ;

  skipped:=TDictionary<String,Boolean>.Create() ;
  list.fillHiddenGIDs(skipped) ;
  for b in list do
    if not skipped.ContainsKey(b.gid) then
      Result.Add(b) ;

  list.Free ;
  skipped.Free ;
end;

initialization
  ForceRef(TDrawToolSphere) ;

end.
