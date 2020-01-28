unit DrawToolSmooth;


interface
uses DrawTools, Model, CommonClasses ;

type
  TDrawToolSmooth = class(TDrawTool)
  public
     function Caption():string; override ;
     function Order():Integer ; override ;
     function Apply(model:TModel; block:TBlock; dir:TBlockDir):Boolean ; override ;
  end;

implementation
uses Generics.Collections, ModelMap ;

{ TDrawToolSmooth }

function TDrawToolSmooth.Apply(model: TModel; block:TBlock;
  dir: TBlockDir): Boolean;
var x,y,z:Integer ;
    i,j,k:integer ;
    b:TBlock ;
    mm:TModelMap ;
    cnt:Integer ;
begin
  inherited Apply(model,block,dir) ;

  x:=block.x ; y:=block.y ; z:=block.z ;
  updateXYZByDir(dir,x,y,z) ;

  mm:=TModelMap.Create(model.getBlocks()) ;
  for i :=-tmpCubeSize to tmpCubeSize do
    for j :=-tmpCubeSize to tmpCubeSize do
      for k :=-tmpCubeSize to tmpCubeSize do begin
        cnt:=mm.getCountBlocksAround26(x+i,y+j,z+k) ;
        if (cnt<13)and(mm.isBlockAt(x+i,y+j,z+k)) then
          model.DeleteBlock(x+i,y+j,z+k);
        if (cnt>13)and(not mm.isBlockAt(x+i,y+j,z+k)) then
          model.AddBlock(x+i,y+j,z+k,tmpTexName,True) ;
      end ;
  mm.Free ;

  model.RebuildSkippedBlocks() ;
end;

function TDrawToolSmooth.Caption: string;
begin
  Result:='Выравнивание' ;
end;

function TDrawToolSmooth.Order: Integer;
begin
  Result:=10 ;
end;

initialization
  ForceRef(TDrawToolSmooth) ;

end.
