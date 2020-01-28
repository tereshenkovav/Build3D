unit DrawToolGrow;

interface
uses DrawTools, Model, CommonClasses ;

type
  TDrawToolGrow = class(TDrawTool)
  public
     function Caption():string; override ;
     function Order():Integer ; override ;
     function Apply(model:TModel; block:TBlock; dir:TBlockDir):Boolean ; override ;
  end;

implementation
uses Generics.Collections, ModelMap ;

{ TDrawToolGrow }

function TDrawToolGrow.Apply(model: TModel; block:TBlock;
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
      for k :=-tmpCubeSize to tmpCubeSize do
        if not mm.isBlockAt(x+i,y+j,z+k) then
          if mm.getCountBlocksAround6(x+i,y+j,z+k)>0 then
            model.AddBlock(x+i,y+j,z+k,tmpTexName,True) ;
  mm.Free ;

  model.RebuildSkippedBlocks() ;
end;

function TDrawToolGrow.Caption: string;
begin
  Result:='Наращивание' ;
end;

function TDrawToolGrow.Order: Integer;
begin
  Result:=11 ;
end;

initialization
  ForceRef(TDrawToolGrow) ;

end.
