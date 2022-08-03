unit CommonClasses;

interface

type
  TBlockType = (btFull,btUpper,btLower) ;

  TBlock = record
    x:Integer ;
    y:Integer ;
    z:Integer ;
    texcode:string ;
    bt:TBlockType ;
    gid:string ;
  end;

  TPoint3D = record
    x:Double ;
    y:Double ;
    z:Double ;
  end ;

  TPoint3I = record
    x:Integer ;
    y:Integer ;
    z:Integer ;
    procedure setFromBlock(const b:TBlock) ;
    class operator Equal(a: TPoint3I; b: TPoint3I) : Boolean;
  end;

  TZone3I = record
    x1:Integer ;
    y1:Integer ;
    z1:Integer ;
    x2:Integer ;
    y2:Integer ;
    z2:Integer ;
    function isBlockIn(const b:TBlock):Boolean ;
    function isPointIn(const p:TPoint3I): Boolean;
    function dX():Integer ;
    function dY():Integer ;
    function dZ():Integer ;
  end;

  TBlockDir = ( dirXle,dirXgr,dirYle,dirYgr,dirZle,dirZgr ) ;
  TAxis = ( axisX, axisY, axisZ ) ;
  TAxisSet = set of TAxis ;
  TBlockDirSet = set of TBlockDir ;

  TLoadResult = (lrOk, lrError, lrUnknownTextures) ;

  TMovingMode = (mmDecart, mmSphere) ;
  TSelectionMode = (smPreview, smGreenSide) ;

  PBlock = ^TBlock ;

  TPackedBlocks = array of TBlock ;

  TSelectionState = (stNone,stFirstBlock,stLastBlock) ;

implementation

class operator TPoint3I.Equal(a, b: TPoint3I): Boolean;
begin
  Result:=(a.x=b.x)and(a.y=b.y)and(a.z=b.z) ;
end;

procedure TPoint3I.setFromBlock(const b:TBlock) ;
begin
  x:=b.x ;
  y:=b.y ;
  z:=b.z ;
end;

{ TZone3I }

function TZone3I.dX: Integer;
begin
  Result:=x2-x1+1 ;
end;

function TZone3I.dY: Integer;
begin
  Result:=y2-y1+1 ;
end;

function TZone3I.dZ: Integer;
begin
  Result:=z2-z1+1 ;
end;

function TZone3I.isBlockIn(const b: TBlock): Boolean;
begin
  Result:=(x1<=b.x)and(b.x<=x2)and(y1<=b.y)and(b.y<=y2)and(z1<=b.z)and(b.z<=z2) ;
end;

function TZone3I.isPointIn(const p: TPoint3I): Boolean;
begin
  Result:=(x1<=p.x)and(p.x<=x2)and(y1<=p.y)and(p.y<=y2)and(z1<=p.z)and(p.z<=z2) ;
end;

end.
