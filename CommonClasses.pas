unit CommonClasses;

interface

type
  TPoint3D = record
    x:Double ;
    y:Double ;
    z:Double ;
  end ;

  TPoint3I = record
    x:Integer ;
    y:Integer ;
    z:Integer ;
  end;

  TBlockDir = ( dirXle,dirXgr,dirYle,dirYgr,dirZle,dirZgr ) ;
  TAxis = ( axisX, axisY, axisZ ) ;

  TLoadResult = (lrOk, lrError, lrUnknownTextures) ;

  TMovingMode = (mmDecart, mmSphere) ;
  TSelectionMode = (smPreview, smGreenSide) ;

  TBlock = record
    x:Integer ;
    y:Integer ;
    z:Integer ;
    texcode:string ;
    gid:string ;
  end;

  PBlock = ^TBlock ;

  TPackedBlocks = array of TBlock ;

implementation

end.
