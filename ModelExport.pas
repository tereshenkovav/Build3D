unit ModelExport;

interface
uses Graphics, Generics.Collections,
     Model, CommonClasses ;

type
  TModelExport = class
  private
    model:TModel ;
    Texs,TexsGray:TDictionary<string,TGraphic> ;
    function findVisibleBlock(x, y, z: Integer; out block: TBlock): Boolean;
    function createLayerBitmap(w,h:Integer):TBitmap ;
    function getCachedTex(code:string):TGraphic ;
    function getCachedTexGray(code:string; level:Integer):TGraphic ;
    procedure fillGridOnBitmap(bmp:TBitmap; gridwidth:Integer) ;
  public
    constructor Create(model:TModel) ;
    destructor Destroy ; override ;
    procedure SaveLayers(Dir:string; FileTpl:string; FormatExt:string; axis:TAxis;
       showgrid:Boolean; showpriorlayer:Boolean; gridwidth:Integer; layerbr:Integer) ;
  end;

implementation
uses Classes, Types, Jpeg, StrUtils, pngimage,SysUtils,
  CommonProc, Constants, BlockListHelper ;

const
  SIZE=50 ;

function TModelExport.createLayerBitmap(w,h:Integer):TBitmap ;
begin
  Result:=TBitmap.Create() ;
  Result.Width:=w ;
  Result.Height:=h ;
  Result.Canvas.Brush.Color:=clBlack ;
  Result.Canvas.FillRect(Rect(0,0,w,h));
  Result.Canvas.Pen.Color:=clGreen ;
  Result.Canvas.Brush.Color:=clBlack ;
  Result.Canvas.Pen.Width:=3 ;
  Result.Canvas.Font.Size:=16 ;
  Result.Canvas.Font.Color:=clWhite ;
end;

destructor TModelExport.Destroy;
begin
  texs.Free ;
  texsgray.Free ;
  inherited Destroy;
end;

function TModelExport.getCachedTex(code:string):TGraphic ;
var tex:TGraphic ;
begin
  if not texs.ContainsKey(code) then begin
    tex:=TJpegImage.Create() ;
    tex.LoadFromFile(TEXDIR+'\'+code);
    texs.Add(code,tex);
  end;
  Result:=texs[code] ;
end;

function TModelExport.getCachedTexGray(code:string; level:Integer):TGraphic ;
var tex:TGraphic ;
    bmp:TBitmap ;
    i,j:Integer ;
begin
  if not texsgray.ContainsKey(code) then begin
    tex:=TJpegImage.Create() ;
    tex.LoadFromFile(TEXDIR+'\'+code);
    bmp:=TBitmap.Create() ;
    bmp.Assign(tex);
    tex.Free ;

    // gray
    for i:=0 to bmp.Width-1 do
      for j:=0 to bmp.Height-1 do
        bmp.Canvas.Pixels[i,j]:=getGrayColor(bmp.Canvas.Pixels[i,j],level) ;

    texsgray.Add(code,bmp);
  end;
  Result:=texsgray[code] ;
end;

procedure TModelExport.fillGridOnBitmap(bmp: TBitmap; gridwidth:Integer);
var x,y:Integer ;
begin
  bmp.Canvas.Pen.Width:=gridwidth ;
  bmp.Canvas.Pen.Color:=clWhite ;
  y:=SIZE ;
  while y<bmp.Height-1 do begin
    bmp.Canvas.MoveTo(0,y);
    bmp.Canvas.LineTo(bmp.Width-1,y);
    Inc(y,size) ;
  end;

  x:=SIZE ;
  while x<bmp.Width-1 do begin
    bmp.Canvas.MoveTo(x,0);
    bmp.Canvas.LineTo(x,bmp.Height-1);
    Inc(x,SIZE) ;
  end;
end;

function TModelExport.findVisibleBlock(x, y, z: Integer; out block: TBlock): Boolean;
begin
  Result:=model.getBlocks().Find(function(b:TBlock):Boolean
  begin
    Result:=(b.x=x)and(b.y=y)and(b.z=z) ;
  end, block) ;

  if Result then
    if model.isBlockSkiped(block) then Result:=False ;
end;

constructor TModelExport.Create(model:TModel) ;
begin
  self.model:=model ;
  Texs:=TDictionary<string,TGraphic>.Create() ;
  TexsGray:=TDictionary<string,TGraphic>.Create() ;
end;

procedure TModelExport.SaveLayers(
  Dir, FileTpl, FormatExt: string; axis: TAxis;
  showgrid:Boolean; showpriorlayer:Boolean; gridwidth:Integer; layerbr:Integer);

procedure processBlock(bmp:TBitmap; Px,Py,Pz,PgrayX,PgrayY,PgrayZ,Pi,Pj:Integer) ;
var b:TBlock ;
    blockrect:TRect ;
begin
  blockrect:=Rect(Pj*SIZE,Pi*SIZE,Pj*SIZE+SIZE-1,Pi*SIZE+SIZE-1) ;
  if showpriorlayer then
    if findVisibleBlock(PgrayX,PgrayY,PgrayZ,b) then begin
      bmp.Canvas.StretchDraw(blockrect,getCachedTexGray(b.texcode,layerbr));
      if b.bt<>btFull then
        bmp.Canvas.TextOut(blockrect.Left,blockrect.Top,IfThen(b.bt=btUpper,'U','L'));
    end ;
  if findVisibleBlock(Px,Py,Pz,b) then begin
    bmp.Canvas.StretchDraw(blockrect,getCachedTex(b.texcode));
    if b.bt<>btFull then
      bmp.Canvas.TextOut(blockrect.Left,blockrect.Top,IfThen(b.bt=btUpper,'U','L'));
  end ;
end;

procedure saveBitmap(bmp:TBitmap; n:Integer) ;
var gra:TGraphic ;
begin
  if FormatExt='bmp' then
    bmp.SaveToFile(Dir+'\'+Format(FileTpl,[n])+'.'+FormatExt)
  else begin
    if (FormatExt='jpg') then gra:=TJPEGImage.Create else gra:=TPNGImage.Create ;
    gra.Assign(bmp);
    gra.SaveToFile(Dir+'\'+Format(FileTpl,[n])+'.'+FormatExt);
    gra.Free ;
  end;
end ;

var x,y,z,x1,x2,y1,y2,z1,z2:Integer ;
    bmp:TBitmap ;
    blocks:TList<TBlock> ;
begin
  if model.isEmpty() then Exit ;

  model.RebuildSkippedBlocks() ;

  blocks:=model.getBlocks() ;
  x1:=blocks.Min(getX) ;
  x2:=blocks.Max(getX) ;
  y1:=blocks.Min(getY) ;
  y2:=blocks.Max(getY) ;
  z1:=blocks.Min(getZ) ;
  z2:=blocks.Max(getZ) ;

  if axis=axisX then
    for x := x1 to x2 do begin
      bmp:=createLayerBitmap(SIZE*(z2-z1+1),SIZE*(y2-y1+1)) ;
      for y := y1 to y2 do
        for z := z1 to z2 do
          processBlock(bmp,x,y,z,x-1,y,z,y-y1,z-z1) ;
      if showgrid then fillGridOnBitmap(bmp,gridwidth) ;
      saveBitmap(bmp,x-x1) ;
      bmp.Free ;
    end;

  if axis=axisY then
    for y := y1 to y2 do begin
      bmp:=createLayerBitmap(SIZE*(z2-z1+1),SIZE*(x2-x1+1)) ;
      for x := x1 to x2 do
        for z := z1 to z2 do
          processBlock(bmp,x,y,z,x,y-1,z,x-x1,z-z1) ;
      if showgrid then fillGridOnBitmap(bmp,gridwidth) ;
      saveBitmap(bmp,y-y1) ;
      bmp.Free ;
    end;

  if axis=axisZ then
    for z := z1 to z2 do begin
      bmp:=createLayerBitmap(SIZE*(x2-x1+1),SIZE*(y2-y1+1)) ;
      for y := y1 to y2 do
        for x := x1 to x2 do
          processBlock(bmp,x,y,z,x,y,z-1,y-y1,x-x1) ;
      if showgrid then fillGridOnBitmap(bmp,gridwidth) ;
      saveBitmap(bmp,z-z1) ;
      bmp.Free ;
    end;

end;

end.
