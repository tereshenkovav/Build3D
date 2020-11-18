unit ModelExport;

interface
uses Model, CommonClasses ;

type
  TModelExport = class
  private
    model:TModel ;
    function findVisibleBlock(x, y, z: Integer; out block: TBlock): Boolean;
  public
    constructor Create(model:TModel) ;
    procedure SaveLayers(Dir:string; FileTpl:string; FormatExt:string; axis:TAxis;
       showgrid:Boolean; showpriorlayer:Boolean; gridwidth:Integer; layerbr:Integer) ;
  end;

implementation
uses Generics.Collections, Classes, Types, Graphics, Jpeg, StrUtils, pngimage,SysUtils,
  CommonProc, Constants, BlockListHelper ;

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
end;

procedure TModelExport.SaveLayers(
  Dir, FileTpl, FormatExt: string; axis: TAxis;
  showgrid:Boolean; showpriorlayer:Boolean; gridwidth:Integer; layerbr:Integer);
var p,p1,p2:Integer ;
    y,z,y1,y2,z1,z2,w,h:Integer ;
    i,j:Integer ;
    bmp:TBitmap ;
    Texs,TexsGray:TDictionary<string,TGraphic> ;
    code:string ;
    b:TBlock ;
    gra:TGraphic ;
    blockrect:TRect ;
const
  SIZE=50 ;

function getCachedTex(code:string):TGraphic ;
var tex:TGraphic ;
begin
  if not texs.ContainsKey(code) then begin
    tex:=TJpegImage.Create() ;
    tex.LoadFromFile(TEXDIR+'\'+code);
    texs.Add(code,tex);
  end;
  Result:=texs[code] ;
end;

function getCachedTexGray(code:string; level:Integer):TGraphic ;
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

var blocks:TList<TBlock> ;
begin
  if model.isEmpty() then Exit ;

  Texs:=TDictionary<string,TGraphic>.Create() ;
  TexsGray:=TDictionary<string,TGraphic>.Create() ;

  model.RebuildSkippedBlocks() ;

  blocks:=model.getBlocks() ;
    y1:=blocks.Min(getY) ;
    y2:=blocks.Max(getY) ;
    z1:=blocks.Min(getZ) ;
    z2:=blocks.Max(getZ) ;

  p1:=blocks.Min(getX) ;
  p2:=blocks.Max(getX) ;
  for p := p1 to p2 do begin

    w:=SIZE*(z2-z1+1) ;
    h:=SIZE*(y2-y1+1) ;
    bmp:=TBitmap.Create() ;
    bmp.Height:=h ;
    bmp.Width:=w ;
    bmp.Canvas.Brush.Color:=clBlack ;
    bmp.Canvas.FillRect(Rect(0,0,w,h));
    bmp.Canvas.Pen.Color:=clGreen ;
    bmp.Canvas.Brush.Color:=clBlack ;
    bmp.Canvas.Pen.Width:=3 ;
    bmp.Canvas.Font.Size:=16 ;
    bmp.Canvas.Font.Color:=clWhite ;
    i:=y2-y1 ;
    for y := y1 to y2 do begin
      j:=0 ;
      for z := z1 to z2 do begin
        blockrect:=Rect(j*SIZE,i*SIZE,j*SIZE+SIZE-1,i*SIZE+SIZE-1) ;
        if showpriorlayer then
          if findVisibleBlock(p-1,y,z,b) then begin
            bmp.Canvas.StretchDraw(blockrect,getCachedTexGray(b.texcode,layerbr));
            if b.bt<>btFull then
              bmp.Canvas.TextOut(blockrect.Left,blockrect.Top,IfThen(b.bt=btUpper,'U','L'));
          end ;
        if findVisibleBlock(p,y,z,b) then begin
          bmp.Canvas.StretchDraw(blockrect,getCachedTex(b.texcode));
          if b.bt<>btFull then
            bmp.Canvas.TextOut(blockrect.Left,blockrect.Top,IfThen(b.bt=btUpper,'U','L'));
        end ;
        Inc(j) ;
      end;
      Dec(i) ;
    end;

    // сетка
    if showgrid then begin
      bmp.Canvas.Pen.Width:=gridwidth ;
      bmp.Canvas.Pen.Color:=clWhite ;
      for i := 1 to y2-y1 do begin
        bmp.Canvas.MoveTo(0,i*SIZE);
        bmp.Canvas.LineTo(w-1,i*SIZE);
      end;

      for j := 1 to z2-z1 do begin
        bmp.Canvas.MoveTo(j*SIZE,0);
        bmp.Canvas.LineTo(j*SIZE,h);
      end;
    end;

    if FormatExt='bmp' then
      bmp.SaveToFile(Dir+'\'+Format(FileTpl,[p-p1])+'.'+FormatExt)
    else begin
      if (FormatExt='jpg') then gra:=TJPEGImage.Create else gra:=TPNGImage.Create ;
      gra.Assign(bmp);
      gra.SaveToFile(Dir+'\'+Format(FileTpl,[p-p1])+'.'+FormatExt);
      gra.Free ;
    end;
    bmp.Free ;
  end;

  Texs.Free ;
end;

end.
