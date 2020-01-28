unit Model;

interface
uses Generics.Collections, Classes,
  CommonClasses ;

type
  TModel = class
  private
     blocks:TList<TBlock> ;
     history:TList<TPackedBlocks> ;
     limits:TPoint3I ;
     skipped_gids:TDictionary<String,Boolean> ;
     function findBlock(x,y,z:Integer; out block:TBlock):Boolean ;
     function findVisibleBlock(x,y,z:Integer; out block:TBlock):Boolean ;
  public
     class function newBlock(x,y,z:Integer; texcode:string):TBlock ;
     constructor Create() ;
     destructor Destroy ; override ;
     procedure SaveToFile(FileName:string) ;
     function LoadFromFile(FileName:string; texcodes:TStringList; out errmsg:string):TLoadResult ;
     function getBlocks():TList<TBlock> ;
     procedure AddBlock(x,y,z:Integer; texcode:string; DeleteExisted:Boolean=True) ;
     procedure DeleteBlock(x,y,z:Integer) ;
     function getSizeInfo():string ;
     procedure setLimits(v:TPoint3I) ;
     function getLimits():TPoint3I ;
     function isLimitOver():Boolean ;
     procedure Clear() ;
     function IsEmpty():Boolean ;
     procedure SaveLayers(Dir:string; FileTpl:string; FormatExt:string; axis:TAxis;
       showgrid:Boolean; showpriorlayer:Boolean; gridwidth:Integer; layerbr:Integer) ;
     procedure PushBlocks() ;
     function PopBlocks():Boolean ;
     function buildBlockReport():string ;
     procedure RebuildSkippedBlocks() ;
     function isBlockSkiped(const b:TBlock):Boolean ;
     procedure fillUsedTextures(texcodes:TStrings) ;
  end;

procedure UpdateXYZByDir(dir:TBlockDir; var x:Integer; var y:Integer; var z:Integer) ;

implementation
uses SysUtils, Graphics, Jpeg, pngimage, Types, Clipbrd,
  OmniXML,
  BlockListHelper, Constants, CommonProc, Measure ;

procedure UpdateXYZByDir(dir:TBlockDir; var x:Integer; var y:Integer; var z:Integer) ;
begin
  case dir of
    dirXle: Dec(x) ;
    dirXgr: Inc(x) ;
    dirYle: Dec(y) ;
    dirYgr: Inc(y) ;
    dirZle: Dec(z) ;
    dirZgr: Inc(z) ;
  end;
end ;

{ TModel }

procedure TModel.AddBlock(x, y, z: Integer; texcode: string; DeleteExisted:Boolean=True);
//var block:TBlock ;
begin
  if DeleteExisted then deleteBlock(x,y,z) ;

//  block.x:=x ;
//  block.y:=y ;
//  block.z:=z ;
//  block.texcode:=texcode ;
  blocks.Add(newBlock(x,y,z,texcode)) ;
end;

function TModel.buildBlockReport: string;
var b:TBlock ;
    rep:TDictionary<string,Integer> ;
    texname:string ;
begin
  rep:=TDictionary<string,Integer>.Create ;
  for b in blocks do
    if rep.ContainsKey(b.texcode) then
      rep[b.texcode]:=rep[b.texcode]+1
    else
      rep.Add(b.texcode,1);

  Result:=Format('Всего блоков: %d',[blocks.Count])+#13#10 ;
  for texname in rep.Keys do
    Result:=Result+Format('%s: %d%',[texname,rep[texname]])+#13#10 ;
end;

procedure TModel.Clear;
begin
  blocks.Clear() ;
end;

constructor TModel.Create;
begin
  blocks:=TList<TBlock>.Create() ;
  history:=TList<TPackedBlocks>.Create() ;
  skipped_gids:=TDictionary<String,Boolean>.Create() ;
  limits.x:=100 ;
  limits.y:=100 ;
  limits.z:=100 ;
end;

destructor TModel.Destroy;
begin
  blocks.Free ;
  inherited Destroy ;
end;

function TModel.getBlocks: TList<TBlock>;
begin
  Result:=blocks ;
end;

class function TModel.newBlock(x, y, z: Integer; texcode: string): TBlock;
var G:TGuid ;
begin
  CreateGUID(G) ;
  Result.gid:=GUIDToString(G) ; ;
  Result.x:=x ;
  Result.y:=y ;
  Result.z:=z ;
  Result.texcode:=texcode ;
end;

function TModel.PopBlocks:Boolean ;
var i:Integer ;
begin
  if history.Count=0 then begin
    Result:=False ;
    Exit ;
  end;

  blocks.Clear() ;
  for i := 0 to Length(history.Last)-1 do
    blocks.Add(history.Last[i]) ;

  history.Delete(history.Count-1);

  RebuildSkippedBlocks() ;

  Result:=True ;
end;

procedure TModel.PushBlocks;
var rec:TPackedBlocks ;
    i:Integer ;
begin
  while history.Count>MAX_HISTORY_SIZE do
    history.Delete(0) ;

  SetLength(rec,blocks.Count) ;
  for i:=0 to blocks.Count-1 do
    rec[i]:=blocks[i] ;
  history.Add(rec) ;
end;

procedure TModel.RebuildSkippedBlocks;
begin
  blocks.fillHiddenGIDs(skipped_gids) ;
end;

procedure TModel.fillUsedTextures(texcodes: TStrings);
var b:TBlock ;
    list:TStringList ;
begin
  list:=TStringList.Create() ;
  for b in blocks do
    if list.IndexOf(b.texcode)=-1 then list.Add(b.texcode) ;
  list.Sort() ;
  texcodes.Assign(list);
end;

function TModel.findBlock(x, y, z: Integer; out block: TBlock): Boolean;
begin
  Result:=blocks.Find(function(b:TBlock):Boolean
  begin
    Result:=(b.x=x)and(b.y=y)and(b.z=z) ;
  end, block) ;
end;

function TModel.findVisibleBlock(x, y, z: Integer; out block: TBlock): Boolean;
begin
  Result:=blocks.Find(function(b:TBlock):Boolean
  begin
    Result:=(b.x=x)and(b.y=y)and(b.z=z) ;
  end, block) ;

  if Result then
    if skipped_gids.ContainsKey(block.gid) then Result:=False ;
end;

procedure TModel.DeleteBlock(x,y,z:Integer) ;
var block:TBlock ;
begin
  if findBlock(x,y,z, block) then blocks.Remove(block) ;
end;

function TModel.getSizeInfo():string ;
begin
  if blocks.Count=0 then Result:='0 x 0 x 0' else
  Result:=Format('%d x %d x %d',[
    blocks.Max(getX)-blocks.Min(getX)+1,
    blocks.Max(getY)-blocks.Min(getY)+1,
    blocks.Max(getZ)-blocks.Min(getZ)+1]) ;
end;

function TModel.isBlockSkiped(const b: TBlock): Boolean;
begin
  Result:=skipped_gids.ContainsKey(b.gid) ;
end;

function TModel.IsEmpty: Boolean;
begin
  Result:=blocks.Count=0 ;
end;

procedure TModel.setLimits(v:TPoint3I) ;
begin
  limits:=v ;
end;

function TModel.getLimits():TPoint3I ;
begin
  Result:=limits ;
end;

function TModel.isLimitOver():Boolean ;
begin
  Result:=False ;
  if blocks.Count=0 then Exit ;
  
  if blocks.Max(getX)-blocks.Min(getX)+1>limits.x then Result:=True ;
  if blocks.Max(getY)-blocks.Min(getY)+1>limits.y then Result:=True ;
  if blocks.Max(getZ)-blocks.Min(getZ)+1>limits.z then Result:=True ;
end;

procedure TModel.SaveLayers(Dir, FileTpl, FormatExt: string; axis: TAxis;
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

begin
  if isEmpty() then Exit ;

  Texs:=TDictionary<string,TGraphic>.Create() ;
  TexsGray:=TDictionary<string,TGraphic>.Create() ;

  RebuildSkippedBlocks() ;

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
    bmp.Canvas.Brush.Color:=clGreen ;
    bmp.Canvas.Pen.Width:=3 ;
    i:=y2-y1 ;
    for y := y1 to y2 do begin
      j:=0 ;
      for z := z1 to z2 do begin
        blockrect:=Rect(j*SIZE,i*SIZE,j*SIZE+SIZE-1,i*SIZE+SIZE-1) ;
        if showpriorlayer then
          if findVisibleBlock(p-1,y,z,b) then
            bmp.Canvas.StretchDraw(blockrect,getCachedTexGray(b.texcode,layerbr));
        if findVisibleBlock(p,y,z,b) then
          bmp.Canvas.StretchDraw(blockrect,getCachedTex(b.texcode));
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

procedure TModel.SaveToFile(FileName: string);
var XML: IXMLDocument;
    b:TBlock ;
begin
  XML := CreateXMLDoc('blocks', True);
  for b in blocks do begin
     with XML.DocumentElement.AddChild('block') do begin
       AddChild('x').Text:=b.x.ToString() ;
       AddChild('y').Text:=b.y.ToString() ;
       AddChild('z').Text:=b.z.ToString() ;
       AddChild('gid').Text:=b.gid ;
       AddChild('texcode').Text:=b.texcode
     end;
  end;
  xml.SaveToFile(FileName,TOutputFormat.ofIndent);
end;

function TModel.LoadFromFile(FileName: string; texcodes:TStringList;
   out errmsg:string):TLoadResult;
var XML: IXMLDocument;
    Node: IXMLNode ;
    b:TBlock ;
    notfoundlist:TStringList ;
begin
  Result:=lrOk ;
  errmsg:='' ;
  notfoundlist:=TStringList.Create() ;
  try

  XML := CreateXMLDoc;
  XML.LoadFromFile(FileName);
  for Node in XML.DocumentElement.SelectNodes('block') do begin
    b.x:=StrToInt(Node.SelectSingleNode('x').Text) ;
    b.y:=StrToInt(Node.SelectSingleNode('y').Text) ;
    b.z:=StrToInt(Node.SelectSingleNode('z').Text) ;
    b.gid:=Node.SelectSingleNode('gid').Text ;
    b.texcode:=Node.SelectSingleNode('texcode').Text.ToLower ;
//    code:=code.ToLower() ;
    if texcodes.IndexOf(b.texcode)<>-1 then
      blocks.Add(b)
    else begin
      Result:=lrUnknownTextures ;
      if notfoundlist.IndexOf(b.texcode)=-1 then notfoundlist.Add(b.texcode) ;
    end;
  end;

  if Result=lrUnknownTextures then
    errmsg:='Не найдены текстуры :'#13#10+notfoundlist.Text ;

  except
    on E:Exception do begin
      Result:=lrError ;
      errmsg:='Системная ошибка: '+E.Message ;
    end;
  end;

  notfoundlist.Free ;
  RebuildSkippedBlocks() ;
end;

end.
