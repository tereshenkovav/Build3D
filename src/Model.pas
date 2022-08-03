unit Model;

interface
uses Generics.Collections, Classes,
  CommonClasses ;

type
  TModel = class
  private
     blocks:TList<TBlock> ;
     history:TList<TPackedBlocks> ;
     history_head:Integer ;
     limits:TPoint3I ;
     skipped_gids:TDictionary<String,Boolean> ;
     skipped_planes:TDictionary<String,TBlockDirSet> ;
     function findBlock(x,y,z:Integer; out block:TBlock):Boolean ;
  public
     class function newBlock(x,y,z:Integer; texcode:string; bt:TBlockType=btFull):TBlock ;
     constructor Create() ;
     destructor Destroy ; override ;
     procedure SaveToFile(FileName:string) ;
     function LoadFromFile(FileName:string; texcodes:TStringList; out errmsg:string):TLoadResult ;
     function getBlocks():TList<TBlock> ;
     procedure AddBlock(x,y,z:Integer; texcode:string; DeleteExisted:Boolean=True) ;
     procedure AddTypedBlock(x,y,z:Integer; texcode:string; bt:TBlockType; DeleteExisted:Boolean=True) ;
     procedure DeleteBlock(x,y,z:Integer) ;
     function getSizeInfo():string ;
     procedure setLimits(v:TPoint3I) ;
     function getLimits():TPoint3I ;
     function isLimitOver():Boolean ;
     procedure Clear() ;
     function IsEmpty():Boolean ;
     procedure PushBlocks() ;
     function Undo():Boolean ;
     function Redo():Boolean ;
     function buildBlockReport():string ;
     procedure RebuildSkippedBlocks() ;
     function isBlockSkiped(const b:TBlock):Boolean ;
     function isPlaneSkiped(const b:TBlock; dir:TBlockDir):Boolean ;
     procedure fillUsedTextures(texcodes:TStrings) ;
     procedure CopyZoneTo(const zone:TZone3I; dP:TPoint3I; mirrors:TAxisSet) ;
     procedure RotFillZone(const zone: TZone3I; switchside:Boolean);
     procedure ClearZone(const zone: TZone3I);
  end;

procedure UpdateXYZByDir(dir:TBlockDir; var x:Integer; var y:Integer; var z:Integer;
  dist:Integer=1) ;

implementation
uses SysUtils, Types, Math,
  OmniXML,
  BlockListHelper, Constants, CommonProc, Measure, DebugClient, ModelMap ;

procedure UpdateXYZByDir(dir:TBlockDir; var x:Integer; var y:Integer; var z:Integer;
  dist:Integer=1) ;
begin
  case dir of
    dirXle: x:=x-dist ;
    dirXgr: x:=x+dist ;
    dirYle: y:=y-dist ;
    dirYgr: y:=y+dist ;
    dirZle: z:=z-dist ;
    dirZgr: z:=z+dist ;
  end;
end ;

{ TModel }

procedure TModel.AddBlock(x, y, z: Integer; texcode: string; DeleteExisted:Boolean=True);
//var block:TBlock ;
begin
  AddTypedBlock(x,y,z,texcode,btFull,DeleteExisted) ;
end;

procedure TModel.AddTypedBlock(x,y,z:Integer; texcode:string; bt:TBlockType; DeleteExisted:Boolean=True) ;
begin
  if DeleteExisted then deleteBlock(x,y,z) ;
  blocks.Add(newBlock(x,y,z,texcode,bt)) ;
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
  history.Clear() ;
  history_head:=0 ;
  limits.x:=100 ;
  limits.y:=100 ;
  limits.z:=100 ;
end;

procedure TModel.CopyZoneTo(const zone: TZone3I; dP:TPoint3I; mirrors:TAxisSet);
var b:TBlock ;
    newx,newy,newz:Integer ;
begin
  PushBlocks() ;

  for b in blocks do
    if zone.isBlockIn(b) then begin
      newx:=b.x+dP.x ;
      newy:=b.y+dP.y ;
      newz:=b.z+dP.z ;
      if AxisX in mirrors then newx:=dp.x+zone.x2-(b.x-zone.x1) ;
      if AxisY in mirrors then newy:=dp.y+zone.y2-(b.y-zone.y1) ;
      if AxisZ in mirrors then newz:=dp.z+zone.z2-(b.z-zone.z1) ;
      AddTypedBlock(newx,newy,newz,b.texcode,b.bt,True) ;
    end;

  RebuildSkippedBlocks() ;
end;

procedure TModel.RotFillZone(const zone: TZone3I; switchside:Boolean);
var p1,p2,p:TPoint3I ;
    dPx,dPy,dPz:Integer ;
    dRx,dRy,dRz:Integer ;
    b,bold:TBlock ;
    a,da:Single ;
    newx,newy,newz,r:Integer ;
    z:Boolean ;
begin
  PushBlocks() ;

  if zone.dX()=1 then begin
    p1.x:=zone.x1 ;
    p2.x:=zone.x1 ;
    dRx:=0 ;
    if zone.dY()>zone.dZ() then begin
      p1.y:=zone.y1 ;
      p2.y:=zone.y2 ;
      p1.z:=IfThen(switchside,zone.z1,zone.z2) ; // switch by monoseq
      p2.z:=p1.z ;
      dRy:=0 ;
      dRz:=IfThen(switchside,1,-1)*Sign(zone.z2-zone.z1) ; // switch by monoseq
    end
    else begin
      p1.y:=IfThen(switchside,zone.y1,zone.y2) ; // switch by monoseq
      p2.y:=p1.y ;
      p1.z:=zone.z1 ;
      p2.z:=zone.z2 ;
      dRy:=IfThen(switchside,1,-1)*Sign(zone.y2-zone.y1) ; // switch by monoseq
      dRz:=0 ;
    end;
  end;

  if zone.dY()=1 then begin
    p1.y:=zone.y1 ;
    p2.y:=zone.y1 ;
    dRy:=0 ;
    if zone.dX()>zone.dZ() then begin
      p1.x:=zone.x1 ;
      p2.x:=zone.x2 ;
      p1.z:=IfThen(switchside,zone.z1,zone.z2) ; // switch by monoseq
      p2.z:=p1.z ;
      dRx:=0 ;
      dRz:=IfThen(switchside,1,-1)*Sign(zone.z2-zone.z1) ; // switch by monoseq
    end
    else begin
      p1.x:=IfThen(switchside,zone.x1,zone.x2) ; // switch by monoseq
      p2.x:=p1.x ;
      p1.z:=zone.z1 ;
      p2.z:=zone.z2 ;
      dRx:=IfThen(switchside,1,-1)*Sign(zone.x2-zone.x1) ; // switch by monoseq
      dRz:=0 ;
    end;
  end;

  if zone.dZ()=1 then begin
    p1.z:=zone.z1 ;
    p2.z:=zone.z1 ;
    dRz:=0 ;
    if zone.dX()>zone.dY() then begin
      p1.x:=zone.x1 ;
      p2.x:=zone.x2 ;
      p1.y:=IfThen(switchside,zone.y1,zone.y2) ; // switch by monoseq
      p2.y:=p1.y ;
      dRx:=0 ;
      dRy:=IfThen(switchside,1,-1)*Sign(zone.y2-zone.y1) ; // switch by monoseq
    end
    else begin
      p1.x:=IfThen(switchside,zone.x1,zone.x2) ; ; // switch by monoseq
      p2.x:=p1.x ;
      p1.y:=zone.y1 ;
      p2.y:=zone.y2 ;
      dRx:=IfThen(switchside,1,-1)*Sign(zone.x2-zone.x1) ; // switch by monoseq
      dRy:=0 ;
    end;
  end;

  dPx:=Sign(p2.x-p1.x) ;
  dPy:=Sign(p2.y-p1.y) ;
  dPz:=Sign(p2.z-p1.z) ;

  while zone.isPointIn(p1) do begin

    p.x:=p1.x+dRx ;
    p.y:=p1.y+dRy ;
    p.z:=p1.z+dRz ;

    while zone.isPointIn(p) do begin
      if findBlock(p.x,p.y,p.z,b) then begin
        a:=0 ;
        r:=Ceil(Sqrt((p.x-p1.x)*(p.x-p1.x)+(p.y-p1.y)*(p.y-p1.y)+(p.z-p1.z)*(p.z-p1.z))) ;
        da:=0.1/r ;
        while (a<2*PI) do begin
          if dPx<>0 then begin
            newx:=p1.x ;
            newy:=p1.y+Round(r*Sin(a)) ;
            newz:=p1.z+Round(r*Cos(a)) ;
          end;
          if dPy<>0 then begin
            newx:=p1.x+Round(r*Sin(a)) ;
            newy:=p1.y ;
            newz:=p1.z+Round(r*Cos(a)) ;
          end;
          if dPz<>0 then begin
            newx:=p1.x+Round(r*Sin(a)) ;
            newy:=p1.y+Round(r*Cos(a)) ;
            newz:=p1.z ;
          end;
          z:=False ;
          if not findBlock(newx,newy,newz,bold) then z:=True else
            z:=(b.texcode<>bold.texcode)or(b.bt<>bold.bt) ;
          if z then AddTypedBlock(newx,newy,newz,b.texcode,b.bt) ;
          a:=a+da ;
        end ;
      end;
      Inc(p.x,dRx) ;
      Inc(p.y,dRy) ;
      Inc(p.z,dRz) ;
    end;

    Inc(p1.x,dPx) ;
    Inc(p1.y,dPy) ;
    Inc(p1.x,dPz) ;
  end ;

  RebuildSkippedBlocks() ;
end;

procedure TModel.ClearZone(const zone: TZone3I);
var i:Integer ;
begin
  PushBlocks() ;

  i:=0 ;
  while i<blocks.Count do
    if zone.isBlockIn(blocks[i]) then blocks.Delete(i) else Inc(i) ;

  RebuildSkippedBlocks() ;
end;

constructor TModel.Create;
begin
  blocks:=TList<TBlock>.Create() ;
  history:=TList<TPackedBlocks>.Create() ;
  history_head:=0 ;
  skipped_gids:=TDictionary<String,Boolean>.Create() ;
  skipped_planes:=TDictionary<String,TBlockDirSet>.Create() ;
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

class function TModel.newBlock(x, y, z: Integer; texcode: string; bt:TBlockType): TBlock;
var G:TGuid ;
begin
  CreateGUID(G) ;
  Result.gid:=GUIDToString(G) ; ;
  Result.x:=x ;
  Result.y:=y ;
  Result.z:=z ;
  Result.bt:=bt ;
  Result.texcode:=texcode ;
end;

function TModel.Undo:Boolean ;
var i:Integer ;
    rec:TPackedBlocks ;
begin
  if history_head<=0 then Exit(False) ;

  DebugClient.WriteToServer('TModel.Undo',
    Format('history_head=%d, history.Count=%d',[history_head,history.Count])) ;

  if history_head=history.Count then begin
    SetLength(rec,blocks.Count) ;
    for i:=0 to blocks.Count-1 do
      rec[i]:=blocks[i] ;
    history.Add(rec) ;
  end;

  Dec(history_head) ;

  blocks.Clear() ;
  for i := 0 to Length(history[history_head])-1 do
    blocks.Add(history[history_head][i]) ;

  RebuildSkippedBlocks() ;

  Result:=True ;
end;

function TModel.Redo:Boolean ;
var i:Integer ;
begin
  if history_head+1>=history.Count then Exit(False) ;

  DebugClient.WriteToServer('TModel.Redo',
    Format('history_head=%d, history.Count=%d',[history_head,history.Count])) ;

  Inc(history_head) ;
  blocks.Clear() ;
  for i := 0 to Length(history[history_head])-1 do
    blocks.Add(history[history_head][i]) ;

  RebuildSkippedBlocks() ;

  Result:=True ;
end;

procedure TModel.PushBlocks;
var rec:TPackedBlocks ;
    i:Integer ;
begin
  while history.Count>history_head do
    history.Delete(history.Count-1) ;

  SetLength(rec,blocks.Count) ;
  for i:=0 to blocks.Count-1 do
    rec[i]:=blocks[i] ;
  history.Add(rec) ;

  if history.Count=MAX_HISTORY_SIZE then history.Delete(0) ;

  history_head:=history.Count ;
end;

procedure TModel.RebuildSkippedBlocks;
begin
  blocks.fillHiddenGIDs(skipped_gids,skipped_planes) ;
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

function TModel.isPlaneSkiped(const b: TBlock; dir: TBlockDir): Boolean;
begin
  if not skipped_planes.ContainsKey(b.gid) then
    Result:=False
  else
    Result:=(dir in skipped_planes[b.gid]) ;
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
       if b.bt=btFull then AddChild('bt').Text:='full' else
       if b.bt=btUpper then AddChild('bt').Text:='upper' else
       if b.bt=btLower then AddChild('bt').Text:='lower' ;
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
    btstr:string ;
begin
  blocks.Clear() ;

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
    b.bt:=btFull ;
    if Node.SelectSingleNode('bt')<>nil then begin
      btstr:=Node.SelectSingleNode('bt').Text ;
      if btstr='uppper' then b.bt:=btUpper else
      if btstr='lower' then b.bt:=btLower ;
    end;
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

  history_head:=0 ;
  history.Clear() ;
end;

end.
