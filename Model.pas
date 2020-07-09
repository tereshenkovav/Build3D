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
     procedure fillUsedTextures(texcodes:TStrings) ;
  end;

procedure UpdateXYZByDir(dir:TBlockDir; var x:Integer; var y:Integer; var z:Integer) ;

implementation
uses SysUtils, Types,
  OmniXML,
  BlockListHelper, Constants, CommonProc, Measure, DebugClient ;

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
end;

constructor TModel.Create;
begin
  blocks:=TList<TBlock>.Create() ;
  history:=TList<TPackedBlocks>.Create() ;
  history_head:=0 ;
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
