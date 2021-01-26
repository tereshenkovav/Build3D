unit KeysConfig;

interface
uses Classes, Generics.Collections ;

type
  TCtrlAltShift = ( sNo, sCtrl, sAlt, sShift ) ;

  TKeysConfig = class
  private
    type
      TKey = record
        cas:TCtrlAltShift ;
        key:Word ;
        caption:string ;
        class function getKey(Acaption:string; Acas:TCtrlAltShift; Akey:Word):TKey ; static ;
        function isMatch(ss:TShiftState; Akey:Word):Boolean ;
      end;

    var keys:TDictionary<string,TKey> ;
    orderedkeys:TStringList ;
    casstrings:TStringList ;
    keystrings:TStringList ;
  public
    constructor Create() ;
    destructor Destroy ; override ;
    procedure addKey(code,caption:string; cas:TCtrlAltShift; key:Word) ;
    procedure setKey(code:string; cas:TCtrlAltShift; key:Word) ;
    function isKeyMatch(code:string; ss:TShiftState; key:Word):Boolean ;
    function isValidKeys():Boolean ;

    function getKeyCaption(code:string):string ;
    function getCSA(code:string):TCtrlAltShift ;
    function getKey(code:string):Word ;
    function getKeyView(code:string):string ;

    function getAllCodes():TStringList ;
    function getCASStrings():TStringList ;
    function getKeyStrings():TStringList ;

    procedure saveToFile() ;
    procedure loadFromFile() ;
  end;

implementation
uses IniFiles, SysUtils, Windows,
  CommonProc ;

class function TKeysConfig.TKey.getKey(Acaption:string; Acas:TCtrlAltShift; Akey:Word):TKey ;
begin
  Result.cas:=Acas ;
  Result.key:=Akey ;
  Result.caption:=Acaption ;
end;

function TKeysConfig.TKey.isMatch(ss:TShiftState; Akey:Word):Boolean ;
begin
  Result:=True ;
  if cas=sNo then Result:=(not (ssCtrl in ss)) and (not (ssAlt in ss)) and (not (ssShift in ss)) ;
  if cas=sCtrl then Result:=ssCtrl in ss ;
  if cas=sAlt then Result:=ssAlt in ss ;
  if cas=sShift then Result:=ssShift in ss ;

  Result:=Result and(key=Akey) ;
end;

{ TKeysConfig }

constructor TKeysConfig.Create;
var i:Integer ;
begin
  keys:=TDictionary<string,TKey>.Create() ;
  orderedkeys:=TStringList.Create ;
  casstrings:=TStringList.Create() ;
  casstrings.Add('-') ;
  casstrings.Add('Ctrl') ;
  casstrings.Add('Alt') ;
  casstrings.Add('Shift') ;
  keystrings:=TStringList.Create() ;
  for i:= 0 to 11 do
    keystrings.Add(Format('%d=F%d',[VK_F1+i,i+1])) ;
  for i:= 0 to 9 do
    keystrings.Add(Format('%d=%d',[ord('0')+i,i])) ;
  for i:= 0 to 25 do
    keystrings.Add(Format('%d=%s',[ord('A')+i,chr(ord('A')+i)])) ;
  keystrings.Add(Format('%d=Escape',[VK_ESCAPE])) ;
  keystrings.Add(Format('%d=Insert',[VK_INSERT])) ;
  keystrings.Add(Format('%d=Delete',[VK_DELETE])) ;
  keystrings.Add(Format('%d=Home',[VK_INSERT])) ;
  keystrings.Add(Format('%d=End',[VK_END])) ;
  keystrings.Add(Format('%d=PageUp',[VK_PRIOR])) ;
  keystrings.Add(Format('%d=PageDown',[VK_NEXT])) ;
  keystrings.Add(Format('%d=Space',[VK_SPACE])) ;
  keystrings.Add(Format('%d=Enter',[VK_RETURN])) ;
  keystrings.Add(Format('%d=Backspace',[VK_BACK])) ;
  keystrings.Add(Format('%d=Tab',[VK_TAB])) ;
  keystrings.Add(Format('%d=+',[VK_ADD])) ;
  keystrings.Add(Format('%d=-',[VK_DECIMAL])) ;
  keystrings.Add(Format('%d=/',[VK_DIVIDE])) ;
  keystrings.Add(Format('%d=*',[VK_MULTIPLY])) ;
end;

destructor TKeysConfig.Destroy;
begin
  keys.Free ;
  orderedkeys.Free ;
  casstrings.Free ;
  inherited Destroy ;
end;

function TKeysConfig.getAllCodes: TStringList;
//var code:string ;
begin
  Result:=TStringList.Create() ;
  Result.Assign(orderedkeys);
//  for code in keys.Keys do
//    Result.Add(code) ;
end;

function TKeysConfig.getCASStrings: TStringList;
begin
  Result:=casstrings ;
end;

function TKeysConfig.getCSA(code: string): TCtrlAltShift;
begin
  if keys.ContainsKey(code) then Result:=keys[code].cas else Result:=sNo ;
end;

function TKeysConfig.getKey(code: string): Word;
begin
  if keys.ContainsKey(code) then Result:=keys[code].key else Result:=0 ;
end;

function TKeysConfig.getKeyCaption(code: string): string;
begin
  if keys.ContainsKey(code) then Result:=keys[code].Caption else Result:='' ;
end;

function TKeysConfig.getKeyStrings: TStringList;
begin
  Result:=keystrings ;
end;

function TKeysConfig.getKeyView(code: string): string;
begin
  if not keys.ContainsKey(code) then Exit('Нет ключа') ;

  Result:='' ;
  if keys[code].cas=sCtrl then Result:='Ctrl+' ;
  if keys[code].cas=sAlt then Result:='Alt+' ;
  if keys[code].cas=sShift then Result:='Shift+' ;

  Result:=Result+keystrings.Values[IntToStr(keys[code].key)] ;
end;

function TKeysConfig.isKeyMatch(code: string; ss: TShiftState;
  key: Word): Boolean;
begin
  if not keys.ContainsKey(code) then Exit(False) ;

  Result:=keys[code].isMatch(ss,key) ;
end;

function TKeysConfig.isValidKeys: Boolean;
var code1,code2:string ;
begin
  for code1 in keys.Keys do
    for code2 in keys.Keys do
      if code1<>code2 then
        if (keys[code1].cas=keys[code2].cas)and
           (keys[code1].key=keys[code2].key) then Exit(False) ;
  Result:=True ;
end;

procedure TKeysConfig.setKey(code: string; cas:TCtrlAltShift; key: Word);
var k:TKey ;
begin
  if code='' then Exit() ;
  if not keys.ContainsKey(code) then Exit() ;

  k:=keys[code] ;
  k.cas:=cas ;
  k.key:=key ;
  keys[code]:=k ;
end;

procedure TKeysConfig.addKey(code, caption: string; cas:TCtrlAltShift; key: Word);
begin
  if code='' then Exit ;

  orderedkeys.Add(code) ;
  keys.Add(code,TKey.getKey(caption,cas,key)) ;
end;

procedure TKeysConfig.loadFromFile;
var list:TStringList ;
    sect:string ;
begin
  list:=TStringList.Create() ;
  with TIniFile.Create(AppPath+'\keys.ini') do begin
    ReadSections(list) ;
    for sect in list do
      setKey(sect,
             TCtrlAltShift(ReadInteger(sect,'Cas',0)),
             ReadInteger(sect,'Key',0)) ;
    Free ;
  end;
  list.Free ;
end;

procedure TKeysConfig.saveToFile;
var code:string ;
    k:TCtrlAltShift ;
    w:Word ;
begin
  k:=TCtrlAltShift(0) ;
  w:=Word(k) ;
  with TIniFile.Create(AppPath+'\keys.ini') do begin
    for code in keys.Keys do begin
      WriteInteger(code,'Cas',ord(keys[code].cas)) ;
      WriteInteger(code,'Key',keys[code].key) ;
    end;
    Free ;
  end;

end;

end.
