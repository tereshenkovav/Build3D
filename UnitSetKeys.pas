unit UnitSetKeys;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  KeysConfig ;

type
  TFormSetKeys = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    procedure BitBtn1Click(Sender: TObject);
  private
    kc:TKeysConfig ;
    combo_csa:array of TComboBox ;
    combo_keys:array of TComboBox ;
  public
    procedure setKeysConfig(kc:TKeysConfig) ;
  end;

implementation

{$R *.dfm}

{ TFormSetKeys }

procedure TFormSetKeys.BitBtn1Click(Sender: TObject);
var i,j:Integer ;
    codes:TStringList ;
begin
  codes:=kc.getAllCodes() ;
  for i:=0 to codes.Count-1 do
    kc.setKey(codes[i],
      TCtrlAltShift(combo_csa[i].ItemIndex),
      StrToInt(kc.getKeyStrings().Names[combo_keys[i].ItemIndex]));
  codes.Free ;
  if not kc.isValidKeys then begin
    ShowMessage('Найдены повторяющиеся комбинации клавиш') ;
    Exit ;
  end ;
  ModalResult:=mrOk ;
  Hide() ;
end;

procedure TFormSetKeys.setKeysConfig(kc: TKeysConfig);
var i,j:Integer ;
    codes:TStringList ;
    lab:TLabel ;
    combo:TComboBox ;
const
    STEP=30 ;
    SPACE=40 ;
    X1=400 ;
    X2=540 ;
begin
  Self.kc:=kc ;

  lab:=TLabel.Create(Self);
  lab.Parent:=Self ;
  lab.Left:=X1 ;
  lab.Top:=10 ;
  lab.AutoSize:=True ;
  lab.Caption:='Модификатор' ;
  lab.Font.Style:=[fsBold] ;
  lab.Font.Size:=12 ;

  lab:=TLabel.Create(Self);
  lab.Parent:=Self ;
  lab.Left:=X2 ;
  lab.Top:=10 ;
  lab.AutoSize:=True ;
  lab.Caption:='Клавиша' ;
  lab.Font.Style:=[fsBold] ;
  lab.Font.Size:=12 ;

  codes:=kc.getAllCodes() ;
  SetLength(combo_csa,codes.Count) ;
  SetLength(combo_keys,codes.Count) ;
  for i:=0 to codes.Count-1 do begin
    lab:=TLabel.Create(Self);
    lab.Parent:=Self ;
    lab.Left:=10 ;
    lab.Top:=i*STEP+SPACE ;
    lab.AutoSize:=True ;
    lab.Caption:=kc.getKeyCaption(codes[i]) ;
    lab.Font.Style:=[fsBold] ;
    lab.Font.Size:=12 ;

    combo:=TComboBox.Create(Self) ;
    combo.Parent:=Self ;
    combo.Items.Assign(kc.getCASStrings());
    combo.ItemIndex:=ord(kc.getCSA(codes[i])) ;
    combo.Style:=csDropDownList ;
    combo.Left:=X1 ;
    combo.Width:=100 ;
    combo.Top:=i*STEP+SPACE ;
    combo.Font.Style:=[fsBold] ;
    combo.Font.Size:=12 ;
    combo_csa[i]:=combo ;

    combo:=TComboBox.Create(Self) ;
    combo.Parent:=Self ;
    for j := 0 to kc.getKeyStrings().Count-1 do
      combo.Items.Add(kc.getKeyStrings().ValueFromIndex[j]) ;
    combo.ItemIndex:=kc.getKeyStrings().IndexOfName(IntToStr(kc.getKey(codes[i]))) ;
    combo.Style:=csDropDownList ;
    combo.Left:=X2 ;
    combo.Width:=100 ;
    combo.Top:=i*STEP+SPACE ;
    combo.Font.Style:=[fsBold] ;
    combo.Font.Size:=12 ;
    combo.DropDownCount:=16 ;
    combo_keys[i]:=combo ;
  end;
  codes.Free ;
end;

end.
