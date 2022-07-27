unit UnitTranspTexs;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons;

type
  TFormTranspTexs = class(TForm)
    Label1: TLabel;
    MemoTexs: TMemo;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    pal:TStringList ;
  public
    procedure setTexs(pal: string);
    function getTexs: string;
  end;

implementation
uses Palette ;

{$R *.dfm}

procedure TFormTranspTexs.BitBtn1Click(Sender: TObject);
begin
  with TPalette.Create(MemoTexs.Lines) do begin
    pal.Assign(getList());
    Free ;
  end;

  if pal.Count=0 then begin
    ShowMessage('Не введено ни одной текстуры или все они не существуют в каталоге') ;
    Exit ;
  end;

  ModalResult:=mrOK ;
  Hide() ;

end;

procedure TFormTranspTexs.FormCreate(Sender: TObject);
begin
  pal:=TStringList.Create() ;
end;

procedure TFormTranspTexs.FormDestroy(Sender: TObject);
begin
  pal.Free ;
end;

function TFormTranspTexs.getTexs: string;
begin
  Result:=pal.CommaText ;
end;

procedure TFormTranspTexs.setTexs(pal: string);
begin
  MemoTexs.Lines.CommaText:=pal ;
end;

end.
