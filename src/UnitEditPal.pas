unit UnitEditPal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons;

type
  TFormEditPal = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    MemoPal: TMemo;
    Label1: TLabel;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    pal:TStringList ;
  public
    function getPal():TStrings ;
    procedure setPal(pal:TStrings) ;
  end;

implementation
uses Palette ;

{$R *.dfm}

procedure TFormEditPal.BitBtn1Click(Sender: TObject);
begin
  with TPalette.Create(MemoPal.Lines) do begin
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

procedure TFormEditPal.FormCreate(Sender: TObject);
begin
  pal:=TStringList.Create() ;
end;

procedure TFormEditPal.FormDestroy(Sender: TObject);
begin
  pal.Free ;
end;

function TFormEditPal.getPal: TStrings;
begin
  Result:=pal ;
end;

procedure TFormEditPal.setPal(pal: TStrings);
begin
  MemoPal.Lines.Assign(pal);
end;

end.
