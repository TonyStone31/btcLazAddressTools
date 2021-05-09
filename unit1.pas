unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnBrainpvtKey: TButton;
    btnRNDpvtKey: TButton;
    Button1: TButton;
    Button2: TButton;
    edtBTCaddrCompr: TLabeledEdit;
    edtBTCaddress: TLabeledEdit;
    edtPassPhrase: TEdit;
    edtPubKey: TLabeledEdit;
    edtPubKeyComp: TLabeledEdit;
    edtPvtKey: TEdit;
    edtPvtKeyB64: TLabeledEdit;
    edtPvtKeyHex: TLabeledEdit;
    edtPvtKeyWIF: TLabeledEdit;
    edtPvtKeyWIFComp: TLabeledEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    StatusBar1: TStatusBar;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure btnBrainpvtKeyClick(Sender: TObject);
  private

  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.btnBrainpvtKeyClick(Sender: TObject);
begin

end;

end.

