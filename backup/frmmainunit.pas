unit frmMainunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, btckeyfunctions, ClpEncoders, Clipbrd, MaskEdit, Spin, EditBtn,
  USha256;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnBrainpvtKey: TButton;
    btnRNDpvtKey: TButton;
    btnComputePrvKeyDet: TButton;
    btnComputePubKeyDet: TButton;
    btnBulkGen: TButton;
    btnCancel: TButton;
    btnSaveBulk: TButton;
    edtBulkAmount: TEdit;
    edtBTCaddrCompr: TLabeledEdit;
    edtBTCaddress: TLabeledEdit;
    edtPubKey: TLabeledEdit;
    edtPubKeyComp: TLabeledEdit;
    edtPassPhrase: TLabeledEdit;
    edtPvtKeyB64: TLabeledEdit;
    edtPvtKeyHex: TLabeledEdit;
    edtPvtKeyWIF: TLabeledEdit;
    edtPvtKey: TLabeledEdit;
    edtPvtKeyWIFComp: TLabeledEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    memBulk: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    dlgSave: TSaveDialog;
    StatusBar: TStatusBar;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    tmrKeyPerSec: TTimer;
    procedure btnBrainpvtKeyClick(Sender: TObject);
    procedure btnComputePrvKeyDetClick(Sender: TObject);
    procedure btnComputePubKeyDetClick(Sender: TObject);
    procedure btnRNDpvtKeyClick(Sender: TObject);
    procedure btnBulkGenClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnSaveBulkClick(Sender: TObject);
    procedure edtBulkAmountKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure DblClick2CLPBRD(Sender: TObject);
    procedure edtDbgHintLength(Sender: TObject);
    procedure memBulkChange(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure tmrKeyPerSecTimer(Sender: TObject);

  private

  public




  end;


function ByteToString(const Value: TBytes): string;
function StrToByte(const Value: string): TBytes;
function ByteToHexString(input: TBytes): string;


var
  frmMain: TfrmMain;
  IsPRVkeyInputValid: boolean;
  KeyPair: TKeyPair;
  forcedCancel: boolean;
  keysGenPerSec: integer;


implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.btnBrainpvtKeyClick(Sender: TObject);
var
  sbt: TBytes;
  astr: string;

begin

  edtPvtKey.Text := SHA256ToStr(CalcSHA256(edtPassPhrase.Text));
  IsPRVkeyInputValid := True;
  // i will need to create some function or procedure to actually validate input
  // for now i will just tell it that the private key is valid
  if IsPRVkeyInputValid = True then
  begin

    // write some code to get public key from this generated private key (brain wallet)
    // KeyPair := TbtcKeyFunctions.GenPubKeyFromPvtInput(TKeyType.SECP256K1);
    // now I am stuck... how do I tell this function what my input private key is?

    //TbtcKeyFunctions.GenPubKeyFromPvtInput();
    btnBrainpvtKey.Enabled := False;
    btnRNDpvtKey.Enabled := False;
    btnComputePrvKeyDet.Enabled := True;
    btnComputePubKeyDet.Enabled := True;
  end;

end;

procedure TfrmMain.btnComputePrvKeyDetClick(Sender: TObject);
begin
  edtPvtKeyHex.Text := edtPvtKey.Text;

  edtPvtKeyB64.Text := TBase64.Encode(THex.Decode(edtPvtKeyHex.Text));
  edtPvtKeyWIF.Text := TbtcKeyFunctions.GetPrivateKeyWIF(edtPvtKeyHex.Text, False);
  edtPvtKeyWIFComp.Text := TbtcKeyFunctions.GetPrivateKeyWIF(edtPvtKeyHex.Text, True);
  btnComputePrvKeyDet.Enabled := False;
  if (btnComputePubKeyDet.Enabled = False) and (btnComputePrvKeyDet.Enabled = False) then
  begin
    btnRNDpvtKey.Enabled := True;
    btnBrainpvtKey.Enabled := True;
  end;
end;

procedure TfrmMain.btnComputePubKeyDetClick(Sender: TObject);
var
  Address, PubKey: String;
begin
  TbtcKeyFunctions.GetPublicKeyDetails(edtPvtKey.Text, False, Address, PubKey);
  edtBTCaddress.Text := Address;
  edtPubKey.Text := PubKey;
  TbtcKeyFunctions.GetPublicKeyDetails(edtPvtKey.Text, True, Address, PubKey);
  edtBTCaddrCompr.Text := Address;
  edtPubKeyComp.Text := PubKey;
  btnComputePubKeyDet.Enabled := False;
  if (btnComputePubKeyDet.Enabled = False) and
    (btnComputePrvKeyDet.Enabled = False) then
  begin
    btnRNDpvtKey.Enabled := True;
    btnBrainpvtKey.Enabled := True;
  end;
end;

procedure TfrmMain.btnRNDpvtKeyClick(Sender: TObject);
begin

  edtPvtKey.Text := TbtcKeyFunctions.GenerateValidRandomBytesForPrivateKey();

  edtPvtKey.Hint := IntToStr(length(edtPvtKey.Text)) +
    ' Characters Counted (debuging hint)';

  IsPRVkeyInputValid := True;



  // add checks to see if valid private key is generated
  // then enable "compute below" buttons
  //  will obviously need some validation code written in future

  if IsPRVkeyInputValid = True then
  begin
    btnComputePrvKeyDet.Enabled := True;
    btnComputePubKeyDet.Enabled := True;
    btnRNDpvtKey.Enabled := False;
    btnBrainpvtKey.Enabled := False;
    btnRNDpvtKey.Caption := 'Generate New Random Private Key (More Secure)';
  end;

end;

procedure TfrmMain.btnBulkGenClick(Sender: TObject);
var
  qty: integer;
  i: integer;
begin

  // playing with idea of counting keys per second
  keysGenPerSec := 0;
  tmrKeyPerSec.Enabled := True;


  qty := StrToInt(edtBulkAmount.Text);
  memBulk.Lines.Clear;
  forcedCancel := False;
  edtBulkAmount.Enabled:=false;
  btnBulkGen.Visible := False;  // so user doesnt keep clicking if generating large list



  for i := 1 to qty do
  begin
    if forcedCancel = True then
      exit;

    KeyPair := TbtcKeyFunctions.GenerateECKeyPair(TKeyType.SECP256K1);
    memBulk.Lines.Add(ByteToHexString(KeyPair.PrivateKey) + ' , ' +
      ByteToHexString(KeyPair.PublicKey));


    // make the memo scroll as it is populated... just looks cool i guess
    //
    memBulk.SelStart := memBulk.GetTextLen;
    memBulk.SelLength := 0;
    memBulk.ScrollBy(0, memBulk.Lines.Count);
    memBulk.Refresh;


    // just toying with generating mass keys and counting how many per second using a timer
    Inc(keysGenPerSec, 1);


    application.ProcessMessages;
    // so if user does a ridculous amount of keys form will stay responsive and can click cancel

  end;

  keysGenPerSec := 0;
  StatusBar.Panels.Items[0].Text := '0/Keys Per Second';
  tmrKeyPerSec.Enabled := False; // stop timer... no point in keeping it enabled
  btnBulkGen.Visible := True;  // re-enables so user can smash all day long... whatever
  edtBulkAmount.Enabled:=true;

end;

procedure TfrmMain.btnCancelClick(Sender: TObject);
begin
  forcedCancel := True;
  btnBulkGen.Visible := True;  // re-enables so user can smash all day long... whatever

  keysGenPerSec := 0;
  StatusBar.Panels.Items[0].Text := '0/Keys Per Second';
  tmrKeyPerSec.Enabled := False;
  edtBulkAmount.Enabled:=true;

end;

procedure TfrmMain.btnSaveBulkClick(Sender: TObject);
var
fileout: TFileName;
begin

  if dlgSave.Execute then begin

     dlgSave.Options:=[ofOverwritePrompt];
     memBulk.Lines.SaveToFile(dlgSave.FileName);
    end;



end;


procedure TfrmMain.edtBulkAmountKeyPress(Sender: TObject; var Key: char);
begin


     if (key = #13) and (StrToInt(edtBulkAmount.Text) > 0) then
     begin
          btnBulkGen.Click;
          abort;
     end;

     // #8 is Backspace
  if not (Key in [#8, '0'..'9']) then begin
    ShowMessage('Invalid key');
    // Discard the key
    Key := #0;
  end;


end;

procedure TfrmMain.DblClick2CLPBRD(Sender: TObject);
var
  theText: TLabeledEdit;
begin
  theText := Sender as TLabeledEdit;
  clipboard.AsText := theText.Text;
end;


procedure TfrmMain.edtDbgHintLength(Sender: TObject);
var
  theDbgHint: TLabeledEdit;
begin
  theDbgHint := Sender as TLabeledEdit;
  theDbgHint.Hint := IntToStr(length(theDbgHint.Text)) +
    ' Characters Counted (debuging hint)';
end;

procedure TfrmMain.memBulkChange(Sender: TObject);
begin
  btnSaveBulk.Enabled:=true;
end;

procedure TfrmMain.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePageIndex = 1 then
    StatusBar.Panels.Items[0].Text := '0/Keys Per Second';

  if PageControl1.ActivePageIndex = 0 then
    StatusBar.Panels.Items[0].Text :=
      'Double click any field to copy to clip board.  Hover over addresses for QR Code.';

  if PageControl1.ActivePageIndex = 2 then
    StatusBar.Panels.Items[0].Text :=
      'Yeah... its a pointless tool so far... Maybe some day it will be useful and cool!';

end;

procedure TfrmMain.tmrKeyPerSecTimer(Sender: TObject);
begin
  StatusBar.Panels.Items[0].Text := IntToStr(keysGenPerSec) + '/Keys Per Second';
  keysGenPerSec := 0;
  // set back to 0 so the next time this executes in 1000ms it should be how many times the keygen looped.... lets try
end;




procedure TfrmMain.FormCreate(Sender: TObject);
begin
  PageControl1Change(self);
  Application.ShowHint := True;
end;




function ByteToHexString(input: TBytes): string;
var
  index: Int32;
begin
  Result := '';
  for index := System.Low(input) to System.High(input) do
  begin
    Result := Result + IntToHex(input[index], 2);
  end;

end;

function ByteToString(const Value: TBytes): string;
var
  I: integer;
  S: string;
  Letra: char;
begin
  S := '';
  for I := Length(Value) - 1 downto 0 do
  begin
    letra := Chr(Value[I]);
    S := letra + S;
  end;
  Result := S;
end;

function StrToByte(const Value: string): TBytes;
var
  I: integer;
begin
  SetLength(Result, Length(Value));
  for I := 0 to Length(Value) - 1 do
    Result[I] := Ord(Value[I + 1]);
end;


end.









