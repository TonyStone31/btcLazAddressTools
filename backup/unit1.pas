unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, btckeyfunctions, ClpEncoders, Clipbrd, USha256;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnBrainpvtKey: TButton;
    btnRNDpvtKey: TButton;
    btnComputePrvKeyDet: TButton;
    btnComputePubKeyDet: TButton;
    btnBulkGen: TButton;
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
    Label5: TLabel;
    Label6: TLabel;
    memBulk: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    StatusBar1: TStatusBar;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure btnBrainpvtKeyClick(Sender: TObject);
    procedure btnComputePrvKeyDetClick(Sender: TObject);
    procedure btnComputePubKeyDetClick(Sender: TObject);
    procedure btnRNDpvtKeyClick(Sender: TObject);
    procedure btnBulkGenClick(Sender: TObject);
    procedure edtBTCaddrComprChange(Sender: TObject);

    procedure edtBTCaddressChange(Sender: TObject);

    procedure edtPassPhraseChange(Sender: TObject);

    procedure edtPubKeyChange(Sender: TObject);
    procedure edtPubKeyCompChange(Sender: TObject);


    procedure edtPvtKeyB64Change(Sender: TObject);


    procedure edtPvtKeyChange(Sender: TObject);

    procedure edtPvtKeyHexChange(Sender: TObject);

    procedure edtPvtKeyWIFCompChange(Sender: TObject);

    procedure FormCreate(Sender: TObject);
    procedure DblClick2CLPBRD(Sender: TObject);

  private

  public




  end;


    function ByteToString(const Value: TBytes): String;
    function StrToByte(const Value: String): TBytes;
    function ByteToHexString(input: TBytes): String;


var
frmMain: TfrmMain;
IsPRVkeyInputValid: Boolean;
KeyPair: TKeyPair;


implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.btnBrainpvtKeyClick(Sender: TObject);
var
sbt: TBytes;
astr : string;

begin

  edtPvtKey.Text:=SHA256ToStr(CalcSHA256(edtPassPhrase.text));
  IsPRVkeyInputValid:=true; // i will need to create some function or procedure to actually validate input
                      // for now i will just tell it that the private key is valid
  if IsPRVkeyInputValid = true then
     begin

       // write some code to get public key from this generated private key (brain wallet)
       // KeyPair := TbtcKeyFunctions.GenPubKeyFromPvtInput(TKeyType.SECP256K1);
        // now I am stuck... how do I tell this function what my input private key is?

        //TbtcKeyFunctions.GenPubKeyFromPvtInput();
        btnBrainpvtKey.Enabled:=false;
        btnRNDpvtKey.enabled:=false;
        btnComputePrvKeyDet.enabled:=true;
        btnComputePubKeyDet.Enabled:=true;
  end;

end;

procedure TfrmMain.btnComputePrvKeyDetClick(Sender: TObject);
begin
  edtPvtKeyHex.Text:=edtPvtKey.Text;

  edtPvtKeyB64.Text:= TBase64.Encode(KeyPair.PrivateKey);
  edtPvtKeyWIFComp.Text:= TBase58.Encode(KeyPair.PrivateKey);
  btnComputePrvKeyDet.Enabled:=false;
  if (btnComputePubKeyDet.enabled = false) and (btnComputePrvKeyDet.enabled = false) then
     begin
       btnRNDpvtKey.enabled:= true;
       btnBrainpvtKey.enabled:=true;
    end;
end;

procedure TfrmMain.btnComputePubKeyDetClick(Sender: TObject);
begin
  edtPubKey.Text:= ByteToHexString(KeyPair.PublicKey);
  btnComputePubKeyDet.Enabled:=false;
    if (btnComputePubKeyDet.enabled = false) and (btnComputePrvKeyDet.enabled = false) then
     begin
       btnRNDpvtKey.enabled:= true;
       btnBrainpvtKey.enabled:=true;
    end;
end;

procedure TfrmMain.btnRNDpvtKeyClick(Sender: TObject);
begin

     KeyPair := TbtcKeyFunctions.GenerateECKeyPair(TKeyType.SECP256K1);
     edtPvtKey.Text:= ByteToHexString(KeyPair.PrivateKey);

     edtPvtKey.Hint:= IntToStr(length(edtPvtKey.Text)) + ' Characters Counted (debuging hint)';

     IsPRVkeyInputValid:=true;



     // add checks to see if valid private key is generated
     // then enable "compute below" buttons
     //  will obviously need some validation code written in future

     if IsPRVkeyInputValid = true then
     begin
          btnComputePrvKeyDet.enabled := true;
          btnComputePubKeyDet.Enabled:=true;
          btnRNDpvtKey.enabled:=false;
          btnBrainpvtKey.Enabled:=False;
          btnRNDpvtKey.Caption:='Generate New Random Private Key (More Secure)';
     end;



end;

procedure TfrmMain.btnBulkGenClick(Sender: TObject);
var
qty: integer;
i: Integer;
begin

     qty := StrToInt(edtBulkAmount.Text);
     memBulk.Lines.Clear;
     for i:= 1 to qty do
     begin
     KeyPair := TbtcKeyFunctions.GenerateECKeyPair(TKeyType.SECP256K1);
     memBulk.Lines.Add(ByteToHexString(KeyPair.PrivateKey) + ' , ' + ByteToHexString(KeyPair.PublicKey));
     end;


end;

procedure TfrmMain.edtBTCaddrComprChange(Sender: TObject);
begin
   edtBTCaddrCompr.Hint:= IntToStr(length(edtBTCaddrCompr.Text)) + ' Characters Counted (debuging hint)';
end;



procedure TfrmMain.edtBTCaddressChange(Sender: TObject);
begin
  edtBTCaddress.Hint:= IntToStr(length(edtBTCaddress.Text)) + ' Characters Counted (debuging hint)';
end;


procedure TfrmMain.edtPassPhraseChange(Sender: TObject);
begin
  edtPassPhrase.Hint:= IntToStr(length(edtPassPhrase.Text)) + ' Characters Counted (debuging hint)';
end;


procedure TfrmMain.edtPubKeyChange(Sender: TObject);
begin
     edtPubKey.Hint:= IntToStr(length(edtPubKey.Text)) + ' Characters Counted (debuging hint)';
end;

procedure TfrmMain.edtPubKeyCompChange(Sender: TObject);
begin
   edtPubKeyComp.Hint:= IntToStr(length(edtPubKeyComp.Text)) + ' Characters Counted (debuging hint)';
end;



procedure TfrmMain.edtPvtKeyB64Change(Sender: TObject);
begin
     edtPvtKeyB64.Hint:= IntToStr(length(edtPvtKeyB64.Text)) + ' Characters Counted (debuging hint)';
end;



procedure TfrmMain.edtPvtKeyChange(Sender: TObject);
begin
   edtPvtKey.Hint:= IntToStr(length(edtPvtKey.Text)) + ' Characters Counted (debuging hint)';
end;



procedure TfrmMain.edtPvtKeyHexChange(Sender: TObject);
begin
     edtPvtKeyHex.Hint:= IntToStr(length(edtPvtKeyHex.Text)) + ' Characters Counted (debuging hint)';
end;





procedure TfrmMain.DblClick2CLPBRD(Sender: TObject);
var
theText: TLabeledEdit;
begin
     theText:= sender as TLabeledEdit;
   clipboard.AsText:=theText.text;
end;






procedure TfrmMain.edtPvtKeyWIFCompChange(Sender: TObject);
begin
      edtPvtKeyWIFComp.Hint:= IntToStr(length(edtPvtKeyWIFComp.Text)) + ' Characters Counted (debuging hint)';
end;







procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Application.ShowHint:=true;
end;










function ByteToHexString(input: TBytes): String;
  var
  index: Int32;
begin
  result := '';
  for index := System.Low(input) to System.High(input) do
  begin
    result := result + IntToHex(input[index], 2);
  end;


 end;

function ByteToString(const Value: TBytes): String;
var
  I: integer;
  S : String;
  Letra: char;
begin
S := '';
for I := Length(Value)-1 Downto 0 do
   begin
   letra := Chr(Value[I]);
   S := letra + S;
   end;
Result := S;
end;

function StrToByte(const Value: String): TBytes;
var
  I: integer;
begin
SetLength(Result, Length(Value));
   for I := 0 to Length(Value) - 1 do
      Result[I] := ord(Value[I + 1]);
end;


end.

