unit UBitcoinKey;

interface

uses
  Classes,
  SysUtils,
  UECKey,
  ClpEncoders,
  ClpBigInteger,
  ClpArrayUtils,
  ClpSecureRandom,
  ClpISecureRandom,
  ClpDigestUtilities,
  ClpIECPublicKeyParameters,
  ClpIECPrivateKeyParameters;

resourcestring
  SInvalidECKeySize = 'The size of an EC key should be %d but ACount is %d';
  SInvalidECKey = 'Invalid EC Key';

type
  IBitcoinKey = interface(IInterface)
    ['{5CEAC001-F1A9-426F-A770-1E2D60BDDCB6}']
    function GeneratePublicAddress(Compressed: Boolean = True): String;
    function GenerateWIFPrivateKey(Compressed: Boolean = True): String;
    function GetPrivateKey: IECPrivateKeyParameters;
    function GetPublicKey: IECPublicKeyParameters;
    function GetIsCompressed: Boolean;
    property IsCompressed: Boolean read GetIsCompressed;
    property PrivateKey: IECPrivateKeyParameters read GetPrivateKey;
    property PublicKey: IECPublicKeyParameters read GetPublicKey;
  end;

type
  TBitcoinKey = class sealed(TInterfacedObject, IBitcoinKey)

  strict private
  const
    ValidKeySize = 32;
    VersionByte = '00';
    WIFPrivateKeyHeader = '80';
    CompressedWIFPrivateKeyMarker = '01';

  var
    FECKey: IECKey;
    FIsCompressed: Boolean;

    class var

      FSecureRandom: ISecureRandom;

    function GetIsCompressed: Boolean; inline;
    function GetPrivateKey: IECPrivateKeyParameters; inline;
    function GetPublicKey: IECPublicKeyParameters; inline;

    class function ValidateScalarKey(const AData: TBytes): Boolean;
      static; inline;

    class function DoubleSHA256Hash(const AMessage: TBytes): TBytes;
      static; inline;

    class constructor BitcoinKey();

  public
    constructor Create(); overload;
    constructor Create(ACompressed: Boolean); overload;
    constructor Create(const AData: TBytes; ACount: Int32 = -1;
      ACompressed: Boolean = True); overload;

    function GeneratePublicAddress(Compressed: Boolean = True): String;
    function GenerateWIFPrivateKey(Compressed: Boolean = True): String;

    property IsCompressed: Boolean read GetIsCompressed;
    property PrivateKey: IECPrivateKeyParameters read GetPrivateKey;
    property PublicKey: IECPublicKeyParameters read GetPublicKey;
    class property SecureRandom: ISecureRandom read FSecureRandom;

    class function GenerateValidRandomBytesForPrivateKey(): TBytes; static;
  end;

implementation

{ TBitcoinKey }

function TBitcoinKey.GetIsCompressed: Boolean;
begin
  Result := FIsCompressed;
end;

function TBitcoinKey.GetPrivateKey: IECPrivateKeyParameters;
begin
  Result := FECKey.PrivateKey;
end;

function TBitcoinKey.GetPublicKey: IECPublicKeyParameters;
begin
  Result := FECKey.PublicKey;
end;

class function TBitcoinKey.ValidateScalarKey(const AData: TBytes): Boolean;
var
  D: TBigInteger;
begin
  Result := True;
  D := TBigInteger.Create(1, AData);
  if ((D.CompareTo(TBigInteger.One) < 0) or (D.CompareTo(TECKey.Secp256k1.N)
    >= 0)) then
  begin
    Exit(False);
  end;
end;

constructor TBitcoinKey.Create();
begin
  Create(True);
end;

constructor TBitcoinKey.Create(ACompressed: Boolean);
var
  data: TBytes;
begin
  Inherited Create();
  data := GenerateValidRandomBytesForPrivateKey();
  FIsCompressed := ACompressed;
  FECKey := TECKey.Create(data, True);
end;

constructor TBitcoinKey.Create(const AData: TBytes; ACount: Int32;
  ACompressed: Boolean);
begin
  if (ACount = -1) then
    ACount := System.Length(AData);

  if (ACount <> ValidKeySize) then
  begin
    raise EArgumentException.CreateResFmt(@SInvalidECKeySize,
      [ValidKeySize, ACount]);
  end;

  if (ValidateScalarKey(AData)) then
  begin
    FIsCompressed := ACompressed;
    FECKey := TECKey.Create(System.Copy(AData, 0, ACount), True);
  end
  else
    raise EArgumentException.CreateRes(@SInvalidECKey);
end;

class function TBitcoinKey.DoubleSHA256Hash(const AMessage: TBytes): TBytes;
begin
  Result := TDigestUtilities.CalculateDigest('SHA256',
    TDigestUtilities.CalculateDigest('SHA256', AMessage));
end;

function TBitcoinKey.GeneratePublicAddress(Compressed: Boolean): String;
var
  Hash, WithVersionByte, Checksum: TBytes;
begin
  Hash := TDigestUtilities.CalculateDigest('RIPEMD160',
    TDigestUtilities.CalculateDigest('SHA256',
    FECKey.PublicKey.Q.GetEncoded(Compressed)));
  WithVersionByte := TArrayUtils.Concatenate(THex.Decode(VersionByte), Hash);
  Hash := DoubleSHA256Hash(WithVersionByte);
  Checksum := System.Copy(Hash, 0, 4);
  Result := TBase58.Encode(TArrayUtils.Concatenate(WithVersionByte, Checksum));
end;

class function TBitcoinKey.GenerateValidRandomBytesForPrivateKey: TBytes;
begin
  System.SetLength(Result, ValidKeySize);
  repeat
    FSecureRandom.NextBytes(Result);
  until (ValidateScalarKey(Result));
end;

function TBitcoinKey.GenerateWIFPrivateKey(Compressed: Boolean): String;
var
  PrivateKeyBytes, Checksum, Hash, WithVersionByte: TBytes;
begin
  PrivateKeyBytes := FECKey.GetPrivateKeyAsBytes;
  WithVersionByte := TArrayUtils.Concatenate(THex.Decode(WIFPrivateKeyHeader),
    PrivateKeyBytes);
  if Compressed then
  begin
    WithVersionByte := TArrayUtils.Concatenate(WithVersionByte,
      THex.Decode(CompressedWIFPrivateKeyMarker));
  end;
  Hash := DoubleSHA256Hash(WithVersionByte);
  Checksum := System.Copy(Hash, 0, 4);
  Result := TBase58.Encode(TArrayUtils.Concatenate(WithVersionByte, Checksum));
end;

class constructor TBitcoinKey.BitcoinKey;
begin
  FSecureRandom := TSecureRandom.Create();
end;

end.
