unit UECKey;

interface

uses
  SysUtils,
  ClpIECC,
  ClpIDsa,
  ClpX9ECC,
  ClpIDigest,
  ClpBigInteger,
  ClpECDsaSigner,
  ClpIECDsaSigner,
  ClpECAlgorithms,
  ClpCryptoLibTypes,
  ClpDigestUtilities,
  ClpIX9ECParameters,
  ClpIECKeyParameters,
  ClpSecP256K1Custom,
  ClpISecP256K1Custom,
  ClpECKeyPairGenerator,
  ClpCustomNamedCurves,
  ClpECDomainParameters,
  ClpHMacDsaKCalculator,
  ClpIHMacDsaKCalculator,
  ClpIECDomainParameters,
  ClpECPublicKeyParameters,
  ClpIECPublicKeyParameters,
  ClpECPrivateKeyParameters,
  ClpIECPrivateKeyParameters;

type
  IECKey = interface(IInterface)
    ['{B861A90F-C5B7-43CF-BEBC-DEE5C953B43A}']
    function GetPrivateKey: IECPrivateKeyParameters;
    property PrivateKey: IECPrivateKeyParameters read GetPrivateKey;
    function GetPublicKey: IECPublicKeyParameters;
    property PublicKey: IECPublicKeyParameters read GetPublicKey;
    function GetDomainParameter: IECDomainParameters;
    property DomainParameter: IECDomainParameters read GetDomainParameter;

    function GetPrivateKeyAsBytes(): TBytes;
    function GetPublicKeyAsBytes(Compressed: Boolean): TBytes;
  end;

type
  TECKey = class(TInterfacedObject, IECKey)
  strict private
  var
    FKey: IECKeyParameters;
    function GetPrivateKey: IECPrivateKeyParameters; inline;
    function GetPublicKey: IECPublicKeyParameters; inline;
    function GetDomainParameter: IECDomainParameters; inline;
    function GetPublicKeyParameters(): IECPublicKeyParameters; inline;

  class var

    FSecp256k1: IX9ECParameters;
    FCurve, FDomainParameter: IECDomainParameters;
    FCurveOrder, FHalfCurveOrder: TBigInteger;

    class constructor ECKey();

  public
    property PrivateKey: IECPrivateKeyParameters read GetPrivateKey;
    property PublicKey: IECPublicKeyParameters read GetPublicKey;
    property DomainParameter: IECDomainParameters read GetDomainParameter;

    class property Secp256k1: IX9ECParameters read FSecp256k1;
    class property Curve: IECDomainParameters read FCurve;
    class property CurveOrder: TBigInteger read FCurveOrder;
    class property HalfCurveOrder: TBigInteger read FHalfCurveOrder;

    constructor Create(const vch: TBytes; isPrivate: Boolean);

    function GetPrivateKeyAsBytes(): TBytes; inline;
    function GetPublicKeyAsBytes(Compressed: Boolean): TBytes; inline;

  end;

implementation

{ TECKey }

class constructor TECKey.ECKey;
begin
  FSecp256k1 := TCustomNamedCurves.TSecP256K1Holder.Instance.Parameters;
  FCurve := TECDomainParameters.Create(FSecp256k1.Curve, FSecp256k1.G,
    FSecp256k1.N, FSecp256k1.H);
  FHalfCurveOrder := FSecp256k1.N.ShiftRight(1);
  FCurveOrder := FSecp256k1.N;
end;

constructor TECKey.Create(const vch: TBytes; isPrivate: Boolean);
var
  q: IECPoint;
begin
  Inherited Create();
  if (isPrivate) then
  begin
    FKey := TECPrivateKeyParameters.Create(TBigInteger.Create(1, vch),
      DomainParameter);
  end
  else
  begin
    q := FSecp256k1.Curve.DecodePoint(vch);
    FKey := TECPublicKeyParameters.Create('ECDSA', q, DomainParameter);
  end;
end;

function TECKey.GetDomainParameter: IECDomainParameters;
begin
  if (FDomainParameter = Nil) then
  begin
    FDomainParameter := TECDomainParameters.Create(FSecp256k1.Curve,
      FSecp256k1.G, FSecp256k1.N, FSecp256k1.H);
  end;
  Result := FDomainParameter;
end;

function TECKey.GetPrivateKey: IECPrivateKeyParameters;
begin
  Result := FKey as IECPrivateKeyParameters;
end;

function TECKey.GetPublicKeyParameters: IECPublicKeyParameters;
begin
  if Supports(FKey, IECPublicKeyParameters) then
  begin
    Result := FKey as IECPublicKeyParameters;
  end
  else
  begin
    Result := TECKeyPairGenerator.GetCorrespondingPublicKey(PrivateKey);
  end;
end;

function TECKey.GetPublicKey(): IECPublicKeyParameters;
begin
  Result := GetPublicKeyParameters();
end;

function TECKey.GetPrivateKeyAsBytes: TBytes;
begin
  Result := PrivateKey.D.ToByteArrayUnsigned;
end;

function TECKey.GetPublicKeyAsBytes(Compressed: Boolean): TBytes;
begin
  Result := GetPublicKeyParameters().q.GetEncoded(Compressed);
end;

end.
