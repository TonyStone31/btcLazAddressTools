unit btckeyfunctions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ClpEncoders,
  TypInfo,
  ClpISigner,
  ClpBigInteger,
  ClpISecureRandom,
  ClpSecureRandom,
  ClpSignerUtilities,
  ClpIX9ECParameters,
  ClpIECPublicKeyParameters,
  ClpIECPrivateKeyParameters,
  ClpIAsymmetricCipherKeyPair,
  ClpGeneratorUtilities,
  ClpCustomNamedCurves,
  ClpECPrivateKeyParameters,
  ClpECPublicKeyParameters,
  ClpIECDomainParameters,
  ClpECDomainParameters,
  ClpECKeyGenerationParameters,
  ClpIECKeyGenerationParameters,
  ClpIAsymmetricCipherKeyPairGenerator,
  UBitcoinKey;

type
  TKeyPair = record
    PublicKey: TBytes;
    PrivateKey: TBytes;
  end;

type
{$SCOPEDENUMS ON}
  TKeyType = (SECP256K1, SECP384R1, SECP521R1, SECT283K1);
{$SCOPEDENUMS OFF}

type

  { TbtcKeyFunctions }

  TbtcKeyFunctions = class sealed(TObject)

  strict private

  const

    SigningAlgorithm = 'SHA-1withECDSA';

    class var FSecureRandom: ISecureRandom;

    class function GetCurveFromKeyType(AKeyType: TKeyType): IX9ECParameters;
      static; inline;

    class function GetSecureRandom: ISecureRandom; static; inline;
    class property SecureRandom: ISecureRandom read GetSecureRandom;
  private
    class function GetSigner(): ISigner; static;
    class function GetCurve(keyType: TKeyType): IX9ECParameters; static;
    class function GetDomain(curve: IX9ECParameters)
      : IECDomainParameters; static;
  public
    class function GenerateECKeyPair(AKeyType: TKeyType): TKeyPair; static;

    class function SignMessage(const &message: TBytes; const PrivateKey: TBytes;
      AKeyType: TKeyType): TBytes; static;

    class function VerifySignature(const signature: TBytes;
      const &message: TBytes; const PublicKey: TBytes; AKeyType: TKeyType)
      : Boolean; static;

    class function GenPubKeyFromPvtInput(AKeyType: TKeyType): TKeyPair;

    class function GenerateValidRandomBytesForPrivateKey(): String; static;

    class procedure GetPublicKeyDetails(const APrivateKey: String;
      isCompressed: Boolean; var Address, PubKey: String); static;

    class function GetPrivateKeyWIF(const APrivateKey: String;
      isCompressed: Boolean): String; static;
  end;

implementation

class function TbtcKeyFunctions.GetCurveFromKeyType(AKeyType: TKeyType
  ): IX9ECParameters;
var
  CurveName: string;
begin
  CurveName := GetEnumName(TypeInfo(TKeyType), Ord(AKeyType));
  Result := TCustomNamedCurves.GetByName(CurveName);
end;

class function TbtcKeyFunctions.GetCurve(keyType: TKeyType): IX9ECParameters;
begin
  Result := GetCurveFromKeyType(keyType);
end;

class function TbtcKeyFunctions.GetDomain(curve: IX9ECParameters
  ): IECDomainParameters;
begin
  Result := TECDomainParameters.Create(curve.curve, curve.G, curve.N, curve.H,
    curve.GetSeed);
end;

class function TbtcKeyFunctions.GetSecureRandom: ISecureRandom;
begin
  if FSecureRandom <> Nil then
  begin
    Result := FSecureRandom
  end
  else
  begin
    FSecureRandom := TSecureRandom.Create();
    Result := FSecureRandom;
  end;
end;

class function TbtcKeyFunctions.GetSigner(): ISigner;
begin
  Result := TSignerUtilities.GetSigner(SigningAlgorithm);
end;

class function TbtcKeyFunctions.SignMessage(const &message: TBytes;
  const PrivateKey: TBytes; AKeyType: TKeyType): TBytes;
var
  LSigner: ISigner;
  LRecreatedPrivKey: IECPrivateKeyParameters;
  LCurve: IX9ECParameters;
  domain: IECDomainParameters;
begin
  LCurve := GetCurve(AKeyType);
  domain := GetDomain(LCurve);
  LRecreatedPrivKey := TECPrivateKeyParameters.Create('ECDSA',
    TBigInteger.Create(1, PrivateKey), domain);
  LSigner := GetSigner();
  LSigner.Init(True, LRecreatedPrivKey);
  LSigner.BlockUpdate(&message, 0, System.Length(&message));
  Result := LSigner.GenerateSignature();
end;

class function TbtcKeyFunctions.VerifySignature(const signature: TBytes;
  const &message: TBytes; const PublicKey: TBytes; AKeyType: TKeyType): Boolean;
var
  LSigner: ISigner;
  LRecreatedPubKey: IECPublicKeyParameters;
  LCurve: IX9ECParameters;
  domain: IECDomainParameters;
begin
  LCurve := GetCurve(AKeyType);
  domain := GetDomain(LCurve);
  LRecreatedPubKey := TECPublicKeyParameters.Create('ECDSA',
    LCurve.curve.DecodePoint(PublicKey), domain);
  LSigner := GetSigner();
  LSigner.Init(False, LRecreatedPubKey);
  LSigner.BlockUpdate(&message, 0, System.Length(&message));
  Result := LSigner.VerifySignature(signature);
end;

class function TbtcKeyFunctions.GenerateECKeyPair(AKeyType: TKeyType): TKeyPair;
var
  LCurve: IX9ECParameters;
  domain: IECDomainParameters;
  KeyPairGeneratorInstance: IAsymmetricCipherKeyPairGenerator;
  askp: IAsymmetricCipherKeyPair;
begin
  LCurve := GetCurve(AKeyType);
  domain := GetDomain(LCurve);
  KeyPairGeneratorInstance := TGeneratorUtilities.GetKeyPairGenerator('ECDSA');
  KeyPairGeneratorInstance.Init(TECKeyGenerationParameters.Create(domain,
    SecureRandom) as IECKeyGenerationParameters);
  askp := KeyPairGeneratorInstance.GenerateKeyPair();
  Result.PrivateKey := (askp.Private as IECPrivateKeyParameters)
    .D.ToByteArrayUnsigned;
  Result.PublicKey := (askp.Public as IECPublicKeyParameters).Q.GetEncoded();
end;





















//
//
//
//       Starting my own functions from here down.  All of the above
//       was copied from an example I found on the Lazarus forums
//       from xor-el.  Below I am trying to implement my own code to
//       share with others that want to implement bitcoin functions in
//       their applications.
//
//
//




class function TbtcKeyFunctions.GenPubKeyFromPvtInput(AKeyType: TKeyType
  ): TKeyPair;
var
  LCurve: IX9ECParameters;
  domain: IECDomainParameters;
  KeyPairGeneratorInstance: IAsymmetricCipherKeyPairGenerator;
  askp: IAsymmetricCipherKeyPair;
begin
  LCurve := GetCurve(AKeyType);
  domain := GetDomain(LCurve);

  KeyPairGeneratorInstance := TGeneratorUtilities.GetKeyPairGenerator('ECDSA');
  KeyPairGeneratorInstance.Init(TECKeyGenerationParameters.Create(domain,
    SecureRandom) as IECKeyGenerationParameters);
  askp := KeyPairGeneratorInstance.GenerateKeyPair();



  Result.PrivateKey := (askp.Private as IECPrivateKeyParameters)
    .D.ToByteArrayUnsigned;
  Result.PublicKey := (askp.Public as IECPublicKeyParameters).Q.GetEncoded();
end;

class function TbtcKeyFunctions.GenerateValidRandomBytesForPrivateKey(): String;
begin
  Result := THex.Encode(TBitcoinKey.GenerateValidRandomBytesForPrivateKey());
end;

class procedure TbtcKeyFunctions.GetPublicKeyDetails(const APrivateKey: String;
  isCompressed: Boolean; var Address, PubKey: String);
var
  Key: IBitcoinKey;
begin
  Key := TBitcoinKey.Create(THex.Decode(APrivateKey), -1, isCompressed);
  Address := Key.GeneratePublicAddress(Key.IsCompressed);
  PubKey := THex.Encode(Key.PublicKey.Q.Normalize.GetEncoded(Key.IsCompressed));
end;

class function TbtcKeyFunctions.GetPrivateKeyWIF(const APrivateKey: String;
  isCompressed: Boolean): String;
var
  Key: IBitcoinKey;
begin
  Key := TBitcoinKey.Create(THex.Decode(APrivateKey), -1, isCompressed);
  Result := Key.GenerateWIFPrivateKey(Key.IsCompressed);
end;




end.

