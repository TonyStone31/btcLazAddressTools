{ *********************************************************************************** }
{ *                              CryptoLib Library                                  * }
{ *                Copyright (c) 2018 - 20XX Ugochukwu Mmaduekwe                    * }
{ *                 Github Repository <https://github.com/Xor-el>                   * }

{ *  Distributed under the MIT software license, see the accompanying file LICENSE  * }
{ *          or visit http://www.opensource.org/licenses/mit-license.php.           * }

{ *                              Acknowledgements:                                  * }
{ *                                                                                 * }
{ *      Thanks to Sphere 10 Software (http://www.sphere10.com/) for sponsoring     * }
{ *                           development of this library                           * }

{ * ******************************************************************************* * }

(* &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& *)

unit ClpPkcsObjectIdentifiers;

{$I ..\..\Include\CryptoLib.inc}

interface

uses
  ClpAsn1Objects,
  ClpIAsn1Objects;

type
  TPkcsObjectIdentifiers = class abstract(TObject)

  strict private

  const
    //
    // object identifiers for digests
    //
    DigestAlgorithm: String = '1.2.840.113549.2';
    //
    // pkcs-3 OBJECT IDENTIFIER ::= {
    // iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) 3 }
    //
    Pkcs3: String = '1.2.840.113549.1.3';

    //
    // pkcs-5 OBJECT IDENTIFIER ::= {
    // iso(1) member-body(2) us(840) rsadsi(113549) pkcs(1) 5 }
    //
    Pkcs5: String = '1.2.840.113549.1.5';

  class var

    FIsBooted: Boolean;
    FDhKeyAgreement, FIdPbkdf2, FMD2, FMD4, FMD5, FIdHmacWithSha1,
      FIdHmacWithSha224, FIdHmacWithSha256, FIdHmacWithSha384, FIdHmacWithSha512
      : IDerObjectIdentifier;

    class function GetDhKeyAgreement: IDerObjectIdentifier; static; inline;
    class function GetIdPbkdf2: IDerObjectIdentifier; static; inline;
    class function GetMD2: IDerObjectIdentifier; static; inline;
    class function GetMD4: IDerObjectIdentifier; static; inline;
    class function GetMD5: IDerObjectIdentifier; static; inline;

    class function GetIdHmacWithSha1: IDerObjectIdentifier; static; inline;
    class function GetIdHmacWithSha224: IDerObjectIdentifier; static; inline;
    class function GetIdHmacWithSha256: IDerObjectIdentifier; static; inline;
    class function GetIdHmacWithSha384: IDerObjectIdentifier; static; inline;
    class function GetIdHmacWithSha512: IDerObjectIdentifier; static; inline;

    class constructor PkcsObjectIdentifiers();

  public

    class property DhKeyAgreement: IDerObjectIdentifier read GetDhKeyAgreement;

    class property IdPbkdf2: IDerObjectIdentifier read GetIdPbkdf2;
    //
    // md2 OBJECT IDENTIFIER ::=
    // {iso(1) member-body(2) US(840) rsadsi(113549) DigestAlgorithm(2) 2}
    //
    class property MD2: IDerObjectIdentifier read GetMD2;
    //
    // md4 OBJECT IDENTIFIER ::=
    // {iso(1) member-body(2) US(840) rsadsi(113549) DigestAlgorithm(2) 4}
    //
    class property MD4: IDerObjectIdentifier read GetMD4;
    //
    // md5 OBJECT IDENTIFIER ::=
    // {iso(1) member-body(2) US(840) rsadsi(113549) DigestAlgorithm(2) 5}
    //
    class property MD5: IDerObjectIdentifier read GetMD5;

    class property IdHmacWithSha1: IDerObjectIdentifier read GetIdHmacWithSha1;
    class property IdHmacWithSha224: IDerObjectIdentifier
      read GetIdHmacWithSha224;
    class property IdHmacWithSha256: IDerObjectIdentifier
      read GetIdHmacWithSha256;
    class property IdHmacWithSha384: IDerObjectIdentifier
      read GetIdHmacWithSha384;
    class property IdHmacWithSha512: IDerObjectIdentifier
      read GetIdHmacWithSha512;

    class procedure Boot(); static;

  end;

implementation

{ TPkcsObjectIdentifiers }

class procedure TPkcsObjectIdentifiers.Boot;
begin
  if not FIsBooted then
  begin
    FDhKeyAgreement := TDerObjectIdentifier.Create(Pkcs3 + '.1');
    FIdPbkdf2 := TDerObjectIdentifier.Create(Pkcs5 + '.12');
    FMD2 := TDerObjectIdentifier.Create(DigestAlgorithm + '.2');
    FMD4 := TDerObjectIdentifier.Create(DigestAlgorithm + '.4');
    FMD5 := TDerObjectIdentifier.Create(DigestAlgorithm + '.5');
    FIdHmacWithSha1 := TDerObjectIdentifier.Create(DigestAlgorithm + '.7');
    FIdHmacWithSha224 := TDerObjectIdentifier.Create(DigestAlgorithm + '.8');
    FIdHmacWithSha256 := TDerObjectIdentifier.Create(DigestAlgorithm + '.9');
    FIdHmacWithSha384 := TDerObjectIdentifier.Create(DigestAlgorithm + '.10');
    FIdHmacWithSha512 := TDerObjectIdentifier.Create(DigestAlgorithm + '.11');

    FIsBooted := True;
  end;
end;

class function TPkcsObjectIdentifiers.GetDhKeyAgreement: IDerObjectIdentifier;
begin
  result := FDhKeyAgreement;
end;

class function TPkcsObjectIdentifiers.GetIdHmacWithSha1: IDerObjectIdentifier;
begin
  result := FIdHmacWithSha1;
end;

class function TPkcsObjectIdentifiers.GetIdHmacWithSha224: IDerObjectIdentifier;
begin
  result := FIdHmacWithSha224;
end;

class function TPkcsObjectIdentifiers.GetIdHmacWithSha256: IDerObjectIdentifier;
begin
  result := FIdHmacWithSha256;
end;

class function TPkcsObjectIdentifiers.GetIdHmacWithSha384: IDerObjectIdentifier;
begin
  result := FIdHmacWithSha384;
end;

class function TPkcsObjectIdentifiers.GetIdHmacWithSha512: IDerObjectIdentifier;
begin
  result := FIdHmacWithSha512;
end;

class function TPkcsObjectIdentifiers.GetIdPbkdf2: IDerObjectIdentifier;
begin
  result := FIdPbkdf2;
end;

class function TPkcsObjectIdentifiers.GetMD2: IDerObjectIdentifier;
begin
  result := FMD2;
end;

class function TPkcsObjectIdentifiers.GetMD4: IDerObjectIdentifier;
begin
  result := FMD4;
end;

class function TPkcsObjectIdentifiers.GetMD5: IDerObjectIdentifier;
begin
  result := FMD5;
end;

class constructor TPkcsObjectIdentifiers.PkcsObjectIdentifiers;
begin
  TPkcsObjectIdentifiers.Boot;
end;

end.

