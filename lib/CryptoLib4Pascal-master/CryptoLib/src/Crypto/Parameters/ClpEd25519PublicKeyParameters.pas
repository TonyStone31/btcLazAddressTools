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

unit ClpEd25519PublicKeyParameters;

{$I ..\..\Include\CryptoLib.inc}

interface

uses
  Classes,
  ClpEd25519,
  ClpAsymmetricKeyParameter,
  ClpIEd25519PublicKeyParameters,
  ClpArrayUtils,
  ClpAsn1Objects,
  ClpCryptoLibTypes;

resourcestring
  SEOFInPublicKey = 'EOF encountered in middle of Ed25519 public key';

type
  TEd25519PublicKeyParameters = class sealed(TAsymmetricKeyParameter,
    IEd25519PublicKeyParameters)

  strict private
  var
    FData: TCryptoLibByteArray;

  public

    const
    KeySize = Int32(TEd25519.PublicKeySize);

    constructor Create(const buf: TCryptoLibByteArray; off: Int32); overload;
    constructor Create(input: TStream); overload;

    procedure Encode(const buf: TCryptoLibByteArray; off: Int32); inline;
    function GetEncoded(): TCryptoLibByteArray; inline;

    function Equals(const other: IEd25519PublicKeyParameters): Boolean;
      reintroduce; overload;
    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}override;

  end;

implementation

{ TEd25519PublicKeyParameters }

function TEd25519PublicKeyParameters.GetEncoded: TCryptoLibByteArray;
begin
  result := System.Copy(FData);
end;

constructor TEd25519PublicKeyParameters.Create(const buf: TCryptoLibByteArray;
  off: Int32);
begin
  Inherited Create(false);
  System.SetLength(FData, KeySize);
  System.Move(buf[off], FData[0], KeySize * System.SizeOf(Byte));
end;

constructor TEd25519PublicKeyParameters.Create(input: TStream);
begin
  Inherited Create(false);
  System.SetLength(FData, KeySize);
  if (KeySize <> TStreamUtils.ReadFully(input, FData)) then
  begin
    raise EEndOfStreamCryptoLibException.CreateRes(@SEOFInPublicKey);
  end;
end;

procedure TEd25519PublicKeyParameters.Encode(const buf: TCryptoLibByteArray;
  off: Int32);
begin
  System.Move(FData[0], buf[off], KeySize * System.SizeOf(Byte));
end;

function TEd25519PublicKeyParameters.Equals(const other
  : IEd25519PublicKeyParameters): Boolean;
begin
  if (other = Self as IEd25519PublicKeyParameters) then
  begin
    result := true;
    Exit;
  end;

  if (other = Nil) then
  begin
    result := false;
    Exit;
  end;
  result := TArrayUtils.ConstantTimeAreEqual(FData, other.GetEncoded())
end;

function TEd25519PublicKeyParameters.GetHashCode: {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
begin
  result := TArrayUtils.GetArrayHashCode(FData);
end;

end.
