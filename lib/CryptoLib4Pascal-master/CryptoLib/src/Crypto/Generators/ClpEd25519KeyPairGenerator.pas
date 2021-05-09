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

unit ClpEd25519KeyPairGenerator;

{$I ..\..\Include\CryptoLib.inc}

interface

uses
  ClpIEd25519,
  ClpAsymmetricCipherKeyPair,
  ClpIEd25519KeyPairGenerator,
  ClpIEd25519PrivateKeyParameters,
  ClpEd25519PrivateKeyParameters,
  ClpIEd25519PublicKeyParameters,
  ClpISecureRandom,
  ClpIKeyGenerationParameters,
  ClpIAsymmetricCipherKeyPair,
  ClpIAsymmetricCipherKeyPairGenerator;

type
  TEd25519KeyPairGenerator = class(TInterfacedObject, IEd25519KeyPairGenerator,
    IAsymmetricCipherKeyPairGenerator)

  strict private
  var
    FRandom: ISecureRandom;
    FEd25519Instance: IEd25519;

  public
    constructor Create(const Ed25519Instance: IEd25519);
    procedure Init(const parameters: IKeyGenerationParameters);

    function GenerateKeyPair(): IAsymmetricCipherKeyPair;

  end;

implementation

{ TEd25519KeyPairGenerator }

constructor TEd25519KeyPairGenerator.Create(const Ed25519Instance: IEd25519);
begin
  inherited Create();
  FEd25519Instance := Ed25519Instance;
end;

function TEd25519KeyPairGenerator.GenerateKeyPair: IAsymmetricCipherKeyPair;
var
  privateKey: IEd25519PrivateKeyParameters;
  publicKey: IEd25519PublicKeyParameters;
begin
  privateKey := TEd25519PrivateKeyParameters.Create(FEd25519Instance, FRandom);
  publicKey := privateKey.GeneratePublicKey();
  result := TAsymmetricCipherKeyPair.Create(publicKey, privateKey);
end;

procedure TEd25519KeyPairGenerator.Init(const parameters
  : IKeyGenerationParameters);
begin
  FRandom := parameters.random;
end;

end.
