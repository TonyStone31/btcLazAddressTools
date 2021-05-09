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

unit ClpIECKeyParameters;

{$I ..\Include\CryptoLib.inc}

interface

uses

  ClpIAsymmetricKeyParameter,
  ClpIECDomainParameters;

type
  IECKeyParameters = interface(IAsymmetricKeyParameter)
    ['{50966A0E-21A4-41C3-9246-87B4ED67CE4D}']

    function GetAlgorithmName: String;
    function GetParameters: IECDomainParameters;

    function Equals(const other: IECKeyParameters): Boolean; overload;

    property AlgorithmName: String read GetAlgorithmName;
    property Parameters: IECDomainParameters read GetParameters;

  end;

implementation

end.
