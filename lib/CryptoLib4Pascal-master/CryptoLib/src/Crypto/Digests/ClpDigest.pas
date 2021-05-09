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

unit ClpDigest;

{$I ..\..\Include\CryptoLib.inc}

interface

uses
  SysUtils,
  HlpIHash,
  HlpIHashInfo,
  ClpIDigest,
  ClpCryptoLibTypes;

resourcestring
  SOutputBufferTooShort = 'Output Buffer Too Short';

type

  /// <summary>
  /// Hash Wrapper For the Proper Implementation in HashLib4Pascal
  /// </summary>
  TDigest = class sealed(TInterfacedObject, IDigest)

  strict private
  var
    FHash: IHash;

    function GetAlgorithmName: string; inline;

    function DoFinal: TCryptoLibByteArray; overload;

  public
    constructor Create(const hash: IHash; doInitialize: Boolean = True);

    /// <summary>
    /// Gets the Underlying <b>IHash</b> Instance
    /// </summary>
    function GetUnderlyingIHash: IHash; inline;

    /// <summary>
    /// the size, in bytes, of the digest produced by this message digest.
    /// </summary>
    function GetDigestSize(): Int32; inline;

    /// <summary>
    /// the size, in bytes, of the internal buffer used by this digest.
    /// </summary>
    function GetByteLength(): Int32; inline;

    /// <summary>
    /// update the message digest with a single byte.
    /// </summary>
    procedure Update(input: Byte);

    /// <summary>
    /// update the message digest with a block of bytes.
    /// </summary>
    /// <param name="input">
    /// the byte array containing the data.
    /// </param>
    /// <param name="inOff">
    /// the offset into the byte array where the data starts.
    /// </param>
    /// <param name="len">
    /// the length of the data.
    /// </param>
    procedure BlockUpdate(const input: TCryptoLibByteArray; inOff, len: Int32);

    /// <summary>
    /// Close the digest, producing the final digest value. The doFinal call
    /// leaves the digest reset.
    /// </summary>
    /// <param name="output">
    /// the array the digest is to be copied into.
    /// </param>
    /// <param name="outOff">
    /// the offset into the out array the digest is to start at.
    /// </param>
    function DoFinal(const output: TCryptoLibByteArray; outOff: Int32)
      : Int32; overload;

    /// <summary>
    /// Resets the digest back to it's initial state.
    /// </summary>
    procedure Reset();

    /// <summary>
    /// Clone the digest instance
    /// </summary>
    function Clone(): IDigest;

    /// <summary>
    /// the algorithm name
    /// </summary>
    property AlgorithmName: String read GetAlgorithmName;

  end;

implementation

{ TDigest }

function TDigest.GetAlgorithmName: string;
var
  LName: String;
  LowPoint, HighPoint: Int32;
begin
  LName := FHash.Name;
{$IFDEF DELPHIXE3_UP}
  LowPoint := System.Low(LName);
  HighPoint := System.High(LName);
{$ELSE}
  LowPoint := 1;
  HighPoint := System.Length(LName);
{$ENDIF DELPHIXE3_UP}
  result := Copy(LName, LowPoint + 1, HighPoint - 1);
end;

function TDigest.GetByteLength: Int32;
begin
  result := FHash.BlockSize;
end;

function TDigest.GetDigestSize: Int32;
begin
  result := FHash.HashSize;
end;

function TDigest.GetUnderlyingIHash: IHash;
begin
  result := FHash;
end;

procedure TDigest.Reset;
begin
  FHash.Initialize;
end;

procedure TDigest.BlockUpdate(const input: TCryptoLibByteArray;
  inOff, len: Int32);
begin
  FHash.TransformBytes(input, inOff, len);
end;

constructor TDigest.Create(const hash: IHash; doInitialize: Boolean);
begin
  Inherited Create();
  FHash := hash;
  if doInitialize then
  begin
    FHash.Initialize;
  end;
end;

function TDigest.DoFinal(const output: TCryptoLibByteArray;
  outOff: Int32): Int32;
var
  buf: TCryptoLibByteArray;
  Limit, LXOFSizeInBits: Int32;
begin

  if Supports(FHash, IXOF) then
  begin
    LXOFSizeInBits := (System.Length(output) - outOff) * 8;
    (FHash as IXOF).XOFSizeInBits := LXOFSizeInBits;
    Limit := LXOFSizeInBits shr 3;
  end
  else
  begin
    Limit := GetDigestSize;
  end;

  if (System.Length(output) - outOff) < Limit then
  begin
    raise EDataLengthCryptoLibException.CreateRes(@SOutputBufferTooShort);
  end
  else
  begin
    buf := DoFinal();
    System.Move(buf[0], output[outOff], System.Length(buf) *
      System.SizeOf(Byte));
  end;

  result := System.Length(buf);
end;

function TDigest.DoFinal: TCryptoLibByteArray;
begin
  result := FHash.TransformFinal.GetBytes();
end;

procedure TDigest.Update(input: Byte);
begin
  FHash.TransformUntyped(input, System.SizeOf(Byte));
end;

function TDigest.Clone(): IDigest;
begin
  result := TDigest.Create(FHash.Clone(), False);
end;

end.
