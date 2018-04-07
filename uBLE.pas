unit uBLE;

{$mode delphi}{$H+}
//{$define show_data}

interface

uses 
Classes, SysUtils;

const 
 STANDBY_STATE               = 1;
 ADVERTISING_STATE           = 2;
 SCANNING_STATE              = 3;
 INITIATING_STATE            = 4;
 CONNECTION_STATE            = 5;

 // Markers
 INITIAL_SETUP_DONE          = 1;

 // HCI Funcional Specification Section 7.8.10, Core 4.1 page 1255
 LL_SCAN_PASSIVE            = $00;
 LL_SCAN_ACTIVE            = $01;

 //  BLUETOOTH SPECIFICATION Version 4.2 [Vol 2, Part E] page 970
 // advertising type
 ADV_IND                     = $00; // Connectable undirected advertising (default)
 ADV_DIRECT_IND_HI           = $01; // Connectable high duty cycle directed advertising
 ADV_SCAN_IND                = $02; // Scannable undirected advertising
 ADV_NONCONN_IND             = $03; // Non connectable undirected advertising
 ADV_DIRECT_IND_LO           = $04; // Connectable low duty cycle directed advertising
 // $05 – $FF Reserved for future use

 // own address type
 LL_ADR_PUBLIC               = $00; // Public Device Address (default)
 ll_ADR_RANDOM               = $01; // Random Device Address
 LL_ADR_PRIVATE_PUBLIC       = $02; // Controller generates Resolvable Private Address based on the local
 // IRK from resolving list. If resolving list contains no matching entry,
 // use public address.
 LL_ADR_PRIVATE_RANDOM       = $03; // Controller generates Resolvable Private Address based on the local
 // IRK from resolving list. If resolving list contains no matching entry,
 // use random address from LE_Set_Random_Address.
 // $04 – $FF Reserved for future use
 // peer address type
 LL_PEER_PUBLI               = $00; // Public Device Address (default) or Public Identity Address
 LL_PEER_RANDOM              = $01; // Random Device Address or Random (static) Identity Address
 // $02 – $FF Reserved for future use
  (*
  Value Parameter Description
  0xXXXXXXXXXXXX Public Device Address, Random Device Address, Public Identity
  Address, or Random (static) Identity Address of the device to be
  connected        *)

 // Advertising Data Types
 ADT_FLAGS                   = $01;      // Flags
 ADT_INCOMPLETE_UUID16       = $02;      // Incomplete List of 16-bit Service Class UUIDs
 ADT_COMPLETE_UUID16         = $03;      // Complete List of 16-bit Service Class UUIDs
 ADT_INCOMPLETE_UUID32       = $04;      // Incomplete List of 32-bit Service Class UUIDs
 ADT_COMPLETE_UUID32         = $05;      // Complete List of 32-bit Service Class UUIDs
 ADT_INCOMPLETE_UUID128      = $06;      // Incomplete List of 128-bit Service Class UUIDs
 ADT_COMPLETE_UUDI128        = $07;      // Complete List of 128-bit Service Class UUIDs
 ADT_SHORTENED_LOCAL_NAME    = $08;      // Shortened Local name
 ADT_COMPLETE_LOCAL_NAME     = $09;      // Complete Local name
 ADT_POWER_LEVEL             = $0A;      // Tx Power Level
 ADT_DEVICE_CLASS            = $0D;      // Class of Device
 ADT_MANUFACTURER_SPECIFIC   = $FF;

var 
 LLState : integer = STANDBY_STATE; // link layer state
 AdData : array of byte;
 MostRecentConnectionHandle:LongWord;

procedure InitialSetup;               // page 133 v4.2
procedure StartUndirectedAdvertising; // page 136 v4.2
procedure StopAdvertising;
function AdvertisingTypeToStr (Type_ : byte) : string;

// helper functions
procedure ClearAdvertisingData;
procedure AddAdvertisingData (Type_ : byte); overload;
procedure AddAdvertisingData (Type_ : byte; Data : array of byte); overload;
procedure AddAdvertisingData (Type_ : byte; Data : string); overload;

implementation

uses uHCI,Logging;

procedure InitialSetup;
begin
 NoOP;
 ReadLocalSupportedCommands;
 ReadLocalSupportedFeatures;
 SetLEEventMask ($ff);
 ReadLEBufferSize;
 ReadLESupportedFeatures;
 ReadBDADDR;
 AddMarker (INITIAL_SETUP_DONE);
end;

function AdvertisingTypeToStr (Type_ : byte) : string;
begin
 case Type_ of 
  ADV_IND           : Result:='Connectable undirected advertising (default)';
  ADV_DIRECT_IND_HI : Result:='Connectable high duty cycle directed advertising';
  ADV_SCAN_IND      : Result:='Scannable undirected advertising';
  ADV_NONCONN_IND   : Result:='Non connectable undirected advertising';
  ADV_DIRECT_IND_LO : Result:='Connectable low duty cycle directed advertising';
  else                Result:='Reserved for future use (' + Type_.ToHexString(2) + ')';
 end;
end;

procedure StartUndirectedAdvertising;
begin
 ReadLEAdvertisingChannelTxPower;
 SetLEAdvertisingData (AdData);
 SetLEAdvertisingEnable (true);
 AddMarker (ADVERTISING_STATE);
end;

procedure StopAdvertising;
begin
 SetLEAdvertisingEnable (false);
end;

procedure ClearAdvertisingData;
begin
 SetLength (AdData, 0);
end;

procedure AddAdvertisingData (Type_ : byte);
begin
 AddAdvertisingData (Type_, []);
end;

procedure AddAdvertisingData (Type_ : byte; Data : array of byte);
var 
 Len : byte;
 i : integer;
begin
 Len:=Length (AdData);
 SetLength (AdData, Len + length (Data) + 2);
 AdData[Len]:=Length (Data) + 1;
 AdData[Len + 1]:=Type_;
 for i:=0 to high (Data) do
  AdData[Len + 2 + i]:=Data[i];
end;

procedure AddAdvertisingData (Type_ : byte; Data : string);
var 
 Len : byte;
 i : integer;
begin
 Len:=Length (AdData);
 SetLength (AdData, Len + length (Data) + 2);
 AdData[Len]:=Length (Data) + 1;
 AdData[Len + 1]:=Type_;
 for i:=1 to length (Data) do
  AdData[Len + 1 + i]:=ord (Data[i]);
end;

function UUIDToStr (uuid : array of byte) : string;
begin
 if length (uuid) = 16 then
  Result:=format ('%.2X%.2X%.2X%.2X-%.2X%.2X-%.2X%.2X-%.2X%.2X-%.2X%.2X%.2X%.2X%.2X%.2X',
          [uuid[0], uuid[1], uuid[2], uuid[3], uuid[4], uuid[5], uuid[6], uuid[7],
          uuid[8], uuid[9], uuid[10], uuid[11], uuid[12], uuid[13], uuid[14], uuid[15]])
 else
  Result:='';
end;

procedure DoLEEvent (SubEvent : byte; Params : array of byte);
const 
 ReportHeaderLength = 9;
var 
 ConnectionHandle:Word;
{$ifdef show_data}
 s : string;
{$endif}
begin
{$ifdef show_data}
 s:='';
 for i:=low (Params) to high (Params) do
  s:=s + ' ' + Params[i].ToHexString (2);
 Log ('LEEvent ' + SubEvent.ToHexString (2) + ' Params ' + s);
{$endif}
 case SubEvent of 
  $01 :
       begin
        ConnectionHandle:=Params[1] + Params[2]*256;
        MostRecentConnectionHandle:=ConnectionHandle;
        Log (Format('%02.2x) connected',[ConnectionHandle]));
        //      L:=6;
        //      SendData([HCI_ACLDATA_PKT,lo(ConnectionHandle),hi(ConnectionHandle),4+L+1,0,L+1,0,4,0,$8,1,0,$ff,$ff,$03,$28]);
       end;
 end;
end;

initialization
ClearAdvertisingData;
SetLEEvent (@DoLEEvent);
end.
