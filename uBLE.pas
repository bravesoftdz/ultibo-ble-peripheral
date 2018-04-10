unit uBLE;

{$mode delphi}{$H+}
//{$define show_data}

interface

uses 
SysUtils;

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

 ManufacturerTesting         = $ffff;
 ManufacturerApple           = $004c;
 ManufacturerMicrosoft       = $0006;

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
procedure StartPassiveScanning;
procedure StopScanning;

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

procedure StartPassiveScanning;
const 
 UnitsPerSecond = 1600;
begin
 SetLEScanParameters(LL_SCAN_PASSIVE,5*UnitsPerSecond,Round(0.5*UnitsPerSecond),$00,$00);
 SetLEScanEnable(True,False);
end;

procedure StopScanning;
begin
 SetLEScanEnable(False,False);
end;

function AdvertisingTypeToStr (Type_ : byte) : string;
begin
 case Type_ of 
  ADV_IND           : Result:='connectable undirected advertising (default)';
  ADV_DIRECT_IND_HI : Result:='connectable high duty cycle directed advertising';
  ADV_SCAN_IND      : Result:='scannable undirected advertising';
  ADV_NONCONN_IND   : Result:='non-connectable undirected advertising';
  ADV_DIRECT_IND_LO : Result:='connectable low duty cycle directed advertising';
  else                Result:='reserved for future use (' + Type_.ToHexString(2) + ')';
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

function dBm(Rssi:Byte):String;
var 
 si:String;
begin
 if Rssi = 127 then si := 'NU'
 else if Rssi > 128 then si := '-' + IntToStr (256 - Rssi) + 'dBm'
 else if Rssi <= 20 then si := '+' + IntToStr (Rssi) + 'dBm'
 else si := '??';
 Result:=si;
end;

procedure LeCreateConnection(Address:String;A,B,C,D,E,F:Byte);
begin
 // AddHCICommand (OGF_LE_CONTROL, $0d, [100,0, 100,0, 0,1, A,B,C,D,E,F, 1, 100,0,150,0, 50,0, $80,$0C, 0,0,0,8])
end;

function Mfg(Manufacturer:Word):String;
begin
 case Manufacturer of 
  ManufacturerTesting:Result:='Testing';
  ManufacturerApple:Result:='Apple';
  ManufacturerMicrosoft:Result:='Microsoft'
                        else
                         Result:='?';
 end;
end;

procedure DecodeADS (ads : array of byte; RxRssi : byte);
var 
 uuid : array of byte;
 len : integer;
 manufacturer: word;
 parsed : boolean;
 //{$ifdef show_data}
 i : integer;
 s : string;
 name : string;
 //{$endif}
begin
 //{$ifdef show_data}
 s := '';
 name := '';
 for i := low (ads) + 1 to high (ads) do
  begin
   s := s + IntToHex (ads[i], 2) + ' ';
   name:=name + Char(ads[i]);
  end;
 //{$endif}
 len := length (ads);
 if len = 0 then exit;
 case ads[0] of 
  ADT_FLAGS :
             begin
              Log(Format('    flags %s',[s]));
              if (ads[1] and $01) <> 0 then
               Log('        LE Limited Discoverable Mode');
              if (ads[1] and $02) <> 0 then
               Log('        LE General Discoverable Mode');
              if (ads[1] and $04) <> 0 then
               Log('        BR/EDR is supported. This is used if your iBeacon is Dual Mode device');
              if (ads[1] and $08) <> 0 then
               Log('        LE and BR/EDR Controller operates simultaneously');
              if (ads[1] and $10) <> 0 then
               Log('        LE and BR/EDR Host operates simultaneously');
             end;

  ADT_SHORTENED_LOCAL_NAME  : Log(Format('    shortened local name %s',[name]));
  ADT_COMPLETE_LOCAL_NAME   : Log(Format('    complete local name %s',[name]));
  ADT_POWER_LEVEL           : Log(Format('    transmit power level (as stated by sender) %s',[dBm(ads[1])]));
  ADT_MANUFACTURER_SPECIFIC :
                             begin
                              parsed:=False;
                              manufacturer:=(ads[2] shl 8) or ads[1];
                              if (len = 26) and (manufacturer = ManufacturerApple) and (ads[3] = $02) and (ads[4] = $15) then
                               begin
                                SetLength (uuid, 16);
                                Move(ads[5],uuid[0],16);
                                Log(Format('    iBeacon uuid %s major %04.4x minor %04.4x signal power %s',[UUIDToStr(uuid),ads[21] shl 8 + ads[22],ads[23] shl 8 + ads[24],ads[25]]));
                                parsed:=True;
                               end;
                              if not parsed then
                               Log(Format('    manufacturer %04.4x/%s %s',[manufacturer,Mfg(manufacturer),RightStr(s,Length(s) - 6)]));
                             end;
  else
   Log(Format('    type %02.2x len %d - %s',[ads[0],Length(ads),s]));
 end;
end;

procedure DecodeReport (Report : array of byte; RxRssi : byte);
var 
 {$ifdef show_data}
 s : string;
 {$endif}
 i, len : integer;
 ads : array of byte;
 gl : boolean;        // getting length
begin
{$ifdef show_data}
 s := '';
 for i := low (Report) to high (Report) do
  s := s + ' ' + IntToHex (Report[i], 2);
 log ('Report ' + s);
{$endif}
 gl := true;
 len := 0;
 i := low (Report);
 while i <= high (Report) do
  begin
   if gl then
    begin
     gl := false;
     len := Report[i];
     i := i + 1;
     if len = 0 then break;
    end
   else if (len + i - 1 <= high (Report)) then
         begin
          SetLength (ads, len);
          Move (Report[i], ads[0], len);
          i := i + len;
          DecodeADS (ads, RxRssi);
          gl := true;
         end
   else
    begin
     Log ('Error decoding AD structure');
     break;
    end;
  end;
end;

function PublicOrRandom(X:Byte):String;
begin
 if X = 0 then
  Result:='public'
 else
  Result:='random';
end;

var 
 ReceivedMacList:Array of String;

procedure DoLEEvent (SubEvent : byte; Params : array of byte);
const 
 ReportHeaderLength = 9;
var 
 ConnectionHandle:Word;
 ofs,i,j,len,rl:Integer;
 nr:Byte;
 rpt:Array of Byte;
 ReceivedAddr:String;
 Found:Boolean;
{$ifdef show_data}
 s : string;
{$endif}
begin
 len := length (Params);
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
  $02 : // le advertsing report
       begin
        nr := Params[0]; // num reports
        ofs := 1;
        for i := 1 to nr do
         begin
          if ofs + 8 <= len then
           begin
            rl := Params[ofs + 8]; // length
            if ofs + ReportHeaderLength + rl <= len then
             begin
              ReceivedAddr:=Params[ofs + 7].ToHexString (2) + ':' +
                            Params[ofs + 6].ToHexString (2) + ':' +
                            Params[ofs + 5].ToHexString (2) + ':' +
                            Params[ofs + 4].ToHexString (2) + ':' +
                            Params[ofs + 3].ToHexString (2) + ':' +
                            Params[ofs + 2].ToHexString (2);
              Found:=False;
              for J:=Low(ReceivedMacList) to High(ReceivedMacList) do
               if ReceivedMacList[J] = ReceivedAddr then
                begin
                 Found:=True;
                 break;
                end;
              if not Found then
               begin
                SetLength(ReceivedMacList,1 + Length(ReceivedMacList));
                ReceivedMacList[High(ReceivedMacList)]:=ReceivedAddr;
                Log(Format('scan report %d of %d %s/%s %7s',[
                    i, nr,
                    ReceivedAddr,PublicOrRandom(Params[ofs + 1]),
                dBm(Params[ofs + ReportHeaderLength + rl])]));
                Log(Format('    %s',[EventTypeToStr(Params[ofs])]));
                SetLength (rpt, rl);
                Move(Params[ofs + ReportHeaderLength],rpt[0],rl);
                DecodeReport(rpt,Params[ofs + ReportHeaderLength + rl]);
                if (Params[ofs] = ADV_IND) then
                 LeCreateConnection(ReceivedAddr,Params[ofs+2],Params[ofs+3],Params[ofs+4],Params[ofs+5],Params[ofs+6],Params[ofs+7]);
               end;
              ofs := ofs + rl + ReportHeaderLength + 1;
             end
            else
             begin
              Log ('Invalid Report');
              break;
             end;
           end;
          //          Log ('Event Type ' + EventTypeToStr (ev[ofs + 1]));
          //          Log ('Address Type ' + ev[ofs + 2].ToHexString);
         end;
       end;
 end;
end;

initialization
SetLength(ReceivedMacList,0);
ClearAdvertisingData;
SetLEEvent (@DoLEEvent);
end.
