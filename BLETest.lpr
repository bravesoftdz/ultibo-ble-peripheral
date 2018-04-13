program BLETest;

{$mode objfpc}{$H+}

(****************************************************
May 2017 PJ Design Engineering P/L
*****************************************************)

uses 
HTTP,WebStatus, GlobalConfig, GlobalConst, GlobalTypes,
Platform, Threads, SysUtils, Classes,Console,Ultibo,Logging,
DWCOTG,FileSystem,MMC,FATFS,Keyboard,
uBLE,uHCI;

var 
 Console1:TWindowHandle;
 ch : char;
 HTTPListener:THTTPListener;
 KeyboardMonitorHandle:TThreadHandle = INVALID_HANDLE_VALUE;
 LightBulb:PBleAttribute;

procedure Log1(s : string);
begin
 ConsoleWindowWriteLn(Console1,s);
end;

procedure WaitForSDDrive;
begin
 while not DirectoryExists('C:\') do
  sleep(500);
end;

procedure RestoreBootFile(Prefix,FileName: String);
var 
 Source:String;
begin
 Source:=Prefix + '-' + FileName;
 Log1(Format('Restoring from %s ...',[Source]));
 while not DirectoryExists('C:\') do
  sleep(500);
 if FileExists(Source) then
  CopyFile(PChar(Source),PChar(FileName),False);
 Log1(Format('Restoring from %s done',[Source]));
end;

var 
 UltiboAd:Array of Byte;
 ScanResponseSerialNumber:Byte;

procedure UpdateBroadcastData;
var 
 UpTime:Longword;
begin
 UpTime:=ClockGetCount div (1*1000*1000);
 SetLength(UltiboAd,8);
 UltiboAd[0]:=Lo(ManufacturerTesting);
 UltiboAd[1]:=Hi(ManufacturerTesting);
 UltiboAd[2]:=Lo(UltiboSignature);
 UltiboAd[3]:=Hi(UltiboSignature);
 UltiboAd[4]:=$00;
 UltiboAd[5]:=ScanResponseSerialNumber;
 UltiboAd[6]:=Lo(UpTime);
 UltiboAd[7]:=Hi(UpTime);
 ClearAdvertisingData;
 AddAdvertisingData(ADT_FLAGS,[$1a]);
 AddAdvertisingData(ADT_INCOMPLETE_UUID16,[Lo($180f),Hi($180f)]);
 AddAdvertisingData(ADT_COMPLETE_LOCAL_NAME,'Ultibo');
 AddAdvertisingData(ADT_MANUFACTURER_SPECIFIC,UltiboAd);
end;

procedure StartLeAdvertising;
var 
 ZeroAddress:TBDAddr = ($00,$00,$00,$00,$00,$00);
begin
 SetLEAdvertisingParameters(1000,1000,ADV_IND,$00,$00,ZeroAddress,$07,$00);
 UpdateBroadcastData;
 StartUndirectedAdvertising;
end;

// called when a marker event is processed by the comms queue
procedure DoMarkerEvent(no : integer);
begin
 case no of 
  FIRMWARE_START : Log1('Load Firmware ...');
  FIRMWARE_END   : Log1('Load Firmware done');
  SYSTEM_RESTART :
                  begin
                   Log1('test was successful - delaying 3 seconds then restarting to try to obtain failure ...');
                   Sleep(3*1000);
                   RestoreBootFile('ultibo-ble-peripheral','config.txt');
                   Log1('restarting ...');
                   Sleep(1*1000);
                   SystemRestart(0);
                  end;
  CONNECTION_TERMINATED: StartLeAdvertising;
  INIT_COMPLETE  :
                  begin
                   Log1('BLE Chip Initialised');
                   Log1('  Name    : ' + ChipName);
                   Log1(format('  Version : %d Revision : %d',[Ver,Rev]));
                   Log1('  Address : ' + BDAddrToStr(BDAddr));
                   Log1('Broadcasting as "Ultibo"');
                  end;
 end;
end;

procedure ChangeBatteryLevel(Delta:Integer);
var 
 NewLevel:Integer;
begin
 NewLevel:=BatteryLevel^.ReadByte + Delta;
 if NewLevel < 0 then
  NewLevel:=0
 else if NewLevel > 100 then
       NewLevel:=100;
 BatteryLevel^.SetByte(NewLevel);
 BatteryLevel^.Notify;
end;

procedure Help;
begin
 ConsoleWindowWriteLn(Console1,'');
 ConsoleWindowWriteLn(Console1,'H - help');
 ConsoleWindowWriteLn(Console1,'C - clear screen');
 ConsoleWindowWriteLn(Console1,'R - restart with ultibo-ble-peripheral-config.txt');
 ConsoleWindowWriteLn(Console1,'Q - restart with default-config.txt');
 ConsoleWindowWriteLn(Console1,'+ - increase battery level and notify via ble');
 ConsoleWindowWriteLn(Console1,'- - decrease battery level and notify via ble');
 ConsoleWindowWriteLn(Console1,'');
end;

function KeyboardMonitor(Parameter:Pointer):PtrInt;
begin
 Result:=0;
 while True do
  begin
   if ConsoleGetKey(ch,nil) then
    case uppercase(ch) of 
     '+' : ChangeBatteryLevel(+1);
     '-' : ChangeBatteryLevel(-1);
     'Q' : SystemRestart(0);
     'R' :
          begin
           RestoreBootFile('ultibo-ble-peripheral','config.txt');
           SystemRestart(0);
          end;
     'C' : ConsoleWindowClear(Console1);
     'H' : Help;
    end;
  end;
end;

procedure StartLogging;
begin
 LOGGING_INCLUDE_COUNTER:=False;
 LOGGING_INCLUDE_TICKCOUNT:=True;
 CONSOLE_REGISTER_LOGGING:=True;
 CONSOLE_LOGGING_POSITION:=CONSOLE_POSITION_BOTTOMRIGHT;
 LoggingConsoleDeviceAdd(ConsoleDeviceGetDefault);
 LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_CONSOLE));
end;

procedure ImmediateAlertChanged(Attribute:PBleAttribute);
begin
 Log('');
 Log(Format('Immediate Alert - Level %d',[Attribute^.Value[0]]));
end;

var 
 BluetoothFirmwareFileName:String = 'BCM43430A1.hcd';

procedure RequireBlueTooth;
begin
 Log(Format('Board is %s',[BoardTypeToString(BoardGetType)]));
 case BoardGetType of 
  BOARD_TYPE_RPI3B:
  ;
  BOARD_TYPE_RPI3B_PLUS:
                        BluetoothFirmwareFileName:='BCM4345C0.hcd';
  BOARD_TYPE_RPI_ZERO_W:
  ;
  else
   begin
    Log('');
    Log('');
    Log('Bluetooth is not available on this board');
    ThreadHalt(0);
   end;
 end;
end;

begin
 Console1:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_LEFT,True);
 Log1('Bluetooth Low Energy (BLE) Peripheral Test');
 RestoreBootFile('default','config.txt');
 StartLogging;
 KeyboardMonitorHandle:=BeginThread(@KeyboardMonitor,Nil,KeyboardMonitorHandle,THREAD_STACK_DEFAULT_SIZE);
 Help;
 RequireBluetooth;
 BuildAttributes;
 ImmediateAlertLevel^.OnChanged:=@ImmediateAlertChanged;
 AddService($ffe5);
 AddCharacteristic(LightBulb,$ffe9,7,7,'Light Bulb',ATT_PROPERTY_READ or ATT_PROPERTY_WRITE);
 LightBulb^.SetArray([$56,0,0,0,$00,$F0,$AA]);
 WaitForSDDrive;

 HTTPListener:=THTTPListener.Create;
 HTTPListener.Active:=True;
 WebStatusRegister(HTTPListener,'','',True);

 SetMarkerEvent(@DoMarkerEvent);       // set marker event(called when marker processed on event queue)
 AddMarker(OPEN_PORT);                 // open uart
 AddMarker(DELAY_50MSEC);              // ensure read thread has started
 ResetChip;                            // reset chip
 BCMLoadFirmware('BCM43430A1.hcd');    // load firmware
 AddMarker(DELAY_50MSEC);              // ensure read thread has started
 ReadLocalName;                        // read new chip name
 AddMarker(DELAY_50MSEC);              // ensure read thread has started
 ReadLocalVersion;                     // read new HCI version
 AddMarker(DELAY_50MSEC);              // ensure read thread has started
 ReadBDADDR;                           // read newly assigned BD address
 AddMarker(INIT_COMPLETE);             // indicate initialisation complete
 ScanResponseSerialNumber:=0;
 StartLeAdvertising;
 StartPassiveScanning;
end.
