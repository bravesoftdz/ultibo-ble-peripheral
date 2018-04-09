unit uHCI;

{$mode delphi}{$H+}
//{$define show_data}

interface

uses 
Classes,SysUtils,Serial,BCM2710,GlobalConst;

const 
 ATT_PROPERTY_BROADCAST                   = $01;
 ATT_PROPERTY_READ                        = $02;
 ATT_PROPERTY_WRITE_WITHOUT_RESPONSE      = $04;
 ATT_PROPERTY_WRITE                       = $08;
 ATT_PROPERTY_NOTIFY                      = $10;
 ATT_PROPERTY_INDICATE                    = $20;
 ATT_PROPERTY_AUTHENTICATED_SIGNED_WRITES = $40;
 ATT_PROPERTY_EXTENDED_PROPERTIES         = $80;

 ServiceUuid                     = $2800;
 CharacteristicUuid              = $2803;
 AttributePermissionRead         = $01;
 AttributePermissionWrite        = $02;
 AttributePermissionWriteCommand = $04;

 HCI_COMMAND_PKT                 = $01;
 HCI_ACLDATA_PKT                 = $02;
 HCI_SCODATA_PKT                 = $03;
 HCI_EVENT_PKT                   = $04;
 HCI_VENDOR_PKT                  = $ff;

 // Markers
 FIRMWARE_START                  = 100;
 FIRMWARE_END                    = 101;
 DELAY_50MSEC                    = 102;
 DELAY_2SEC                      = 103;
 INIT_COMPLETE                   = 104;
 FLUSH_PORT                      = 105;
 OPEN_PORT                       = 106;
 CLOSE_PORT                      = 107;
 SEND_DATA                       = 109;
 SYSTEM_RESTART                  = 109;
 CONNECTION_TERMINATED           = 110;
 SET_DEFAULT_BAUD                = 111;
 SET_TURBO_BAUD                  = 112;

 BDADDR_LEN                      = 6;

 OGF_MARKER                      = $00;
 OGF_LINK_CONTROL                = $01;
 OGF_LINK_POLICY                 = $02;
 OGF_HOST_CONTROL                = $03;
 OGF_INFORMATIONAL               = $04;
 OGF_LE_CONTROL                  = $08;
 OGF_VENDOR                      = $3f;

 BT_DEFAULT_BAUD                 = 115200;
 BT_TURBO_BAUD                   = 115200; // 230400; // 460800; 921600;

type 
 TBTMarkerEvent = procedure (no:integer);
 TBTLEEvent = procedure (SubEvent:byte; Params:array of byte);

 TBDAddr = array [0 .. BDADDR_LEN - 1] of byte;

 PQueueItem = ^TQueueItem;
 TQueueItem = record
  OpCode:Word;
  Params:array of byte;
 end;

function  OpenUART0:boolean;
procedure CloseUART0;

procedure SendData(ConnectionHandle:Word;Data:Array of Byte);
procedure AddHCICommand(OGF:byte; OCF:Word; Params:array of byte); overload;
procedure AddHCICommand(OpCode:Word; Params:array of byte); overload;

function ogf(Op:Word):byte;
function ocf(Op:Word):Word;
function BDAddrToStr(Addr:TBDAddr):string;
function ErrToStr(Code:byte):string;

procedure NoOP;
procedure AddMarker(Marker:Word);
procedure SetMarkerEvent(anEvent:TBTMarkerEvent);
procedure SetLEEvent(anEvent:TBTLEEvent);
procedure SetLEScanParameters (Type_ : byte; Interval, Window : Word; OwnAddressType, FilterPolicy : byte);
procedure SetLEScanEnable (State, Duplicates : boolean);

// HCI Commands
procedure ResetChip;
procedure ReadLocalName;

// Informational Parameters
procedure ReadLocalVersion;
procedure ReadLocalSupportedCommands;
procedure ReadLocalSupportedFeatures;
procedure ReadBDADDR;

// LE
procedure SetLEEventMask(Mask:QWord);
procedure ReadLEBufferSize;
procedure ReadLESupportedFeatures;
procedure SetLERandomAddress(Addr:TBDAddr);
procedure SetLEAdvertisingParameters(MinInterval,MaxInterval:Word; Type_:byte; OwnAddressType,PeerAddressType:byte; PeerAddr:TBDAddr; ChannelMap,FilterPolicy:byte);
procedure ReadLEAdvertisingChannelTxPower;
procedure SetLEAdvertisingData(Data:array of byte);
procedure SetLEScanResponseData(Data:array of byte);
procedure SetLEAdvertisingEnable(State:boolean);
procedure LERand;

// BCM Vendor Specific
procedure BCMSetBDAddr(Addr:TBDAddr);
procedure BCMLoadFirmware(fn:string);
function EventTypeToStr(Type_:byte):string;

type 
 PBleAttribute = ^TBleAttribute;
 TBleAttributeProc = procedure (Attribute:PBleAttribute);
 TBleAttribute = record
  Handle:Word;
  AttributeType:Word;
  Value:Array[0..31] of Byte;
  ValueLength:Integer;
  ValueMinWriteLength:Integer;
  ValueMaxWriteLength:Integer;
  Permissions:Byte;
  OnChanged:TBleAttributeProc;
  procedure AddByte(X:Byte);
  procedure AddWord(X:Word);
  procedure SetByte(X:Byte);
  procedure SetWord(X:Word);
  procedure SetArray(X:Array of Byte);
  procedure SetString(X:String);
  function ReadByte:Byte;
  function ReadWord:Word;
  procedure Notify;
  procedure SendValue(ConnectionHandle:Word;Message:String;Header:Array of Byte);
 end;

var 
 ChipName:string = '';
 Ver:byte = 0;
 Rev:Word = 0;
 BDAddr:TBDAddr = ($b8,$27,$e8,$cc,$72,$27);
 FWHandle:integer; // firmware file handle
 RxBuffer:array of byte;
 HciSequenceNumber:Integer;
 AttributesByHandle:Array of PBleAttribute;
 ImmediateAlertLevel:PBleAttribute;
 BatteryLevel:PBleAttribute;

procedure Log(Message:String);
procedure BuildAttributes;
procedure AddService(Uuid:Word);
procedure AddCharacteristic(var Value:PBleAttribute;Uuid:Word;MinWriteLength,MaxWriteLength:Integer;UserDescriptor:String;Properties:Byte);

const 
 ADV_IND                     = $00; // Connectable undirected advertising(default)
 ADV_DIRECT_IND_HI           = $01; // Connectable high duty cycle directed advertising
 ADV_SCAN_IND                = $02; // Scannable undirected advertising
 ADV_NONCONN_IND             = $03; // Non connectable undirected advertising
 ADV_DIRECT_IND_LO           = $04; // Connectable low duty cycle directed advertising

implementation

uses Platform,GlobalTypes,GlobalConfig,Threads,FileSystem,SyncObjs,Logging;

procedure Log(Message:String);
begin
 LoggingOutput(Message);
end;

var 
 UART0:PSerialDevice = Nil;
 ReadHandle:TThreadHandle = INVALID_HANDLE_VALUE;
 Queue:TMailslotHandle;
 QueueHandle:TThreadHandle = INVALID_HANDLE_VALUE;
 QueueEvent:TEvent;
 MarkerEvent:TBTMarkerEvent = Nil;
 LEEvent:TBTLEEvent = Nil;

function EventTypeToStr(Type_:byte):string;
begin
 case Type_ of 
  ADV_IND          :Result:='Connectable undirected advertising (default)';
  ADV_DIRECT_IND_HI:Result:='Connectable high duty cycle directed advertising';
  ADV_SCAN_IND     :Result:='Scannable undirected advertising';
  ADV_NONCONN_IND  :Result:='Non connectable undirected advertising';
  ADV_DIRECT_IND_LO:Result:='Connectable low duty cycle directed advertising';
  else                Result:='Reserved for future use(' + Type_.ToHexString(2) + ')';
 end;
end;

function ogf(op:Word):byte;
begin
 Result:=(op shr 10) and $3f;
end;

function ocf(op:Word):Word;
begin
 Result:=op and $3ff;
end;

procedure SetMarkerEvent(anEvent:TBTMarkerEvent);
begin
 MarkerEvent:=anEvent;
end;

procedure SetLEEvent(anEvent:TBTLEEvent);
begin
 LEEvent:=anEvent;
end;

function BDAddrToStr(Addr:TBDAddr):string;
var 
 i:integer;
begin
 Result:='';
 for i:=5 downto 0 do
  if i = 5 then
   Result:=Addr[i].ToHexString(2)
  else
   Result:=Result + ':' + Addr[i].ToHexString(2);
end;

procedure DecodeEvent(ev:array of byte);
var 
 len,num:byte;
 op:Word;
 se:byte; // number of responses
 i:integer;
 prm:array of byte;
 s:string;
begin
 if length(ev) < 3 then exit;
 if ev[0] <> HCI_EVENT_PKT then
  exit;
 len:=ev[2];
 num:=0;
 if len + 2 <> high(ev) then exit;
 case ev[1] of 
  // event code
  $05 :
       begin
        Log(Format('%02.2x) connection terminated status 0x%02.2x reason 0x%02.2x',[256*ev[5] + ev[4],ev[3],ev[6]]));
        AddMarker(CONNECTION_TERMINATED);
       end;
  $0e:  // command complete
      begin
       num:=ev[3];          // num packets controller can accept
       op:=ev[5] * $100 + ev[4];
       if (len > 3) and(ev[6] > 0) then
        begin
         Log(Format('%s OGF %02.2x OCF %03.3x OP Code %04.4x Num %d Len %d',[ErrToStr(ev[6]),ogf(op),ocf(op),op,num,len]));
         Sleep(30*1000);
        end;
       case op of 
        $0c14:// read name
              begin
               ChipName:='';
               i:=7;
               while (i <= len + 3) and(ev[i] <> $00) do
                begin
                 ChipName:=ChipName + chr(ev[i]);
                 i:=i + 1;
                end;
              end;
        $1001:// read local version
              begin
               if len = 12 then
                begin
                 Ver:=ev[7];
                 Rev:=ev[9] * $100 + ev[8];
                end;
              end;
        $1009:// read bd addr
              begin
               if len = 10 then for i:=0 to 5 do
                                 BDAddr[i]:=ev[7 + i];
              end;
        $2007:// read le channel tx power
              begin
               //             if len = 5 then Log('Tx Power ' + ev[7].ToString);
              end;
       end;
       // case op
      end;
  // command complete
  $0f:// command status
      begin
       //    Log('Command Status');
       if len = 4 then
        begin
         num:=ev[4];
         if ev[3] <> 0 then
          begin
           op:=ev[6] * $100 + ev[5];
           Log('  Status ' + inttohex(ev[3],2));
           Log('  OGF ' + inttohex(ogf(op),2) + ' OCF ' + inttohex(ocf(op),3) + ' OP Code ' + inttohex(op,4));
          end;
        end;
      end;
  $13:// transmit status
      begin
      end;
  $3e:// le meta event
      begin
       if (len > 2) then
        begin
         se:=ev[3];
         if Assigned(LEEvent) then
          begin
           SetLength(prm,len - 1);
           Move(ev[4],prm[0],len - 1);
           LEEvent(se,prm);
          end;
        end;
       // len <=
      end
      // case
      else
       begin
        s:='';
        for i:=low(ev) + 1 to high(ev) do
         s:=s + ' ' + ev[i].ToHexString(2);
        Log('Unknown event ' + s);
       end;
 end;
 if num > 0 then
  QueueEvent.SetEvent;
end;

procedure SendData(ConnectionHandle:Word;Data:Array of Byte);
var 
 Packet:Array of Byte;
 I:Integer;
procedure AddByte(X:Byte);
begin
 SetLength(Packet,Length(Packet) + 1);
 Packet[Length(Packet) - 1]:=X;
end;
begin
 SetLength(Packet,0);
 AddByte(HCI_ACLDATA_PKT);
 AddByte(lo(ConnectionHandle));
 AddByte(hi(ConnectionHandle));
 AddByte(4+Length(Data));
 AddByte(0);
 AddByte(Length(Data));
 AddByte(0);
 AddByte(4);
 AddByte(0);
 for I:=Low(Data) to High(Data) do
  AddByte(Data[I]);
 AddHCICommand(OGF_MARKER,SEND_DATA and $3ff,Packet);
end;

procedure SendInvalidHandle(ConnectionHandle:Word;Message:String;OpCode:Byte;Handle:Word);
begin
 Log(Format('%s -> handle %02.2x invalid',[Message,Handle]));
 SendData(ConnectionHandle,[1,OpCode,lo(Handle),hi(Handle),$01]);
end;

procedure SendWriteNotPermitted(ConnectionHandle:Word;Message:String;OpCode:Byte;Handle:Word);
begin
 Log(Format('%s -> handle %02.2x write not permitted',[Message,Handle]));
 SendData(ConnectionHandle,[1,OpCode,lo(Handle),hi(Handle),$03]);
end;

procedure SendHandleNotFound(ConnectionHandle:Word;Message:String;OpCode:Byte;Handle:Word);
begin
 Log(Format('%s -> handle %02.2x not found',[Message,Handle]));
 SendData(ConnectionHandle,[1,OpCode,lo(Handle),hi(Handle),$0a]);
end;

procedure SendInvalidAttributeValueLength(ConnectionHandle:Word;Message:String;OpCode:Byte;Handle:Word);
begin
 Log(Format('%s -> handle %02.2x invalid attribute value length',[Message,Handle]));
 SendData(ConnectionHandle,[1,OpCode,lo(Handle),hi(Handle),$0d]);
end;

procedure AclDataReceived(ConnectionHandle: Word; pkt:array of byte);
var 
 I,K:Integer;
 S:String;
 OpCode:Byte;
 StartHandle,EndHandle,AnotherHandle,NextHandle,FinalHandle,SearchType:Word;
 Properties:Byte;
 Found:Boolean;
 Uuid:Word;
 Message:String;
 IsCommand:Boolean;
 DataLength:Integer;
function GetByte:Byte;
begin
 Result:=pkt[I];
 Inc(I);
end;
function GetWord:Word;
begin
 Result:=pkt[I] + 256*pkt[I+1];
 Inc(I,2);
end;
begin
 I:=0;
 OpCode:=GetByte;
 // IsSigned:=(OpCode and $80) <> 0;
 IsCommand:=(OpCode and $40) <> 0;
 case (OpCode and $3f) of 
  $01:
      begin
       s:='';
       while I < Length(pkt) do
        s:=s + GetByte.ToHexString(2);
       Log(Format('%02.2x) rx error data %s',[ConnectionHandle,s]));
      end;
  // $04:
  //    begin
  //     StartHandle:=GetWord;
  //     EndHandle:=GetWord;
  //     Message:=Format('%02.2x) find information %04.4x-%04.4x',[ConnectionHandle,StartHandle,EndHandle]);
  //     if (StartHandle = 1) and(EndHandle >= 8) then
  //      SendData(ConnectionHandle,[5,1,3,0,$00,$2a,5,0,$01,$2a,8,0,$05,$2a])
  //     else
  //      SendHandleNotFound(ConnectionHandle,Message,OpCode,StartHandle);
  //    end;
  $08:
      begin
       StartHandle:=GetWord;
       EndHandle:=GetWord;
       SearchType:=GetWord;
       Message:=Format('%02.2x) read by type %04.4x handles %02.2x-%02.2x',[ConnectionHandle,SearchType,StartHandle,EndHandle]);
       Found:=False;
       for AnotherHandle:=StartHandle to Min(EndHandle,Length(AttributesByHandle) - 1) do
        begin
         with AttributesByHandle[AnotherHandle]^ do
          begin
           if AttributeType = SearchType then
            begin
             s:='';
             I:=0;
             while I < ValueLength do
              begin
               s:=s + Value[I].ToHexString(2) + ' ';
               Inc(I);
              end;
             SendValue(ConnectionHandle,Message,[$09,2 + ValueLength,lo(AnotherHandle),hi(AnotherHandle)]);
             Found:=True;
             break;
            end;
          end;
        end;
       if not Found then
        SendHandleNotFound(ConnectionHandle,Message,OpCode,StartHandle)
      end;
  $09:
      begin
       GetByte;
       while I < High(Pkt) do
        begin
         StartHandle:=GetWord;
         Properties:=GetByte;
         AnotherHandle:=GetWord;
         SearchType:=GetWord;
         Log(Format('%02.2x.%02.2x) properties 0x%02.2x handle 0x%02.2x type 0x%04.4x',[ConnectionHandle,StartHandle,Properties,AnotherHandle,SearchType]));
         if (Properties and $02) <> 0 then
          begin
           //              L:=2; // can only have one outstanding request at a time
           //              SendData([HCI_ACLDATA_PKT,lo(ConnectionHandle),hi(ConnectionHandle),4+L+1,0,L+1,0,4,0,$a,lo(AnotherHandle),hi(AnotherHandle)]);
          end;
        end;
       NextHandle:=StartHandle+1;
       SendData(ConnectionHandle,[$8,lo(NextHandle),hi(NextHandle),$ff,$ff,$03,$28]);
      end;
  $0A:
      begin
       StartHandle:=GetWord;
       Message:=Format('%02.2x) read handle %02.2x',[ConnectionHandle,StartHandle]);
       if (StartHandle >= 1) and (StartHandle < Length(AttributesByHandle)) then
        begin
         with AttributesByHandle[StartHandle]^ do
          SendValue(ConnectionHandle,Message,[$b]);
        end
       else
        SendHandleNotFound(ConnectionHandle,Message,OpCode,StartHandle);
      end;
  $0B:
      begin
       s:='';
       while I < Length(pkt) do
        s:=s + GetByte.ToHexString(2);
       Log(Format('%02.2x) rx value %s',[ConnectionHandle,s]));
      end;
  $10:
      begin
       StartHandle:=GetWord;
       EndHandle:=GetWord;
       SearchType:=GetWord;
       Message:=Format('%02.2x) read by group type %04.4x handles %02.2x-%02.2x',[ConnectionHandle,SearchType,StartHandle,EndHandle]);
       Found:=False;
       if SearchType = ServiceUuid then
        begin
         for AnotherHandle:=StartHandle to Min(EndHandle,Length(AttributesByHandle) - 1) do
          begin
           with AttributesByHandle[AnotherHandle]^ do
            begin
             if AttributeType = ServiceUuid then
              begin
               Uuid:=ReadWord;
               for FinalHandle:=AnotherHandle + 1 to Length(AttributesByHandle) - 1 do
                if AttributesByHandle[FinalHandle].AttributeType = ServiceUuid then
                 begin
                  Found:=True;
                  break;
                 end;
               if Found then
                Dec(FinalHandle);
               Log(Format('%s -> service %04.4x handles %02.2x-%02.2x',[Message,Uuid,AnotherHandle,FinalHandle]));
               SendData(ConnectionHandle,[$11,6,Lo(AnotherHandle),Hi(AnotherHandle),Lo(FinalHandle),Hi(FinalHandle),Lo(Uuid),Hi(Uuid)]);
               Found:=True;
               break;
              end;
            end;
          end;
        end;
       if not Found then
        SendHandleNotFound(ConnectionHandle,Message,OpCode,StartHandle);
      end;
  $12:
      begin
       StartHandle:=GetWord;
       if IsCommand then
        Message:='write command'
       else
        Message:='write request';
       if not (StartHandle >= 1) and (StartHandle <= Length(AttributesByHandle) - 1) then
        SendInvalidHandle(ConnectionHandle,Message,OpCode,StartHandle)
       else
        with AttributesByHandle[StartHandle]^ do
         begin
          if (not IsCommand and ((Permissions and AttributePermissionWrite) = 0)) or (IsCommand and ((Permissions and AttributePermissionWriteCommand) = 0)) then
           SendWriteNotPermitted(ConnectionHandle,Message,OpCode,StartHandle)
          else
           begin
            DataLength:=Length(pkt) - I;
            if not (DataLength >= ValueMinWriteLength) and (DataLength <= ValueMaxWriteLength) then
             SendInvalidAttributeValueLength(ConnectionHandle,Message,OpCode,StartHandle)
            else
             begin
              s:='';
              K:=0;
              while I < Length(pkt) do
               begin
                Value[K]:=GetByte;
                s:=s + Value[K].ToHexString(2) + ' ';
                Inc(K);
               end;
              ValueLength:=DataLength;
              Log(Format('%02.2x) write handle %02.2x data %s',[ConnectionHandle,StartHandle,s]));
              if not IsCommand then
               Log('must respond');
              if Assigned(OnChanged) then
               OnChanged(AttributesByHandle[StartHandle]);
             end;
           end;
         end;
      end;
  $1b:
      begin
       StartHandle:=GetWord;
       s:='';
       while I < Length(pkt) do
        s:=s + GetByte.ToHexString(2);
       Log(Format('%02.2x.%02.2x) notified %s',[ConnectionHandle,StartHandle,s]));
      end;
  $1d:
      begin
       StartHandle:=GetWord;
       s:='';
       while I < Length(pkt) do
        s:=s + GetByte.ToHexString(2);
       Log(Format('%02.2x.%02.2x) indicated %s',[ConnectionHandle,StartHandle,s]));
       SendData(ConnectionHandle,[$1e]);
      end;
  else
   begin
    s:='';
    while I < Length(pkt) do
     s:=s + GetByte.ToHexString(2);
    Log(Format('%02.2x) rx opcode %02.2x data %s',[ConnectionHandle,OpCode,s]));
   end;
 end;
end;

function ReadExecute(Parameter:Pointer):PtrInt;
var 
 DataLength:Integer;
 c:LongWord;
 b:byte;
 i,j,rm:integer;
 decoding:boolean;
 pkt:array of byte;
 res:LongWord;
begin
 try
  Result:=0;
  c:=0;
  while True do
   begin
    res:=SerialDeviceRead(UART0,@b,1,SERIAL_READ_NONE,c);
    //  if (res <> ERROR_SUCCESS) and (res <> ERROR_NO_MORE_ITEMS) then
    //   begin
    //    Log('ReadExecute exit');
    //    break;
    //   end;
    if (res = ERROR_SUCCESS) and (c = 1) then
     begin
      // One byte was received,try to read everything that is available
      SetLength(RxBuffer,length(RxBuffer) + 1);
      RxBuffer[high(RxBuffer)]:=b;
      res:=SerialDeviceRead(UART0,@b,1,SERIAL_READ_NON_BLOCK,c);
      //    if (res <> ERROR_SUCCESS) and (res <> ERROR_NO_MORE_ITEMS) then
      //     begin
      //      Log('ReadExecute exit');
      //      break;
      //     end;
      while (res = ERROR_SUCCESS) and(c = 1) do
       begin
        SetLength(RxBuffer,length(RxBuffer) + 1);
        RxBuffer[high(RxBuffer)]:=b;
        res:=SerialDeviceRead(UART0,@b,1,SERIAL_READ_NON_BLOCK,c);
        //      if (res <> ERROR_SUCCESS) and (res <> ERROR_NO_MORE_ITEMS) then
        //       begin
        //        Log('ReadExecute exit');
        //        break;
        //       end;
       end;
      //if Length(RxBuffer) > 50 then
      // Log(Format('rx buffer %d',[Length(RxBuffer)]));
      i:=0;
      decoding:=True;
      while decoding do
       begin
        decoding:=False;
        if RxBuffer[i] = HCI_ACLDATA_PKT then
         begin
          if i + 5 - 1 <= High(RxBuffer) then
           begin
            DataLength:=256 * RxBuffer[i + 6] + RxBuffer[i + 5];
            //                Log(Format('waiting ACLDATA 0x%02.2x%02.2x %d',[RxBuffer[i + 2],RxBuffer[i + 1],DataLength]));
            if i + 9 + DataLength - 1 <= High(RxBuffer) then
             begin
              SetLength(pkt,DataLength);
              for j:=0 to length(pkt) - 1 do
               pkt[j]:=RxBuffer[9 + j];
              AclDataReceived((RxBuffer[1] + RxBuffer[2]*256) and $3ff,pkt);
              Inc(I,9 + DataLength);
              decoding:=i < high(RxBuffer);
             end;
           end;
         end
        else if RxBuffer[i] <> HCI_EVENT_PKT then
              Log(Format('not event %d',[RxBuffer[i]]))
        else if (i + 2 <= high(RxBuffer)) then // mimumum
              if i + RxBuffer[i + 2] + 2 <= high(RxBuffer) then
               begin
                SetLength(pkt,RxBuffer[i + 2] + 3);
                for j:=0 to length(pkt) - 1 do
                 pkt[j]:=RxBuffer[i + j];
{$ifdef show_data}
                s:='';
                for j:=low(pkt) to high(pkt) do
                 s:=s + ' ' + pkt[j].ToHexString(2);
                Log('<--' + s);
{$endif}
                DecodeEvent(pkt);
                i:=i + length(pkt);
                decoding:=i < high(RxBuffer);
               end;
       end;
      // decoding
      if i > 0 then
       begin
        rm:=length(RxBuffer) - i;
        //              Log('Remaining ' + IntToStr(rm));
        if rm > 0 then
         for j:=0 to rm - 1 do
          RxBuffer[j]:=RxBuffer[j + i];
        SetLength(RxBuffer,rm);
       end;
     end;
   end;
 except
  on E:Exception do
       Log(Format('ReadExecute exception %s',[E.Message]));
end;
end;

var 
 SpecifiedBaud:LongWord = BT_DEFAULT_BAUD;

procedure SetBaud(Baud:LongWord);
var 
 Params:Array of Byte;
 LW,HW:Word;
begin
 SetLength(Params, 6);
 LW:=Baud and $ffff;
 HW:=(Baud shr 16) and $ffff;
 Params[0]:=$00;
 Params[1]:=$00;
 Params[2]:=Lo(LW);
 Params[3]:=Hi(LW);
 Params[4]:=Lo(HW);
 Params[5]:=Hi(HW);
 AddHCICommand(OGF_VENDOR,$018,Params);
 SpecifiedBaud:=Baud;
 Log('');
 Log(Format('setting baud %d',[SpecifiedBaud]));
end;

function OpenUART0:boolean;
var 
 res:LongWord;
begin
 Result:=False;
 UART0:=SerialDeviceFindByDescription(BCM2710_UART0_DESCRIPTION);
 if UART0 = nil then
  begin
   Log('Can''t find UART0');
   exit;
  end;
 Log('');
 Log(Format('opening baud %d',[SpecifiedBaud]));
 res:=SerialDeviceOpen(UART0,SpecifiedBaud,SERIAL_DATA_8BIT,SERIAL_STOP_1BIT,SERIAL_PARITY_NONE,SERIAL_FLOW_NONE,0,0);
 if res = ERROR_SUCCESS then
  begin
   Result:=True;
   GPIOFunctionSelect(GPIO_PIN_14,GPIO_FUNCTION_IN);
   GPIOFunctionSelect(GPIO_PIN_15,GPIO_FUNCTION_IN);
   GPIOFunctionSelect(GPIO_PIN_32,GPIO_FUNCTION_ALT3);     // TXD0
   GPIOFunctionSelect(GPIO_PIN_33,GPIO_FUNCTION_ALT3);     // RXD0
   GPIOPullSelect(GPIO_PIN_32,GPIO_PULL_NONE);             //Added
   GPIOPullSelect(GPIO_PIN_33,GPIO_PULL_UP);               //Added
   ReadHandle:=SysBeginThreadEx(Nil,THREAD_STACK_DEFAULT_SIZE,@ReadExecute,Nil,0,THREAD_PRIORITY_NORMAL,CPU_AFFINITY_2,CPU_ID_2,'ReadExecute',ReadHandle);
   Result:=ReadHandle <> INVALID_HANDLE_VALUE;
  end;
end;

procedure CloseUART0;
begin
 //  WaitForThreadTerminate(ReadHandle,0);
 if ReadHandle <> INVALID_HANDLE_VALUE then
  KillThread(ReadHandle);
 Sleep(2*1000);
 ReadHandle:=INVALID_HANDLE_VALUE;
 if UART0 <> nil then
  SerialDeviceClose(UART0);
 UART0:=Nil;
 Sleep(2*1000);
end;

procedure AddHCICommand(OGF:byte; OCF:Word; Params:array of byte);
begin
 AddHCICommand((OGF shl 10) or OCF,Params);
end;

procedure AddHCICommand(OpCode:Word; Params:array of byte);
var 
 anItem:PQueueItem;
 i:integer;
begin
 New(anItem);
 anItem^.OpCode:=OpCode;
 SetLength(anItem^.Params,length(Params));
 for i:=0 to length(Params) - 1 do
  anItem^.Params[i]:=Params[i];
 if MailSlotSend(Queue,Integer(anItem)) <> ERROR_SUCCESS then
  Log('Error adding Command to queue.');
end;

function QueueHandler(Parameter:Pointer):PtrInt;
var 
 anItem:PQueueItem;
 Cmd:array of byte;
 i:integer;
 res,count:LongWord;
 s:string;
begin
 Result:=0;
 while True do
  begin
   QueueEvent.ResetEvent;
   anItem:=PQueueItem(MailslotReceive(Queue));
   if anItem <> nil then
    begin
     Inc(HciSequenceNumber);
     //if anItem^.OpCode <> $fc4c then
     // Log(Format('started hci sequence %d op code %04.4x',[HciSequenceNumber,anItem^.OpCode]));
     if (ogf(anItem^.OpCode) = OGF_MARKER) and(ocf(anItem^.OpCode) = SEND_DATA) then
      begin
       count:=0;
       s:='';
       for i:=10 to length(anItem^.Params) - 1 do
        s:=s + ' ' + anItem^.Params[i].ToHexString(2);
       //     Log(Format('%02.2x) tx opcode %02.2x%s',[anItem^.Params[1] + 256*anItem^.Params[2],anItem^.Params[9],s]));
       res:=SerialDeviceWrite(UART0,@anItem^.Params[0],length(anItem^.Params),SERIAL_WRITE_NONE,count);
       if res <> ERROR_SUCCESS then
        Log('Error writing to BT.');
      end
     else if (ogf(anItem^.OpCode) = OGF_MARKER) and(ocf(anItem^.OpCode) > 0) then
           begin
            case ocf(anItem^.OpCode) of 
             DELAY_50MSEC      : QueueEvent.WaitFor(50);
             DELAY_2SEC        : QueueEvent.WaitFor(2000);
             OPEN_PORT         : OpenUART0;
             CLOSE_PORT        : CloseUART0;
             SET_DEFAULT_BAUD  : SetBaud(BT_DEFAULT_BAUD);
             SET_TURBO_BAUD    : SetBaud(BT_TURBO_BAUD);
             FIRMWARE_END      : SpecifiedBaud:=BT_DEFAULT_BAUD;
            end;
            if Assigned(@MarkerEvent) then MarkerEvent(ocf(anItem^.OpCode));
           end
     else
      begin
       SetLength(Cmd,length(anItem^.Params) + 4);
       Cmd[0]:=HCI_COMMAND_PKT;
       Cmd[1]:=lo(anItem^.OpCode);          // little endian so lowest sent first
       Cmd[2]:=hi(anItem^.OpCode);
       Cmd[3]:=length(anItem^.Params);
       for i:=0 to length(anItem^.Params) - 1 do
        Cmd[4 + i]:=anItem^.Params[i];
       count:=0;
{$ifdef show_data}
       s:='';
       for i:=0 to length(Cmd) - 1 do
        s:=s + ' ' + Cmd[i].ToHexString(2);
       Log('--> ' + s);
{$endif}
       res:=SerialDeviceWrite(UART0,@Cmd[0],length(Cmd),SERIAL_WRITE_NONE,count);
       if res = ERROR_SUCCESS then
        begin
         if QueueEvent.WaitFor(3*1000) <> wrSignaled then
          begin
           s:='';
           for i:=0 to length(Cmd) - 1 do
            s:=s + ' ' + Cmd[i].ToHexString(2);
           Log(Format('hci command sequence number %d op code %04.4x',[HciSequenceNumber,anItem^.OpCode]));
           Log(Format('-->(%d) %s',[Length(Cmd),s]));
           Log('Timeout waiting for BT Response.'); // should send nop ???
           s:='';
           for i:=0 to length(RxBuffer) - 1 do
            s:=s + ' ' + RxBuffer[i].ToHexString(2);
           Log('<-- ' + s);
           ThreadHalt(0);
          end;
        end
       else
        Log('Error writing to BT.');
      end;
     SetLength(anItem^.Params,0);
     Dispose(anItem);
    end;
  end;
end;

procedure NoOP;  // in spec but not liked by BCM chip
begin
 AddHCICommand($00,$00,[]);
end;

procedure AddMarker(Marker:Word);
begin
 AddHCICommand(OGF_MARKER,Marker and $3ff,[]);
end;

// host control
procedure ResetChip;
begin
 AddHCICommand(OGF_HOST_CONTROL,$03,[]);
end;

procedure ReadLocalName;
begin
 AddHCICommand(OGF_HOST_CONTROL,$14,[]);
end;

// informational parameters
procedure ReadLocalVersion;
begin
 AddHCICommand(OGF_INFORMATIONAL,$01,[]);
end;

procedure ReadLocalSupportedCommands;
begin
 AddHCICommand(OGF_INFORMATIONAL,$02,[]);
end;

procedure ReadLocalSupportedFeatures;
begin
 AddHCICommand(OGF_INFORMATIONAL,$03,[]);
end;

procedure ReadBDADDR;
begin
 AddHCICommand(OGF_INFORMATIONAL,$09,[]);
end;

// le control
procedure SetLEEventMask(Mask:QWord);
var 
 Params:array of byte;
 MaskHi,MaskLo:DWord;
begin
 MaskHi:=(Mask shr 32) and $FFFFFFFF;
 MaskLo:=Mask and $FFFFFFFF;
 SetLength(Params,8);
 Params[0]:=MaskLo and $ff;   // lsb
 Params[1]:=(MaskLo shr 8) and $ff;
 Params[2]:=(MaskLo shr 16) and $ff;
 Params[3]:=(MaskLo shr 24) and $ff;
 Params[4]:=MaskHi and $ff;   // lsb
 Params[5]:=(MaskHi shr 8) and $ff;
 Params[6]:=(MaskHi shr 16) and $ff;
 Params[7]:=(MaskHi shr 24) and $ff;
 AddHCICommand(OGF_LE_CONTROL,$01,Params);
end;

procedure ReadLEBufferSize;
begin
 AddHCICommand(OGF_LE_CONTROL,$02,[]);
end;

procedure ReadLESupportedFeatures;
begin
 AddHCICommand(OGF_LE_CONTROL,$03,[]);
end;

procedure SetLERandomAddress(Addr:TBDAddr);
begin
 AddHCICommand(OGF_LE_CONTROL,$05,[Addr[5],Addr[4],Addr[3],Addr[2],Addr[1],Addr[0]]);
end;

procedure SetLEScanParameters (Type_ : byte; Interval, Window : Word; OwnAddressType, FilterPolicy : byte);
begin
 AddHCICommand (OGF_LE_CONTROL, $0b, [Type_, lo (Interval), hi (Interval), lo (Window), hi (Window), OwnAddressType, FilterPolicy]);
end;

procedure SetLEScanEnable (State, Duplicates : boolean);
var 
 Params : array of byte;
begin
 SetLength (Params, 2);
 if State then Params[0] := $01
 else Params[0] := $00;
 if Duplicates then Params[1] := $01
 else Params[1] := $00;
 AddHCICommand (OGF_LE_CONTROL, $0c, Params);
end;

procedure SetLEAdvertisingParameters(MinInterval,MaxInterval:Word; Type_:byte; OwnAddressType,PeerAddressType:byte; PeerAddr:TBDAddr; ChannelMap,FilterPolicy:byte);
begin
 AddHCICommand(OGF_LE_CONTROL,$06,[lo(MinInterval),hi(MinInterval),
 lo(MaxInterval),hi(MaxInterval),
 Type_,OwnAddressType,PeerAddressType,
 PeerAddr[0],PeerAddr[1],PeerAddr[2],
 PeerAddr[3],PeerAddr[4],PeerAddr[5],
 ChannelMap,FilterPolicy]);
end;

procedure ReadLEAdvertisingChannelTxPower;
begin
 AddHCICommand(OGF_LE_CONTROL,$07,[]);
end;

procedure SetLEAdvertisingData(Data:array of byte);
var 
 Params:array of byte;
 Len:byte;
 i:integer;
begin
 SetLength(Params,32);
 for i:=1 to 31 do
  Params[i]:=0;       // clear data
 Len:=length(Data);
 if Len > 31 then Len:=31;
 Params[0]:=Len;
 for i:=0 to Len - 1 do
  Params[i + 1]:=Data[i];
 AddHCICommand(OGF_LE_CONTROL,$08,Params);
end;

procedure SetLEScanResponseData(Data:array of byte);
var 
 Params:array of byte;
 Len:byte;
 i:integer;
begin
 SetLength(Params,32);
 for i:=1 to 31 do
  Params[i]:=0;       // clear data
 Len:=length(Data);
 if Len > 31 then Len:=31;
 Params[0]:=Len;
 for i:=0 to Len - 1 do
  Params[i + 1]:=Data[i];
 AddHCICommand(OGF_LE_CONTROL,$09,Params);
end;

procedure SetLEAdvertisingEnable(State:boolean);
begin
 if State then
  AddHCICommand(OGF_LE_CONTROL,$0a,[$01])
 else
  AddHCICommand(OGF_LE_CONTROL,$0a,[$00]);
end;

procedure LERand;
begin
 AddHCICommand(OGF_LE_CONTROL,$18,[]);
end;

// BCM vendor specific      http://www.cypress.com/file/298311/download
procedure BCMSetBDAddr(Addr:TBDAddr);
var 
 i:integer;
 Params:array of byte;
begin
 SetLength(Params,6);
 for i:=0 to 5 do
  Params[i]:=Addr[i];
 AddHCICommand(OGF_VENDOR,$001,Params);
end;

procedure BCMLoadFirmware(fn:string);
var 
 hdr:array [0 .. 2] of byte;
 Params:array of byte;
 n,len:integer;
 Op:Word;
begin
 // AddMarker(SET_TURBO_BAUD);
 // AddMarker(CLOSE_PORT);
 // AddMarker(DELAY_50MSEC);
 // AddMarker(OPEN_PORT);
 FWHandle:=FSFileOpen(fn,fmOpenRead);
 if FWHandle > 0 then
  begin
   AddMarker(FIRMWARE_START);
   AddHCICommand(OGF_VENDOR,$02e,[]);
   n:=FSFileRead(FWHandle,hdr,3);
   while (n = 3) do
    begin
     Op:=(hdr[1] * $100) + hdr[0];
     len:=hdr[2];
     SetLength(Params,len);
     n:=FSFileRead(FWHandle,Params[0],len);
     if (len <> n) then Log('Data mismatch.');
     AddHCICommand(Op,Params);
     n:=FSFileRead(FWHandle,hdr,3);
    end;
   FSFileClose(FWHandle);
   AddMarker(FIRMWARE_END);
   AddMarker(CLOSE_PORT);
   AddMarker(DELAY_50MSEC);
   AddMarker(OPEN_PORT);
  end
 else
  Log('Error loading Firmware file ' + fn);
end;

function ErrToStr(code:byte):string;
begin
 // page 377 onwards 4.2
 case code of 
  $00:Result:='Success';
  $01:Result:='Unknown HCI Command';
  $02:Result:='Unknown Connection Identifier';
  $03:Result:='Hardware Failure';
  $04:Result:='Page Timeout';
  $05:Result:='Authentication Failure';
  $06:Result:='PIN or Key Missing';
  $07:Result:='Memory Capacity Exceeded';
  $08:Result:='Connection Timeout';
  $09:Result:='Connection Limit Exceeded';
  $0A:Result:='Synchronous Connection Limit To A Device Exceeded';
  $0B:Result:='ACL Connection Already Exists';
  $0C:Result:='Command Disallowed';
  $0D:Result:='Connection Rejected due to Limited Resources';
  $0E:Result:='Connection Rejected due To Security Reasons';
  $0F:Result:='Connection Rejected due to Unacceptable BD_ADDR';
  $10:Result:='Connection Accept Timeout Exceeded';
  $11:Result:='Unsupported Feature or Parameter Value';
  $12:Result:='Invalid HCI Command Parameters';
  $13:Result:='Remote User Terminated Connection';
  $14:Result:='Remote Device Terminated Connection due to Low Resources';
  $15:Result:='Remote Device Terminated Connection due to Power Off';
  $16:Result:='Connection Terminated By Local Host';
  $17:Result:='Repeated Attempts';
  $18:Result:='Pairing Not Allowed';
  $19:Result:='Unknown LMP PDU';
  $1A:Result:='Unsupported Remote Feature / Unsupported LMP Feature';
  $1B:Result:='SCO Offset Rejected';
  $1C:Result:='SCO Interval Rejected';
  $1D:Result:='SCO Air Mode Rejected';
  $1E:Result:='Invalid LMP Parameters / Invalid LL Parameters';
  $1F:Result:='Unspecified Error';
  $20:Result:='Unsupported LMP Parameter Value / Unsupported LL Parameter Value';
  $21:Result:='Role Change Not Allowed';
  $22:Result:='LMP Response Timeout / LL Response Timeout';
  $23:Result:='LMP Error Transaction Collision';
  $24:Result:='LMP PDU Not Allowed';
  $25:Result:='Encryption Mode Not Acceptable';
  $26:Result:='Link Key cannot be Changed';
  $27:Result:='Requested QoS Not Supported';
  $28:Result:='Instant Passed';
  $29:Result:='Pairing With Unit Key Not Supported';
  $2A:Result:='Different Transaction Collision';
  $2B:Result:='Reserved';
  $2C:Result:='QoS Unacceptable Parameter';
  $2D:Result:='QoS Rejected';
  $2E:Result:='Channel Classification Not Supported';
  $2F:Result:='Insufficient Security';
  $30:Result:='Parameter Out Of Mandatory Range';
  $31:Result:='Reserved';
  $32:Result:='Role Switch Pending';
  $33:Result:='Reserved';
  $34:Result:='Reserved Slot Violation';
  $35:Result:='Role Switch Failed';
  $36:Result:='Extended Inquiry Response Too Large';
  $37:Result:='Secure Simple Pairing Not Supported By Host';
  $38:Result:='Host Busy - Pairing';
  $39:Result:='Connection Rejected due to No Suitable Channel Found';
  $3A:Result:='Controller Busy';
  $3B:Result:='Unacceptable Connection Parameters';
  $3C:Result:='Directed Advertising Timeout';
  $3D:Result:='Connection Terminated due to MIC Failure';
  $3E:Result:='Connection Failed to be Established';
  $3F:Result:='MAC Connection Failed';
  $40:Result:='Coarse Clock Adjustment Rejected but Will Try to Adjust Using Clock';
 end;
end;

var 
 DeviceName:PBleAttribute;
 Appearance:PBleAttribute;
 ServicesChanged:PBleAttribute;

procedure TBleAttribute.AddByte(X:Byte);
begin
 Value[ValueLength]:=X;
 Inc(ValueLength);
end;

procedure TBleAttribute.AddWord(X:Word);
begin
 Value[ValueLength]:=Lo(X);
 Value[ValueLength + 1]:=Hi(X);
 Inc(ValueLength,2);
end;

procedure TBleAttribute.SetByte(X:Byte);
begin
 Value[0]:=X;
 ValueLength:=1;
end;

procedure TBleAttribute.SetWord(X:Word);
begin
 Value[0]:=Lo(X);
 Value[1]:=Hi(X);
 ValueLength:=2;
end;

procedure TBleAttribute.SetArray(X:Array of Byte);
var 
 I:Integer;
begin
 for I:=Low(X) to High(X) do
  Value[I]:=X[I];
 ValueLength:=Length(X);
end;

procedure TBleAttribute.SetString(X:String);
var 
 I:Integer;
begin
 for I:=1 to Length(X) do
  Value[I - 1]:=Ord(X[I]);
 ValueLength:=Length(X);
end;

function TBleAttribute.ReadByte:Byte;
begin
 Result:=Value[0];
end;

function TBleAttribute.ReadWord:Word;
begin
 Result:=(Value[1] shl 8) + Value[0];
end;

procedure TBleAttribute.SendValue(ConnectionHandle:Word;Message:String;Header:Array of Byte);
var 
 I:Integer;
 Data:Array of Byte;
 S:String;
begin
 SetLength(Data,Length(Header) + ValueLength);
 for I:=0 to High(Header) do
  Data[I]:=Header[I];
 I:=0;
 while I < ValueLength do
  begin
   Data[I + Length(Header)]:=Value[I];
   Inc(I);
  end;
 S:='';
 for I:=0 to Length(Data) - 1 do
  S:=S + Data[I].ToHexString(2) + ' ';
 Log(Format('%s -> opcode %s',[Message,S]));
 SendData(ConnectionHandle,Data);
end;

//(ConnectionHandle:Word;Handle:Word;Value:Byte);
procedure TBleAttribute.Notify;
begin
 // Log(Format('notifying handle %04.4x value %d',[Handle,Value]));
 // SendData(ConnectionHandle,[$1b,3,lo(Handle),hi(Handle),Value]);
end;

function AddAttribute(AttributeType:Word;MinWriteLength,MaxWriteLength:Integer):PBleAttribute;
begin
 New(Result);
 Result.Handle:=Length(AttributesByHandle);
 SetLength(AttributesByHandle,Result.Handle + 1);
 AttributesByHandle[Result.Handle]:=Result;
 Result.AttributeType:=AttributeType;
 Result.Permissions:=0;
 Result.ValueLength:=0;
 Result.ValueMinWriteLength:=MinWriteLength;
 Result.ValueMaxWriteLength:=MaxWriteLength;
end;

procedure AddService(Uuid:Word);
var 
 Service:PBleAttribute;
begin
 Service:=AddAttribute(ServiceUuid,2,2);
 Service.AddWord(Uuid);
end;

procedure AddCharacteristic(var Value:PBleAttribute;Uuid:Word;MinWriteLength,MaxWriteLength:Integer;UserDescriptor:String;Properties:Byte);
var 
 Characteristic:PBleAttribute;
begin
 Characteristic:=AddAttribute(CharacteristicUuid,0,0);
 Value:=AddAttribute(Uuid,MinWriteLength,MaxWriteLength);
 Characteristic.AddByte(Properties);
 Characteristic.AddWord(Value.Handle);
 Characteristic.AddWord(Uuid);
 if (Properties and ATT_PROPERTY_READ) <> 0 then
  Value.Permissions:=Value.Permissions or AttributePermissionRead;
 if (Properties and ATT_PROPERTY_WRITE) <> 0 then
  Value.Permissions:=Value.Permissions or AttributePermissionWrite;
 if (Properties and ATT_PROPERTY_WRITE_WITHOUT_RESPONSE) <> 0 then
  Value.Permissions:=Value.Permissions or AttributePermissionWriteCommand;
end;

procedure BuildAttributes;
begin
 SetLength(AttributesByHandle,1); // skip handle number 0
 AttributesByHandle[0]:=Nil;
 AddService($1800);
 AddCharacteristic(DeviceName,$2a00,0,20,'Device Name',ATT_PROPERTY_READ);
 DeviceName.SetString('Ultibo Name');
 AddCharacteristic(Appearance,$2a01,2,2,'Appearance',ATT_PROPERTY_READ);
 Appearance.SetWord(0);
 AddService($1801);
 AddCharacteristic(ServicesChanged,$2a05,1,1,'Services Changed',ATT_PROPERTY_INDICATE);
 AddService($1802);
 AddCharacteristic(ImmediateAlertLevel,$2a06,1,1,'Alert Level',ATT_PROPERTY_WRITE_WITHOUT_RESPONSE);
 AddService($180f);
 AddCharacteristic(BatteryLevel,$2a19,1,1,'Battery Level in Percent',ATT_PROPERTY_READ or ATT_PROPERTY_NOTIFY);
 BatteryLevel.SetByte(50);
end;

initialization
SetLength(RxBuffer,0);
Queue:=MailSlotCreate(1024);
QueueEvent:=TEvent.Create(Nil,True,False,'');
QueueHandle:=SysBeginThreadEx(Nil,THREAD_STACK_DEFAULT_SIZE,@QueueHandler,Nil,0,THREAD_PRIORITY_NORMAL,CPU_AFFINITY_3,CPU_ID_3,'QueueHandler',QueueHandle);
end.
