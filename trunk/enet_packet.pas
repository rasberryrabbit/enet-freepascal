unit enet_packet;

(**
 @file  packet.c
 @brief ENet packet management functions

 freepascal

 1.3.6
 - fix enet_packet_destroy
*)

interface

uses enet_consts, enet_callbacks, enet_socket;

function enet_crc32 (buffers : pENetBuffer; bufferCount : enet_size_t):enet_uint32;
function enet_packet_create (data : pointer; dataLength : enet_size_t; flags : enet_uint32):pENetPacket;
procedure enet_packet_destroy (packet : pENetPacket);
function enet_packet_resize (packet : pENetPacket; dataLength  : enet_size_t):integer;


implementation

(** @defgroup Packet ENet packet functions
    @{
*)

(** Creates a packet that may be sent to a peer.
    @param dataContents initial contents of the packet's data; the packet's data will remain uninitialized if dataContents is NULL.
    @param dataLength   size of the data allocated for this packet
    @param flags        flags for this packet as described for the ENetPacket structure.
    @returns the packet on success, NULL on failure
*)

function enet_packet_create (data : pointer; dataLength : enet_size_t; flags : enet_uint32):pENetPacket;
var
  packet : pENetPacket;
begin
    packet  := pENetPacket (enet_malloc (sizeof (ENetPacket)));
    if packet=nil then begin
       Result:=nil; exit;
    end;

    if (flags and ENET_PACKET_FLAG_NO_ALLOCATE)<>0 then
      packet ^. data := penet_uint8 (data)
    else
    if (dataLength <= 0) then
      packet ^.data:=nil
    else
    begin
       packet ^. data := penet_uint8 (enet_malloc (dataLength));
       if (packet ^. data = nil) then
       begin
          enet_free (packet);
          result := nil; exit;
       end;

       if (data <> nil) then
         system.Move(data^, packet ^. data^, dataLength);
    end;

    packet ^. referenceCount := 0;
    packet ^. flags := flags;
    packet ^. dataLength := dataLength;
    packet ^. freeCallback := nil;

    result := packet;
end;

(** Destroys the packet and deallocates its data.
    @param packet packet to be destroyed
*)
procedure enet_packet_destroy (packet : pENetPacket);
begin
    if packet=nil then
       exit;

    if (packet ^. freeCallback <> nil) then
      packet ^. freeCallback(packet);
    if (packet ^. flags and ENET_PACKET_FLAG_NO_ALLOCATE=0) and
        (packet ^. data <> nil) then
      enet_free (packet ^. data);
    enet_free (packet);
end;

(** Attempts to resize the data in the packet to length specified in the
    dataLength parameter
    @param packet packet to resize
    @param dataLength new size for the packet data
    @returns 0 on success, < 0 on failure
*)
function enet_packet_resize (packet : pENetPacket; dataLength  : enet_size_t):integer;
var
    newData : penet_uint8;
begin

    if (dataLength <= packet ^. dataLength) or ((packet ^. flags and ENET_PACKET_FLAG_NO_ALLOCATE)<>0) then
    begin
       packet ^. dataLength := dataLength;

       result := 0; exit;
    end;

    newData := penet_uint8 (enet_malloc (dataLength));
    if (newData = nil) then
      begin result := -1; exit; end;

    system.Move(packet ^. data^, newData^, packet ^. dataLength);
    enet_free (packet ^. data);

    packet ^. data := newData;
    packet ^. dataLength := dataLength;

    result := 0;
end;

var
  initializedCRC32 : integer = 0;
  crcTable : array[0..255] of enet_uint32;

function reflect_crc (val, bits :integer):enet_uint32;
var
  bit : integer;
begin
  Result := 0;

  for bit:=0 to bits-1 do begin
    if (val and 1)<>0 then
      Result := Result or enet_uint32(1 shl (bits - 1 - bit));
    val := val shr 1;
  end;
end;

procedure initialize_crc32;
var
  ibyte : integer;
  crc : enet_uint32;
  offset : integer;
begin

    for ibyte := 0 to 255 do
    begin
        crc := reflect_crc (ibyte, 8) shl 24;

        for offset := 0 to 7 do
        begin
            if (crc and $80000000)<>0 then
                crc := (crc shl 1) xor $04c11db7
            else
                crc := crc shl 1;
        end;

        crcTable [ibyte] := reflect_crc (crc, 32);
    end;

    initializedCRC32 := 1;
end;

function enet_crc32 (buffers : pENetBuffer; bufferCount : enet_size_t):enet_uint32;
var
    crc : enet_uint32;
    data, dataEnd : penet_uint8;
begin
    crc := longword($FFFFFFFF);

    if not longbool(initializedCRC32) then initialize_crc32;

    while (bufferCount > 0) do
    begin
        data := penet_uint8 (buffers ^. data);
        dataEnd := penet_uint8 (PAnsiChar(data)+ buffers ^. dataLength);

        while PAnsiChar(data)<PAnsiChar(dataEnd) do
        begin
            crc := (crc shr 8) xor crcTable [(crc and $FF) xor data^];
            inc(data);
        end;

        inc(buffers);
        dec(bufferCount);
    end;

    result := ENET_HOST_TO_NET_32 (not crc);
end;

(** @} *)

end.

