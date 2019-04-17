unit enet_protocol;

(**
 @file  protocol.c
 @brief ENet protocol functions

 1.3.12 freepascal

 - fix enet_host_service mistranslation
 - fix enet_protocol_send_acknowledgements

 - cleanup
*)

{$GOTO ON}

interface

uses enet_consts;

procedure enet_host_flush (host : pENetHost);
function enet_host_service (host : pENetHost; event : pENetEvent; timeout : enet_uint32):integer;
function enet_protocol_check_timeouts (host : pENetHost; peer : pENetPeer; event : pENetEvent):integer;
function enet_protocol_command_size (commandNumber : enet_uint8):enet_size_t;
function enet_protocol_dispatch_incoming_commands (host : pENetHost;event : pENetEvent):integer;
function enet_protocol_handle_acknowledge (host : pENetHost; event : pENetEvent; peer : pENetPeer; command : pENetProtocol):integer;
function enet_protocol_handle_bandwidth_limit (host : pENetHost; peer : pENetPeer; command : pENetProtocol):integer;
function enet_protocol_handle_connect (host : pENetHost;header : pENetProtocolHeader;command : pENetProtocol):pENetPeer;
function enet_protocol_handle_disconnect (host : pENetHost; peer : pENetPeer; command : pENetProtocol):integer;
function enet_protocol_handle_incoming_commands (host : pENetHost; event : pENetEvent):integer;
function enet_protocol_handle_ping (host : pENetHost; peer : pENetPeer; command : pENetProtocol):integer;
function enet_protocol_handle_send_fragment (host : pENetHost; peer : pENetPeer; command : pENetProtocol; currentData : ppenet_uint8):integer;
function enet_protocol_handle_send_reliable (host : pENetHost; peer : pENetPeer; command : pENetProtocol; currentData : ppenet_uint8):integer;
function enet_protocol_handle_send_unreliable (host : pENetHost; peer : pENetPeer;command : pENetProtocol;currentData : ppenet_uint8):integer;
function enet_protocol_handle_send_unsequenced (host : pENetHost; peer : pENetPeer; command : pENetProtocol; currentData : ppenet_uint8):integer;
function enet_protocol_handle_throttle_configure (host : pENetHost; peer : pENetPeer; command : pENetProtocol):integer;
function enet_protocol_handle_verify_connect (host : pENetHost; event : pENetEvent; peer : pENetPeer; command : pENetProtocol):integer;
procedure enet_protocol_notify_connect (host : pENetHost;peer : pENetPeer;event : pENetEvent);
procedure enet_protocol_notify_disconnect (host : pENetHost;peer : pENetPeer;event : pENetEvent);
function enet_protocol_receive_incoming_commands (host : pENetHost; event : pENetEvent):integer;
function enet_protocol_remove_sent_reliable_command (peer : pENetPeer;reliableSequenceNumber : enet_uint16;channelID : enet_uint8):ENetProtocolCommand;
procedure enet_protocol_remove_sent_unreliable_commands (peer : pENetPeer);
procedure enet_protocol_send_acknowledgements (host : pENetHost; peer : pENetPeer);
function enet_protocol_send_outgoing_commands (host : pENetHost; event : pENetEvent; checkForTimeouts : integer):integer;
function enet_protocol_send_reliable_outgoing_commands (host : pENetHost; peer : pENetPeer):Integer;
procedure enet_protocol_send_unreliable_outgoing_commands (host : pENetHost;peer : pENetPeer);
function enet_host_check_events (host : pENetHost; event : pENetEvent):integer;

procedure enet_protocol_change_state (host:pENetHost; peer:pENetPeer; state: (*ENetPeerState*)enet_uint32 );
procedure enet_protocol_dispatch_state (host:pENetHost; peer:pENetPeer; state: (*ENetPeerState*)enet_uint32);

const
  commandSizes : array[0..(ENET_PROTOCOL_COMMAND_COUNT)-1] of enet_size_t =
(
    0,
    sizeof (ENetProtocolAcknowledge),
    sizeof (ENetProtocolConnect),
    sizeof (ENetProtocolVerifyConnect),
    sizeof (ENetProtocolDisconnect),
    sizeof (ENetProtocolPing),
    sizeof (ENetProtocolSendReliable),
    sizeof (ENetProtocolSendUnreliable),
    sizeof (ENetProtocolSendFragment),
    sizeof (ENetProtocolSendUnsequenced),
    sizeof (ENetProtocolBandwidthLimit),
    sizeof (ENetProtocolThrottleConfigure),
    sizeof (ENetProtocolSendFragment)
);


implementation

uses enet_packet, enet_host, enet_callbacks, enet_list, enet_socket, enet_peer;

function enet_protocol_command_size (commandNumber : enet_uint8):enet_size_t;
begin
    result := commandSizes [commandNumber and ENET_PROTOCOL_COMMAND_MASK];
end;

procedure enet_protocol_change_state (host:pENetHost; peer:pENetPeer; state: (*ENetPeerState*)enet_uint32);
begin
    if ((state = ENET_PEER_STATE_CONNECTED) or (state = ENET_PEER_STATE_DISCONNECT_LATER)) then
      enet_peer_on_connect (peer)
    else
      enet_peer_on_disconnect (peer);

    peer ^. state := state;
end;

procedure enet_protocol_dispatch_state (host:pENetHost; peer:pENetPeer; state: (*ENetPeerState*)enet_uint32);
begin
    enet_protocol_change_state (host, peer, state);

    if (peer ^. needsDispatch = 0) then
    begin
       enet_list_insert (enet_list_end (@ host ^. dispatchQueue), @ peer ^. dispatchList);

       peer ^. needsDispatch := 1;
    end;
end;

function enet_protocol_dispatch_incoming_commands (host : pENetHost;event : pENetEvent):integer;
var
    peer : pENetPeer;
begin
    while (not enet_list_empty (@ host ^. dispatchQueue)) do
    begin
       peer := pENetPeer ( enet_list_remove (enet_list_begin (@ host ^. dispatchQueue)));

       peer ^. needsDispatch := 0;

       case (peer ^. state) of
       ENET_PEER_STATE_CONNECTION_PENDING,
       ENET_PEER_STATE_CONNECTION_SUCCEEDED:
          begin
           enet_protocol_change_state (host, peer, ENET_PEER_STATE_CONNECTED);

           event ^. EventType := ENET_EVENT_TYPE_CONNECT;
           event ^. peer  := peer;
           event ^. data  := peer ^. eventData;

           Result:=1; exit;
          end;
       ENET_PEER_STATE_ZOMBIE:
          begin
           host ^. recalculateBandwidthLimits := 1;

           event ^. EventType := ENET_EVENT_TYPE_DISCONNECT;
           event ^. peer := peer;
           event ^. data := peer ^. eventData;

           enet_peer_reset (peer);

           Result:=1; exit;
          end;

       ENET_PEER_STATE_CONNECTED:
          begin
           if (enet_list_empty (@ peer ^. dispatchedCommands)) then
             continue;

           event ^. packet := enet_peer_receive (peer, @ event ^. channelID);
           if (event ^. packet = nil) then
             continue;

           event ^. EventType := ENET_EVENT_TYPE_RECEIVE;
           event ^. peer := peer;

           if (not enet_list_empty (@ peer ^. dispatchedCommands)) then
           begin
              peer ^. needsDispatch := 1;

              enet_list_insert (enet_list_end (@ host ^. dispatchQueue), @ peer ^. dispatchList);
           end;

           Result:=1; exit;
          end;
       end;
    end;

    Result:=0;
end;

procedure enet_protocol_notify_connect (host : pENetHost;peer : pENetPeer;event : pENetEvent);
var
    state : Integer;
begin
    host ^. recalculateBandwidthLimits := 1;

    if (event <> nil) then
    begin
       enet_protocol_change_state (host, peer, ENET_PEER_STATE_CONNECTED);

       event ^. EventType := ENET_EVENT_TYPE_CONNECT;
       event ^. peer := peer;
       event ^. data := peer ^. eventData;
    end
    else begin
         if peer ^. state = ENET_PEER_STATE_CONNECTING then
           state := ENET_PEER_STATE_CONNECTION_SUCCEEDED
           else
               state := ENET_PEER_STATE_CONNECTION_PENDING;
        enet_protocol_dispatch_state (host, peer, state);
    end;
end;

procedure enet_protocol_notify_disconnect (host : pENetHost;peer : pENetPeer;event : pENetEvent);
begin
    if (peer ^. state >= ENET_PEER_STATE_CONNECTION_PENDING) then
        host ^. recalculateBandwidthLimits := 1;

    if (peer ^. state <> ENET_PEER_STATE_CONNECTING) and (peer ^. state < ENET_PEER_STATE_CONNECTION_SUCCEEDED) then
        enet_peer_reset (peer)
    else
    if (event <> nil) then
    begin
        event ^. EventType := ENET_EVENT_TYPE_DISCONNECT;
        event ^. peer := peer;
        event ^. data := 0;

        enet_peer_reset (peer);
    end
    else
    begin
        peer ^. eventData := 0;

        enet_protocol_dispatch_state (host, peer, ENET_PEER_STATE_ZOMBIE);
    end;
end;

procedure enet_protocol_remove_sent_unreliable_commands (peer : pENetPeer);
var
    outgoingCommand : pENetOutgoingCommand;
begin

    while (not enet_list_empty (@ peer ^. sentUnreliableCommands)) do
    begin
        outgoingCommand := pENetOutgoingCommand(enet_list_front (@ peer ^. sentUnreliableCommands));
        
        enet_list_remove (@ outgoingCommand ^. outgoingCommandList);

        if (outgoingCommand ^. packet <> nil) then
        begin
           dec(outgoingCommand ^. packet ^. referenceCount);

           if (outgoingCommand ^. packet ^. referenceCount = 0) then
           begin
              outgoingCommand ^. packet ^. flags := outgoingCommand ^. packet ^. flags or ENET_PACKET_FLAG_SENT;

              enet_packet_destroy (outgoingCommand ^. packet);
           end;
        end;

        enet_free (outgoingCommand);
    end;
end;

function enet_protocol_remove_sent_reliable_command (peer : pENetPeer;reliableSequenceNumber : enet_uint16;channelID : enet_uint8):ENetProtocolCommand;
var
    outgoingCommand : pENetOutgoingCommand;
    currentCommand : ENetListIterator;
    commandNumber : ENetProtocolCommand;
    channel : pENetChannel;
    reliableWindow : enet_uint16;
    wasSent : Integer;
begin
    outgoingCommand:=nil;
    wasSent:=1;

    currentCommand := enet_list_begin (@ peer ^. sentReliableCommands);
    while PAnsiChar(currentCommand)<>PAnsiChar(enet_list_end (@ peer ^. sentReliableCommands)) do
    begin
       outgoingCommand := pENetOutgoingCommand (currentCommand);

       if (outgoingCommand ^. reliableSequenceNumber = reliableSequenceNumber) and
           (outgoingCommand ^. command.header.channelID = channelID) then
         break;
       currentCommand := enet_list_next (currentCommand);
    end;

    if PAnsiChar(currentCommand)=PAnsiChar(enet_list_end (@ peer ^. sentReliableCommands)) then
    begin
      currentCommand := enet_list_begin (@ peer ^. outgoingReliableCommands);
      while PAnsiChar(currentCommand)<>PAnsiChar(enet_list_end (@ peer ^. outgoingReliableCommands)) do
       begin
          outgoingCommand := pENetOutgoingCommand (currentCommand);

          if (outgoingCommand ^. sendAttempts < 1) then begin result := ENET_PROTOCOL_COMMAND_NONE; exit; end;

          if (outgoingCommand ^. reliableSequenceNumber = reliableSequenceNumber) and
              (outgoingCommand ^. command.header.channelID = channelID) then
            break;
          currentCommand := enet_list_next (currentCommand);
       end;

      if PAnsiChar(currentCommand)=PAnsiChar(enet_list_end (@ peer ^. outgoingReliableCommands)) then
       begin result := ENET_PROTOCOL_COMMAND_NONE; exit; end;

      wasSent:=0;
    end;

    if (outgoingCommand = nil) then
      begin Result:= ENET_PROTOCOL_COMMAND_NONE; exit; end;

    if (channelID < peer ^. channelCount) then
    begin
       channel := @ (pENetChannelArray(peer ^. channels)^[channelID]);
       reliableWindow := reliableSequenceNumber div ENET_PEER_RELIABLE_WINDOW_SIZE;
       if (channel ^. reliableWindows [reliableWindow] > 0) then
       begin
          dec( channel ^. reliableWindows [reliableWindow]);
          if (0 = channel ^. reliableWindows [reliableWindow]) then
            channel ^. usedReliableWindows := channel ^. usedReliableWindows and not(1 shl reliableWindow);
       end;
    end;

    commandNumber := ENetProtocolCommand(outgoingCommand ^. command.header.command and ENET_PROTOCOL_COMMAND_MASK);

    enet_list_remove (@ outgoingCommand ^. outgoingCommandList);

    if (outgoingCommand ^. packet <> nil) then
    begin
       if wasSent<>0 then
         dec(peer ^. reliableDataInTransit , outgoingCommand ^. fragmentLength);

       dec( outgoingCommand ^. packet ^. referenceCount);

       if (outgoingCommand ^. packet ^. referenceCount = 0) then
       begin
          outgoingCommand ^. packet ^. flags := outgoingCommand ^. packet ^. flags or ENET_PACKET_FLAG_SENT;

          enet_packet_destroy (outgoingCommand ^. packet);
       end;
    end;

    enet_free (outgoingCommand);

    if (enet_list_empty (@ peer ^. sentReliableCommands)) then
      begin result := commandNumber; exit; end;

    outgoingCommand := pENetOutgoingCommand (enet_list_front (@ peer ^. sentReliableCommands));

    peer ^. nextTimeout := outgoingCommand ^. sentTime + outgoingCommand ^. roundTripTimeout;

    result := commandNumber;
end;

function enet_protocol_handle_connect (host : pENetHost;header : pENetProtocolHeader;command : pENetProtocol):pENetPeer;
var
    incomingSessionID, outgoingSessionID : enet_uint8;
    mtu, windowSize : enet_uint32;
    channel : pENetChannel;
    channelCount, duplicatePeers : enet_size_t;
    currentPeer, peer : pENetPeer;
    verifyCommand : ENetProtocol;
begin
    duplicatePeers :=0;
    peer :=nil;
    channelCount := ENET_NET_TO_HOST_32 (command ^. connect.channelCount);

    if (channelCount < ENET_PROTOCOL_MINIMUM_CHANNEL_COUNT) or
        (channelCount > ENET_PROTOCOL_MAXIMUM_CHANNEL_COUNT) then
      begin result := nil; exit; end;

    currentPeer := host ^. peers;
    while PAnsiChar(currentPeer)< PAnsiChar(@(pENetPeerArray(host ^. peers)^[host ^. peerCount])) do
    begin
       if (currentPeer ^. state = ENET_PEER_STATE_DISCONNECTED) then
       begin
           if (peer = nil) then
             peer := currentPeer;
       end
       else
       if ((currentPeer ^. state <> ENET_PEER_STATE_CONNECTING) and
           (currentPeer ^. address.host = host ^. receivedAddress.host)) then
       begin
           if ((currentPeer ^. address.port = host ^. receivedAddress.port) and
               (currentPeer ^. connectID = command ^. connect.connectID)) then
             begin Result:=nil; exit; end;

           Inc(duplicatePeers);
       end;
       inc(currentPeer);
    end;

    if ((peer = nil) or (duplicatePeers >= host ^. duplicatePeers)) then
      begin result := nil; exit; end;

    if (channelCount > host ^. channelLimit) then
      channelCount := host ^. channelLimit;
    peer ^. channels := pENetChannel (enet_malloc (channelCount * sizeof (ENetChannel)));
    if (peer ^. channels = nil) then begin
      Result:=nil; exit;
    end;
    peer ^. channelCount := channelCount;
    peer ^. state := ENET_PEER_STATE_ACKNOWLEDGING_CONNECT;
    peer ^. connectID := command ^. connect.connectID;
    peer ^. address := host ^. receivedAddress;
    peer ^. outgoingPeerID := ENET_NET_TO_HOST_16 (command ^. connect.outgoingPeerID);
    peer ^. incomingBandwidth := ENET_NET_TO_HOST_32 (command ^. connect.incomingBandwidth);
    peer ^. outgoingBandwidth := ENET_NET_TO_HOST_32 (command ^. connect.outgoingBandwidth);
    peer ^. packetThrottleInterval := ENET_NET_TO_HOST_32 (command ^. connect.packetThrottleInterval);
    peer ^. packetThrottleAcceleration := ENET_NET_TO_HOST_32 (command ^. connect.packetThrottleAcceleration);
    peer ^. packetThrottleDeceleration := ENET_NET_TO_HOST_32 (command ^. connect.packetThrottleDeceleration);
    peer ^. eventData := ENET_NET_TO_HOST_32 (command ^. connect.data);

    if command ^. connect.incomingSessionID=$0ff then
      incomingSessionID:= Peer ^. outgoingSessionID
      else
          incomingSessionID:=command ^. connect.incomingSessionID;
    incomingSessionID := (incomingSessionID + 1) and  (ENET_PROTOCOL_HEADER_SESSION_MASK shr ENET_PROTOCOL_HEADER_SESSION_SHIFT);
    if (incomingSessionID = Peer ^. outgoingSessionID) then
      incomingSessionID := (incomingSessionID + 1) and (ENET_PROTOCOL_HEADER_SESSION_MASK shr ENET_PROTOCOL_HEADER_SESSION_SHIFT);
    Peer ^. outgoingSessionID := incomingSessionID;

    if command ^. connect.outgoingSessionID=$0ff then
      outgoingSessionID:=Peer ^. incomingSessionID
      else
          outgoingSessionID:=command ^. connect.outgoingSessionID;
    outgoingSessionID := (outgoingSessionID + 1) and (ENET_PROTOCOL_HEADER_SESSION_MASK shr ENET_PROTOCOL_HEADER_SESSION_SHIFT);
    if (outgoingSessionID = Peer ^. incomingSessionID) then
      outgoingSessionID := (outgoingSessionID + 1) and (ENET_PROTOCOL_HEADER_SESSION_MASK shr ENET_PROTOCOL_HEADER_SESSION_SHIFT);
    Peer ^. incomingSessionID := outgoingSessionID;

    channel := Peer ^. channels;
    while PAnsiChar(channel)< PAnsiChar(@(pENetChannelArray(Peer ^. channels)^[channelCount])) do
    begin
        channel ^. outgoingReliableSequenceNumber := 0;
        channel ^. outgoingUnreliableSequenceNumber := 0;
        channel ^. incomingReliableSequenceNumber := 0;
        channel ^. incomingUnreliableSequenceNumber := 0;

        enet_list_clear (@ channel ^. incomingReliableCommands);
        enet_list_clear (@ channel ^. incomingUnreliableCommands);

        channel ^. usedReliableWindows := 0;
        FillChar(channel ^. reliableWindows,  sizeof (channel ^. reliableWindows), 0);

        inc(channel);
    end;

    mtu := ENET_NET_TO_HOST_32 (command ^. connect.mtu);

    if (mtu < ENET_PROTOCOL_MINIMUM_MTU) then
      mtu := ENET_PROTOCOL_MINIMUM_MTU
    else
    if (mtu > ENET_PROTOCOL_MAXIMUM_MTU) then
      mtu := ENET_PROTOCOL_MAXIMUM_MTU;

    Peer ^. mtu := mtu;

    if (host ^. outgoingBandwidth = 0) and
        (Peer ^. incomingBandwidth = 0) then
      Peer ^. windowSize := ENET_PROTOCOL_MAXIMUM_WINDOW_SIZE
    else
    if (host ^. outgoingBandwidth = 0) or
        (Peer ^. incomingBandwidth = 0) then
      Peer ^. windowSize := (ENET_MAX (host ^. outgoingBandwidth, Peer ^. incomingBandwidth) div
                                    ENET_PEER_WINDOW_SIZE_SCALE) *
                                      ENET_PROTOCOL_MINIMUM_WINDOW_SIZE
    else
      Peer ^. windowSize := (ENET_MIN (host ^. outgoingBandwidth, Peer ^. incomingBandwidth) div
                                    ENET_PEER_WINDOW_SIZE_SCALE) *
                                      ENET_PROTOCOL_MINIMUM_WINDOW_SIZE;

    if (Peer ^. windowSize < ENET_PROTOCOL_MINIMUM_WINDOW_SIZE) then
      Peer ^. windowSize := ENET_PROTOCOL_MINIMUM_WINDOW_SIZE
    else
    if (Peer ^. windowSize > ENET_PROTOCOL_MAXIMUM_WINDOW_SIZE) then
      Peer ^. windowSize := ENET_PROTOCOL_MAXIMUM_WINDOW_SIZE;

    if (host ^. incomingBandwidth = 0) then
      windowSize := ENET_PROTOCOL_MAXIMUM_WINDOW_SIZE
    else
      windowSize := (host ^. incomingBandwidth div ENET_PEER_WINDOW_SIZE_SCALE) *
                     ENET_PROTOCOL_MINIMUM_WINDOW_SIZE;

    if (windowSize > ENET_NET_TO_HOST_32 (command ^. connect.windowSize)) then
      windowSize := ENET_NET_TO_HOST_32 (command ^. connect.windowSize);

    if (windowSize < ENET_PROTOCOL_MINIMUM_WINDOW_SIZE) then
      windowSize := ENET_PROTOCOL_MINIMUM_WINDOW_SIZE
    else
    if (windowSize > ENET_PROTOCOL_MAXIMUM_WINDOW_SIZE) then
      windowSize := ENET_PROTOCOL_MAXIMUM_WINDOW_SIZE;

    verifyCommand.header.command := ENET_PROTOCOL_COMMAND_VERIFY_CONNECT or ENET_PROTOCOL_COMMAND_FLAG_ACKNOWLEDGE;
    verifyCommand.header.channelID := $FF;
    verifyCommand.verifyConnect.outgoingPeerID := ENET_HOST_TO_NET_16 (Peer ^. incomingPeerID);
    verifyCommand.verifyConnect.incomingSessionID := incomingSessionID;
    verifyCommand.verifyConnect.outgoingSessionID := outgoingSessionID;
    verifyCommand.verifyConnect.mtu := ENET_HOST_TO_NET_32 (Peer ^. mtu);
    verifyCommand.verifyConnect.windowSize := ENET_HOST_TO_NET_32 (windowSize);
    verifyCommand.verifyConnect.channelCount := ENET_HOST_TO_NET_32 (channelCount);
    verifyCommand.verifyConnect.incomingBandwidth := ENET_HOST_TO_NET_32 (host ^. incomingBandwidth);
    verifyCommand.verifyConnect.outgoingBandwidth := ENET_HOST_TO_NET_32 (host ^. outgoingBandwidth);
    verifyCommand.verifyConnect.packetThrottleInterval := ENET_HOST_TO_NET_32 (Peer ^. packetThrottleInterval);
    verifyCommand.verifyConnect.packetThrottleAcceleration := ENET_HOST_TO_NET_32 (Peer ^. packetThrottleAcceleration);
    verifyCommand.verifyConnect.packetThrottleDeceleration := ENET_HOST_TO_NET_32 (Peer ^. packetThrottleDeceleration);
    verifyCommand.verifyConnect.connectID := Peer ^. connectID;

    enet_peer_queue_outgoing_command (Peer, @ verifyCommand, nil, 0, 0);

    result := Peer;
end;

function enet_protocol_handle_send_reliable (host : pENetHost; peer : pENetPeer; command : pENetProtocol; currentData : ppenet_uint8):integer;
var
    dataLength : enet_size_t;
begin

    if (command ^. header.channelID >= peer ^. channelCount) or
        ((peer ^. state <> ENET_PEER_STATE_CONNECTED) and (peer ^. state <> ENET_PEER_STATE_DISCONNECT_LATER)) then
      begin result := -1; exit; end;

    dataLength := ENET_NET_TO_HOST_16 (command ^. sendReliable.dataLength);
    Inc(PAnsiChar(currentData^) , dataLength);
    if (dataLength > host ^. maximumPacketSize) or
       (currentData^ < host ^. receivedData) or
       (currentData^ > @ penet_uint8array(host ^. receivedData)[host ^. receivedDataLength]) then
       begin result := -1; exit; end;

    if (enet_peer_queue_incoming_command (peer, command, pchar(command) + sizeof (ENetProtocolSendReliable), dataLength, ENET_PACKET_FLAG_RELIABLE, 0) = nil) then
      begin result := -1; exit; end;

    result := 0;
end;

function enet_protocol_handle_send_unsequenced (host : pENetHost; peer : pENetPeer; command : pENetProtocol; currentData : ppenet_uint8):integer;
var
    unsequencedGroup, index : enet_uint32;
    dataLength : enet_size_t;
begin

    if (command ^. header.channelID >= peer ^. channelCount) or
        ((peer ^. state <> ENET_PEER_STATE_CONNECTED) and (peer ^. state <> ENET_PEER_STATE_DISCONNECT_LATER)) then
      begin result := -1; exit; end;

    dataLength := ENET_NET_TO_HOST_16 (command ^. sendUnsequenced.dataLength);
    inc(PAnsiChar(currentData^) , dataLength);
    if (dataLength > host ^. maximumPacketSize) or
       (currentData^ < host ^. receivedData) or
       (currentData^ > @ penet_uint8array(host ^. receivedData)[host ^. receivedDataLength]) then
      begin result := -1; exit; end;

    unsequencedGroup := ENET_NET_TO_HOST_16 (command ^. sendUnsequenced.unsequencedGroup);
    index := unsequencedGroup mod ENET_PEER_UNSEQUENCED_WINDOW_SIZE;

    if (unsequencedGroup < peer ^. incomingUnsequencedGroup) then
      inc(unsequencedGroup , $10000);

    if (unsequencedGroup >= enet_uint32(peer ^. incomingUnsequencedGroup + ENET_PEER_FREE_UNSEQUENCED_WINDOWS * ENET_PEER_UNSEQUENCED_WINDOW_SIZE)) then
      begin result := 0; exit; end;

    unsequencedGroup := unsequencedGroup and $FFFF;

    if ((unsequencedGroup - index) <> peer ^. incomingUnsequencedGroup) then
    begin
        peer ^. incomingUnsequencedGroup := unsequencedGroup - index;

        FillChar (peer ^. unsequencedWindow, sizeof (peer ^. unsequencedWindow), 0);
    end
    else
    if 0<>(peer ^. unsequencedWindow [index div 32] and (1 shl (index mod 32))) then
      begin result := 0; exit; end;

    if (enet_peer_queue_incoming_command (peer, command, pchar(command) + sizeof (ENetProtocolSendUnsequenced), dataLength, ENET_PACKET_FLAG_UNSEQUENCED, 0) = nil) then
      begin result := -1; exit; end;

    peer ^. unsequencedWindow [index div 32] := peer ^. unsequencedWindow [index div 32] or enet_uint32(1 shl (index mod 32));

    result := 0;
end;

function enet_protocol_handle_send_unreliable (host : pENetHost; peer : pENetPeer;command : pENetProtocol;currentData : ppenet_uint8):integer;
var
    dataLength : enet_size_t;
begin

    if (command ^. header.channelID >= peer ^. channelCount) or
        ((peer ^. state <> ENET_PEER_STATE_CONNECTED) and (peer ^. state <> ENET_PEER_STATE_DISCONNECT_LATER)) then
      begin result := -1; exit; end;

    dataLength := ENET_NET_TO_HOST_16 (command ^. sendUnreliable.dataLength);
    inc(PAnsiChar(currentData^), dataLength);
    if (dataLength > host ^. maximumPacketSize) or
       (currentData^ < host ^. receivedData) or
       (currentData^ > @ penet_uint8array(host ^. receivedData)[host ^. receivedDataLength]) then
      begin result := -1; exit; end;

    if (enet_peer_queue_incoming_command (peer, command, pchar(command) + sizeof (ENetProtocolSendUnreliable), dataLength, 0, 0) = nil) then
      begin result:=-1; exit; end;

    result := 0;
end;

function enet_protocol_handle_send_fragment (host : pENetHost; peer : pENetPeer; command : pENetProtocol; currentData : ppenet_uint8):integer;
var
    fragmentNumber , fragmentCount, fragmentOffset, fragmentLength,
    startSequenceNumber, totalLength : enet_uint32;
    channel : pENetChannel;
    startWindow, currentWindow : enet_uint16;
    currentCommand : ENetListIterator;
    startCommand : pENetIncomingCommand;
    packet : pENetPacket;
    hostCommand : ENetProtocol;
    incomingCommand : pENetIncomingCommand;
label continuework1;
begin
    startCommand := nil;

    if (command ^. header.channelID >= peer ^. channelCount) or
        ((peer ^. state <> ENET_PEER_STATE_CONNECTED) and (peer ^. state <> ENET_PEER_STATE_DISCONNECT_LATER)) then
      begin result := -1; exit; end;

    fragmentLength := ENET_NET_TO_HOST_16 (command ^. sendFragment.dataLength);
    inc( currentData^ , fragmentLength);
    if (fragmentLength > host ^. maximumPacketSize) or
       (currentData^ < host ^. receivedData) or
       (currentData^ > @ penet_uint8array(host ^. receivedData)[host ^. receivedDataLength]) then
      begin result := -1; exit; end;

    channel := @ pENetChannelArray(peer ^. channels)^[command ^. header.channelID];
    startSequenceNumber := ENET_NET_TO_HOST_16 (command ^. sendFragment.startSequenceNumber);
    startWindow := startSequenceNumber div ENET_PEER_RELIABLE_WINDOW_SIZE;
    currentWindow := channel ^. incomingReliableSequenceNumber div ENET_PEER_RELIABLE_WINDOW_SIZE;

    if (startSequenceNumber < channel ^. incomingReliableSequenceNumber) then
      inc(startWindow , ENET_PEER_RELIABLE_WINDOWS);

    if (startWindow < currentWindow) or (startWindow >= (currentWindow + ENET_PEER_FREE_RELIABLE_WINDOWS - 1)) then
      begin result := 0; exit; end;

    fragmentNumber := ENET_NET_TO_HOST_32 (command ^. sendFragment.fragmentNumber);
    fragmentCount := ENET_NET_TO_HOST_32 (command ^. sendFragment.fragmentCount);
    fragmentOffset := ENET_NET_TO_HOST_32 (command ^. sendFragment.fragmentOffset);
    totalLength := ENET_NET_TO_HOST_32 (command ^. sendFragment.totalLength);

    if (fragmentCount > ENET_PROTOCOL_MAXIMUM_FRAGMENT_COUNT) or
       (fragmentNumber >= fragmentCount) or
       (totalLength > host ^. maximumPacketSize) or
       (fragmentOffset >= totalLength) or
       (fragmentLength > totalLength - fragmentOffset) then
      begin result := -1; exit; end;

    currentCommand := enet_list_previous (enet_list_end (@channel ^. incomingReliableCommands));
    while PAnsiChar(currentCommand) <> PAnsiChar(enet_list_end (@ channel ^. incomingReliableCommands)) do
    begin
       incomingCommand := pENetIncomingCommand(currentCommand);

       if (startSequenceNumber >= channel ^. incomingReliableSequenceNumber) then
       begin
          if (incomingCommand ^. reliableSequenceNumber < channel ^. incomingReliableSequenceNumber) then
            goto continuework1;
       end
       else
       if (incomingCommand ^. reliableSequenceNumber >= channel ^. incomingReliableSequenceNumber) then
         break;

       if (incomingCommand ^. reliableSequenceNumber <= startSequenceNumber) then
       begin
          if (incomingCommand ^. reliableSequenceNumber < startSequenceNumber) then
            break;

          if ((incomingCommand ^. command.header.command and ENET_PROTOCOL_COMMAND_MASK) <> ENET_PROTOCOL_COMMAND_SEND_FRAGMENT) or
              (totalLength <> incomingCommand ^. packet ^. dataLength) or
              (fragmentCount <> incomingCommand ^. fragmentCount) then
            begin result := -1; exit; end;

          startCommand := incomingCommand;
          break;
       end;
continuework1:
       currentCommand := enet_list_previous (currentCommand);
    end;

    if (startCommand = nil) then
    begin
       hostCommand := command^;

       hostCommand.header.reliableSequenceNumber := startSequenceNumber;

       startCommand := enet_peer_queue_incoming_command (peer, @ hostCommand, nil, totalLength, ENET_PACKET_FLAG_RELIABLE, fragmentCount);
       if (startCommand = nil) then
         begin result := -1; exit; end;
    end;

    if enet_uint32(pIntegerArray(startCommand ^. fragments)^[fragmentNumber div 32]) and (1 shl (fragmentNumber mod 32)) = 0 then
    begin
       dec(startCommand ^. fragmentsRemaining);

       PIntegerArray(startCommand ^. fragments)^[fragmentNumber div 32] := PIntegerArray(startCommand ^. fragments)^[fragmentNumber div 32] or (1 shl (fragmentNumber mod 32));

       if (fragmentOffset + fragmentLength > startCommand ^. packet ^. dataLength) then
         fragmentLength := startCommand ^. packet ^. dataLength - fragmentOffset;

       system.Move ((PAnsiChar(command) + sizeof (ENetProtocolSendFragment))^,
             (PAnsiChar(startCommand ^. packet ^. data) + fragmentOffset)^,
               fragmentLength);

       if (startCommand ^. fragmentsRemaining <= 0) then
         enet_peer_dispatch_incoming_reliable_commands (peer, channel);
     end;

    result := 0;
end;

function enet_protocol_handle_send_unreliable_fragment (host:pENetHost; peer:pENetPeer; command : pENetProtocol; currentData : ppenet_uint8):Integer;
var
    fragmentNumber,
    fragmentCount,
    fragmentOffset,
    fragmentLength,
    reliableSequenceNumber,
    startSequenceNumber,
    totalLength : enet_uint32;
    reliableWindow, currentWindow : enet_uint16;
    channel : pENetChannel;
    currentCommand : ENetListIterator;
    startCommand : pENetIncomingCommand;
    incomingCommand : pENetIncomingCommand;
    packet : pENetPacket;
label continue1;
begin
    startCommand := nil;

    if (command ^. header.channelID >= peer ^. channelCount) or
       ((peer ^. state <> ENET_PEER_STATE_CONNECTED) and (peer ^. state <> ENET_PEER_STATE_DISCONNECT_LATER))
       then begin
         Result:=-1; exit;
       end;

    fragmentLength := ENET_NET_TO_HOST_16 (command ^. sendFragment.dataLength);
    Inc(currentData^, fragmentLength);
    if (fragmentLength > host ^. maximumPacketSize) or
       (currentData^ < host ^. receivedData) or
       (currentData^ > @ penet_uint8array(host ^. receivedData )[host ^. receivedDataLength]) then
       begin
         Result:=-1; exit;
       end;

    channel := @ peer ^. channels [command ^. header.channelID];
    reliableSequenceNumber := command ^. header.reliableSequenceNumber;
    startSequenceNumber := ENET_NET_TO_HOST_16 (command ^. sendFragment.startSequenceNumber);

    reliableWindow := reliableSequenceNumber div ENET_PEER_RELIABLE_WINDOW_SIZE;
    currentWindow := channel ^. incomingReliableSequenceNumber div ENET_PEER_RELIABLE_WINDOW_SIZE;

    if (reliableSequenceNumber < channel ^. incomingReliableSequenceNumber) then
      Inc(reliableWindow , ENET_PEER_RELIABLE_WINDOWS);

    if (reliableWindow < currentWindow) or (reliableWindow >= currentWindow + ENET_PEER_FREE_RELIABLE_WINDOWS - 1) then
      begin
           Result:=0; exit;
      end;

    if (reliableSequenceNumber = channel ^. incomingReliableSequenceNumber) and
       (startSequenceNumber <= channel ^. incomingUnreliableSequenceNumber) then
          begin
            Result:=0; exit;
          end;

    fragmentNumber := ENET_NET_TO_HOST_32 (command ^. sendFragment.fragmentNumber);
    fragmentCount := ENET_NET_TO_HOST_32 (command ^. sendFragment.fragmentCount);
    fragmentOffset := ENET_NET_TO_HOST_32 (command ^. sendFragment.fragmentOffset);
    totalLength := ENET_NET_TO_HOST_32 (command ^. sendFragment.totalLength);

    if (fragmentCount > ENET_PROTOCOL_MAXIMUM_FRAGMENT_COUNT) or
       (fragmentNumber >= fragmentCount) or
       (totalLength > host ^. maximumPacketSize) or
       (fragmentOffset >= totalLength) or
       (fragmentLength > totalLength - fragmentOffset) then
         begin
           Result:=-1; exit;
         end;


    currentCommand := enet_list_previous (enet_list_end (@ channel ^. incomingUnreliableCommands));
    while (currentCommand <> enet_list_end (@ channel ^. incomingUnreliableCommands)) do
    begin
       incomingCommand := pENetIncomingCommand ( currentCommand );

       if (reliableSequenceNumber >= channel ^. incomingReliableSequenceNumber) then
       begin
          if (incomingCommand ^. reliableSequenceNumber < channel ^. incomingReliableSequenceNumber) then
            goto continue1;
       end
       else
       if (incomingCommand ^. reliableSequenceNumber >= channel ^. incomingReliableSequenceNumber) then
         break;

       if (incomingCommand ^. reliableSequenceNumber < reliableSequenceNumber) then
         break;

       if (incomingCommand ^. reliableSequenceNumber > reliableSequenceNumber) then
         goto continue1;

       if (incomingCommand ^. unreliableSequenceNumber <= startSequenceNumber) then
       begin
          if (incomingCommand ^. unreliableSequenceNumber < startSequenceNumber) then
            break;

          if ((incomingCommand ^. command.header.command and ENET_PROTOCOL_COMMAND_MASK) <> ENET_PROTOCOL_COMMAND_SEND_UNRELIABLE_FRAGMENT) or
             (totalLength <> incomingCommand ^. packet ^. dataLength) or
             (fragmentCount <> incomingCommand ^. fragmentCount) then
             begin
               Result:=-1; exit;
             end;

          startCommand := incomingCommand;
          break;
       end;
continue1:
          currentCommand := enet_list_previous (currentCommand);
    end;

    if (startCommand = nil) then
    begin
       startCommand := enet_peer_queue_incoming_command (peer, command, nil, totalLength, ENET_PACKET_FLAG_UNRELIABLE_FRAGMENT, fragmentCount);
       if (startCommand = nil) then
         begin
           Result:=-1; exit;
         end;
    end;

    if ((PIntegerArray(startCommand ^. fragments)^ [fragmentNumber div 32] and (1 shl (fragmentNumber mod 32))) = 0) then
    begin
       Dec( startCommand ^. fragmentsRemaining);

       PIntegerArray(startCommand ^. fragments)^[fragmentNumber div 32] := PIntegerArray(startCommand ^. fragments)^[fragmentNumber div 32] or (1 shl (fragmentNumber mod 32));

       if (fragmentOffset + fragmentLength > startCommand ^. packet ^. dataLength) then
         fragmentLength := startCommand ^. packet ^. dataLength - fragmentOffset;

       system.Move((PAnsiChar(command) + sizeof (ENetProtocolSendFragment))^,
                   (PAnsiChar(startCommand ^. packet ^. data) + fragmentOffset)^,
                    fragmentLength);

        if (startCommand ^. fragmentsRemaining <= 0) then
          enet_peer_dispatch_incoming_unreliable_commands (peer, channel);
    end;

    Result:=0;
end;

function enet_protocol_handle_ping (host : pENetHost; peer : pENetPeer; command : pENetProtocol):integer;
begin
    if (peer ^. state <> ENET_PEER_STATE_CONNECTED) and (peer ^. state <> ENET_PEER_STATE_DISCONNECT_LATER) then
       begin
         Result:=-1; exit;
       end;

    result := 0;
end;

function enet_protocol_handle_bandwidth_limit (host : pENetHost; peer : pENetPeer; command : pENetProtocol):integer;
begin
    if (peer ^. state <> ENET_PEER_STATE_CONNECTED) and (peer ^. state <> ENET_PEER_STATE_DISCONNECT_LATER) then
      begin
        Result:=-1; exit;
      end;

    if (peer ^. incomingBandwidth <> 0) then
      Dec(host ^. bandwidthLimitedPeers);

    peer ^. incomingBandwidth := ENET_NET_TO_HOST_32 (command ^. bandwidthLimit.incomingBandwidth);
    peer ^. outgoingBandwidth := ENET_NET_TO_HOST_32 (command ^. bandwidthLimit.outgoingBandwidth);

    if (peer ^. incomingBandwidth <> 0) then
      Inc(host ^. bandwidthLimitedPeers);

    if (peer ^. incomingBandwidth = 0) and (host ^. outgoingBandwidth = 0) then
      peer ^. windowSize := ENET_PROTOCOL_MAXIMUM_WINDOW_SIZE
    else
      peer ^. windowSize := (ENET_MIN (peer ^. incomingBandwidth, host ^. outgoingBandwidth) div
                             ENET_PEER_WINDOW_SIZE_SCALE) * ENET_PROTOCOL_MINIMUM_WINDOW_SIZE;

    if (peer ^. windowSize < ENET_PROTOCOL_MINIMUM_WINDOW_SIZE) then
      peer ^. windowSize := ENET_PROTOCOL_MINIMUM_WINDOW_SIZE
    else
    if (peer ^. windowSize > ENET_PROTOCOL_MAXIMUM_WINDOW_SIZE) then
      peer ^. windowSize := ENET_PROTOCOL_MAXIMUM_WINDOW_SIZE;

    result := 0;
end;

function enet_protocol_handle_throttle_configure (host : pENetHost; peer : pENetPeer; command : pENetProtocol):integer;
begin
    if (peer ^. state <> ENET_PEER_STATE_CONNECTED) and (peer ^. state <> ENET_PEER_STATE_DISCONNECT_LATER) then
      begin
        Result:=-1; exit;
      end;

    peer ^. packetThrottleInterval := ENET_NET_TO_HOST_32 (command ^. throttleConfigure.packetThrottleInterval);
    peer ^. packetThrottleAcceleration := ENET_NET_TO_HOST_32 (command ^. throttleConfigure.packetThrottleAcceleration);
    peer ^. packetThrottleDeceleration := ENET_NET_TO_HOST_32 (command ^. throttleConfigure.packetThrottleDeceleration);

    result := 0;
end;

function enet_protocol_handle_disconnect (host : pENetHost; peer : pENetPeer; command : pENetProtocol):integer;
begin
    if (peer ^. state = ENET_PEER_STATE_DISCONNECTED) or (peer ^. state = ENET_PEER_STATE_ZOMBIE) or (peer ^. state = ENET_PEER_STATE_ACKNOWLEDGING_DISCONNECT) then
      begin
        Result:=0; exit;
      end;

    enet_peer_reset_queues (peer);

    if ((peer ^. state = ENET_PEER_STATE_CONNECTION_SUCCEEDED) or (peer ^. state = ENET_PEER_STATE_DISCONNECTING) or (peer ^. state = ENET_PEER_STATE_CONNECTING)) then
        enet_protocol_dispatch_state (host, peer, ENET_PEER_STATE_ZOMBIE)
    else
    if (peer ^. state <> ENET_PEER_STATE_CONNECTED) and (peer ^. state <> ENET_PEER_STATE_DISCONNECT_LATER) then
    begin
        if (peer ^. state = ENET_PEER_STATE_CONNECTION_PENDING) then host ^. recalculateBandwidthLimits := 1;

        enet_peer_reset (peer);
    end
    else
    if (command ^. header.command and ENET_PROTOCOL_COMMAND_FLAG_ACKNOWLEDGE)<>0 then
      enet_protocol_change_state (host, peer, ENET_PEER_STATE_ACKNOWLEDGING_DISCONNECT)
    else
      enet_protocol_dispatch_state (host, peer, ENET_PEER_STATE_ZOMBIE);

    if (peer ^. state <> ENET_PEER_STATE_DISCONNECTED) then
      peer ^. eventData := ENET_NET_TO_HOST_32 (command ^. disconnect.data);

    result := 0;
end;

function enet_protocol_handle_acknowledge (host : pENetHost; event : pENetEvent; peer : pENetPeer; command : pENetProtocol):integer;
var
    roundTripTime, receivedSentTime, receivedReliableSequenceNumber : enet_uint32;
    commandNumber : ENetProtocolCommand;
begin
    if (peer ^. state = ENET_PEER_STATE_DISCONNECTED) or (peer ^. state = ENET_PEER_STATE_ZOMBIE) then
       begin
         Result:=0; exit;
       end;

    receivedSentTime := ENET_NET_TO_HOST_16 (command ^. acknowledge.receivedSentTime);
    receivedSentTime := receivedSentTime or (host ^. serviceTime and $FFFF0000);
    if ((receivedSentTime and $8000) > (host ^. serviceTime and $8000)) then
        dec(receivedSentTime , $10000);

    if (ENET_TIME_LESS (host ^. serviceTime, receivedSentTime)) then
      begin result := 0; exit; end;

    peer ^. lastReceiveTime := host ^. serviceTime;
    peer ^. earliestTimeout := 0;

    roundTripTime := ENET_TIME_DIFFERENCE (host ^. serviceTime, receivedSentTime);

    enet_peer_throttle (peer, roundTripTime);

    dec(peer ^. roundTripTimeVariance , peer ^. roundTripTimeVariance div 4);

    if (roundTripTime >= peer ^. roundTripTime) then
    begin
       inc(peer ^. roundTripTime , (roundTripTime - peer ^. roundTripTime) div 8);
       inc(peer ^. roundTripTimeVariance , (roundTripTime - peer ^. roundTripTime) div 4);
    end
    else
    begin
       dec(peer ^. roundTripTime , (peer ^. roundTripTime - roundTripTime) div 8);
       inc(peer ^. roundTripTimeVariance , (peer ^. roundTripTime - roundTripTime) div 4);
    end;

    if (peer ^. roundTripTime < peer ^. lowestRoundTripTime) then
      peer ^. lowestRoundTripTime := peer ^. roundTripTime;

    if (peer ^. roundTripTimeVariance > peer ^. highestRoundTripTimeVariance) then
      peer ^. highestRoundTripTimeVariance := peer ^. roundTripTimeVariance;

    if (peer ^. packetThrottleEpoch = 0) or
        (ENET_TIME_DIFFERENCE (host ^. serviceTime, peer ^. packetThrottleEpoch) >= peer ^. packetThrottleInterval) then
    begin
        peer ^. lastRoundTripTime := peer ^. lowestRoundTripTime;
        peer ^. lastRoundTripTimeVariance := peer ^. highestRoundTripTimeVariance;
        peer ^. lowestRoundTripTime := peer ^. roundTripTime;
        peer ^. highestRoundTripTimeVariance := peer ^. roundTripTimeVariance;
        peer ^. packetThrottleEpoch := host ^. serviceTime;
    end;

    receivedReliableSequenceNumber := ENET_NET_TO_HOST_16 (command ^. acknowledge.receivedReliableSequenceNumber);

    commandNumber := enet_protocol_remove_sent_reliable_command (peer, receivedReliableSequenceNumber, command ^. header.channelID);

    case peer ^. state of
    ENET_PEER_STATE_ACKNOWLEDGING_CONNECT: begin
       if (commandNumber <> ENET_PROTOCOL_COMMAND_VERIFY_CONNECT) then
         begin result := -1; exit; end;

       enet_protocol_notify_connect (host, peer, event);
    end; // break;

    ENET_PEER_STATE_DISCONNECTING: begin
       if (commandNumber <> ENET_PROTOCOL_COMMAND_DISCONNECT) then
         begin result := -1; exit; end;

       enet_protocol_notify_disconnect (host, peer, event);
    end; // break;

    ENET_PEER_STATE_DISCONNECT_LATER: begin
       if (enet_list_empty (@ peer ^. outgoingReliableCommands) and
           enet_list_empty (@ peer ^. outgoingUnreliableCommands) and
           enet_list_empty (@ peer ^. sentReliableCommands)) then
         enet_peer_disconnect (peer, peer ^. eventData);
    end; // break

    else ;
    end;

    result := 0;
end;

function enet_protocol_handle_verify_connect (host : pENetHost; event : pENetEvent; peer : pENetPeer; command : pENetProtocol):integer;
var
    mtu, windowSize : enet_uint32;
    channelCount : enet_size_t;
begin

    if (peer ^. state <> ENET_PEER_STATE_CONNECTING) then
      begin result := 0; exit; end;

    channelCount := ENET_NET_TO_HOST_32 (command ^. verifyConnect.channelCount);

    if (channelCount < ENET_PROTOCOL_MINIMUM_CHANNEL_COUNT) or (channelCount > ENET_PROTOCOL_MAXIMUM_CHANNEL_COUNT) or
       (ENET_NET_TO_HOST_32 (command ^. verifyConnect.packetThrottleInterval) <> peer ^. packetThrottleInterval) or
       (ENET_NET_TO_HOST_32 (command ^. verifyConnect.packetThrottleAcceleration) <> peer ^. packetThrottleAcceleration) or
       (ENET_NET_TO_HOST_32 (command ^. verifyConnect.packetThrottleDeceleration) <> peer ^. packetThrottleDeceleration) or
       (command ^. verifyConnect.connectID <> peer ^. connectID) then
    begin
      peer ^. eventData := 0;
      enet_protocol_dispatch_state (host, peer, ENET_PEER_STATE_ZOMBIE);

      result := -1; exit;
    end;

    enet_protocol_remove_sent_reliable_command (peer, 1, $FF);

    if (channelCount < peer ^. channelCount) then
      peer ^. channelCount := channelCount;

    peer ^. outgoingPeerID := ENET_NET_TO_HOST_16 (command ^. verifyConnect.outgoingPeerID);
    peer ^. incomingSessionID := command ^. verifyConnect.incomingSessionID;
    peer ^. outgoingSessionID := command ^. verifyConnect.outgoingSessionID;

    mtu := ENET_NET_TO_HOST_32 (command ^. verifyConnect.mtu);

    if (mtu < ENET_PROTOCOL_MINIMUM_MTU) then
      mtu := ENET_PROTOCOL_MINIMUM_MTU
    else
    if (mtu > ENET_PROTOCOL_MAXIMUM_MTU) then
      mtu := ENET_PROTOCOL_MAXIMUM_MTU;

    if (mtu < peer ^. mtu) then
      peer ^. mtu := mtu;

    windowSize := ENET_NET_TO_HOST_32 (command ^. verifyConnect.windowSize);

    if (windowSize < ENET_PROTOCOL_MINIMUM_WINDOW_SIZE) then
      windowSize := ENET_PROTOCOL_MINIMUM_WINDOW_SIZE;

    if (windowSize > ENET_PROTOCOL_MAXIMUM_WINDOW_SIZE) then
      windowSize := ENET_PROTOCOL_MAXIMUM_WINDOW_SIZE;

    if (windowSize < peer ^. windowSize) then
      peer ^. windowSize := windowSize;

    peer ^. incomingBandwidth := ENET_NET_TO_HOST_32 (command ^. verifyConnect.incomingBandwidth);
    peer ^. outgoingBandwidth := ENET_NET_TO_HOST_32 (command ^. verifyConnect.outgoingBandwidth);

    enet_protocol_notify_connect (host, peer, event);
    result := 0;
end;

function enet_protocol_handle_incoming_commands (host : pENetHost; event : pENetEvent):integer;
var
    header : pENetProtocolHeader;
    command : pENetProtocol;
    peer : pENetPeer;
    currentData : penet_uint8;
    headerSize : enet_size_t;
    peerID, flags : enet_uint16;
    sessionID : enet_uint8;
    commandNumber : enet_uint8;
    commandSize : enet_size_t;
    sentTime : enet_uint16;
    originalSize : enet_size_t;
    checksum : penet_uint32;
    desiredChecksum : enet_uint32;
    buffer : ENetBuffer;
label commandError;
const _ENetProtocolHeadMin = sizeof(ENetProtocolHeader)-sizeof(ENetProtocolHeader.sentTime);
begin

    if (host ^. receivedDataLength < enet_size_t( _ENetProtocolHeadMin )) then
      begin result := 0; exit; end;

    header := pENetProtocolHeader(@host ^. receivedData[0]);

    peerID := ENET_NET_TO_HOST_16 (header ^. peerID);
    sessionID := (peerID and ENET_PROTOCOL_HEADER_SESSION_MASK) shr ENET_PROTOCOL_HEADER_SESSION_SHIFT;
    flags := peerID and ENET_PROTOCOL_HEADER_FLAG_MASK;
    peerID := peerID and not (ENET_PROTOCOL_HEADER_FLAG_MASK or ENET_PROTOCOL_HEADER_SESSION_MASK);

    if flags and ENET_PROTOCOL_HEADER_FLAG_SENT_TIME <> 0 then
      headerSize:= sizeof (ENetProtocolHeader)
      else headerSize:=_ENetProtocolHeadMin;
    if (host ^. checksumfunc <> nil) then
      Inc(headerSize, sizeof (enet_uint32));
    if (peerID = ENET_PROTOCOL_MAXIMUM_PEER_ID) then
      peer := nil
    else
    if (peerID >= host ^. peerCount) then
      begin result := 0; exit; end
    else
    begin
       peer := @ pENetPeerArray(host ^. peers)^[peerID];

       if (peer ^. state = ENET_PEER_STATE_DISCONNECTED) or
          (peer ^. state = ENET_PEER_STATE_ZOMBIE) or
          ( ( (host ^. receivedAddress.host <> peer ^. address.host) or
              (host ^. receivedAddress.port <> peer ^. address.port) ) and
            (peer ^. address.host <> ENET_HOST_BROADCAST_) ) or
          ( (peer ^. outgoingPeerID < ENET_PROTOCOL_MAXIMUM_PEER_ID) and
            (sessionID <> peer ^. incomingSessionID) ) then
        begin
          Result:=0; exit;
        end;
    end;

   if (flags and ENET_PROTOCOL_HEADER_FLAG_COMPRESSED<>0) then
   begin
       if (host ^. compressor.context = nil ) or ( host ^. compressor.decompress = nil) then
          begin
            Result:=0; exit;
          end;

       originalSize := host ^. compressor.decompress (host ^. compressor.context,
                                   host ^. receivedData + headerSize,
                                   host ^. receivedDataLength - headerSize,
                                   @ (host ^. packetData [1][0]) + headerSize,
                                   sizeof (host ^. packetData [1]) - headerSize);
       if (originalSize <= 0) or (originalSize > sizeof (host ^. packetData [1]) - headerSize) then
         begin result := 0; exit; end;

       system.Move (header^, host ^. packetData [1], headerSize);
       host ^. receivedData := host ^. packetData [1];
       host ^. receivedDataLength := headerSize + originalSize;
   end;

   if (host ^. checksumfunc <> nil) then
   begin
       checksum := penet_uint32 ( @ penet_uint8array(host ^. receivedData)[headerSize - sizeof (enet_uint32)]);
       desiredChecksum := checksum^;

       if peer<>nil then
         checksum^ := peer ^. connectID
         else checksum^ := 0;

       buffer.data := host ^. receivedData;
       buffer.dataLength := host ^. receivedDataLength;

       if (ENET_CALLBACK_ENetChecksumCallback(host ^. checksumfunc)(@ buffer, 1) <> desiredChecksum) then
          begin
            Result:=0; exit;
          end;
   end;

    if (peer <> nil) then
    begin
       peer ^. address.host := host ^. receivedAddress.host;
       peer ^. address.port := host ^. receivedAddress.port;
       inc(peer ^. incomingDataTotal , host ^. receivedDataLength);
    end;

    currentData := penet_uint8(PAnsiChar(@host ^. receivedData[0]) + headerSize);

    while PAnsiChar(currentData)< PAnsiChar(@ penet_uint8array(host ^. receivedData)[host ^. receivedDataLength]) do
    begin

       command := pENetProtocol (currentData);

       if (PAnsiChar(currentData) + sizeof (ENetProtocolCommandHeader) > PAnsiChar(@ penet_uint8array(host ^. receivedData)[host ^. receivedDataLength])) then
         break;

       commandNumber := command ^. header.command and ENET_PROTOCOL_COMMAND_MASK;
       if (commandNumber >= ENET_PROTOCOL_COMMAND_COUNT) then
         break;

       commandSize := commandSizes [commandNumber];
       if (commandSize = 0) or (PAnsiChar(currentData) + commandSize > PAnsiChar(@ penet_uint8array(host ^. receivedData)[host ^. receivedDataLength])) then
         break;

       inc(PAnsiChar(currentData), commandSize);

       if (peer = nil) and (commandNumber <> ENET_PROTOCOL_COMMAND_CONNECT) then
         break;
         
       command ^. header.reliableSequenceNumber := ENET_NET_TO_HOST_16 (command ^. header.reliableSequenceNumber);

       case (commandNumber) of
       ENET_PROTOCOL_COMMAND_ACKNOWLEDGE: begin
          if 0<>enet_protocol_handle_acknowledge (host, event, peer, command) then
            goto commandError;
       end; //   break;

       ENET_PROTOCOL_COMMAND_CONNECT: begin
          if (peer <> nil) then
            goto commandError;
          peer := enet_protocol_handle_connect (host, header, command);
          if (peer = nil) then
            goto commandError;
       end; //    break;

       ENET_PROTOCOL_COMMAND_VERIFY_CONNECT: begin
          if 0<>enet_protocol_handle_verify_connect (host, event, peer, command) then
            goto commandError;
       end; //   break;

       ENET_PROTOCOL_COMMAND_DISCONNECT: begin
          if longbool(enet_protocol_handle_disconnect (host, peer, command)) then
            goto commandError;
       end; //   break;

       ENET_PROTOCOL_COMMAND_PING: begin
          if longbool(enet_protocol_handle_ping (host, peer, command)) then
            goto commandError;
       end; //   break;

       ENET_PROTOCOL_COMMAND_SEND_RELIABLE: begin
          if longbool(enet_protocol_handle_send_reliable (host, peer, command, @ currentData)) then
            goto commandError;
       end;  //  break;

       ENET_PROTOCOL_COMMAND_SEND_UNRELIABLE: begin
          if longbool(enet_protocol_handle_send_unreliable (host, peer, command, @ currentData)) then
            goto commandError;
       end; //   break;

       ENET_PROTOCOL_COMMAND_SEND_UNSEQUENCED: begin
          if longbool(enet_protocol_handle_send_unsequenced (host, peer, command, @ currentData)) then
            goto commandError;
       end; //   break;

       ENET_PROTOCOL_COMMAND_SEND_FRAGMENT: begin
          if longbool(enet_protocol_handle_send_fragment (host, peer, command, @ currentData)) then
            goto commandError;
       end; //   break;

       ENET_PROTOCOL_COMMAND_BANDWIDTH_LIMIT: begin
          if longbool(enet_protocol_handle_bandwidth_limit (host, peer, command)) then
            goto commandError;
       end; //   break;

       ENET_PROTOCOL_COMMAND_THROTTLE_CONFIGURE: begin
          if longbool(enet_protocol_handle_throttle_configure (host, peer, command)) then
            goto commandError;
       end; //   break;

       ENET_PROTOCOL_COMMAND_SEND_UNRELIABLE_FRAGMENT: begin
          if (enet_protocol_handle_send_unreliable_fragment (host, peer, command, @ currentData)<>0) then
            goto commandError;
       end; //   break;

       else goto commandError;
       end;

       if (peer <> nil) and
           ((command ^. header.command and ENET_PROTOCOL_COMMAND_FLAG_ACKNOWLEDGE) <> 0) then
       begin

           if (flags and ENET_PROTOCOL_HEADER_FLAG_SENT_TIME)=0 then
             break;

           sentTime := ENET_NET_TO_HOST_16 (header ^. sentTime);

           case peer ^. state of
           ENET_PEER_STATE_DISCONNECTING,
           ENET_PEER_STATE_ACKNOWLEDGING_CONNECT,
           ENET_PEER_STATE_DISCONNECTED,
           ENET_PEER_STATE_ZOMBIE: ;
           //  break;

           ENET_PEER_STATE_ACKNOWLEDGING_DISCONNECT:
              if ((command ^. header.command and ENET_PROTOCOL_COMMAND_MASK) = ENET_PROTOCOL_COMMAND_DISCONNECT) then
                enet_peer_queue_acknowledgement (peer, command, sentTime);
           else
              enet_peer_queue_acknowledgement (peer, command, sentTime);
           end;
       end;
    end;

commandError:
    if (event <> nil) and (event ^. EventType <> ENET_EVENT_TYPE_NONE) then
      begin result:=1; exit; end;

    result := 0;
end;
 
function enet_protocol_receive_incoming_commands (host : pENetHost; event : pENetEvent):integer;
var
       receivedLength : integer;
       buffer : ENetBuffer;
begin
    while true do
    begin

       buffer.data := @ host ^. packetData [0];
       buffer.dataLength := sizeof (host ^. packetData [0]);

       receivedLength := enet_socket_receive (host ^. socket,
                                             @ host ^. receivedAddress,
                                             @ buffer,
                                             1);

       if (receivedLength < 0) then
         begin result:=-1; exit; end;

       if (receivedLength = 0) then
         begin result :=0; exit; end;

       host ^. receivedData := host ^. packetData [0];
       host ^. receivedDataLength := receivedLength;

       Inc(host ^. totalReceivedData, receivedLength);
       Inc(host ^. totalReceivedPackets);

       if (host ^. interceptfunc <> nil) then
       begin
          case ( ENET_CALLBACK_ENetInterceptCallback(host ^. interceptfunc)(host, event)) of
          1: begin
               if (event<>nil) and (event ^. EventType <> ENET_EVENT_TYPE_NONE) then
                 begin
                   Result:=1; exit;
                 end;
               continue;
             end;

          -1: begin
                Result:=-1; exit;
              end;

          else ;
            // break;
          end;
       end;

       case enet_protocol_handle_incoming_commands (host, event) of
       1: begin
          result := 1; exit;
       end;
       -1: begin
          result := -1; exit;
       end;
       else ; // break
       end;
    end;

    result := -1;
end;

{$push}
{$R-}
procedure enet_protocol_send_acknowledgements (host : pENetHost; peer : pENetPeer);
var
    command : pENetProtocol;
    buffer : pENetBuffer;
    acknowledgement : pENetAcknowledgement;
    currentAcknowledgement : ENetListIterator;
    reliableSequenceNumber : enet_uint16;
begin
    command := @ host ^. commands [host ^. commandCount];
    buffer := @ host ^. buffers [host ^. bufferCount];

    currentAcknowledgement := enet_list_begin (@ peer ^. acknowledgements);

    while (currentAcknowledgement <> enet_list_end (@ peer ^. acknowledgements)) do
    begin
       if (PAnsiChar(command) >= PAnsiChar( @ host ^. commands [sizeof(host ^. commands) div sizeof(ENetProtocol)])) or
          (PAnsiChar(buffer) >= PAnsiChar( @ host ^. buffers [sizeof(host ^. buffers) div sizeof(ENetBuffer)])) or
           (peer ^. mtu - host ^. packetSize < sizeof (ENetProtocolAcknowledge)) then
       begin
          host ^. continueSending := 1;

          break;
       end;

       acknowledgement := pENetAcknowledgement( currentAcknowledgement);

       currentAcknowledgement := enet_list_next (currentAcknowledgement);

       buffer ^. data := command;
       buffer ^. dataLength := sizeof (ENetProtocolAcknowledge);

       inc(host ^. packetSize , buffer ^. dataLength);

       reliableSequenceNumber := ENET_HOST_TO_NET_16 (acknowledgement ^. command.header.reliableSequenceNumber);

       command ^. header.command := ENET_PROTOCOL_COMMAND_ACKNOWLEDGE;
       command ^. header.channelID := acknowledgement ^. command.header.channelID;
       command ^. header.reliableSequenceNumber := reliableSequenceNumber;
       command ^. acknowledge.receivedReliableSequenceNumber := reliableSequenceNumber;
       command ^. acknowledge.receivedSentTime := ENET_HOST_TO_NET_16 (acknowledgement ^. sentTime);

       if ((acknowledgement ^. command.header.command and ENET_PROTOCOL_COMMAND_MASK) = ENET_PROTOCOL_COMMAND_DISCONNECT) then
         enet_protocol_dispatch_state (host, peer, ENET_PEER_STATE_ZOMBIE);

       enet_list_remove (@ acknowledgement ^. acknowledgementList);
       enet_free (acknowledgement);

       inc(command);
       inc(buffer);
    end;

    host ^. commandCount := (PAnsiChar(command)-PAnsiChar( @host ^. commands[0])) div sizeof(ENetProtocol);
    host ^. bufferCount := (PAnsiChar(buffer)-PAnsiChar( @host ^. buffers[0])) div sizeof(ENetBuffer);
end;

procedure enet_protocol_send_unreliable_outgoing_commands (host : pENetHost;peer : pENetPeer);
var
    command : pENetProtocol;
    buffer : pENetBuffer;
    outgoingCommand : pENetOutgoingCommand;
    currentCommand : ENetListIterator;
    commandSize : enet_size_t;
    reliableSequenceNumber, unreliableSequenceNumber : enet_uint16;
begin
    command := @ host ^. commands [host ^. commandCount];
    buffer := @ host ^. buffers [host ^. bufferCount];

    currentCommand := enet_list_begin (@ peer ^. outgoingUnreliableCommands);

    while (PAnsiChar(currentCommand) <> PAnsiChar(enet_list_end (@ peer ^. outgoingUnreliableCommands))) do
    begin

       outgoingCommand := pENetOutgoingCommand (currentCommand);
       commandSize := commandSizes [outgoingCommand ^. command.header.command and ENET_PROTOCOL_COMMAND_MASK];

       if (PAnsiChar(command) >= PAnsiChar( @ host ^.commands [sizeof(host ^. commands) div sizeof(ENetProtocol)])) or
          (PAnsiChar(buffer)+sizeof(ENetBuffer) >= PAnsiChar( @ host ^. buffers [sizeof(host ^. buffers) div sizeof(ENetBuffer)])) or
          (peer ^. mtu - host ^. packetSize < commandSize) or
          ( (outgoingCommand ^. packet <> nil) and
            (peer ^. mtu - host ^. packetSize < commandSize + outgoingCommand ^. fragmentLength) ) then
       begin
          host ^. continueSending := 1;

          break;
       end;

       currentCommand := enet_list_next (currentCommand);

       if (outgoingCommand ^. packet <> nil) and (outgoingCommand ^. fragmentOffset = 0) then
       begin
          inc(peer ^. packetThrottleCounter , ENET_PEER_PACKET_THROTTLE_COUNTER);
          peer ^. packetThrottleCounter := peer ^. packetThrottleCounter mod ENET_PEER_PACKET_THROTTLE_SCALE;

          if (peer ^. packetThrottleCounter > peer ^. packetThrottle) then
          begin
            reliableSequenceNumber := outgoingCommand ^. reliableSequenceNumber;
            unreliableSequenceNumber := outgoingCommand ^. unreliableSequenceNumber;

            while True do
            begin

             dec( outgoingCommand ^. packet ^. referenceCount );

             if (outgoingCommand ^. packet ^. referenceCount = 0) then
               enet_packet_destroy (outgoingCommand ^. packet);

             enet_list_remove (@ outgoingCommand ^. outgoingCommandList);
             enet_free (outgoingCommand);

             if (PAnsiChar(currentCommand) = PAnsiChar(enet_list_end (@ peer ^. outgoingUnreliableCommands))) then
               break;

             outgoingCommand := pENetOutgoingCommand (currentCommand);
             if (outgoingCommand ^. reliableSequenceNumber <> reliableSequenceNumber) or
                (outgoingCommand ^. unreliableSequenceNumber <> unreliableSequenceNumber) then
               break;

             currentCommand := enet_list_next (currentCommand);
            end;

             continue;
          end;
       end;

       buffer ^. data := command;
       buffer ^. dataLength := commandSize;

       inc(host ^. packetSize , buffer ^. dataLength);

       command^ := outgoingCommand ^. command;

       enet_list_remove (@ outgoingCommand ^. outgoingCommandList);

       if (outgoingCommand ^. packet <> nil) then
       begin
          inc( buffer );

          buffer ^. data := PAnsiChar(outgoingCommand ^. packet ^. data) + outgoingCommand ^. fragmentOffset;
          buffer ^. dataLength := outgoingCommand ^. fragmentLength;

          inc(host ^. packetSize , buffer ^. dataLength);

          enet_list_insert (enet_list_end (@ peer ^. sentUnreliableCommands), outgoingCommand);
       end
       else
         enet_free (outgoingCommand);

       inc(command);
       inc(buffer);
    end;

    host ^. commandCount := (PAnsiChar(command) - PAnsiChar( @host ^. commands[0])) div sizeof(ENetProtocol);
    host ^. bufferCount := (PAnsiChar(buffer) - PAnsiChar( @host ^. buffers[0])) div sizeof(ENetBuffer);

    if (peer ^. state = ENET_PEER_STATE_DISCONNECT_LATER) and
        enet_list_empty (@ peer ^. outgoingReliableCommands) and
        enet_list_empty (@ peer ^. outgoingUnreliableCommands) and
        enet_list_empty (@ peer ^. sentReliableCommands) then
          enet_peer_disconnect (peer, peer ^. eventData);
end;

function enet_protocol_check_timeouts (host : pENetHost; peer : pENetPeer; event : pENetEvent):integer;
var
    outgoingCommand : pENetOutgoingCommand;
    currentCommand, insertPosition : ENetListIterator;
begin

    currentCommand := enet_list_begin (@ peer ^. sentReliableCommands);
    insertPosition := enet_list_begin (@ peer ^. outgoingReliableCommands);

    while (PAnsiChar(currentCommand) <> PAnsiChar(enet_list_end (@ peer ^. sentReliableCommands))) do
    begin
       outgoingCommand := pENetOutgoingCommand(currentCommand);

       currentCommand := enet_list_next (currentCommand);

       if (ENET_TIME_DIFFERENCE (host ^. serviceTime, outgoingCommand ^. sentTime) < outgoingCommand ^. roundTripTimeout) then
         continue;

       if(peer ^. earliestTimeout = 0) or
          ENET_TIME_LESS(outgoingCommand ^. sentTime, peer ^. earliestTimeout) then
           peer ^. earliestTimeout := outgoingCommand ^. sentTime;

       if (peer ^. earliestTimeout <> 0) and
          ( (ENET_TIME_DIFFERENCE (host ^. serviceTime, peer ^. earliestTimeout) >= peer ^. timeoutMaximum) or
               ( (outgoingCommand ^. roundTripTimeout >= outgoingCommand ^. roundTripTimeoutLimit) and
                 (ENET_TIME_DIFFERENCE (host ^. serviceTime, peer ^. earliestTimeout) >= peer ^. timeoutMinimum) ) ) then
       begin
          enet_protocol_notify_disconnect (host, peer, event);

          result := 1; exit;
       end;

       if (outgoingCommand ^. packet <> nil) then
         dec(peer ^. reliableDataInTransit , outgoingCommand ^. fragmentLength);

       inc(peer ^. packetsLost);

       outgoingCommand ^. roundTripTimeout := outgoingCommand ^. roundTripTimeout * 2;

       enet_list_insert (insertPosition, enet_list_remove (@ outgoingCommand ^. outgoingCommandList)); // 2007/10/12

       if (currentCommand = enet_list_begin (@ peer ^. sentReliableCommands)) and
           (not enet_list_empty (@ peer ^. sentReliableCommands)) then
       begin
          outgoingCommand := pENetOutgoingCommand( currentCommand);

          peer ^. nextTimeout := outgoingCommand ^. sentTime + outgoingCommand ^. roundTripTimeout;
       end;
    end;

    result := 0;
end;

function enet_protocol_send_reliable_outgoing_commands (host : pENetHost; peer : pENetPeer):Integer;
var
    command : pENetProtocol;
    buffer : pENetBuffer;
    outgoingCommand : pENetOutgoingCommand;
    currentCommand : ENetListIterator;
    channel : pENetChannel;
    reliableWindow : enet_uint16;
    commandSize : enet_size_t;
    windowExceeded, windowWrap, canPing: Integer;
    windowSize : enet_uint32;
begin
    command := @ host ^. commands [host ^. commandCount];
    buffer  := @ host ^. buffers [host ^. bufferCount];
    windowExceeded := 0;
    windowWrap := 0;
    canPing := 1;

    currentCommand := enet_list_begin (@ peer ^. outgoingReliableCommands);

    while (PAnsiChar(currentCommand) <> PAnsiChar(enet_list_end (@ peer ^. outgoingReliableCommands))) do
    begin
       outgoingCommand := pENetOutgoingCommand(currentCommand);

       if (outgoingCommand ^. command.header.channelID < peer ^. channelCount) then
        channel := @ pENetChannelArray(peer ^. channels)^[outgoingCommand ^. command.header.channelID]
        else channel := nil;
       reliableWindow := outgoingCommand ^. reliableSequenceNumber div ENET_PEER_RELIABLE_WINDOW_SIZE;
       if (channel <> nil) then
       begin
           if (windowWrap=0) and
              (outgoingCommand ^. sendAttempts < 1) and
              (outgoingCommand ^. reliableSequenceNumber mod ENET_PEER_RELIABLE_WINDOW_SIZE=0) and
              ( (channel ^. reliableWindows [(reliableWindow + ENET_PEER_RELIABLE_WINDOWS - 1) mod ENET_PEER_RELIABLE_WINDOWS] >= ENET_PEER_RELIABLE_WINDOW_SIZE) or
                ((channel ^. usedReliableWindows and ((((1 shl ENET_PEER_FREE_RELIABLE_WINDOWS) - 1) shl reliableWindow) or
                (((1 shl ENET_PEER_FREE_RELIABLE_WINDOWS) - 1) shr (ENET_PEER_RELIABLE_WINDOW_SIZE - reliableWindow))))<>0) ) then
                 windowWrap := 1;
         if (windowWrap<>0) then
         begin
           currentCommand := enet_list_next (currentCommand);

           continue;
         end;
       end;

       if (outgoingCommand ^. packet <> nil) then
       begin
         if (windowExceeded=0) then
         begin
            windowSize := (peer ^. packetThrottle * peer ^. windowSize) div ENET_PEER_PACKET_THROTTLE_SCALE;

            if (peer ^. reliableDataInTransit + outgoingCommand ^. fragmentLength > ENET_MAX (windowSize, peer ^. mtu)) then
              windowExceeded := 1;
         end;
         if (windowExceeded<>0) then
         begin
            currentCommand := enet_list_next (currentCommand);

            continue;
         end;
      end;

      canPing := 0;

      commandSize := commandSizes [outgoingCommand ^. command.header.command and ENET_PROTOCOL_COMMAND_MASK];
      if (PAnsiChar(command) >= PAnsiChar( @ host ^. commands [sizeof (host ^. commands) div sizeof (ENetProtocol)])) or
         (PAnsiChar(buffer)+SizeOf(ENetBuffer) >= PAnsiChar( @ host ^. buffers [sizeof (host ^. buffers) div sizeof (ENetBuffer)])) or
         (peer ^. mtu - host ^. packetSize < commandSize) or
         ((outgoingCommand ^. packet <> nil) and
          (enet_uint16(peer ^. mtu - host ^. packetSize) < enet_uint16(commandSize + outgoingCommand ^. fragmentLength))) then
          begin
             host ^. continueSending := 1;

             break;
          end;

       currentCommand := enet_list_next (currentCommand);

       if (channel <> nil) and (outgoingCommand ^. sendAttempts < 1) then
       begin
          channel ^. usedReliableWindows := channel ^. usedReliableWindows or (1 shl reliableWindow);
          inc( channel ^. reliableWindows [reliableWindow]);
       end;

       inc( outgoingCommand ^. sendAttempts);

       if (outgoingCommand ^. roundTripTimeout = 0) then
       begin
          outgoingCommand ^. roundTripTimeout := peer ^. roundTripTime + 4 * peer ^. roundTripTimeVariance;
          outgoingCommand ^. roundTripTimeoutLimit := peer ^. timeoutLimit * outgoingCommand ^. roundTripTimeout;
       end;

       if (enet_list_empty (@ peer ^. sentReliableCommands)) then
         peer ^. nextTimeout := host ^. serviceTime + outgoingCommand ^. roundTripTimeout;

       enet_list_insert (enet_list_end (@ peer ^. sentReliableCommands),
                         enet_list_remove (@ outgoingCommand ^. outgoingCommandList));

       outgoingCommand ^. sentTime := host ^. serviceTime;

       buffer ^. data := command;
       buffer ^. dataLength := commandSize;

       inc(host ^. packetSize , buffer ^. dataLength);
       host ^. headerFlags := host ^. headerFlags or ENET_PROTOCOL_HEADER_FLAG_SENT_TIME;

       command^ := outgoingCommand ^. command;

       if (outgoingCommand ^. packet <> nil) then
       begin
          inc(buffer);
          
          buffer ^. data := PAnsiChar(outgoingCommand ^. packet ^. data) + outgoingCommand ^. fragmentOffset;
          buffer ^. dataLength := outgoingCommand ^. fragmentLength;

          inc(host ^. packetSize , outgoingCommand ^. fragmentLength);

          inc(peer ^. reliableDataInTransit , outgoingCommand ^. fragmentLength);
       end;

       inc(peer ^. packetsSent);

       inc(command);
       inc(buffer);
    end;

    host ^. commandCount := (PAnsiChar(command)-PAnsiChar( @host ^. commands[0])) div sizeof(ENetProtocol);
    host ^. bufferCount := (PAnsiChar(buffer)-PAnsiChar( @host ^. buffers[0])) div sizeof(ENetBuffer);

    Result:=canPing;
end;
{$pop}

function enet_protocol_send_outgoing_commands (host : pENetHost; event : pENetEvent; checkForTimeouts : integer):integer;
var
    headerData : array[0..sizeof (ENetProtocolHeader) + sizeof (enet_uint32)-1] of enet_uint8;
    header : pENetProtocolHeader;
    currentPeer : pENetPeer;
    sentLength : integer;
    packetLoss : enet_uint32;
    shouldCompress : enet_size_t;
    originalSize, compressedSize : enet_size_t;
    checksum : penet_uint32;
label continuework;
begin
    header := pENetProtocolHeader (@headerData);
    shouldCompress := 0;

    host ^. continueSending := 1;

    while (host ^. continueSending <> 0) do begin
    host ^. continueSending := 0;
    currentPeer := host ^. peers;
    while PAnsiChar(currentPeer) < PAnsiChar(@ pENetPeerArray(host ^. peers)^[host ^. peerCount]) do
    begin
        if (currentPeer ^. state = ENET_PEER_STATE_DISCONNECTED) or
            (currentPeer ^. state = ENET_PEER_STATE_ZOMBIE) then
          goto continuework;

        host ^. headerFlags := 0;
        host ^. commandCount := 0;
        host ^. bufferCount := 1;
        host ^. packetSize := sizeof (ENetProtocolHeader);

        if (not enet_list_empty (@ currentPeer ^. acknowledgements)) then
          enet_protocol_send_acknowledgements (host, currentPeer);

        if (checkForTimeouts <> 0) and
            not enet_list_empty (@ currentPeer ^. sentReliableCommands) and
            ENET_TIME_GREATER_EQUAL (host ^. serviceTime, currentPeer ^. nextTimeout) and
            (enet_protocol_check_timeouts (host, currentPeer, event) = 1) then
          begin
            if (event <> nil) and (event ^. EventType <> ENET_EVENT_TYPE_NONE) then
            begin result:=1; exit; end
            else goto continuework;
          end;

        if  ( enet_list_empty (@ currentPeer ^. outgoingReliableCommands) or
              (enet_protocol_send_reliable_outgoing_commands (host, currentPeer)<>0) ) and
            enet_list_empty (@ currentPeer ^. sentReliableCommands) and
            (ENET_TIME_DIFFERENCE (host ^. serviceTime, currentPeer ^. lastReceiveTime) >= currentPeer ^. pingInterval) and
            (currentPeer ^. mtu - host ^. packetSize >= sizeof (ENetProtocolPing)) then
        begin
            enet_peer_ping (currentPeer);
            enet_protocol_send_reliable_outgoing_commands (host, currentPeer);
        end;

        if (not enet_list_empty (@ currentPeer ^. outgoingUnreliableCommands)) then
          enet_protocol_send_unreliable_outgoing_commands (host, currentPeer);

        if (host ^. commandCount = 0) then
          goto continuework;

        if (currentPeer ^. packetLossEpoch = 0) then
          currentPeer ^. packetLossEpoch := host ^. serviceTime
        else
        if (ENET_TIME_DIFFERENCE (host ^. serviceTime, currentPeer ^. packetLossEpoch) >= ENET_PEER_PACKET_LOSS_INTERVAL) and
            (currentPeer ^. packetsSent > 0) then
        begin
           packetLoss := currentPeer ^. packetsLost * ENET_PEER_PACKET_LOSS_SCALE div currentPeer ^. packetsSent;

           dec(currentPeer ^. packetLossVariance , currentPeer ^. packetLossVariance div 4);

           if (packetLoss >= currentPeer ^. packetLoss) then
           begin
              inc(currentPeer ^. packetLoss , (packetLoss - currentPeer ^. packetLoss) div 8);
              inc(currentPeer ^. packetLossVariance , (packetLoss - currentPeer ^. packetLoss) div 4);
           end
           else
           begin
              dec(currentPeer ^. packetLoss , (currentPeer ^. packetLoss - packetLoss) div 8);
              inc(currentPeer ^. packetLossVariance , (currentPeer ^. packetLoss - packetLoss) div 4);
           end;

           currentPeer ^. packetLossEpoch := host ^. serviceTime;
           currentPeer ^. packetsSent := 0;
           currentPeer ^. packetsLost := 0;
        end;

        host ^. buffers[0] . data := @headerData;
        if (host ^. headerFlags and ENET_PROTOCOL_HEADER_FLAG_SENT_TIME)<>0 then
        begin
          header ^. sentTime := ENET_HOST_TO_NET_16 (host ^. serviceTime and $0FFFF);

          host ^. buffers[0] . dataLength := sizeof (ENetProtocolHeader);
        end
        else
          host ^. buffers[0] . dataLength := sizeof(ENetProtocolHeader)-sizeof(ENetProtocolHeader.sentTime);

        shouldCompress := 0;
        if (host ^. compressor.context <> nil) and (host ^. compressor.compress <> nil) then
        begin
            originalSize := host ^. packetSize - sizeof(ENetProtocolHeader);
            compressedSize := host ^. compressor.compress (host ^. compressor.context,
                                        @ host ^. buffers [1], host ^. bufferCount - 1,
                                        originalSize,
                                        @ host ^. packetData [1],
                                        originalSize);
            if (compressedSize > 0) and (compressedSize < originalSize) then
            begin
                host ^. headerFlags := host ^. headerFlags or ENET_PROTOCOL_HEADER_FLAG_COMPRESSED;
                shouldCompress := compressedSize;
            end;
        end;

        if (currentPeer ^. outgoingPeerID < ENET_PROTOCOL_MAXIMUM_PEER_ID) then
          host ^. headerFlags := host ^. headerFlags or (currentPeer ^. outgoingSessionID shl ENET_PROTOCOL_HEADER_SESSION_SHIFT);
        header ^. peerID := ENET_HOST_TO_NET_16 (currentPeer ^. outgoingPeerID or host ^. headerFlags);
        if (host ^. checksumfunc <> nil) then
        begin
            checksum := penet_uint32 ( @ headerData [host ^. buffers[0] . dataLength] );
            if (currentPeer ^. outgoingPeerID < ENET_PROTOCOL_MAXIMUM_PEER_ID) then
               checksum^ := currentPeer ^. connectID
               else
                   checksum^ := 0;
            Inc(host ^. buffers[0] . dataLength , sizeof (enet_uint32));
            checksum^ :=  ENET_CALLBACK_ENetChecksumCallback(host ^. checksumfunc) (host ^. buffers, host ^. bufferCount);
        end;

        if (shouldCompress > 0) then
        begin
            host ^. buffers [1].data := @host ^. packetData [1];
            host ^. buffers [1].dataLength := shouldCompress;
            host ^. bufferCount := 2;
        end;

        currentPeer ^. lastSendTime := host ^. serviceTime;

        sentLength := enet_socket_send (host ^. socket, @ currentPeer ^. address, @host ^. buffers[0], host ^. bufferCount);

        enet_protocol_remove_sent_unreliable_commands (currentPeer);

        if (sentLength < 0) then
          begin result:=-1; exit; end;

        Inc(host ^. totalSentData , sentLength);
        Inc(host ^. totalSentPackets);

continuework:
        inc(currentPeer);
    end;

    end; // while
    
    result := 0;
end;

(** Sends any queued packets on the host specified to its designated peers.

    @param host   host to flush
    @remarks this function need only be used in circumstances where one wishes to send queued packets earlier than in a call to enet_host_service().
    @ingroup host
*)
procedure enet_host_flush (host : pENetHost);
begin
    host ^. serviceTime := enet_time_get ();

    enet_protocol_send_outgoing_commands (host, nil, 0);
end;

(** Checks for any queued events on the host and dispatches one if available.

    @param host    host to check for events
    @param event   an event structure where event details will be placed if available
    @retval > 0 if an event was dispatched
    @retval 0 if no events are available
    @retval < 0 on failure
    @ingroup host
*)

function enet_host_check_events (host : pENetHost; event : pENetEvent):integer;
begin
    if (event = nil) then begin result := -1; exit; end;

    event ^. EventType := ENET_EVENT_TYPE_NONE;
    event ^. peer := nil;
    event ^. packet := nil;

    result := enet_protocol_dispatch_incoming_commands (host, event);
end;

(** Waits for events on the host specified and shuttles packets between
    the host and its peers.

    @param host    host to service
    @param event   an event structure where event details will be placed if one occurs
                   if event = NULL then no events will be delivered
    @param timeout number of milliseconds that ENet should wait for events
    @retval > 0 if an event occurred within the specified time limit
    @retval 0 if no event occurred
    @retval < 0 on failure
    @remarks enet_host_service should be called fairly regularly for adequate performance
    @ingroup host
*)
function enet_host_service (host : pENetHost; event : pENetEvent; timeout : enet_uint32):integer;
var
    waitCondition : enet_uint32;
begin

    if (event <> nil) then
    begin
        event ^. EventType := ENET_EVENT_TYPE_NONE;
        event ^. peer := nil;
        event ^. packet := nil;

        case enet_protocol_dispatch_incoming_commands (host, event) of
        1: begin
            result := 1; exit;
        end;
        -1: begin
            //raise exception.Create('Error dispatching incoming packets');
            result := -1; exit;
        end;
        else ;
        end;
    end;

    host ^. serviceTime := enet_time_get ();

    inc(timeout , host ^. serviceTime);

    repeat
       if (ENET_TIME_DIFFERENCE (host ^. serviceTime, host ^. bandwidthThrottleEpoch) >= ENET_HOST_BANDWIDTH_THROTTLE_INTERVAL) then //
         enet_host_bandwidth_throttle (host);

       case enet_protocol_send_outgoing_commands (host, event, 1) of
       1: begin
          result :=1; exit;
       end;
       -1: begin
          //perror ("Error sending outgoing packets");

          result := -1; exit;
       end;
       else ;
       end;

       case enet_protocol_receive_incoming_commands (host, event) of
       1: begin
          result :=1; exit;
       end;

       -1: begin
          //perror ("Error receiving incoming packets");

          result:=-1; exit;
       end;
       else ;
       end;

       case enet_protocol_send_outgoing_commands (host, event, 1) of
       1: begin
          result :=1; exit;
       end;

       -1: begin
          //perror ("Error sending outgoing packets");

          result :=-1; exit;
       end;
       else ;
       end;

       if (event <> nil) then
       begin
          case enet_protocol_dispatch_incoming_commands (host, event) of
          1: begin
             result :=1; exit;
          end;

          -1: begin
             //perror ("Error dispatching incoming packets");

             result := -1; exit;
          end;
          end;
       end;

       host ^. serviceTime := enet_time_get ();

       if (ENET_TIME_GREATER_EQUAL (host ^. serviceTime, timeout)) then
         begin result:=0; exit; end;

       repeat
          host ^. serviceTime := enet_time_get ();

          if (ENET_TIME_GREATER_EQUAL (host ^. serviceTime, timeout)) then
            begin result:=0; exit; end;

          waitCondition := ENET_SOCKET_WAIT_RECEIVE or ENET_SOCKET_WAIT_INTERRUPT;

          if (enet_socket_wait (host ^. socket, @ waitCondition, ENET_TIME_DIFFERENCE (timeout, host ^. serviceTime)) <> 0) then
            begin result:=-1; exit; end;
       until (waitCondition and ENET_SOCKET_WAIT_INTERRUPT = 0);

       host ^. serviceTime := enet_time_get ();
    until (waitCondition and ENET_SOCKET_WAIT_RECEIVE = 0);

    result := 0;
end;


end.


