unit enet_peer;

(**
 @file  peer.c
 @brief ENet peer management functions

 freepascal
 1.3.14
*)

{$GOTO ON}

interface


uses enet_consts, enet_socket, enet_list, enet_packet;

procedure enet_peer_disconnect (peer : pENetPeer; data : enet_uint32);
procedure enet_peer_disconnect_later (peer : pENetPeer;data : enet_uint32);
procedure enet_peer_disconnect_now (peer : pENetPeer;data : enet_uint32);
procedure enet_peer_ping (peer : pENetPeer);
function enet_peer_queue_acknowledgement (peer : pENetPeer; command : pENetProtocol; sentTime : enet_uint16):pENetAcknowledgement;
function enet_peer_queue_incoming_command (peer:pENetPeer; command: pENetProtocol; data:Pointer; dataLength: sizeint; flags, fragmentCount: enet_uint32):pENetIncomingCommand;
function enet_peer_queue_outgoing_command (peer : pENetPeer;command : pENetProtocol;packet : pENetPacket;offset : enet_uint32;length : enet_uint16):pENetOutgoingCommand;
function enet_peer_receive (peer : pENetPeer; channelID : penet_uint8):pENetPacket;
procedure enet_peer_reset (peer : pENetPeer);
procedure enet_peer_reset_incoming_commands (queue : pENetList);
procedure enet_peer_reset_outgoing_commands (queue : pENetList);
procedure enet_peer_reset_queues (peer : pENetPeer);
function enet_peer_send (peer : pENetPeer;channelID : enet_uint8;packet : pENetPacket):integer;
function enet_peer_throttle (peer : pENetPeer;rtt : enet_uint32):integer;
procedure enet_peer_throttle_configure (peer : pENetPeer;interval : enet_uint32;acceleration : enet_uint32;deceleration : enet_uint32);
procedure enet_peer_dispatch_incoming_reliable_commands (peer:pENetPeer; channel:pENetChannel);
procedure enet_peer_dispatch_incoming_unreliable_commands (peer:pENetPeer; channel:pENetChannel);
procedure enet_peer_setup_outgoing_command (peer:pENetPeer; outgoingCommand:pENetOutgoingCommand);

procedure enet_peer_on_connect (peer:pENetPeer);
procedure enet_peer_on_disconnect (peer:pENetPeer);

implementation

uses enet_host, enet_protocol, enet_callbacks;

(** @defgroup peer ENet peer functions 
    @{
*)

(** Configures throttle parameter for a peer.

    Unreliable packets are dropped by ENet in response to the varying conditions
    of the Internet connection to the peer.  The throttle represents a probability
    that an unreliable packet should not be dropped and thus sent by ENet to the peer.
    The lowest mean round trip time from the sending of a reliable packet to the
    receipt of its acknowledgement is measured over an amount of time specified by
    the interval parameter in milliseconds.  If a measured round trip time happens to
    be significantly less than the mean round trip time measured over the interval, 
    then the throttle probability is increased to allow more traffic by an amount
    specified in the acceleration parameter, which is a ratio to the ENET_PEER_PACKET_THROTTLE_SCALE
    constant.  If a measured round trip time happens to be significantly greater than
    the mean round trip time measured over the interval, then the throttle probability
    is decreased to limit traffic by an amount specified in the deceleration parameter, which
    is a ratio to the ENET_PEER_PACKET_THROTTLE_SCALE constant.  When the throttle has
    a value of ENET_PEER_PACKET_THROTTLE_SCALE, on unreliable packets are dropped by 
    ENet, and so 100% of all unreliable packets will be sent.  When the throttle has a
    value of 0, all unreliable packets are dropped by ENet, and so 0% of all unreliable
    packets will be sent.  Intermediate values for the throttle represent intermediate
    probabilities between 0% and 100% of unreliable packets being sent.  The bandwidth
    limits of the local and foreign hosts are taken into account to determine a 
    sensible limit for the throttle probability above which it should not raise even in
    the best of conditions.

    @param peer peer to configure
    @param interval interval, in milliseconds, over which to measure lowest mean RTT; the default value is ENET_PEER_PACKET_THROTTLE_INTERVAL.
    @param acceleration rate at which to increase the throttle probability as mean RTT declines
    @param deceleration rate at which to decrease the throttle probability as mean RTT increases
*)
procedure enet_peer_throttle_configure (peer : pENetPeer;interval : enet_uint32;acceleration : enet_uint32;deceleration : enet_uint32);
var
    command : ENetProtocol;
begin

    peer^. packetThrottleInterval := interval;
    peer^. packetThrottleAcceleration := acceleration;
    peer^. packetThrottleDeceleration := deceleration;

    command.header.command := ENET_PROTOCOL_COMMAND_THROTTLE_CONFIGURE or ENET_PROTOCOL_COMMAND_FLAG_ACKNOWLEDGE;
    command.header.channelID := $FF;

    command.throttleConfigure.packetThrottleInterval := ENET_HOST_TO_NET_32 (interval);
    command.throttleConfigure.packetThrottleAcceleration := ENET_HOST_TO_NET_32 (acceleration);
    command.throttleConfigure.packetThrottleDeceleration := ENET_HOST_TO_NET_32 (deceleration);

    enet_peer_queue_outgoing_command (peer, @command, nil, 0, 0);
end;

function enet_peer_throttle (peer : pENetPeer;rtt : enet_uint32):integer;
begin
    if (peer^. lastRoundTripTime <= peer^. lastRoundTripTimeVariance) then
    begin
        peer^. packetThrottle := peer^. packetThrottleLimit;
    end
    else
    if (rtt < peer^. lastRoundTripTime) then
    begin
        inc(peer^. packetThrottle , peer^. packetThrottleAcceleration);

        if (peer^. packetThrottle > peer^. packetThrottleLimit) then
          peer^. packetThrottle := peer^. packetThrottleLimit;

        result := 1; exit;
    end
    else
    if rtt > (peer^. lastRoundTripTime + 2 * peer^. lastRoundTripTimeVariance) then
    begin
        if (peer^. packetThrottle > peer^. packetThrottleDeceleration) then
          dec(peer^. packetThrottle, peer^. packetThrottleDeceleration)
        else
          peer^. packetThrottle := 0;

        result := -1; exit;
    end;

    result := 0;
end;

(** Queues a packet to be sent.
    @param peer destination for the packet
    @param channelID channel on which to send
    @param packet packet to send
    @retval 0 on success
    @retval < 0 on failure
*)
function enet_peer_send (peer : pENetPeer;channelID : enet_uint8;packet : pENetPacket):integer;
var
   channel : pENetChannel;
   command : ENetProtocol;
   fragmentLength : enet_size_t;
   startSequenceNumber : enet_uint16;
   fragmentCount, fragmentNumber, fragmentOffset : enet_uint32;
   commandNumber : enet_uint8;
   fragments : ENetList;
   fragment : pENetOutgoingCommand;
begin
   channel := pENetChannel( @ pENetChannelArray(peer^. channels)^[channelID]);

   if (peer^. state <> ENET_PEER_STATE_CONNECTED) or
   (channelID >= peer ^. channelCount) or
   (packet ^. dataLength > peer ^. host ^. maximumPacketSize) then
     begin result := -1; exit; end;

   fragmentLength := peer^. mtu - sizeof (ENetProtocolHeader) - sizeof (ENetProtocolSendFragment);
   if (peer ^. host ^. checksumfunc <> nil) then
     fragmentLength := fragmentLength - sizeof(enet_uint32);

   if (packet^. dataLength > fragmentLength) then
   begin
      fragmentCount := (packet ^. dataLength + fragmentLength - 1) div fragmentLength;

       if (fragmentCount > ENET_PROTOCOL_MAXIMUM_FRAGMENT_COUNT) then
       begin
         Result:=-1; exit;
       end;

       if (packet ^. flags and (ENET_PACKET_FLAG_RELIABLE or ENET_PACKET_FLAG_UNRELIABLE_FRAGMENT) = ENET_PACKET_FLAG_UNRELIABLE_FRAGMENT) and
           (channel ^. outgoingUnreliableSequenceNumber < $0FFFF) then
       begin
          commandNumber := ENET_PROTOCOL_COMMAND_SEND_UNRELIABLE_FRAGMENT;
          startSequenceNumber := ENET_HOST_TO_NET_16 (channel ^. outgoingUnreliableSequenceNumber + 1);
       end
       else
       begin
          commandNumber := ENET_PROTOCOL_COMMAND_SEND_FRAGMENT or ENET_PROTOCOL_COMMAND_FLAG_ACKNOWLEDGE;
          startSequenceNumber := ENET_HOST_TO_NET_16 (channel ^. outgoingReliableSequenceNumber + 1);
       end;

       enet_list_clear (@ fragments);

      fragmentNumber := 0;
      fragmentOffset := 0;
      while fragmentOffset < packet ^. dataLength do
      begin
         if (packet ^. dataLength - fragmentOffset < fragmentLength) then
           fragmentLength := packet ^. dataLength - fragmentOffset;

         fragment := pENetOutgoingCommand (enet_malloc (sizeof (ENetOutgoingCommand)));
         if (fragment = nil ) then
         begin
            while (not enet_list_empty (@ fragments)) do
            begin
               fragment := pENetOutgoingCommand ( enet_list_remove (enet_list_begin (@ fragments)));

               enet_free (fragment);
            end;

            result := -1; exit;
         end;

        fragment ^. fragmentOffset := fragmentOffset;
        fragment ^. fragmentLength := fragmentLength;
        fragment ^. packet := packet;
        fragment ^. command.header.command := commandNumber;
        fragment ^. command.header.channelID := channelID;
        fragment ^. command.sendFragment.startSequenceNumber := startSequenceNumber;
        fragment ^. command.sendFragment.dataLength := ENET_HOST_TO_NET_16 (fragmentLength);
        fragment ^. command.sendFragment.fragmentCount := ENET_HOST_TO_NET_32 (fragmentCount);
        fragment ^. command.sendFragment.fragmentNumber := ENET_HOST_TO_NET_32 (fragmentNumber);
        fragment ^. command.sendFragment.totalLength := ENET_HOST_TO_NET_32 (packet ^. dataLength);
        fragment ^. command.sendFragment.fragmentOffset := ENET_NET_TO_HOST_32 (fragmentOffset);

        enet_list_insert (enet_list_end (@ fragments), fragment);

        inc(fragmentNumber);
        inc(fragmentOffset, fragmentLength);
      end;

     Inc(packet ^. referenceCount, fragmentNumber);

     while (not enet_list_empty (@ fragments)) do
     begin
        fragment := pENetOutgoingCommand ( enet_list_remove (enet_list_begin (@ fragments)));

        enet_peer_setup_outgoing_command (peer, fragment);
     end;

     Result:=0; exit;
   end;

   command.header.channelID := channelID;

   if ((packet ^. flags and (ENET_PACKET_FLAG_RELIABLE or ENET_PACKET_FLAG_UNSEQUENCED)) = ENET_PACKET_FLAG_UNSEQUENCED) then
   begin
      command.header.command := ENET_PROTOCOL_COMMAND_SEND_UNSEQUENCED or ENET_PROTOCOL_COMMAND_FLAG_UNSEQUENCED;
      command.sendUnsequenced.dataLength := ENET_HOST_TO_NET_16 (packet^. dataLength);
   end
   else
   if (packet ^. flags and ENET_PACKET_FLAG_RELIABLE<>0) or (channel ^. outgoingUnreliableSequenceNumber >= $0FFFF) then
   begin
      command.header.command := ENET_PROTOCOL_COMMAND_SEND_RELIABLE or ENET_PROTOCOL_COMMAND_FLAG_ACKNOWLEDGE;
      command.sendReliable.dataLength := ENET_HOST_TO_NET_16 (packet ^. dataLength);
   end
   else
   begin
      command.header.command := ENET_PROTOCOL_COMMAND_SEND_UNRELIABLE;
      command.sendUnreliable.dataLength := ENET_HOST_TO_NET_16 (packet^. dataLength);
   end;

   if (enet_peer_queue_outgoing_command (peer, @ command, packet, 0, packet ^. dataLength) = nil) then
   begin
     Result:=-1; exit;
   end;

   result := 0;
end;

(** Attempts to dequeue any incoming queued packet.
    @param peer peer to dequeue packets from
    @param channelID holds the channel ID of the channel the packet was received on success
    @returns a pointer to the packet, or NULL if there are no available incoming queued packets
*)
function enet_peer_receive (peer : pENetPeer; channelID : penet_uint8):pENetPacket;
var
  incomingCommand : pENetIncomingCommand;
begin
   Result:=nil;

   if (enet_list_empty (@ peer ^. dispatchedCommands)) then
     exit;

   incomingCommand := pENetIncomingCommand (enet_list_remove (enet_list_begin (@ peer ^. dispatchedCommands)));

   if (channelID <> nil) then
     channelID^ := incomingCommand ^. command.header.channelID;

   Result := incomingCommand^. packet;

   Dec(Result ^. referenceCount);

   if (incomingCommand^. fragments <> nil) then
     enet_free (incomingCommand^. fragments);

   enet_free (incomingCommand);

   Dec(peer ^. totalWaitingData , Result ^. dataLength);

end;

procedure enet_peer_reset_outgoing_commands (queue : pENetList);
var
  outgoingCommand : pENetOutgoingCommand;
begin

    while (not enet_list_empty (queue)) do
    begin
       outgoingCommand := pENetOutgoingCommand (enet_list_remove (ENetListIterator(enet_list_begin (queue))));

       if (outgoingCommand^. packet <> nil) then
       begin
          Dec(outgoingCommand^. packet^. referenceCount);

          if (outgoingCommand^. packet^. referenceCount = 0) then
            enet_packet_destroy (outgoingCommand^. packet);
       end;

       enet_free (outgoingCommand);
    end;
end;

procedure enet_peer_remove_incoming_commands (queue : pENetList; startCommand, endCommand : ENetListIterator);
var
  currentCommand : ENetListIterator;
  incomingCommand : pENetIncomingCommand;
begin

   currentCommand:=startCommand;
   while (PChar(currentCommand)<>PChar(endCommand)) do
   begin
       incomingCommand := pENetIncomingCommand (currentCommand);

       currentCommand := enet_list_next (currentCommand);

       enet_list_remove (@ incomingCommand ^. incomingCommandList);

       if (incomingCommand^. packet <> nil) then
       begin
          Dec(incomingCommand^. packet^. referenceCount);

          if (incomingCommand^. packet^. referenceCount = 0) then
            enet_packet_destroy (incomingCommand^. packet);
       end;

       if (incomingCommand^. fragments <> nil) then
         enet_free (incomingCommand^. fragments);

       enet_free (incomingCommand);
    end;
end;


procedure enet_peer_reset_incoming_commands (queue:pENetList);
begin
    enet_peer_remove_incoming_commands(queue, enet_list_begin (queue), enet_list_end (queue));
end;

procedure enet_peer_reset_queues (peer : pENetPeer);
var
    channel : pENetChannel;
begin
   if (peer ^. needsDispatch <> 0) then
   begin
      enet_list_remove (@ peer ^. dispatchList);

      peer ^. needsDispatch := 0;
   end;

    while (not enet_list_empty (@peer^. acknowledgements)) do
      enet_free (enet_list_remove (ENetListIterator(enet_list_begin (@ peer^. acknowledgements))));

    enet_peer_reset_outgoing_commands (@peer ^. sentReliableCommands);
    enet_peer_reset_outgoing_commands (@peer ^. sentUnreliableCommands);
    enet_peer_reset_outgoing_commands (@peer ^. outgoingReliableCommands);
    enet_peer_reset_outgoing_commands (@peer ^. outgoingUnreliableCommands);
    enet_peer_reset_incoming_commands (@peer ^. dispatchedCommands);

    if (peer^. channels <> nil) and (peer^. channelCount > 0) then
    begin
        channel := peer^. channels;
        while PChar(channel) < PChar(@ pENetChannelArray(peer^. channels)^[peer^. channelCount]) do
        begin
            enet_peer_reset_incoming_commands (@ channel^. incomingReliableCommands);
            enet_peer_reset_incoming_commands (@ channel^. incomingUnreliableCommands);
            inc(channel);
        end;

        enet_free (peer^. channels);
    end;

    peer^. channels := nil;
    peer^. channelCount := 0;
end;

procedure enet_peer_on_connect (peer:pENetPeer);
begin
    if ((peer ^. state <> ENET_PEER_STATE_CONNECTED) and (peer ^. state <> ENET_PEER_STATE_DISCONNECT_LATER)) then
    begin
        if (peer ^. incomingBandwidth <> 0) then
            Inc(peer ^. host ^. bandwidthLimitedPeers);

        Inc(peer ^. host ^. connectedPeers);
    end;
end;

procedure enet_peer_on_disconnect (peer:pENetPeer);
begin
    if ((peer ^. state = ENET_PEER_STATE_CONNECTED) or (peer ^. state = ENET_PEER_STATE_DISCONNECT_LATER)) then
    begin
        if (peer ^. incomingBandwidth <> 0) then
          Dec(peer ^. host ^. bandwidthLimitedPeers);

        Dec(peer ^. host ^. connectedPeers);
    end;
end;

(** Forcefully disconnects a peer.
    @param peer peer to forcefully disconnect
    @remarks The foreign host represented by the peer is not notified of the disconnection and will timeout
    on its connection to the local host.
*)
procedure enet_peer_reset (peer : pENetPeer);
begin
    enet_peer_on_disconnect (peer);

    peer^. outgoingPeerID := ENET_PROTOCOL_MAXIMUM_PEER_ID;
    peer^. connectID := 0;

    peer^. state := ENET_PEER_STATE_DISCONNECTED;

    peer^. incomingBandwidth := 0;
    peer^. outgoingBandwidth := 0;
    peer^. incomingBandwidthThrottleEpoch := 0;
    peer^. outgoingBandwidthThrottleEpoch := 0;
    peer^. incomingDataTotal := 0;
    peer^. outgoingDataTotal := 0;
    peer^. lastSendTime := 0;
    peer^. lastReceiveTime := 0;
    peer^. nextTimeout := 0;
    peer^. earliestTimeout := 0;
    peer^. packetLossEpoch := 0;
    peer^. packetsSent := 0;
    peer^. packetsLost := 0;
    peer^. packetLoss := 0;
    peer^. packetLossVariance := 0;
    peer^. packetThrottle := ENET_PEER_DEFAULT_PACKET_THROTTLE;
    peer^. packetThrottleLimit := ENET_PEER_PACKET_THROTTLE_SCALE;
    peer^. packetThrottleCounter := 0;
    peer^. packetThrottleEpoch := 0;
    peer^. packetThrottleAcceleration := ENET_PEER_PACKET_THROTTLE_ACCELERATION;
    peer^. packetThrottleDeceleration := ENET_PEER_PACKET_THROTTLE_DECELERATION;
    peer^. packetThrottleInterval := ENET_PEER_PACKET_THROTTLE_INTERVAL;
    peer^. pingInterval := ENET_PEER_PING_INTERVAL_;
    peer^. timeoutLimit := ENET_PEER_TIMEOUT_LIMIT;
    peer^. timeoutMinimum := ENET_PEER_TIMEOUT_MINIMUM;
    peer^. timeoutMaximum := ENET_PEER_TIMEOUT_MAXIMUM;
    peer^. lastRoundTripTime := ENET_PEER_DEFAULT_ROUND_TRIP_TIME;
    peer^. lowestRoundTripTime := ENET_PEER_DEFAULT_ROUND_TRIP_TIME;
    peer^. lastRoundTripTimeVariance := 0;
    peer^. highestRoundTripTimeVariance := 0;
    peer^. roundTripTime := ENET_PEER_DEFAULT_ROUND_TRIP_TIME;
    peer^. roundTripTimeVariance := 0;
    peer^. mtu := peer^. host^. mtu;
    peer^. reliableDataInTransit := 0;
    peer^. outgoingReliableSequenceNumber := 0;
    peer^. windowSize := ENET_PROTOCOL_MAXIMUM_WINDOW_SIZE;
    peer^. incomingUnsequencedGroup := 0;
    peer^. outgoingUnsequencedGroup := 0;
    peer^. eventData := 0;
    peer^. totalWaitingData := 0;

    fillchar (peer^. unsequencedWindow[0], sizeof (peer^. unsequencedWindow), 0);
    
    enet_peer_reset_queues (peer);
end;

(** Sends a ping request to a peer.
    @param peer destination for the ping request
    @remarks ping requests factor into the mean round trip time as designated by the 
    roundTripTime field in the ENetPeer structure.  Enet automatically pings all connected
    peers at regular intervals, however, this function may be called to ensure more
    frequent ping requests.
*)
procedure enet_peer_ping (peer : pENetPeer);
var
    command : ENetProtocol;
begin

    if (peer^. state <> ENET_PEER_STATE_CONNECTED) then
      exit;

    command.header.command := ENET_PROTOCOL_COMMAND_PING or ENET_PROTOCOL_COMMAND_FLAG_ACKNOWLEDGE;
    command.header.channelID := $FF;
   
    enet_peer_queue_outgoing_command (peer, @ command, nil, 0, 0);
end;

(** Sets the interval at which pings will be sent to a peer.

    Pings are used both to monitor the liveness of the connection and also to dynamically
    adjust the throttle during periods of low traffic so that the throttle has reasonable
    responsiveness during traffic spikes.

    @param peer the peer to adjust
    @param pingInterval the interval at which to send pings; defaults to ENET_PEER_PING_INTERVAL if 0
*)
procedure enet_peer_ping_interval (peer:pENetPeer; pingInterval : enet_uint32 );
begin
   if pingInterval<>0 then
    peer^. pingInterval := pingInterval
    else peer^. pingInterval := ENET_PEER_PING_INTERVAL_;
end;

(** Sets the timeout parameters for a peer.

    The timeout parameter control how and when a peer will timeout from a failure to acknowledge
    reliable traffic. Timeout values use an exponential backoff mechanism, where if a reliable
    packet is not acknowledge within some multiple of the average RTT plus a variance tolerance,
    the timeout will be doubled until it reaches a set limit. If the timeout is thus at this
    limit and reliable packets have been sent but not acknowledged within a certain minimum time
    period, the peer will be disconnected. Alternatively, if reliable packets have been sent
    but not acknowledged for a certain maximum time period, the peer will be disconnected regardless
    of the current timeout limit value.

    @param peer the peer to adjust
    @param timeoutLimit the timeout limit; defaults to ENET_PEER_TIMEOUT_LIMIT if 0
    @param timeoutMinimum the timeout minimum; defaults to ENET_PEER_TIMEOUT_MINIMUM if 0
    @param timeoutMaximum the timeout maximum; defaults to ENET_PEER_TIMEOUT_MAXIMUM if 0
*)

procedure enet_peer_timeout (peer:pENetPeer; timeoutLimit, timeoutMinimum, timeoutMaximum : enet_uint32);
begin
   if timeoutLimit<>0 then
      peer^. timeoutLimit := timeoutLimit
          else peer^. timeoutLimit := ENET_PEER_TIMEOUT_LIMIT;
   if timeoutMinimum<>0 then
      peer^. timeoutMinimum := timeoutMinimum
      else peer^. timeoutMinimum := ENET_PEER_TIMEOUT_MINIMUM;
   if timeoutMaximum<>0 then
      peer^. timeoutMaximum := timeoutMaximum
      else peer^. timeoutMaximum := ENET_PEER_TIMEOUT_MAXIMUM;
end;

(** Force an immediate disconnection from a peer.
    @param peer peer to disconnect
    @param data data describing the disconnection
    @remarks No ENET_EVENT_DISCONNECT event will be generated. The foreign peer is not
    guarenteed to receive the disconnect notification, and is reset immediately upon
    return from this function.
*)
procedure enet_peer_disconnect_now (peer : pENetPeer;data : enet_uint32);
var
    command : ENetProtocol;
begin

    if (peer^. state = ENET_PEER_STATE_DISCONNECTED) then
      exit;

    if (peer^. state <> ENET_PEER_STATE_ZOMBIE) and
        (peer^. state <> ENET_PEER_STATE_DISCONNECTING) then
    begin
        enet_peer_reset_queues (peer);

        command.header.command := ENET_PROTOCOL_COMMAND_DISCONNECT or ENET_PROTOCOL_COMMAND_FLAG_UNSEQUENCED;
        command.header.channelID := $FF;
        command.disconnect.data := ENET_HOST_TO_NET_32 (data);

        enet_peer_queue_outgoing_command (peer, @ command, nil, 0, 0);

        enet_host_flush (peer^. host);
    end;

    enet_peer_reset (peer);
end;

(** Request a disconnection from a peer.
    @param peer peer to request a disconnection
    @param data data describing the disconnection
    @remarks An ENET_EVENT_DISCONNECT event will be generated by enet_host_service()
    once the disconnection is complete.
*)
procedure enet_peer_disconnect (peer : pENetPeer; data : enet_uint32);
var
    command : ENetProtocol;
begin

    if (peer^. state = ENET_PEER_STATE_DISCONNECTING) or
        (peer^. state = ENET_PEER_STATE_DISCONNECTED) or
        (peer^. state = ENET_PEER_STATE_ACKNOWLEDGING_DISCONNECT) or
        (peer^. state = ENET_PEER_STATE_ZOMBIE) then 
      exit;

    enet_peer_reset_queues (peer);

    command.header.command := ENET_PROTOCOL_COMMAND_DISCONNECT;
    command.header.channelID := $FF;
    command.disconnect.data := ENET_HOST_TO_NET_32 (data);

    if (peer^. state = ENET_PEER_STATE_CONNECTED) or (peer^. state = ENET_PEER_STATE_DISCONNECT_LATER) then
      command.header.command := command.header.command or ENET_PROTOCOL_COMMAND_FLAG_ACKNOWLEDGE
    else
      command.header.command := command.header.command or ENET_PROTOCOL_COMMAND_FLAG_UNSEQUENCED;
    
    enet_peer_queue_outgoing_command (peer, @ command, nil, 0, 0);

    if (peer^. state = ENET_PEER_STATE_CONNECTED) or (peer^. state = ENET_PEER_STATE_DISCONNECT_LATER) then
    begin
        enet_peer_on_disconnect (peer);

        peer ^. state := ENET_PEER_STATE_DISCONNECTING;
    end
    else
    begin
        enet_host_flush (peer^. host);
        enet_peer_reset (peer);
    end;
end;

(** Request a disconnection from a peer, but only after all queued outgoing packets are sent.
    @param peer peer to request a disconnection
    @param data data describing the disconnection
    @remarks An ENET_EVENT_DISCONNECT event will be generated by enet_host_service()
    once the disconnection is complete.
*)
procedure enet_peer_disconnect_later (peer : pENetPeer;data : enet_uint32);
begin
    if ((peer^. state = ENET_PEER_STATE_CONNECTED) or (peer^. state = ENET_PEER_STATE_DISCONNECT_LATER)) and
        (not (enet_list_empty (@ peer^. outgoingReliableCommands) and
           enet_list_empty (@ peer^. outgoingUnreliableCommands) and
           enet_list_empty (@ peer^. sentReliableCommands))) then
    begin
        peer^. state := ENET_PEER_STATE_DISCONNECT_LATER;
        peer^. eventData := data;
    end
    else
      enet_peer_disconnect (peer, data);
end;

function enet_peer_queue_acknowledgement (peer : pENetPeer; command : pENetProtocol; sentTime : enet_uint16):pENetAcknowledgement;
var
    //acknowledgement : pENetAcknowledgement;
    channel : pENetChannel;
    reliableWindow, currentWindow : enet_uint16;
begin
    Result:=nil;

    if (command^. header.channelID < peer^. channelCount) then
    begin
        channel := @(pENetChannelArray(peer^. channels)^[command^. header.channelID]);
        reliableWindow := command^. header.reliableSequenceNumber div ENET_PEER_RELIABLE_WINDOW_SIZE;
        currentWindow := channel^. incomingReliableSequenceNumber div ENET_PEER_RELIABLE_WINDOW_SIZE;

        if (command^. header.reliableSequenceNumber < channel^. incomingReliableSequenceNumber) then
           inc(reliableWindow , ENET_PEER_RELIABLE_WINDOWS);

        if (reliableWindow >= (currentWindow + ENET_PEER_FREE_RELIABLE_WINDOWS - 1)) and (reliableWindow <= (currentWindow + ENET_PEER_FREE_RELIABLE_WINDOWS)) then // 2007/10/22
          begin result := nil; exit; end;
    end;

    Result := pENetAcknowledgement( enet_malloc (sizeof (ENetAcknowledgement)));
    if (Result = nil) then
       exit;

    Inc(peer^. outgoingDataTotal , sizeof (ENetProtocolAcknowledge));

    Result^. sentTime := sentTime;
    Result^. command := command^;

    enet_list_insert (enet_list_end (@ peer^. acknowledgements), pENetListNode(Result));

end;


procedure enet_peer_setup_outgoing_command (peer:pENetPeer; outgoingCommand:pENetOutgoingCommand);
var
    channel : pENetChannel;
begin
    channel := @ (pENetChannelArray(peer^. channels)^[outgoingCommand^. command.header.channelID]);

    Inc(peer^. outgoingDataTotal , enet_protocol_command_size (outgoingCommand^. command.header.command) + outgoingCommand^. fragmentLength);

    if (outgoingCommand^. command.header.channelID = $0FF) then
    begin
       inc( peer^. outgoingReliableSequenceNumber);

       outgoingCommand^. reliableSequenceNumber := peer^. outgoingReliableSequenceNumber;
       outgoingCommand^. unreliableSequenceNumber := 0;
    end
    else
    if (outgoingCommand^. command.header.command and ENET_PROTOCOL_COMMAND_FLAG_ACKNOWLEDGE <> 0) then
    begin
       inc( channel^. outgoingReliableSequenceNumber);
       channel^. outgoingUnreliableSequenceNumber := 0;

       outgoingCommand^. reliableSequenceNumber := channel^. outgoingReliableSequenceNumber;
       outgoingCommand^. unreliableSequenceNumber := 0;
    end
    else
    if (outgoingCommand^. command.header.command and ENET_PROTOCOL_COMMAND_FLAG_UNSEQUENCED <> 0) then
    begin
       inc( peer^. outgoingUnsequencedGroup);

       outgoingCommand^. reliableSequenceNumber := 0;
       outgoingCommand^. unreliableSequenceNumber := 0;
    end
    else
    begin
       if (outgoingCommand^. fragmentOffset = 0) then
       inc( channel^. outgoingUnreliableSequenceNumber);

       outgoingCommand^. reliableSequenceNumber := channel^. outgoingReliableSequenceNumber;
       outgoingCommand^. unreliableSequenceNumber := channel^. outgoingUnreliableSequenceNumber;
    end;

    outgoingCommand^. sendAttempts := 0;
    outgoingCommand^. sentTime := 0;
    outgoingCommand^. roundTripTimeout := 0;
    outgoingCommand^. roundTripTimeoutLimit := 0;
    outgoingCommand^. command.header.reliableSequenceNumber := ENET_HOST_TO_NET_16 (outgoingCommand^. reliableSequenceNumber);

    case (outgoingCommand^. command.header.command and ENET_PROTOCOL_COMMAND_MASK) of
    ENET_PROTOCOL_COMMAND_SEND_UNRELIABLE:
        outgoingCommand^. command.sendUnreliable.unreliableSequenceNumber := ENET_HOST_TO_NET_16 (outgoingCommand^. unreliableSequenceNumber);
    ENET_PROTOCOL_COMMAND_SEND_UNSEQUENCED:
        outgoingCommand^. command.sendUnsequenced.unsequencedGroup := ENET_HOST_TO_NET_16 (peer^. outgoingUnsequencedGroup);
    end;

    if (outgoingCommand^. command.header.command and ENET_PROTOCOL_COMMAND_FLAG_ACKNOWLEDGE <> 0) then
       enet_list_insert (enet_list_end (@ peer^. outgoingReliableCommands), pENetListNode(outgoingCommand))
       else
          enet_list_insert (enet_list_end (@ peer^. outgoingUnreliableCommands), pENetListNode(outgoingCommand));
end;


function enet_peer_queue_outgoing_command (peer:pENetPeer; command:pENetProtocol; packet:pENetPacket; offset:enet_uint32; length:enet_uint16):pENetOutgoingCommand;
begin
    Result := pENetOutgoingCommand ( enet_malloc (sizeof (ENetOutgoingCommand)));
    if (Result = nil) then
       exit;

    Result^. command := command^;
    Result^. fragmentOffset := offset;
    Result^. fragmentLength := length;
    Result^. packet := packet;
    if (packet <> nil) then
      Inc(packet^. referenceCount);

    enet_peer_setup_outgoing_command (peer, Result);

end;

procedure enet_peer_dispatch_incoming_unreliable_commands (peer:pENetPeer; channel:pENetChannel);
var
    droppedCommand, startCommand, currentCommand : ENetListIterator;
    incomingCommand : pENetIncomingCommand;
    reliableWindow, currentWindow : enet_uint16;
label
    continue1;
begin
    droppedCommand:=enet_list_begin (@ channel^. incomingUnreliableCommands);
    startCommand:=droppedCommand;
    currentCommand:=droppedCommand;


    while currentCommand <> enet_list_end (@ channel^. incomingUnreliableCommands) do
    begin
       incomingCommand := pENetIncomingCommand (currentCommand);

       if ((incomingCommand^. command.header.command and ENET_PROTOCOL_COMMAND_MASK) = ENET_PROTOCOL_COMMAND_SEND_UNSEQUENCED) then
         goto continue1;

       if (incomingCommand^. reliableSequenceNumber = channel^. incomingReliableSequenceNumber) then
       begin
          if (incomingCommand^. fragmentsRemaining <= 0) then
          begin
             channel^. incomingUnreliableSequenceNumber := incomingCommand^. unreliableSequenceNumber;
             goto continue1;
          end;

          if (startCommand <> currentCommand) then
          begin
             enet_list_move (enet_list_end (@ peer^. dispatchedCommands), startCommand, enet_list_previous (currentCommand));

             if (peer^. needsDispatch=0) then
             begin
                enet_list_insert (enet_list_end (@ peer^. host^. dispatchQueue), @ peer^. dispatchList);

                peer^. needsDispatch := 1;
             end;

             droppedCommand := currentCommand;
          end
          else
          if (droppedCommand <> currentCommand) then
            droppedCommand := enet_list_previous (currentCommand);
       end
       else
       begin
          reliableWindow := incomingCommand^. reliableSequenceNumber div ENET_PEER_RELIABLE_WINDOW_SIZE;
          currentWindow := channel^. incomingReliableSequenceNumber div ENET_PEER_RELIABLE_WINDOW_SIZE;
          if (incomingCommand^. reliableSequenceNumber < channel^. incomingReliableSequenceNumber) then
            Inc(reliableWindow ,ENET_PEER_RELIABLE_WINDOWS);
          if (reliableWindow >= currentWindow) and (reliableWindow < currentWindow + ENET_PEER_FREE_RELIABLE_WINDOWS - 1) then
            break;

          droppedCommand := enet_list_next (currentCommand);

          if (startCommand <> currentCommand) then
          begin
             enet_list_move (enet_list_end (@ peer^. dispatchedCommands), startCommand, enet_list_previous (currentCommand));

             if (peer^. needsDispatch=0) then
             begin
                enet_list_insert (enet_list_end (@ peer^. host^. dispatchQueue), @ peer^. dispatchList);

                peer^. needsDispatch := 1;
             end;
          end;
       end;
       startCommand := enet_list_next (currentCommand);
continue1:
       currentCommand := enet_list_next (currentCommand);
    end;

    if (startCommand <> currentCommand) then
    begin
       enet_list_move (enet_list_end (@ peer^. dispatchedCommands), startCommand, enet_list_previous (currentCommand));

       if (peer^. needsDispatch=0) then
       begin
           enet_list_insert (enet_list_end (@ peer^. host^. dispatchQueue), @ peer^. dispatchList);

           peer^. needsDispatch := 1;
       end;

       droppedCommand := currentCommand;
    end;

    enet_peer_remove_incoming_commands (@channel^. incomingUnreliableCommands, enet_list_begin (@ channel^. incomingUnreliableCommands), droppedCommand);
end;

procedure enet_peer_dispatch_incoming_reliable_commands (peer:pENetPeer; channel:pENetChannel);
var
    currentCommand : ENetListIterator;
    incomingCommand : pENetIncomingCommand;
begin
    currentCommand:=enet_list_begin (@ channel^. incomingReliableCommands);
    while currentCommand <> enet_list_end (@ channel^. incomingReliableCommands) do
    begin
       incomingCommand := pENetIncomingCommand ( currentCommand );

       if (incomingCommand^. fragmentsRemaining > 0) or
          (incomingCommand^. reliableSequenceNumber <> enet_uint16 ((channel^. incomingReliableSequenceNumber + 1))) then
         break;

       channel^. incomingReliableSequenceNumber := incomingCommand^. reliableSequenceNumber;

       if (incomingCommand^. fragmentCount > 0) then
         Inc(channel^. incomingReliableSequenceNumber , incomingCommand^. fragmentCount - 1);

       currentCommand := enet_list_next (currentCommand);
    end;

    if (currentCommand = enet_list_begin (@ channel^. incomingReliableCommands)) then
      exit;

    channel^. incomingUnreliableSequenceNumber := 0;

    enet_list_move (enet_list_end (@ peer^. dispatchedCommands), enet_list_begin (@ channel^. incomingReliableCommands), enet_list_previous (currentCommand));

    if (peer^. needsDispatch=0) then
    begin
       enet_list_insert (enet_list_end (@ peer^. host^. dispatchQueue), @ peer^. dispatchList);

       peer^. needsDispatch := 1;
    end;

    if (not enet_list_empty (@ channel^. incomingUnreliableCommands)) then
       enet_peer_dispatch_incoming_unreliable_commands (peer, channel);
end;

var
    dummyCommand : ENetIncomingCommand; // static

function enet_peer_queue_incoming_command (peer:pENetPeer; command: pENetProtocol; data:Pointer; dataLength: sizeint; flags, fragmentCount: enet_uint32):pENetIncomingCommand;
var
    channel : pENetChannel;
    unreliableSequenceNumber, reliableSequenceNumber : enet_uint32;
    reliableWindow, currentWindow : enet_uint16;
    incomingCommand : pENetIncomingCommand;
    currentCommand : ENetListIterator;
    packet : pENetPacket;
label discardCommand, continuework1, continuework2, NotifyError;
begin
    channel := @ pENetChannelArray(peer^. channels)^[command^. header.channelID];
    unreliableSequenceNumber := 0;
    reliableSequenceNumber:=0;
    packet := nil;

    if (peer^. state = ENET_PEER_STATE_DISCONNECT_LATER) then
      goto discardCommand;

    if ((command^. header.command and ENET_PROTOCOL_COMMAND_MASK) <> ENET_PROTOCOL_COMMAND_SEND_UNSEQUENCED) then
    begin
        reliableSequenceNumber := command^. header.reliableSequenceNumber;
        reliableWindow := reliableSequenceNumber div ENET_PEER_RELIABLE_WINDOW_SIZE;
        currentWindow := channel^. incomingReliableSequenceNumber div ENET_PEER_RELIABLE_WINDOW_SIZE;

        if (reliableSequenceNumber < channel^. incomingReliableSequenceNumber) then
           inc(reliableWindow , ENET_PEER_RELIABLE_WINDOWS);

        if (reliableWindow < currentWindow) or (reliableWindow >= (currentWindow + ENET_PEER_FREE_RELIABLE_WINDOWS - 1)) then
          goto discardCommand;
    end;

    case (command^. header.command and ENET_PROTOCOL_COMMAND_MASK) of
    ENET_PROTOCOL_COMMAND_SEND_FRAGMENT,
    ENET_PROTOCOL_COMMAND_SEND_RELIABLE: begin
       if (reliableSequenceNumber = channel^. incomingReliableSequenceNumber) then
           goto discardCommand;

       currentCommand := enet_list_previous (enet_list_end (@ channel^. incomingReliableCommands));
       while PChar(currentCommand) <> PChar( enet_list_end (@ channel^. incomingReliableCommands)) do
       begin
          incomingCommand := pENetIncomingCommand (currentCommand);

          if (reliableSequenceNumber >= channel^. incomingReliableSequenceNumber) then
          begin
             if (incomingCommand^. reliableSequenceNumber < channel^. incomingReliableSequenceNumber) then
               goto continuework2;
          end
          else
          if (incomingCommand^. reliableSequenceNumber >= channel^. incomingReliableSequenceNumber) then
            break;

          if (incomingCommand^. reliableSequenceNumber <= reliableSequenceNumber) then
          begin
             if (incomingCommand^. reliableSequenceNumber < reliableSequenceNumber) then
               break;

             goto discardCommand;
          end;
continuework2:
          currentCommand := enet_list_previous (currentCommand);
       end;
       end; // case break;

    ENET_PROTOCOL_COMMAND_SEND_UNRELIABLE,
    ENET_PROTOCOL_COMMAND_SEND_UNRELIABLE_FRAGMENT:
      begin
       unreliableSequenceNumber := ENET_NET_TO_HOST_16 (command^. sendUnreliable.unreliableSequenceNumber);

       if (reliableSequenceNumber = channel^. incomingReliableSequenceNumber) and
          (unreliableSequenceNumber <= channel^. incomingUnreliableSequenceNumber) then
         goto discardCommand;

       currentCommand := enet_list_previous (enet_list_end (@ channel^. incomingUnreliableCommands));
       while PChar(currentCommand) <> PChar(enet_list_end (@ channel^. incomingUnreliableCommands)) do
       begin
          incomingCommand := pENetIncomingCommand( currentCommand);

        if ((command^. header.command and ENET_PROTOCOL_COMMAND_MASK) = ENET_PROTOCOL_COMMAND_SEND_UNSEQUENCED) then
            goto continuework1;

          if (reliableSequenceNumber >= channel^. incomingReliableSequenceNumber) then
          begin
             if (incomingCommand^. reliableSequenceNumber < channel^. incomingReliableSequenceNumber) then
               goto continuework1;
          end
          else
          if (incomingCommand^. reliableSequenceNumber >= channel^. incomingReliableSequenceNumber) then
            break;

          if (incomingCommand^. reliableSequenceNumber < reliableSequenceNumber) then
            break;

          if (incomingCommand^. reliableSequenceNumber > reliableSequenceNumber) then
            goto continuework1;

          if (incomingCommand^. unreliableSequenceNumber <= unreliableSequenceNumber) then
          begin
             if (incomingCommand^. unreliableSequenceNumber < unreliableSequenceNumber) then
               break;

             goto discardCommand;
          end;
continuework1:
          currentCommand := enet_list_previous (currentCommand);
       end;
       end; // break;

    ENET_PROTOCOL_COMMAND_SEND_UNSEQUENCED: begin
       currentCommand := enet_list_end (@ channel^. incomingUnreliableCommands);
       end; // break;

    else goto discardCommand;
    end;  // end case

    if (peer ^. totalWaitingData >= peer ^. host ^. maximumWaitingData) then
      goto notifyError;

    packet := enet_packet_create (data, dataLength, flags);
    if (packet = nil) then
      goto notifyError;

    incomingCommand := pENetIncomingCommand(enet_malloc (sizeof (ENetIncomingCommand)));
    if (incomingCommand = nil) then
      goto notifyError;

    incomingCommand^. reliableSequenceNumber := command^. header.reliableSequenceNumber;
    incomingCommand^. unreliableSequenceNumber := unreliableSequenceNumber and $FFFF;
    incomingCommand^. command := command^;
    incomingCommand^. fragmentCount := fragmentCount;
    incomingCommand^. fragmentsRemaining := fragmentCount;
    incomingCommand^. packet := packet;
    incomingCommand^. fragments := nil;
    
    if (fragmentCount > 0) then
    begin
       if (fragmentCount <= ENET_PROTOCOL_MAXIMUM_FRAGMENT_COUNT) then
         incomingCommand^. fragments := penet_uint32(enet_malloc ((fragmentCount + 31) div 32 * sizeof (enet_uint32)));
       if (incomingCommand^. fragments = nil) then
       begin
          enet_free (incomingCommand);

          goto notifyError;
       end;
       fillchar (incomingCommand^. fragments^, (fragmentCount + 31) div 32 * sizeof (enet_uint32), 0);
    end;

    if (packet <> nil) then
    begin
       Inc(packet ^. referenceCount);

       Inc(peer ^. totalWaitingData, packet ^. dataLength);
    end;

    enet_list_insert (enet_list_next (currentCommand), pENetListNode(incomingCommand));

    case (command^. header.command and ENET_PROTOCOL_COMMAND_MASK) of
    ENET_PROTOCOL_COMMAND_SEND_FRAGMENT,
    ENET_PROTOCOL_COMMAND_SEND_RELIABLE:
       enet_peer_dispatch_incoming_reliable_commands (peer, channel);
    else
       enet_peer_dispatch_incoming_unreliable_commands (peer, channel);
    end;

    result := incomingCommand; exit;

discardCommand:
    if (fragmentCount > 0) then goto notifyError;

    if (packet <> nil ) and (packet^. referenceCount = 0) then
      enet_packet_destroy (packet);

    Result := @dummyCommand;  exit;

notifyError:
    if (packet <> nil) and (packet^. referenceCount = 0) then
         enet_packet_destroy (packet);

    result := nil;
end;

(** @} *)

end.



