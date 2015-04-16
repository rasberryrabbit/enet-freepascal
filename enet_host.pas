unit enet_host;

(**
 @file host.c
 @brief ENet host management functions

 freepascal
 1.3.12
*)

{$GOTO ON}

interface

uses enet_consts;

procedure enet_host_bandwidth_limit (host : pENetHost;incomingBandwidth : enet_uint32; outgoingBandwidth : enet_uint32);
procedure enet_host_bandwidth_throttle (host : pENetHost);
procedure enet_host_broadcast (host : pENetHost; channelID : enet_uint8; packet : pENetPacket);
function enet_host_connect (host : pENetHost; address : pENetAddress; channelCount : enet_size_t; data : enet_uint32):pENetPeer;
function enet_host_create (address : pENetAddress;peerCount, channelLimit : enet_size_t; incomingBandwidth : enet_uint32;outgoingBandwidth : enet_uint32):pENetHost;
procedure enet_host_destroy (host : pENetHost);


implementation

uses enet_peer, enet_packet, enet_list, enet_socket, enet_callbacks
  {$ifdef MSWINDOWS}
  , mmsystem
  {$else}
  , oldlunux
  {$endif}
  ;


(** @defgroup host ENet host functions
    @{
*)

(** Creates a host for communicating to peers.

    @param address   the address at which other peers may connect to this host.  If NULL, then no peers may connect to the host.
    @param peerCount the maximum number of peers that should be allocated for the host.
    @param channelLimit the maximum number of channels allowed; if 0, then this is equivalent to ENET_PROTOCOL_MAXIMUM_CHANNEL_COUNT
    @param incomingBandwidth downstream bandwidth of the host in bytes/second; if 0, ENet will assume unlimited bandwidth.
    @param outgoingBandwidth upstream bandwidth of the host in bytes/second; if 0, ENet will assume unlimited bandwidth.

    @returns the host on success and NULL on failure

    @remarks ENet will strategically drop packets on specific sides of a connection between hosts
    to ensure the host's bandwidth is not overwhelmed.  The bandwidth parameters also determine
    the window size of a connection which limits the amount of reliable packets that may be in transit
    at any given time.
*)
function enet_host_create (address : pENetAddress;peerCount, channelLimit : enet_size_t; incomingBandwidth : enet_uint32;outgoingBandwidth : enet_uint32):pENetHost;
var
  host : pENetHost;
  currentPeer : pENetPeer;
begin
    if (peerCount > ENET_PROTOCOL_MAXIMUM_PEER_ID) then
      begin result := nil; exit; end;

    host := pENetHost(enet_malloc (sizeof (ENetHost)));
    if host = nil then
      begin result := nil; exit; end;
    fillchar(host^,SizeOf(ENetHost) ,0);

    host ^. peers := pENetPeer( enet_malloc (peerCount * sizeof (ENetPeer)) );
    if host ^. peers = nil then
       begin
         enet_free(host);
         result:=nil;
         exit;
       end;
    fillchar (host ^. peers^, peerCount * sizeof (ENetPeer), 0);

    host ^. socket := enet_socket_create (ENET_SOCKET_TYPE_DATAGRAM);
    if (host ^. socket = ENET_SOCKET_NULL) or ((address <> nil) and (enet_socket_bind (host ^. socket, address) < 0)) then
    begin
       if (host ^. socket <> ENET_SOCKET_NULL) then
           enet_socket_destroy (host ^. socket);

       enet_free (host ^. peers);
       enet_free (host);

       result := nil; exit;
    end;

    enet_socket_set_option (host ^. socket, ENET_SOCKOPT_NONBLOCK, 1);
    enet_socket_set_option (host ^. socket, ENET_SOCKOPT_BROADCAST, 1);
    enet_socket_set_option (host ^. socket, ENET_SOCKOPT_RCVBUF, ENET_HOST_RECEIVE_BUFFER_SIZE);
    enet_socket_set_option (host ^. socket, ENET_SOCKOPT_SNDBUF, ENET_HOST_SEND_BUFFER_SIZE);

    if ((address <> nil) and (enet_socket_get_address (host ^. socket, @host ^. address) < 0)) then
      host ^. address := address^;

    if ((channelLimit=0) or (channelLimit > ENET_PROTOCOL_MAXIMUM_CHANNEL_COUNT)) then
      channelLimit := ENET_PROTOCOL_MAXIMUM_CHANNEL_COUNT
    else
    if (channelLimit < ENET_PROTOCOL_MINIMUM_CHANNEL_COUNT) then
      channelLimit := ENET_PROTOCOL_MINIMUM_CHANNEL_COUNT;

    host ^. randomSeed := enet_uint32(host);
    host ^. randomSeed := host ^. randomSeed + enet_host_random_seed ();
    host ^. randomSeed := (host ^. randomSeed shl 16) or (host ^. randomSeed shr 16);
    host ^. channelLimit := channelLimit;
    host ^. incomingBandwidth := incomingBandwidth;
    host ^. outgoingBandwidth := outgoingBandwidth;
    host ^. bandwidthThrottleEpoch := 0;
    host ^. recalculateBandwidthLimits := 0;
    host ^. mtu := ENET_HOST_DEFAULT_MTU;
    host ^. peerCount := peerCount;
    host ^. commandCount := 0;
    host ^. bufferCount := 0;
    host ^. checksumfunc := nil;
    host ^. receivedAddress.host := ENET_HOST_ANY;
    host ^. receivedAddress.port := 0;
    host ^. receivedData := nil;
    host ^. receivedDataLength := 0;

    host ^. totalSentData := 0;
    host ^. totalSentPackets := 0;
    host ^. totalReceivedData := 0;
    host ^. totalReceivedPackets := 0;

    host ^. connectedPeers := 0;
    host ^. bandwidthLimitedPeers := 0;
    host ^. duplicatePeers := ENET_PROTOCOL_MAXIMUM_PEER_ID;
    host ^. maximumPacketSize := ENET_HOST_DEFAULT_MAXIMUM_PACKET_SIZE;
    host ^. maximumWaitingData := ENET_HOST_DEFAULT_MAXIMUM_WAITING_DATA;

    host ^. compressor.context := nil;
    host ^. compressor.compress := nil;
    host ^. compressor.decompress := nil;
    host ^. compressor.destroy := nil;

    host ^. interceptfunc := nil;

    enet_list_clear ( @ host ^. dispatchQueue);

    currentPeer := host ^. peers;
    while PAnsiChar(currentPeer)< PAnsiChar(@ pENetPeerArray(host ^. peers)^[host ^. peerCount] ) do
    begin
       currentPeer ^. host := host;
       currentPeer ^. incomingPeerID := (PAnsiChar(currentPeer)- PAnsiChar(host ^. peers)) div sizeof(ENetPeer);
       currentPeer ^. outgoingSessionID := $0FF;
       currentPeer ^. incomingSessionID := $0FF;
       currentPeer ^. data := nil;

       enet_list_clear (@currentPeer ^. acknowledgements);
       enet_list_clear (@currentPeer ^. sentReliableCommands);
       enet_list_clear (@currentPeer ^. sentUnreliableCommands);
       enet_list_clear (@currentPeer ^. outgoingReliableCommands);
       enet_list_clear (@currentPeer ^. outgoingUnreliableCommands);
       enet_list_clear (@currentPeer ^. dispatchedCommands);

       enet_peer_reset (currentPeer);

       inc(currentPeer);
    end;

    result := host;
end;

(** Destroys the host and all resources associated with it.
    @param host pointer to the host to destroy
*)
procedure enet_host_destroy (host : pENetHost);
var
    currentPeer : pENetPeer;
begin
    if host=nil then exit;

    enet_socket_destroy (host ^. socket);

    currentPeer := host ^. peers;
    while PAnsiChar(currentPeer) < PAnsiChar(@ pENetPeerArray(host ^. peers)^[host ^. peerCount]) do
    begin
       enet_peer_reset (currentPeer);
       inc(currentPeer);
    end;

    if ((host ^. compressor.context <> nil) and (@host ^. compressor.destroy<>nil)) then
               host ^. compressor.destroy(host ^. compressor.context);


    enet_free (host ^. peers);
    enet_free (host);
end;

(** Initiates a connection to a foreign host.
    @param host host seeking the connection
    @param address destination for the connection
    @param channelCount number of channels to allocate
    @param data user data supplied to the receiving host
    @returns a peer representing the foreign host on success, NULL on failure
    @remarks The peer returned will have not completed the connection until enet_host_service()
    notifies of an ENET_EVENT_TYPE_CONNECT event for the peer.
*)
function enet_host_connect (host : pENetHost; address : pENetAddress; channelCount : enet_size_t; data : enet_uint32):pENetPeer;
var
    currentPeer : pENetPeer;
    channel : pENetChannel;
    command : ENetProtocol;
begin

    if (channelCount < ENET_PROTOCOL_MINIMUM_CHANNEL_COUNT) then
      channelCount := ENET_PROTOCOL_MINIMUM_CHANNEL_COUNT
    else
    if (channelCount > ENET_PROTOCOL_MAXIMUM_CHANNEL_COUNT) then
      channelCount := ENET_PROTOCOL_MAXIMUM_CHANNEL_COUNT;

    currentPeer := host ^. peers;
    while PAnsiChar(currentPeer) < PAnsiChar(@ pENetPeerArray(host ^. peers)^[host ^. peerCount]) do
    begin
       if (currentPeer ^. state = ENET_PEER_STATE_DISCONNECTED) then
         break;
       inc(currentPeer);
    end;

    if PAnsiChar(currentPeer) >= PAnsiChar(@ pENetPeerArray(host ^. peers)^[host ^. peerCount]) then
      begin result := nil; exit; end;

    currentPeer ^. channels := pENetChannel( enet_malloc (channelCount * sizeof (ENetChannel)) );
    if (currentPeer ^. channels = nil) then begin
       result:=nil; exit;
    end;

    currentPeer ^. channelCount := channelCount;
    currentPeer ^. state := ENET_PEER_STATE_CONNECTING;
    currentPeer ^. address := address^;
    Inc(host);
    currentPeer ^. connectID := host ^. randomSeed;


    if (host ^. outgoingBandwidth = 0) then
      currentPeer ^. windowSize := ENET_PROTOCOL_MAXIMUM_WINDOW_SIZE
    else
      currentPeer ^. windowSize := (host ^. outgoingBandwidth div
                                    ENET_PEER_WINDOW_SIZE_SCALE) *
                                      ENET_PROTOCOL_MINIMUM_WINDOW_SIZE;

    if (currentPeer ^. windowSize < ENET_PROTOCOL_MINIMUM_WINDOW_SIZE) then
      currentPeer ^. windowSize := ENET_PROTOCOL_MINIMUM_WINDOW_SIZE
    else
    if (currentPeer ^. windowSize > ENET_PROTOCOL_MAXIMUM_WINDOW_SIZE) then
      currentPeer ^. windowSize := ENET_PROTOCOL_MAXIMUM_WINDOW_SIZE;

    channel := currentPeer ^. channels;
    while PAnsiChar(channel) < PAnsiChar(@ pENetChannelArray(currentPeer ^. channels)^[channelCount]) do
    begin
        channel ^. outgoingReliableSequenceNumber := 0;
        channel ^. outgoingUnreliableSequenceNumber := 0;
        channel ^. incomingReliableSequenceNumber := 0;
        channel ^. incomingUnreliableSequenceNumber := 0;

        enet_list_clear (@ channel ^. incomingReliableCommands);
        enet_list_clear (@ channel ^. incomingUnreliableCommands);

        channel ^. usedReliableWindows := 0;
        fillchar(channel ^. reliableWindows, sizeof (channel ^. reliableWindows), 0);

        inc(channel);
    end;
        
    command.header.command := ENET_PROTOCOL_COMMAND_CONNECT or ENET_PROTOCOL_COMMAND_FLAG_ACKNOWLEDGE;
    command.header.channelID := $FF;
    command.connect.outgoingPeerID := ENET_HOST_TO_NET_16 (currentPeer ^. incomingPeerID);
    command.connect.incomingSessionID := currentPeer ^. incomingSessionID;
    command.connect.outgoingSessionID := currentPeer ^. outgoingSessionID;
    command.connect.mtu := ENET_HOST_TO_NET_32 (currentPeer ^. mtu);
    command.connect.windowSize := ENET_HOST_TO_NET_32 (currentPeer ^. windowSize);
    command.connect.channelCount := ENET_HOST_TO_NET_32 (channelCount);
    command.connect.incomingBandwidth := ENET_HOST_TO_NET_32 (host ^. incomingBandwidth);
    command.connect.outgoingBandwidth := ENET_HOST_TO_NET_32 (host ^. outgoingBandwidth);
    command.connect.packetThrottleInterval := ENET_HOST_TO_NET_32 (currentPeer ^. packetThrottleInterval);
    command.connect.packetThrottleAcceleration := ENET_HOST_TO_NET_32 (currentPeer ^. packetThrottleAcceleration);
    command.connect.packetThrottleDeceleration := ENET_HOST_TO_NET_32 (currentPeer ^. packetThrottleDeceleration);
    command.connect.connectID := currentPeer ^. connectID;
    command.connect.data := ENET_HOST_TO_NET_32 (data);

    
    enet_peer_queue_outgoing_command (currentPeer, @ command, nil, 0, 0);

    result := currentPeer;
end;

(** Queues a packet to be sent to all peers associated with the host.
    @param host host on which to broadcast the packet
    @param channelID channel on which to broadcast
    @param packet packet to broadcast
*)
procedure enet_host_broadcast (host : pENetHost; channelID : enet_uint8; packet : pENetPacket);
var
    currentPeer : pENetPeer;
label continuework;
begin

    currentPeer := host ^. peers;
    while PAnsiChar(currentPeer) < PAnsiChar(@ pENetPeerArray(host ^. peers)^[host ^. peerCount]) do
    begin
       if (currentPeer ^. state <> ENET_PEER_STATE_CONNECTED) then
         goto continuework;

       enet_peer_send (currentPeer, channelID, packet);
continuework:
       inc(currentPeer);
    end;

    if (packet ^. referenceCount = 0) then
      enet_packet_destroy (packet);
end;

(** Sets the packet compressor the host should use to compress and decompress packets.
    @param host host to enable or disable compression for
    @param compressor callbacks for for the packet compressor; if NULL, then compression is disabled
*)
procedure enet_host_compress (host:pENetHost; compressor:pENetCompressor);
begin
    if ((host ^. compressor.context <> nil) and (@ host ^. compressor.destroy<>nil)) then
      host ^.compressor.destroy(host ^.compressor.context);

    if (compressor <> nil) then
      host ^. compressor := compressor^
    else
      host ^. compressor.context := nil;
end;

(** Limits the maximum allowed channels of future incoming connections.
    @param host host to limit
    @param channelLimit the maximum number of channels allowed; if 0, then this is equivalent to ENET_PROTOCOL_MAXIMUM_CHANNEL_COUNT
*)
procedure enet_host_channel_limit (host : pENetHost; channelLimit: enet_size_t);
begin
    if ((channelLimit=0) or (channelLimit > ENET_PROTOCOL_MAXIMUM_CHANNEL_COUNT)) then
      channelLimit := ENET_PROTOCOL_MAXIMUM_CHANNEL_COUNT
    else
    if (channelLimit < ENET_PROTOCOL_MINIMUM_CHANNEL_COUNT) then
      channelLimit := ENET_PROTOCOL_MINIMUM_CHANNEL_COUNT;

    host ^. channelLimit := channelLimit;
end;

(** Adjusts the bandwidth limits of a host.
    @param host host to adjust
    @param incomingBandwidth new incoming bandwidth
    @param outgoingBandwidth new outgoing bandwidth
    @remarks the incoming and outgoing bandwidth parameters are identical in function to those
    specified in enet_host_create().
*)
procedure enet_host_bandwidth_limit (host : pENetHost;incomingBandwidth : enet_uint32; outgoingBandwidth : enet_uint32);
begin
    host ^. incomingBandwidth := incomingBandwidth;
    host ^. outgoingBandwidth := outgoingBandwidth;
    host ^. recalculateBandwidthLimits := 1;
end;

procedure enet_host_bandwidth_throttle (host : pENetHost);
var
  timeCurrent, elapsedTime, peersTotal,
  dataTotal, peersRemaining, bandwidth,
  throttle, bandwidthLimit : enet_uint32;
  needsAdjustment : integer;
  peer : pENetPeer;
  command : ENetProtocol;
  peerBandwidth : enet_uint32;
label continuework1, continuework2, continuework3, continuework4, continuework5;
begin
    timeCurrent := enet_time_get ();
    elapsedTime := timeCurrent - host ^. bandwidthThrottleEpoch;
    peersRemaining := enet_uint32( host ^. connectedPeers);
    dataTotal := enet_uint32(not 0);
    bandwidth := enet_uint32(not 0);
    throttle := 0;
    bandwidthLimit := 0;
    if host ^. bandwidthLimitedPeers > 0 then
       needsAdjustment:=1
       else
         needsAdjustment:=0;

    if (elapsedTime < ENET_HOST_BANDWIDTH_THROTTLE_INTERVAL) then exit;

    host ^. bandwidthThrottleEpoch := timeCurrent;

    if (peersRemaining = 0) then
      exit;

    if (host ^. outgoingBandwidth <> 0) then
    begin
        dataTotal := 0;
        bandwidth := (host ^. outgoingBandwidth * elapsedTime) div 1000;

        peer := host ^. peers;
        while pchar(peer) < pchar(@ pENetPeerArray(host ^. peers)^[host ^. peerCount]) do
        begin
            if ((peer ^. state <> ENET_PEER_STATE_CONNECTED) and (peer ^. state <> ENET_PEER_STATE_DISCONNECT_LATER)) then
              goto continuework1;
            Inc(dataTotal, peer ^. outgoingDataTotal);
continuework1:
            Inc(peer);
        end;
    end;

    while (peersRemaining > 0) and (needsAdjustment <> 0) do
    begin
        needsAdjustment := 0;

        if (dataTotal <= bandwidth) then
          throttle := ENET_PEER_PACKET_THROTTLE_SCALE
        else
          throttle := (bandwidth * ENET_PEER_PACKET_THROTTLE_SCALE) div dataTotal;

        peer := host ^. peers;
        while PAnsiChar(peer) < PAnsiChar(@ pENetPeerArray(host ^. peers)^[host ^. peerCount]) do
        begin

            if ((peer ^. state <> ENET_PEER_STATE_CONNECTED) and (peer ^. state <> ENET_PEER_STATE_DISCONNECT_LATER)) or
                (peer ^. incomingBandwidth = 0) or
                (peer ^. outgoingBandwidthThrottleEpoch = timeCurrent) then
              goto continuework2;

            peerBandwidth := (peer ^. incomingBandwidth * elapsedTime) div 1000;
            if ((throttle * peer ^. outgoingDataTotal) div ENET_PEER_PACKET_THROTTLE_SCALE <= peerBandwidth) then
              goto continuework2;

            peer ^. packetThrottleLimit := (peerBandwidth *
                                            ENET_PEER_PACKET_THROTTLE_SCALE) div peer ^. outgoingDataTotal;

            if (peer ^. packetThrottleLimit = 0) then
              peer ^. packetThrottleLimit := 1;

            if (peer ^. packetThrottle > peer ^. packetThrottleLimit) then
              peer ^. packetThrottle := peer ^. packetThrottleLimit;

            peer ^. outgoingBandwidthThrottleEpoch := timeCurrent;

            peer ^. incomingDataTotal := 0;
            peer ^. outgoingDataTotal := 0;

            needsAdjustment := 1;
            dec(peersRemaining);
            dec(bandwidth, peerBandwidth);
            dec(dataTotal,peerBandwidth);
continuework2:
            inc(peer);
        end;
    end;

    if (peersRemaining > 0) then begin
      if (dataTotal <= bandwidth) then
         throttle := ENET_PEER_PACKET_THROTTLE_SCALE
       else
         throttle := (bandwidth * ENET_PEER_PACKET_THROTTLE_SCALE) div dataTotal;

      peer := host ^. peers;
      while PChar(peer) < PChar(@ pENetPeerArray(host ^. peers)^[host ^. peerCount]) do
      begin
           if ((peer ^. state <> ENET_PEER_STATE_CONNECTED) and (peer ^. state <> ENET_PEER_STATE_DISCONNECT_LATER)) or
               (peer ^. outgoingBandwidthThrottleEpoch = timeCurrent) then
             goto continuework3;

           peer ^. packetThrottleLimit := throttle;

           if (peer ^. packetThrottle > peer ^. packetThrottleLimit) then
             peer ^. packetThrottle := peer ^. packetThrottleLimit;

           peer ^. incomingDataTotal := 0;
           peer ^. outgoingDataTotal := 0;
continuework3:
           Inc(peer);
      end;
    end;

    if host ^. recalculateBandwidthLimits<>0 then
    begin
       host ^. recalculateBandwidthLimits := 0;

       peersRemaining := enet_uint32(host ^. connectedPeers);
       bandwidth := host ^. incomingBandwidth;
       needsAdjustment := 1;

       if (bandwidth = 0) then
         bandwidthLimit := 0
       else
       while (peersRemaining > 0) and (needsAdjustment <> 0) do
       begin
           needsAdjustment := 0;
           bandwidthLimit := bandwidth div peersRemaining;


           peer := host ^. peers;
           while PAnsiChar(peer) < PAnsiChar(@ pENetPeerArray(host ^. peers)^[host ^. peerCount]) do
           begin
               if ((peer ^. state <> ENET_PEER_STATE_CONNECTED) and (peer ^. state <> ENET_PEER_STATE_DISCONNECT_LATER)) or
                   (peer ^. incomingBandwidthThrottleEpoch = timeCurrent) then
                 goto continuework4;

               if (peer ^. outgoingBandwidth > 0) and
                   (peer ^. outgoingBandwidth >= bandwidthLimit) then
                 goto continuework4;

               peer ^. incomingBandwidthThrottleEpoch := timeCurrent;

               needsAdjustment := 1;
               dec(peersRemaining);
               dec(bandwidth,peer ^. outgoingBandwidth);
continuework4:
               inc(peer);
           end;
       end;

       peer := host ^. peers;
       while PAnsiChar(peer) < PAnsiChar(@ pENetPeerArray(host ^. peers)^[host ^. peerCount]) do
       begin
           if (peer ^. state <> ENET_PEER_STATE_CONNECTED) and (peer ^. state <> ENET_PEER_STATE_DISCONNECT_LATER) then
             goto continuework5;

           command.header.command := ENET_PROTOCOL_COMMAND_BANDWIDTH_LIMIT or ENET_PROTOCOL_COMMAND_FLAG_ACKNOWLEDGE;
           command.header.channelID := $FF;
           command.bandwidthLimit.outgoingBandwidth := ENET_HOST_TO_NET_32 (host ^. outgoingBandwidth);

           if (peer ^. incomingBandwidthThrottleEpoch = timeCurrent) then
             command.bandwidthLimit.incomingBandwidth := ENET_HOST_TO_NET_32 (peer ^. outgoingBandwidth)
           else
             command.bandwidthLimit.incomingBandwidth := ENET_HOST_TO_NET_32 (bandwidthLimit);

           enet_peer_queue_outgoing_command (peer, @ command, nil, 0, 0);
continuework5:
           inc(peer);
       end;
    end;
end;

(** @} *)


end.

