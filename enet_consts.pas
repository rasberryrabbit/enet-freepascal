unit enet_consts;

(*
  enet freepascal conversion constant and some functions


  1.3.12 freepascal

  - fix time function
  - add missing commnet in Packet Flag
  - fix shift operator on constants
  - fix decaration of invalid socket
*)

// for C/C++ interface compatiblity. but useless
{.$DEFINE PACK}

interface

uses
{$ifdef WINDOWS}
  WinSock2
{$else}
  Sockets
{$endif}
 ;

type
// types.h <-----
  enet_uint32 = longword;
  enet_uint16 = word;
  enet_uint8  = byte;
// -----> types.h
  penet_uint32 = ^enet_uint32;
  penet_uint16 = ^enet_uint16;
  penet_uint8  = pbyte;
  // for pointer access
  ppenet_uint8 = ^penet_uint8;
  enet_uint8array = array[0..0] of enet_uint8;
  penet_uint8array = ^enet_uint8array;
  enet_size_t = longword;
  ENetProtocolCommand = integer;

var
// enet.h <-----
  ENetVersion : enet_uint32;
// -----> enet.h

const
// time.h <-----
   ENET_TIME_OVERFLOW = 86400000;
// -----> time.h

// enet.h <-----
   ENET_VERSION_MAJOR = 1;
   ENET_VERSION_MINOR = 3;
   ENET_VERSION_PATCH = 12;
   ENET_VERSION = (ENET_VERSION_MAJOR shl 16) or (ENET_VERSION_MINOR shl 8) or ENET_VERSION_PATCH;
   //ENetVersion;

   ENET_SOCKET_TYPE_STREAM   = 1;
   ENET_SOCKET_TYPE_DATAGRAM = 2;
   //ENetSocketType;

   ENET_SOCKET_WAIT_NONE    = 0;
   ENET_SOCKET_WAIT_SEND    = (1 shl 0);
   ENET_SOCKET_WAIT_RECEIVE = (1 shl 1);
   ENET_SOCKET_WAIT_INTERRUPT = (1 shl 2);
   //ENetSocketWait;

   ENET_SOCKOPT_NONBLOCK  = 1;
   ENET_SOCKOPT_BROADCAST = 2;
   ENET_SOCKOPT_RCVBUF    = 3;
   ENET_SOCKOPT_SNDBUF    = 4;
   ENET_SOCKOPT_REUSEADDR = 5;
   ENET_SOCKOPT_RCVTIMEO  = 6;
   ENET_SOCKOPT_SNDTIMEO  = 7;
   ENET_SOCKOPT_ERROR     = 8;
   ENET_SOCKOPT_NODELAY   = 9;
   //ENetSocketOption;

   ENET_SOCKET_SHUTDOWN_READ       = 0;
   ENET_SOCKET_SHUTDOWN_WRITE      = 1;
   ENET_SOCKET_SHUTDOWN_READ_WRITE = 2;
   //ENetSocketShutdown

(**
 * Portable internet address structure.
 *
 * The host must be specified in network byte-order, and the port must be in host
 * byte-order. The constant ENET_HOST_ANY may be used to specify the default
 * server host. The constant ENET_HOST_BROADCAST may be used to specify the
 * broadcast address (255.255.255.255).  This makes sense for enet_host_connect,
 * but not for enet_host_create.  Once a server responds to a broadcast, the
 * address is updated from ENET_HOST_BROADCAST to the server's actual IP address.
 *)


   ENET_HOST_ANY       = 0;            (**< specifies the default server host *)
   ENET_HOST_BROADCAST_= longword($FFFFFFFF);    (**< specifies a subnet-wide broadcast *)

   ENET_PORT_ANY       = 0;            (**< specifies that a port should be automatically chosen *)


(**
 * Packet flag bit constants.
 *
 * The host must be specified in network byte-order, and the port must be in
 * host byte-order. The constant ENET_HOST_ANY may be used to specify the
 * default server host.

   @sa ENetPacket
*)

   (** packet must be received by the target peer and resend attempts should be
     * made until the packet is delivered *)
   ENET_SOCKET_NULL = Tsocket({$ifdef WINDOWS}INVALID_SOCKET{$else}-1{$endif});

   (** packet must be received by the target peer and resend attempts should be
     * made until the packet is delivered *)
   ENET_PACKET_FLAG_RELIABLE    = (1 shl 0);
   (** packet will not be sequenced with other packets
     * not supported for reliable packets
     *)
   ENET_PACKET_FLAG_UNSEQUENCED = (1 shl 1);
   (** packet will not allocate data, and user must supply it instead *)
   ENET_PACKET_FLAG_NO_ALLOCATE = (1 shl 2);
   (** packet will be fragmented using unreliable (instead of reliable) sends
     * if it exceeds the MTU *)
   ENET_PACKET_FLAG_UNRELIABLE_FRAGMENT = (1 shl 3);
   (** whether the packet has been sent from all queues it has been entered into *)
   ENET_PACKET_FLAG_SENT = (1 shl 8);
   //ENetPacketFlag
// -----> enet.h

// protocol.h <-----
   ENET_PROTOCOL_MINIMUM_MTU             = 576;
   ENET_PROTOCOL_MAXIMUM_MTU             = 4096;
   ENET_PROTOCOL_MAXIMUM_PACKET_COMMANDS = 32;
   ENET_PROTOCOL_MINIMUM_WINDOW_SIZE     = 4096;
   ENET_PROTOCOL_MAXIMUM_WINDOW_SIZE     = 65536;
   ENET_PROTOCOL_MINIMUM_CHANNEL_COUNT   = 1;
   ENET_PROTOCOL_MAXIMUM_CHANNEL_COUNT   = 255;
   ENET_PROTOCOL_MAXIMUM_PEER_ID         = $0FFF;
   // ENET_PROTOCOL_MAXIMUM_PACKET_SIZE     = 1024 * 1024 * 1024;
   ENET_PROTOCOL_MAXIMUM_FRAGMENT_COUNT  = 1024 * 1024;
   //

   ENET_PROTOCOL_COMMAND_NONE               = 0;
   ENET_PROTOCOL_COMMAND_ACKNOWLEDGE        = 1;
   ENET_PROTOCOL_COMMAND_CONNECT            = 2;
   ENET_PROTOCOL_COMMAND_VERIFY_CONNECT     = 3;
   ENET_PROTOCOL_COMMAND_DISCONNECT         = 4;
   ENET_PROTOCOL_COMMAND_PING               = 5;
   ENET_PROTOCOL_COMMAND_SEND_RELIABLE      = 6;
   ENET_PROTOCOL_COMMAND_SEND_UNRELIABLE    = 7;
   ENET_PROTOCOL_COMMAND_SEND_FRAGMENT      = 8;
   ENET_PROTOCOL_COMMAND_SEND_UNSEQUENCED   = 9;
   ENET_PROTOCOL_COMMAND_BANDWIDTH_LIMIT    = 10;
   ENET_PROTOCOL_COMMAND_THROTTLE_CONFIGURE = 11;
   ENET_PROTOCOL_COMMAND_SEND_UNRELIABLE_FRAGMENT = 12;
   ENET_PROTOCOL_COMMAND_COUNT              = 13;

   ENET_PROTOCOL_COMMAND_MASK               = $0F;
   //ENetProtocolCommand

   ENET_PROTOCOL_COMMAND_FLAG_ACKNOWLEDGE = (1 shl 7);
   ENET_PROTOCOL_COMMAND_FLAG_UNSEQUENCED = (1 shl 6);

   ENET_PROTOCOL_HEADER_FLAG_COMPRESSED = (1 shl 14);
   ENET_PROTOCOL_HEADER_FLAG_SENT_TIME = (1 shl 15);
   ENET_PROTOCOL_HEADER_FLAG_MASK       = ENET_PROTOCOL_HEADER_FLAG_COMPRESSED or ENET_PROTOCOL_HEADER_FLAG_SENT_TIME;

   ENET_PROTOCOL_HEADER_SESSION_MASK    = (3 shl 12);
   ENET_PROTOCOL_HEADER_SESSION_SHIFT   = 12;
   //ENetProtocolFlag
// -----> protocol.h

// enet.h <-----
(**
 * An ENet event type, as specified in @ref ENetEvent.
 *)

   (** no event occurred within the specified time limit *)
   ENET_EVENT_TYPE_NONE       = 0;

   (** a connection request initiated by enet_host_connect has completed.
     * The peer field contains the peer which successfully connected.
     *)
   ENET_EVENT_TYPE_CONNECT    = 1;

   (** a peer has disconnected.  This event is generated on a successful
     * completion of a disconnect initiated by enet_pper_disconnect, if
     * a peer has timed out, or if a connection request intialized by
     * enet_host_connect has timed out.  The peer field contains the peer
     * which disconnected. The data field contains user supplied data
     * describing the disconnection, or 0, if none is available.
     *)
   ENET_EVENT_TYPE_DISCONNECT = 2;

   (** a packet has been received from a peer.  The peer field specifies the
     * peer which sent the packet.  The channelID field specifies the channel
     * number upon which the packet was received.  The packet field contains
     * the packet that was received; this packet must be destroyed with
     * enet_packet_destroy after use.
     *)
   ENET_EVENT_TYPE_RECEIVE    = 3;
   //ENetEventType;

   ENET_PEER_STATE_DISCONNECTED                = 0;
   ENET_PEER_STATE_CONNECTING                  = 1;
   ENET_PEER_STATE_ACKNOWLEDGING_CONNECT       = 2;
   ENET_PEER_STATE_CONNECTION_PENDING          = 3;
   ENET_PEER_STATE_CONNECTION_SUCCEEDED        = 4;
   ENET_PEER_STATE_CONNECTED                   = 5;
   ENET_PEER_STATE_DISCONNECT_LATER            = 6;
   ENET_PEER_STATE_DISCONNECTING               = 7;
   ENET_PEER_STATE_ACKNOWLEDGING_DISCONNECT    = 8;
   ENET_PEER_STATE_ZOMBIE                      = 9;
   //ENetPeerState;

//#ifndef ENET_BUFFER_MAXIMUM
//#define ENET_BUFFER_MAXIMUM (1 + 2 * ENET_PROTOCOL_MAXIMUM_PACKET_COMMANDS)
   ENET_BUFFER_MAXIMUM = (1 + 2 * ENET_PROTOCOL_MAXIMUM_PACKET_COMMANDS);
//#endif

   ENET_HOST_RECEIVE_BUFFER_SIZE          = 256 * 1024;
   ENET_HOST_SEND_BUFFER_SIZE             = 256 * 1024;
   ENET_HOST_BANDWIDTH_THROTTLE_INTERVAL  = 1000;
   ENET_HOST_DEFAULT_MTU                  = 1400;
   ENET_HOST_DEFAULT_MAXIMUM_PACKET_SIZE  = 32 * 1024 * 1024;
   ENET_HOST_DEFAULT_MAXIMUM_WAITING_DATA = 32 * 1024 * 1024;

   ENET_PEER_DEFAULT_ROUND_TRIP_TIME      = 500;
   ENET_PEER_DEFAULT_PACKET_THROTTLE      = 32;
   ENET_PEER_PACKET_THROTTLE_SCALE        = 32;
   ENET_PEER_PACKET_THROTTLE_COUNTER      = 7;
   ENET_PEER_PACKET_THROTTLE_ACCELERATION = 2;
   ENET_PEER_PACKET_THROTTLE_DECELERATION = 2;
   ENET_PEER_PACKET_THROTTLE_INTERVAL     = 5000;
   ENET_PEER_PACKET_LOSS_SCALE            = (1 shl 16);
   ENET_PEER_PACKET_LOSS_INTERVAL         = 10000;
   ENET_PEER_WINDOW_SIZE_SCALE            = 64 * 1024;
   ENET_PEER_TIMEOUT_LIMIT                = 32;
   ENET_PEER_TIMEOUT_MINIMUM              = 5000;
   ENET_PEER_TIMEOUT_MAXIMUM              = 30000;
   ENET_PEER_PING_INTERVAL_                = 500;
   ENET_PEER_UNSEQUENCED_WINDOWS          = 64;
   ENET_PEER_UNSEQUENCED_WINDOW_SIZE      = 1024;
   ENET_PEER_FREE_UNSEQUENCED_WINDOWS     = 32;
   ENET_PEER_RELIABLE_WINDOWS             = 16;
   ENET_PEER_RELIABLE_WINDOW_SIZE         = $1000;
   ENET_PEER_FREE_RELIABLE_WINDOWS        = 8;

// enet.h <-----
type
  pENetPacket = ^ENetPacket;

  ENETPacketFreeCallback = procedure (Packet : pENetPacket); stdcall;

(**
 * ENet packet structure.
 *
 * An ENet data packet that may be sent to or received from a peer. The shown
 * fields should only be read and never modified. The data field contains the
 * allocated data for the packet. The dataLength fields specifies the length
 * of the allocated data.  The flags field is either 0 (specifying no flags),
 * or a bitwise-or of any combination of the following flags:
 *
 *    ENET_PACKET_FLAG_RELIABLE - packet must be received by the target peer
 *    and resend attempts should be made until the packet is delivered
 *
 *    ENET_PACKET_FLAG_UNSEQUENCED - packet will not be sequenced with other packets
 *    (not supported for reliable packets)
 *
 *    ENET_PACKET_FLAG_NO_ALLOCATE - packet will not allocate data, and user must supply it instead

   @sa ENetPacketFlag
 *)
 // -----> enet.h

// callbacks.h <-----
  ENETCALLBACK_malloc = function (size : longword):pointer; stdcall;
  ENETCALLBACK_free   = procedure (ptr : pointer); stdcall;
  ENETCALLBACK_nomemory   = procedure; stdcall;
  pENetCallbacks = ^ENetCallbacks;
  ENetCallbacks = record
    malloc : ENETCALLBACK_malloc;
    free : ENETCALLBACK_free;
    nomemory : ENETCALLBACK_nomemory;
  end;
// -----> callbacks.h

  pENetAddress = ^ENetAddress;
  ENetAddress = record
    host : enet_uint32;
    port : enet_uint16;
  end;

// list.h <-----
  pENetListNode = ^ENetListNode;
  ENetListNode = record
   next : pENetListNode;
   previous : pENetListNode;
  end;

  ENetListIterator = pENetListNode;

  pENetList = ^ENetList;
  ENetList = record
   sentinel : ENetListNode;
  end;
// -----> list.h

{$push}
{$align 1}
// protocol.h <-----
  pENetProtocolHeader = ^ENetProtocolHeader;
  ENetProtocolHeader = record
   peerID : enet_uint16;
   sentTime : enet_uint16;
  end;

  ENetProtocolCommandHeader = record
   command :enet_uint8;
   channelID :enet_uint8;
   reliableSequenceNumber : enet_uint16;
  end;

  ENetProtocolAcknowledge = record
   header : ENetProtocolCommandHeader;
   receivedReliableSequenceNumber : enet_uint16;
   receivedSentTime : enet_uint16;
  end;

  ENetProtocolConnect = record
   header : ENetProtocolCommandHeader;
   outgoingPeerID : enet_uint16;
   incomingSessionID : enet_uint8;
   outgoingSessionID : enet_uint8;
   mtu : enet_uint32;
   windowSize : enet_uint32;
   channelCount : enet_uint32;
   incomingBandwidth : enet_uint32;
   outgoingBandwidth : enet_uint32;
   packetThrottleInterval: enet_uint32;
   packetThrottleAcceleration : enet_uint32;
   packetThrottleDeceleration : enet_uint32;
   connectID : enet_uint32;
   data : enet_uint32;
  end;

  ENetProtocolVerifyConnect = record
   header : ENetProtocolCommandHeader;
   outgoingPeerID : enet_uint16;
   incomingSessionID : enet_uint8;
   outgoingSessionID : enet_uint8;
   mtu : enet_uint32;
   windowSize : enet_uint32;
   channelCount : enet_uint32;
   incomingBandwidth : enet_uint32;
   outgoingBandwidth : enet_uint32;
   packetThrottleInterval : enet_uint32;
   packetThrottleAcceleration : enet_uint32;
   packetThrottleDeceleration : enet_uint32;
   connectID : enet_uint32;
  end;

  ENetProtocolBandwidthLimit = record
   header : ENetProtocolCommandHeader;
   incomingBandwidth : enet_uint32;
   outgoingBandwidth : enet_uint32;
  end;

  ENetProtocolThrottleConfigure = record
   header : ENetProtocolCommandHeader;
   packetThrottleInterval : enet_uint32;
   packetThrottleAcceleration : enet_uint32;
   packetThrottleDeceleration : enet_uint32;
  end;

  ENetProtocolDisconnect = record
   header : ENetProtocolCommandHeader;
   data : enet_uint32;
  end;

  ENetProtocolPing = record
   header : ENetProtocolCommandHeader;
  end;

  ENetProtocolSendReliable = record
    header : ENetProtocolCommandHeader;
    dataLength :enet_uint16;
  end;

  ENetProtocolSendUnreliable = record
   header : ENetProtocolCommandHeader;
   unreliableSequenceNumber :enet_uint16;
   dataLength :enet_uint16;
  end;

  ENetProtocolSendUnsequenced = record
   header : ENetProtocolCommandHeader;
   unsequencedGroup :enet_uint16;
   dataLength :enet_uint16;
  end;

  ENetProtocolSendFragment = record
   header : ENetProtocolCommandHeader;
   startSequenceNumber :enet_uint16;
   dataLength :enet_uint16;
   fragmentCount : enet_uint32;
   fragmentNumber : enet_uint32;
   totalLength : enet_uint32;
   fragmentOffset : enet_uint32;
  end;

  pENetProtocol = ^ENetProtocol;
  ENetProtocol = record
   case integer of
   0 : (header : ENetProtocolCommandHeader);
   1 : (acknowledge : ENetProtocolAcknowledge);
   2 : (connect : ENetProtocolConnect);
   3 : (verifyConnect : ENetProtocolVerifyConnect);
   4 : (disconnect : ENetProtocolDisconnect);
   5 : (ping : ENetProtocolPing );
   6 : (sendReliable : ENetProtocolSendReliable);
   7 : (sendUnreliable : ENetProtocolSendUnreliable);
   8 : (sendUnsequenced : ENetProtocolSendUnsequenced);
   9 : (sendFragment : ENetProtocolSendFragment);
   10: (bandwidthLimit : ENetProtocolBandwidthLimit);
   11: (throttleConfigure : ENetProtocolThrottleConfigure);
  end;
// -----> protocol.h
{$pop}

  ENetPacket = record
    referenceCount  : enet_uint32;  (**< internal use only *)
    flags           : enet_uint32;           (**< bitwise-or of ENetPacketFlag constants *)
    data            : pbyte;            (**< allocated data for packet *)
    dataLength      : enet_uint32;      (**< length of data *)
    freeCallback    : ENetPacketFreeCallback;    (**< function to be called when the packet is no longer in use *)
    userData        : Pointer;        (**< application private data, may be freely modified *)
  end;

  pENetAcknowledgement = ^ENetAcknowledgement;
  ENetAcknowledgement = record
    acknowledgementList   : ENetListNode;
    sentTime              : enet_uint32;
    command               : ENetProtocol;
  end;

  pENetOutgoingCommand= ^ENetOutgoingCommand;
  ENetOutgoingCommand = record
    outgoingCommandList     : ENetListNode;
    reliableSequenceNumber  :enet_uint16;
    unreliableSequenceNumber:enet_uint16;
    sentTime                : enet_uint32;
    roundTripTimeout        : enet_uint32;
    roundTripTimeoutLimit   : enet_uint32;
    fragmentOffset          : enet_uint32;
    fragmentLength          :enet_uint16;
    sendAttempts            :enet_uint16;
    command                 : ENetProtocol;
    packet                  : pENetPacket;
  end;

  pENetIncomingCommand = ^ENetIncomingCommand;
  ENetIncomingCommand = record
    incomingCommandList     : ENetListNode;
    reliableSequenceNumber  :enet_uint16;
    unreliableSequenceNumber:enet_uint16;
    command                 : ENetProtocol;
    fragmentCount           : enet_uint32;
    fragmentsRemaining      : enet_uint32;
    fragments               : penet_uint32;
    packet                  : pENetPacket;
  end;

// enet.h <-----
    pENetChannel = ^ENetChannel;
    ENetChannel = record
      outgoingReliableSequenceNumber    :enet_uint16;
      outgoingUnreliableSequenceNumber  :enet_uint16;
      usedReliableWindows               :enet_uint16;
      reliableWindows                   :array[0..ENET_PEER_RELIABLE_WINDOWS-1] of enet_uint16;
      incomingReliableSequenceNumber    :enet_uint16;
      incomingUnreliableSequenceNumber  :enet_uint16;
      incomingReliableCommands          : ENetList;
      incomingUnreliableCommands        : ENetList;
    end;

    ENetChannelArray = array[0..0] of ENetChannel;
    pENetChannelArray = ^ENetChannelArray;

(**
 * An ENet peer which data packets may be sent or received from.
 *
 * No fields should be modified unless otherwise specified.
 *)
  pENetHost = ^ENetHost;
  pENetPeer = ^ENetPeer;
  ENetPeer = record
   dispatchList     : ENetListNode;
   host             : pENetHost;
   outgoingPeerID   :enet_uint16;
   incomingPeerID   :enet_uint16;
   connectID        :enet_uint32;
   outgoingSessionID:enet_uint8;
   incomingSessionID:enet_uint8;
   address          : ENetAddress;            (**< Internet address of the peer *)
   data             : pointer;               (**< Application private data, may be freely modified *)
   state            : integer;    // ENetPeerState
   channels         : pENetChannel;
   channelCount     : enet_uint32;       (**< Number of channels allocated for communication with peer *)
   incomingBandwidth : enet_uint32;  (**< Downstream bandwidth of the client in bytes/second *)
   outgoingBandwidth : enet_uint32;  (**< Upstream bandwidth of the client in bytes/second *)
   incomingBandwidthThrottleEpoch : enet_uint32;
   outgoingBandwidthThrottleEpoch : enet_uint32;
   incomingDataTotal : enet_uint32;
   outgoingDataTotal : enet_uint32;
   lastSendTime     : enet_uint32;
   lastReceiveTime  : enet_uint32;
   nextTimeout      : enet_uint32;
   earliestTimeout  : enet_uint32;
   packetLossEpoch  : enet_uint32;
   packetsSent      : enet_uint32;
   packetsLost      : enet_uint32;
   packetLoss       : enet_uint32;          (**< mean packet loss of reliable packets as a ratio with respect to the constant ENET_PEER_PACKET_LOSS_SCALE *)
   packetLossVariance : enet_uint32;
   packetThrottle   : enet_uint32;
   packetThrottleLimit : enet_uint32;
   packetThrottleCounter : enet_uint32;
   packetThrottleEpoch : enet_uint32;
   packetThrottleAcceleration : enet_uint32;
   packetThrottleDeceleration : enet_uint32;
   packetThrottleInterval : enet_uint32;
   pingInterval           : enet_uint32;
   timeoutLimit           : enet_uint32;
   timeoutMinimum         : enet_uint32;
   timeoutMaximum         : enet_uint32;
   lastRoundTripTime : enet_uint32;
   lowestRoundTripTime : enet_uint32;
   lastRoundTripTimeVariance : enet_uint32;
   highestRoundTripTimeVariance : enet_uint32;
   roundTripTime : enet_uint32;            (**< mean round trip time (RTT), in milliseconds, between sending a reliable packet and receiving its acknowledgement *)
   roundTripTimeVariance : enet_uint32;
   mtu              :enet_uint32;
   windowSize       : enet_uint32;
   reliableDataInTransit : enet_uint32;
   outgoingReliableSequenceNumber :enet_uint16;
   acknowledgements : ENetList;
   sentReliableCommands : ENetList;
   sentUnreliableCommands : ENetList;
   outgoingReliableCommands : ENetList;
   outgoingUnreliableCommands : ENetList;
   dispatchedCommands         : ENetList;
   needsDispatch              : Integer;
   incomingUnsequencedGroup :enet_uint16;
   outgoingUnsequencedGroup :enet_uint16;
   unsequencedWindow : array[0..(ENET_PEER_UNSEQUENCED_WINDOW_SIZE div 32)-1] of enet_uint32;
   eventData         : enet_uint32;
   totalWaitingData  : enet_size_t;
  end;

  ENetPeerArray = array[0..0] of ENetPeer;
  pENetPeerArray = ^ENetPeerArray;

(** An ENet host for communicating with peers.
  *
  * No fields should be modified unless otherwise stated.

    @sa enet_host_create()
    @sa enet_host_destroy()
    @sa enet_host_connect()
    @sa enet_host_service()
    @sa enet_host_flush()
    @sa enet_host_broadcast()
    @sa enet_host_compress()
    @sa enet_host_compress_with_range_coder()
    @sa enet_host_channel_limit()
    @sa enet_host_bandwidth_limit()
    @sa enet_host_bandwidth_throttle()
  *)

  ENetSocket = TSocket;   // SOCKET = UINT_PTR

  pENetBuffer = ^ENetBuffer;
  ENetBuffer = record
    dataLength : enet_uint32;
    data : pointer;
  end;

  ENET_CALLBACK_Compress = function (context:Pointer; inBuffers:PEnetBuffer; inBufferCount, inLimit:enet_size_t; outData : penet_uint8; outLimit:enet_size_t):enet_size_t;
  ENET_CALLBACK_DeCompress = function (context:Pointer; inData:penet_uint8; inLimit:enet_size_t; outData:penet_uint8; outLimit:enet_size_t):enet_size_t;
  ENET_CALLBACK_Destroy = procedure (context:Pointer);
  (** An ENet packet compressor for compressing UDP packets before socket sends or receives.
   *)
  pENetCompressor = ^ENetCompressor;
  ENetCompressor = record
     (** Context data for the compressor. Must be non-NULL. *)
     context : Pointer;
     (** Compresses from inBuffers[0:inBufferCount-1], containing inLimit bytes, to outData, outputting at most outLimit bytes. Should return 0 on failure. *)
     compress : ENET_CALLBACK_Compress;
     (** Decompresses from inData, containing inLimit bytes, to outData, outputting at most outLimit bytes. Should return 0 on failure. *)
     decompress : ENET_CALLBACK_DeCompress;
     (** Destroys the context when compression is disabled or the host is destroyed. May be NULL. *)
     destroy : ENET_CALLBACK_Destroy;
  end;

{$push}
{$Align 4}
  ENetHost = record
   socket : ENetSocket;
   address : ENetAddress;                     (**< Internet address of the host *)
   incomingBandwidth : enet_uint32;           (**< downstream bandwidth of the host *)
   outgoingBandwidth : enet_uint32;           (**< upstream bandwidth of the host *)
   bandwidthThrottleEpoch : enet_uint32;
   mtu : enet_uint32;
   randomSeed : enet_uint32;
   recalculateBandwidthLimits : integer;
   peers : pENetPeer;                       (**< array of peers allocated for this host *)
   peerCount : enet_uint32;                   (**< number of peers allocated for this host *)
   channelLimit : enet_size_t;                (**< maximum number of channels allowed for connected peers *)
   serviceTime : enet_uint32;
   dispatchQueue : ENetList;
   lastServicedPeer : pENetPeer;
   continueSending : integer;
   packetSize : enet_uint32;
   headerFlags :enet_uint16;
   commands : array[0..(ENET_PROTOCOL_MAXIMUM_PACKET_COMMANDS)-1] of ENetProtocol;
   commandCount : enet_uint32;
   buffers : array[0..(ENET_BUFFER_MAXIMUM)-1] of ENetBuffer;
   bufferCount : enet_uint32;
   checksumfunc : Pointer; //ENetChecksumCallback     (**< callback the user can set to enable packet checksums for this host *)
   compressor : ENetCompressor;
   packetData : array[0..1,0..ENET_PROTOCOL_MAXIMUM_MTU-1] of enet_uint8;
   receivedAddress : ENetAddress;
   receivedData : penet_uint8;
   receivedDataLength : enet_size_t;
   totalSentData : enet_uint32;               (**< total data sent, user should reset to 0 as needed to prevent overflow *)
   totalSentPackets : enet_uint32;            (**< total UDP packets sent, user should reset to 0 as needed to prevent overflow *)
   totalReceivedData : enet_uint32;           (**< total data received, user should reset to 0 as needed to prevent overflow *)
   totalReceivedPackets : enet_uint32;        (**< total UDP packets received, user should reset to 0 as needed to prevent overflow *)
   interceptfunc : Pointer; //ENetInterceptCallback  (**< callback the user can set to intercept received raw UDP packets *)
   connectedPeers: enet_size_t;
   bandwidthLimitedPeers : enet_size_t;
   duplicatePeers : enet_size_t;              (**< optional number of allowed peers from duplicate IPs, defaults to ENET_PROTOCOL_MAXIMUM_PEER_ID *)
   maximumPacketSize : enet_size_t;           (**< the maximum allowable packet size that may be sent or received on a peer *)
   maximumWaitingData : enet_size_t;          (**< the maximum aggregate amount of buffer space a peer may use waiting for packets to be delivered *)
  end;
{$pop}

(**
 * An ENet event as returned by enet_host_service().

   @sa enet_host_service
 *)

  pENetEvent = ^ENetEvent;
  ENetEvent = record
   EventType : integer;     // ENetEventType   (**< type of the event *)
   peer : pENetPeer;      (**< peer that generated a connect, disconnect or receive event *)
   channelID :enet_uint8; (**< channel on the peer that generated the event, if appropriate *)
   data : enet_uint32;      (**< data associated with the event, if appropriate *)
   packet : pENetPacket;    (**< packet associated with the event, if appropriate *)
  end;

  (** Callback that computes the checksum of the data held in buffers[0:bufferCount-1] *)
  ENET_CALLBACK_ENetChecksumCallback = function (buffers:pENetBuffer; bufferCount:enet_size_t):enet_uint32;

  (** Callback for intercepting received raw UDP packets. Should return 1 to intercept, 0 to ignore, or -1 to propagate an error. *)
  ENET_CALLBACK_ENetInterceptCallback = function (host:pENetHost; _event:pENetEvent):Integer;


// time.h <-----
  function ENET_TIME_LESS(a, b:longword):boolean;
  function ENET_TIME_GREATER(a, b:longword):boolean;
  function ENET_TIME_LESS_EQUAL(a, b : longword):boolean;
  function ENET_TIME_GREATER_EQUAL(a, b:longword):boolean;
  function ENET_TIME_DIFFERENCE(a, b:longword):longword;
// -----> time.h

// utility.h <-----
  function ENET_MAX(x, y:longword):longword;
  function ENET_MIN(x, y:longword):longword;
// -----> utility.h


implementation

function ENET_TIME_LESS(a, b : longword):boolean;
var
  c : enet_uint32;
begin
  c := a - b;
  result := c >= ENET_TIME_OVERFLOW;
end;

function ENET_TIME_GREATER(a, b : longword):boolean;
var
  c : enet_uint32;
begin
  c := b - a;
  result := c >= ENET_TIME_OVERFLOW;
end;

function ENET_TIME_LESS_EQUAL(a, b : longword):boolean;
begin
  result := not ENET_TIME_GREATER (a, b);
end;

function ENET_TIME_GREATER_EQUAL(a, b:longword):boolean;
begin
  result := not ENET_TIME_LESS (a, b);
end;

function ENET_TIME_DIFFERENCE(a, b:longword):longword;
var
  c : enet_uint32;
begin
  c := a - b;
  if c >= ENET_TIME_OVERFLOW then result := b - a
     else result := c;
end;

function ENET_MAX(x, y:longword):longword;
begin
 result := y;
 if x > y then result := x;
end;

function ENET_MIN(x, y:longword):longword;
begin
 result := y;
 if x < y then result := x;
end;



end.
