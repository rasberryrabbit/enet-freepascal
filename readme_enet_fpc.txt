
enet 1.3.12 to Freepascal
-------------------------

UDP C library ENet 1.3.12 to freepascal source translation.
I also added a simple class wrapper unit for freepascal.

Tested on windows 32 bit, also supports linux (simple test on linux mint x86-64).

It supports only IPv4, IPv6 is unsupported.

Special thanks to Lee Salzman.

For more information about ENet
visit http://enet.bespin.org


---------
file list
---------
enet_callbacks.pas
enet_consts.pas
enet_host.pas
enet_list.pas
enet_packet.pas
enet_peer.pas
enet_protocol.pas
enet_socket.pas (1)

(1) If you are using it to other oses/environments, you should modify this source unit. You will need to change the time and socket functions.

----------
changes
----------
2015/04/21
- win32/linux works ok.
- fix some alignment problem.
- works on linux.

- initial work, with ENet 1.3.6.
- add SendMsgEventPeer
