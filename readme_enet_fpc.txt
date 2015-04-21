
enet 1.3.12 to Freepascal
-------------------------

UDP C library ENet 1.3.12 to freepascal source translation.
And I add simple class version freepascal unit code.

tested in window 32, also support linux(simple test on linux mint x86-64).
If you get any error in linux, there is one unit to modify for work.

It support only IPv4, not IPv6.

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

(1) if you using it to other os environment, you should modify this source unit. You will make changes to time and socket functions.

----------
changes
----------
2015/04/21
- win32/linux works ok.
- fix some alignment problem.
- works on linux.

- initial work, with ENet 1.3.6.
- add SendMsgEventPeer
