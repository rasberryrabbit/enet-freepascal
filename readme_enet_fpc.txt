
enet 1.3.6 to Freepascal
-------------------------

UDP C library ENet 1.3.6 to freepascal source translation.
And I add simple class version freepascal unit code.

But only tested in window 32, linux support is unknown.
If you get any error in linux, there is two unit to modify for work.

Special thanks to Lee Salzman.

For more information about ENet
visit http://enet.bespin.org


---------
file list
---------
enet_callbacks.pas
enet_consts.pas
enet_host.pas (1)
enet_list.pas
enet_packet.pas
enet_peer.pas
enet_protocol.pas
enet_socket.pas (1)

(1) if you using it to other os environment, you should modify this source unit. You will make changes to time and socket functions.

----------
changes
----------
- initial work, with ENet 1.3.6 svn.

