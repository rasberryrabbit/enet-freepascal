unit enet_callbacks;

(**
 @file callbacks.c
 @brief ENet callback functions

 1.3.6 freepascal

 - fix callback pointer nil check
*)

interface

uses enet_consts, enet_socket;

function enet_initialize_with_callbacks (version : integer; inits : pENetCallbacks):integer;
function enet_malloc (size : enet_size_t):pointer;
procedure enet_free (memory : pointer);

implementation

uses sysutils;

var
  callbacks : ENetCallbacks;


function enet_initialize_with_callbacks (version : integer; inits : pENetCallbacks):integer;
begin
   if (version < $00103000) then
     begin result := -1; exit; end;

   if (inits^ . malloc <> nil) or (inits^ . free <> nil) then
   begin
      if (inits^ . malloc = nil) or (inits^ . free = nil) then
        begin result := -1; exit; end;

      callbacks.malloc := ENETCALLBACK_malloc(inits^ . malloc);
      callbacks.free := ENETCALLBACK_free(inits^ . free);
   end;

   if (inits^ . nomemory <> nil) then
     callbacks.nomemory := ENETCALLBACK_nomemory(inits^ . nomemory);

   result := enet_initialize;
end;

function enet_malloc (size : enet_size_t):pointer;
begin
   result := callbacks.malloc (size);

   if (result = nil) then
     callbacks.nomemory;
end;

procedure enet_free (memory : pointer);
begin
   callbacks.free (memory);
end;

function __malloc(size : enet_size_t):pointer; stdcall;
begin
  getmem(result,size);
end;

procedure __free(memory : pointer); stdcall;
begin
  freemem(memory);
end;

procedure __nomemory; stdcall;
begin
  raise Exception.Create('Out of Memory');
end;

initialization
  callbacks.malloc := @__malloc;
  callbacks.free := @__free;
  callbacks.nomemory := @__nomemory;

end.

