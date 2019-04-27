unit enet_list;

(**
 @file list.c
 @brief ENet linked list functions

 freepascal

 1.3.6
*)

(**
    @defgroup list ENet linked list utility functions
    @ingroup private
    @{
*)

interface

uses enet_consts;


function enet_list_begin(list:pointer):pENetListNode;

function enet_list_end(list:pointer):pENetListNode;

function enet_list_empty(list:pointer):boolean;

function enet_list_next(iterator:pENetListNode):pENetListNode;

function enet_list_previous(iterator:pENetListNode):pENetListNode;

function enet_list_front(list:pointer):pENetListNode;

function enet_list_back(list:pointer):pENetListNode;

procedure enet_list_clear (list : pointer);

function enet_list_insert (position : ENetListIterator; data : pointer):ENetListIterator;

function enet_list_remove (position : ENetListIterator):pENetListNode;

function enet_list_move (position : ENetListIterator; dataFirst, dataLast : Pointer):ENetListIterator;

function enet_list_size (list : pENetList):enet_size_t;


implementation


function enet_list_begin(list:pointer):pENetListNode;
begin
  result := pENetlist(list)^.sentinel.next;
end;

function enet_list_end(list:pointer):pENetListNode;
begin
  result := @(pEnetList(list)^.sentinel);
end;

function enet_list_empty(list:pointer):boolean;
begin
  result := enet_list_begin (list) = enet_list_end (list);
end;

function enet_list_next(iterator:pENetListNode):pENetListNode;
begin
  result := iterator ^. next;
end;

function enet_list_previous(iterator:pENetListNode):pENetListNode;
begin
  result := iterator ^. previous;
end;

function enet_list_front(list:pointer):pENetListNode;
begin
  result := pENetList(list)^.sentinel.next;
end;
function enet_list_back(list:pointer):pENetListNode;
begin
  result := pENetList (list) ^. sentinel.previous;
end;

procedure enet_list_clear (list : pointer);
begin
   pENetList(list)^.sentinel.next := @(pENetList(list)^. sentinel);
   pEnetList(list)^.sentinel.previous := @(pENetList(list)^. sentinel);
end;

function enet_list_insert (position : ENetListIterator; data : pointer):ENetListIterator;
begin
   result := ENetListIterator(data);

   result ^. previous := position ^. previous;
   result ^. next := pENetListNode(position);

   result ^. previous ^. next := pENetListNode(result);
   position ^. previous := pENetListNode(result);
end;

function enet_list_remove (position : ENetListIterator):pENetListNode;
begin
   position ^. previous ^. next := position ^. next;
   position ^. next ^. previous := position ^. previous;

   result := pENetListNode(position);
end;

function enet_list_move (position : ENetListIterator; dataFirst, dataLast : Pointer):ENetListIterator;
var
  first, last : ENetListIterator;
begin
   first := ENetListIterator(dataFirst);
   last := ENetListIterator(dataLast);

   first^. previous^. next := last^. next;
   last^. next^. previous := first^. previous;

   first^. previous := position^. previous;
   last^. next := position;

   first^. previous^. next := first;
   position^. previous := last;

   result := first;
end;

function enet_list_size (list : pENetList):enet_size_t;
var
  position : pENetListNode;  // ENetListIterator
begin
   result :=0;

   position := enet_list_begin(list);
   while position <> enet_list_end(list) do begin
     inc(result);
     position := enet_list_next(position);
   end;
end;

(** @} *)

end.

