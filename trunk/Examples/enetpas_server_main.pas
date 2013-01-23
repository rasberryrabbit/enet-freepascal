unit enetpas_server_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, enet_consts, uENetClass;

type

  { TForm1 }

  TForm1 = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    ListBox1: TListBox;
    Timer1: TTimer;
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure OnConnect(const Event:ENetEvent);
    procedure OnReceive(const Event:ENetEvent; var BroadcastMsg : Boolean; var BroadcastChannel : Byte);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

var
  myServer : TENetClass;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  myServer := TENetClass.Create(30000,True);
  myServer.OnReceive:=@OnReceive;
  myServer.OnConnect:=@OnConnect;
  myServer.OnDisconnect:=@OnConnect;
  myServer.OnNone:=@OnConnect;
  myServer.MessageTimeout:=100; // ideal for application idle?
  myServer.InitHost;
  Timer1.Enabled:=True;
end;

procedure TForm1.ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
begin
    while myServer.ProcessMsg do
        Application.ProcessMessages;

end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  Timer1.Enabled:=False;
  myServer.Free;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin

end;

procedure TForm1.FormShow(Sender: TObject);
begin

end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
end;

procedure TForm1.OnConnect(const Event: ENetEvent);
begin
  if Event.EventType=ord(ENetEventConnect) then
     ListBox1.AddItem('Connect',nil)
     else if Event.EventType=ord(ENetEventDisConnect) then
       ListBox1.AddItem('Disconnect',nil)
       else
         ListBox1.AddItem('None',nil);
end;

procedure TForm1.OnReceive(const Event: ENetEvent; var BroadcastMsg: Boolean;
  var BroadcastChannel: Byte);
var
  msg : string;
begin
  msg := PChar(Event.packet^.data);
  ListBox1.AddItem(msg,nil);
end;



end.

