unit enet_client_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, enet_consts, uENetClass;

type

  { TForm1 }

  TForm1 = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Edit1: TEdit;
    ListBox1: TListBox;
    Timer1: TTimer;
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OnConnect(const Event:ENetEvent);
    procedure OnReceive(const Event:ENetEvent; var BroadcastMsg : Boolean; var BroadcastChannel : Byte);
    procedure Timer1Timer(Sender: TObject);
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
  myClient: TENetClass;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  myClient := TENetClass.Create(30000,False);
  myClient.MessageTimeout:=100;
  myClient.OnReceive:=@OnReceive;
  myClient.OnConnect:=@OnConnect;
  myClient.OnDisconnect:=@OnConnect;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  myClient.Connect('localhost',30000);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  myClient.BroadcastMsg(1,PChar(Edit1.Text),Length(Edit1.Text)+1,[ENetPacketReliable]);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  myClient.SendMsg(1,PChar(Edit1.Text),Length(Edit1.Text)+1,[ENetPacketReliable]);
  Application.ProcessMessages;
end;

procedure TForm1.ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
begin
  myClient.ProcessMsg;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  myClient.Free;
end;

procedure TForm1.OnConnect(const Event: ENetEvent);
begin
  if Event.EventType=ord(ENetEventConnect) then
     ListBox1.AddItem('Connected',nil)
     else
       ListBox1.AddItem('Disconnected',nil);
end;

procedure TForm1.OnReceive(const Event: ENetEvent; var BroadcastMsg: Boolean;
  var BroadcastChannel: Byte);
var
  msg : string;
begin
  msg := StrPas(PChar(Event.packet^.data));
  ListBox1.AddItem(msg,nil);
  Application.ProcessMessages;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  myClient.Ping;
end;



end.

