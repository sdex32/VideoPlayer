unit Unit1;

interface

uses
  BFFmpegVideo,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.AppEvnts;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    OpenDialog1: TOpenDialog;
    ScrollBar1: TScrollBar;
    Button9: TButton;
    ApplicationEvents1: TApplicationEvents;
    Label1: TLabel;
    Button10: TButton;
    Button11: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure ApplicationEvents1Message(var Msg: tagMSG; var Handled: Boolean);
    procedure FormResize(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure ScrollBar1Scroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure Button11Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation



{$R *.dfm}

function  btvideo_Init(winhand:longword; Xpos,Ypos:longint; Xlng,Ylng,Reserved:longword):longword; stdcall; external 'btvideo32.dll';
procedure btvideo_Close(hand:longword); stdcall; external 'btvideo32.dll';
function  btvideo_Play(hand:longword; Filename:pansichar; Res:longword):longint; stdcall; external 'btvideo32.dll';

var h:longword;
    pl:BTFFmpegVideoPlayer;
    movietotaltime:int64;


procedure TForm1.ApplicationEvents1Message(var Msg: tagMSG;
  var Handled: Boolean);

  var i:integer;
begin
   if Msg.message = PLM_UPDATEPROGRESS then
   begin
      ScrollBar1.Position := longint( pl.Position div movietotaltime);
   //   Label1.Caption := GetTimeString(pl.Position);
   end;
   if Msg.message = PLM_USERPAINT then
   begin
      Label1.Caption := GetTimeString(pl.Position);
//      for i:= 1 to 200 do SetPixel(Msg.wParam,i,i,rgb(255,255,255));
   end;
   if Msg.message = PLM_THEEND then
   begin
      Label1.Caption := 'Finish';
   end;


end;

procedure TForm1.Button10Click(Sender: TObject);
begin
   width:=1200;
end;

procedure TForm1.Button11Click(Sender: TObject);
begin
    ScrollBar1.Position := 0;
   pl.Url := self.Edit1.Text;
   pl.PlayEx(45000,50000);
 if pl.playing then movietotaltime := pl.Duration div ScrollBar1.max;
end;


procedure TForm1.Button5Click(Sender: TObject);
begin
    ScrollBar1.Position := 0;
   pl.Url := self.Edit1.Text;
   pl.Play;
    if pl.playing then movietotaltime := pl.Duration div ScrollBar1.max;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
   pl.Stop;
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
   if pl.Pausing then pl.Resume
                 else pl.Pause;

end;

procedure TForm1.Button8Click(Sender: TObject);
begin
   OpenDialog1.Execute;
   Edit1.Text := OpenDialog1.FileName;
end;

procedure TForm1.Button9Click(Sender: TObject);
begin
   if pl.Mute = true then pl.MUTE := false
                     else pl.Mute := true;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin



   pl := BTFFmpegVideoPlayer.Create(self.Handle,10,10,640,480,0);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin

   pl.Free;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  pl.Resize(10,10,Width-40,Height-250);
  self.Repaint;
end;

procedure TForm1.ScrollBar1Change(Sender: TObject);
begin
 //
end;

procedure TForm1.ScrollBar1Scroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
   var a:int64;
begin
   a := (pl.Duration div ScrollBar1.max) * int64(ScrollPos); //int64(ScrollBar1.Position);
   pl.Seek(a);
end;

end.
