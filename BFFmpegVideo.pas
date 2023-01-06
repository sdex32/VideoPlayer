unit BFFmpegVideo;

{
  Written by R.Sombrowsky info@somby.de
  rebuild by Bogi aka Sdex32 :) 2019-2020
  totaly rewrite audio thread and sound swr encoding and put some improvments
}
{ $ mode delphi}


interface

uses
   BWinMMsound, // My liv for souns
   Windows,
   Messages,
   // all you need from FFmpeg
   ffutils,
   fftypes,
   libavutil_frame, libavutil_pixfmt, libavutil_rational,  libavutil_buffer,
   libavutil_samplefmt, libavutil_error, libavutil_imgutils, libavutil_mem,
   libavutil_opt, libavutil_time, libavfilter_formats, libavutil_channel_layout,
   libavformat, libavutil, libavcodec, libswscale, libswresample, libavdevice;


const
   PLM_MSG = WM_USER + 700;
   PLM_USERPAINT = WM_USER + 700 + 1;
   PLM_UPDATEPROGRESS = WM_USER + 700 + 2;
   PLM_THEEND = WM_USER + 700 + 3;


type

      BTFFmpegVideoPlayer=class;

      BTFFmpegVideoPlayerQueueRec=record
         FValid:boolean;
         FFrame:PAVFrame;
      end;

 { BTFFmpegVideoPlayerQueue }
      BTFFmpegVideoPlayerQueue=class
         private
 //  FType:integer;  // nur zum test
            FPlayer:BTFFmpegVideoPlayer;
            FPutEvent,               // Event will set after a valid Put
            FGetEvent:THandle;       // Event will set after a valid Get
            FPutIndex,
            FGetIndex:integer;
            FQueue:array of BTFFmpegVideoPlayerQueueRec;
            FUnblockGet: boolean;
            FUnblockPut:boolean;
            procedure IncIndex(var ix:integer);
         protected
         public
            constructor Create(player:BTFFmpegVideoPlayer;maxframes:integer);virtual;
            destructor Destroy;override;
            function Put(fr:PAVFrame;wait:boolean=true):boolean;
            function Get(var fr:PAVFrame;wait:boolean=true):boolean;
            // Get the next frame if exists if the frame is valid you must free the frame self
            function Peek(var fr:PAVFrame):boolean;
            procedure Del; // invalidate the current getindex but the frame dont free
            procedure WakeUpPut; // if its waiting
            procedure WakeUpGet; // if its waiting
            procedure Clear;
            property UnblockPut:boolean read FUnblockPut write FUnblockPut;
            property UnblockGet:boolean read FUnblockGet write FUnblockGet;
      end;


 { BTFFmpegVideoPlayer }
      BTFFmpegVideoPlayer = class
         private
            aLastError :longint;
            WinHand   :longword;
            wndph     :longword; // old WndProc
            WinDc     :longword;
            Xpos,Ypos :longint;
            Xlng,Ylng :longword;
            AspXlng   :longword;
            AspYlng   :longword;
            AspXpos   :longword;
            AspYpos   :longword;
            BSound    :BTWinMMSound;
            bshand    :longword;
            binfo     :BITMAPINFOHEADER; // for frame buffer
            cmask     :array [0..2] of longword; // do not move   it is part of BMH header
            pbinfo    :pointer;
            pbitmap   :pointer;

            ThreadID  :longword; // main read thread ID
            FReadThreadId:longword; // main read thread handle
            FUrl: string; // file name to play

            FEndPos:int64;
            FTimerId:longword;
            FBadFrameTolerance: double;
            FSeeking:boolean;
            FDisableAudio: boolean;
            FDisableVideo: boolean;
            FPausing,
            FPlaying:boolean;
            FMute: boolean;
            FTimerInterval:integer;
            FAVPixelFormat:TAVPixelFormat;
            FFormatCtx: PAVFormatContext;
            // Video
            FVideoStreamIndex:integer;
            FVideoTimeBase:double;
            FVideoCodec:PAVCodec;
            FVideoCtx:PAVCodecContext;
            FVideoSwsCtx:PSwsContext;
            FVideoDecodeFrame:PAVFrame;
            FVideoDecodeFrameBuffer:PByte;
            FVideoQueue:BTFFmpegVideoPlayerQueue;
            FVideoSynchPts,              // Synchronize-frame-pts
            FVideoSynchTime,             // Synchronize-real-time
            FVideoPts:int64;             // pts of the last shown videoframe
            FVideoWidth,
            FVideoHeight:integer;
            FVideoShowNextFrame:boolean; // will be set to show the first frame if the state is paused
            FVideoSar:TAVRational;       // aspect-ratio of the last shown frame
            // Audio
            FAudioStreamIndex:integer;
            FAudioTimeBase:double;
            FAudioCodec:PAVCodec;
            FAudioCtx:PAVCodecContext;
            FAudioSwrCtx:PSwrContext;
            FAudioBuffer:PByte;          // here are the konvertioned Audiodata kumulativ
            FAudioBufSizeMax,
            FAudioBufSize,
            FAudioBufIndex:integer;
            FAudioQueue:BTFFmpegVideoPlayerQueue;
            FAudioPts:int64;

            FReadThreadSleep:boolean;    // if the  ReadThread schould sleeping
            FReadThreadSleeping:boolean; // if the  Readthread is sleeping

            FEof:boolean;               // if the readthread has reached eof

            FDelayAudioQueueing:boolean;
            FMaxAudioFrames,
            FMaxVideoFrames:integer;
            FVolume:longword;

            procedure SetAVolume(value:longword);
            function GetAudioAvailable: boolean;
            function GetDuration: int64;
            function GetPosition: int64;
            function GetVideoAvailable: boolean;
            function GetVideoTimeCheck: integer;
            procedure SetMaxAudioFrames(AValue: integer);
            procedure SetMaxVideoFrames(AValue: integer);
            procedure SetVideoTimeCheck(AValue: integer);

            procedure StartTimer;
            procedure StopTimer;

            procedure Paint;     // Main showing
            function  CalcDisplayContext:boolean;
         public
            constructor Create(whand:longword; Yp,Xp:longint; Xl,Yl,Flags:longword);
            destructor  Destroy; override;
            function  Play:boolean;  // Staring the play
            procedure Pause;        // Pause the play
            procedure Resume;       // Resume a pausing play
            procedure Stop;         // Stops the play

            procedure Seek(ms:int64);  // Seeking only if there is a duration and
                                       // the time in ms is greater 0 or lower as duration

            procedure Resize(Yp,Xp:longint; Xl,Yl:longword);
            function  PlayEx(StartPos,EndPos:int64):boolean;  // Staring the play


            property Url:string read FUrl write FUrl;   // url or filename to play
            property Playing:boolean read FPlaying;  // The media is playing
            property Pausing:boolean read FPausing;  // The media is pausing
            property Duration:int64 read GetDuration;  // Duration in ms, bei Livestreams -1
            property Position:int64 read GetPosition;  // Current Position in ms, bei Livestreams -1
            property Mute:boolean read FMute write FMute; // Mute the sound
            property Volume:longword read FVolume write SetAVolume;

            property VideoTimeCheck:integer read GetVideoTimeCheck write SetVideoTimeCheck; //set before play winproc timer interval

            property DisableAudio:boolean read FDisableAudio write FDisableAudio;  // disable then sound (must be set before play)
            property DisableVideo:boolean read FDisableVideo write FDisableVideo;  // disable the video (must be set before play)
            property MaxAudioFrames:integer read FMaxAudioFrames write SetMaxAudioFrames;  // set the max capacity of audioqueue
            property MaxVideoFrames:integer read FMaxVideoFrames write SetMaxVideoFrames;  // set the max capacity of audioqueue
            property BadFrameTolerance:double read FBadFrameTolerance write FBadFrameTolerance;
            // in s.
            // after check i found that on livestreams it could be that one or more frames
            // had a pts far away from the last frame-pts.
            // This frames are deleted because these freezing the video.
            property AudioAvailable:boolean read GetAudioAvailable;
            // Only when Playing
            property VideoAvailable:boolean read GetVideoAvailable;
            // Only when Playing
      end;


function GetTimeString(t:int64):string;

implementation


function GetTimeString(t:int64):string;
var hh,mm,ss:integer;
    s:shortstring;
begin
//   ms:=t mod 1000;
   t:=t div 1000;
   ss:=t mod 60;
   t:=t div 60;
   mm:=t mod 60;
   t:=t div 60;
   hh:=t mod 60;
//   dd:=t div 24;
   str(ss,s);
   Result := string(s);
   str(mm,s);
   Result := string(s) +'.'+Result;
   str(hh,s);
   Result := string(s) +'.'+Result;
 end;






const
   PLWP_EOF = 0;
   PLWP_TIMER = 1;
   PLWP_UPDATEPROGRESS = 2;
   PLWP_USERPAINT = 3;


var InitSFOk:boolean;
    InitSFCount:integer;


//------------------------------------------------------------------------------
procedure InitSF;  //  FFMPEG initialize
begin
   inc(InitSFCount);
   if InitSFCount<>1 then exit;   // already initialized
   InitSFOk:=false;
   av_register_all();
   avcodec_register_all();
	 avdevice_register_all();
   InitSFOk:=true;
end;

//------------------------------------------------------------------------------
procedure DoneSF;
begin
   if InitSFCount>0 then begin
      dec(InitSFCount);
   end else exit;
   if InitSFCount>0 then exit;
end;


//------------------------------------------------------------------------------
function ReadThread(p: Pointer): Integer; stdcall;
  // Read all packets decode it an put it in then Queues if there is space
var rc:integer;
    pc:BTFFmpegVideoPlayer;
    EnableAudioQueue:boolean;
    pa:PAVPacket;

   procedure QueueFrame(ct:PAVCodecContext;q:BTFFmpegVideoPlayerQueue;pp:PAVPacket;video:boolean);
   var rc:integer;
       ts:int64;
       pq:boolean;
       fr:PAVFrame;
   begin
       rc := avcodec_send_packet(ct, pp);
       if (rc < 0) then begin
          exit;
       end;
       repeat
          if pc.FReadThreadSleep then break;
          fr:=av_frame_alloc();
          if fr=nil then break;
          rc:=avcodec_receive_frame(ct, fr);
{
        fillchar(a,sizeof(a),0);
        av_strerror(rc,@a,sizeof(a));
        wLog(format('  V:%d rc:%d %s',[integer(video),rc,string(a)]));
}
          if (rc=0) then
          begin
             ts:=av_frame_get_best_effort_timestamp(fr);
            // we Queded only the frames with timestamp>=0
             pq:=false;
             if (not pc.FReadThreadSleep) and (ts>=0) then
             begin
                if video then
                begin
                   // we enable the queuing of audioframes only
                   // if we have the first videoframe.
                   // We do it so because we could see a dark image for a longer time
                   EnableAudioQueue:=true;
                   pq:=true;
                end else begin
                   if EnableAudioQueue then pq:=true;
                end;
             end;

             if pq then
             begin
                if not q.Put(fr) then
                begin
                   // not FPlaying or Unblock
                   av_frame_free(@fr);
                   break;
                end;
             end else begin
                av_frame_free(@fr);
             end;
          end else begin
             av_frame_free(@fr);
             break;
          end;
       until false;
   end;

begin
//   repeat
   result:=0;
    pa:=av_packet_alloc;
    pc:=BTFFmpegVideoPlayer(p);
    with pc do
    begin
       EnableAudioQueue:=FVideoStreamIndex<0;
       // If we want to show video we wait at the beginning with queueing of audioframes
       // so long if we have the first videoframe
       while FPlaying do
       begin
          if FReadThreadSleep or FEof then
          begin
             // In the sleeping-mode the Thread can be suspended because it is not blocked
             FReadThreadSleeping:=true;
             Sleep(20);
             Continue;
          end;
          if FDelayAudioQueueing then
          begin
             if FVideoStreamIndex>=0 then EnableAudioQueue:=false; // we wait until to the first videoframe
             FDelayAudioQueueing:=false;
          end;
          rc := av_read_frame(FFormatCtx, pa);
          if (rc < 0) then
          begin
             av_packet_unref(pa);
             if rc=AVERROR_EOF then
             begin
                FEof:=true;
                Continue;
             end else begin
                break;
             end;
          end;
          if (FAudioStreamIndex>=0) and (pa.stream_index = FAudioStreamIndex) then
          begin
             QueueFrame(FAudioCtx,FAudioQueue,pa,false);
          end else begin
             if (FVideoStreamIndex>=0) and (pa.stream_index = FVideoStreamIndex) then
             begin
                QueueFrame(FVideoCtx,FVideoQueue,pa,true);
             end;
          end;
          av_packet_unref(pa);
       end;
    end;
    av_packet_free(@pa);
//   until true;
end;

//------------------------------------------------------------------------------
// callback to BMMSound
function AudioThread(userdata:Pointer; stream: PByte; len:integer):longint; stdcall;
var si,len1:integer;
    b:PByte;
    fr:PAVFrame;
    da:PByte;
    ts:int64;
    o:BTFFmpegVideoPlayer;

   function FillBuffer:boolean;
   begin
      Result := true; //exit is ON
      da:=nil;

      if not o.FAudioQueue.Get(fr,false) then
      begin
         if (not o.FPlaying) then exit; //break; // end
         // clear buffer
         fillchar(stream^,len,0);   // silence
         if o.FEof then PostMessage(o.winhand,PLM_MSG,PLWP_EOF,0);     //PostMessage(winhand,PLM_EOF,0,0);
         exit; //break;
      end;
      si:=av_samples_get_buffer_size(nil,o.FAudioCtx.channels,
                                     fr.nb_samples,
                                     AV_SAMPLE_FMT_FLT, 1);
      ReAllocMem(da,si*2);
      if o.FMute or o.FVideoShowNextFrame then
      begin
         fillchar(o.FAudioBuffer^,si,0);
      end else begin
         swr_convert(o.FAudioSwrCtx, @da,
                     fr.nb_samples,
                     @fr.data[0], fr.nb_samples);
         Move(da^,o.FAudioBuffer^,si);
      end;
      o.FAudioBufIndex := 0;
      o.FAudioBufSize := fr.nb_samples * 8; //si; // stereo 32 bit
      ts:=av_frame_get_best_effort_timestamp(fr);
      if ts>=0 then
      begin
         o.FAudioPts:=ts;
         PostMessage(o.winhand,PLM_UPDATEPROGRESS,0,0);
         if o.FEndPos <> 0 then //for PlayEX
         begin
            if o.Position > o.FEndPos then
            begin
               PostMessage(o.winhand,PLM_MSG,PLWP_EOF,0); //end
            end;
         end;
      end;
      av_frame_free(@fr);
      ReAllocMem(da,0);
      Result := false;
   end;

begin
   Result := 0;
   o := BTFFmpegVideoPlayer(userdata);
   if o.FPlaying then
   begin
      repeat
         if (o.FAudioBufIndex = 0) then  // first time 0=0
         begin
            if FillBuffer then Exit; // fill BufSeze and Index = 0  -> AudioBuffer
         end;

         len1:=o.FAudioBufSize-o.FAudioBufIndex; // What is to send from then audiobuffer
         if (len1 > len) then len1 := len;   // First we send only a part
         b:=o.FAudioBuffer;
         inc(b,o.FAudioBufIndex);
         Move(b^,stream^,len1);
         dec(len,len1);
         inc(stream,len1);
	       inc(o.FAudioBufIndex,len1);
         if o.FAudioBufIndex = o.FaudioBufSize then o.FAudioBufIndex := 0;

      until len = 0;
   end else begin
      fillchar(stream^,len,0);
   end;
end;

{ BTFFmpegVideoPlayerQueue ==============================================================}

procedure BTFFmpegVideoPlayerQueue.IncIndex(var ix: integer);
begin
   inc(ix);
   if ix>=Length(FQueue) then ix:=0;
end;

//------------------------------------------------------------------------------
constructor BTFFmpegVideoPlayerQueue.Create(player: BTFFmpegVideoPlayer; maxframes: integer);
var i:integer;
begin
   FPlayer:=player;
   FGetEvent:=CreateEvent(nil,false,false,nil);
   FPutEvent:=CreateEvent(nil,false,false,nil);

   SetLength(FQueue,maxframes);
   for i:=0 to Length(FQueue)-1 do with FQueue[i] do
   begin
      FValid:=false;
      FFrame:=nil;
   end;
end;

//------------------------------------------------------------------------------
destructor BTFFmpegVideoPlayerQueue.Destroy;
begin
   Clear;
   SetLength(FQueue,0);
   CloseHandle(FGetEvent);
   CloseHandle(FPutEvent);
   inherited Destroy;
end;

//------------------------------------------------------------------------------
function BTFFmpegVideoPlayerQueue.Put(fr: PAVFrame; wait: boolean): boolean;
begin
   result:=false;
   if fr=nil then exit;
   if (not FPlayer.FPlaying) or FUnblockPut then exit;
   with FQueue[FPutIndex] do
   begin
      while FValid do
      begin
         // Current index valid yet
         if (not FPlayer.FPlaying) or FUnblockPut then exit;
         if not wait then exit;
         WaitForSingleObject(FGetEvent,INFINITE); // Warten, bis abgeholt wird
      end;
      // Element ist leer und kann gefüllt werden
      FFrame:=fr;
      FValid:=true;
   end;
   IncIndex(FPutIndex);
   SetEvent(FPutEvent);
   result:=true;
end;

//------------------------------------------------------------------------------
function BTFFmpegVideoPlayerQueue.Get(var fr: PAVFrame; wait: boolean): boolean;
begin
   result:=false;
   if (not FPlayer.FPlaying) or FUnblockGet then exit;
   with FQueue[FGetIndex] do
   begin
      while not FValid do
      begin
         if (not FPlayer.FPlaying) or FUnblockGet then exit;
         if not wait then exit;
         WaitForSingleObject(FPutEvent,INFINITE); // Warten, bis etwas da ist
      end;
      // Element ist voll und kann geleert werden
      fr:=FFrame;
      FFrame:=nil;
      FValid:=false;  // the position can be used to put
   end;
   IncIndex(FGetIndex);
   SetEvent(FGetEvent);
   result:=true;
end;

//------------------------------------------------------------------------------
function BTFFmpegVideoPlayerQueue.Peek(var fr: PAVFrame): boolean;
begin
   with FQueue[FGetIndex] do
   begin
      fr:=FFrame;
      result:=FValid;
   end;
end;

//------------------------------------------------------------------------------
procedure BTFFmpegVideoPlayerQueue.Del;
begin
   with FQueue[FGetIndex] do
   begin
      if not FValid then exit;
      FFrame:=nil;
      FValid:=false;
   end;
   IncIndex(FGetIndex);
   SetEvent(FGetEvent);
end;

//------------------------------------------------------------------------------
procedure BTFFmpegVideoPlayerQueue.WakeUpPut;
begin
   SetEvent(FGetEvent);   // get-event must be wake up because put could be waiting for it
end;

//------------------------------------------------------------------------------
procedure BTFFmpegVideoPlayerQueue.WakeUpGet;
begin
   SetEvent(FPutEvent);   // put-event must be wake up because get could be waiting for it
end;

//------------------------------------------------------------------------------
procedure BTFFmpegVideoPlayerQueue.Clear;
var i:integer;
begin
   for i:=0 to Length(FQueue)-1 do with FQueue[i] do begin
      FValid:=false;
      if FFrame<>nil then av_frame_free(@FFrame);
   end;
   FPutIndex:=0;
   FGetIndex:=0;
end;


//==============================================================================

function _WindowProc(aWindow: HWnd; aMessage: UINT; WParam : WPARAM;
                    LParam: LPARAM): LRESULT; stdcall;
var res: LRESULT;
    obj:BTFFmpegVideoPlayer;
begin
   res := 0;
   obj := BTFFmpegVideoPlayer(GetProp(aWindow,'myFFMPG'));
   if obj <> nil then
   begin
//   PLM_UPDATEPROGRESS
      if aMessage = PLM_MSG then
      begin
         case wParam  of
            PLWP_EOF: begin
               Obj.Stop; // EOF found
               PostMessage(aWindow,PLM_THEEND,0,0);
            end;
            PLWP_TIMER: begin

               if (not obj.FPlaying) or (obj.FPausing and not obj.FVideoShowNextFrame) then
               begin
                  // nothing to do ????
                  //todo refresh only bmp
                  { ne stava pri pauza ne stuga tuka
    SetDIBitsToDevice(obj.windc, obj.Xpos, obj.Ypos,obj.Xlng, obj.Ylng, 0, 0, 0, obj.Ylng,
                               obj.pbitmap, bitmapinfo(obj.pbinfo^),
                              DIB_RGB_COLORS);
                   }
               end else begin
                   obj.Paint;
                   PostMessage(aWindow,PLM_USERPAINT,obj.WinDc,0);
               end;



            end;
         end;
      end;
   end;
   if aMessage = WM_TIMER then
   begin
      if wParam = 11123 then
      begin
         SendMessage(aWindow,PLM_MSG,PLWP_TIMER,0);
      end;
   end;


   if res = 0 then res := CallWindowProc(pointer(Obj.wndph), aWindow, AMessage, WParam, LParam);
//              res := DefWindowProc(aWindow, AMessage, WParam, LParam);
   _WindowProc := res;
end;

//------------------------------------------------------------------------------
constructor BTFFmpegVideoPlayer.Create(whand:longword; Yp,Xp:longint; Xl,Yl,Flags:longword);
begin
   InitSF;
   FTimerInterval:=21;// 50; //15;      41.67ms =24cadara

   FVolume := 255;
   bshand := 0;

   //48000 is sampling *4 byte for 32bit = 192000


   FAudioBufSizeMax:=(192000*8); //7.1 chanels
{
 FVideoQueue:=BTFFmpegVideoPlayerQueue.Create(self,false);
 FVideoQueue.FMaxFrames:=50;
 FAudioQueue:=BTFFmpegVideoPlayerQueue.Create(self,true);
 FAudioQueue.FMaxFrames:=250;
}
   FMaxVideoFrames:=50;
   FMaxAudioFrames:=250;
   FAudioStreamIndex:=-1;
   FVideoStreamIndex:=-1;
   FBadFrameTolerance:=20.0;

   winhand := 0;
   Xpos := Xp;
   Ypos := Yp;
   Xlng := Xl;
   Ylng := Yl;

   if (whand <> 0) and InitSFOk then
   begin
      winhand := whand;
      windc := GetDC(winhand);
      SetProp(whand,'myFFMPG',longword(self));
      wndph := GetWindowLong(whand,GWL_WNDPROC);
      SetWindowLong(whand,GWL_WNDPROC,dword(@_WindowProc));
   end;

end;

//------------------------------------------------------------------------------
destructor BTFFmpegVideoPlayer.Destroy;
begin
   Stop;
   sleep(100);
   if winhand <> 0 then
   begin
     SetWindowLong(winhand,GWL_WNDPROC,dword(wndph));
     RemoveProp(winhand,'myFFMPG');
     ReleaseDC(winhand,windc);
  end;
   inherited;
end;

//------------------------------------------------------------------------------
function BTFFmpegVideoPlayer.GetVideoTimeCheck: integer;
begin
   result:=FTimerInterval;
end;

//------------------------------------------------------------------------------
function BTFFmpegVideoPlayer.GetDuration: int64;
begin
   result:=-1;
   if FFormatCtx=nil then exit;
   result:=FFormatCtx.duration div 1000;
end;

//------------------------------------------------------------------------------
function BTFFmpegVideoPlayer.GetAudioAvailable: boolean;
begin
   result:=FAudioStreamIndex>=0;
end;

//------------------------------------------------------------------------------
function BTFFmpegVideoPlayer.GetPosition: int64;
begin
   result:=-1;
   if (FAudioStreamIndex>=0) then begin
       if (FAudioPts>=0) then begin
          result:=round(FAudioPts*FAudioTimebase*1000);
          if FFormatCtx.start_time <> AV_NOPTS_VALUE then
             result:=result-round(FFormatCtx.start_time/1000);
       end;
   end;
   if result>=0 then exit;
   if (FVideoStreamIndex>=0) then begin
      if (FVideoPts>=0) then begin
         result:=round(FVideoPts*FVideoTimebase*1000);
         if FFormatCtx.start_time <> AV_NOPTS_VALUE then
             result:=result-round(FFormatCtx.start_time/1000);
      end;
   end;
end;

//------------------------------------------------------------------------------
function BTFFmpegVideoPlayer.GetVideoAvailable: boolean;
begin
   result:=FVideoStreamIndex>=0;
end;

//------------------------------------------------------------------------------
procedure BTFFmpegVideoPlayer.SetMaxAudioFrames(AValue: integer);
begin
   if (AValue<=0) then exit;
   FMaxAudioFrames:=AValue;
end;

//------------------------------------------------------------------------------
procedure BTFFmpegVideoPlayer.SetMaxVideoFrames(AValue: integer);
begin
   if (AValue<=0) then exit;
   FMaxVideoFrames:=AValue;
end;

//------------------------------------------------------------------------------
procedure BTFFmpegVideoPlayer.SetVideoTimeCheck(AValue: integer);
begin
   FTimerInterval:=AValue;
end;

//------------------------------------------------------------------------------
procedure BTFFmpegVideoPlayer.StartTimer;
begin
   if FTimerId<>0 then exit;
   FTimerId:=windows.SetTimer(winhand,11123,FTimerInterval,nil);
 //  FTimerId:=SDL_AddTimer(FTimerInterval,TimerThread,Pointer(handle));
end;

//------------------------------------------------------------------------------
procedure BTFFmpegVideoPlayer.StopTimer;
begin
   if FTimerId<>0 then begin
      KillTimer(winhand, FTimerID);
      FTimerId:=0;
   end;
end;

(*
   {var
     ValueY_298,
                    ValueU_100,
                    ValueU_516,
                    ValueV_409,
                    ValueV_208    : ARRAY[byte] OF integer;
//                    ValueClip     : ARRAY[-1023..1023] OF byte;
                    ValueClip     : ARRAY[-32000..32000] OF byte; //1023
    }


PROCEDURE BTFFmpegVideoPlayer.PrepareTables;
VAR
  i : integer;
BEGIN
  FOR i := 0 TO 255 DO
    BEGIN
      // http://msdn.microsoft.com/en-us/library/ms893078.aspx
      // GREEN - -

      ValueY_298[i] := (i- 16) * 298  +  128;      //  -4640 .. 71350
      ValueU_100[i] := (i-128) * 100;              // -12800 .. 12700
      ValueU_516[i] := (i-128) * 516;              // -66048 .. 65532
      ValueV_409[i] := (i-128) * 409;              // -52352 .. 51943
      ValueV_208[i] := (i-128) * 208;              // -26624 .. 26416

      // http://en.wikipedia.org/wiki/YCbCr  (ITU-R BT.601)
      // GREEN is + +
      {
      ValueY_298[i] := round(i *  298.082);
      ValueU_100[i] := round(i * -100.291);
      ValueU_516[i] := round(i *  516.412  - 276.836*256);
      ValueV_409[i] := round(i *  408.583  - 222.921*256);
      ValueV_208[i] := round(i * -208.120  + 135.576*256);
      }
    END;
  FillChar(ValueClip, SizeOf(ValueClip), #0);
  FOR i := 0 TO 255 DO
    ValueClip[i] := i;
//  FOR i := 256 TO 1023 DO
  FOR i := 256 TO 32000 DO
    ValueClip[i] := 255;
  //fYUY2TablesPrepared := true;
END;



procedure BTFFmpegVideoPlayer.I420_to_RGB(pData: pointer);
// http://en.wikipedia.org/wiki/YCbCr
VAR
  L, X, Y    : integer;
  ps         : pbyte;
  pY, pU, pV : pbyte;
begin
  pY := pData;
  // Y = root
  FOR Y := 0 TO FVideoHeight-1 DO
    BEGIN
      ps := pointer(longword(FTexture) + y*(FVideoWidth*4));  // target

      pU := pData;
      // u = width*heihgt + (row/2)*(width/2)
      Inc(pU, fVideoWidth*(fVideoheight+ Y div 4));
      pV := PU;
      // v = u + (width*height)/4
      Inc(pV, (fvideoWidth*fvideoheight) div 4);

      FOR X := 0 TO (fvideoWidth div 2)-1 DO
        begin
          // C = (Y-16) *298
          // D = U -128
          // E = V - 128
          // R = (C +409*E + 128) /256
          // G = (C -100*D -208*E + 128) /256
          // B = (C +516*D+128)/256


          L := ValueY_298[pY^];

{
          inc(ps); //A
          ps^ := ValueClip[(L + ValueU_516[pU^]                  ) div 256];
          Inc(ps);
          ps^ := ValueClip[(L + ValueU_100[pU^] + ValueV_208[pV^]) div 256];
          Inc(ps);
          ps^ := ValueClip[(L                   + ValueV_409[pV^]) div 256];
          Inc(ps);
//            inc(ps);
          Inc(pY);

          L := ValueY_298[pY^];
          inc(ps); //A
          ps^ := ValueClip[(L + ValueU_516[pU^]                     ) div 256];
          Inc(ps);
          ps^ := ValueClip[(L + ValueU_100[pU^] + ValueV_208[pV^]) div 256];
          Inc(ps);
          ps^ := ValueClip[(L                   + ValueV_409[pV^]) div 256];
          Inc(ps);
//            inc(ps);
          Inc(pY);
 }


//          inc(ps); //A
          //R
          //B
          ps^ := ValueClip[(L + ValueU_516[pU^]                  ) div 256];
//          ps^ := 0;
          Inc(ps);
          //G
          ps^ := ValueClip[(L - ValueU_100[pU^] - ValueV_208[pV^]) div 256];
//          ps^ := 0;
          Inc(ps);
          //R
          ps^ := ValueClip[(L                   + ValueV_409[pV^]) div 256];
//          ps^ := 255;
          Inc(ps);
          //A
          inc(ps);

          Inc(pY);

          L := ValueY_298[pY^];
//          inc(ps); //A
          //B
          ps^ := ValueClip[(L + ValueU_516[pU^]                  ) div 256];
//          ps^ := 0;
          Inc(ps);
          //G
          ps^ := ValueClip[(L - ValueU_100[pU^] - ValueV_208[pV^]) div 256];
//          ps^ := 0;
          Inc(ps);
          //R
          ps^ := ValueClip[(L                   + ValueV_409[pV^]) div 256];
//          ps^ := 255;
          Inc(ps);

          //A
          inc(ps);

          Inc(pY);



{

//          inc(ps); //A
          ps^ := ValueClip[(L + ValueU_100[pU^] + ValueV_208[pV^]) div 256];
          Inc(ps);
          ps^ := ValueClip[(L                   + ValueV_409[pV^]) div 256];
          Inc(ps);
          ps^ := ValueClip[(L + ValueU_516[pU^]                  ) div 256];
          Inc(ps);

            inc(ps);
          Inc(pY);

          L := ValueY_298[pY^];
//          inc(ps); //A
          ps^ := ValueClip[(L + ValueU_100[pU^] + ValueV_208[pV^]) div 256];
          Inc(ps);
          ps^ := ValueClip[(L                   + ValueV_409[pV^]) div 256];
          Inc(ps);
          ps^ := ValueClip[(L + ValueU_516[pU^]                   ) div 256];
          Inc(ps);

            inc(ps);
          Inc(pY);
 }


          Inc(pU);
          Inc(pV);
        end;
    END;
end;

*)

//------------------------------------------------------------------------------
procedure BTFFmpegVideoPlayer.Paint;
var nfr,fr:PAVFrame;
    its,ts:int64;
    tr,tv:double;
    i:longword;
    src:pointer;
    des:pointer;
begin

   nfr:=nil;
   its:=-1;
   repeat
      if (not FPlaying) or (FSeeking) then //or (FTexture=nil) then
      begin
         break;
      end;
      if FPausing and not FVideoShowNextFrame then break;
      if not FVideoQueue.Peek(fr) then
      begin
// There is no frame we wait for the next invalidate be timer-tick
         if FEof then PostMessage(winhand,PLM_MSG,PLWP_EOF,0);  //PostMessage(winhand,PLM_EOF,0,0);
         break;
      end;
   // valid frame
      ts:=av_frame_get_best_effort_timestamp(fr);
      if ts<0 then
      begin  // could be a B-Frame
         FVideoQueue.Del;
         av_frame_free(@fr);
         Continue;  // next
      end;
      if FVideoSynchPts<0 then
      begin
      // We set the sysnchonize if we have no audio
         FVideoSynchPts:=ts;
         FVideoSynchTime:=av_gettime_relative;
      end;
      tv:=ts*FVideoTimebase;
      if (FAudioStreamIndex>=0) then
      begin
         tr:=FAudioPts*FAudioTimeBase;
      end else begin
         tr:=(av_gettime_relative-FVideoSynchTime)/1000000.0+FVideoSynchPts*FVideoTimebase;
      end;

      if (tv>tr+FBadFrameTolerance) then
      begin  // if the frame is out of time
         FVideoQueue.Del;
         av_frame_free(@fr);
         // next
      end else begin
        if (tr>=0) and (tv<=tr) then
        begin                   // if there is a audiopacket
           if nfr<>nil then  av_frame_free(@nfr);  // old frame not to render
           FVideoQueue.Del;
           nfr:=fr;         // mark next frame to render, at the and it will only
                            // render the latest frame
           fr:=nil;
           its:=ts;
        end else begin
           // The pts of current frame should be shown later,
           // thats why we hold the frame in the queue (not deleting)
           // for the next check.
           // We don't need to presend the renderer
           break;
        end;
      end;
   until false;

   if nfr<>nil then
   begin
      if FVideoShowNextFrame then
      begin
         if FPausing then
         begin
            // Stop the Timer
            StopTimer;
            if FAudioStreamIndex>=0 then
            begin
               BSound.Pause(bshand);
//               SDL_PauseAudio(1);
            end;
         end;
         FVideoShowNextFrame:=false;
      end;

      // decodeframe
      sws_scale(FVideoSwsCtx,
					          FFTypes.PPbyte(@nfr.data),
					          Pinteger(@nfr.linesize),
					          0,
					          FVideoCtx.height,
					          FFTypes.PPbyte(@FVideoDecodeFrame.data),
					          Pinteger(@FVideoDecodeFrame.linesize));
      //!!!NOTЕ!!!!!
      //The new picture will be shrink in same memory location to Xlng Ylng put Pitch and Image are wil be with the full size


 //     I420_to_RGB(FVideoDecodeFrame.data[0]); no need any more i sues sws context to decode to rgb



//      FVideoSar:=av_guess_sample_aspect_ratio(FFormatCtx,FFormatCtx.streams[FVideoStreamIndex],nfr);
      FVideoSar:=av_guess_sample_aspect_ratio(FFormatCtx,PPtrIdx(FFormatCtx.streams,FVideoStreamIndex),nfr);
      FVideoPts:=its;   // pts of last shown image


              (*
      if (FAudioStreamIndex<0) then
      begin
         PostMessage(winhand,PLM_UPDATEPROGRESS,0,0); // UpdateProgress
         if FEndPos <> 0 then //for PlayEX
         begin
            if Position > FEndPos then
            begin
               PostMessage(winhand,PLM_MSG,PLWP_EOF,0); //end
            end;
         end;
      end;
                *)
      av_frame_free(@nfr);

      pbinfo := @binfo;


 {
   --last work
     StretchDiBits(windc, Xpos,Ypos,Xlng,Ylng, 0,0,Xlng,Ylng,
                                   pointer(FVideoDecodeFrame.data[0]), bitmapinfo(pbinfo^),
                                                                      DIB_RGB_COLORS, SRCCOPY);
                                   }

      // copy From buffer to BMP to uses SetDIB
      src := pointer(FVideoDecodeFrame.data[0]);
      des := pointer(longword(pbitmap) + aspYpos*Xlng*4 + aspXpos*4);
      for i := 0 to aspYlng - 1 do
      begin
         move(src^,des^,aspXlng*4);
         src := pointer(longword(src) + longword(FVideoWidth)*4);
         des := pointer(longword(des) + Xlng*4);
      end;

      SetDIBitsToDevice(windc, Xpos, Ypos, Xlng, Ylng, 0, 0, 0, Ylng,
                                 pbitmap, bitmapinfo(pbinfo^), DIB_RGB_COLORS);
   end; //nrf <> nill
end;

//------------------------------------------------------------------------------
function  BTFFmpegVideoPlayer.CalcDisplayContext:boolean;
var rc:integer;
    small:boolean;
    asp,asps:single;
    numbytes:longword;
    xmax,ymax:longword;
begin
   Result := false; //fail

   // free all old if exist
   if FVideoQueue<>nil then
   begin
      FVideoQueue.WakeUpGet;
      FVideoQueue.WakeUpPut;
   end;

   if (FVideoDecodeFrameBuffer<>nil) then
   begin
      av_free(FVideoDecodeFrameBuffer);
      FVideoDecodeFrameBuffer:=nil;
   end;

   if (FVideoDecodeFrame<>nil) then
   begin
	    av_frame_free(@FVideoDecodeFrame);
   end;

   if (FVideoSwsCtx<>nil) then
   begin
      sws_freeContext(FVideoSwsCtx);
   end;

   if pBitmap <> nil then
   begin
      ReAllocMem(pBitmap,0);
   end;


// setup the display context

//   FSDLPixelFormat:=SDL_PIXELFORMAT_IYUV;
//         FAVPixelFormat:=AV_PIX_FMT_YUV420P;
   FAVPixelFormat:=AV_PIX_FMT_BGRA;
//    AV_PIX_FMT_ARGB,      ///< packed ARGB 8:8:8:8, 32bpp, ARGBARGB... no
//    AV_PIX_FMT_RGBA,      ///< packed RGBA 8:8:8:8, 32bpp, RGBARGBA...
//    AV_PIX_FMT_ABGR,      ///< packed ABGR 8:8:8:8, 32bpp, ABGRABGR... no
//    AV_PIX_FMT_BGRA,      ///< packed BGRA 8:8:8:8, 32bpp, BGRABGRA... no

  // FAVPixelFormat:=AV_PIX_FMT_RGBA; //AV_PIX_FMT_YUV420P;

   // Calculate aspect ratio picture by given area Xlng Ylng
   small := false;
   asp := FVideoCtx.width / FVideoCtx.height; // movie aspect
   asps := Xlng/ylng; // display aspect
   if longword(FVideoCtx.width) > Xlng then
   begin
      if asp > asps then
      begin // 16/7 > 16/9  Xlng ostava sustoto
         aspXlng := Xlng;
//      aspYlng := round(FVideoCtx.Height / asp);
         aspYlng := round(FVideoCtx.Height / (FVideoCtx.width/ Xlng));
         aspXpos := 0;
         aspYpos := (Ylng - aspYlng) div 2;
      end else begin
//      aspXlng := round(FVideoCtx.width / asp);
         aspXlng := round(FVideoCtx.width / (FVideoCtx.Height/ Ylng));
         aspYlng := Ylng;
         aspXpos := (Xlng - aspXlng) div 2;
         aspYpos := 0;
      end;
   end else begin
      small := true;
      if asp > asps then
      begin // 16/7 > 16/9  Xlng ostava sustoto
         aspXlng := Xlng;
//      aspYlng := round(FVideoCtx.Height / asp);
         aspYlng := round(Xlng / asp);
         aspXpos := 0;
         aspYpos := (Ylng - aspYlng) div 2;
      end else begin
//      aspXlng := round(FVideoCtx.width / asp);
         aspXlng := round(Ylng / asp);
         aspYlng := Ylng;
         aspXpos := (Xlng - aspXlng) div 2;
         aspYpos := 0;
      end;

   end;

   if aspXlng > Xlng  then  aspXlng := Xlng;
   if aspYlng > Ylng  then  aspYlng := Ylng;


   FVideoSwsCtx := sws_getContext( FVideoCtx.width,
                                   FVideoCtx.height,
                                   FVideoCtx.pix_fmt,
                                   aspXlng, //FVideoCtx.width,
                                   aspYlng, //FVideoCtx.height,
                                   FAVpixelformat,
    //   SWS_BICUBIC,
                                  SWS_BILINEAR,
                                   nil, nil, nil);
	 if (FVideoSwsCtx=nil) then
   begin
      aLastError:=-1020;
      exit;
   end;

//   FVideoWidth := FFormatCtx.streams[FVideoStreamIndex].codecpar.width;
	 FVideoWidth := PPtrIdx(FFormatCtx.streams,FVideoStreamIndex).codecpar.width;
   // Videowidth


//   FVideoHeight := FFormatCtx.streams[FVideoStreamIndex].codecpar.height;
   FVideoHeight := PPtrIdx(FFormatCtx.streams,FVideoStreamIndex).codecpar.height;
   // Videoheight
   if small then
   begin // change size else alloc memem is samll and error
      FVideoWidth := aspXlng;
      FVideoHeight := aspYlng;
   end;

	 numbytes := av_image_get_buffer_size(FAVPixelFormat, FVideoWidth, FVideoHeight, 8);
	 if (numbytes < 1) then
   begin
      aLastError:=-1008;
      exit;
	  end;
   FVideoDecodeFrame := av_frame_alloc();
	 if (FVideoDecodeFrame=nil) then
   begin
      aLastError:=-1018;
      exit;
   end;

	 FVideoDecodeFrameBuffer := av_malloc(numBytes);
   if FVideoDecodeFrameBuffer=nil then
   begin
      aLastError:=-1019;
      exit;
   end;
                               //allocate video frame decode bubber
   xmax := FVideoWidth;
   if xmax < Xlng then  xmax := Xlng;
   ymax := FVideoHeight;
   if ymax < Ylng then  ymax := Ylng;

	 rc := av_image_fill_arrays(@FVideoDecodeFrame.data[0],
                              @FVideoDecodeFrame.linesize[0],
                              FVideoDecodeFrameBuffer,
                              FAVPixelFormat,
                             Xmax,// FVideoWidth,
                             Ymax,1); // FVideoHeight, 1);
	 if (rc < 0) then
   begin
      aLastError:=-1009;
      exit;
   end;

   fillchar(binfo,sizeof(BITMAPINFOHEADER) , 0);
   binfo.biSize := sizeof(BITMAPINFOHEADER); //12 for c mask
   binfo.biWidth  := xlng;
   binfo.biHeight  := - Ylng;
   binfo.biPlanes  := 1;
   binfo.biBitCount  := 32;
   binfo.biCompression := BI_BITFIELDS;
//   binfo.biSizeImage    := Xlng * Ylng * 4;
   {!!!!! WARNIBG !!!!!}
   { cmask must be after binfo inside memoery }
   cmask[0]:=$FF0000;                       {Bit-Positions B G R A in memoey  24/32Bit }
   cmask[1]:=$00FF00;
   cmask[2]:=$0000FF;

   pbitmap := nil;
   ReallocMem(pbitmap,Xlng*Ylng*4);
   if pbitmap = nil then
   begin
      aLastError:=-1022;
      exit;
   end;
   fillchar(pbitmap^,Xlng*Ylng*4 , 0);
   Result := true;
end;

//------------------------------------------------------------------------------
function BTFFmpegVideoPlayer.Play:boolean;
var rc:integer;
//,numbytes:integer;
//    xmax,ymax:longword;
//    asp,asps:single;
//    small:boolean;
begin
   result:=false; // fail
   aLastError := 0;

   FEndPos := 0;

   if FPlaying then
   begin
      aLastError:=-1000;
      exit;
   end;
   try
      if (not InitSFOk) then
      begin
         aLastError:=-1001;
         exit;
      end;
      if (winhand = 0) then exit;

  //b   fillchar(wantedSpec,sizeof(wantedSpec),0);

	    rc := avformat_open_input(@FFormatCtx, PAnsiChar(ansistring(FUrl)), nil, nil);
	    if (rc<0) then
      begin
         aLastError:=-1002;
         exit;
      end;

      rc := avformat_find_stream_info(FFormatCtx, nil);
      // Load of stream-infos
  	  if (rc<0) then
      begin
         aLastError:=-1003;
         exit;
      end;

      FVideoStreamIndex:=-1;
      if not FDisableVideo then
      begin
         rc:=av_find_best_stream(FFormatCtx, AVMEDIA_TYPE_VIDEO,-1,-1,@FVideoCodec,0);
   	     if (rc>=0) then FVideoStreamIndex:=rc;
      end;

      FAudioStreamIndex:=-1;
      if not FDisableAudio then
      begin
         rc:=av_find_best_stream(FFormatCtx, AVMEDIA_TYPE_AUDIO,-1,FVideoStreamIndex,@FAudioCodec,0);
	       if (rc>=0) then FAudioStreamIndex:=rc;
      end;

      if (FVideoStreamIndex<0) and (FAudioStreamIndex<0) then
      begin
         aLastError:=-1004;
         exit;
      end;


      FVideoWidth:=0;
      FVideoHeight:=0;
      if (FVideoStreamIndex >= 0) then
      begin
         FVideoQueue:=BTFFmpegVideoPlayerQueue.Create(self,FMaxVideoFrames);

//         FVideoTimeBase:=FFormatCtx.streams[FVideoStreamIndex].time_base;
         FVideoTimeBase:=av_q2d(PPtrIdx(FFormatCtx.streams,FVideoStreamIndex).time_base);
 		     FVideoCtx := avcodec_alloc_context3(FVideoCodec);
		     if (FVideoCtx=nil) then
         begin
            aLastError:=-1005;
            exit;
         end;

//         rc := avcodec_parameters_to_context(FVideoCtx,FFormatCtx.streams[FVideoStreamIndex].codecpar);
         rc := avcodec_parameters_to_context(FVideoCtx,PPtrIdx(FFormatCtx.streams,FVideoStreamIndex).codecpar);
         if (rc<0) then
         begin
            aLastError:=-1006;
            exit;
         end;

//         av_codec_set_pkt_timebase(FVideoCtx,FFormatCtx.streams[FVideoStreamIndex].time_base);
         av_codec_set_pkt_timebase(FVideoCtx,PPtrIdx(FFormatCtx.streams,FVideoStreamIndex).time_base);
		     rc := avcodec_open2(FVideoCtx, FVideoCodec, nil);
		     if (rc<0) then
         begin
            aLastError:=-1007;
            exit;
         end;


         if not CalcDisplayContext then Exit;

	    end;

   // Audio
	    if (FAudioStreamIndex >= 0) then
      begin
         FAudioQueue:=BTFFmpegVideoPlayerQueue.Create(self,FmaxAudioFrames);
//       FAudioQueue.FType:=1;
//         FAudioTimeBase:=av_q2d(FFormatCtx.streams[FAudioStreamIndex].time_base);
         FAudioTimeBase:=av_q2d(PPtrIdx(FFormatCtx.streams,FAudioStreamIndex).time_base);
         FAudioCtx := avcodec_alloc_context3(FAudioCodec);
		     if (FAudioCtx=nil) then
         begin
            aLastError:=-1011;
            exit;
         end;
//         rc := avcodec_parameters_to_context(FAudioCtx, FFormatCtx.streams[FAudioStreamIndex].codecpar);
         rc := avcodec_parameters_to_context(FAudioCtx, PPtrIdx(FFormatCtx.streams,FAudioStreamIndex).codecpar);
	 	     if (rc<0) then
         begin
            aLastError:=-1012;
            exit;
         end;

//         av_codec_set_pkt_timebase(FAudioCtx, FFormatCtx.streams[FAudioStreamIndex].time_base);
         av_codec_set_pkt_timebase(FAudioCtx, PPtrIdx(FFormatCtx.streams,FAudioStreamIndex).time_base);

         rc := avcodec_open2(FAudioCtx, FAudioCodec, nil);
	       if (rc<>0) then
         begin
            aLastError:=-1013;
            exit;
         end;
         FAudioSwrCtx := swr_alloc();
		     if (FAudioSwrCtx=nil) then
         begin
            aLastError:=-1014;
            exit;
         end;


         // conevrt any audio to stereo 48000 32nit float
         case FaudioCtx.channels of
           1: av_opt_set_channel_layout(FAudioSwrCtx, 'in_channel_layout',AV_CH_LAYOUT_MONO, 0);
           2: av_opt_set_channel_layout(FAudioSwrCtx, 'in_channel_layout',AV_CH_LAYOUT_STEREO, 0);
           3: av_opt_set_channel_layout(FAudioSwrCtx, 'in_channel_layout',AV_CH_LAYOUT_2_1, 0);
           4: av_opt_set_channel_layout(FAudioSwrCtx, 'in_channel_layout',AV_CH_LAYOUT_2_2, 0);
           5: av_opt_set_channel_layout(FAudioSwrCtx, 'in_channel_layout',AV_CH_LAYOUT_4POINT1, 0);
           6: av_opt_set_channel_layout(FAudioSwrCtx, 'in_channel_layout',AV_CH_LAYOUT_5POINT1, 0);
           7: av_opt_set_channel_layout(FAudioSwrCtx, 'in_channel_layout',AV_CH_LAYOUT_6POINT1, 0);
           8: av_opt_set_channel_layout(FAudioSwrCtx, 'in_channel_layout',AV_CH_LAYOUT_7POINT1, 0);
           else av_opt_set_channel_layout(FAudioSwrCtx, 'in_channel_layout',AV_CH_LAYOUT_NATIVE, 0);

         end;
         av_opt_set_channel_layout(FAudioSwrCtx, 'out_channel_layout', AV_CH_LAYOUT_STEREO, 0); //AV_CH_LAYOUT_MONO, 0);

         av_opt_set_int(FAudioSwrCtx, 'in_sample_rate', FAudioCtx.sample_rate, 0);
//		     av_opt_set_int(FAudioSwrCtx, 'out_sample_rate', FAudioCtx.sample_rate, 0);
		     av_opt_set_int(FAudioSwrCtx, 'out_sample_rate', 48000, 0);
		     av_opt_set_sample_fmt(FAudioSwrCtx, 'in_sample_fmt', FAudioCtx.sample_fmt, 0);
		     av_opt_set_sample_fmt(FAudioSwrCtx, 'out_sample_fmt', AV_SAMPLE_FMT_FLT, 0);
		     rc := swr_init(FAudioSwrCtx);
		     if (rc<0) then
         begin
            aLastError:=-1015;
            exit;
         end;

         FAudioBuffer := nil;
         ReAllocMem(FAudioBuffer,FAudioBufSizeMax);    //todo
         if FAudioBuffer = nil then
         begin
            aLastError:=-1016;
            exit;
         end;

//         ReAllocMem(FAudioBuffer2,FAudioBufSizeMax);
         BSound := BTWinMMSound.Create;

         bshand := BSound.OpenRawSound(48000{FAudioCtx.sample_rate},2 { FAudioCtx.channels}, 32, FVolume, 8,@AudioThread, pointer(self));

         if bshand = 0 then
         begin
            aLastError:=-1017;
            exit;
         end;


      end;
      FEof:=false;
      FAudioBufSize:=0;
      FAudioBufIndex:=0;
//   FImagePts:=-1;
      FAudioPts:=-1;
      FVideoPts:=-1;
      FVideoSynchPts:=-1;
      FVideoSynchTime:=-1;           // SynchRealtime

      FPlaying:=true;

      FReadThreadId:= CreateThread(nil,0,@ReadThread,self,0,ThreadID);


      if FAudioStreamIndex>=0 then begin
         BSound.Play(bshand);

      end;
      StartTimer;


      FPausing:=false;
      //FLastTimeString:='';
      result:=true;
   finally
      if not Result then Stop;
   end;
end;

//------------------------------------------------------------------------------
procedure BTFFmpegVideoPlayer.Pause;
begin
   if not FPlaying then exit;
   FPausing:=true;
   StopTimer;
   if FAudioStreamIndex>=0 then
   begin
      BSound.Pause(bshand);
   end;
end;

//------------------------------------------------------------------------------
procedure BTFFmpegVideoPlayer.Resume;
begin
   if (not FPlaying) or (not FPausing) then exit;
 // Set SynchTime new
   FVideoSynchPts:=-1;
   FVideoSynchTime:=-1;

   FPausing:=false;
   StartTimer;
   if FAudioStreamIndex>=0 then
   begin
      BSound.Resume(bshand);
   end;
end;

//------------------------------------------------------------------------------
procedure BTFFmpegVideoPlayer.Stop;
begin

   if FPlaying then
   begin
      StopTimer;
      FPlaying:=false;
   end;

   if FVideoQueue<>nil then with FVideoQueue do
   begin
      WakeUpGet;
      WakeUpPut;
   end;
   if FAudioQueue<>nil then with FAudioQueue do
   begin
      WakeUpGet;
      WakeUpPut;
   end;


   if FReadThreadId<>0 then
   begin
      WaitForSingleObject(FReadThreadId,INFINITE);
      TerminateThread(FReadThreadId,0);
      CloseHandle(FReadThreadId);
      FReadThreadId:=0;
   end;
  if FAudioCodec<>nil then
   begin
      BSound.Close(bshand);
      bshand := 0;
      BSound.Free;
   end;

   FVideoQueue.Free;
   FVideoQueue:=nil;

   FAudioQueue.Free;
   FAudioQueue:=nil;

   ReAllocMem(FAudioBuffer,0);
   if (FAudioSwrCtx<>nil) then
   begin
	   	swr_free(@FAudioSwrCtx);
   end;
   if (FAudioCtx<>nil) then
   begin
	    avcodec_free_context(@FAudioCtx);
   end;

   if (FVideoDecodeFrameBuffer<>nil) then
   begin
	    av_free(FVideoDecodeFrameBuffer);
      FVideoDecodeFrameBuffer:=nil;
   end;

	 if (FVideoDecodeFrame<>nil) then
   begin
	   	av_frame_free(@FVideoDecodeFrame);
   end;
	 if (FVideoSwsCtx<>nil) then
   begin
      sws_freeContext(FVideoSwsCtx);
      FVideoSwsCtx:=nil;
   end;

   if (FVideoCtx<>nil) then
   begin
		   avcodec_free_context(@FVideoCtx);
   end;

   FAudioStreamIndex:=-1;
   FAudioTimeBase:=0;
   FVideoTimeBase:=0;

   FVideoStreamIndex:=-1;
   FAudioCodec:=nil;
	 FVideoCodec:=nil;
   if (FFormatCtx<>nil) then
   begin
    	avformat_close_input(@FFormatCtx);
   end;
   FVideoWidth:=0;
   FVideoHeight:=0;
   FVideoSar.den:=0;
   FVideoSar.num:=0;


   if pBitmap <> nil then
   begin
      ReAllocMem(pBitmap,0);
   end;

end;

//------------------------------------------------------------------------------
procedure BTFFmpegVideoPlayer.Seek(ms:int64);
var rc,si,flags:integer;
    ts:int64;
begin
   if not FPlaying then exit;
   if (ms<0) or (Duration<=0) or (ms>Duration) then exit;
   if FSeeking then exit;
   FSeeking:=true;
   try
      StopTimer;
   // we stop the AudioThread if it is not in pause-mode
      if (FAudioStreamIndex>=0) and not FPausing then
      begin
         BSound.Pause(bshand);
//      SDL_PauseAudio(1);
      end;
      FReadThreadSleeping:=false;
      FReadThreadSleep:=true;

      FAudioQueue.FUnblockPut:=true;
      FAudioQueue.WakeUpPut;
      FVideoQueue.FUnblockPut:=true;
      FVideoQueue.WakeUpPut;

   // wait for sleeping readthread
      while not FReadThreadSleeping do sleep(10);

      FAudioQueue.FUnblockPut:=false;
      FVideoQueue.FUnblockPut:=false;

      si:=-1;
      ts:=ms*int64(1000);
      if FFormatCtx.start_time <> AV_NOPTS_VALUE then ts:=ts+FFormatCtx.start_time;
  {
   if FAudioStreamIndex>=0 then begin
      si:=FAudioStreamIndex;
      ts:=round(ms /(1000*FAudioTimeBase));
   end else
   if FVideoStreamIndex>=0 then begin
      si:=FVideoStreamIndex;
      ts:=round(ms /(1000* FVideoTimeBase));
   end;
  }
  {
    logcount:=0;
    enablelog:=true;
    wLog('Seek >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>');
  }
//   avformat_flush(FFormatCtx);
      flags:=AVSEEK_FLAG_ANY;
//   flags:=0;

//      rc:=avformat_seek_file(FFormatCtx,si,int64.MinValue,ts,int64.MaxValue,flags);
      rc:=avformat_seek_file(FFormatCtx,si,0,ts,$FFFFFFFFFFFF,flags);

//   rc:=av_seek_frame(FFormatCtx,si,ts,flags);
      if rc>=0 then
      begin
   // we clear all queue and buffers
         FAudioQueue.Clear;
         FVideoQueue.Clear;
         FAudioPts:=-1;
         FVideoPts:=-1;
         FVideoSynchPts:=-1;
         FVideoSynchTime:=-1;
         FAudioBufSize:=0;
         FAudioBufIndex:=0;
         avformat_flush(FFormatCtx);
         if FVideoCtx<>nil then avcodec_flush_buffers(FVideoCtx);
         if FAudioCtx<>nil then avcodec_flush_buffers(FAudioCtx);
//      avformat_flush(FFormatCtx);
      end;

      FEof:=false;
      FDelayAudioQueueing:=true;  // audioqueing after the first video-frame
      FReadThreadSleep:=false;    // ReadThread can work again
      if (FVideoStreamIndex>=0) and Pausing then FVideoShowNextFrame:=true;  // we show in pausing the next frame
      StartTimer;
      if (FAudioStreamIndex>=0) then
      begin
         BSound.Resume(bshand);

//      SDL_PauseAudio(0);
      end;
//todo   Invalidate;
   finally
      FSeeking:=false;
   end;
end;

//------------------------------------------------------------------------------
procedure BTFFmpegVideoPlayer.SetAVolume(value:longword);
begin
   if Value > 255 then  Value := 255;
   FVolume := Value;
   if self.Playing then
   begin
      if bshand <>0 then BSound.SetVolume(bshand,Value,Value);
   end;
end;

//------------------------------------------------------------------------------
procedure BTFFmpegVideoPlayer.Resize(Yp,Xp:longint; Xl,Yl:longword);
var pp:boolean;
begin
   pp := false;
   if FPlaying then
   begin
      pp := FPausing;
      if not pp then Pause;
   end;
   Xpos := Xp;
   Ypos := Yp;
   Xlng := Xl;
   Ylng := YL;
   if FPlaying then
   begin
      if not CalcDisplayContext then
      begin
         Stop;
         Exit;
      end;
      if not pp then  Resume;
   end;
end;

//------------------------------------------------------------------------------
function  BTFFmpegVideoPlayer.PlayEx(StartPos,EndPos:int64):boolean;
begin
   Result := Play;
   if Result then
   begin
      Seek(StartPos);
      FEndPos := EndPos;
   end;
end;


begin
   // Globals init
   InitSFOk:=false;
   InitSFCount:=0;
end.

