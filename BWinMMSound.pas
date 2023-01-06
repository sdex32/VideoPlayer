unit BWinMMSound;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses windows, SysUtils;

const
   BTWINMMSOUNDMAXCHANELS = 16; // danger dont change
   Flags_Loop   = $00000001;
   Flags_2buf   = $00000002;
   Flags_CBack            = $00000004;
   Flags_WinMMSound_Float = $00000008;


type
   USER_CALLBACK = function(b:pointer; data:pointer; len:longword):longint; stdcall;
   // if non zero close thread len is total bytes in buffer div by bps*chan to get samples

   _BTWinMMSoundChannel = record
      Handle :longword;
      PlayStatus :longword;
      WaveHdr :array [0..3] of pointer;
      CurHdr :longword;
      callback :pointer;
      cbdat :pointer;
      flags :longword;
      buffer :pointer;
      audio_sem :longword;
      threadId :longword;
      threadH :longword;
      rchan :longword;
      samf :longword;
      mcbuf :pointer;
//      cs : _RTL_CRITICAL_SECTION;
   end;
   _PBTWinMMSoundChannel = ^_BTWinMMSoundChannel;

   BTWinMMSound = class
      private
//         mcs : _RTL_CRITICAL_SECTION;
         aChData    :array[1..BTWINMMSOUNDMAXCHANELS] of _BTWinMMSoundChannel;
         function    _GoodHand(hand:longword):boolean;
         procedure   _FreeBuf(indx:longword);
      public
         constructor Create;
         destructor  Destroy; override;
//         function    Device_Add
//         procedure   Device_Active
//         procedure   Device_Close
//         procedure   Device_Volume

         procedure   SetVolume(hand, L, R:longword);
         procedure   Pause(hand :longword);
         procedure   Resume(hand :longword);
         procedure   Play(hand :longword);
         procedure   Stop(hand :longword);
         function    OpenRawSound(freq,chan,bits,vol,flg:longword; callback:pointer; cb_data:pointer) :longword;
         procedure   Close(hand :longword);
   end;





implementation





// micro MMsystems
type
  PWaveFormatEx = ^TWaveFormatEx;
  TWAVEFORMATEX = packed record
    wFormatTag: Word;       { format type }
    nChannels: Word;        { number of channels (i.e. mono, stereo, etc.) }
    nSamplesPerSec: DWORD;  { sample rate }
    nAvgBytesPerSec: DWORD; { for buffer estimation }
    nBlockAlign: Word;      { block size of data }
    wBitsPerSample: Word;   { number of bits per sample of mono data }
    cbSize: Word;           { the count in bytes of the size of }
  end;
  PWaveHdr = ^TWaveHdr;
  TWaveHdr = record
    lpData: PChar;              { pointer to locked data buffer }
    dwBufferLength: DWORD;      { length of data buffer }
    dwBytesRecorded: DWORD;     { used for input only }
    dwUser: DWORD;              { for client's use }
    dwFlags: DWORD;             { assorted flags (see defines) }
    dwLoops: DWORD;             { loop control counter }
    lpNext: PWaveHdr;           { reserved for driver }
    reserved: DWORD;            { reserved for driver }
  end;


function waveOutOpen(lphWaveOut: pointer; uDeviceID: longword;
  lpFormat: PWaveFormatEx; dwCallback, dwInstance, dwFlags: longword): longint; stdcall; external 'winmm.dll' name 'waveOutOpen';
function waveOutClose(hWaveOut: longword): longint; stdcall; external 'winmm.dll' name 'waveOutClose';
function waveOutReset(hWaveOut: longword): longint; stdcall; external 'winmm.dll' name 'waveOutReset';
function waveOutPrepareHeader(hWaveOut: longword; lpWaveOutHdr: pointer;
  uSize: longword): longint; stdcall; external 'winmm.dll' name 'waveOutPrepareHeader';
function waveOutUnprepareHeader(hWaveOut: longword; lpWaveOutHdr: pointer;
  uSize: longword): longint; stdcall; external 'winmm.dll' name 'waveOutUnprepareHeader';
function waveOutWrite(hWaveOut: longword; lpWaveOutHdr: pointer;
  uSize: longword): longint; stdcall; external 'winmm.dll' name 'waveOutWrite';
function waveOutSetVolume(hWaveOut, dwVolume: longword): longint;  stdcall; external 'winmm.dll' name 'waveOutSetVolume';
function waveOutPause(hWaveOut: longword): longint; stdcall; external 'winmm.dll' name 'waveOutPause';
function waveOutRestart(hWaveOut: longword): longint; stdcall; external 'winmm.dll' name 'waveOutRestart';

const
       NUMBUFFERS = 4;


//------------------------------------------------------------------------------
procedure waveOutProc(hwo:longword; uMsg,dwInstance,dwParam1,dwParam2:longword); stdcall;
var obj: _PBTWinMMSoundChannel;
begin
   if (uMsg = $3BD {WOM_DONE}) then
   begin
      obj := pointer(dwInstance);
      ReleaseSemaphore(obj.audio_sem, 1, nil);
   end;
end;

type feear = array[0..7] of single;
     pfeear = ^feear;

const a5:single = 0.5;
      a4:single = 0.4;

function waveOutThread(a:longword):longint; stdcall;
var obj: _PBTWinMMSoundChannel;
    cb:USER_CALLBACK;
    C_hdr,B_size:longword;

    function FillData :longint;
    var i:longword;
    sbuf:pfeear;
    dbuf:pfeear;
    samp:longword;
    buf:longword;
    c:single;
    begin
       Result := 0;
       if obj.samf = 3 then
       begin
         //B_size chanels 1-1 2-2 3-2 4-2 all after sterea B-size is anly stereo
//bobi         buf := ( B_Size div 2 ) * obj.rchan; {Bufsize div Stereo = buf size for mono }
          buf := B_Size;
          if obj.rchan < 3 then  // Mono Stereo
          begin
             Result := cb(obj.cbdat, PWaveHdr(obj.WaveHdr[C_Hdr]).lpData, buf {B_Size} );
          end else begin
             buf := ( B_Size div 2 ) * obj.rchan; {Bufsize div Stereo = buf size for mono }
             samp := B_Size div 8;  // Stereo by BPS(float 4 byte)  2*4 = 8
             Result := cb(obj.cbdat, obj.mcbuf, buf );
             if Result = 1 then Exit;

             sbuf := obj.mcbuf;
             dbuf := pointer(PWaveHdr(obj.WaveHdr[C_Hdr]).lpData);
             case obj.rchan of
               3: begin {FL FR LFE (2.1 surround)}
                  end;
               4: begin { FL FR BL BR (quad) }
                     for i := 1 to samp do
                     begin
                        dbuf[0] := (sbuf[0] + sbuf[2]) * 0.5;
                        dbuf[1] := (sbuf[1] + sbuf[3]) * 0.5;
                        dbuf := pointer(longword(dbuf) + 8);
                        sbuf := pointer(longword(sbuf) + 16);
                     end;
                  end;
               5: begin { FL FR FC BL BR  (quad + center) }
                  end;
               6: begin { FL FR FC LFE SL SR  (5.1 surround - last two can also be BL BR) }
                     {
                     asm
                        push ecx
                        push ebx
                        push edx
                        mov  ecx, samp
                        mov  ebx, sbuf
                        mov  edx, dbuf
                     @@aa:
                        fld  dword [ebx+8]
                        fmul a5
                        fld  dword [ebx]
                        fadd dword [ebx+16]
                        fadd st, st(1)
                        fmul a4
                        fstp dword [edx]
                        fld  dword [ebx+4]
                        fadd dword [ebx+20]
                        faddp st(1), st
                        fmul a4
                        fstp dword [edx+4]
                        add   ebx, 24
                        add   edx, 8
                        loop @@aa
                        pop edx
                        pop ebx
                        pop ecx
                     end;
                     }
                     for i := 1 to samp do
                     begin
                        c := sbuf[2] * 0.5;
                        dbuf[0] := (sbuf[0] + sbuf[4] + c ) * 0.4;  // /2.5
                        dbuf[1] := (sbuf[1] + sbuf[5] + c ) * 0.4;
                        dbuf := pointer(longword(dbuf) + 8);
                        sbuf := pointer(longword(sbuf) + 24);
                     end;

                  end;
               7: begin { FL FR FC LFE BC SL SR (6.1 surround) }
                  end;
               8: begin { FL FR FC LFE BL BR SL SR  (7.1 surround) }
                     for i := 1 to samp do
                     begin
                     {
                        const float surround_left_distributed = src[6] * 0.5f;
                        const float surround_right_distributed = src[7] * 0.5f;
                        dst[0] = (src[0] + surround_left_distributed) / 1.5f;  /* FL */
                        dst[1] = (src[1] + surround_right_distributed) / 1.5f;  /* FR */
                        dst[2] = src[2] / 1.5f; /* CC */
                        dst[3] = src[3] / 1.5f; /* LFE */
                        dst[4] = (src[4] + surround_left_distributed) / 1.5f;  /* BL */
                        dst[5] = (src[5] + surround_right_distributed) / 1.5f;  /* BR */
                      }
                        dbuf[0] := (sbuf[0] + sbuf[4] + sbuf[6] + sbuf[2])*0.2;
                        dbuf[1] := (sbuf[1] + sbuf[5] + sbuf[7] + sbuf[2])*0.2;
                        dbuf := pointer(longword(dbuf) + 8);
                        sbuf := pointer(longword(sbuf) + 24);
                     end;
                  end;
             end;
          end;
       end else begin
 //bobi         buf := ( B_Size div 2 ) * obj.rchan; {Bufsize div Stereo = buf size for mono }
          buf := B_Size;
          if obj.rchan < 3 then  // Mono Stereo
          begin
             Result := cb(obj.cbdat, PWaveHdr(obj.WaveHdr[C_Hdr]).lpData, buf {B_Size} );
          end;
       end;
    end;

    procedure Close_sem;
    begin
       FileClose(obj.audio_sem); { *Converted from CloseHandle* }
       obj.audio_sem := 0;
       obj.threadH := 0;
       obj.threadId := 0;
       obj.PlayStatus := 2; //prepared
    end;

begin
   Result := 0;
   obj := pointer(a);
   cb := pointer(obj.callback);

//   EnterCriticalSection(obj.cs);

   obj.audio_sem := CreateSemaphore(nil, NUMBUFFERS - 1, NUMBUFFERS, nil);

   C_hdr := obj.CurHdr;
   B_size := PWaveHdr(obj.WaveHdr[C_Hdr]).dwBufferLength;

   //1
   waveOutUnPrepareHeader(obj.Handle, obj.WaveHdr[C_Hdr], sizeof(TWaveHDR));
   if FillData = 1 then
   begin
      Close_sem;
//      LeaveCriticalSection(obj.cs);
      Exit; // terminate thread;
   end;
   waveOutPrepareHeader(obj.Handle, obj.WaveHdr[C_Hdr], sizeof(TWaveHDR));
   waveOutWrite(obj.Handle, obj.WaveHdr[C_Hdr], sizeof(TWaveHDR));
//   obj.CurHdr := (obj.CurHdr + 1 ) mod NUMBUFFERS;  //next
   C_Hdr := (C_Hdr + 1 ) and 3;  //next

   //2
   waveOutUnPrepareHeader(obj.Handle, obj.WaveHdr[C_Hdr], sizeof(TWaveHDR));
//   cb(obj.cbdat, PWaveHdr(obj.WaveHdr[C_Hdr]).lpData, B_Size );
   if FillData = 1 then
   begin
      Close_sem;
//      LeaveCriticalSection(obj.cs);
      Exit; // terminate thread;
   end;
   waveOutPrepareHeader(obj.Handle, obj.WaveHdr[C_Hdr], sizeof(TWaveHDR));
   waveOutWrite(obj.Handle, obj.WaveHdr[C_Hdr], sizeof(TWaveHDR));
   C_Hdr := (C_Hdr + 1 ) and 3;  //next

   //3
   waveOutUnPrepareHeader(obj.Handle, obj.WaveHdr[C_Hdr], sizeof(TWaveHDR));
//   cb(obj.cbdat, PWaveHdr(obj.WaveHdr[C_Hdr]).lpData, B_Size );
   if FillData = 1 then
   begin
      Close_sem;
//      LeaveCriticalSection(obj.cs);
      Exit; // terminate thread;
   end;
   waveOutPrepareHeader(obj.Handle, obj.WaveHdr[C_Hdr], sizeof(TWaveHDR));
   waveOutWrite(obj.Handle, obj.WaveHdr[C_Hdr], sizeof(TWaveHDR));
   C_Hdr := (C_Hdr + 1 ) and 3;  //next



//   waveOutUnPrepareHeader(obj.Handle, obj.WaveHdr[obj.CurHdr], sizeof(TWaveHDR));
//   cb(obj.cbdat, PWaveHdr(obj.WaveHdr[obj.CurHdr]).lpData, PWaveHdr(obj.WaveHdr[obj.CurHdr]).dwBufferLength );
//   waveOutPrepareHeader(obj.Handle, obj.WaveHdr[obj.CurHdr], sizeof(TWaveHDR));
//   waveOutWrite(obj.Handle, obj.WaveHdr[obj.CurHdr], sizeof(TWaveHDR));
//   obj.CurHdr := (obj.CurHdr + 1 ) mod NUMBUFFERS;  //next

//   WaitForSingleObject(obj.audio_sem, INFINITE);

   while(obj.PlayStatus = 1) do
   begin
      //4
      waveOutUnPrepareHeader(obj.Handle, obj.WaveHdr[C_Hdr], sizeof(TWaveHDR));
//      cb(obj.cbdat, PWaveHdr(obj.WaveHdr[C_Hdr]).lpData, B_Size );
      if FillData = 1 then
      begin
         Close_sem;
//         LeaveCriticalSection(obj.cs);
         Exit; // terminate thread;
      end;

      waveOutPrepareHeader(obj.Handle, obj.WaveHdr[C_Hdr], sizeof(TWaveHDR));
      // Put to play
      if obj.PlayStatus = 1 then
      begin
         waveOutWrite(obj.Handle, obj.WaveHdr[C_Hdr], sizeof(TWaveHDR));
         C_Hdr := (C_Hdr + 1 ) and 3;  //next
         WaitForSingleObject(obj.audio_sem, INFINITE);
      end;

   end;


   Close_sem;
//   LeaveCriticalSection(obj.cs);

//      /* Wait for the audio to drain. */
//    SDL_Delay(((device->spec.samples * 1000) / device->spec.freq) * 2);

end;

//------------------------------------------------------------------------------
constructor BTWinMMSound.Create;
var j,i:longword;
begin
//   InitializeCriticalSection(mcs);
//   InitializeCriticalSectionandSpinCount(mcs,2);
   for j:= 1 to BTWINMMSOUNDMAXCHANELS do
   begin
      aChData[j].PlayStatus := 0;
      aChData[j].buffer := nil;
      aChData[j].mcbuf := nil;
//      aChData[j].cs := mcs;
      for i :=0 to 3 do  aChData[j].WaveHdr[i] := nil;
   end;
end;

//------------------------------------------------------------------------------
destructor  BTWinMMSound.Destroy;
var i:longword;
begin
   for i := 1 to BTWINMMSOUNDMAXCHANELS do
   begin
      if aChData[i].PlayStatus <> 0 then Close(i);
   end;
//   DeleteCriticalSection(mcs);
   inherited;
end;

//------------------------------------------------------------------------------
function    BTWinMMSound._GoodHand(hand:longword):boolean;
begin
   Result := false;
   if (hand = 0) or (hand > BTWINMMSOUNDMAXCHANELS) then Exit;
   if aChData[hand].PlayStatus = 0 then Exit;
   if aChData[hand].Handle <>  0 then Result := true;
end;

//------------------------------------------------------------------------------
procedure   BTWinMMSound._FreeBuf(indx:longword);
var i:longword;
begin
   for i :=0 to NUMBUFFERS - 1 do
   begin
      if aChData[indx].WaveHdr[i] <> nil then ReallocMem(aChData[indx].WaveHdr[i],0);
      aChData[indx].WaveHdr[i] := nil;
   end;
   if aChData[indx].Buffer <> nil then ReallocMem(aChData[indx].Buffer,0);
   aChData[indx].Buffer := nil;
   if aChData[indx].mcbuf <> nil then ReallocMem(aChData[indx].mcbuf,0);
   aChData[indx].mcbuf := nil;
end;

//------------------------------------------------------------------------------
procedure   BTWinMMSound.Close(hand:longword);
begin
   if _GoodHand(hand) then
   begin
      Stop(hand);
      aChData[hand].PlayStatus := 0; // free;
      _FreeBuf(hand);
   end;
end;

//------------------------------------------------------------------------------
procedure   BTWinMMSound.SetVolume(hand, L,R:longword);
var a:longword;
begin
   if _GoodHand(hand) then
   begin
      if L > 255 then L := 255;
      if R > 255 then R := 255;
      a := (L shl 8) or ((R shl 8) shl 16);
      waveOutSetVolume(aChData[hand].Handle,a);
   end;
end;

//------------------------------------------------------------------------------
procedure   BTWinMMSound.Pause(hand:longword);
begin
   if _GoodHand(hand) then waveoutPause(aChData[hand].Handle)
end;

//------------------------------------------------------------------------------
procedure   BTWinMMSound.Resume(hand:longword);
begin
   if _GoodHand(hand) then  waveoutRestart(aChData[hand].Handle);
end;

//------------------------------------------------------------------------------
function    BTWinMMSound.OpenRAWsound(freq,chan,bits,vol,flg:longword; callback:pointer; cb_data:pointer) :longword;
var i,j,a,bufsize,samples,pow2:longword;
    wfx :TWAVEFORMATEX;
    errcode :longint;
//    cb:USER_CALLBACK;
begin
   Result := 0;

   for j:= 1 to BTWINMMSOUNDMAXCHANELS do
   begin
      if aChData[j].PlayStatus = 0 then // free
      begin
//         cb := CallBack;
         if CallBack = nil then Exit;

         aChData[j].flags := Flg; //???????
         aChData[j].callback := CallBack;
         aChData[j].cbdat := cb_data; // call back user data
         aChData[j].handle:= 0;  // handle

         FillChar(wfx,sizeof(TWAVEFORMATEX),0);
         if (Flg and Flags_WinMMSound_Float) <> 0 then wfx.wFormatTag := 3 {WAVE_FORMAT_IEEE_FLOAT}
                                                  else wfx.wFormatTag := 1; // PCM standart.

         aChData[j].samf := wfx.wFormatTag ;  //TODO
         aChData[j].rchan := chan; // for conversion

         if chan > 2 then
         begin
            chan := 2;
            if wfx.wFormatTag <> 3 then Exit; // I dont have converters
         end;

         wfx.nChannels := Chan;  // 2 setereo 1 moni
         wfx.nSamplesPerSec := Freq; //44100;
         wfx.wBitsPerSample := Bits;  //16;
         wfx.nBlockAlign := wfx.nChannels * (wfx.wBitsPerSample div 8);
         wfx.nAvgBytesPerSec := wfx.nSamplesPerSec*wfx.nBlockAlign;
         wfx.cbSize := 0;

         samples := round((freq / 1000) * 25); //25);  //46 ms on any freq
         pow2 := 1;
         while (pow2 < samples) do pow2 := pow2 * 2;
         samples := pow2;
         bufsize := (wfx.wBitsPerSample div 8) * wfx.nChannels * samples;
       //  bufsize := 8192;

         errCode := waveOutOpen(@aChData[j].handle, UINT(-1){WAVE_MAPPER},@wfx,
                    longword(@waveOutProc),  // callback
                    longword(@aChData[j]),      // User data.
                    $00030000 {CALLBACK_FUNCTION});

         if (errCode =  0 {MMSYSERR_NOERROR}) then
         begin

            aChData[j].mcbuf := nil;
            ReallocMem(aChData[j].mcbuf, ( bufsize div 2) * aChData[j].rchan ); // from stereo to multi chanel buf
            if aChData[j].mcbuf = nil then Exit;

            aChData[j].buffer := nil;
//            ReallocMem(aChData[j].buffer, 2 {NUM_BUFFERS} * bufsize);
            ReallocMem(aChData[j].buffer, NUMBUFFERS * bufsize);
            if aChData[j].buffer = nil then
            begin
               _FreeBuf(j);
               Exit;
            end;

            aChData[j].CurHdr := 0;
            for i := 0 to NUMBUFFERS -1 do
            begin
               aChData[j].WaveHdr[i] := nil;
               ReallocMem(aChData[j].WaveHdr[i], sizeof(TWaveHDR));
               if (aChData[j].WaveHdr[i] = nil) then
               begin
                  _FreeBuf(j);
                  Exit;
               end;

               fillchar(aChData[j].WaveHdr[i]^,sizeof(TWaveHDR),0);  // header data clear
               PWaveHdr(aChData[j].WaveHdr[i]).lpData := pointer(longword(aChData[j].buffer) + (bufsize * i));
               PWaveHdr(aChData[j].WaveHdr[i]).dwBufferLength := bufsize;

               WaveOutPrepareHeader(aChData[j].handle, aChData[j].WaveHdr[0], sizeof(TWaveHdr));

            end;

            aChData[j].audio_sem := 0;
            aChData[j].threadH := 0;

//            cb(aChData[j].CBdat,PWaveHdr(aChData[j].WaveHdr[aChData[j].CurHdr]).lpData,PWaveHdr(aChData[j].WaveHdr[aChData[j].CurHdr]).dwBufferLength);

            // start to play it

//            WaveOutWrite(aChData[j].handle, aChData[j].WaveHdr, sizeof(TWaveHdr)); // play this


               // put in tail second buffer
             //  WaveOutPrepareHeader(aChData[j].handle, aChData[j].WaveHdr2, sizeof(TWaveHdr));
//               WaveOutWrite(aChData[j].handle, aChData[j].WaveHdr2, sizeof(TWaveHdr)); // play this

            if Vol > 255 then Vol := 255;
            a := (Vol shl 8) or ((Vol shl 8) shl 16);
            waveOutSetVolume(aChData[j].Handle,a);

            aChData[j].PlayStatus := 2;  // state sound loaded

            Result := j;

         end;
         break;
      end;
   end;


end;


//------------------------------------------------------------------------------
procedure BTWinMMSound.Play(hand :longword);
//var cb:USER_CALLBACK;
begin
   if _GoodHand(hand) then
   begin
      if aChData[hand].PlayStatus = 2 then
      begin
      aChData[hand].PlayStatus := 1;
//      cb := pointer(aChData[hand].callback);
//      cb(aChData[hand].cbdat, PWaveHdr(aChData[hand].WaveHdr[aChData[hand].CurHdr]).lpData, PWaveHdr(aChData[hand].WaveHdr[aChData[hand].CurHdr]).dwBufferLength );
//      waveOutWrite(aChData[hand].Handle, aChData[hand].WaveHdr[aChData[hand].CurHdr], sizeof(TWaveHDR));
      aChData[hand].CurHdr := 0;
//      aChData[hand].audio_sem := CreateSemaphore(nil, NUMBUFFERS - 1, NUMBUFFERS, nil);
      aChData[hand].threadH := CreateThread(nil,0,@WaveOutThread,@aChData[hand],0,aChData[hand].ThreadID);
      SetThreadPriority(aChData[hand].threadH, 15); //TIME CRITICYL
      end;
   end;
end;


//------------------------------------------------------------------------------
procedure BTWinMMSound.Stop(hand :longword);
var i:longword;
begin
   if _GoodHand(hand) then
   begin
      aChData[hand].PlayStatus := 2; // free;
//      waveOutPause(aChData[hand].Handle); //this hang the system
//      waveOutReset(aChData[hand].Handle);
      waveOutClose(aChData[hand].Handle);
      for i :=0 to NUMBUFFERS - 1 do
      begin
         waveOutUnPrepareHeader(aChData[hand].Handle, aChData[hand].WaveHdr[i], sizeof(TWaveHDR));
      end;
      if aChData[hand].threadH <> 0 then
      begin
         TerminateThread(aChData[hand].threadH,0);
         FileClose(aChData[hand].threadH); { *Converted from CloseHandle* }
         aChData[hand].threadH := 0;
         aChData[hand].threadId := 0;
      end;
      if aChData[hand].audio_sem <> 0 then
      begin
         FileClose(aChData[hand].audio_sem); { *Converted from CloseHandle* }
         aChData[hand].audio_sem := 0;
      end;
   end;
end;


(*

super small midi uses mmsound

var
  mo: HMIDIOUT;

const
  MIDI_NOTE_ON = $90;
  MIDI_NOTE_OFF = $80;
  MIDI_CHANGE_INSTRUMENT = $C0;

function MIDIEncodeMessage(Msg, Param1, Param2: byte): integer;
begin
  result := Msg + (Param1 shl 8) + (Param2 shl 16);
end;

procedure NoteOn(NewNote, NewIntensity: byte);
begin
  midiOutShortMsg(mo, MIDIEncodeMessage(MIDI_NOTE_ON, NewNote, NewIntensity));
end;

procedure NoteOff(NewNote, NewIntensity: byte);
begin
  midiOutShortMsg(mo, MIDIEncodeMessage(MIDI_NOTE_OFF, NewNote, NewIntensity));
end;

procedure SetInstrument(NewInstrument: byte);
begin
  midiOutShortMsg(mo, MIDIEncodeMessage(MIDI_CHANGE_INSTRUMENT, NewInstrument, 0));
end;

procedure InitMIDI;
begin
  midiOutOpen(@mo, 0, 0, 0, CALLBACK_NULL);
  midiOutShortMsg(mo, MIDIEncodeMessage(MIDI_CHANGE_INSTRUMENT, 0, 0));
end;


InitMidi;
NoteOn(50, 127);
Sleep(500);
SetInstrument(60);
NoteOn(60, 127);
Sleep(500);
NoteOff(60, 127);
SetInstrument(80);
NoteOn(70, 127);
Sleep(500);
NoteOff(70, 127);
SetInstrument(90);
NoteOn(80, 127);
Sleep(500);
NoteOff(80, 127);
SetInstrument(100);
NoteOn(90, 127);
Sleep(500);
NoteOff(90, 127);
SetInstrument(12);
NoteOn(40, 127);
Sleep(1000);
NoteOff(40, 127);
midiOUTclose(mo);

*)

  {
type  BTWinMMSoundDevice = class
         private
         public
            constructor Create;
            destructor  Destroy; override;
            function    Init(freq,chan,bits,vol,flg:longword; callback:pointer; cb_data:longword) :longword;
            procedure   Close;
            procedure   Active(on_off:boolean);
            procedure   Volume(value:longword);
      end;




function  BWinMMSound_OpenDevice(freq,chan,bits,vol,flg:longword; callback:pointer; cb_data:longword) :longword;
begin

end;

function  BWinMMSound_CloseDevice(hnad:longword) :longword;
begin

end;
   }













end.
