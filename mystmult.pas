program mystmult;

{                  Swiss Army Knife for Mystic BBS Program

  Copyright (C) 2018 Dan Richter (aka Black Panther) dan@castlerockbbs.com

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.
}


{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads, clocale
  {$ENDIF}{$ENDIF}
  Classes, sysutils, strutils, DateUtils, crt;

Const
  prog   = 'RCS MysticBBS MultiTool';
  ver    = '1.0.1.4';
  author = 'DRPanther(RCS)';

Type
  echorecord = Record
     e_name  : String[80];
     e_icount: Integer;
     e_ocount: Integer;
  end;

Var
  rec      : echorecord;
  sortdata : array [1..500] of echorecord;
  echo     : echorecord;
  f        : file of echorecord;
  o        : file of echorecord;
  logfile  : textfile;
  mutillog : textfile;
  echorpt  : textfile;
  info     : TSearchRec;
  logdate  : string;
  lyear    : string;
  lmonth   : string;
  lday     : string;
  logpath  : string;
  runpath  : string;
  s        : string;
  s1       : string;
  x        : integer;
  nodebool : boolean;
  logbool  : boolean;
  foundrec : Integer;
  findname : string[80];
  foundflag: Integer;
  ucs      : string;
  uci      : Integer;
  III      : Integer;
  currec   : Integer;
  lastrec  : Integer;
  ti       : Integer;
  ts       : Integer;
  n        : Integer;
  totalin  : Integer;
  totalout : Integer;


procedure logmainthelp;
begin
  Writeln;
  Writeln('-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-');
  Writeln;
  Writeln(prog+' '+ver);
  Writeln;
  Writeln('RCS Log Maintenance ');
  Writeln;
  Writeln('Make sure you are running this program from the Mystic directory!');
  Writeln;
  Writeln('Command Line Parameters:');
  Writeln(' L - Perform Log Backup');
  Writeln(' N - Create Node Report');
  Writeln(' H - This help file');
  Writeln;
  Writeln(author);
  Writeln;
  Writeln('-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-');
  Writeln;
  write(logfile,FormatDateTime('yyyy mmm dd hh:nn:ss',(Now)));
  writeln(logfile,' Program run and displayed Help file!');
  CloseFile(logfile);
  ChDir(runpath);
  writeln(runpath);
  writeln(GetCurrentDir);
  RenameFile('logmaint.log','logs'+PathDelim+'logmaint.log');
  halt;
end;


procedure startup;
begin
  x:=0;
  totalin:=0;
  totalout:=0;
  runpath:=GetCurrentDir;
  RenameFile('logs'+PathDelim+'logmaint.log','logmaint.log');

  assign(LogFile,'logmaint.log');
  if fileexists('logmaint.log')=false then Rewrite(logfile)
    else append(logfile);

  WriteLn(logfile,' -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- ');
  write(logfile,FormatDateTime('yyyy mmm dd hh:nn:ss',(Now)));
  writeln(logfile,' ',prog,' ',ver);
  write(logfile,FormatDateTime('yyyy mmm dd hh:nn:ss',(Now)));
  write(logfile,' ');
  for x:=0 To ParamCount do begin
    write(logfile,ParamStr(x));
    write(logfile,' ');
  end;
  writeln(logfile);
  if ParamCount=0 then logmainthelp;
  x:=0;
  for x:=1 To ParamCount do
  begin
  case paramstr(x) of
    'h','H','?' : logmainthelp;
    'n','N'     : nodebool:=true;
    'l','L'     : logbool:=true;

  end;
  end;
end;

procedure createdirectory;
begin
  ChDir('logs');
  if not DirectoryExists(lyear) then Mkdir(lyear);
  write(logfile,FormatDateTime('yyyy mmm dd hh:nn:ss',(Now)));
  writeln(logfile,' Created directory ',lyear);
  ChDir(lyear);
  if not DirectoryExists(lmonth) then Mkdir(lmonth);
  write(logfile,FormatDateTime('yyyy mmm dd hh:nn:ss',(Now)));
  writeln(logfile,' Created directory ',lmonth);
  ChDir(lmonth);
  if not DirectoryExists(lday) then Mkdir(lday);
  write(logfile,FormatDateTime('yyyy mmm dd hh:nn:ss',(Now)));
  writeln(logfile,' Created directory ',lday);
  ChDir(lday);
  logpath:=GetCurrentDir;
  writeln(logpath);
  write(logfile,FormatDateTime('yyyy mmm dd hh:nn:ss',(Now)));
  writeln(logfile,' Changed to directory ',logpath);

end;

procedure movelogs;
begin
  ChDir('..');
  ChDir('..');
  ChDir('..');
  If FindFirst('*.log',faAnyFile,Info)=0 then
  begin
    Repeat
      RenameFile(info.Name,(logpath+PathDelim+info.Name));
        write(logfile,FormatDateTime('yyyy mmm dd hh:nn:ss',(Now)));
        writeln(logfile,' Moved Log file ',info.Name);

    until FindNext(info)<>0;
  end;
  FindClose(Info);
end;

Function UpCaseStr(us:string): String;
begin
  ucs:=us;
  for uci:=1 to Length(ucs) do
    ucs[uci] := UpCase(ucs[uci]);
  UpCaseStr:=ucs;
end;

Function FindRecord(N:String): Integer; { Find Record Routine }
Begin
  foundrec:=0;
  currec:=1;
  III:=0;
  foundflag:=0;
  repeat
   III := III + 1;
   Seek(f,III);
   Read(f,echo);
   If upcasestr(echo.e_name) = upcasestr(N) then
   begin
     foundrec:=III;
     foundflag:=1;
   end;
  until (III=lastrec) or (foundrec>0) or (eof(f));
 if foundrec=0 then
  Begin
   foundflag:=0;
  End;
 if foundrec>0 then
  begin
   currec:=III;
   foundflag:=1;
  end;
 if foundrec=0 then foundflag:=0;
 if foundrec>0 then FindRecord:=III;
End;

Procedure EditRecord;     { Edit Current Record Routine }
Begin
  seek(f,currec);
  Read(f,echo);
  inc(echo.e_icount);
  seek(f,currec);
  write(f,echo);
  write(logfile,FormatDateTime('yyyy mmm dd hh:nn:ss',(Now)));
  writeln(logfile,' Adding incoming count to echo ',echo.e_name);

End;

Procedure EditRecordOut;     { Edit Current Record Routine }
Begin
  seek(f,currec);
  Read(f,echo);
  inc(echo.e_ocount);
  seek(f,currec);
  write(f,echo);
  write(logfile,FormatDateTime('yyyy mmm dd hh:nn:ss',(Now)));
  writeln(logfile,' Adding outgoing count to echo ',echo.e_name);

End;

procedure nodestat;

begin
  chdir(runpath);
  s:='';
  s1:='';
  x:=0;
  ts:=0;
  rec.e_icount:=0;
  echo.e_icount:=0;
  Assign(f,'echocnt.dat');
  rewrite(f);
  seek(f,1);
  rec.e_name:='';
  echo.e_name:='';
  rec.e_ocount:=0;
  echo.e_ocount:=0;
  write(f,rec);
 Assign(mutillog,runpath+PathDelim+'logs'+PathDelim+'mutil.log');
  {$I-}
  reset(mutillog);
  {$I+}
  if IOresult<>0 then
    begin
    writeln('File mutil.log, could not be opened');
    halt;
    end;
  write(logfile,FormatDateTime('yyyy mmm dd hh:nn:ss',(Now)));
  writeln(logfile,' Processing Incoming Echomail');
  While not eof(mutillog) do
  begin
    readln(mutillog,s);
    if AnsiContainsStr(s,' Import #') then begin

      if AnsiContainsStr(s,' to ') then begin
      x:=(pos('to ',s))+3;
        FindName:=(ExtractSubstr(s,x,[' ']));
        FindName:=UpCaseStr(FindName);
        inc(totalin);
        ti:=FindRecord(FindName);
        if foundflag=0 then
        begin
          echo.e_name:=FindName;
          echo.e_icount:=1;
          echo.e_ocount:=0;
          write(f,echo);
          write(logfile,FormatDateTime('yyyy mmm dd hh:nn:ss',(Now)));
          writeln(logfile,' Incoming Echomail in ',echo.e_name);
        end;
        if foundflag=1 then
        begin
          currec:=ti;
          EditRecord;
        end;
        echo.e_icount:=0;
        echo.e_name:='';
        currec:=1;
      end;
    end;
    if AnsiContainsStr(s,' Export ') then begin
      if AnsiContainsStr(s,' to ') then begin
        x:=(pos('Export ',s))+7;
        FindName:=(ExtractSubstr(s,x,[' ']));
        if FindName<>'to' then begin
        FindName:=UpCaseStr(FindName);
        inc(totalout);
        ti:=FindRecord(FindName);
        if foundflag=0 then
        begin
          echo.e_name:=FindName;
          echo.e_ocount:=1;
          echo.e_icount:=0;
          write(f,echo);
          write(logfile,FormatDateTime('yyyy mmm dd hh:nn:ss',(Now)));
          writeln(logfile,' Outgoing Echomail in ',echo.e_name);
        end;
        if foundflag=1 then
        begin
          currec:=ti;
          EditRecordOut;
        end;
        echo.e_ocount:=0;
        echo.e_name:='';
        currec:=1;
        end;
      end;
    end;
  inc(ts);
  end;
  writeln(ts);
  n:=filesize(f);
  seek(f,n);
  lastrec:=filepos(f)-1;
  CloseFile(f);
  CloseFile(mutillog);
  //reportsort;
end;


procedure datecheck;
begin
  logdate:=(FormatDateTime('YYYY-M-D',(today-1)));
  lyear:=(FormatDateTime('YYYY',(today-1)));
  lmonth:=(FormatDateTime('M',(today-1)));
  lday:=(FormatDateTime('D',(today-1)));
end;

procedure reportsort;

var
  temp:integer;
  z   :integer;
  d   :integer;
begin
  z:=1;
  d:=1;
  temp:=lastrec+1;
  reset(f);
  seek(f,1);
  for d:=1 to lastrec do begin
    seek(f,d);
    read(f,echo);
    sortdata[d].e_name:=echo.e_name;
    sortdata[d].e_icount:=echo.e_icount;
    sortdata[d].e_ocount:=echo.e_ocount;
  end;
  for d:=1 to lastrec do begin
    for z:=d+1 to lastrec do begin
      if ((sortdata[d].e_icount)<>0)and((sortdata[z].e_icount)<>0) then begin
        if ((sortdata[d].e_icount)<(sortdata[z].e_icount))then begin
          sortdata[temp]:=sortdata[d];
          sortdata[d]:=sortdata[z];
          sortdata[z]:=sortdata[temp];
        end;
      end;
    end;
  end;
end;

procedure nodereport;
begin
  III:=1;
  reset(f);
  Assign(echorpt,'echorpt.rpt');
  rewrite(echorpt);
  write(logfile,FormatDateTime('yyyy mmm dd hh:nn:ss',(Now)));
  writeln(logfile,' Generating Report');
  writeln(echorpt,(PadCenter('RCS Echomail Report',78)));
  writeln(echorpt,(PadCenter('***********************',78)));
  writeln(echorpt);
  write(echorpt,(PadRight('Echo Name',38)));
  writeln(echorpt,'  Incoming     Outgoing');
  writeln(echorpt,'-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-');
  writeln(III);
  writeln(lastrec);
  For III:=2 to lastrec do
  begin
//    seek(f,III);
//    Read(f,echo);
    write(echorpt,PadRight(sortdata[III].e_name,40));
    write(echorpt,PadRight(IntToStr(sortdata[III].e_icount),10));
    write(echorpt,'   ');
    writeln(echorpt,sortdata[III].e_ocount);
  end;
  writeln(echorpt,'-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-');
  write(echorpt,'                                        ');
  write(echorpt,totalin);
  write(echorpt,'         ');
  writeln(echorpt,totalout);

  CloseFile(echorpt);
  CloseFile(f);
end;

procedure wrapup;
begin
  ChDir(runpath);
  write(logfile,FormatDateTime('yyyy mmm dd hh:nn:ss',(Now)));
  writeln(logfile,' Completed All Processes');
  CloseFile(logfile);
  RenameFile('logmaint.log','logs'+PathDelim+'logmaint.log');
//  CloseFile(logfile);

end;

{$R *.res}

begin
  startup;
  datecheck;
  if nodebool=true then begin
    nodestat;
    reportsort;
    nodereport;
  end;

  if logbool=true then begin
    createdirectory;
    movelogs;
  end;

  wrapup;
end.

