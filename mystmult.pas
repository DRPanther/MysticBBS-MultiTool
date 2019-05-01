program mystmult;

{                  Swiss Army Knife for Mystic BBS Program

  Copyright (C) 2018 Dan Richter (aka Black Panther(RCS)) dan@castlerockbbs.com

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
  //{$IFDEF WINDOWS}
  //windows
  //{$ENDIF}
  Classes, sysutils, strutils, DateUtils, crt, vinfo, versiontypes;

Const
  prog       = 'RCS MysticBBS Multi Tool';
  author     = 'DRPanther(RCS)';
  ConfigFile = 'rcs.ini';

Type
  echorecord = Record
     e_name  : String[50];
     e_domain: String[8];
     e_icount: Integer;
     e_ocount: Integer;
     e_dupe  : Integer;
  end;

  noderecord = Record
     n_node  : String[15];
     n_conne : Integer;
     n_ifile : Integer;
     n_ofile : Integer;
     n_isize : Integer;
     n_osize : Integer;
  end;

  netaddressrecord = Record
    zone   : integer;
    net    : integer;
    node   : integer;
    domain : string[8];
  end;

  othernets = Record
    zone   : integer;
    net    : integer;
    node   : integer;
    domain : string[8];
    matched: integer;
    nodes  : integer;
  end;

  mailerlog = Record
    mailer  : string[60];
    matches : integer;
  end;

Var
  rec            : echorecord;
  node           : noderecord;
  onet           : othernets;
  mail           : mailerlog;
  sortdata       : array [1..1000] of echorecord;
  nodedata       : array [1..100] of noderecord;
  onetdata       : array [1..500] of othernets;
  maildata       : array [1..100] of mailerlog;
  echo           : echorecord;
  f              : file of echorecord;
  e              : file of noderecord;
  t              : file of othernets;
  m              : file of mailerlog;
  logfile        : textfile;                            //mystmult.log
  mutillog       : textfile;                            //mutil.log
  mislog         : textfile;                            //mis.log
  fidopoll       : textfile;                            //fidopoll.log
  qwkpoll        : textfile;
  echorpt        : textfile;                            //echorpt.rpt
  noderpt        : textfile;                            //noderpt.rpt
  onetreport     : textfile;
  mailreport     : textfile;
  cffile         : textfile;                            //Config File
  BBSName        : string;                              //From config file
  Sysop          : string;                              //From config file
  MysticLogs     : string;                              //Path to Mystic logs directory
  LogsPath       : string;                              //Path to use for mystmult.log
  NetAddress     : array [1..50] of netaddressrecord;   //Array of the network addresses (5d)
  echoexclude    : array [1..10] of string;             //Array of echos to exclude from reports
  exclude        : integer;                             //Number of excluded echos
  networkexclude : array [1..10] of string;             //Array of Networks to exclude from reports
  netexclude     : integer;                             //Number of excluded networks
  NACounter      : integer;                             //Number of Network Addresses
  info           : TSearchRec;                          //Used for version infomation
  reportdate     : integer;
  logdate        : string;
  lyear          : string;
  lmonth         : string;
  lday           : string;
  logpath        : string;                              //Path to where to put the log files during backup
  runpath        : string;                              //Path where program was run from
  s              : string;
  s1             : string;
  x              : integer;
  MISNode        : integer;                             //Used to separate nodes in mis.log
  counter        : integer;
  echodate       : string;
  nodedate       : string;
  tempdomain     : string[8];
  echobool       : boolean;                             //User asked for echo report
  nodebool       : boolean;                             //User asked for node report
  logbool        : boolean;                             //User asked for log backup
  onetbool       : boolean;                             //User asked for other network report
  mailbool       : boolean;                             //User asked for mailer usage report
  esortmess      : boolean;                             //User wants echo report sorted by incoming messages
  esortnet       : boolean;                             //User wants echo report sorted by network
  esortdupe      : boolean;                             //User wants echo report sorted by incoming dupe messages
  esortout       : boolean;                             //User wants echo report sorted by outgoing messages
  esortecho      : boolean;                             //User wants echo report sorted by echo name
  foundrec       : Integer;
  findname       : string[80];
  foundflag      : Integer;
  ucs            : string;
  uci            : Integer;
  III            : Integer;
  currec         : Integer;
  lastrec        : Integer;
  ti             : Integer;
  ts             : Integer;
  n              : Integer;
  totalin        : Integer;                             //Total incoming messages
  totalout       : Integer;                             //Total outgoing messages
  totaldupe      : Integer;                             //Total incoming dupe messages
  sysos          : string;                              //Operating system and bit of compiling system
  ver            : string;                              //Version of program

procedure logmainthelp;
begin
  Writeln('-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-');
  Writeln(prog+' '+ver);
  Writeln;
  Writeln('Command Line Parameters:');
  Writeln(' L   - Perform Log Backup');
  Writeln(' E#  - Create Detailed Echo Report');
  Writeln('       Sort Echo Report by:');
  Writeln('       1 - Incoming Message count');
  Writeln('       2 - Message Network');
  Writeln('       3 - Duplicate Message count');
  Writeln('       4 - Outbound Message count');
  Writeln('       5 - Echo Name');
  Writeln(' N   - Create Detailed Node Report');
  Writeln(' T   - Generate a list of other networks');
  Writeln(' M   - Generate a list of mailers used to connect');
  Writeln(' D   - Number of day of information on reports');
  Writeln(' H   - This help file :)');
  Writeln;
  Write('Example...   ');
  if AnsiStartsStr('Linux',sysos) then begin
    writeln(sysos,':');
    writeln('./mystmult D7 N E1 L T M');
  end;
  if AnsiStartsStr('Windows',sysos) then begin
    writeln(sysos,':');
    writeln('mystmult D7 N E1 L T M');
  end;
  writeln('     This will run all five modes while sorting the echo report');
  writeln('     by the number of incoming messages, and report will contain');
  Writeln('     7 days of information.');
  Writeln;
  Writeln(author);
  Writeln('-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-');
  Writeln;
  write(logfile,FormatDateTime('yyyy mmm dd hh:nn:ss',(Now)));
  writeln(logfile,' Program run and displayed Help file!');
  CloseFile(logfile);
  ChDir(runpath);
  RenameFile('mystmult.log','logs'+PathDelim+'mystmult.log');
  halt(0);
end;

function OSVersion: String;
var
  SizeofPointe: string;
begin
  {$IFDEF LCLcarbon}
  OSVersion := 'Mac OS X 10.';
  {$ELSE}
  {$IFDEF Linux}
  OSVersion := 'Linux';
  {$ELSE}
  {$IFDEF UNIX}
  OSVersion := 'Unix';
  {$ELSE}
  {$IFDEF WINDOWS}
  OSVersion:= 'Windows';
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$ifdef CPU32}
      SizeofPointe:='/32';   // 32-bit = 32
  {$endif}
  {$ifdef CPU64}
      SizeofPointe:='/64';   // 64-bit = 64
  {$endif}
  sysos:=OSVersion+SizeofPointe;
end;

function ProductVersionToString(PV: TFileProductVersion): String;
   begin
     Result := Format('%d.%d.%d.%d', [PV[0],PV[1],PV[2],PV[3]])
   end;

procedure ProgVersion;
var
   Info: TVersionInfo;
begin
   Info := TVersionInfo.Create;
   Info.Load(HINSTANCE);
   ver:=(ProductVersionToString(Info.FixedInfo.FileVersion));
   Info.Free;
end;

Procedure ReadConfigFile;
var
  s       : string;
  counter : integer;
  x       : integer;
begin
  NetAddress[1].zone:=0;
  NetAddress[1].net:=0;
  NetAddress[1].node:=0;
  NetAddress[1].domain:='';
  counter:=1;
  NACounter:=1;
  x:=1;
  AssignFile(cffile, ConfigFile);
  try
  reset(cffile);
  While not eof(cffile) do begin
    s:='';
    readln(cffile, s);
    if AnsiStartsStr('BBS=',s) then begin
      Delete(s, 1, 4);
      BBSName:=s;
    end;
    if AnsiStartsStr('Sysop=',s) then begin
      Delete(s, 1, 6);
      Sysop:=s;
    end;
    if AnsiStartsStr('LogPath=',s) then begin
      Delete(s, 1, 8);
      LogsPath:=s;
    end;
    if AnsiStartsStr('MysticLogs=',s) then begin
      Delete(s,1,11);
      MysticLogs:=s;
    end;
    if AnsiStartsStr('NetAddress=',s) then begin
      Delete(s,1,11);
      x:=pos(':',s);
      NetAddress[counter].zone:=StrToInt(copy(s,1,x-1));
      x:=(pos(':',s)+1);
      NetAddress[counter].net:=StrToInt(ExtractSubStr(s,x,['/']));
      x:=(pos('/',s)+1);
      NetAddress[counter].node:=StrToInt(ExtractSubStr(s,x,['@']));
      x:=(pos('@',s)+1);
      NetAddress[counter].domain:=ExtractSubStr(s,x,[' ']);
      x:=1;
      inc(counter);
      inc(NACounter);
    end;
    if AnsiStartsStr('EchoExclude=',s) then begin
      Delete(s,1,12);
      echoexclude[exclude]:=s;
      inc(exclude);
    end;
    if AnsiStartsStr('NetworkExclude=',s) then begin
      Delete(s,1,15);
      networkexclude[netexclude]:=s;
      inc(netexclude);
    end;
  end;
  except
    on E: EInOutError do begin
    writeln('File handling error occurred. Details: ',E.Message);
    end;
  end;
end;

Function commainsert(com:string):string;
var
  len : integer;
begin
  result:='';
  len:=Length(com);
  case(len) of
    4:insert(',',com,2);
    5:insert(',',com,3);
    6:insert(',',com,4);
    7:begin
      insert(',',com,5);
      insert(',',com,2);
      end;
    8:begin
      insert(',',com,6);
      insert(',',com,3);
      end;
    9:begin
      insert(',',com,7);
      insert(',',com,4);
      end;
    10:begin
      insert(',',com,8);
      insert(',',com,5);
      insert(',',com,2);
      end;
    11:begin
      insert(',',com,9);
      insert(',',com,6);
      insert(',',com,3);
      end;
  end;
  result:=com;
end;

Procedure ProgramHalt;
Begin
  CloseFile(logfile);
  RenameFile('mystmult.log','logs'+PathDelim+'mystmult.log');
  halt(1);
end;

Procedure InsertLogDate;
begin
  write(logfile,FormatDateTime('yyyy mmm dd hh:nn:ss',(Now)));
end;

Procedure LogBreak;
begin
  WriteLn(logfile,' -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- ');
end;

procedure startup;
var
  temp : string;
begin
  ReadConfigFile;
  OSVersion;
  ProgVersion;
  logbool:=false;
  x:=0;
  totalin:=0;
  totalout:=0;
  totaldupe:=0;
  counter:=1;
  runpath:=GetCurrentDir;
  //RenameFile(LogsPath+'mystmult.log','mystmult.log');
  assign(LogFile,'mystmult.log');
  if fileexists('mystmult.log')=false then Rewrite(logfile)
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
  echobool:=false;
  nodebool:=false;
  esortmess:=false;
  esortnet:=false;
  esortdupe:=false;
  esortout:=false;
  esortecho:=false;
  for x:=1 To ParamCount do
  begin
  temp:=paramstr(x);
  if AnsiStartsStr('D',temp) then begin
    //temp:=paramstr(x);
    Delete(temp,1,1);
    reportdate:=StrToInt(temp);
  end;
  if AnsiStartsStr('d',temp) then begin
    Delete(temp,1,1);
    reportdate:=StrToInt(temp);
  end;
  case paramstr(x) of
    'h','H','?' : logmainthelp;
    'e','E'     : begin
                    echobool:=true;
                    esortmess:=true;
                  end;
    'e1','E1'   : begin
                    echobool:=true;
                    esortmess:=true;
                  end;
    'e2','E2'   : begin
                    echobool:=true;
                    esortnet:=true;
                  end;
    'e3','E3'   : begin
                    echobool:=true;
                    esortdupe:=true;
                  end;
    'e4','E4'   : begin
                    echobool:=true;
                    esortout:=true;
                  end;
    'e5','E5'   : begin
                    echobool:=true;
                    esortecho:=true;
                  end;
    'n','N'     : nodebool:=true;
    'l','L'     : logbool:=true;
    't','T'     : onetbool:=true;
    'm','M'     : mailbool:=true;
    end;
  end;
end;

procedure createdirectory;
begin
  ChDir(runpath);
  ChDir(MysticLogs);
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

Function FindNodeRecord(N:String): Integer; { Find Record Routine }
Begin
  foundrec:=0;
  currec:=1;
  III:=0;
  foundflag:=0;
  repeat
   III := III + 1;
   Seek(e,III);
   Read(e,node);
   If node.n_node = N then
   begin
     foundrec:=III;
     foundflag:=1;
   end;
  until (III=lastrec) or (foundrec>0) or (eof(e));
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
 if foundrec>0 then FindNodeRecord:=III;
End;

Function FindDomainZone(temp:string):string;
var
  x:integer;
  y:string;
  z:integer;
begin
 for x:=1 to NACounter-1 do begin
   z:=1;
   y:=ExtractSubStr(temp,z,[':']);
   if y=IntToStr(NetAddress[x].zone) then result:=NetAddress[x].domain;
   if (y=IntToStr(1))or(y=IntToStr(2))or(y=IntToStr(3))or(y=IntToStr(4)) then result:='fidonet';
 end;
end;

Function FindDomain(temp:string):string;
var
  x:integer;
  y:string;
begin
  for x:=1 to NACounter-1 do begin
    y:=IntToStr(NetAddress[x].zone)+':'+IntToStr(NetAddress[x].net)+'/'+IntToStr(NetAddress[x].node);
    if y=temp then tempdomain:=NetAddress[x].domain;
  end;
  result:=tempdomain
end;

Function FindNodeFromDomain(temp:string):string;
var
  x:integer;
  y:string;
  tempnode:string;
begin
  for x:=1 to NACounter-1 do begin
    y:=NetAddress[x].domain;
    if y=temp then tempnode:=IntToStr(NetAddress[x].zone)+':'+IntToStr(NetAddress[x].net)+'/'+IntToStr(NetAddress[x].node);
  end;
  result:=tempnode;
end;

Procedure EditRecord;     { Edit Current Record Routine }
Begin
  seek(f,currec);
  Read(f,echo);
  inc(echo.e_icount);
  echo.e_domain:=tempdomain;
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

Procedure EditNodeRecordIn;
Begin
  seek(e,currec);
  Read(e,node);
  inc(node.n_ifile);
  seek(e,currec);
  write(e,node);
  InsertLogDate;
  writeln(logfile,' Adding message count from node ',node.n_node);
end;

Procedure EditNodeRecordInQWK(qwkin,qwksize:integer);
Begin
  seek(e,currec);
  read(e,node);
  node.n_ifile:=node.n_ifile+qwkin;
  node.n_isize:=node.n_isize+qwksize;
  inc(node.n_conne);
  seek(e,currec);
  write(e,node);
  InsertLogDate;
  writeln(logfile,' Adding QWK message count from node ',node.n_node);
end;

Procedure EditNodeRecordOutQWK(qwkout,qwksize:integer);
begin
  seek(e,currec);
  read(e,node);
  node.n_ofile:=node.n_ofile+qwkout;
  node.n_osize:=node.n_osize+qwksize;
  inc(node.n_conne);
  seek(e,currec);
  write(e,node);
  InsertLogDate;
  writeln(logfile,' Adding outbound QWK message count to node ',node.n_node);
end;

Procedure EditRecordDupe;
Begin
  seek(f,currec);
  Read(f,echo);
  inc(echo.e_dupe);
  seek(f,currec);
  write(f,echo);
  InsertLogDate;
  writeln(logfile,' Adding dupe message count to echo ',echo.e_name);
end;

Procedure EditNodeRecordOut;
Begin
  seek(e,currec);
  Read(e,node);
  inc(node.n_ofile);
  seek(e,currec);
  write(e,node);
  InsertLogDate;
  writeln(logfile,' Adding message count to node ',node.n_node);
end;

Procedure EditNodeRecordInSize(insert:integer);
Begin
  seek(e,currec);
  Read(e,node);
  inc(node.n_conne);
  node.n_isize:=node.n_isize+insert;
  seek(e,currec);
  write(e,node);
  InsertLogDate;
  writeln(logfile,' Adding incoming connection and size to node ',node.n_node);
end;

Procedure EditNodeRecordOutSize(insert:integer);
Begin
  seek(e,currec);
  Read(e,node);
  inc(node.n_conne);
  node.n_osize:=node.n_osize+insert;
  seek(e,currec);
  write(e,node);
  InsertLogDate;
  writeln(logfile,' Adding outgoing connection and size to node ',node.n_node);
end;

Procedure EditNodeRecordConnect;
Begin
  seek(e,currec);
  Read(e,node);
  inc(node.n_conne);
  seek(e,currec);
  write(e,node);
  InsertLogDate;
  writeln(logfile,' Adding Connection from node ',node.n_node);
end;

Procedure GenericFooter;
var
  b:integer;
  bbs:string;
Begin
  b:=78;
  bbs:='';
  Insert(sysos,bbs,1);
  Insert(' ',bbs,1);
  Insert(ver,bbs,1);
  Insert(' v',bbs,1);
  Insert(prog,bbs,1);
  writeln(noderpt,PadCenter('Report Generated by: ',b));
  writeln(noderpt,PadCenter(bbs,b));
  writeln(noderpt);
  writeln(noderpt,PadCenter('RCS - CRBBS(2017-2018)',b));
  writeln(noderpt);
end;

Procedure GenericEchoFooter;
var
  b:integer;
  bbs:string;
Begin
  b:=78;
  bbs:='';
  Insert(sysos,bbs,1);
  Insert(' ',bbs,1);
  Insert(ver,bbs,1);
  Insert(' v',bbs,1);
  Insert(prog,bbs,1);
  writeln(echorpt,PadCenter('Report Generated by: ',b));
  writeln(echorpt,PadCenter(bbs,b));
  writeln(echorpt);
  writeln(echorpt,PadCenter('RCS - CRBBS(2017-2018)',b));
  writeln(echorpt);
end;

Procedure onetsort;
var
  temp:integer;
  z   :integer;
  d   :integer;
begin
  d:=1;
  z:=1;
  temp:=lastrec+1;
  reset(t);
  seek(t,1);
  for d:=1 to lastrec do begin
    seek(t,d);
    read(t,onet);
    onetdata[d].zone:=onet.zone;
    onetdata[d].net:=onet.net;
    onetdata[d].node:=onet.node;
    onetdata[d].domain:=onet.domain;
    onetdata[d].matched:=onet.matched;
    onetdata[d].nodes:=onet.nodes;
  end;

(*  for d:=1 to lastrec-1 do begin
    for z:=d+1 to lastrec-1 do begin
      if (onetdata[d].zone)=(onetdata[z].zone) then begin
        onetdata[z].zone:=0;
      end;
    end;
  end; *)
  for d:=1 to lastrec-1 do begin
    for z:=d+1 to lastrec-1 do begin
      if((onetdata[d].zone)>(onetdata[z].zone))then begin
        onetdata[temp]:=onetdata[d];
        onetdata[d]:=onetdata[z];
        onetdata[z]:=onetdata[temp];
      end;
      if((onetdata[d].net)>(onetdata[z].net))and((onetdata[d].zone)=(onetdata[z].zone))then begin
        onetdata[temp]:=onetdata[d];
        onetdata[d]:=onetdata[z];
        onetdata[z]:=onetdata[temp];
      end;
      if((onetdata[d].zone)=(onetdata[z].zone))and((onetdata[d].net)=(onetdata[z].net))and((onetdata[d].node)>(onetdata[z].node))then begin
        onetdata[temp]:=onetdata[d];
        onetdata[d]:=onetdata[z];
        onetdata[z]:=onetdata[temp];
      end;

    end;
  end;
(*  for d:=1 to lastrec-1 do begin
    for z:=d+1 to lastrec-1 do begin
      if((onetdata[d].net)>(onetdata[z].net))then begin
        onetdata[temp]:=onetdata[d];
        onetdata[d]:=onetdata[z];
        onetdata[z]:=onetdata[temp];
      end;
    end;
  end; *)
  for d:=1 to lastrec-1 do begin
    for z:=d+1 to lastrec-1 do begin
      if ((onetdata[d].zone)=(onetdata[z].zone))and((onetdata[d].net)=(onetdata[z].net)) then begin
        onetdata[z].zone:=0;
      end;
    end;
  end;
end;

procedure nodesort;
var
  temp:integer;
  z   :integer;
  d   :integer;
  sort1:integer;
  sort2:integer;
begin
  z:=1;
  d:=1;
  temp:=lastrec+1;
  for d:=1 to lastrec do begin
    for z:=d+1 to lastrec do begin
      if nodedata[d].n_node='' then nodedata[d].n_node:='0:0/0';
      if nodedata[z].n_node='' then nodedata[z].n_node:='0:0/0';
      if (AnsiContainsStr(nodedata[d].n_node,'net'))or(AnsiContainsStr(nodedata[d].n_node,'Net'))then nodedata[d].n_node:='999:999/999';
      if (AnsiContainsStr(nodedata[z].n_node,'net'))or(AnsiContainsStr(nodedata[z].n_node,'Net'))then nodedata[z].n_node:='999:999/999';
      sort1:=StrToInt(ExtractDelimited(1,(nodedata[d].n_node),[':']));
      sort2:=StrToInt(ExtractDelimited(1,(nodedata[z].n_node),[':']));
        if (sort1)>(sort2)then begin
          nodedata[temp]:=nodedata[d];
          nodedata[d]:=nodedata[z];
          nodedata[z]:=nodedata[temp];
        end;
      if (sort1)=(sort2)then begin
        sort1:=StrToInt(ExtractDelimited(2,(nodedata[d].n_node),[':','/']));
        sort2:=StrToInt(ExtractDelimited(2,(nodedata[z].n_node),[':','/']));
        if (sort1)>(sort2)then begin
          nodedata[temp]:=nodedata[d];
          nodedata[d]:=nodedata[z];
          nodedata[z]:=nodedata[temp];
        end
      else if (sort1=sort2) then begin
        sort1:=StrToInt(ExtractDelimited(3,(nodedata[d].n_node),[':','/']));
        sort2:=StrToInt(ExtractDelimited(3,(nodedata[z].n_node),[':','/']));
        if (sort1)>(sort2)then begin
          nodedata[temp]:=nodedata[d];
          nodedata[d]:=nodedata[z];
          nodedata[z]:=nodedata[temp];
          end;
        end;
      end;
    end;
  end;
end;

Procedure nodereport;
var
  i:integer;
  j:integer;
  nconne:integer;
  nifile:integer;
  nofile:integer;
  nisize:integer;
  nosize:integer;
  nodetemp:string;
begin
  i:=1;
  j:=1;
  nconne:=0;
  nifile:=0;
  nofile:=0;
  nisize:=0;
  nosize:=0;
  lastrec:=filepos(e);
  assign(noderpt,'noderpt.rpt');
  rewrite(noderpt);
  seek(e,i);
  while not eof(e) do
  begin
    read(e,node);
    nodedata[j].n_node:=node.n_node;
    nodedata[j].n_conne:=node.n_conne;
    nodedata[j].n_ifile:=node.n_ifile;
    nodedata[j].n_ofile:=node.n_ofile;
    nodedata[j].n_isize:=node.n_isize;
    nodedata[j].n_osize:=node.n_osize;
    inc(i);
    inc(j);
    seek(e,i);
  end;
  lastrec:=j;
  nodesort;
  for i:=1 to lastrec do begin
    nconne:=nodedata[i].n_conne+nconne;
    nifile:=nodedata[i].n_ifile+nifile;
    nofile:=nodedata[i].n_ofile+nofile;
    nisize:=nodedata[i].n_isize+nisize;
    nosize:=nodedata[i].n_osize+nosize;
  end;
  writeln(noderpt);
  writeln(noderpt,PadCenter('RCS Node Connection Summary',78));
  writeln(noderpt,PadCenter(BBSName,78));
  writeln(noderpt,PadCenter('***********************',78));
  writeln(noderpt,PadCenter(nodedate,78));
  writeln(noderpt,PadCenter('***********************',78));
  writeln(noderpt);
  writeln(noderpt,'                                  In      Out     In           Out');
  writeln(noderpt,' Node          Network    Connect Message Message Size         Size ');
  writeln(noderpt,PadCenter('-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-',78));
  writeln(noderpt);
  for j:=1 to lastrec do begin
    if nodedata[j].n_node<>'0:0/0' then
      begin
        if (nodedata[j].n_node='0:0/0')and(nodedata[j].n_conne=0)and(nodedata[j].n_ifile=0)and(nodedata[j].n_ofile=0)and(nodedata[j].n_isize=0)and(nodedata[j].n_osize=0)then continue;
        nodetemp:=FindDomainZone(nodedata[j].n_node);
        if nodedata[j].n_node='0:0/0' then nodetemp:='Unsecure';
        if nodedata[j].n_node='999:999/999' then nodetemp:='QWKNet';
        writeln(noderpt,' '+PadRight(nodedata[j].n_node,11)+'   '+PadRight(nodetemp,8)+'   '+PadRight(IntToStr(nodedata[j].n_conne),5)+'   '+PadRight(IntToStr(nodedata[j].n_ifile),5)+'   '+PadRight(IntToStr(nodedata[j].n_ofile),5)+'   '+PadRight(commainsert(IntToStr(nodedata[j].n_isize)),10)+'   '+PadRight(commainsert(IntToStr(nodedata[j].n_osize)),10));
      end;
  end;
  writeln(noderpt);
  writeln(noderpt,PadCenter('-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-',78));
  writeln(noderpt,' '+PadRight('Totals: ',11)+'              '+PadRight(IntToStr(nconne),5)+'   '+PadRight(IntToStr(nifile),5)+'   '+PadRight(IntToStr(nofile),5)+'   '+PadRight(commainsert(IntToStr(nisize)),10)+'   '+PadRight(commainsert(IntToStr(nosize)),10));
  writeln(noderpt);
  writeln(noderpt);
  GenericFooter;
  closefile(e);
  closefile(noderpt);
end;

Function RemoveComma(comma:string):integer;
var
  nocomma:string;
begin
  nocomma:=StringReplace(comma,',','',[rfReplaceAll]);
  result:=StrToInt(nocomma);
end;

Function FindOnetZone(Z,N,O:Integer):Integer;
Begin
  foundrec:=0;
  currec:=1;
  III:=0;
  foundflag:=0;
  repeat
   III := III + 1;
   Seek(t,III);
   Read(t,onet);
   If (onet.zone=Z)and(onet.net=N)and(onet.node=O) then
   begin
     foundrec:=III;
     foundflag:=1;
   end;
  until (III=lastrec) or (foundrec>0) or (eof(t));
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
 if foundrec>0 then FindOnetZone:=III;
end;

Procedure EditOnetRecord(Z,N,O:integer;D:string);
var
  found : boolean;
Begin
  found:=false;
  currec:=1;
  repeat
  seek(t,currec);
  Read(t,onet);
  if (onet.zone=Z)and(onet.net=N)and(onet.node=O)and(onet.domain=D) then begin
    inc(onet.matched);
    seek(t,currec);
    write(t,onet);
    found:=true;
  end;
  inc(currec);
  until (found)or(eof(t));
  if (found<>true) then begin
    seek(t,lastrec);
    onet.zone:=Z;
    onet.net:=N;
    onet.node:=O;
    onet.domain:=D;
    onet.matched:=1;
    write(t,onet);
    //inc(lastrec);
  end;
end;

Procedure othernetmis;
var
  lzone   : integer;
  lnet    : integer;
  lnode   : integer;
  ldomain : string;
  y       : integer;
begin
  s:='';
  lastrec:=0;
  repeat
  readln(mislog,s);
  if AnsiStartsStr('+',s) then
    begin
      Delete(s,1,2);
      Delete(s,11,80);
      echodate:=s;
    end;
  until (echodate<>'')or(eof(mislog));
  reset(mislog);
  readln(mislog,s);
  y:=0;
  onetdata[1].zone:=0;
  onetdata[1].net:=0;
  onetdata[1].node:=0;
  onetdata[1].domain:='';
  onetdata[1].matched:=0;
  onetdata[1].nodes:=0;
  write(t,onet);
  While not eof(mislog) do
  begin
    if AnsiContainsStr(s,' ADR ')then
    begin
      x:=(pos('ADR ',s))+3;
      Delete(s,1,x);
      x:=1;
      if (s='')then continue;
      lzone:=StrToInt(ExtractSubStr(s,x,[':']));
      lnet:=StrToInt(ExtractSubStr(s,x,['/']));
      lnode:=StrToInt(ExtractSubStr(s,x,['.','@']));
      x:=(pos('@',s))+1;
      ldomain:=(ExtractSubStr(s,x,[' ']));
      ti:=FindOnetZone(lzone,lnet,lnode);
      if foundflag=0 then
      begin
        onet.zone:=lzone;
        onet.net:=lnet;
        onet.node:=lnode;
        onet.domain:=ldomain;
        onet.matched:=1;
        onet.nodes:=1;
        write(t,onet);
        inc(lastrec);
      end;
      if foundflag=1 then
      begin
        currec:=ti;
        EditOnetRecord(lzone,lnet,lnode,ldomain);
      end;
      Delete(s,1,x-1);
      if s<>'' then begin
        repeat begin
          x:=1;
          lzone:=StrToInt(ExtractSubStr(s,x,[':']));
          lnet:=StrToInt(ExtractSubStr(s,x,['/']));
          lnode:=StrToInt(ExtractSubStr(s,x,['.','@']));
          x:=(pos('@',s))+1;
          ldomain:=(ExtractSubStr(s,x,[' ']));
          ti:=FindOnetZone(lzone,lnet,lnode);
          if foundflag=0 then
          begin
            onet.zone:=lzone;
            onet.net:=lnet;
            onet.node:=lnode;
            onet.domain:=ldomain;
            onet.matched:=1;
            write(t,onet);
            inc(lastrec);
          end;
          if foundflag=1 then
          begin
            currec:=ti;
            EditOnetRecord(lzone,lnet,lnode,ldomain);
          end;
          inc(y);
          Delete(s,1,x-1);
        end;
        until s='';
      end;
    end;
    readln(mislog,s);
  end;
//  lastrec:=y;
  closefile(mislog);
end;

Procedure othernetreport;
var
//  d : integer;
//  z : integer;
  x : integer;
  b : integer;
  bbs : string;
begin
  Assign(onetreport,'onetrpt.rpt');
  rewrite(onetreport);
  InsertLogDate;
  LogBreak;
  InsertLogDate;
  writeln(logfile,' Generating Other Network Report');
  writeln(onetreport);
  writeln(onetreport,PadCenter('RCS Other Network Listing',78));
  writeln(onetreport,PadCenter(BBSName,78));
  writeln(onetreport,PadCenter('***********************',78));
  writeln(onetreport,PadCenter(echodate,78));
  writeln(onetreport,PadCenter('***********************',78));
  writeln(onetreport);
  writeln(onetreport,'           Zone                     Domain              Matches');
  writeln(onetreport,PadCenter('-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-',78));
  writeln(onetreport);
  for x:=0 to lastrec-1 do begin
  if onetdata[x].zone<>0 then begin
    write(onetreport,' '+PadRight(IntToStr(onetdata[x].zone),5)+' '+PadRight(IntToStr(onetdata[x].net),10)+' '+PadRight(IntToStr(onetdata[x].node),10)+' '+PadRight(onetdata[x].domain,15)+' '+IntToStr(onetdata[x].matched));
    writeln(onetreport);
    end;
  end;
  writeln(onetreport);
  writeln(onetreport,PadCenter('-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-',78));
  writeln(onetreport);
  //z:=0;
(*  for x:=0 to lastrec-1 do begin
    for d:=x+1 to lastrec-1 do begin
      if UpCase(onetdata[x].domain)=UpCase(onetdata[d].domain)then inc(z);
      if ((Upcase(onetdata[x].domain))<>(UpCase(onetdata[d].domain)))and(z<>0) then begin
        writeln(onetreport,'  '+onetdata[x].domain+'     '+IntToStr(z));
      end;
    end;
  end;  *)
  b:=78;
  bbs:='';
  insert(sysos,bbs,1);
  Insert(' ',bbs,1);
  Insert(ver,bbs,1);
  Insert(' v',bbs,1);
  Insert(prog,bbs,1);
  writeln(onetreport,PadCenter('Report Generated by: ',b));
  writeln(onetreport,PadCenter(bbs,b));
  writeln(onetreport);
  writeln(onetreport,PadCenter('RCS - CRBBS(2017-2018)',b));
  writeln(onetreport);
  closefile(onetreport);
end;

Procedure nodestatfidopoll;
var
  FindName1:string;
  Receiving1:string;
  Receiving:integer;
  Sending1:string;
  Sending:integer;
begin
  chdir(runpath);
  s:='';
  Assign(fidopoll,LogsPath+'fidopoll.log');
  {$I-}
  reset(fidopoll);
  {$I+}
  if IOresult<>0 then
    begin
      writeln('File fidopoll.log, could not be opened');
      InsertLogDate;
      LogBreak;
      InsertLogDate;
      writeln(logfile,' ERROR: Could not open FIDOPOLL.LOG! ');
      ProgramHalt;
    end;
  Reset(e);
  Seek(e,1);
  InsertLogDate;
  LogBreak;
  InsertLogDate;
  writeln(logfile,' Reading the FIDOPOLL.LOG file');
  Readln(fidopoll,s);
  While not eof(fidopoll) do
  Begin
    if AnsiContainsStr(s,' Polling BINKP node ') then
      Begin
        x:=(pos('node ',s)+5);
        FindName:=(ExtractSubStr(s,x,[' ']));
        FindName1:=FindName;
        ti:=FindNodeRecord(FindName);
        if foundflag=0 then
        begin
          node.n_node:=FindName;
          node.n_ifile:=0;
          node.n_ofile:=0;
          node.n_isize:=0;
          node.n_osize:=0;
          node.n_conne:=1;
          write(e,node);
          InsertLogDate;
          writeln(logfile,' Adding Connection to ',node.n_node);
        end;
        if foundflag=1 then
        begin
          currec:=ti;
          EditNodeRecordConnect;
        end;
        end;
        Repeat
            Begin
              if AnsiContainsStr(s,'Receiving: ') then
              begin
                FindName:=FindName1;
                x:=(pos('(',s)+1);
                Receiving1:=(ExtractSubStr(s,x,[' ']));
                Receiving:=(RemoveComma(Receiving1));
                ti:=FindNodeRecord(FindName);
                if foundflag=0 then
                begin
                  node.n_node:=FindName;
                  node.n_ifile:=0;
                  node.n_ofile:=0;
                  node.n_isize:=Receiving;
                  node.n_osize:=0;
                  node.n_conne:=1;
                  write(e,node);
                  InsertLogDate;
                  writeln(logfile,' Connection to ',node.n_node);
                end;
                if foundflag=1 then
                begin
                  currec:=ti;
                  EditNodeRecordInSize(Receiving);
                end;
              end;
              if AnsiContainsStr(s,'Sending: ') then
              begin
                FindName:=FindName1;
                x:=(pos('(',s)+1);
                Sending1:=(ExtractSubStr(s,x,[' ']));
                Sending:=(RemoveComma(Sending1));
                ti:=FindNodeRecord(FindName);
                if foundflag=0 then
                begin
                  node.n_node:=FindName;
                  node.n_ifile:=0;
                  node.n_ofile:=0;
                  node.n_isize:=0;
                  node.n_osize:=Sending;
                  node.n_conne:=1;
                  write(e,node);
                  InsertLogDate;
                  writeln(logfile,' Connection to ',node.n_node);
                end;
                if foundflag=1 then
                begin
                  currec:=ti;
                  EditNodeRecordOutSize(Sending);
                end;
              end;
            end;
            readln(fidopoll,s);
            if eof(fidopoll) then break;
        until (AnsiContainsStr(s,' Polling BINKP node '));
      end;
  CloseFile(fidopoll);
end;

Procedure nodestatmis;
var
  FindName1:string;
  Receiving1:string;
  Receiving:integer;
  Sending1:string;
  Sending:integer;
begin
  chdir(runpath);
  s:='';
  Reset(e);
  Seek(e,1);
  InsertLogDate;
  LogBreak;
  InsertLogDate;
  writeln(logfile,' Reading the MIS.LOG file for node# '+IntToStr(MISNode));
  Readln(mislog,s);
  While not eof(mislog) do
  begin
    if AnsiContainsStr(s,' BINKP '+IntToStr(MISNode))then
    begin
      if AnsiContainsStr(s,'Authenticating ') then
      begin
        x:=(pos('Authenticating ',s)+15);
        if AnsiContainsStr(s,'@') then
          begin
            FindName:=(ExtractSubStr(s,x,['@']));
          end
        else FindName:=(ExtractSubStr(s,x,[' ']));
        FindName1:=FindName;
        ti:=FindNodeRecord(FindName);
        if foundflag=0 then
        begin
          node.n_node:=FindName;
          node.n_ifile:=0;
          node.n_ofile:=0;
          node.n_isize:=0;
          node.n_osize:=0;
          node.n_conne:=1;
          write(e,node);
          InsertLogDate;
          writeln(logfile,' Adding Connection from ',node.n_node);
        end;
        if foundflag=1 then
        begin
          currec:=ti;
          EditNodeRecordConnect;
        end;
      end;
      Repeat
        Begin
          if AnsiContainsStr(s,'Unsecured session') then Begin
            FindName:='';
            FindName1:='';
          end;
          if AnsiContainsStr(s,'Receiving: ') then
          begin
            FindName:=FindName1;
            x:=(pos('(',s)+1);
            Receiving1:=(ExtractSubStr(s,x,[' ']));
            Receiving:=(RemoveComma(Receiving1));
            ti:=FindNodeRecord(FindName);
            if foundflag=0 then
            begin
              node.n_node:=FindName;
              node.n_ifile:=0;
              node.n_ofile:=0;
              node.n_isize:=Receiving;
              node.n_osize:=0;
              node.n_conne:=1;
              write(e,node);
              InsertLogDate;
              writeln(logfile,' Incoming Connection from ',node.n_node);
            end;
            if foundflag=1 then
            begin
              currec:=ti;
              EditNodeRecordInSize(Receiving);
            end;
          end;
          if AnsiContainsStr(s,'Sending: ') then
          begin
            FindName:=FindName1;
            x:=(pos('(',s)+1);
            Sending1:=(ExtractSubStr(s,x,[' ']));
            Sending:=(RemoveComma(Sending1));
            ti:=FindNodeRecord(FindName);
            if foundflag=0 then
            begin
              node.n_node:=FindName;
              node.n_ifile:=0;
              node.n_ofile:=0;
              node.n_isize:=0;
              node.n_osize:=Sending;
              node.n_conne:=1;
              write(e,node);
              InsertLogDate;
              writeln(logfile,' Outbound connection to ',node.n_node);
            end;
            if foundflag=1 then
            begin
              currec:=ti;
              EditNodeRecordOutSize(Sending);
            end;
          end;
        end;
        readln(mislog,s);
        if eof(mislog) then break;
      until (AnsiContainsStr(s,'Authenticating '));
    end
    else readln(mislog,s);
  end;
  CloseFile(mislog);
end;

Procedure nodeqwk;
var
  FindName1:string;
  qwksize:integer;
  qwkcount:integer;
begin
  chdir(runpath);
  s:='';
  Assign(qwkpoll,MysticLogs+'qwkpoll.log');
  {$I-}
  reset(qwkpoll);
  {$I+}
  if IOresult<>0 then
  begin
    writeln('File QWKPoll.log, could not be opened');
    InsertLogDate;
    LogBreak;
    InsertLogDate;
    writeln(logfile,' ERROR: Could not open QWKPOLL.LOG! ');
    ProgramHalt;
  end;
  Assign(e,'nodecnt.dat');
  reset(e);
  seek(e,Filesize(e));
  InsertLogDate;
  LogBreak;
  InsertLogDate;
  writeln(logfile,' Reading QWKPOLL.LOG to gather information');
  readln(qwkpoll,s);
  While not eof(qwkpoll) do
  begin
    if AnsiContainsStr(s,'Exchanging Mail for') then begin
      x:=(pos('for ',s))+4;
      FindName:=(ExtractSubStr(s,x,[' ']));
      FindName1:=FindName;
    end;
    repeat
      if AnsiContainsStr(s,' OK: ') then begin
        x:=(pos('(',s))+1;
        qwksize:=RemoveComma(ExtractSubStr(s,x,[' ']));
      end;
      if AnsiContainsStr(s,' Imported')then begin
        x:=(pos('Imported ',s))+9;
        qwkcount:=RemoveComma(ExtractSubStr(s,x,[' ']));
        FindName:=FindName1;
        ti:=FindNodeRecord(FindName);
        if foundflag=0 then
        begin
          node.n_node:=FindName;
          node.n_ifile:=qwkcount;
          node.n_ofile:=0;
          node.n_isize:=qwksize;
          node.n_osize:=0;
          node.n_conne:=1;
          write(e,node);
          InsertLogDate;
          writeln(logfile,' Incoming Echomail from ',node.n_node);
        end;
        if foundflag=1 then
        begin
          currec:=ti;
          EditNodeRecordInQWK(qwkcount,qwksize);
        end;
        node.n_node:=FindName;
        currec:=1;
      end;
      if (AnsiContainsStr(s,' Exported '))and not(AnsiContainsStr(s,'@'))then begin
        x:=(pos('Exported ',s))+9;
        qwkcount:=StrToInt(ExtractSubStr(s,x,[' ']));
        qwksize:=0;
        FindName:=FindName1;
        ti:=FindNodeRecord(FindName);
        if foundflag=0 then
        begin
          node.n_node:=FindName;
          node.n_ifile:=0;
          node.n_ofile:=qwkcount;
          node.n_isize:=0;
          node.n_osize:=0;
          node.n_conne:=1;
          write(e,node);
          InsertLogDate;
          writeln(logfile,' Outgoing Echomail to ',node.n_node);
        end;
        if foundflag=1 then
        begin
          currec:=ti;
          EditNodeRecordOutQWK(qwkcount,qwksize);
        end;
        node.n_conne:=0;
        currec:=1;
      end;
      readln(qwkpoll,s);
      if eof(qwkpoll) then break;
    until (AnsiContainsStr(s,'Exchanging Mail for'));
  end;
  closeFile(qwkpoll);
end;

Procedure nodestat;
var
  counter:integer;
  FindName1:string;
begin
  chdir(runpath);
  s:='';
  counter:=0;
  Assign(mutillog,MysticLogs+'mutil.log');
  {$I-}
  reset(mutillog);
  {$I+}
  if IOresult<>0 then
  begin
    writeln('File mutil.log, could not be opened');
    InsertLogDate;
    LogBreak;
    InsertLogDate;
    writeln(logfile,' ERROR: Could not open MUTIL.LOG! ');
    ProgramHalt;
  end;
  repeat
  readln(mutillog,s);
  if AnsiStartsStr('+',s) then
    begin
      Delete(s,1,2);
      Delete(s,7,80);
      nodedate:=s;
    end;
  until (nodedate<>'')or(eof(mutillog));
  reset(mutillog);

  Assign(e,'nodecnt.dat');
  rewrite(e);
  seek(e,1);
  node.n_node:='';
  node.n_conne:=0;
  node.n_ifile:=0;
  node.n_ofile:=0;
  node.n_isize:=0;
  node.n_osize:=0;
  write(e,node);
  InsertLogDate;
  LogBreak;
  InsertLogDate;
  writeln(logfile,' Reading MUTIL.LOG to gather information');
  readln(mutillog,s);
  While not eof(mutillog) do
  begin
    if AnsiContainsStr(s,'Importing ') then begin
      if (AnsiContainsStr(s,'.pkt '))or(AnsiContainsStr(s,'.PKT ')) then begin
        inc(counter);
        x:=(pos(' (',s))+2;
        FindName:=(ExtractSubstr(s,x,[' ']));
        FindName1:=FindName;
      end;
    end;
    repeat
      if (AnsiContainsStr(s,'Import '))and not(AnsiContainsStr(s,'Import from')) then begin
        FindName:=FindName1;
        ti:=FindNodeRecord(FindName);
        if foundflag=0 then
        begin
          node.n_node:=FindName;
          node.n_ifile:=1;
          node.n_ofile:=0;
          node.n_isize:=0;
          node.n_osize:=0;
          node.n_conne:=0;
          write(e,node);
          InsertLogDate;
          writeln(logfile,' Incoming Echomail from ',node.n_node);
        end;
        if foundflag=1 then
        begin
          currec:=ti;
          EditNodeRecordIn;
        end;
        node.n_conne:=0;
        currec:=1;
      end;
      if (AnsiContainsStr(s,'Export ')) then begin
        x:=(pos(' to ',s))+4;
        FindName:=(ExtractSubStr(s,x,['@']));
        x:=(pos(' ',FindName));
        if AnsiContainsStr(FindName,' ') then FindName:=ExtractWord(1,FindName,[' ']);
        ti:=FindNodeRecord(FindName);
        if foundflag=0 then
        begin
          node.n_node:=FindName;
          node.n_ifile:=0;
          node.n_ofile:=1;
          node.n_isize:=0;
          node.n_osize:=0;
          node.n_conne:=0;
          write(e,node);
          InsertLogDate;
          writeln(logfile,' Outgoing Echomail to ',node.n_node);
        end;
        if foundflag=1 then
        begin
          currec:=ti;
          EditNodeRecordOut;
        end;
        node.n_conne:=0;
        currec:=1;
      end;
      readln(mutillog,s);
      if eof(mutillog) then break;
    until (AnsiContainsStr(s,'Importing '));
  end;
  CloseFile(mutillog);
end;

procedure echostat;
var
  tempnode : string;
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
  rec.e_dupe:=0;
  echo.e_dupe:=0;
  write(f,rec);
 Assign(mutillog,MysticLogs+'mutil.log');
  {$I-}
  reset(mutillog);
  {$I+}
  if IOresult<>0 then
    begin
    writeln('File mutil.log, could not be opened');
    Programhalt;
    end;
  write(logfile,FormatDateTime('yyyy mmm dd hh:nn:ss',(Now)));
  writeln(logfile,' Processing Incoming Echomail');
  repeat
  readln(mutillog,s);
  if AnsiStartsStr('+',s) then
    begin
      Delete(s,1,2);
      Delete(s,7,80);
      echodate:=s;
    end;
  until (echodate<>'')or(eof(mutillog));
  reset(mutillog);

  While not eof(mutillog) do
  begin
    readln(mutillog,s);
    if AnsiStartsStr('+',s) then begin
      if (AnsiContainsStr(s,'.pkt'))or(AnsiContainsStr(s,'.PKT')) then begin
        x:=pos('to ',s)+3;
        tempnode:=(ExtractSubStr(s,x,[')']));
        FindDomain(tempnode);
      end;
    end;
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
          echo.e_domain:=tempdomain;
          echo.e_dupe:=0;
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
        x:=(pos('@',s))+1;
        tempdomain:=(ExtractSubStr(s,x,[' ']));
        inc(totalout);
        ti:=FindRecord(FindName);
        if foundflag=0 then
        begin
          echo.e_name:=FindName;
          echo.e_ocount:=1;
          echo.e_icount:=0;
          echo.e_domain:=tempdomain;
          echo.e_dupe:=0;
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
  if AnsiContainsStr(s,' Duplicate message found in ')then begin
    x:=(pos('Duplicate message found in ',s))+27;
    FindName:=UpCaseStr(ExtractSubStr(s,x,[' ']));
    ti:=FindRecord(FindName);
    if foundflag=0 then
    begin
      echo.e_name:=FindName;
      echo.e_ocount:=0;
      echo.e_icount:=0;
      echo.e_dupe:=1;
      write(f,echo);
    end;
    if foundflag=1 then
    begin
      currec:=ti;
      EditRecordDupe;
    end;
    echo.e_name:='';
    echo.e_dupe:=0;
    inc(totaldupe);
  end;
  inc(ts);
  end;
  n:=filesize(f);
  seek(f,n);
  lastrec:=filepos(f)-1;
  CloseFile(f);
  CloseFile(mutillog);
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
  if (esortmess=false)and(esortnet=false)and(esortdupe=false)and(esortout=false)and(esortecho=false) then esortmess:=true;
  reset(f);
  seek(f,1);
  for d:=1 to lastrec do begin
    seek(f,d);
    read(f,echo);
    sortdata[d].e_name:=echo.e_name;
    sortdata[d].e_domain:=echo.e_domain;
    sortdata[d].e_icount:=echo.e_icount;
    sortdata[d].e_ocount:=echo.e_ocount;
    sortdata[d].e_dupe:=echo.e_dupe;
  end;
  if esortdupe=true then begin
    for d:=1 to lastrec do begin
      for z:=d+1 to lastrec do begin
        if ((sortdata[d].e_dupe)<(sortdata[z].e_dupe)) then begin
          sortdata[temp]:=sortdata[d];
          sortdata[d]:=sortdata[z];
          sortdata[z]:=sortdata[temp];
        end;
      end;
    end;
    for d:=1 to lastrec do begin
      for z:=d+1 to lastrec do begin
        if ((sortdata[d].e_dupe)=(sortdata[z].e_dupe))and((sortdata[d].e_icount)<(sortdata[z].e_icount))then begin
          sortdata[temp]:=sortdata[d];
          sortdata[d]:=sortdata[z];
          sortdata[z]:=sortdata[temp];
        end;
      end;
    end;
  end;
  if esortmess=true then begin
    for d:=1 to lastrec do begin
      for z:=d+1 to lastrec do begin
        if ((sortdata[d].e_icount)<(sortdata[z].e_icount))then begin
          sortdata[temp]:=sortdata[d];
          sortdata[d]:=sortdata[z];
          sortdata[z]:=sortdata[temp];
        end;
      end;
    end;
    for d:=1 to lastrec do begin
      for z:=d+1 to lastrec do begin
        if ((sortdata[d].e_icount)<(sortdata[z].e_icount))then begin
          sortdata[temp]:=sortdata[d];
          sortdata[d]:=sortdata[z];
          sortdata[z]:=sortdata[temp];
        end;
      end;
    end;
  end;
  if esortecho=true then begin
    for d:=1 to lastrec do begin
      for z:=d+1 to lastrec do begin
        if ((sortdata[d].e_name)>(sortdata[z].e_name)) then begin
          sortdata[temp]:=sortdata[d];
          sortdata[d]:=sortdata[z];
          sortdata[z]:=sortdata[temp];
        end;
      end;
    end;
    for d:=1 to lastrec do begin
      for z:=d+1 to lastrec do begin
        if ((sortdata[d].e_name)>(sortdata[z].e_name)) then begin
          sortdata[temp]:=sortdata[d];
          sortdata[d]:=sortdata[z];
          sortdata[z]:=sortdata[temp];
        end;
      end;
    end;
  end;
  if esortout=true then begin
    for d:=1 to lastrec do begin
      for z:=d+1 to lastrec do begin
        if ((sortdata[d].e_ocount)<(sortdata[z].e_ocount)) then begin
          sortdata[temp]:=sortdata[d];
          sortdata[d]:=sortdata[z];
          sortdata[z]:=sortdata[temp];
        end;
      end;
    end;
    for d:=1 to lastrec do begin
      for z:=d+1 to lastrec do begin
        if ((sortdata[d].e_ocount)=(sortdata[z].e_ocount))and((sortdata[d].e_icount)<(sortdata[z].e_icount))then begin
          sortdata[temp]:=sortdata[d];
          sortdata[d]:=sortdata[z];
          sortdata[z]:=sortdata[temp];
        end;
      end;
    end;
  end;
  if esortnet=true then begin
    for d:=1 to lastrec do begin
      for z:=d+1 to lastrec do begin
        if ((upcase(sortdata[d].e_domain))>(upcase(sortdata[z].e_domain)))then begin
          sortdata[temp]:=sortdata[d];
          sortdata[d]:=sortdata[z];
          sortdata[z]:=sortdata[temp];
        end;
      end;
    end;
    for d:=1 to lastrec do begin
      for z:=d+1 to lastrec do begin
        if ((upcase(sortdata[d].e_domain))=(upcase(sortdata[z].e_domain)))and((sortdata[d].e_icount)<(sortdata[z].e_icount))then begin
          sortdata[temp]:=sortdata[d];
          sortdata[d]:=sortdata[z];
          sortdata[z]:=sortdata[temp];
        end;
      end;
    end;

  end;
end;

procedure echoreport;
var
  excludecounter:integer;
begin
  excludecounter:=1;
  III:=1;
  reset(f);
  Assign(echorpt,'echorpt.rpt');
  rewrite(echorpt);
  write(logfile,FormatDateTime('yyyy mmm dd hh:nn:ss',(Now)));
  writeln(logfile,' Generating Report');
  writeln(echorpt);
  writeln(echorpt,(PadCenter('RCS Echomail Report',78)));
  writeln(echorpt,(PadCenter(BBSName,78)));
  writeln(echorpt,(PadCenter('***********************',78)));
  writeln(echorpt,(PadCenter(echodate,78)));
  writeln(echorpt,(PadCenter('***********************',78)));
  if (esortmess) then writeln(echorpt,(PadCenter('Sorted by Incoming Messages',78)));
  if (esortnet) then writeln(echorpt,(PadCenter('Sorted by Network',78)));
  if (esortdupe) then writeln(echorpt,(PadCenter('Sorted by Dupe Messages',78)));
  if (esortout) then writeln(echorpt,(PadCenter('Sorted by Outgoing Messages',78)));
  if (esortecho) then writeln(echorpt,(PadCenter('Sorted by Echo Name',78)));
  writeln(echorpt);
  writeln(echorpt);
  write(echorpt,(PadRight('Echo Name',35)));
  writeln(echorpt,'          Inbound     Outbound    Dupes');
  writeln(echorpt,'-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-');
  writeln(echorpt);
  For III:=1 to lastrec do
  begin
    if sortdata[III].e_name<>'' then begin
      For excludecounter:=0 to netexclude do begin
        if sortdata[III].e_domain=networkexclude[excludecounter] then begin
          sortdata[III].e_name:='';
        end;
      end;
      For excludecounter:=0 to exclude do begin
        if sortdata[III].e_name=echoexclude[excludecounter] then begin
            sortdata[III].e_name:='';
          end;
        end;
      end;
    if sortdata[III].e_name<>'' then begin
          write(echorpt,PadRight(sortdata[III].e_name,45));
          //write(echorpt,PadRight(sortdata[III].e_domain,12));
          //write(echorpt,PadRight(FindNodeFromDomain(sortdata[III].e_domain),12));
          write(echorpt,PadRight(IntToStr(sortdata[III].e_icount),12));
          write(echorpt,PadRight(IntToStr(sortdata[III].e_ocount),12));
          writeln(echorpt,PadRight(IntToStr(sortdata[III].e_dupe),8));
      end;
    end;
  writeln(echorpt,'-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-');
  write(echorpt,'System Totals:');
  write(echorpt,PadRight(' ',31));
  write(echorpt,PadRight(IntToStr(totalin),12));
  write(echorpt,PadRight(IntToStr(totalout),12));
  writeln(echorpt,PadRight(IntToStr(totaldupe),8));
  writeln(echorpt);
  writeln(echorpt);
  GenericEchoFooter;
  CloseFile(echorpt);
  CloseFile(f);
end;

Function FindMailer(N:String):Integer;
Begin
  foundrec:=0;
  currec:=1;
  III:=0;
  foundflag:=0;
  repeat
    III:=III+1;
    Seek(m,III);
    Read(m,mail);
    If mail.mailer=N then
    begin
      foundrec:=III;
      foundflag:=1;
    end;
  until (III=lastrec)or(foundrec>0);
  if foundrec=0 then foundflag:=0;
  if foundrec>0 then
  begin
    currec:=III;
    foundflag:=1;
  end;
  if foundrec=0 then foundflag:=0;
  if foundrec>0 then FindMailer:=III;
end;

Procedure EditMailerRecord;
Begin
  seek(m,currec);
  read(m,mail);
  inc(mail.matches);
  seek(m,currec);
  write(m,mail);
end;

procedure maildatamis;
var
  lmailer : string;
begin
  reset(mislog);
  While not eof(mislog) do
  begin
    If AnsiContainsStr(s,'Mailer ') then
    begin
      x:=(pos('Mailer ',s))+6;
      Delete(s,1,x);
      lmailer:=s;
      ti:=FindMailer(lmailer);
      if foundflag=0 then
      begin
        mail.mailer:=lmailer;
        mail.matches:=1;
        write(m,mail);
        inc(lastrec);
      end;
      if foundflag=1 then
      begin
        currec:=ti;
        EditMailerRecord;
      end;
    end;
    readln(mislog,s);
  end;
end;

Procedure maildatasort;
var
  temp : integer;
  d    : integer;
  z    : integer;
begin
  d:=1;
  z:=1;
  temp:=lastrec+1;
  //reset(m);
  //seek(m,1);
  for d:=1 to lastrec-1 do begin
    seek(m,d);
    read(m,mail);
    maildata[d].mailer:=mail.mailer;
    maildata[d].matches:=mail.matches;
  end;
  for d:=1 to lastrec-1 do begin
    for z:=d+1 to lastrec-1 do begin
      if (UpperCase(maildata[d].mailer[1]))>(UpperCase(maildata[z].mailer[1])) then begin
        maildata[temp]:=maildata[d];
        maildata[d]:=maildata[z];
        maildata[z]:=maildata[temp];
      end;
      if ((UpperCase(maildata[d].mailer[1]))=(UpperCase(maildata[z].mailer[1])))and((UpperCase(maildata[d].mailer[2]))>(UpperCase(maildata[z].mailer[2]))) then begin
        maildata[temp]:=maildata[d];
        maildata[d]:=maildata[z];
        maildata[z]:=maildata[temp];
      end;
      if ((UpperCase(maildata[d].mailer[1]))=(UpperCase(maildata[z].mailer[1])))and((UpperCase(maildata[d].mailer[2]))=(UpperCase(maildata[z].mailer[2])))and((UpperCase(maildata[d].mailer[3]))>(UpperCase(maildata[z].mailer[3])))then begin
        maildata[temp]:=maildata[d];
        maildata[d]:=maildata[z];
        maildata[z]:=maildata[temp];
      end;
      if ((UpperCase(maildata[d].mailer[1]))=(UpperCase(maildata[z].mailer[1])))and((UpperCase(maildata[d].mailer[2]))=(UpperCase(maildata[z].mailer[2])))and((UpperCase(maildata[d].mailer[3]))=(UpperCase(maildata[z].mailer[3])))and((UpperCase(maildata[d].mailer[4]))>(UpperCase(maildata[z].mailer[4])))then begin
        maildata[temp]:=maildata[d];
        maildata[d]:=maildata[z];
        maildata[z]:=maildata[temp];
      end;
      if ((UpperCase(maildata[d].mailer[1]))=(UpperCase(maildata[z].mailer[1])))and((UpperCase(maildata[d].mailer[2]))=(UpperCase(maildata[z].mailer[2])))and((UpperCase(maildata[d].mailer[3]))=(UpperCase(maildata[z].mailer[3])))and((UpperCase(maildata[d].mailer[4]))=(UpperCase(maildata[z].mailer[4])))and((Uppercase(maildata[d].mailer[5]))>(UpperCase(maildata[z].mailer[5])))then begin
        maildata[temp]:=maildata[d];
        maildata[d]:=maildata[z];
        maildata[z]:=maildata[temp];
      end;
      if ((UpperCase(maildata[d].mailer[1]))=(UpperCase(maildata[z].mailer[1])))and((UpperCase(maildata[d].mailer[2]))=(UpperCase(maildata[z].mailer[2])))and((UpperCase(maildata[d].mailer[3]))=(UpperCase(maildata[z].mailer[3])))and((UpperCase(maildata[d].mailer[4]))=(UpperCase(maildata[z].mailer[4])))and((Uppercase(maildata[d].mailer[5]))=(UpperCase(maildata[z].mailer[5])))and((Uppercase(maildata[d].mailer[6]))>(UpperCase(maildata[z].mailer[6])))then begin
        maildata[temp]:=maildata[d];
        maildata[d]:=maildata[z];
        maildata[z]:=maildata[temp];
      end;
      if ((UpperCase(maildata[d].mailer[1]))=(UpperCase(maildata[z].mailer[1])))and((UpperCase(maildata[d].mailer[2]))=(UpperCase(maildata[z].mailer[2])))and((UpperCase(maildata[d].mailer[3]))=(UpperCase(maildata[z].mailer[3])))and((UpperCase(maildata[d].mailer[4]))=(UpperCase(maildata[z].mailer[4])))and((Uppercase(maildata[d].mailer[5]))=(UpperCase(maildata[z].mailer[5])))and((Uppercase(maildata[d].mailer[6]))=(UpperCase(maildata[z].mailer[6])))and((UpperCase(maildata[d].mailer[7]))>(UpperCase(maildata[z].mailer[7])))then begin
        maildata[temp]:=maildata[d];
        maildata[d]:=maildata[z];
        maildata[z]:=maildata[temp];
      end;
      if ((UpperCase(maildata[d].mailer[1]))=(UpperCase(maildata[z].mailer[1])))and((UpperCase(maildata[d].mailer[2]))=(UpperCase(maildata[z].mailer[2])))and((UpperCase(maildata[d].mailer[3]))=(UpperCase(maildata[z].mailer[3])))and((UpperCase(maildata[d].mailer[4]))=(UpperCase(maildata[z].mailer[4])))and((Uppercase(maildata[d].mailer[5]))=(UpperCase(maildata[z].mailer[5])))and((Uppercase(maildata[d].mailer[6]))=(UpperCase(maildata[z].mailer[6])))and((UpperCase(maildata[d].mailer[7]))=(UpperCase(maildata[z].mailer[7])))and((UpperCase(maildata[d].mailer[8]))>(UpperCase(maildata[z].mailer[8])))then begin
        maildata[temp]:=maildata[d];
        maildata[d]:=maildata[z];
        maildata[z]:=maildata[temp];
      end;
      if ((UpperCase(maildata[d].mailer[1]))=(UpperCase(maildata[z].mailer[1])))and((UpperCase(maildata[d].mailer[2]))=(UpperCase(maildata[z].mailer[2])))and((UpperCase(maildata[d].mailer[3]))=(UpperCase(maildata[z].mailer[3])))and((UpperCase(maildata[d].mailer[4]))=(UpperCase(maildata[z].mailer[4])))and((Uppercase(maildata[d].mailer[5]))=(UpperCase(maildata[z].mailer[5])))and((Uppercase(maildata[d].mailer[6]))=(UpperCase(maildata[z].mailer[6])))and((UpperCase(maildata[d].mailer[7]))=(UpperCase(maildata[z].mailer[7])))and((UpperCase(maildata[d].mailer[8]))=(UpperCase(maildata[z].mailer[8])))and((UpperCase(maildata[d].mailer[9]))>(UpperCase(maildata[z].mailer[9])))then begin
        maildata[temp]:=maildata[d];
        maildata[d]:=maildata[z];
        maildata[z]:=maildata[temp];
      end;
      if ((UpperCase(maildata[d].mailer[1]))=(UpperCase(maildata[z].mailer[1])))and((UpperCase(maildata[d].mailer[2]))=(UpperCase(maildata[z].mailer[2])))and((UpperCase(maildata[d].mailer[3]))=(UpperCase(maildata[z].mailer[3])))and((UpperCase(maildata[d].mailer[4]))=(UpperCase(maildata[z].mailer[4])))and((Uppercase(maildata[d].mailer[5]))=(UpperCase(maildata[z].mailer[5])))and((Uppercase(maildata[d].mailer[6]))=(UpperCase(maildata[z].mailer[6])))and((UpperCase(maildata[d].mailer[7]))=(UpperCase(maildata[z].mailer[7])))and((UpperCase(maildata[d].mailer[8]))=(UpperCase(maildata[z].mailer[8])))and((UpperCase(maildata[d].mailer[9]))=(UpperCase(maildata[z].mailer[9])))and((UpperCase(maildata[d].mailer[10]))>(UpperCase(maildata[z].mailer[10])))then begin
        maildata[temp]:=maildata[d];
        maildata[d]:=maildata[z];
        maildata[z]:=maildata[temp];
      end;
      if ((UpperCase(maildata[d].mailer[1]))=(UpperCase(maildata[z].mailer[1])))and((UpperCase(maildata[d].mailer[2]))=(UpperCase(maildata[z].mailer[2])))and((UpperCase(maildata[d].mailer[3]))=(UpperCase(maildata[z].mailer[3])))and((UpperCase(maildata[d].mailer[4]))=(UpperCase(maildata[z].mailer[4])))and((Uppercase(maildata[d].mailer[5]))=(UpperCase(maildata[z].mailer[5])))and((Uppercase(maildata[d].mailer[6]))=(UpperCase(maildata[z].mailer[6])))and((UpperCase(maildata[d].mailer[7]))=(UpperCase(maildata[z].mailer[7])))and((UpperCase(maildata[d].mailer[8]))=(UpperCase(maildata[z].mailer[8])))and((UpperCase(maildata[d].mailer[9]))=(UpperCase(maildata[z].mailer[9])))and((UpperCase(maildata[d].mailer[10]))=(UpperCase(maildata[z].mailer[10])))and((UpperCase(maildata[d].mailer[11]))>(UpperCase(maildata[z].mailer[11])))then begin
        maildata[temp]:=maildata[d];
        maildata[d]:=maildata[z];
        maildata[z]:=maildata[temp];
      end;
      if ((UpperCase(maildata[d].mailer[1]))=(UpperCase(maildata[z].mailer[1])))and((UpperCase(maildata[d].mailer[2]))=(UpperCase(maildata[z].mailer[2])))and((UpperCase(maildata[d].mailer[3]))=(UpperCase(maildata[z].mailer[3])))and((UpperCase(maildata[d].mailer[4]))=(UpperCase(maildata[z].mailer[4])))and((Uppercase(maildata[d].mailer[5]))=(UpperCase(maildata[z].mailer[5])))and((Uppercase(maildata[d].mailer[6]))=(UpperCase(maildata[z].mailer[6])))and((UpperCase(maildata[d].mailer[7]))=(UpperCase(maildata[z].mailer[7])))and((UpperCase(maildata[d].mailer[8]))=(UpperCase(maildata[z].mailer[8])))and((UpperCase(maildata[d].mailer[9]))=(UpperCase(maildata[z].mailer[9])))and((UpperCase(maildata[d].mailer[10]))=(UpperCase(maildata[z].mailer[10])))and((UpperCase(maildata[d].mailer[11]))=(UpperCase(maildata[z].mailer[11])))and((UpperCase(maildata[d].mailer[12]))>(UpperCase(maildata[z].mailer[12])))then begin
        maildata[temp]:=maildata[d];
        maildata[d]:=maildata[z];
        maildata[z]:=maildata[temp];
      end;
      if ((UpperCase(maildata[d].mailer[1]))=(UpperCase(maildata[z].mailer[1])))and((UpperCase(maildata[d].mailer[2]))=(UpperCase(maildata[z].mailer[2])))and((UpperCase(maildata[d].mailer[3]))=(UpperCase(maildata[z].mailer[3])))and((UpperCase(maildata[d].mailer[4]))=(UpperCase(maildata[z].mailer[4])))and((Uppercase(maildata[d].mailer[5]))=(UpperCase(maildata[z].mailer[5])))and((Uppercase(maildata[d].mailer[6]))=(UpperCase(maildata[z].mailer[6])))and((UpperCase(maildata[d].mailer[7]))=(UpperCase(maildata[z].mailer[7])))and((UpperCase(maildata[d].mailer[8]))=(UpperCase(maildata[z].mailer[8])))and((UpperCase(maildata[d].mailer[9]))=(UpperCase(maildata[z].mailer[9])))and((UpperCase(maildata[d].mailer[10]))=(UpperCase(maildata[z].mailer[10])))and((UpperCase(maildata[d].mailer[11]))=(UpperCase(maildata[z].mailer[11])))and((UpperCase(maildata[d].mailer[12]))=(UpperCase(maildata[z].mailer[12])))and((UpperCase(maildata[d].mailer[13]))>(UpperCase(maildata[z].mailer[13])))then begin
        maildata[temp]:=maildata[d];
        maildata[d]:=maildata[z];
        maildata[z]:=maildata[temp];
      end;
      if ((UpperCase(maildata[d].mailer[1]))=(UpperCase(maildata[z].mailer[1])))and((UpperCase(maildata[d].mailer[2]))=(UpperCase(maildata[z].mailer[2])))and((UpperCase(maildata[d].mailer[3]))=(UpperCase(maildata[z].mailer[3])))and((UpperCase(maildata[d].mailer[4]))=(UpperCase(maildata[z].mailer[4])))and((Uppercase(maildata[d].mailer[5]))=(UpperCase(maildata[z].mailer[5])))and((Uppercase(maildata[d].mailer[6]))=(UpperCase(maildata[z].mailer[6])))and((UpperCase(maildata[d].mailer[7]))=(UpperCase(maildata[z].mailer[7])))and((UpperCase(maildata[d].mailer[8]))=(UpperCase(maildata[z].mailer[8])))and((UpperCase(maildata[d].mailer[9]))=(UpperCase(maildata[z].mailer[9])))and((UpperCase(maildata[d].mailer[10]))=(UpperCase(maildata[z].mailer[10])))and((UpperCase(maildata[d].mailer[11]))=(UpperCase(maildata[z].mailer[11])))and((UpperCase(maildata[d].mailer[12]))=(UpperCase(maildata[z].mailer[12])))and((UpperCase(maildata[d].mailer[13]))=(UpperCase(maildata[z].mailer[13])))and((UpperCase(maildata[d].mailer[14]))>(UpperCase(maildata[z].mailer[14])))then begin
        maildata[temp]:=maildata[d];
        maildata[d]:=maildata[z];
        maildata[z]:=maildata[temp];
      end;
    end;
  end;
  //closefile(m);
end;

procedure maildatareport;
var
  x   : integer;
  b   : integer;
  bbs : string;
begin
  Assign(mailreport,'mailrpt.rpt');
  rewrite(mailreport);
  writeln(mailreport);
  writeln(mailreport,PadCenter('RCS Mailer Report Listing',78));
  writeln(mailreport,PadCenter(BBSName,78));
  writeln(mailreport,PadCenter('***********************',78));
  writeln(mailreport,PadCenter(echodate,78));
  writeln(mailreport,PadCenter('***********************',78));
  writeln(mailreport);
  writeln(mailreport,'       Mailer                                          Count');
  writeln(mailreport,PadCenter('-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-',78));
  writeln(mailreport);
  for x:=1 to lastrec-2 do
    begin
      if maildata[x].mailer<>'' then
      begin
        writeln(mailreport,'    '+PadRight(maildata[x].mailer,55)+'             '+PadRight(IntToStr(maildata[x].matches),20));
      end;
    end;
  writeln(mailreport);
  writeln(mailreport,PadCenter('-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-',78));
  writeln(mailreport);
  b:=78;
  bbs:='';
  insert(sysos,bbs,1);
  Insert(' ',bbs,1);
  Insert(ver,bbs,1);
  Insert(' v',bbs,1);
  Insert(prog,bbs,1);
  writeln(mailreport,PadCenter('Report Generated by: ',b));
  writeln(mailreport,PadCenter(bbs,b));
  writeln(mailreport);
  writeln(mailreport,PadCenter('RCS - CRBBS(2017-2018)',b));
  writeln(mailreport);
  closefile(mailreport);

end;

procedure wrapup;
begin
  ChDir(runpath);
  write(logfile,FormatDateTime('yyyy mmm dd hh:nn:ss',(Now)));
  writeln(logfile,' Completed All Processes');
  CloseFile(logfile);
  RenameFile('mystmult.log',LogsPath+'mystmult.log');
  writeln(prog+' '+ver+' '+sysos+' completed all tasks');
end;

{$R *.res}
begin
  startup;
  datecheck;
  if echobool=true then begin
    echostat;
    reportsort;
    echoreport;
  end;
  if nodebool=true then begin
    nodestat;
    try
      Assign(mislog,MysticLogs+'mis.log');
      {$I-}
      reset(mislog);
      {$I+}
//    if IOresult<>0 then
    except
     on E: EInOutError do begin
       writeln('File handling error occurred. Details: ',E.Message);
     end;
    end;
//    begin
//      writeln('File mis.log, could not be opened');
//      InsertLogDate;
//      LogBreak;
//      InsertLogDate;
//      writeln(logfile,' ERROR: Could not open MIS.LOG! ');
//      ProgramHalt;
//   end;
    MISNode:=1;
    reset(mislog);
    nodestatmis;
    MISNode:=2;
    reset(mislog);
    nodestatmis;
    MISNode:=3;
    reset(mislog);
    nodestatmis;
    MISNode:=4;
    reset(mislog);
    nodestatmis;
    MISNode:=5;
    reset(mislog);
    nodestatmis;
    MISNode:=6;
    reset(mislog);
    nodestatmis;
    MISNode:=7;
    reset(mislog);
    nodestatmis;
    MISNode:=8;
    reset(mislog);
    nodestatmis;
    MISNode:=9;
    reset(mislog);
    nodestatmis;
    MISNode:=10;
    reset(mislog);
    nodestatmis;
    MISNode:=11;
    reset(mislog);
    nodestatmis;
    MISNode:=12;
    reset(mislog);
    nodestatmis;
    MISNode:=13;
    reset(mislog);
    nodestatmis;
    MISNode:=14;
    reset(mislog);
    nodestatmis;
    MISNode:=15;
    reset(mislog);
    nodestatmis;
    Repeat
      if FileExists(MysticLogs+'mis.'+IntToStr(counter)+'.log')then begin
        try
          AssignFile(mislog,MysticLogs+'mis.'+IntToStr(counter)+'.log');
          reset(mislog);
        except
          on E: EInOutError do begin
          writeln('File handling error occurred. Details: ',E.Message);
          writeln(MysticLogs+'mis.'+IntToStr(counter)+'.log');
          ProgramHalt;
          end;
        end;
        MISNode:=1;
        reset(mislog);
        nodestatmis;
        MISNode:=2;
        reset(mislog);
        nodestatmis;
        MISNode:=3;
        reset(mislog);
        nodestatmis;
        MISNode:=4;
        reset(mislog);
        nodestatmis;
        MISNode:=5;
        reset(mislog);
        nodestatmis;
        MISNode:=6;
        reset(mislog);
        nodestatmis;
        MISNode:=7;
        reset(mislog);
        nodestatmis;
        MISNode:=8;
        reset(mislog);
        nodestatmis;
        MISNode:=9;
        reset(mislog);
        nodestatmis;
        MISNode:=10;
        reset(mislog);
        nodestatmis;
        MISNode:=11;
        reset(mislog);
        nodestatmis;
        MISNode:=12;
        reset(mislog);
        nodestatmis;
        MISNode:=13;
        reset(mislog);
        nodestatmis;
        MISNode:=14;
        reset(mislog);
        nodestatmis;
        MISNode:=15;
        reset(mislog);
        nodestatmis;
        inc(counter);
      end;
    until FileExists(MysticLogs+'mis.'+IntToStr(counter)+'.log')=false;
    nodestatfidopoll;
    if FileExists(MysticLogs+'qwkpoll.log') then nodeqwk;
    nodereport;
  end;
  if mailbool=true then begin
    Assign(m,'maillist.dat');
    {$I-}
    rewrite(m);
    {$I+}
    if IOresult<>0 then
    begin
      writeln('File maillist.dat could not be rewritten');
      InsertLogDate;
      LogBreak;
      InsertLogDate;
      writeln(logfile,' ERROR: Could not rewrite MAILLIST.DAT! ');
      ProgramHalt;
    end;
    seek(m,1);
    Assign(mislog,LogsPath+'mis.log');
    {$I-}
    reset(mislog);
    {$I+}
    if IOresult<>0 then
    begin
      writeln('File mis.log could not be opened');
      InsertLogDate;
      LogBreak;
      InsertLogDate;
      Writeln(logfile,' ERROR: Could not open MIS.LOG! ');
      ProgramHalt;
    end;
    InsertLogDate;
    LogBreak;
    InsertLogDate;
    writeln(logfile,' Reading the MIS.LOG file');
    s:='';
    lastrec:=1;
    repeat
      readln(mislog,s);
      if (AnsiStartsStr('+',s))and(echodate='') then
      begin
        Delete(s,1,2);
        Delete(s,11,80);
        echodate:=s;
      end;
    until (echodate<>'')or(eof(mislog));
    reset(mislog);
    readln(mislog,s);
    maildata[1].mailer:='';
    maildata[1].matches:=0;
    write(m,mail);
    maildatamis;
    if FileExists(MysticLogs+'mis.1.log') then begin
      Assign(mislog,LogsPath+'mis.1.log');
      {$I-}
      reset(mislog);
      {$I+}
      if IOresult<>0 then
      begin
        writeln('File mis.1.log could not be opened');
        InsertLogDate;
        LogBreak;
        InsertLogDate;
        Writeln(logfile,' ERROR: Could not open MIS.1.LOG! ');
        ProgramHalt;
      end;
      InsertLogDate;
      LogBreak;
      InsertLogDate;
      writeln(logfile,' Reading the MIS.1.LOG file');
      maildatamis;
    end;
    if FileExists(MysticLogs+'fidopoll.log') then begin
      Assign(mislog,LogsPath+'fidopoll.log');
      {$I-}
      reset(mislog);
      {$I+}
      if IOresult<>0 then
      begin
        writeln('File fidopoll.log could not be opened');
        InsertLogDate;
        LogBreak;
        InsertLogDate;
        Writeln(logfile,' ERROR: Could not open FIDOPOLL.LOG! ');
        ProgramHalt;
      end;
      InsertLogDate;
      LogBreak;
      InsertLogDate;
      writeln(logfile,' Reading the FIDOPOLL.LOG file');
      maildatamis;
    end;
    maildatasort;
    maildatareport;
  end;
  if onetbool=true then begin
    Assign(t,'onetlist.dat');
    {$I-}
    rewrite(t);
    {$I+}
    if IOresult<>0 then
    begin
      writeln('File onetlist.dat could not be rewritten');
      InsertLogDate;
      LogBreak;
      InsertLogDate;
      writeln(logfile,' ERROR: Could not rewrite ONETLIST.DAT! ');
      ProgramHalt;
    end;
    seek(t,1);
    Assign(mislog,LogsPath+'mis.log');
    {$I-}
    reset(mislog);
    {$I+}
    if IOresult<>0 then
    begin
      writeln('File mis.log could not be opened');
      InsertLogDate;
      LogBreak;
      InsertLogDate;
      Writeln(logfile,' ERROR: Could not open MIS.LOG! ');
      ProgramHalt;
    end;
    InsertLogDate;
    LogBreak;
    InsertLogDate;
    writeln(logfile,' Reading the MIS.LOG file');
    othernetmis;
    //seek(t,1);
    if FileExists(MysticLogs+'mis.1.log') then begin
      Assign(mislog,LogsPath+'mis.1.log');
      {$I-}
      reset(mislog);
      {$I+}
      if IOresult<>0 then
      begin
        writeln('File mis.1.log could not be opened');
        InsertLogDate;
        LogBreak;
        InsertLogDate;
        Writeln(logfile,' ERROR: Could not open MIS.1.LOG! ');
        ProgramHalt;
      end;
      InsertLogDate;
      LogBreak;
      InsertLogDate;
      writeln(logfile,' Reading the MIS.1.LOG file');
      othernetmis;
    end;
    if FileExists(MysticLogs+'fidopoll.log') then begin
      Assign(mislog,LogsPath+'fidopoll.log');
      {$I-}
      reset(mislog);
      {$I+}
      if IOresult<>0 then
      begin
        writeln('File fidopoll.log could not be opened');
        InsertLogDate;
        LogBreak;
        InsertLogDate;
        Writeln(logfile,' ERROR: Could not open FIDOPOLL.LOG! ');
        ProgramHalt;
      end;
      InsertLogDate;
      LogBreak;
      InsertLogDate;
      writeln(logfile,' Reading the FIDOPOLL.LOG file');
      othernetmis;
    end;
    closefile(t);
    onetsort;
    othernetreport;
  end;
  if logbool=true then begin
    createdirectory;
    movelogs;
  end;
  wrapup;
end.
