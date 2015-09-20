program compresskernel;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$ifdef win32}
 {$apptype console}
{$endif}
{$ifdef win64}
 {$apptype console}
{$endif}
{$h+}{$j+}

uses SysUtils,Classes;

type TCompressStatusHook=function(Current,Total:longint):boolean;

const FlagModel=0;
      PrevMatchModel=2;
      MatchLowModel=3;
      LiteralModel=35;
      Gamma0Model=291;
      Gamma1Model=547;
      SizeModels=803;

//{$define OptimalParsing}

function CompressLZBRAWithBruteforceOptimalParsing(SourcePointer,DestinationPointer:pointer;Size,WindowSize:longword;StatusHook:TCompressStatusHook):longword;
type PState=^TState;
     TState=record
      Code:longword;
      Range:longword;
      Model:array[0..SizeModels-1] of longword;
      LastWasMatch:boolean;
      LastOffset:longint;
     end;

     PTemporaryState=^TTemporaryState;
     TTemporaryState=record
      LastWasMatch:boolean;
      LastOffset:longint;
     end;

     PMatch=^TMatch;
     TMatch=record
      Index:longint;
      Next:PMatch;
     end;

     PMatches=^TMatches;
     TMatches=array[0..($7fffffff div sizeof(TMatch))-1] of TMatch;

     POptimalMatch=^TOptimalMatch;
     TOptimalMatch=record
      Offset:longint;
      Length:longint;
      State:TTemporaryState;
      case longint of
       0:(
        Bits:int64;
       );
       1:(
        Next:longint;
       );
     end;

     POptimalMatches=^TOptimalMatches;
     TOptimalMatches=array[0..($7fffffff div sizeof(TOptimalMatch))-1] of TOptimalMatch;

     PBytes=^TBytes;
     TBytes=array[0..$7ffffffe] of byte;

     PLongInts=^TLongInts;
     TLongInts=array[0..$1ffffffe] of longint;

var Source,Destination,EndPointer:pansichar;
    State:PState;
    Matches:PMatches;
    MatchSlots:PMatches;
    OptimalMatches:POptimalMatches;
    MinValues,MaxValues:PLongInts;
    GammaSizes:array[0..2047] of byte;

 function GetRealGammaSize(Value:longword):longword;
 var Mask:longword;
 begin
  result:=0;
  Mask:=Value shr 1;
  while (Mask and (Mask-1))<>0 do begin
   Mask:=Mask and (Mask-1);
  end;
  while Mask<>0 do begin
   inc(result,2);
   Mask:=Mask shr 1;
  end;
 end;

 function GetGammaSize(Value:longword):longword;
 begin
  if Value<=high(GammaSizes) then begin
   result:=GammaSizes[Value];
  end else begin
   result:=GetRealGammaSize(Value);
  end;
 end;

 function CountBits(var CurrentState:TTemporaryState;Offset,Len:longint):longint;
 var TemporaryOffset:longword;
 begin
  if (Len>1) and (Offset>0) then begin
   result:=1;
   if (not CurrentState.LastWasMatch) and (Offset=CurrentState.LastOffset) then begin
    inc(result);
   end else begin
    if not CurrentState.LastWasMatch then begin
     inc(result);
    end;
    TemporaryOffset:=Offset-1;
    inc(result,GetGammaSize((TemporaryOffset shr 4)+2));
    inc(result,4);
    dec(Len,ord(Offset>=96)+ord(Offset>=2048));
   end;
   inc(result,GetGammaSize(Len));
   CurrentState.LastWasMatch:=true;
   CurrentState.LastOffset:=Offset;
  end else begin
   result:=9;
   CurrentState.LastWasMatch:=false;
  end;
 end;

 procedure GetBruteforceOptimalMatches;
 const MaxLength=65536; 
 var Index,MatchIndex,BestLen,Offset,Len,PreviousIndex:longint;
     Bits:int64;
     Match:PMatch;
     OptimalMatch:POptimalMatch;
     Source:pointer;
     TemporaryState:TTemporaryState;
 begin
  Source:=SourcePointer;
  OptimalMatches^[0].Bits:=8;
  OptimalMatches^[0].State.LastWasMatch:=false;
  OptimalMatches^[0].State.LastOffset:=-1;
  for Index:=1 to Size-1 do begin
   if assigned(StatusHook) then begin
    StatusHook(Index,Size*2);
   end;
   OptimalMatches^[Index].Bits:=OptimalMatches^[Index-1].Bits+9;
   OptimalMatches^[Index].State:=OptimalMatches^[Index-1].State;
   MatchIndex:=(PBytes(Source)^[Index-1] shl 8) or PBytes(Source)^[Index];
   BestLen:=1;
   Match:=@Matches^[MatchIndex];
   while assigned(Match^.Next) and (BestLen<MaxLength) do begin
    Offset:=Index-Match^.Next^.Index;
    if Offset>longint(WindowSize) then begin
     Match^.Next:=nil;
     break;
    end;
    Len:=2;
    while Len<=MaxLength do begin
     if Len>BestLen then begin
      BestLen:=Len;
      if (Len>1) and ((Offset>0) and ((Offset<96) or ((Offset>96) and (Len>3)) or ((Offset>2048) and (Len>4)))) then begin
       TemporaryState:=OptimalMatches^[Index-Len].State;
       Bits:=OptimalMatches^[Index-Len].Bits+CountBits(TemporaryState,Offset,Len);
       OptimalMatch:=@OptimalMatches^[Index];
       if OptimalMatch^.Bits>Bits then begin
        OptimalMatch^.Bits:=Bits;
        OptimalMatch^.Offset:=Offset;
        OptimalMatch^.Length:=Len;
        OptimalMatch^.State:=TemporaryState;
       end;
      end;
     end else if ((Index+1)=MaxValues^[Offset]+Len) and (MaxValues^[Offset]<>0) then begin
      Len:=Index-MinValues^[Offset];
      if Len>BestLen then begin
       Len:=BestLen;
      end;
     end;
     if (Index<(Offset+Len)) or (PBytes(Source)^[Index-Len]<>PBytes(Source)^[(Index-Len)-Offset]) then begin
      break;
     end;
     inc(Len);
    end;
    MinValues^[Offset]:=(Index-Len)+1;
    MaxValues^[Offset]:=Index;
    Match:=Match^.Next;
   end;
   MatchSlots^[Index].Index:=Index;
   MatchSlots^[Index].Next:=Matches^[MatchIndex].Next;
   Matches^[MatchIndex].Next:=@MatchSlots^[Index];
  end;
  Index:=Size-1;
  OptimalMatches^[Index].Next:=0;
  while Index>0 do begin
   OptimalMatch:=@OptimalMatches^[Index];
   if OptimalMatch^.Length>0 then begin
    PreviousIndex:=Index-OptimalMatch^.Length;
   end else begin
    PreviousIndex:=Index-1;
   end;
   OptimalMatches^[PreviousIndex].Next:=Index;
   Index:=PreviousIndex;
  end;
 end;

 function EncodeBit(var CurrentState:TState;ModelIndex,Move,Bit:longint):longint;
 var Bound,OldCode:longword;
     p:pansichar;
 begin
  Bound:=(CurrentState.Range shr 12)*CurrentState.Model[ModelIndex];
  if Bit=0 then begin
   CurrentState.Range:=Bound;
   inc(CurrentState.Model[ModelIndex],(4096-CurrentState.Model[ModelIndex]) shr Move);
  end else begin
   OldCode:=CurrentState.Code;
   inc(CurrentState.Code,Bound);
   dec(CurrentState.Range,Bound);
   dec(CurrentState.Model[ModelIndex],CurrentState.Model[ModelIndex] shr Move);
   if CurrentState.Code<OldCode then begin
    p:=@Destination[-1];
    while p^=#$ff do begin
     p^:=#0;
     dec(p);
    end;
    inc(p^);
   end;
  end;
  while CurrentState.Range<$1000000 do begin
   byte(pointer(Destination)^):=CurrentState.Code shr 24;
   inc(Destination);
   CurrentState.Code:=CurrentState.Code shl 8;
   CurrentState.Range:=CurrentState.Range shl 8;
  end;
  result:=Bit;
 end;

 procedure EncoderFlush(var CurrentState:TState);
 var OldCode:longword;
     p:pansichar;
 begin
  OldCode:=CurrentState.Code;
  if CurrentState.Range>$2000000 then begin
   inc(CurrentState.Code,$1000000);
   CurrentState.Range:=$800000;
  end else begin
   inc(CurrentState.Code,$800000);
   CurrentState.Range:=$8000;
  end;
  if CurrentState.Code<OldCode then begin
   p:=@Destination[-1];
   while p^=#$ff do begin
    p^:=#0;
    dec(p);
   end;
   inc(p^);
  end;
  while (CurrentState.Range<$1000000) or ((Destination-pansichar(DestinationPointer))<4) do begin
   byte(pointer(Destination)^):=CurrentState.Code shr 24;
   inc(Destination);
   CurrentState.Code:=CurrentState.Code shl 8;
   CurrentState.Range:=CurrentState.Range shl 8;
  end;
  longword(pointer(@pansichar(DestinationPointer)[0])^):=(byte(pansichar(DestinationPointer)[0]) shl 24) or (byte(pansichar(DestinationPointer)[1]) shl 16) or (byte(pansichar(DestinationPointer)[2]) shl 8) or byte(pansichar(DestinationPointer)[3]);
 end;

 procedure EncodeTree(var CurrentState:TState;ModelIndex,Bits,Move,Value:longint);
 var Context:longint;
 begin
  Context:=1;
  while Bits>0 do begin
   dec(Bits);
   Context:=(Context shl 1) or EncodeBit(CurrentState,ModelIndex+Context,Move,(Value shr Bits) and 1);
  end;
 end;

 procedure EncodeGamma(var CurrentState:TState;ModelIndex,Value:longword);
 var Mask:longword;
     Context:byte;
 begin
  Context:=1;
  Mask:=Value shr 1;
  while (Mask and (Mask-1))<>0 do begin
   Mask:=Mask and (Mask-1);
  end;
  while Mask<>0 do begin
   Context:=(Context shl 1) or EncodeBit(CurrentState,ModelIndex+Context,5,(0-(Mask shr 1)) shr 31);
   Context:=(Context shl 1) or longword(EncodeBit(CurrentState,ModelIndex+Context,5,(0-(Value and Mask)) shr 31));
   Mask:=Mask shr 1;
  end;
 end;

 procedure EncodeEnd(var CurrentState:TState;ModelIndex:longint);
 var Bits:longword;
     Context:byte;
 begin
  Context:=1;
  Bits:=32;
  while Bits>0 do begin
   dec(Bits);
   Context:=(Context shl 1) or EncodeBit(CurrentState,ModelIndex+Context,5,(0-Bits) shr 31);
   EncodeBit(CurrentState,ModelIndex+Context,5,0);
   Context:=Context shl 1;
  end;
 end;

 procedure PutResult(var CurrentState:TState;var Source:pansichar;Offset,Len:longint);
 var TemporaryOffset:longword;
 begin
  if (Len>1) and (Offset>0) then begin
   inc(Source,Len);
   EncodeBit(CurrentState,FlagModel+byte(boolean(CurrentState.LastWasMatch)),5,1);
   if (not CurrentState.LastWasMatch) and (Offset=CurrentState.LastOffset) then begin
    EncodeBit(CurrentState,PrevMatchModel,5,1);
   end else begin
    if not CurrentState.LastWasMatch then begin
     EncodeBit(CurrentState,PrevMatchModel,5,0);
    end;
    TemporaryOffset:=Offset-1;
    EncodeGamma(CurrentState,Gamma0Model,(TemporaryOffset shr 4)+2);
    EncodeTree(CurrentState,MatchLowModel+(ord((TemporaryOffset shr 4)<>0) shl 4),4,5,TemporaryOffset and $f);
    dec(Len,ord(Offset>=96)+ord(Offset>=2048));
   end;
   EncodeGamma(CurrentState,Gamma1Model,Len);
   CurrentState.LastWasMatch:=true;
   CurrentState.LastOffset:=Offset;
  end else begin
   EncodeBit(CurrentState,FlagModel+byte(boolean(CurrentState.LastWasMatch)),5,0);
   EncodeTree(CurrentState,LiteralModel,8,4,byte(pointer(Source)^));
   inc(Source);
   CurrentState.LastWasMatch:=false;
  end;
 end;

var OptimalMatch:POptimalMatch;
    Index:longint;
begin
 result:=0;
 if Size>0 then begin

  GetMem(Matches,65536*SizeOf(TMatch));
  FillChar(Matches^,65536*SizeOf(TMatch),#0);

  GetMem(MatchSlots,(Size+1)*SizeOf(TMatch));
  FillChar(MatchSlots^,(Size+1)*SizeOf(TMatch),#0);

  GetMem(OptimalMatches,(Size+1)*SizeOf(TOptimalMatch));
  FillChar(OptimalMatches^,(Size+1)*SizeOf(TOptimalMatch),#0);

  GetMem(MinValues,(Size+1)*SizeOf(longint));
  FillChar(MinValues^,(Size+1)*SizeOf(longint),#0);

  GetMem(MaxValues,(Size+1)*SizeOf(longint));
  FillChar(MaxValues^,(Size+1)*SizeOf(longint),#0);

  GetMem(State,SizeOf(TState));
  State^.Range:=$ffffffff;
  State^.Code:=0;
  for Index:=0 to SizeModels-1 do begin
   State^.Model[Index]:=2048;
  end;
  State^.LastWasMatch:=false;
  State^.LastOffset:=-1;

  for Index:=low(GammaSizes) to high(GammaSizes) do begin
   GammaSizes[Index]:=GetRealGammaSize(Index);
  end;

  GetBruteforceOptimalMatches;

  Source:=SourcePointer;

  Destination:=DestinationPointer;
  EndPointer:=Source;
  inc(EndPointer,Size);

  EncodeTree(State^,LiteralModel,8,4,byte(pointer(Source)^));
  inc(Source);

  Index:=0;
  while longword(Source)<longword(EndPointer) do begin
   Index:=OptimalMatches^[Index].Next;
   if Index=0 then begin
    break;
   end;
   if assigned(StatusHook) then begin
    StatusHook(longword(pansichar(Source)-pansichar(SourcePointer))+Size,Size*2);
   end;
   OptimalMatch:=@OptimalMatches^[Index];
   if OptimalMatch^.Length>0 then begin
    PutResult(State^,Source,OptimalMatch^.Offset,OptimalMatch^.Length);
   end else begin
    PutResult(State^,Source,0,1);
   end;
  end;

  EncodeBit(State^,FlagModel+byte(boolean(State.LastWasMatch)),5,1);
  if not State.LastWasMatch then begin
   EncodeBit(State^,PrevMatchModel,5,0);
  end;
  EncodeEnd(State^,Gamma0Model);

  EncoderFlush(State^);

  if assigned(StatusHook) then begin
   StatusHook(Size*2,Size*2);
  end;
  
  FreeMem(State);
  FreeMem(MaxValues);
  FreeMem(MinValues);
  FreeMem(OptimalMatches);
  FreeMem(MatchSlots);
  FreeMem(Matches);

  result:=Destination-pansichar(DestinationPointer);
 end;
end;

function CompressLZBRA(SourcePointer,DestinationPointer:pointer;Size,WindowSize:longword;StatusHook:TCompressStatusHook):longword;
type PNode=^TNode;
     TNode=record
      DataPointer:pointer;
      Previous,Next:PNode;
     end;

     PNodes=^TNodes;
     TNodes=array[0..($7fffffff div sizeof(TNode))-1] of TNode;

     PRecentNodes=^TRecentNodes;
     TRecentNodes=array[byte] of PNode;

     PBytes=^TBytes;
     TBytes=array[0..$7ffffffe] of byte;

     PLongInts=^TLongInts;
     TLongInts=array[0..$1ffffffe] of longint;

var Source,Destination,EndPointer,LastHashed:pansichar;
    Nodes:PNodes;
    RecentNodes:PRecentNodes;
    NodePosition:longword;
    Code,Range:longword;
    Model:array[0..SizeModels-1] of longword;
    LastWasMatch:boolean;
    LastPosition:longint;

 function AddNode(Data:pansichar):boolean;
 var Prefix:byte;
     LastNode:PNode;
     NewNode:PNode;
 begin
  result:=NodePosition<(Size-1);
  if result then begin
   Prefix:=byte(pointer(Data)^);
   LastNode:=RecentNodes^[Prefix];
   NewNode:=@Nodes^[NodePosition];
   with NewNode^ do begin
    DataPointer:=Data;
    Previous:=LastNode;
    Next:=nil;
   end;
   if assigned(LastNode) then begin
    LastNode^.Next:=NewNode;
   end;
   RecentNodes^[Prefix]:=NewNode;
   inc(NodePosition);
  end;
 end;

 function RemoveNode(Data:pointer):boolean;
 var Prefix:word;
     Node:PNode;
 begin
  result:=NodePosition<(Size-1);
  if result then begin
   Prefix:=byte(Data^);
   Node:=RecentNodes^[Prefix];
   if assigned(Node) and (Node^.DataPointer=Data) and (Node=@Nodes^[NodePosition-1]) then begin
    RecentNodes^[Prefix]:=Node^.Previous;
    if assigned(Node^.Previous) then begin
     Node^.Previous^.Next:=Node^.Next;
    end;
    if assigned(Node^.Next) then begin
     Node^.Next^.Previous:=Node^.Previous;
    end;
    Node^.Previous:=nil;
    Node^.Next:=nil;
    dec(NodePosition);
   end;
  end;
 end;

 procedure DoHash(Source:pansichar);
 begin
  while LastHashed<Source do begin
   AddNode(LastHashed);
   inc(LastHashed);
  end;
 end;

 procedure DoUnhash(Source:pansichar);
 begin
  while LastHashed>Source do begin
   dec(LastHashed);
   RemoveNode(LastHashed);
  end;
 end;

 function EncodeBit(ModelIndex,Move,Bit:longint):longint;
 var Bound,OldCode:longword;
     p:pansichar;
 begin
  Bound:=(Range shr 12)*Model[ModelIndex];
  if Bit=0 then begin
   Range:=Bound;
   inc(Model[ModelIndex],(4096-Model[ModelIndex]) shr Move);
  end else begin
   OldCode:=Code;
   inc(Code,Bound);
   dec(Range,Bound);
   dec(Model[ModelIndex],Model[ModelIndex] shr Move);
   if Code<OldCode then begin
    p:=@Destination[-1];
    while p^=#$ff do begin
     p^:=#0;
     dec(p);
    end;
    inc(p^);
   end;
  end;
  while Range<$1000000 do begin
   byte(pointer(Destination)^):=Code shr 24;
   inc(Destination);
   Code:=Code shl 8;
   Range:=Range shl 8;
  end;
  result:=Bit;
 end;

 procedure EncoderFlush;
 var OldCode:longword;
     p:pansichar;
 begin
  OldCode:=Code;
  if Range>$2000000 then begin
   inc(Code,$1000000);
   Range:=$800000;
  end else begin
   inc(Code,$800000);
   Range:=$8000;
  end;
  if Code<OldCode then begin
   p:=@Destination[-1];
   while p^=#$ff do begin
    p^:=#0;
    dec(p);
   end;
   inc(p^);
  end;
  while (Range<$1000000) or ((Destination-pansichar(DestinationPointer))<4) do begin
   byte(pointer(Destination)^):=Code shr 24;
   inc(Destination);
   Code:=Code shl 8;
   Range:=Range shl 8;
  end;
  longword(pointer(@pansichar(DestinationPointer)[0])^):=(byte(pansichar(DestinationPointer)[0]) shl 24) or (byte(pansichar(DestinationPointer)[1]) shl 16) or (byte(pansichar(DestinationPointer)[2]) shl 8) or byte(pansichar(DestinationPointer)[3]);
 end;

 procedure EncodeTree(ModelIndex,Bits,Move,Value:longint);
 var Context:longint;
 begin
  Context:=1;
  while Bits>0 do begin
   dec(Bits);
   Context:=(Context shl 1) or EncodeBit(ModelIndex+Context,Move,(Value shr Bits) and 1);
  end;
 end;

 procedure EncodeGamma(ModelIndex,Value:longword);
 var Mask:longword;
     Context:byte;
 begin
  Context:=1;
  Mask:=Value shr 1;
  while (Mask and (Mask-1))<>0 do begin
   Mask:=Mask and (Mask-1);
  end;
  while Mask<>0 do begin
   Context:=(Context shl 1) or EncodeBit(ModelIndex+Context,5,(0-(Mask shr 1)) shr 31);
   Context:=(Context shl 1) or longword(EncodeBit(ModelIndex+Context,5,(0-(Value and Mask)) shr 31));
   Mask:=Mask shr 1;
  end;
 end;

 procedure EncodeEnd(ModelIndex:longint);
 var Bits:longword;
     Context:byte;
 begin
  Context:=1;
  Bits:=32;
  while Bits>0 do begin
   dec(Bits);
   Context:=(Context shl 1) or EncodeBit(ModelIndex+Context,5,(0-Bits) shr 31);
   EncodeBit(ModelIndex+Context,5,0);
   Context:=Context shl 1;
  end;
 end;

 function CompareBytes(FirstComparePointer,SecondComparePointer:pansichar):longword;
 begin
  result:=0;
  while (SecondComparePointer<EndPointer) and (FirstComparePointer^=SecondComparePointer^) do begin
   inc(result);
   inc(FirstComparePointer);
   inc(SecondComparePointer);
  end;
 end;

 procedure DoSearch(Source:pansichar;var BestPosition,BestFoundLength:longint);
 var SearchPointer:pansichar;
     FoundLength,Position:longint;
     Node:PNode;
 begin
  BestPosition:=0;
  BestFoundLength:=1;
  Node:=RecentNodes^[byte(pointer(Source)^)];
  while assigned(Node) and ((longword(Source)-longword(Node^.DataPointer))<=WindowSize) do begin
   SearchPointer:=Node^.DataPointer;
   FoundLength:=CompareBytes(SearchPointer,Source);
   if FoundLength>1 then begin
    Position:=pansichar(Source)-pansichar(SearchPointer);
    if ((Position>0) and ((Position<96) or ((Position>96) and (FoundLength>3)) or ((Position>2048) and (FoundLength>4)))) and
       ((BestFoundLength<FoundLength) or (((BestFoundLength=FoundLength) and (Position<=BestPosition)))) then begin
     BestFoundLength:=FoundLength;
     BestPosition:=Position;
    end;
   end;
   Node:=Node^.Previous;
  end;
 end;

 procedure PutResult(var Source:pansichar;BestPosition,BestFoundLength:longint); register;
 var Offset:longword;
 begin
  if (BestFoundLength>1) and (BestPosition>0) then begin
   inc(Source,BestFoundLength);
   EncodeBit(FlagModel+byte(boolean(LastWasMatch)),5,1);
   if (not LastWasMatch) and (BestPosition=LastPosition) then begin
    EncodeBit(PrevMatchModel,5,1);
   end else begin
    if not LastWasMatch then begin
     EncodeBit(PrevMatchModel,5,0);
    end;
    Offset:=BestPosition-1;
    EncodeGamma(Gamma0Model,(Offset shr 4)+2);
    EncodeTree(MatchLowModel+(ord((Offset shr 4)<>0) shl 4),4,5,Offset and $f);
    dec(BestFoundLength,ord(BestPosition>=96)+ord(BestPosition>=2048));
   end;
   EncodeGamma(Gamma1Model,BestFoundLength);
   LastWasMatch:=true;
   LastPosition:=BestPosition;
  end else begin
   EncodeBit(FlagModel+byte(boolean(LastWasMatch)),5,0);
   EncodeTree(LiteralModel,8,4,byte(pointer(Source)^));
   inc(Source);
   LastWasMatch:=false;
  end;
 end;

var BestPosition,BestFoundLength:longint;
    LookaheadBestPosition,LookaheadBestFoundLength:longint;
    Lookahead,OldLastHashed:pansichar;
begin
 result:=0;
 if Size>0 then begin

  GetMem(Nodes,Size*sizeof(TNode));
  GetMem(RecentNodes,sizeof(TRecentNodes));
  FillChar(Nodes^,Size*sizeof(TNode),#0);
  FillChar(RecentNodes^,sizeof(TRecentNodes),#0);

  for BestPosition:=0 to SizeModels-1 do begin
   Model[BestPosition]:=2048;
  end;

  NodePosition:=0;
  Source:=SourcePointer;
  LastHashed:=Source;

  Destination:=DestinationPointer;
  EndPointer:=Source;
  inc(EndPointer,Size);

  Range:=$ffffffff;
  Code:=0;

  LastPosition:=-1;
  LastWasMatch:=false;

  BestPosition:=0;
  BestFoundLength:=0;

  LookaheadBestPosition:=0;
  LookaheadBestFoundLength:=0;

  EncodeTree(LiteralModel,8,4,byte(pointer(Source)^));
  inc(Source);

  while longword(Source)<longword(EndPointer) do begin
   if assigned(StatusHook) then begin
    StatusHook(pansichar(Source)-pansichar(SourcePointer),Size);
   end;
   DoSearch(Source,BestPosition,BestFoundLength);
   if BestFoundLength>1 then begin
    Lookahead:=Source;
    OldLastHashed:=LastHashed;
    while Lookahead<pansichar(@Source[BestFoundLength]) do begin
     inc(Lookahead);
     DoHash(Lookahead);
     DoSearch(Lookahead,LookaheadBestPosition,LookaheadBestFoundLength);
     if LookaheadBestFoundLength>0 then begin
      if (BestFoundLength+(Lookahead-Source))<=LookaheadBestFoundLength then begin
       BestPosition:=0;
       BestFoundLength:=1;
      end;
      break;
     end;
    end;
    DoUnhash(OldLastHashed);
   end;
   PutResult(Source,BestPosition,BestFoundLength);
   DoHash(Source);
  end;

  EncodeBit(FlagModel+byte(boolean(LastWasMatch)),5,1);
  if not LastWasMatch then begin
   EncodeBit(PrevMatchModel,5,0);
  end;
  EncodeEnd(Gamma0Model);

  EncoderFlush;

  FreeMem(RecentNodes);
  FreeMem(Nodes);

  result:=Destination-pansichar(DestinationPointer);
 end;
end;

function DecompressLZBRA(Source,Destination:pansichar):longword;
var Code,Range:longword;
    Model:array[0..SizeModels-1] of longword;

 function DecodeBit(ModelIndex,Move:longint):longint;
 var Bound:longword;
 begin
  Bound:=(Range shr 12)*Model[ModelIndex];
  if Code<Bound then begin
   Range:=Bound;
   inc(Model[ModelIndex],(4096-Model[ModelIndex]) shr Move);
   result:=0;
  end else begin
   dec(Code,Bound);
   dec(Range,Bound);
   dec(Model[ModelIndex],Model[ModelIndex] shr Move);
   result:=1;
  end;
  while Range<$1000000 do begin
   Code:=(Code shl 8) or byte(pointer(Source)^);
   inc(Source);
   Range:=Range shl 8;
  end;
 end;

 function DecodeTree(ModelIndex,MaxValue,Move:longint):longint;
 begin
  result:=1;
  while result<MaxValue do begin
   result:=(result shl 1) or DecodeBit(ModelIndex+result,Move);
  end;
  dec(result,MaxValue);
 end;

 function DecodeGamma(ModelIndex:longint):longint;
 var Context:byte;
 begin
  result:=1;
  Context:=1;
  repeat
   Context:=(Context shl 1) or DecodeBit(ModelIndex+Context,5);
   result:=(result shl 1) or DecodeBit(ModelIndex+Context,5);
   Context:=(Context shl 1) or (result and 1);
  until (Context and 2)=0;
 end;

var Len,Offset,LastOffset:longint;
    Flag,LastWasMatch:boolean;
    DestinationBegin:pansichar;
begin
 DestinationBegin:=Destination;

 Code:=longword(pointer(Source)^);
 inc(Source,sizeof(longword));

 Range:=$ffffffff;

 for Len:=0 to SizeModels-1 do begin
  Model[Len]:=2048;
 end;

 LastOffset:=0;

 LastWasMatch:=false;

 Flag:=false;

 while true do begin

  if Flag then begin
   if (not LastWasMatch) and (DecodeBit(PrevMatchModel,5)<>0) then begin
    Offset:=LastOffset;
    Len:=0;
   end else begin
    Offset:=DecodeGamma(Gamma0Model);
    if Offset=0 then begin
     result:=Destination-pansichar(DestinationBegin);
     exit;
    end;
    dec(Offset,2);
    Offset:=((Offset shl 4)+DecodeTree(MatchLowModel+(ord(Offset<>0) shl 4),16,5))+1;
    Len:=ord(Offset>=96)+ord(Offset>=2048);
   end;
   LastOffset:=Offset;
   LastWasMatch:=true;
   inc(Len,DecodeGamma(Gamma1Model));
   while Len>0 do begin
    dec(Len);
    Destination^:=Destination[-Offset];
    inc(Destination);
   end;
  end else begin
   byte(pointer(Destination)^):=DecodeTree(LiteralModel,256,4);
   inc(Destination);
   LastWasMatch:=false;
  end;

  Flag:=boolean(byte(DecodeBit(FlagModel+byte(boolean(LastWasMatch)),5)));

 end;
end;                        

const OldTrg:integer=-1;

function CompressStatusHook(Current,Total:integer):boolean; 
const Max=58;
var Trg,Cnt:integer;
    Buf:array[0..Max+2] of char;
begin
 result:=true;
 if Total=0 then begin
  Total:=1;
 end;
 Trg:=(Current*Max) div Total;
 if OldTrg<>Trg then begin
  OldTrg:=Trg;
  write(#13+'Compressing... ');
  Buf[0]:='[';
  Buf[Max+1]:=']';
  Buf[Max+2]:=#0;
  for Cnt:=1 to Max do begin
   if Cnt<=Trg then begin
    Buf[Cnt]:='#';
   end else begin
    Buf[Cnt]:='-';
   end;
  end;
  write(PAnsiChar(@Buf));
 end;
end;

const Signature:array[0..1] of ansichar='mz';

var SourceMemoryStream,DestinationMemoryStream{,OtherDestinationMemoryStream}:TMemoryStream;
    Size,WindowSize:longword;
begin
 if ParamCount>2 then begin
  WindowSize:=StrToIntDef(ParamStr(3),16);
  if WindowSize<16 then begin
   WindowSize:=16;
  end;
 end else begin
  WindowSize:=65536;
 end;
 OldTrg:=-1;
 SourceMemoryStream:=TMemoryStream.Create;
 try
  SourceMemoryStream.LoadFromFile(ParamStr(1));
  DestinationMemoryStream:=TMemoryStream.Create;
  try
   DestinationMemoryStream.Write(Signature,SizeOf(Signature));
   Size:=SourceMemoryStream.Size;
   DestinationMemoryStream.Write(Size,SizeOf(longword));
   DestinationMemoryStream.Size:=SizeOf(Signature)+SizeOf(longword)+(SourceMemoryStream.Size*3);
   if ParamCount>3 then begin
    Size:=CompressLZBRAWithBruteforceOptimalParsing(SourceMemoryStream.Memory,@PAnsiChar(DestinationMemoryStream.Memory)[SizeOf(Signature)+SizeOf(longword)],SourceMemoryStream.Size,WindowSize,CompressStatusHook);
   end else begin
    Size:=CompressLZBRA(SourceMemoryStream.Memory,@PAnsiChar(DestinationMemoryStream.Memory)[SizeOf(Signature)+SizeOf(longword)],SourceMemoryStream.Size,WindowSize,CompressStatusHook);
   end;
   DestinationMemoryStream.Size:=SizeOf(Signature)+SizeOf(longword)+Size;
   DestinationMemoryStream.SaveToFile(ParamStr(2));
   OldTrg:=-1;
   CompressStatusHook(100,100);
{  OtherDestinationMemoryStream:=TMemoryStream.Create;
   try
    OtherDestinationMemoryStream.Size:=SourceMemoryStream.Size*4;
    OtherDestinationMemoryStream.Size:=DecompressLZBRA(@PAnsiChar(DestinationMemoryStream.Memory)[SizeOf(Signature)+SizeOf(longword)],OtherDestinationMemoryStream.Memory);
    OtherDestinationMemoryStream.SaveToFile('output2.dat');
   finally
    OtherDestinationMemoryStream.Free;
   end;{}
  finally
   DestinationMemoryStream.Free;
  end;
 finally
  SourceMemoryStream.Free;
 end;
 writeln;
end.
