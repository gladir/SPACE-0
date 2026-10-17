{ @author: Sylvain Maltais (support@gladir.com)
 @created: 2026
 @website(https://www.gladir.com/space-0)
 @abstract(Target: Turbo Pascal 7, Free Pascal 3.2) }

Program CKBRIEF;

{$A-}
{$E+,N+}

{ Resume du contenu et de la couverture temporelle de fichiers CK (DAF),
  sous-ensemble autonome de CKBRIEF NAIF : aucune unite, aucune inclusion.
  La couverture est celle des segments (debut/fin), fusionnee par objet
  (instrument) sur l'ensemble des fichiers; les lacunes internes d'un segment
  (types 2 et 3) ne sont pas analysees.
  Les temps d'un CK sont des ticks SCLK. Avec un SCLK texte de type 1 (-sclk),
  ils sont convertis en chaine SCLK, ET ou UTC (table NAIF0012 integree,
  1972-2017, 37 s ensuite). Le SCLK doit couvrir l'engin de l'instrument
  (code engin = code instrument Div 1000).
  Ticks continus a travers les partitions, comme les CK et les coefficients
  SCLK01 (precision exacte jusqu'a 2^53 ticks).
  Pas de SCLK type 2, de LSK externe, de FK ni de noms d'objets externes.
  Sources : naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/ug/ckbrief.html
            naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/ck.html
            naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/sclk.html
            naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/daf.html
  Compilation : fpc -Mtp CKBRIEF.PAS
  Limites DOS : 1200 segments, 40 fichiers, fichiers < 2 Gio. }

Const
 MaxSegments = 1200;
 MaxFiles = 40;
 MaxParts = 100;
 MaxCoef = 7500;
 LeapCount = 28;
 LeapYears: Array[1..LeapCount] Of Integer =
  (1972,1972,1973,1974,1975,1976,1977,1978,1979,1980,1981,1982,
   1983,1985,1988,1990,1991,1992,1993,1994,1996,1997,1999,2006,
   2009,2012,2015,2017);
 LeapMonths: Array[1..LeapCount] Of Integer =
  (1,7,1,1,1,1,1,1,1,1,7,7,7,7,1,1,1,7,7,7,1,7,1,1,1,7,7,1);
 MinEpoch = -63082324800.0;
 MaxEpoch = 252455572800.0;

Type
 TBlock = Array[0..1023] Of Byte;
 TBytes8 = Array[0..7] Of Byte;
 TSeg = Record
  First, Last: Double;
  Inst, Frame, Kind, AV, NameRec: LongInt;
  Slot, FileNo: Integer;
 End;
 TSegArray = Array[1..MaxSegments] Of TSeg;
 PSegArray = ^TSegArray;
 TCoefArray = Array[1..MaxCoef] Of Double;
 PCoefArray = ^TCoefArray;

Var
 Kernel: File;
 Buffer: TBlock;
 Segs: PSegArray;
 Coef: PCoefArray;
 LeapEpoch: Array[1..LeapCount] Of Double;
 FileArg: Array[1..MaxFiles] Of Integer;
 FileCount, Total: Integer;
 Opened, BigEndian, DataError, Tabular, Dump: Boolean;
 FileBytes: LongInt;
 CurFile: Integer;
 TimeMode: Integer;
 SclkPath, Msg: String;
 { Horloge SCLK type 1 }
 HaveClock: Boolean;
 ClockID: LongInt;
 NFields, NParts, NCoef, ClockDelim, NOffsets: Integer;
 Moduli, Offsets: Array[1..10] Of Double;
 PStart, PEnd: Array[1..MaxParts] Of Double;
 { Fichier texte SCLK }
 SF: Text;
 SLine: String;
 SPos: Integer;
 SInData: Boolean;
 Arg: String;
 ArgIndex, I: Integer;

Procedure CloseKernel;
Var Err: Word;
Begin
 If Opened Then Begin
  {$I-} Close(Kernel); Err := IOResult; {$I+}
  Opened := False;
 End;
End;

Procedure Fail(S: String; Code: Integer);
Begin
 WriteLn('Erreur : ',S);
 CloseKernel;
 Halt(Code);
End;

Procedure SyntaxError(S: String);
Begin
 Fail(S+' Voir CKBRIEF -h.',2);
End;

Function Upper(S: String): String;
Var K: Integer;
Begin
 For K := 1 To Length(S) Do S[K] := UpCase(S[K]);
 Upper := S;
End;

Function Trim(S: String): String;
Begin
 While (Length(S) > 0) And (S[1] <= ' ') Do Delete(S,1,1);
 While (Length(S) > 0) And (S[Length(S)] <= ' ') Do Delete(S,Length(S),1);
 Trim := S;
End;

Function Num(N: LongInt): String;
Var S: String;
Begin
 Str(N,S);
 Num := S;
End;

Function Zero(N: LongInt; Width: Integer): String;
Var S: String;
Begin
 Str(N,S);
 While Length(S) < Width Do S := '0'+S;
 Zero := S;
End;

Function PadRight(S: String; Width: Integer): String;
Begin
 While Length(S) < Width Do S := S+' ';
 PadRight := S;
End;

Procedure Help;
Begin
 WriteLn('CKBRIEF - Cette commande permet de r'+#130+'sumer le contenu et la couverture ',
         'temporelle des fichiers d''orientation CK.');
 WriteLn;
 WriteLn('Syntaxe : CKBRIEF [options] <fichier.bc> [<fichier.bc> ...]');
 WriteLn;
 WriteLn('Options :');
 WriteLn('  -sclk <fichier>  Kernel SCLK texte (type 1) pour convertir les ticks');
 WriteLn('  -ticks           Temps en ticks SCLK (par defaut sans -sclk)');
 WriteLn('  -sclkstr         Temps en chaines SCLK partition/champs (-sclk requis)');
 WriteLn('  -utc             Dates UTC (-sclk requis; defaut avec -sclk)');
 WriteLn('  -et              Dates ET/TDB (-sclk requis)');
 WriteLn('  -etsec           Secondes ET/TDB depuis J2000 (-sclk requis)');
 WriteLn('  -t               Affichage en tableau');
 WriteLn('  -dump            Detail de chaque segment (avant le resume fusionne)');
 WriteLn('  -h, /?           Aide (-help et --help acceptes)');
 WriteLn;
 WriteLn('Le resume fusionne, par instrument, les segments de tous les fichiers.');
 WriteLn('Couverture = debut/fin des segments (lacunes internes non analysees).');
 WriteLn;
 WriteLn('Exemples :');
 WriteLn('  CKBRIEF mission.bc');
 WriteLn('  CKBRIEF -sclk cassini.tsc -utc -t mission1.bc mission2.bc');
 WriteLn('  CKBRIEF -dump mission.bc');
 WriteLn;
 WriteLn('Limites : CK DAF LTL-IEEE/BIG-IEEE, 1200 segments, SCLK type 1,');
 WriteLn('code engin = code instrument Div 1000. UTC apres 2017 : 37 s.');
 WriteLn('Codes retour : 0 succes, 1 erreur de lecture/format, 2 syntaxe.');
End;

{ ------------------------------------------------------------------ }
{ Temps                                                              }
{ ------------------------------------------------------------------ }

Function JulianDay(Y, M, D: LongInt): LongInt;
Var A: LongInt;
Begin
 A := (14-M) Div 12;
 Y := Y+4800-A; M := M+12*A-3;
 JulianDay := D+(153*M+2) Div 5+365*Y+Y Div 4-Y Div 100+Y Div 400-32045;
End;

{ Temporaire Double : un produit entier*constante reelle serait calcule
  en simple precision par Free Pascal. }
Function DayStart(Y, M, D: LongInt): Double;
Var T: Double;
Begin
 T := JulianDay(Y,M,D)-2451545;
 DayStart := T*86400.0-43200.0;
End;

Function Calendar(Seconds: Double; LeapSecond: Boolean): String;
Var Day, L, N, Y, M, D, H, Minute, Sec, MS: LongInt;
    R, DayD: Double;
Begin
 If (Seconds < MinEpoch) Or (Seconds >= MaxEpoch) Then Begin
  Calendar := '(hors calendrier)'; Exit;
 End;
 R := Seconds+43200.0;
 Day := Trunc(R/86400.0);
 DayD := Day;
 If R < DayD*86400.0 Then Begin Dec(Day); DayD := Day End;
 R := R-DayD*86400.0;
 MS := Round(R*1000.0);
 If MS >= 86400000 Then Begin Inc(Day); MS := 0 End;
 L := Day+2451545+68569;
 N := (4*L) Div 146097;
 L := L-(146097*N+3) Div 4;
 Y := (4000*(L+1)) Div 1461001;
 L := L-(1461*Y) Div 4+31;
 M := (80*L) Div 2447;
 D := L-(2447*M) Div 80;
 L := M Div 11;
 M := M+2-12*L;
 Y := 100*(N-49)+Y+L;
 H := MS Div 3600000;
 Minute := (MS Div 60000) Mod 60;
 Sec := (MS Div 1000) Mod 60;
 If LeapSecond Then Sec := 60;
 Calendar := Zero(Y,4)+'-'+Zero(M,2)+'-'+Zero(D,2)+' '+
  Zero(H,2)+':'+Zero(Minute,2)+':'+Zero(Sec,2)+'.'+Zero(MS Mod 1000,3);
End;

Function UTCText(ET: Double): String;
Var Mean, TAI, U, Threshold, DaySeconds, Millis: Double; K, Delta: Integer;
    IsLeap: Boolean;
Begin
 If (ET < MinEpoch) Or (ET >= MaxEpoch) Then Begin
  UTCText := '(hors calendrier)'; Exit;
 End;
 Mean := 6.239996+1.99096871E-7*ET;
 TAI := ET-32.184-0.001657*Sin(Mean+0.01671*Sin(Mean));
 DaySeconds := Int(TAI/86400.0)*86400.0;
 Millis := Round((TAI-DaySeconds)*1000.0);
 TAI := DaySeconds+Millis/1000.0;
 If TAI < LeapEpoch[1]+10 Then Begin UTCText := '(UTC avant 1972)'; Exit End;
 Delta := 10; IsLeap := False; U := 0;
 For K := 2 To LeapCount Do Begin
  Threshold := LeapEpoch[K]+K+9;
  If TAI >= Threshold Then Delta := K+9
  Else If TAI >= Threshold-1 Then Begin
   IsLeap := True;
   U := LeapEpoch[K]-1+(TAI-(Threshold-1));
  End;
 End;
 If Not IsLeap Then U := TAI-Delta;
 UTCText := Calendar(U,IsLeap);
End;

{ ------------------------------------------------------------------ }
{ Kernel SCLK texte (type 1)                                         }
{ ------------------------------------------------------------------ }

Function ReadSLine: Boolean;
Var Err: Word; T: String;
Begin
 ReadSLine := False;
 While Not EOF(SF) Do Begin
  {$I-} ReadLn(SF,SLine); Err := IOResult; {$I+}
  If Err <> 0 Then Fail('Erreur de lecture du SCLK '+SclkPath+'.',1);
  T := Upper(Trim(SLine));
  If T = '\BEGINDATA' Then SInData := True
  Else If T = '\BEGINTEXT' Then SInData := False
  Else If SInData Then Begin
   SPos := 1; ReadSLine := True; Exit;
  End;
 End;
End;

{ Jeton suivant du kernel (majuscules); False en fin de fichier. }
Function NextTok(Var T: String): Boolean;
Var C: Char;
Begin
 NextTok := False;
 T := '';
 While True Do Begin
  While (SPos <= Length(SLine)) And (SLine[SPos] In [' ',#9,',']) Do Inc(SPos);
  If SPos <= Length(SLine) Then Break;
  If Not ReadSLine Then Exit;
 End;
 C := SLine[SPos];
 If (C = '(') Or (C = ')') Or (C = '=') Then Begin
  T := C; Inc(SPos);
 End Else If (C = '+') And (SPos < Length(SLine)) And (SLine[SPos+1] = '=') Then Begin
  T := '+='; Inc(SPos,2);
 End Else If C = '''' Then Begin
  T := C; Inc(SPos);
  While (SPos <= Length(SLine)) And (SLine[SPos] <> '''') Do Begin
   T := T+SLine[SPos]; Inc(SPos);
  End;
  Inc(SPos);
  T := T+'''';
 End Else Begin
  While (SPos <= Length(SLine)) And Not (SLine[SPos] In [' ',#9,',','(',')','=']) Do Begin
   If Length(T) = 255 Then Fail('Jeton SCLK trop long.',1);
   T := T+SLine[SPos]; Inc(SPos);
  End;
 End;
 T := Upper(T);
 NextTok := True;
End;

Function ParseNumber(S: String; Var V: Double): Boolean;
Var Code, K: Integer;
Begin
 ParseNumber := False;
 If (S = '') Or (Length(S) > 40) Then Exit;
 For K := 1 To Length(S) Do
  If (S[K] = 'D') Or (S[K] = 'd') Then S[K] := 'E';
 Val(S,V,Code);
 If Code <> 0 Then Exit;
 If (V < -1.0E300) Or (V > 1.0E300) Then Exit;
 ParseNumber := True;
End;

{ Genre de cle : 1 modules, 2 offsets, 3 nombre de champs, 4 delimiteur,
  5 debuts de partition, 6 fins de partition, 7 coefficients, 8 type de donnees. }
Function KeyKind(Name: String; Var ID: LongInt): Integer;
Var Prefix: String; K, Code: Integer; V: LongInt; Rest: String;
Const
 Prefixes: Array[1..8] Of String[24] =
  ('SCLK01_MODULI_','SCLK01_OFFSETS_','SCLK01_N_FIELDS_','SCLK01_OUTPUT_DELIM_',
   'SCLK_PARTITION_START_','SCLK_PARTITION_END_','SCLK01_COEFFICIENTS_',
   'SCLK_DATA_TYPE_');
Begin
 KeyKind := 0;
 For K := 1 To 8 Do Begin
  Prefix := Prefixes[K];
  If Copy(Name,1,Length(Prefix)) = Prefix Then Begin
   Rest := Copy(Name,Length(Prefix)+1,255);
   Val(Rest,V,Code);
   If (Code = 0) And (V > 0) Then Begin ID := V; KeyKind := K End;
   Exit;
  End;
 End;
End;

Procedure Store(Kind: Integer; V: Double; Fresh: Boolean);
Begin
 Case Kind Of
  1: Begin
   If Fresh Then NFields := 0;
   If NFields = 10 Then Fail('SCLK : plus de 10 champs.',1);
   Inc(NFields); Moduli[NFields] := V;
  End;
  2: Begin
   If Fresh Then NOffsets := 0;
   If NOffsets = 10 Then Fail('SCLK : plus de 10 offsets.',1);
   Inc(NOffsets); Offsets[NOffsets] := V;
  End;
  3: ;
  4: ClockDelim := Trunc(V);
  5: Begin
   If Fresh Then NParts := 0;
   If NParts = MaxParts Then Fail('SCLK : plus de '+Num(MaxParts)+' partitions.',1);
   Inc(NParts); PStart[NParts] := V;
  End;
  6: ;
  7: Begin
   If Fresh Then NCoef := 0;
   If NCoef = MaxCoef Then Fail('SCLK : trop de coefficients (maximum '+Num(MaxCoef Div 3)+' triplets).',1);
   Inc(NCoef); Coef^[NCoef] := V;
  End;
  8: If V <> 1 Then Fail('SCLK : seul le type 1 est pris en charge.',1);
 End;
End;

Var
 EndCount: Integer;

Procedure LoadSCLK(Path: String);
Var Err: Word; Name, Op, T: String; ID: LongInt; Kind, K: Integer; V: Double;
    Fresh: Boolean; Count: Integer;
Begin
 SclkPath := Path;
 Assign(SF,Path);
 FileMode := 0;
 {$I-} Reset(SF); Err := IOResult; {$I+}
 If Err <> 0 Then Fail('Impossible d''ouvrir le SCLK : '+Path,1);
 SLine := ''; SPos := 1; SInData := False;
 HaveClock := False; NFields := 0; NOffsets := 0; NParts := 0; NCoef := 0;
 EndCount := 0; ClockDelim := 1;
 While NextTok(Name) Do Begin
  If Not NextTok(Op) Then Fail('SCLK : affectation incomplete apres '+Name+'.',1);
  If (Op <> '=') And (Op <> '+=') Then Fail('SCLK : "=" attendu apres '+Name+'.',1);
  Fresh := Op = '=';
  Kind := KeyKind(Name,ID);
  If Kind <> 0 Then Begin
   If Not HaveClock Then Begin HaveClock := True; ClockID := ID End
   Else If ID <> ClockID Then Kind := 0;
  End;
  If Not NextTok(T) Then Fail('SCLK : valeur manquante pour '+Name+'.',1);
  Count := 0;
  If T = '(' Then Begin
   If Not NextTok(T) Then Fail('SCLK : parenthese non fermee pour '+Name+'.',1);
   While T <> ')' Do Begin
    If Kind <> 0 Then Begin
     If Not ParseNumber(T,V) Then Fail('SCLK : nombre invalide pour '+Name+' : '+T,1);
     If Kind = 6 Then Begin
      If (Fresh And (Count = 0)) Then EndCount := 0;
      If EndCount = MaxParts Then Fail('SCLK : trop de partitions.',1);
      Inc(EndCount); PEnd[EndCount] := V;
     End Else Store(Kind,V,Fresh And (Count = 0));
     Inc(Count);
    End;
    If Not NextTok(T) Then Fail('SCLK : parenthese non fermee pour '+Name+'.',1);
   End;
  End Else If Kind <> 0 Then Begin
   If Not ParseNumber(T,V) Then Fail('SCLK : nombre invalide pour '+Name+' : '+T,1);
   If Kind = 6 Then Begin
    If Fresh Then EndCount := 0;
    Inc(EndCount); PEnd[EndCount] := V;
   End Else Store(Kind,V,Fresh);
  End;
 End;
 {$I-} Close(SF); Err := IOResult; {$I+}
 If Not HaveClock Then Fail('Aucune donnee SCLK de type 1 dans '+Path+'.',1);
 If (NFields < 1) Or (NParts < 1) Or (EndCount <> NParts) Or (NCoef < 3) Or
    (NCoef Mod 3 <> 0) Then
  Fail('SCLK incomplet : champs, modules, partitions ou coefficients manquants.',1);
 If NOffsets < NFields Then
  For K := NOffsets+1 To NFields Do Offsets[K] := 0;
 For K := 1 To NFields Do
  If Moduli[K] < 1 Then Fail('SCLK : module invalide.',1);
 K := 4;
 While K <= NCoef Do Begin
  If Coef^[K] <= Coef^[K-3] Then Fail('SCLK : coefficients non croissants.',1);
  Inc(K,3);
 End;
End;

{ Ticks par unite du premier champ. }
Function TicksPerCount: Double;
Var K: Integer; P: Double;
Begin
 P := 1;
 For K := 2 To NFields Do P := P*Moduli[K];
 TicksPerCount := P;
End;

Function TicksToET(T: Double): Double;
Var K, Pick: Integer; TDT, E, Mean: Double;
Begin
 Pick := 1;
 K := 4;
 While K <= NCoef-2 Do Begin
  If Coef^[K] <= T Then Pick := (K+2) Div 3;
  Inc(K,3);
 End;
 TDT := Coef^[3*Pick-1]+(T-Coef^[3*Pick-2])/TicksPerCount*Coef^[3*Pick];
 E := TDT;
 For K := 1 To 6 Do Begin
  Mean := 6.239996+1.99096871E-7*E;
  E := TDT+0.001657*Sin(Mean+0.01671*Sin(Mean));
 End;
 TicksToET := E;
End;

Function Delimiter: String;
Begin
 Case ClockDelim Of
  2: Delimiter := ':';
  3: Delimiter := '-';
  4: Delimiter := ',';
  5: Delimiter := ' ';
 Else Delimiter := '.';
 End;
End;

Function TicksText(T: Double): String;
Var S: String;
Begin
 If (T = Int(T)) And (Abs(T) < 1.0E15) Then Str(T:0:0,S) Else Str(T:0:3,S);
 TicksText := S;
End;

Function ClockString(T: Double): String;
Var K, P, Part: Integer; Cum, Len, InPart, Remain, Q: Double; S, F: String;
    Fields: Array[1..10] Of Double;
Begin
 If T < 0 Then Begin ClockString := TicksText(T); Exit End;
 Cum := 0; Part := 1;
 For P := 1 To NParts Do Begin
  Len := PEnd[P]-PStart[P];
  If T >= Cum Then Part := P;
  If P < NParts Then Cum := Cum+Len;
 End;
 { Debut continu de la partition retenue. }
 Cum := 0;
 For P := 1 To Part-1 Do Cum := Cum+(PEnd[P]-PStart[P]);
 InPart := PStart[Part]+(T-Cum);
 Q := Int(InPart+0.5);
 For K := NFields DownTo 2 Do Begin
  Remain := Q-Int(Q/Moduli[K])*Moduli[K];
  Fields[K] := Remain;
  Q := Int(Q/Moduli[K]);
 End;
 Fields[1] := Q;
 S := Num(Part)+'/';
 For K := 1 To NFields Do Begin
  Str(Fields[K]+Offsets[K]:0:0,F);
  If K > 1 Then S := S+Delimiter;
  S := S+F;
 End;
 ClockString := S;
End;

{ Instrument -> engin; vrai si l'horloge SCLK couvre cet engin. }
Function HasClockFor(Inst: LongInt): Boolean;
Begin
 HasClockFor := HaveClock And (-(Inst Div 1000) = ClockID);
End;

Function TimeFmt(Inst: LongInt; T: Double): String;
Var S: String;
Begin
 If (TimeMode = 0) Or Not HasClockFor(Inst) Then TimeFmt := TicksText(T)
 Else If TimeMode = 1 Then TimeFmt := ClockString(T)
 Else If TimeMode = 2 Then TimeFmt := UTCText(TicksToET(T))
 Else If TimeMode = 3 Then TimeFmt := Calendar(TicksToET(T),False)
 Else Begin
  Str(TicksToET(T):0:6,S);
  TimeFmt := S;
 End;
End;

Function ScaleLabel(Inst: LongInt): String;
Begin
 If (TimeMode = 0) Or Not HasClockFor(Inst) Then ScaleLabel := 'ticks SCLK'
 Else If TimeMode = 1 Then ScaleLabel := 'SCLK'
 Else If TimeMode = 2 Then ScaleLabel := 'UTC'
 Else If TimeMode = 3 Then ScaleLabel := 'ET/TDB'
 Else ScaleLabel := 'ET/TDB, secondes depuis J2000';
End;

{ ------------------------------------------------------------------ }
{ Lecture DAF/CK                                                     }
{ ------------------------------------------------------------------ }

Function TextAt(Offset, Count: Integer): String;
Var K: Integer; S: String;
Begin
 S := '';
 For K := Offset To Offset+Count-1 Do
  If (Buffer[K] >= 32) And (Buffer[K] < 127) Then S := S+Chr(Buffer[K])
  Else S := S+' ';
 TextAt := S;
End;

Function IntAt(Offset: Integer): LongInt;
Var B: Array[0..3] Of Byte; K: Integer; V: LongInt;
Begin
 For K := 0 To 3 Do
  If BigEndian Then B[K] := Buffer[Offset+3-K]
  Else B[K] := Buffer[Offset+K];
 Move(B,V,4);
 IntAt := V;
End;

Function DoubleAt(Offset: Integer): Double;
Var B: TBytes8; K: Integer; V: Double;
Begin
 For K := 0 To 7 Do
  If BigEndian Then B[K] := Buffer[Offset+7-K]
  Else B[K] := Buffer[Offset+K];
 { Rejeter NaN et infini avant toute instruction flottante. }
 If ((B[7] And $7F) = $7F) And ((B[6] And $F0) = $F0) Then Begin
  DataError := True; DoubleAt := 0; Exit;
 End;
 Move(B,V,8);
 DoubleAt := V;
End;

Function ControlAt(Offset: Integer; Maximum: LongInt): LongInt;
Var V: Double; N: LongInt;
Begin
 V := DoubleAt(Offset);
 If (V < 0) Or (V > Maximum) Then Begin DataError := True; ControlAt := 0; Exit End;
 N := Trunc(V);
 If V <> N Then DataError := True;
 ControlAt := N;
End;

Function ReadRecord(Number: LongInt): Boolean;
Var Got, Err: Word;
Begin
 ReadRecord := False;
 If (Number < 1) Or (Number > FileBytes Div 1024) Then Begin
  Msg := 'Record '+Num(Number)+' hors du fichier.'; Exit;
 End;
 {$I-}
 Seek(Kernel,(Number-1)*1024);
 Err := IOResult;
 {$I+}
 If Err <> 0 Then Begin Msg := 'Positionnement impossible.'; Exit End;
 {$I-} BlockRead(Kernel,Buffer,1024,Got); Err := IOResult; {$I+}
 If (Err <> 0) Or (Got <> 1024) Then Begin Msg := 'Record '+Num(Number)+' tronque.'; Exit End;
 ReadRecord := True;
End;

Function OpenCK(FileNo: Integer): Boolean;
Var Err: Word; Path: String;
Begin
 OpenCK := False;
 CloseKernel;
 Path := ParamStr(FileArg[FileNo]);
 Assign(Kernel,Path);
 FileMode := 0;
 {$I-} Reset(Kernel,1); Err := IOResult; {$I+}
 If Err <> 0 Then Begin Msg := 'Impossible d''ouvrir '+Path+'.'; Exit End;
 Opened := True;
 {$I-} FileBytes := FileSize(Kernel); Err := IOResult; {$I+}
 If (Err <> 0) Or (FileBytes < 1024) Or (FileBytes Mod 1024 <> 0) Then Begin
  Msg := 'Taille DAF invalide ou fichier tronque : '+Path; Exit;
 End;
 If Not ReadRecord(1) Then Exit;
 If TextAt(0,8) <> 'DAF/CK  ' Then Begin
  Msg := 'Fichier non CK DAF : '+Path; Exit;
 End;
 If (TextAt(88,8) <> 'LTL-IEEE') And (TextAt(88,8) <> 'BIG-IEEE') Then Begin
  Msg := 'Format binaire non pris en charge : '+TextAt(88,8); Exit;
 End;
 BigEndian := TextAt(88,8) = 'BIG-IEEE';
 If (IntAt(8) <> 2) Or (IntAt(12) <> 6) Then Begin
  Msg := 'Dimensions des descripteurs CK invalides : '+Path; Exit;
 End;
 CurFile := FileNo;
 OpenCK := True;
End;

Procedure ReadCK(FileNo: Integer);
Var
 Fwd, Bwd, Free, Current, Previous, Visited, Next, Prev: LongInt;
 N, J, Offset: Integer;
 A, B: Double;
 Path: String;
Begin
 Path := ParamStr(FileArg[FileNo]);
 DataError := False;
 If Not OpenCK(FileNo) Then Fail(Msg,1);
 Fwd := IntAt(76); Bwd := IntAt(80); Free := IntAt(84);
 If (Free < 129) Or (Free > FileBytes Div 8+1) Then
  Fail('Adresse libre DAF hors fichier : '+Path,1);
 If (Fwd = 0) And (Bwd = 0) Then Begin CloseKernel; Exit End;
 If (Fwd < 2) Or (Bwd < 2) Then Fail('Chaine de descripteurs DAF invalide : '+Path,1);
 Current := Fwd; Previous := 0; Visited := 0;
 While Current <> 0 Do Begin
  Inc(Visited);
  If (Visited > FileBytes Div 1024) Or (Current >= FileBytes Div 1024) Then
   Fail('Chaine de records DAF incoherente : '+Path,1);
  If Not ReadRecord(Current) Then Fail(Msg,1);
  DataError := False;
  Next := ControlAt(0,FileBytes Div 1024);
  Prev := ControlAt(8,FileBytes Div 1024);
  N := ControlAt(16,25);
  If DataError Or (Prev <> Previous) Then
   Fail('Record sommaire '+Num(Current)+' invalide : '+Path,1);
  For J := 0 To N-1 Do Begin
   Offset := 24+J*40;
   A := DoubleAt(Offset); B := DoubleAt(Offset+8);
   If DataError Or (A > B) Then Fail('Intervalle de segment invalide : '+Path,1);
   If Total = MaxSegments Then Fail('Limite de '+Num(MaxSegments)+' segments depassee.',1);
   Inc(Total);
   Segs^[Total].First := A; Segs^[Total].Last := B;
   Segs^[Total].Inst := IntAt(Offset+16);
   Segs^[Total].Frame := IntAt(Offset+20);
   Segs^[Total].Kind := IntAt(Offset+24);
   Segs^[Total].AV := IntAt(Offset+28);
   Segs^[Total].NameRec := Current+1;
   Segs^[Total].Slot := J;
   Segs^[Total].FileNo := FileNo;
  End;
  Previous := Current; Current := Next;
 End;
 If Previous <> Bwd Then Fail('Dernier record sommaire incoherent : '+Path,1);
 CloseKernel;
End;

Function FrameName(ID: LongInt): String;
Var S: String;
Begin
 S := '';
 Case ID Of
  1: S := 'J2000';
  2: S := 'B1950';
  3: S := 'FK4';
  13: S := 'GALACTIC';
  16: S := 'MARSIAU';
  17: S := 'ECLIPJ2000';
  18: S := 'ECLIPB1950';
 End;
 If S = '' Then FrameName := Num(ID) Else FrameName := Num(ID)+' '+S;
End;

Function TypeName(T: LongInt): String;
Var S: String;
Begin
 S := '';
 Case T Of
  1: S := 'quaternions discrets';
  2: S := 'intervalles de pointage';
  3: S := 'interpolation lineaire';
  4: S := 'Tchebychev';
  5: S := 'quaternions sous-echantillonnes';
 End;
 If S = '' Then TypeName := Num(T) Else TypeName := Num(T)+' ('+S+')';
End;

Function SegmentName(J: Integer): String;
Begin
 SegmentName := '';
 If (CurFile <> Segs^[J].FileNo) Or Not Opened Then
  If Not OpenCK(Segs^[J].FileNo) Then Begin SegmentName := '(illisible)'; Exit End;
 If ReadRecord(Segs^[J].NameRec) Then
  SegmentName := Trim(TextAt(Segs^[J].Slot*40,40))
 Else SegmentName := '(illisible)';
End;

Procedure DumpSegments;
Var J: Integer;
Begin
 WriteLn;
 WriteLn('Detail des segments (ordre des fichiers) :');
 For J := 1 To Total Do Begin
  WriteLn;
  WriteLn('Segment ',J,' - fichier ',ParamStr(FileArg[Segs^[J].FileNo]));
  WriteLn('  Nom        : ',SegmentName(J));
  WriteLn('  Instrument : ',Segs^[J].Inst);
  WriteLn('  Repere     : ',FrameName(Segs^[J].Frame));
  WriteLn('  Type       : ',TypeName(Segs^[J].Kind));
  If Segs^[J].AV <> 0 Then WriteLn('  Vitesses angulaires : oui')
  Else WriteLn('  Vitesses angulaires : non');
  WriteLn('  Debut      : ',TimeFmt(Segs^[J].Inst,Segs^[J].First),
          '  (',ScaleLabel(Segs^[J].Inst),')');
  WriteLn('  Fin        : ',TimeFmt(Segs^[J].Inst,Segs^[J].Last));
 End;
 CloseKernel;
End;

{ Tri par instrument puis debut, fusion des intervalles qui se chevauchent. }
Procedure MergeCoverage;
Var J, K, OutCount: Integer; Item: TSeg;
Begin
 For J := 2 To Total Do Begin
  Item := Segs^[J]; K := J-1;
  While K > 0 Do Begin
   If (Segs^[K].Inst < Item.Inst) Or
      ((Segs^[K].Inst = Item.Inst) And (Segs^[K].First <= Item.First)) Then Break;
   Segs^[K+1] := Segs^[K]; Dec(K);
  End;
  Segs^[K+1] := Item;
 End;
 OutCount := 0;
 For J := 1 To Total Do Begin
  If (OutCount > 0) And (Segs^[OutCount].Inst = Segs^[J].Inst) And
     (Segs^[J].First <= Segs^[OutCount].Last) Then Begin
   If Segs^[J].Last > Segs^[OutCount].Last Then Segs^[OutCount].Last := Segs^[J].Last;
  End Else Begin
   Inc(OutCount); Segs^[OutCount] := Segs^[J];
  End;
 End;
 Total := OutCount;
End;

Procedure ShowSummary(Segments: Integer);
Var J: Integer; First: Boolean;
Begin
 WriteLn;
 Write('Resume pour :');
 For J := 1 To FileCount Do Write(' ',ParamStr(FileArg[J]));
 WriteLn;
 WriteLn(Segments,' segment(s), ',Total,' intervalle(s) de couverture.');
 If HaveClock Then WriteLn('SCLK : ',SclkPath,' (engin ',-ClockID,')');
 If Total = 0 Then WriteLn('Aucun objet : fichiers CK vides.');
 If Tabular Then
  WriteLn('Instrument        Debut                          Fin                            Echelle');
 For J := 1 To Total Do Begin
  First := (J = 1) Or (Segs^[J].Inst <> Segs^[J-1].Inst);
  If Tabular Then
   WriteLn(PadRight(Num(Segs^[J].Inst),16),'  ',
           PadRight(TimeFmt(Segs^[J].Inst,Segs^[J].First),29),'  ',
           PadRight(TimeFmt(Segs^[J].Inst,Segs^[J].Last),29),'  ',
           ScaleLabel(Segs^[J].Inst))
  Else Begin
   If First Then WriteLn('Objet : ',Segs^[J].Inst,'  [',ScaleLabel(Segs^[J].Inst),']');
   WriteLn('  Debut : ',TimeFmt(Segs^[J].Inst,Segs^[J].First));
   WriteLn('  Fin   : ',TimeFmt(Segs^[J].Inst,Segs^[J].Last));
  End;
 End;
End;

Function OptionValue: String;
Begin
 Inc(ArgIndex);
 If ArgIndex > ParamCount Then SyntaxError('Valeur manquante pour '+Arg+'.');
 OptionValue := ParamStr(ArgIndex);
End;

Var
 Segments: Integer;

Begin
 Opened := False; Tabular := False; Dump := False;
 TimeMode := -1; SclkPath := ''; HaveClock := False; FileCount := 0; Total := 0;
 CurFile := 0;
 For I := 1 To LeapCount Do
  LeapEpoch[I] := DayStart(LeapYears[I],LeapMonths[I],1);
 If ParamCount = 0 Then Begin Help; Halt(0) End;
 For ArgIndex := 1 To ParamCount Do Begin
  Arg := Upper(ParamStr(ArgIndex));
  If (Arg = '-H') Or (Arg = '-HELP') Or (Arg = '--HELP') Or (Arg = '/?') Then Begin
   Help; Halt(0);
  End;
 End;
 ArgIndex := 1;
 While ArgIndex <= ParamCount Do Begin
  Arg := Upper(ParamStr(ArgIndex));
  If Arg = '-SCLK' Then SclkPath := OptionValue
  Else If Arg = '-TICKS' Then TimeMode := 0
  Else If Arg = '-SCLKSTR' Then TimeMode := 1
  Else If Arg = '-UTC' Then TimeMode := 2
  Else If Arg = '-ET' Then TimeMode := 3
  Else If Arg = '-ETSEC' Then TimeMode := 4
  Else If Arg = '-T' Then Tabular := True
  Else If Arg = '-DUMP' Then Dump := True
  Else If Arg = '' Then SyntaxError('Nom de fichier vide.')
  Else If Arg[1] = '-' Then SyntaxError('Option inconnue : '+ParamStr(ArgIndex))
  Else Begin
   If FileCount = MaxFiles Then SyntaxError('Trop de fichiers (maximum '+Num(MaxFiles)+').');
   Inc(FileCount); FileArg[FileCount] := ArgIndex;
  End;
  Inc(ArgIndex);
 End;
 If FileCount = 0 Then SyntaxError('Fichier CK requis.');
 If (SclkPath = '') And (TimeMode > 0) Then
  SyntaxError('-sclkstr, -utc, -et et -etsec exigent -sclk.');
 If TimeMode = -1 Then Begin
  If SclkPath = '' Then TimeMode := 0 Else TimeMode := 2;
 End;

 New(Segs);
 New(Coef);
 If SclkPath <> '' Then LoadSCLK(SclkPath);
 For I := 1 To FileCount Do ReadCK(I);
 Segments := Total;
 If Dump Then DumpSegments;
 MergeCoverage;
 ShowSummary(Segments);
 CloseKernel;
End.
