{ @author: Sylvain Maltais (support@gladir.com)
 @created: 2026
 @website(https://www.gladir.com/space-0)
 @abstract(Target: Turbo Pascal 7, Free Pascal 3.2) }

Program SPACIT;

{$A-}
{$E+,N+}

{ Examen interactif de fichiers DAF binaires SPICE (SPK, CK, PCK),
  sous-ensemble autonome de SPACIT NAIF : aucune unite, aucune inclusion.
  Fonctions : informations du fichier, sommaire des segments, detail d'un
  segment (descripteur, nom, parametres de quelques types), commentaires.
  Pas de conversion (transfert), de concatenation, de reordonnancement ni
  d'edition de commentaires : la lecture du fichier est en lecture seule.
  Fichiers DAF LTL-IEEE ou BIG-IEEE de 1000 segments au plus, < 2 Gio.
  UTC : table NAIF0012 integree (1972-2017), 37 s ensuite. Les temps d'un
  CK sont des ticks SCLK, affiches bruts.
  Sources : naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/ug/spacit.html
            naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/daf.html
            naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/spk.html
            naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/ck.html
  Compilation : fpc -Mtp SPACIT.PAS }

Const
 MaxSegments = 1000;
 PageLines = 20;
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
 TSegment = Record
  First, Last: Double;
  Ints: Array[1..6] Of LongInt;
  NameRec: LongInt;
  Slot: Integer;
 End;

Var
 Kernel: File;
 Buffer: TBlock;
 Segs: Array[1..MaxSegments] Of TSegment;
 LeapEpoch: Array[1..LeapCount] Of Double;
 Opened, BigEndian, DataError, Interactive, NumericOnly, Quit: Boolean;
 FileBytes, FwdRec, BwdRec, FreeWord: LongInt;
 SegTotal, Kind, NI, TimeMode, Lines: Integer;
 FilePath, InternalName, IDWord, Format, Msg: String;
 Arg, Choice, Raw: String;
 ArgIndex, I, DetailSeg: Integer;
 DoInfo, DoSum, DoComm, Batch: Boolean;

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

Function Pad(S: String; Width: Integer): String;
Begin
 While Length(S) < Width Do S := S+' ';
 Pad := S;
End;

Function PadLeft(S: String; Width: Integer): String;
Begin
 While Length(S) < Width Do S := ' '+S;
 PadLeft := S;
End;

{ ------------------------------------------------------------------ }
{ Sortie paginee et saisie                                           }
{ ------------------------------------------------------------------ }

Function Ask(Prompt: String): String;
Var S: String;
Begin
 Write(Prompt);
 If EOF(Input) Then S := 'Q' Else ReadLn(S);
 Ask := Trim(S);
End;

Procedure Say(S: String);
Var Dummy: String;
Begin
 WriteLn(S);
 Inc(Lines);
 If Interactive And (Lines Mod PageLines = 0) Then Begin
  Dummy := Ask('-- Suite (Entree) --');
 End;
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
  Calendar := 'hors calendrier'; Exit;
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
  UTCText := 'hors calendrier'; Exit;
 End;
 Mean := 6.239996+1.99096871E-7*ET;
 TAI := ET-32.184-0.001657*Sin(Mean+0.01671*Sin(Mean));
 DaySeconds := Int(TAI/86400.0)*86400.0;
 Millis := Round((TAI-DaySeconds)*1000.0);
 TAI := DaySeconds+Millis/1000.0;
 If TAI < LeapEpoch[1]+10 Then Begin UTCText := 'UTC avant 1972'; Exit End;
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

{ TimeMode : 0 calendrier ET/TDB, 1 UTC, 2 secondes ET depuis J2000. }
Function TimeText(V: Double): String;
Var S: String;
Begin
 If Kind = 2 Then Begin Str(V:0:3,S); TimeText := S; Exit End;
 If TimeMode = 2 Then Begin Str(V:0:6,S); TimeText := S End
 Else If TimeMode = 1 Then TimeText := UTCText(V)
 Else TimeText := Calendar(V,False);
End;

Function TimeLabel: String;
Begin
 If TimeMode = 2 Then TimeLabel := 'ET/TDB, secondes depuis J2000'
 Else If TimeMode = 1 Then TimeLabel := 'UTC (NAIF0012 integre, 37 s apres 2017)'
 Else TimeLabel := 'ET/TDB, calendrier';
End;

{ ------------------------------------------------------------------ }
{ Lecture DAF                                                        }
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

{ Mot DAF (8 octets) d'adresse Addr, base 1. }
Function WordAt(Addr: LongInt; Var V: Double): Boolean;
Begin
 WordAt := False;
 If (Addr < 1) Or (Addr > FileBytes Div 8) Then Exit;
 If Not ReadRecord((Addr-1) Div 128+1) Then Exit;
 DataError := False;
 V := DoubleAt(((Addr-1) Mod 128)*8);
 If DataError Then Exit;
 WordAt := True;
End;

Procedure CloseFile;
Var Err: Word;
Begin
 If Opened Then Begin
  {$I-} Close(Kernel); Err := IOResult; {$I+}
  Opened := False;
 End;
 SegTotal := 0;
End;

Function OpenFile(Path: String): Boolean;
Var
 Err: Word;
 Current, Previous, Visited, NextRec, PrevRec: LongInt;
 N, J, K, Offset, SS: Integer;
 A, B: Double;
Begin
 OpenFile := False;
 CloseFile;
 DataError := False;
 Assign(Kernel,Path);
 FileMode := 0;
 {$I-} Reset(Kernel,1); Err := IOResult; {$I+}
 If Err <> 0 Then Begin Msg := 'Impossible d''ouvrir '+Path+'.'; Exit End;
 Opened := True;
 {$I-} FileBytes := FileSize(Kernel); Err := IOResult; {$I+}
 If (Err <> 0) Or (FileBytes < 1024) Or (FileBytes Mod 1024 <> 0) Then Begin
  Msg := 'Taille DAF invalide ou fichier tronque.'; CloseFile; Exit;
 End;
 If Not ReadRecord(1) Then Begin CloseFile; Exit End;
 IDWord := TextAt(0,8);
 If IDWord = 'DAF/SPK ' Then Kind := 1
 Else If IDWord = 'DAF/CK  ' Then Kind := 2
 Else If IDWord = 'DAF/PCK ' Then Kind := 3
 Else Begin
  Msg := 'Fichier non pris en charge (mot d''identification "'+Trim(IDWord)+'").';
  CloseFile; Exit;
 End;
 Format := TextAt(88,8);
 If (Format <> 'LTL-IEEE') And (Format <> 'BIG-IEEE') Then Begin
  Msg := 'Format binaire non pris en charge : '+Format; CloseFile; Exit;
 End;
 BigEndian := Format = 'BIG-IEEE';
 NI := IntAt(12);
 If (IntAt(8) <> 2) Or ((Kind <= 2) And (NI <> 6)) Or ((Kind = 3) And (NI <> 5)) Then Begin
  Msg := 'Dimensions ND/NI invalides pour '+Trim(IDWord)+'.'; CloseFile; Exit;
 End;
 InternalName := Trim(TextAt(16,60));
 FwdRec := IntAt(76); BwdRec := IntAt(80); FreeWord := IntAt(84);
 If (FreeWord < 129) Or (FreeWord > FileBytes Div 8+1) Then Begin
  Msg := 'Adresse libre DAF hors fichier.'; CloseFile; Exit;
 End;
 SegTotal := 0;
 SS := 2+(NI+1) Div 2;
 If (FwdRec = 0) And (BwdRec = 0) Then Begin FilePath := Path; OpenFile := True; Exit End;
 If (FwdRec < 2) Or (BwdRec < 2) Then Begin
  Msg := 'Chaine de descripteurs DAF invalide.'; CloseFile; Exit;
 End;
 Current := FwdRec; Previous := 0; Visited := 0;
 While Current <> 0 Do Begin
  Inc(Visited);
  If (Visited > FileBytes Div 1024) Or (Current >= FileBytes Div 1024) Then Begin
   Msg := 'Chaine de records DAF incoherente.'; CloseFile; Exit;
  End;
  If Not ReadRecord(Current) Then Begin CloseFile; Exit End;
  DataError := False;
  NextRec := ControlAt(0,FileBytes Div 1024);
  PrevRec := ControlAt(8,FileBytes Div 1024);
  N := ControlAt(16,(1024-24) Div (SS*8));
  If DataError Or (PrevRec <> Previous) Then Begin
   Msg := 'Record sommaire '+Num(Current)+' invalide.'; CloseFile; Exit;
  End;
  For J := 0 To N-1 Do Begin
   Offset := 24+J*SS*8;
   A := DoubleAt(Offset); B := DoubleAt(Offset+8);
   If DataError Or (A > B) Then Begin
    Msg := 'Descripteur de segment invalide.'; CloseFile; Exit;
   End;
   If SegTotal = MaxSegments Then Begin
    Msg := 'Limite de '+Num(MaxSegments)+' segments depassee.'; CloseFile; Exit;
   End;
   Inc(SegTotal);
   Segs[SegTotal].First := A; Segs[SegTotal].Last := B;
   For K := 1 To NI Do Segs[SegTotal].Ints[K] := IntAt(Offset+16+4*(K-1));
   For K := NI+1 To 6 Do Segs[SegTotal].Ints[K] := 0;
   Segs[SegTotal].NameRec := Current+1;
   Segs[SegTotal].Slot := J;
  End;
  Previous := Current; Current := NextRec;
 End;
 If Previous <> BwdRec Then Begin
  Msg := 'Dernier record sommaire incoherent.'; CloseFile; Exit;
 End;
 FilePath := Path;
 OpenFile := True;
End;

{ ------------------------------------------------------------------ }
{ Descriptions                                                       }
{ ------------------------------------------------------------------ }

Function BodyName(ID: LongInt): String;
Var S: String;
Begin
 S := '';
 If Not NumericOnly Then Case ID Of
  0: S := 'SOLAR SYSTEM BARYCENTER';
  1: S := 'MERCURY BARYCENTER';
  2: S := 'VENUS BARYCENTER';
  3: S := 'EARTH BARYCENTER';
  4: S := 'MARS BARYCENTER';
  5: S := 'JUPITER BARYCENTER';
  6: S := 'SATURN BARYCENTER';
  7: S := 'URANUS BARYCENTER';
  8: S := 'NEPTUNE BARYCENTER';
  9: S := 'PLUTO BARYCENTER';
  10: S := 'SUN';
  199: S := 'MERCURY';
  299: S := 'VENUS';
  301: S := 'MOON';
  399: S := 'EARTH';
  499: S := 'MARS';
  599: S := 'JUPITER';
  699: S := 'SATURN';
  799: S := 'URANUS';
  899: S := 'NEPTUNE';
  999: S := 'PLUTO';
  3000: S := 'ITRF93';
 End;
 If S = '' Then BodyName := Num(ID) Else BodyName := Num(ID)+' '+S;
End;

Function FrameName(ID: LongInt): String;
Var S: String;
Begin
 S := '';
 If Not NumericOnly Then Case ID Of
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
 If Kind = 1 Then Case T Of
  1: S := 'differences modifiees (MDA)';
  2: S := 'Tchebychev, position';
  3: S := 'Tchebychev, position et vitesse';
  5: S := 'etats discrets, deux corps';
  8: S := 'Lagrange, pas egaux';
  9: S := 'Lagrange, pas inegaux';
  10: S := 'elements Space Command';
  12: S := 'Hermite, pas egaux';
  13: S := 'Hermite, pas inegaux';
  14: S := 'Tchebychev, pas inegaux';
  15: S := 'conique precessante';
  17: S := 'equinoxial';
  18: S := 'ESOC/DDID';
  19: S := 'ESOC/DDID par morceaux';
  20: S := 'Tchebychev, vitesse';
  21: S := 'differences modifiees etendues';
 End
 Else If Kind = 2 Then Case T Of
  1: S := 'quaternions discrets';
  2: S := 'intervalles de pointage';
  3: S := 'interpolation lineaire';
  4: S := 'Tchebychev';
  5: S := 'quaternions sous-echantillonnes';
 End
 Else Case T Of
  2: S := 'Tchebychev, angles d''Euler';
  3: S := 'Tchebychev, angles et vitesse';
  20: S := 'Tchebychev, vitesse';
 End;
 If S = '' Then TypeName := Num(T) Else TypeName := Num(T)+' ('+S+')';
End;

Function KindName: String;
Begin
 If Kind = 1 Then KindName := 'SPK (ephemerides)'
 Else If Kind = 2 Then KindName := 'CK (orientation d''engin)'
 Else KindName := 'PCK (constantes planetaires)';
End;

{ Position des champs : SPK corps,centre,repere,type ; CK instrument,repere,type,AV ;
  PCK corps/repere,reference,type. }
Function SegType(Var S: TSegment): LongInt;
Begin
 If Kind = 1 Then SegType := S.Ints[4]
 Else SegType := S.Ints[3];
End;

Function SegBegin(Var S: TSegment): LongInt;
Begin
 SegBegin := S.Ints[NI-1];
End;

Function SegEnd(Var S: TSegment): LongInt;
Begin
 SegEnd := S.Ints[NI];
End;

{ ------------------------------------------------------------------ }
{ Commandes                                                          }
{ ------------------------------------------------------------------ }

Function Ready: Boolean;
Begin
 Ready := Opened;
 If Not Opened Then WriteLn('Aucun fichier ouvert. Utiliser O.');
End;

{ Dernier record de la zone de commentaires (records 2 a CommentEnd). }
Function CommentEnd: LongInt;
Begin
 If FwdRec = 0 Then CommentEnd := (FreeWord-1) Div 128
 Else CommentEnd := FwdRec-1;
End;

Procedure ShowInfo;
Var J: Integer; Lo, Hi: Double; Comment: LongInt;
Begin
 If Not Ready Then Exit;
 Lines := 0;
 Say('');
 Say('Fichier             : '+FilePath);
 Say('Taille              : '+Num(FileBytes)+' octets ('+Num(FileBytes Div 1024)+' records)');
 Say('Identification      : '+Trim(IDWord)+' - '+KindName);
 Say('Format binaire      : '+Format);
 Say('ND / NI             : 2 / '+Num(NI));
 Say('Nom interne         : '+InternalName);
 Say('Premier sommaire    : record '+Num(FwdRec));
 Say('Dernier sommaire    : record '+Num(BwdRec));
 Say('Premiere adresse libre : '+Num(FreeWord));
 Comment := CommentEnd-1;
 If Comment < 0 Then Comment := 0;
 Say('Records commentaires   : '+Num(Comment));
 Say('Segments            : '+Num(SegTotal));
 If SegTotal > 0 Then Begin
  Lo := Segs[1].First; Hi := Segs[1].Last;
  For J := 2 To SegTotal Do Begin
   If Segs[J].First < Lo Then Lo := Segs[J].First;
   If Segs[J].Last > Hi Then Hi := Segs[J].Last;
  End;
  If Kind = 2 Then Say('Echelle de temps    : ticks SCLK')
  Else Say('Echelle de temps    : '+TimeLabel);
  Say('Couverture          : '+TimeText(Lo));
  Say('                   a  '+TimeText(Hi));
 End;
End;

Procedure ShowSummary;
Var J: Integer; Head: String;
Begin
 If Not Ready Then Exit;
 Lines := 0;
 Say('');
 Say(Num(SegTotal)+' segment(s) dans '+FilePath);
 If SegTotal = 0 Then Exit;
 If Kind = 2 Then Say('Temps : ticks SCLK') Else Say('Temps : '+TimeLabel);
 Say('');
 If Kind = 1 Then Head := 'No  Objet / Centre / Repere / Type'
 Else If Kind = 2 Then Head := 'No  Instrument / Repere / Type / AV'
 Else Head := 'No  Corps ou repere / Reference / Type';
 Say(Head);
 For J := 1 To SegTotal Do Begin
  If Kind = 1 Then
   Say(PadLeft(Num(J),3)+' '+BodyName(Segs[J].Ints[1])+' / '+
       BodyName(Segs[J].Ints[2])+' / '+FrameName(Segs[J].Ints[3])+' / type '+
       Num(Segs[J].Ints[4]))
  Else If Kind = 2 Then
   Say(PadLeft(Num(J),3)+' '+Num(Segs[J].Ints[1])+' / '+FrameName(Segs[J].Ints[2])+
       ' / type '+Num(Segs[J].Ints[3])+' / AV '+Num(Segs[J].Ints[4]))
  Else
   Say(PadLeft(Num(J),3)+' '+BodyName(Segs[J].Ints[1])+' / '+FrameName(Segs[J].Ints[2])+
       ' / type '+Num(Segs[J].Ints[3]));
  Say('      '+TimeText(Segs[J].First)+'  ->  '+TimeText(Segs[J].Last));
 End;
End;

Procedure ShowDetail(N: Integer);
Var S: TSegment; B, E, Count: LongInt; V, V2, V3, V4: Double; Deg: Double;
    Line: String;
    ValidAddr: Boolean; T: LongInt; Ok: Boolean;
Begin
 If Not Ready Then Exit;
 If (N < 1) Or (N > SegTotal) Then Begin
  WriteLn('Numero de segment invalide (1 a ',SegTotal,').');
  Exit;
 End;
 S := Segs[N];
 Lines := 0;
 Say('');
 Say('Segment             : '+Num(N));
 If ReadRecord(S.NameRec) Then
  Say('Nom                 : '+Trim(TextAt(S.Slot*(2+(NI+1) Div 2)*8,(2+(NI+1) Div 2)*8)))
 Else Say('Nom                 : illisible ('+Msg+')');
 If Kind = 1 Then Begin
  Say('Objet               : '+BodyName(S.Ints[1]));
  Say('Centre              : '+BodyName(S.Ints[2]));
  Say('Repere              : '+FrameName(S.Ints[3]));
 End Else If Kind = 2 Then Begin
  Say('Instrument          : '+Num(S.Ints[1]));
  Say('Repere              : '+FrameName(S.Ints[2]));
  If S.Ints[4] <> 0 Then Say('Vitesses angulaires : oui') Else Say('Vitesses angulaires : non');
 End Else Begin
  Say('Corps ou repere     : '+BodyName(S.Ints[1]));
  Say('Reference           : '+FrameName(S.Ints[2]));
 End;
 T := SegType(S);
 Say('Type de donnees     : '+TypeName(T));
 If Kind = 2 Then Say('Debut (ticks SCLK)  : '+TimeText(S.First))
 Else Say('Debut               : '+TimeText(S.First));
 If Kind = 2 Then Say('Fin   (ticks SCLK)  : '+TimeText(S.Last))
 Else Say('Fin                 : '+TimeText(S.Last));
 B := SegBegin(S); E := SegEnd(S);
 ValidAddr := (B >= 129) And (E >= B) And (E < FreeWord) And (E <= FileBytes Div 8);
 If Not ValidAddr Then Begin
  Say('Adresses            : '+Num(B)+' a '+Num(E)+' (invalides)');
  Exit;
 End;
 Count := E-B+1;
 Say('Adresses de donnees : '+Num(B)+' a '+Num(E)+' ('+Num(Count)+' mots)');
 If Kind = 1 Then Begin
  Ok := False;
  If ((T = 2) Or (T = 3)) And (Count >= 4) Then Begin
   If WordAt(E-3,V) And WordAt(E-2,V2) And WordAt(E-1,V3) And WordAt(E,V4) Then Begin
    Say('Debut du 1er enreg. : '+TimeText(V));
    Str(V2:0:6,Line);
    Say('Duree d''un enreg.   : '+Line+' s');
    Say('Taille enregistrement : '+Num(Round(V3))+' mots');
    Say('Nombre d''enreg.     : '+Num(Round(V4)));
    If T = 2 Then Deg := (V3-2)/3-1 Else Deg := (V3-2)/6-1;
    Say('Degre Tchebychev    : '+Num(Round(Deg)));
    Ok := True;
   End;
  End;
  If ((T = 9) Or (T = 13)) And (Count >= 2) Then Begin
   If WordAt(E-1,V) And WordAt(E,V2) Then Begin
    Say('Degre du polynome   : '+Num(Round(V)));
    Say('Nombre d''etats      : '+Num(Round(V2)));
    Ok := True;
   End;
  End;
  If ((T = 1) Or (T = 21)) And (Count >= 1) Then Begin
   If WordAt(E,V) Then Begin
    Say('Nombre d''enreg.     : '+Num(Round(V)));
    Ok := True;
   End;
  End;
  If Not Ok Then Say('Parametres du type : non decodes par cet outil');
 End;
End;

Procedure ShowComments;
Var Rec, Last: LongInt; K: Integer; Line: String; Done: Boolean; Any: Boolean;
Begin
 If Not Ready Then Exit;
 Lines := 0;
 Say('');
 Last := CommentEnd;
 Line := ''; Done := False; Any := False;
 Rec := 2;
 While (Rec <= Last) And Not Done Do Begin
  If Not ReadRecord(Rec) Then Begin Say('Erreur : '+Msg); Exit End;
  For K := 0 To 999 Do Begin
   If Buffer[K] = 4 Then Begin Done := True; Break End;
   If Buffer[K] = 0 Then Begin Say(Line); Line := ''; Any := True End
   Else If Length(Line) < 250 Then Begin
    If (Buffer[K] >= 32) And (Buffer[K] < 127) Then Line := Line+Chr(Buffer[K])
    Else Line := Line+' ';
   End;
  End;
  Inc(Rec);
 End;
 If Line <> '' Then Begin Say(Line); Any := True End;
 If Not Any Then Say('(aucun commentaire)');
End;

Procedure OpenCommand(Path: String);
Begin
 If Path = '' Then Path := Ask('Nom du fichier DAF (SPK, CK, PCK) : ');
 If Path = '' Then Exit;
 If OpenFile(Path) Then
  WriteLn('Ouvert : ',Path,' - ',Trim(IDWord),', ',SegTotal,' segment(s).')
 Else WriteLn('Erreur : ',Msg);
End;

Procedure Help;
Begin
 WriteLn('SPACIT - Cette commande permet d''examiner des fichiers SPICE.');
 WriteLn;
 WriteLn('Syntaxe : SPACIT [options] [<fichier>]');
 WriteLn;
 WriteLn('Sans action (-i, -s, -c, -d), le programme demande les commandes.');
 WriteLn('Options :');
 WriteLn('  -i       Informations generales du fichier');
 WriteLn('  -s       Sommaire des segments');
 WriteLn('  -c       Zone de commentaires');
 WriteLn('  -d <n>   Detail du segment numero n');
 WriteLn('  -utc     Dates UTC (table integree NAIF0012, depuis 1972)');
 WriteLn('  -etsec   Secondes ET/TDB depuis J2000');
 WriteLn('  -n       Identifiants NAIF numeriques uniquement');
 WriteLn('  -h, /?   Aide (-help et --help acceptes)');
 WriteLn;
 WriteLn('Commandes interactives :');
 WriteLn('  O  ouvrir un fichier        I  informations');
 WriteLn('  S  sommaire des segments    D  detail d''un segment');
 WriteLn('  C  commentaires             T  echelle de temps (ET, UTC, secondes)');
 WriteLn('  N  noms / numeros           F  fermer     Q  quitter');
 WriteLn;
 WriteLn('Exemples :');
 WriteLn('  SPACIT mission.bsp');
 WriteLn('  SPACIT -s -utc mission.bsp');
 WriteLn('  SPACIT -d 2 mission.bsp');
 WriteLn;
 WriteLn('Limites : DAF SPK, CK et PCK LTL-IEEE/BIG-IEEE, 1000 segments, < 2 Gio.');
 WriteLn('Lecture seule : pas de conversion, concatenation ni edition.');
 WriteLn('Codes retour : 0 succes, 1 erreur de lecture/format, 2 syntaxe.');
End;

Procedure Menu;
Begin
 WriteLn;
 WriteLn('SPACIT - Examen de fichiers DAF (SPK, CK, PCK)');
 If Opened Then WriteLn('Fichier : ',FilePath) Else WriteLn('Fichier : (aucun)');
 WriteLn('Temps   : ',TimeLabel);
 WriteLn(' O Ouvrir    I Informations   S Sommaire   D Detail d''un segment');
 WriteLn(' C Commentaires   T Temps   N Noms/numeros   F Fermer   H Aide   Q Quitter');
End;

Procedure SyntaxError(S: String);
Begin
 WriteLn('Erreur : ',S,' Voir SPACIT -h.');
 Halt(2);
End;

Function ToNumber(S: String): Integer;
Var V: LongInt; Code: Integer;
Begin
 S := Trim(S);
 If (S = '') Or (Length(S) > 5) Then Begin ToNumber := 0; Exit End;
 Val(S,V,Code);
 If (Code <> 0) Or (V < 0) Or (V > MaxSegments) Then ToNumber := 0
 Else ToNumber := V;
End;

Begin
 Opened := False; Interactive := False; NumericOnly := False; Quit := False;
 TimeMode := 0; Kind := 1; NI := 6; SegTotal := 0; FilePath := '';
 DoInfo := False; DoSum := False; DoComm := False; DetailSeg := 0;
 For I := 1 To LeapCount Do
  LeapEpoch[I] := DayStart(LeapYears[I],LeapMonths[I],1);
 For ArgIndex := 1 To ParamCount Do Begin
  Arg := Upper(ParamStr(ArgIndex));
  If (Arg = '-H') Or (Arg = '-HELP') Or (Arg = '--HELP') Or (Arg = '/?') Then Begin
   Help; Halt(0);
  End;
 End;
 ArgIndex := 1;
 While ArgIndex <= ParamCount Do Begin
  Arg := Upper(ParamStr(ArgIndex));
  If Arg = '-I' Then DoInfo := True
  Else If Arg = '-S' Then DoSum := True
  Else If Arg = '-C' Then DoComm := True
  Else If Arg = '-D' Then Begin
   Inc(ArgIndex);
   If ArgIndex > ParamCount Then SyntaxError('Numero de segment manquant pour -d.');
   DetailSeg := ToNumber(ParamStr(ArgIndex));
   If DetailSeg = 0 Then SyntaxError('Numero de segment invalide pour -d.');
  End
  Else If Arg = '-UTC' Then TimeMode := 1
  Else If Arg = '-ETSEC' Then TimeMode := 2
  Else If Arg = '-N' Then NumericOnly := True
  Else If Arg = '' Then SyntaxError('Nom de fichier vide.')
  Else If Arg[1] = '-' Then SyntaxError('Option inconnue : '+ParamStr(ArgIndex))
  Else If FilePath <> '' Then SyntaxError('Un seul fichier est accepte.')
  Else FilePath := ParamStr(ArgIndex);
  Inc(ArgIndex);
 End;
 Batch := DoInfo Or DoSum Or DoComm Or (DetailSeg > 0);
 If Batch And (FilePath = '') Then SyntaxError('Fichier requis avec -i, -s, -c ou -d.');
 If Batch Then Begin
  If Not OpenFile(FilePath) Then Begin
   WriteLn('Erreur : ',Msg);
   Halt(1);
  End;
  If DoInfo Then ShowInfo;
  If DoSum Then ShowSummary;
  If DoComm Then ShowComments;
  If DetailSeg > 0 Then Begin
   If DetailSeg > SegTotal Then Begin
    WriteLn('Erreur : segment ',DetailSeg,' absent (',SegTotal,' segment(s)).');
    CloseFile; Halt(1);
   End;
   ShowDetail(DetailSeg);
  End;
  CloseFile;
  Halt(0);
 End;
 Interactive := True;
 If FilePath <> '' Then Begin
  Arg := FilePath;
  FilePath := '';
  OpenCommand(Arg);
 End;
 While Not Quit Do Begin
  Menu;
  Raw := Ask('Commande ? ');
  Choice := Upper(Raw);
  If Choice = '' Then
  Else If (Choice = 'Q') Or (Choice = 'QUIT') Then Quit := True
  Else If (Choice[1] = 'O') And ((Length(Choice) = 1) Or (Choice[2] = ' ')) Then
   OpenCommand(Trim(Copy(Raw,2,255)))
  Else If Choice = 'I' Then ShowInfo
  Else If Choice = 'S' Then ShowSummary
  Else If Choice = 'C' Then ShowComments
  Else If Choice = 'D' Then Begin
   If Ready Then Begin
    I := ToNumber(Ask('Numero du segment (1 a '+Num(SegTotal)+') : '));
    ShowDetail(I);
   End;
  End
  Else If Choice = 'T' Then TimeMode := (TimeMode+1) Mod 3
  Else If Choice = 'N' Then NumericOnly := Not NumericOnly
  Else If Choice = 'F' Then Begin CloseFile; FilePath := '' End
  Else If (Choice = 'H') Or (Choice = '?') Then Help
  Else WriteLn('Commande inconnue : ',Choice);
 End;
 CloseFile;
End.
