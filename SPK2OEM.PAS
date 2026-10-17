{ @author: Sylvain Maltais (support@gladir.com)
 @created: 2026
 @website(https://www.gladir.com/space-0)
 @abstract(Target: Turbo Pascal 7, Free Pascal 3.2) }

Program SPK2OEM;

{$A-}
{$E+,N+}

{ Export d'une trajectoire SPK vers un message d'ephemerides CCSDS OEM
  (version 2.0, texte KVN), sous-ensemble autonome de SPK2OEM NAIF.
  Seule l'unite Dos est utilisee (date de creation); aucune inclusion.
  Etats geometriques (sans temps-lumiere ni aberration), en km et km/s, par
  chainage des segments du fichier jusqu'au centre demande.
  Types SPK evalues : 2 et 3 (Tchebychev), 8 et 9 (Lagrange), 12 et 13
  (Hermite). Repere des segments : J2000 ou ECLIPJ2000; sortie EME2000 ou
  ICRF (memes valeurs : le repere SPICE J2000 est aligne sur l'ICRF).
  Un bloc OEM (META_START ... META_STOP et lignes d'etats) est ecrit pour
  chaque intervalle continu couvert par le fichier SPK.
  Systemes de temps : UTC (table NAIF0012 integree, 1972-2017, 37 s ensuite),
  TAI, TT et TDB. Pas d'accelerations, de covariances ni de manoeuvres.
  Sources : naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/ug/spk2oem.html
            public.ccsds.org/Pubs/502x0b2c1.pdf (CCSDS 502.0-B-2)
            naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/spk.html
            naif.jpl.nasa.gov/pub/naif/generic_kernels/lsk/naif0012.tls
  Compilation : fpc -Mtp SPK2OEM.PAS
  Limites DOS : 1500 segments, 200 blocs, fichiers < 2 Gio. }

Uses Dos;

Const
 MaxSegments = 1500;
 MaxLevel = 11;
 LeapCount = 28;
 LeapYears: Array[1..LeapCount] Of Integer =
  (1972,1972,1973,1974,1975,1976,1977,1978,1979,1980,1981,1982,
   1983,1985,1988,1990,1991,1992,1993,1994,1996,1997,1999,2006,
   2009,2012,2015,2017);
 LeapMonths: Array[1..LeapCount] Of Integer =
  (1,7,1,1,1,1,1,1,1,1,7,7,7,7,1,1,1,7,7,7,1,7,1,1,1,7,7,1);
 MinEpoch = -63082324800.0;
 MaxEpoch = 252455572800.0;
 MaxPoints = 10000000;
 { Obliquite J2000 (84381.448 secondes d'arc) en radians. }
 Obliquity = 0.40909280422232897;
 NameCount = 33;
 NameText: Array[1..NameCount] Of String[24] =
  ('SSB','SOLAR SYSTEM BARYCENTER','MERCURY BARYCENTER','VENUS BARYCENTER',
   'EMB','EARTH BARYCENTER','MARS BARYCENTER','JUPITER BARYCENTER',
   'SATURN BARYCENTER','URANUS BARYCENTER','NEPTUNE BARYCENTER',
   'PLUTO BARYCENTER','SUN','MERCURY','VENUS','MOON','EARTH','MARS',
   'JUPITER','SATURN','URANUS','NEPTUNE','PLUTO','PHOBOS','DEIMOS','IO',
   'EUROPA','GANYMEDE','CALLISTO','TITAN','CERES','VESTA','HALLEY');
 NameCode: Array[1..NameCount] Of LongInt =
  (0,0,1,2,3,3,4,5,6,7,8,9,10,199,299,301,399,499,599,699,799,899,999,
   401,402,501,502,503,504,606,2000001,2000004,1000036);

Type
 TBlock = Array[0..1023] Of Byte;
 TBytes8 = Array[0..7] Of Byte;
 TState = Array[1..6] Of Double;
 TSegment = Record
  First, Last: Double;
  Body, Center, Frame, Kind, Start, Finish: LongInt;
 End;
 TSegArray = Array[1..MaxSegments] Of TSegment;
 PSegArray = ^TSegArray;
 TKernel = Record
  F: File;
  Path: String;
  Opened, Big: Boolean;
  Bytes: LongInt;
  Segs: PSegArray;
  Count: Integer;
  Cache: TBlock;
  CacheRec: LongInt;
 End;
 PKernel = ^TKernel;
 TPath = Record
  N: Integer;
  ID: Array[0..MaxLevel] Of LongInt;
  St: Array[0..MaxLevel] Of TState;
 End;
 TNodes = Array[0..27] Of Double;
 TValues = Array[0..27,1..6] Of Double;

Var
 Kern: Array[1..2] Of PKernel;
 LeapEpoch: Array[1..LeapCount] Of Double;
 DataError: Boolean;
 ErrMsg: String;
 Frame: Integer;

Procedure Fail(S: String; Code: Integer);
Begin
 WriteLn('Erreur : ',S);
 Halt(Code);
End;

Procedure SyntaxError(S: String);
Begin
 Fail(S+' Voir SPKDIFF -h.',2);
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

Function Pad(S: String; Width: Integer): String;
Begin
 While Length(S) < Width Do S := ' '+S;
 Pad := S;
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

{ ------------------------------------------------------------------ }
{ Calendrier et echelles de temps                                    }
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

Function DaysInMonth(Y, M: Integer): Integer;
Begin
 Case M Of
  4,6,9,11: DaysInMonth := 30;
  2: If ((Y Mod 4 = 0) And (Y Mod 100 <> 0)) Or (Y Mod 400 = 0) Then
      DaysInMonth := 29
     Else DaysInMonth := 28;
 Else DaysInMonth := 31;
 End;
End;

Function Calendar(Seconds: Double; LeapSecond: Boolean): String;
Var Day, L, N, Y, M, D, H, Minute, Sec, MS: LongInt;
    R, DayD: Double;
Begin
 If (Seconds < MinEpoch) Or (Seconds >= MaxEpoch) Then
  Fail('Date hors plage du calendrier (0001-9999). Choisir un autre systeme de temps.',1);
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
 Calendar := Zero(Y,4)+'-'+Zero(M,2)+'-'+Zero(D,2)+'T'+
  Zero(H,2)+':'+Zero(Minute,2)+':'+Zero(Sec,2)+'.'+Zero(MS Mod 1000,3);
End;

Function NextField(Var S: String; Sep: Char): String;
Var P: Integer;
Begin
 P := Pos(Sep,S);
 If P = 0 Then Begin NextField := S; S := '' End
 Else Begin NextField := Copy(S,1,P-1); Delete(S,1,P) End;
End;

Function IntegerField(S: String; Text: String): LongInt;
Var V: LongInt; Code: Integer;
Begin
 If (S = '') Or (Length(S) > 9) Then SyntaxError('Date invalide : '+Text);
 Val(S,V,Code);
 If (Code <> 0) Or (V < 0) Then SyntaxError('Date invalide : '+Text);
 IntegerField := V;
End;

Function RealNumber(S: String; What: String): Double;
Var V: Double; Code: Integer;
Begin
 S := Trim(S);
 If S = '' Then SyntaxError('Valeur manquante pour '+What+'.');
 Val(S,V,Code);
 If Code <> 0 Then SyntaxError('Nombre invalide pour '+What+' : '+S);
 If (V < -1.0E15) Or (V > 1.0E15) Then SyntaxError('Valeur hors limites pour '+What+'.');
 RealNumber := V;
End;

{ Lit AAAA-MM-JJ[T| ]HH[:MM[:SS[.f]]] ; Extra signale la seconde 60 (UTC). }
Function ParseCalendar(Text: String; Var Extra: Boolean): Double;
Var S, DatePart, TimePart, F: String;
    Y, M, D, H, Mi: LongInt; Sec: Double; P: Integer;
Begin
 Extra := False;
 S := Trim(Text);
 If (S <> '') And (UpCase(S[Length(S)]) = 'Z') Then Delete(S,Length(S),1);
 P := Pos('T',Upper(S));
 If P = 0 Then P := Pos(' ',S);
 If P = 0 Then Begin DatePart := S; TimePart := '' End
 Else Begin DatePart := Copy(S,1,P-1); TimePart := Trim(Copy(S,P+1,255)) End;
 F := NextField(DatePart,'-'); Y := IntegerField(F,Text);
 F := NextField(DatePart,'-'); M := IntegerField(F,Text);
 F := NextField(DatePart,'-'); D := IntegerField(F,Text);
 If DatePart <> '' Then SyntaxError('Date invalide : '+Text);
 If (Y < 1) Or (Y > 9999) Or (M < 1) Or (M > 12) Or (D < 1) Or
    (D > DaysInMonth(Y,M)) Then SyntaxError('Date inexistante : '+Text);
 H := 0; Mi := 0; Sec := 0;
 If TimePart <> '' Then Begin
  F := NextField(TimePart,':'); H := IntegerField(F,Text);
  If TimePart <> '' Then Begin
   F := NextField(TimePart,':'); Mi := IntegerField(F,Text);
   If TimePart <> '' Then Sec := RealNumber(TimePart,'les secondes');
  End;
 End;
 If (H > 23) Or (Mi > 59) Or (Sec < 0) Or (Sec >= 61) Then
  SyntaxError('Heure invalide : '+Text);
 If Sec >= 60 Then Extra := True;
 ParseCalendar := DayStart(Y,M,D)+(H*3600.0+Mi*60.0)+Sec;
End;

Function UTCtoET(U: Double; Extra: Boolean): Double;
Var K, Offset: Integer; TAI, ET, Mean: Double; Found: Boolean;
Begin
 If U < LeapEpoch[1] Then Fail('UTC avant 1972 non pris en charge.',1);
 Offset := 10;
 If Extra Then Begin
  Found := False;
  For K := 2 To LeapCount Do
   If (U >= LeapEpoch[K]) And (U < LeapEpoch[K]+1) Then Begin
    Offset := K+8; Found := True;
   End;
  If Not Found Then SyntaxError('Seconde intercalaire inexistante.');
 End Else
  For K := 2 To LeapCount Do If U >= LeapEpoch[K] Then Offset := K+9;
 TAI := U+Offset;
 ET := TAI+32.184;
 For K := 1 To 6 Do Begin
  Mean := 6.239996+1.99096871E-7*ET;
  ET := TAI+32.184+0.001657*Sin(Mean+0.01671*Sin(Mean));
 End;
 UTCtoET := ET;
End;

{ ------------------------------------------------------------------ }
{ Lecture DAF/SPK                                                    }
{ ------------------------------------------------------------------ }

Function IntAtK(K: PKernel; Offset: Integer): LongInt;
Var B: Array[0..3] Of Byte; J: Integer; V: LongInt;
Begin
 For J := 0 To 3 Do
  If K^.Big Then B[J] := K^.Cache[Offset+3-J]
  Else B[J] := K^.Cache[Offset+J];
 Move(B,V,4);
 IntAtK := V;
End;

Function DblAtK(K: PKernel; Offset: Integer): Double;
Var B: TBytes8; J: Integer; V: Double;
Begin
 For J := 0 To 7 Do
  If K^.Big Then B[J] := K^.Cache[Offset+7-J]
  Else B[J] := K^.Cache[Offset+J];
 { Rejeter NaN et infini avant toute instruction flottante. }
 If ((B[7] And $7F) = $7F) And ((B[6] And $F0) = $F0) Then Begin
  DataError := True; DblAtK := 0; Exit;
 End;
 Move(B,V,8);
 DblAtK := V;
End;

Function TextAtK(K: PKernel; Offset, Count: Integer): String;
Var J: Integer; S: String;
Begin
 S := '';
 For J := Offset To Offset+Count-1 Do
  If (K^.Cache[J] >= 32) And (K^.Cache[J] < 127) Then S := S+Chr(K^.Cache[J])
  Else S := S+' ';
 TextAtK := S;
End;

Function ReadRec(K: PKernel; Number: LongInt): Boolean;
Var Got, Err: Word;
Begin
 ReadRec := False;
 If K^.CacheRec = Number Then Begin ReadRec := True; Exit End;
 K^.CacheRec := 0;
 If (Number < 1) Or (Number > K^.Bytes Div 1024) Then Begin
  ErrMsg := 'Record '+Num(Number)+' hors du fichier '+K^.Path+'.'; Exit;
 End;
 {$I-}
 Seek(K^.F,(Number-1)*1024);
 Err := IOResult;
 {$I+}
 If Err <> 0 Then Begin ErrMsg := 'Positionnement impossible.'; Exit End;
 {$I-} BlockRead(K^.F,K^.Cache,1024,Got); Err := IOResult; {$I+}
 If (Err <> 0) Or (Got <> 1024) Then Begin
  ErrMsg := 'Record '+Num(Number)+' tronque dans '+K^.Path+'.'; Exit;
 End;
 K^.CacheRec := Number;
 ReadRec := True;
End;

{ Mot DAF d'adresse Addr (base 1). Positionne DataError en cas d'echec. }
Function Wd(K: PKernel; Addr: LongInt): Double;
Begin
 Wd := 0;
 If (Addr < 1) Or (Addr > K^.Bytes Div 8) Then Begin
  DataError := True; ErrMsg := 'Adresse de donnees hors fichier.'; Exit;
 End;
 If Not ReadRec(K,(Addr-1) Div 128+1) Then Begin DataError := True; Exit End;
 Wd := DblAtK(K,((Addr-1) Mod 128)*8);
End;

Function IntWord(K: PKernel; Addr: LongInt; Var V: LongInt): Boolean;
Var D: Double;
Begin
 IntWord := False;
 D := Wd(K,Addr);
 If DataError Then Exit;
 If (D < 0) Or (D > 1.0E9) Or (D <> Int(D)) Then Begin
  DataError := True; ErrMsg := 'Parametre entier invalide dans un segment.'; Exit;
 End;
 V := Trunc(D);
 IntWord := True;
End;

Procedure CloseKernel(K: PKernel);
Var Err: Word;
Begin
 If K^.Opened Then Begin
  {$I-} Close(K^.F); Err := IOResult; {$I+}
  K^.Opened := False;
 End;
End;

Function OpenKernel(K: PKernel; Path: String): Boolean;
Var
 Err: Word;
 Fwd, Bwd, Free, Current, Previous, Visited, Next, Prev: LongInt;
 N, J, Offset: Integer;
 A, B: Double;
 Format: String;
Begin
 OpenKernel := False;
 K^.Path := Path; K^.Opened := False; K^.Count := 0; K^.CacheRec := 0;
 DataError := False;
 Assign(K^.F,Path);
 FileMode := 0;
 {$I-} Reset(K^.F,1); Err := IOResult; {$I+}
 If Err <> 0 Then Begin ErrMsg := 'Impossible d''ouvrir '+Path+'.'; Exit End;
 K^.Opened := True;
 {$I-} K^.Bytes := FileSize(K^.F); Err := IOResult; {$I+}
 If (Err <> 0) Or (K^.Bytes < 1024) Or (K^.Bytes Mod 1024 <> 0) Then Begin
  ErrMsg := 'Taille DAF invalide ou fichier tronque : '+Path; Exit;
 End;
 K^.Big := False;
 If Not ReadRec(K,1) Then Exit;
 If TextAtK(K,0,8) <> 'DAF/SPK ' Then Begin
  ErrMsg := 'Fichier non SPK DAF : '+Path; Exit;
 End;
 Format := TextAtK(K,88,8);
 If (Format <> 'LTL-IEEE') And (Format <> 'BIG-IEEE') Then Begin
  ErrMsg := 'Format binaire non pris en charge : '+Format; Exit;
 End;
 K^.Big := Format = 'BIG-IEEE';
 If (IntAtK(K,8) <> 2) Or (IntAtK(K,12) <> 6) Then Begin
  ErrMsg := 'Dimensions des descripteurs SPK invalides : '+Path; Exit;
 End;
 Fwd := IntAtK(K,76); Bwd := IntAtK(K,80); Free := IntAtK(K,84);
 If (Free < 129) Or (Free > K^.Bytes Div 8+1) Then Begin
  ErrMsg := 'Adresse libre DAF hors fichier.'; Exit;
 End;
 If (Fwd = 0) And (Bwd = 0) Then Begin OpenKernel := True; Exit End;
 If (Fwd < 2) Or (Bwd < 2) Then Begin
  ErrMsg := 'Chaine de descripteurs DAF invalide.'; Exit;
 End;
 Current := Fwd; Previous := 0; Visited := 0;
 While Current <> 0 Do Begin
  Inc(Visited);
  If (Visited > K^.Bytes Div 1024) Or (Current >= K^.Bytes Div 1024) Then Begin
   ErrMsg := 'Chaine de records DAF incoherente.'; Exit;
  End;
  If Not ReadRec(K,Current) Then Exit;
  DataError := False;
  A := DblAtK(K,0); Next := Trunc(A);
  B := DblAtK(K,8); Prev := Trunc(B);
  N := Trunc(DblAtK(K,16));
  If DataError Or (A < 0) Or (A > K^.Bytes Div 1024) Or (B < 0) Or
     (B > K^.Bytes Div 1024) Or (N < 0) Or (N > 25) Or (Prev <> Previous) Then Begin
   ErrMsg := 'Record sommaire '+Num(Current)+' invalide.'; Exit;
  End;
  For J := 0 To N-1 Do Begin
   Offset := 24+J*40;
   A := DblAtK(K,Offset); B := DblAtK(K,Offset+8);
   If DataError Or (A > B) Then Begin
    ErrMsg := 'Descripteur de segment invalide.'; Exit;
   End;
   If K^.Count = MaxSegments Then Begin
    ErrMsg := 'Limite de '+Num(MaxSegments)+' segments depassee : '+Path; Exit;
   End;
   Inc(K^.Count);
   With K^.Segs^[K^.Count] Do Begin
    First := A; Last := B;
    Body := IntAtK(K,Offset+16); Center := IntAtK(K,Offset+20);
    Frame := IntAtK(K,Offset+24); Kind := IntAtK(K,Offset+28);
    Start := IntAtK(K,Offset+32); Finish := IntAtK(K,Offset+36);
    If (Start < 129) Or (Finish < Start) Or (Finish >= Free) Or
       (Finish > K^.Bytes Div 8) Or (Body = Center) Then Begin
     ErrMsg := 'Segment SPK invalide dans '+Path+'.'; Exit;
    End;
   End;
  End;
  Previous := Current; Current := Next;
 End;
 If Previous <> Bwd Then Begin ErrMsg := 'Dernier record sommaire incoherent.'; Exit End;
 OpenKernel := True;
End;

{ Dernier segment du fichier couvrant ET pour le corps (priorite au dernier). }
Function FindSegment(K: PKernel; Body: LongInt; ET: Double): Integer;
Var J: Integer;
Begin
 FindSegment := 0;
 For J := K^.Count DownTo 1 Do
  If (K^.Segs^[J].Body = Body) And (K^.Segs^[J].First <= ET) And
     (K^.Segs^[J].Last >= ET) Then Begin
   FindSegment := J; Exit;
  End;
End;

{ ------------------------------------------------------------------ }
{ Evaluation des segments                                            }
{ ------------------------------------------------------------------ }

Function EvalCheb(K: PKernel; Var Sg: TSegment; ET: Double; Var S: TState): Boolean;
Var
 E, N, RS, NC, NComp, Idx, Addr, J, C: LongInt;
 Init, IntLen, Mid, Rad, X, Sum, SumD, Coef: Double;
 T, DT: Array[0..199] Of Double;
Begin
 EvalCheb := False;
 E := Sg.Finish;
 If Not IntWord(K,E,N) Then Exit;
 If Not IntWord(K,E-1,RS) Then Exit;
 IntLen := Wd(K,E-2);
 Init := Wd(K,E-3);
 If DataError Then Exit;
 If Sg.Kind = 2 Then NComp := 3 Else NComp := 6;
 If (N < 1) Or (IntLen <= 0) Or (RS < 2+NComp) Or ((RS-2) Mod NComp <> 0) Then Begin
  ErrMsg := 'Parametres Tchebychev invalides.'; Exit;
 End;
 NC := (RS-2) Div NComp;
 If NC > 200 Then Begin ErrMsg := 'Degre Tchebychev trop eleve.'; Exit End;
 If (Sg.Start+N*RS+3) <> E Then Begin
  ErrMsg := 'Taille de segment Tchebychev incoherente.'; Exit;
 End;
 X := (ET-Init)/IntLen;
 If X < 0 Then Idx := 0 Else Idx := Trunc(X);
 If Idx >= N Then Idx := N-1;
 Addr := Sg.Start+Idx*RS;
 Mid := Wd(K,Addr); Rad := Wd(K,Addr+1);
 If DataError Then Exit;
 If Rad <= 0 Then Begin ErrMsg := 'Rayon Tchebychev invalide.'; Exit End;
 X := (ET-Mid)/Rad;
 T[0] := 1; DT[0] := 0;
 If NC > 1 Then Begin T[1] := X; DT[1] := 1 End;
 For J := 2 To NC-1 Do Begin
  T[J] := 2*X*T[J-1]-T[J-2];
  DT[J] := 2*T[J-1]+2*X*DT[J-1]-DT[J-2];
 End;
 For C := 0 To 2 Do Begin
  Sum := 0; SumD := 0;
  For J := 0 To NC-1 Do Begin
   Coef := Wd(K,Addr+2+C*NC+J);
   Sum := Sum+Coef*T[J];
   SumD := SumD+Coef*DT[J];
  End;
  S[C+1] := Sum;
  If Sg.Kind = 2 Then S[C+4] := SumD/Rad;
 End;
 If Sg.Kind = 3 Then
  For C := 3 To 5 Do Begin
   Sum := 0;
   For J := 0 To NC-1 Do Sum := Sum+Wd(K,Addr+2+C*NC+J)*T[J];
   S[C+1] := Sum;
  End;
 If DataError Then Exit;
 EvalCheb := True;
End;

Function Lagrange(G: Integer; Var Z: TNodes; Var Y: TValues; Comp: Integer;
                  X: Double): Double;
Var J, M: Integer; Sum, Term: Double;
Begin
 Sum := 0;
 For J := 0 To G-1 Do Begin
  Term := Y[J,Comp];
  For M := 0 To G-1 Do
   If M <> J Then Term := Term*(X-Z[M])/(Z[J]-Z[M]);
  Sum := Sum+Term;
 End;
 Lagrange := Sum;
End;

Procedure Hermite(G: Integer; Var Z: TNodes; Var Y: TValues; Comp: Integer;
                  X: Double; Var Value, Deriv: Double);
Var NZ, F: TNodes; N, J, M, L: Integer; P, DP, Base: Double;
Begin
 N := 2*G;
 { Retrancher la premiere valeur : meme derivee, moins de cancellation. }
 Base := Y[0,Comp];
 For J := 0 To G-1 Do Begin
  NZ[2*J] := Z[J]; NZ[2*J+1] := Z[J];
  F[2*J] := Y[J,Comp]-Base; F[2*J+1] := Y[J,Comp]-Base;
 End;
 For M := 1 To N-1 Do
  For L := N-1 DownTo M Do
   If (M = 1) And Odd(L) Then F[L] := Y[L Div 2,Comp+3]
   Else F[L] := (F[L]-F[L-1])/(NZ[L]-NZ[L-M]);
 P := F[N-1]; DP := 0;
 For J := N-2 DownTo 0 Do Begin
  DP := DP*(X-NZ[J])+P;
  P := P*(X-NZ[J])+F[J];
 End;
 Value := P+Base; Deriv := DP;
End;

Function EvalDiscrete(K: PKernel; Var Sg: TSegment; ET: Double; Var S: TState): Boolean;
Var
 E, N, Deg, G, I, Low, Closest, Lo, Hi, Mid, J, C: LongInt;
 T0, Step, Ref, Tk, Tn, V, D: Double;
 Equal, Herm: Boolean;
 Z: TNodes;
 Y: TValues;

 Function EpochAt(Index: LongInt): Double;
 Var Fk: Double;
 Begin
  If Equal Then Begin
   Fk := Index-1;
   EpochAt := T0+Fk*Step;
  End Else EpochAt := Wd(K,Sg.Start+6*N+Index-1);
 End;

Begin
 EvalDiscrete := False;
 E := Sg.Finish;
 Equal := (Sg.Kind = 8) Or (Sg.Kind = 12);
 Herm := (Sg.Kind = 12) Or (Sg.Kind = 13);
 If Not IntWord(K,E,N) Then Exit;
 If Not IntWord(K,E-1,Deg) Then Exit;
 T0 := 0; Step := 0;
 If Equal Then Begin
  Step := Wd(K,E-2);
  T0 := Wd(K,E-3);
  If DataError Then Exit;
  If Step <= 0 Then Begin ErrMsg := 'Pas de segment invalide.'; Exit End;
 End;
 If Herm Then G := (Deg+1) Div 2 Else G := Deg+1;
 If (N < 1) Or (G < 1) Or (G > N) Or (Herm And (G > 14)) Or (G > 28) Then Begin
  ErrMsg := 'Fenetre d''interpolation invalide (degre '+Num(Deg)+', '+Num(N)+' etats).';
  Exit;
 End;
 If Equal Then Begin
  Tk := (ET-T0)/Step;
  If Tk < 0 Then I := 1
  Else If Tk > N Then I := N
  Else I := Trunc(Tk)+1;
  If I > N Then I := N;
 End Else Begin
  Lo := 1; Hi := N;
  While Lo < Hi Do Begin
   Mid := (Lo+Hi+1) Div 2;
   If EpochAt(Mid) <= ET Then Lo := Mid Else Hi := Mid-1;
  End;
  I := Lo;
 End;
 If DataError Then Exit;
 If Odd(G) Then Begin
  Closest := I;
  If I < N Then Begin
   Tk := EpochAt(I); Tn := EpochAt(I+1);
   If ET-Tk > Tn-ET Then Closest := I+1;
  End;
  Low := Closest-(G-1) Div 2;
 End Else Low := I-G Div 2+1;
 If Low > N-G+1 Then Low := N-G+1;
 If Low < 1 Then Low := 1;
 Ref := EpochAt(Low);
 For J := 0 To G-1 Do Begin
  Z[J] := EpochAt(Low+J)-Ref;
  For C := 1 To 6 Do Y[J,C] := Wd(K,Sg.Start+6*(Low+J-1)+C-1);
 End;
 If DataError Then Exit;
 For J := 1 To G-1 Do
  If Z[J] <= Z[J-1] Then Begin ErrMsg := 'Epochs non croissantes dans un segment.'; Exit End;
 If Herm Then
  For C := 1 To 3 Do Begin
   Hermite(G,Z,Y,C,ET-Ref,V,D);
   S[C] := V; S[C+3] := D;
  End
 Else
  For C := 1 To 6 Do S[C] := Lagrange(G,Z,Y,C,ET-Ref);
 EvalDiscrete := True;
End;

Function EvalSegment(K: PKernel; Var Sg: TSegment; ET: Double; Var S: TState): Boolean;
Begin
 DataError := False;
 ErrMsg := 'Donnees de segment illisibles.';
 If (Sg.Kind = 2) Or (Sg.Kind = 3) Then EvalSegment := EvalCheb(K,Sg,ET,S)
 Else If (Sg.Kind = 8) Or (Sg.Kind = 9) Or (Sg.Kind = 12) Or (Sg.Kind = 13) Then
  EvalSegment := EvalDiscrete(K,Sg,ET,S)
 Else Begin
  ErrMsg := 'Type SPK '+Num(Sg.Kind)+' non pris en charge (corps '+Num(Sg.Body)+').';
  EvalSegment := False;
 End;
End;

{ Rotation d'un etat entre J2000 et ECLIPJ2000 (ToEcliptic) ou inverse. }
Procedure RotateState(Var S: TState; ToEcliptic: Boolean);
Var C, Sn, Y, Z: Double; J: Integer;
Begin
 C := Cos(Obliquity); Sn := Sin(Obliquity);
 If Not ToEcliptic Then Sn := -Sn;
 For J := 0 To 1 Do Begin
  Y := S[2+3*J]; Z := S[3+3*J];
  S[2+3*J] := C*Y+Sn*Z;
  S[3+3*J] := -Sn*Y+C*Z;
 End;
End;

{ Construit la chaine corps -> parent -> ... jusqu'a 0 ou jusqu'a un corps sans
  segment; St[i] = etat cumule du corps par rapport a ID[i], repere J2000.
  Retourne 0, ou 2 sur erreur de donnees. }
Function BuildPath(K: PKernel; Body: LongInt; ET: Double; Var P: TPath): Integer;
Var Cur: LongInt; Idx, J: Integer; S: TState; Done: Boolean;
Begin
 BuildPath := 0;
 P.N := 0; P.ID[0] := Body;
 For J := 1 To 6 Do P.St[0][J] := 0;
 Cur := Body; Done := False;
 While (Cur <> 0) And Not Done Do Begin
  Idx := FindSegment(K,Cur,ET);
  If Idx = 0 Then Done := True
  Else Begin
   If P.N >= MaxLevel Then Begin
    ErrMsg := 'Chaine de segments trop longue pour le corps '+Num(Body)+'.';
    BuildPath := 2; Exit;
   End;
   If Not EvalSegment(K,K^.Segs^[Idx],ET,S) Then Begin BuildPath := 2; Exit End;
   If K^.Segs^[Idx].Frame = 17 Then RotateState(S,False)
   Else If K^.Segs^[Idx].Frame <> 1 Then Begin
    ErrMsg := 'Repere '+Num(K^.Segs^[Idx].Frame)+' non pris en charge (corps '+
              Num(Cur)+').';
    BuildPath := 2; Exit;
   End;
   Inc(P.N);
   P.ID[P.N] := K^.Segs^[Idx].Center;
   For J := 1 To 6 Do P.St[P.N][J] := P.St[P.N-1][J]+S[J];
   Cur := K^.Segs^[Idx].Center;
  End;
 End;
End;

{ Etat de Body par rapport a Center : 0 ok, 1 sans couverture, 2 erreur. }
Function GetState(K: PKernel; Body, Center: LongInt; ET: Double; Var S: TState): Integer;
Var P1, P2: TPath; R, A, B, J, Res: Integer;
Begin
 Res := 1;
 GetState := 1;
 If Body = Center Then Begin
  For J := 1 To 6 Do S[J] := 0;
  GetState := 0; Exit;
 End;
 R := BuildPath(K,Body,ET,P1);
 If R <> 0 Then Begin GetState := 2; Exit End;
 R := BuildPath(K,Center,ET,P2);
 If R <> 0 Then Begin GetState := 2; Exit End;
 For A := 0 To P1.N Do
  For B := 0 To P2.N Do
   If (P1.ID[A] = P2.ID[B]) And (Res = 1) Then Begin
    For J := 1 To 6 Do S[J] := P1.St[A][J]-P2.St[B][J];
    Res := 0;
   End;
 If (Res = 0) And (Frame = 17) Then RotateState(S,True);
 GetState := Res;
End;

Function BodyID(S: String; What: String): LongInt;
Var V: LongInt; Code, J: Integer; U: String;
Begin
 U := Upper(Trim(S));
 If U = '' Then SyntaxError('Corps manquant pour '+What+'.');
 Val(U,V,Code);
 If Code = 0 Then Begin BodyID := V; Exit End;
 For J := 1 To NameCount Do
  If NameText[J] = U Then Begin BodyID := NameCode[J]; Exit End;
 SyntaxError('Corps inconnu pour '+What+' : '+S);
End;

Function BodyLabel(ID: LongInt): String;
Var J: Integer;
Begin
 BodyLabel := Num(ID);
 For J := NameCount DownTo 1 Do
  If (NameCode[J] = ID) And (NameText[J] <> 'SSB') And (NameText[J] <> 'EMB') Then
   BodyLabel := Num(ID)+' '+NameText[J];
End;

Procedure LoadKernel(Which: Integer; Path: String);
Begin
 New(Kern[Which]);
 New(Kern[Which]^.Segs);
 Kern[Which]^.Opened := False;
 If Not OpenKernel(Kern[Which],Path) Then Fail(ErrMsg,1);
End;

{ Couverture des segments directs du corps dans le fichier. }
Function Coverage(K: PKernel; Body: LongInt; Var Lo, Hi: Double): Boolean;
Var J: Integer; Found: Boolean;
Begin
 Found := False;
 For J := 1 To K^.Count Do
  If K^.Segs^[J].Body = Body Then Begin
   If Not Found Then Begin
    Lo := K^.Segs^[J].First; Hi := K^.Segs^[J].Last; Found := True;
   End Else Begin
    If K^.Segs^[J].First < Lo Then Lo := K^.Segs^[J].First;
    If K^.Segs^[J].Last > Hi Then Hi := K^.Segs^[J].Last;
   End;
  End;
 Coverage := Found;
End;


{ ------------------------------------------------------------------ }
{ Systemes de temps                                                  }
{ ------------------------------------------------------------------ }

Var
 TimeSys, SpkName, OutName, RefName, Interp, ObjName, CenName: String;
 StartText, StopText, Arg, Dummy: String;
 Body, Center: LongInt;
 Degree, Decimals, ArgIndex, I, Runs, RunNo, Code: Integer;
 Start, Stop, Step, ET, Used, Lo, Hi: Double;
 Points, Index: LongInt;
 HasBody, HasCenter, HasStart, HasStop, HasStep, HasPoints, Force, InRun: Boolean;
 S: TState;
 Out: Text;
 RunFirst, RunLast: Array[1..200] Of LongInt;

Procedure Help;
Begin
 WriteLn('SPK2OEM - Cette commande permet d''exporter un fichier SPK vers un ',
         'message d''',#130,'ph',#130,'m',#130,'rides CCSDS OEM.');
 WriteLn;
 WriteLn('Syntaxe : SPK2OEM -body <corps> -center <centre> [options] <fichier.bsp>');
 WriteLn;
 WriteLn('Options :');
 WriteLn('  -body <corps>    Corps cible (numero NAIF ou nom, requis)');
 WriteLn('  -center <centre> Centre de reference (numero ou nom, requis)');
 WriteLn('  -b <temps>       Debut (couverture du corps dans le SPK par defaut)');
 WriteLn('  -e <temps>       Fin (egale au debut si -b est donne seul)');
 WriteLn('  -s <secondes>    Pas entre les etats (secondes SI)');
 WriteLn('  -n <nombre>      Nombre d''etats (au lieu de -s)');
 WriteLn('  -ts <systeme>    Systeme de temps : UTC (defaut), TAI, TT ou TDB');
 WriteLn('  -r <repere>      REF_FRAME : EME2000 (defaut, J2000 accepte) ou ICRF');
 WriteLn('  -i <methode>     INTERPOLATION : LAGRANGE (defaut) ou HERMITE');
 WriteLn('  -g <degre>       INTERPOLATION_DEGREE, 1 a 27 (7 par defaut)');
 WriteLn('  -on <nom>        OBJECT_NAME (nom du corps par defaut)');
 WriteLn('  -cn <nom>        CENTER_NAME (nom du centre par defaut)');
 WriteLn('  -p <decimales>   Decimales des positions en km, 0 a 9 (6 par defaut;');
 WriteLn('                   les vitesses en ont 3 de plus)');
 WriteLn('  -o <fichier>     Fichier OEM de sortie (sortie standard par defaut)');
 WriteLn('  -f               Ecrase le fichier de sortie s''il existe');
 WriteLn('  -h, /?           Aide (-help et --help acceptes)');
 WriteLn;
 WriteLn('Temps : AAAA-MM-JJ[THH:MM:SS[.fraction]] dans le systeme -ts, entre');
 WriteLn('guillemets s''il y a une espace.');
 WriteLn('Noms : numeros NAIF, SSB, EMB, SUN, MOON, EARTH, MARS, JUPITER, etc.');
 WriteLn;
 WriteLn('Exemples :');
 WriteLn('  SPK2OEM -body MOON -center EARTH -b 2020-01-01 -e 2020-01-02 -s 600 de.bsp');
 WriteLn('  SPK2OEM -body -82 -center 10 -n 100 -ts TDB -o sonde.oem sonde.bsp');
 WriteLn;
 WriteLn('Etats geometriques, types SPK 2, 3, 8, 9, 12 et 13. Un bloc OEM par');
 WriteLn('intervalle continu. Pas d''accelerations ni de covariances.');
 WriteLn('Codes retour : 0 succes, 1 erreur de lecture/donnees, 2 syntaxe.');
End;

Function RoundMs(V: Double): Double;
Var T: Double;
Begin
 If V >= 0 Then T := Int(V*1000.0+0.5) Else T := -Int(-V*1000.0+0.5);
 RoundMs := T/1000.0;
End;

Function ETtoTAI(ET: Double): Double;
Var Mean: Double;
Begin
 Mean := 6.239996+1.99096871E-7*ET;
 ETtoTAI := ET-32.184-0.001657*Sin(Mean+0.01671*Sin(Mean));
End;

Function TAItoET(TAI: Double): Double;
Var K: Integer; E, Mean: Double;
Begin
 E := TAI+32.184;
 For K := 1 To 6 Do Begin
  Mean := 6.239996+1.99096871E-7*E;
  E := TAI+32.184+0.001657*Sin(Mean+0.01671*Sin(Mean));
 End;
 TAItoET := E;
End;

{ Secondes UTC (arrondies a la ms) de ET; IsLeap : seconde intercalaire,
  U est alors dans la derniere seconde de la journee (affichee :60). }
Function ETtoUTC(ET: Double; Var IsLeap: Boolean): Double;
Var TAI, U, Threshold, DaySeconds, Millis: Double; K, Delta: Integer;
Begin
 TAI := ETtoTAI(ET);
 DaySeconds := Int(TAI/86400.0)*86400.0;
 Millis := Round((TAI-DaySeconds)*1000.0);
 TAI := DaySeconds+Millis/1000.0;
 If TAI < LeapEpoch[1]+10 Then
  Fail('UTC avant 1972 non pris en charge. Utiliser -ts TDB.',1);
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
 ETtoUTC := U;
End;

{ Etiquette OEM de l'epoque ET dans le systeme -ts, arrondie a la ms, et
  epoque ET exacte correspondant a l'etiquette (pour evaluer l'etat). }
Function EpochLabel(ET: Double; Var Used: Double): String;
Var U, R: Double; IsLeap: Boolean;
Begin
 If (ET < MinEpoch+100) Or (ET >= MaxEpoch-100) Then
  Fail('Epoque hors plage du calendrier (0001-9999).',1);
 If TimeSys = 'UTC' Then Begin
  U := ETtoUTC(ET,IsLeap);
  EpochLabel := Calendar(U,IsLeap);
  If IsLeap Then Used := UTCtoET(U+1,True) Else Used := UTCtoET(U,False);
 End Else If TimeSys = 'TDB' Then Begin
  R := RoundMs(ET);
  EpochLabel := Calendar(R,False);
  Used := R;
 End Else If TimeSys = 'TT' Then Begin
  R := RoundMs(ETtoTAI(ET)+32.184);
  EpochLabel := Calendar(R,False);
  Used := TAItoET(R-32.184);
 End Else Begin
  R := RoundMs(ETtoTAI(ET));
  EpochLabel := Calendar(R,False);
  Used := TAItoET(R);
 End;
End;

Function ReadTime(Text: String; What: String): Double;
Var V: Double; Extra: Boolean;
Begin
 V := ParseCalendar(Text,Extra);
 If Extra And (TimeSys <> 'UTC') Then SyntaxError('Seconde 60 reservee a UTC.');
 If TimeSys = 'UTC' Then V := UTCtoET(V,Extra)
 Else If TimeSys = 'TAI' Then V := TAItoET(V)
 Else If TimeSys = 'TT' Then V := TAItoET(V-32.184);
 If (V < MinEpoch) Or (V >= MaxEpoch) Then
  SyntaxError('Epoque hors plage du calendrier (0001-9999) pour '+What+'.');
 ReadTime := V;
End;

{ ------------------------------------------------------------------ }
{ Ecriture OEM                                                       }
{ ------------------------------------------------------------------ }

Function Fixed(V: Double; Dec: Integer): String;
Var T: String; K: Integer; Zeros: Boolean;
Begin
 Str(V:0:Dec,T);
 Zeros := True;
 For K := 1 To Length(T) Do
  If Not (T[K] In ['-','0','.']) Then Zeros := False;
 If Zeros And (T[1] = '-') Then Delete(T,1,1);
 Fixed := T;
End;

Function NameOf(ID: LongInt): String;
Var J: Integer; Found: String;
Begin
 Found := Num(ID);
 For J := 1 To NameCount Do
  If NameCode[J] = ID Then Found := NameText[J];
 NameOf := Found;
End;

Function FileExists(Path: String): Boolean;
Var T: Text; Err: Word;
Begin
 Assign(T,Path);
 FileMode := 0;
 {$I-} Reset(T); Err := IOResult; {$I+}
 If Err = 0 Then Begin
  {$I-} Close(T); Err := IOResult; {$I+}
  FileExists := True;
 End Else FileExists := False;
End;

Function OptionValue: String;
Begin
 Inc(ArgIndex);
 If ArgIndex > ParamCount Then SyntaxError('Valeur manquante pour '+Arg+'.');
 OptionValue := ParamStr(ArgIndex);
End;

Function EpochTime(N: LongInt): Double;
Begin
 EpochTime := Start+N*Step;
End;

Procedure WriteHeader;
Var Y, M, D, W, H, Mi, Sc, S100: Word;
Begin
 GetDate(Y,M,D,W);
 GetTime(H,Mi,Sc,S100);
 WriteLn(Out,'CCSDS_OEM_VERS = 2.0');
 WriteLn(Out,'COMMENT Genere par SPK2OEM a partir de ',SpkName);
 WriteLn(Out,'COMMENT Etats geometriques (sans temps-lumiere ni aberration), km et km/s');
 WriteLn(Out,'CREATION_DATE = ',Zero(Y,4),'-',Zero(M,2),'-',Zero(D,2),'T',
         Zero(H,2),':',Zero(Mi,2),':',Zero(Sc,2));
 WriteLn(Out,'ORIGINATOR = SPK2OEM');
End;

Procedure WriteBlock(First, Last: LongInt);
Var N: LongInt; U: Double; T1, T2: String; R, K: Integer;
Begin
 T1 := EpochLabel(EpochTime(First),U);
 T2 := EpochLabel(EpochTime(Last),U);
 WriteLn(Out);
 WriteLn(Out,'META_START');
 WriteLn(Out,'OBJECT_NAME = ',ObjName);
 WriteLn(Out,'OBJECT_ID = ',Num(Body));
 WriteLn(Out,'CENTER_NAME = ',CenName);
 WriteLn(Out,'REF_FRAME = ',RefName);
 WriteLn(Out,'TIME_SYSTEM = ',TimeSys);
 WriteLn(Out,'START_TIME = ',T1);
 WriteLn(Out,'STOP_TIME = ',T2);
 WriteLn(Out,'INTERPOLATION = ',Interp);
 WriteLn(Out,'INTERPOLATION_DEGREE = ',Degree);
 WriteLn(Out,'META_STOP');
 WriteLn(Out);
 For N := First To Last Do Begin
  T1 := EpochLabel(EpochTime(N),U);
  R := GetState(Kern[1],Body,Center,U,S);
  If R = 2 Then Fail(ErrMsg,1);
  If R <> 0 Then Fail('Etat indisponible a '+T1+'.',1);
  Write(Out,T1);
  For K := 1 To 3 Do Write(Out,' ',Fixed(S[K],Decimals));
  For K := 4 To 6 Do Write(Out,' ',Fixed(S[K],Decimals+3));
  WriteLn(Out);
 End;
End;

Begin
 TimeSys := 'UTC'; RefName := 'EME2000'; Interp := 'LAGRANGE'; Degree := 7;
 Decimals := 6; ObjName := ''; CenName := ''; SpkName := ''; OutName := '';
 StartText := ''; StopText := '';
 HasBody := False; HasCenter := False; HasStart := False; HasStop := False;
 HasStep := False; HasPoints := False; Force := False;
 Body := 0; Center := 0; Step := 0; Points := 0; Start := 0; Stop := 0;
 Frame := 1;
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
  If Arg = '-BODY' Then Begin Body := BodyID(OptionValue,'-body'); HasBody := True End
  Else If Arg = '-CENTER' Then Begin Center := BodyID(OptionValue,'-center'); HasCenter := True End
  Else If Arg = '-B' Then Begin StartText := OptionValue; HasStart := True End
  Else If Arg = '-E' Then Begin StopText := OptionValue; HasStop := True End
  Else If Arg = '-S' Then Begin Step := RealNumber(OptionValue,'-s'); HasStep := True End
  Else If Arg = '-N' Then Begin
   Step := RealNumber(OptionValue,'-n');
   If (Step < 1) Or (Step > MaxPoints) Or (Step <> Int(Step)) Then
    SyntaxError('-n doit etre un entier de 1 a 10000000.');
   Points := Trunc(Step); Step := 0; HasPoints := True;
  End
  Else If Arg = '-TS' Then Begin
   TimeSys := Upper(Trim(OptionValue));
   If (TimeSys <> 'UTC') And (TimeSys <> 'TAI') And (TimeSys <> 'TT') And
      (TimeSys <> 'TDB') Then SyntaxError('Systeme de temps inconnu : '+TimeSys+'.');
  End
  Else If Arg = '-R' Then Begin
   RefName := Upper(Trim(OptionValue));
   If RefName = 'J2000' Then RefName := 'EME2000';
   If (RefName <> 'EME2000') And (RefName <> 'ICRF') Then
    SyntaxError('Repere inconnu : '+RefName+' (EME2000 ou ICRF).');
  End
  Else If Arg = '-I' Then Begin
   Interp := Upper(Trim(OptionValue));
   If (Interp <> 'LAGRANGE') And (Interp <> 'HERMITE') Then
    SyntaxError('Interpolation inconnue : '+Interp+' (LAGRANGE ou HERMITE).');
  End
  Else If Arg = '-G' Then Begin
   Degree := Trunc(RealNumber(OptionValue,'-g'));
   If (Degree < 1) Or (Degree > 27) Then SyntaxError('-g doit etre de 1 a 27.');
  End
  Else If Arg = '-ON' Then ObjName := Trim(OptionValue)
  Else If Arg = '-CN' Then CenName := Trim(OptionValue)
  Else If Arg = '-P' Then Begin
   Decimals := Trunc(RealNumber(OptionValue,'-p'));
   If (Decimals < 0) Or (Decimals > 9) Then SyntaxError('-p doit etre de 0 a 9.');
  End
  Else If Arg = '-O' Then OutName := OptionValue
  Else If Arg = '-F' Then Force := True
  Else If Arg = '' Then SyntaxError('Nom de fichier vide.')
  Else If Arg[1] = '-' Then
   SyntaxError('Option inconnue ou non prise en charge : '+ParamStr(ArgIndex))
  Else If SpkName <> '' Then SyntaxError('Un seul fichier SPK est accepte.')
  Else SpkName := ParamStr(ArgIndex);
  Inc(ArgIndex);
 End;
 If SpkName = '' Then SyntaxError('Fichier SPK requis.');
 If Not HasBody Then SyntaxError('-body est requis.');
 If Not HasCenter Then SyntaxError('-center est requis.');
 If Body = Center Then SyntaxError('Le corps et le centre doivent differer.');
 If HasStep And HasPoints Then SyntaxError('-s et -n sont incompatibles.');
 If HasStep And (Step <= 0) Then SyntaxError('-s doit etre positif.');
 If HasStop And Not HasStart Then SyntaxError('-e exige -b.');
 If ObjName = '' Then ObjName := NameOf(Body);
 If CenName = '' Then CenName := NameOf(Center);
 If (OutName <> '') And FileExists(OutName) And Not Force Then
  Fail('Le fichier '+OutName+' existe deja. Utiliser -f pour l''ecraser.',1);

 New(Kern[1]);
 New(Kern[1]^.Segs);
 Kern[1]^.Opened := False;
 If Not OpenKernel(Kern[1],SpkName) Then Fail(ErrMsg,1);

 If HasStart Then Begin
  Start := ReadTime(StartText,'-b');
  If HasStop Then Stop := ReadTime(StopText,'-e') Else Stop := Start;
 End Else Begin
  If Not Coverage(Kern[1],Body,Lo,Hi) Then
   Fail('Aucun segment pour le corps '+Num(Body)+' dans '+SpkName+'. Donner -b.',1);
  Start := Lo; Stop := Hi;
 End;
 If Stop < Start Then SyntaxError('-e doit etre posterieur ou egal a -b.');
 If Stop = Start Then Begin
  If HasStep Or (Points > 1) Then SyntaxError('-s et -n exigent -e posterieur a -b.');
  Points := 1; Step := 0;
 End Else If HasStep Then Begin
  If (Stop-Start)/Step > MaxPoints-1 Then SyntaxError('Trop d''etats (maximum 10000000).');
  Points := Trunc((Stop-Start)/Step+1.0E-9)+1;
 End Else If HasPoints Then Begin
  If Points = 1 Then Step := 0 Else Step := (Stop-Start)/(Points-1);
 End Else SyntaxError('-s ou -n est requis pour un intervalle.');

 { Passe 1 : intervalles continus couverts par le fichier SPK. }
 Runs := 0; InRun := False;
 For Index := 0 To Points-1 Do Begin
  ET := EpochTime(Index);
  Dummy := EpochLabel(ET,Used);
  Code := GetState(Kern[1],Body,Center,Used,S);
  If Code = 2 Then Fail(ErrMsg,1);
  If Code = 0 Then Begin
   If Not InRun Then Begin
    If Runs = 200 Then Fail('Plus de 200 intervalles discontinus dans la couverture.',1);
    Inc(Runs); RunFirst[Runs] := Index; InRun := True;
   End;
   RunLast[Runs] := Index;
  End Else InRun := False;
 End;
 If Runs = 0 Then Fail('Aucune epoque couverte par '+SpkName+' pour ces corps.',1);

 { Passe 2 : ecriture. }
 Assign(Out,OutName);
 {$I-} Rewrite(Out); I := IOResult; {$I+}
 If I <> 0 Then Fail('Impossible de creer '+OutName+'.',1);
 WriteHeader;
 For RunNo := 1 To Runs Do WriteBlock(RunFirst[RunNo],RunLast[RunNo]);
 {$I-} Close(Out); I := IOResult; {$I+}
 If I <> 0 Then Fail('Erreur de fermeture de '+OutName+'.',1);
 CloseKernel(Kern[1]);
 If OutName <> '' Then
  WriteLn('OEM ecrit : ',OutName,' (',Runs,' bloc(s), ',Points,' epoque(s) demandee(s)).');
End.
