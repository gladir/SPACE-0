{ @author: Sylvain Maltais (support@gladir.com)
 @created: 2026
 @website(https://www.gladir.com/space-0)
 @abstract(Target: Turbo Pascal 7, Free Pascal 3.2) }

Program FRMDIFF;

{$A-}
{$E+,N+}

{ Comparaison d'orientations de reperes, sous-ensemble autonome de FRMDIFF NAIF.
  Aucun kernel (SPK, CK, PCK, FK, LSK, meta-kernel) n'est lu : les reperes
  sont calcules par des modeles integres, sans unite externe ni inclusion.
  Reperes : J2000, ECLIPJ2000, B1950, GALACTIC et IAU_<corps> (modele IAU 2009
  des axes de rotation; termes periodiques inclus pour la Lune, Mercure et
  Neptune seulement, omis pour les autres corps).
  Pour chaque epoque, la rotation D = M2 * M1^T (coordonnees repere 1 vers
  repere 2) est analysee; l'angle/axe affiches sont ceux de la rotation
  directe D^T qui amene les axes du repere 1 sur ceux du repere 2.
  Temps : UTC (table NAIF0012 integree, 1972-2017, 37 s ensuite), ET/TDB
  calendrier ou secondes depuis J2000.
  Sources : naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/ug/frmdiff.html
            naif.jpl.nasa.gov/pub/naif/generic_kernels/pck/pck00010.tpc
            naif.jpl.nasa.gov/pub/naif/generic_kernels/lsk/naif0012.tls
  Compilation : fpc -Mtp FRMDIFF.PAS }

Const
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
 FrameCount = 15;
 FrameNames: Array[1..FrameCount] Of String[12] =
  ('J2000','ECLIPJ2000','B1950','GALACTIC','IAU_SUN','IAU_MERCURY',
   'IAU_VENUS','IAU_EARTH','IAU_MOON','IAU_MARS','IAU_JUPITER',
   'IAU_SATURN','IAU_URANUS','IAU_NEPTUNE','IAU_PLUTO');
 { Obliquite J2000 : 84381.448 secondes d'arc, en radians (Pi interdit en constante TP). }
 Obliquity = 0.40909280422232897;
 { J2000 vers B1950 (Standish) et J2000 vers GALACTIC (ICRS), v2 = M * v1. }
 B1950Matrix: Array[1..3,1..3] Of Double =
  ((0.9999256794956877,-0.0111814832204662,-0.0048590038153592),
   (0.0111814832391717,0.9999374848933135,-0.0000271625947142),
   (0.0048590037723143,-0.0000271702937440,0.9999881946023742));
 GalacticMatrix: Array[1..3,1..3] Of Double =
  ((-0.0548755604162154,-0.8734370902348850,-0.4838350155487132),
   (0.4941094278755837,-0.4448296299600112,0.7469822444972189),
   (-0.8676661490190047,-0.1980763734312015,0.4559837761750669));
 { Lune : angles E1..E13 (deg, deg/jour) et coefficients periodiques (deg). }
 MoonAngle0: Array[1..13] Of Double =
  (125.045,250.089,260.008,176.625,357.529,311.589,134.963,276.617,
   34.226,15.134,119.743,239.961,25.053);
 MoonRate: Array[1..13] Of Double =
  (-0.0529921,-0.1059842,13.0120009,13.3407154,0.9856003,26.4057084,
   13.0649930,0.3287146,1.7484877,-0.1589763,0.0036096,0.1643573,
   12.9590088);
 MoonRA: Array[1..13] Of Double =
  (-3.8787,-0.1204,0.0700,-0.0172,0,0.0072,0,0,0,-0.0052,0,0,0.0043);
 MoonDec: Array[1..13] Of Double =
  (1.5419,0.0239,-0.0278,0.0068,0,-0.0029,0.0009,0,0,0.0008,0,0,-0.0009);
 MoonW: Array[1..13] Of Double =
  (3.5610,0.1208,-0.0642,0.0158,0.0252,-0.0066,-0.0047,-0.0046,0.0028,
   0.0052,0.0040,0.0019,-0.0044);
 { Mercure : angles M1..M5 (deg, deg/jour) et coefficients de W (deg). }
 MercAngle0: Array[1..5] Of Double =
  (174.791086,349.582171,164.373257,339.164343,153.955429);
 MercRate: Array[1..5] Of Double =
  (4.092335,8.184670,12.277005,16.369340,20.461675);
 MercW: Array[1..5] Of Double =
  (0.00993822,-0.00104581,-0.00010280,-0.00002364,-0.00000532);

Type
 TMat = Array[1..3,1..3] Of Double;
 TVec = Array[1..3] Of Double;

Var
 LeapEpoch: Array[1..LeapCount] Of Double;
 Frame1, Frame2, Digits, I: Integer;
 TimeMode, DumpMode, AngleName, Arg, StartText, StopText: String;
 Start, Stop, Step, AngleFactor, ET: Double;
 Points, Index: LongInt;
 HasStep, HasPoints, HasStop, HasFrame1, HasFrame2: Boolean;
 ArgIndex: Integer;
 M1, M2, T1, D: TMat;
 Axis: TVec;
 Angle, MinAngle, MaxAngle, MinET, MaxET, SumAngle, SumSquare: Double;
 MaxAxis: TVec;

Procedure Fail(S: String; Code: Integer);
Begin
 WriteLn('Erreur : ',S);
 Halt(Code);
End;

Procedure SyntaxError(S: String);
Begin
 Fail(S+' Voir FRMDIFF -h.',2);
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

Procedure Help;
Begin
 WriteLn('FRMDIFF - Cette commande permet de comparer des rep'+#138+'res de r'+#130+'f'+#130+'rence.');
 WriteLn;
 WriteLn('Syntaxe : FRMDIFF -f1 <repere1> [-f2 <repere2>] -b <debut> [options]');
 WriteLn;
 WriteLn('Options :');
 WriteLn('  -f1 <repere>   Premier repere (requis)');
 WriteLn('  -f2 <repere>   Second repere (J2000 par defaut)');
 WriteLn('  -b <temps>     Epoque de debut (requise)');
 WriteLn('  -e <temps>     Epoque de fin (egale au debut par defaut)');
 WriteLn('  -s <secondes>  Pas entre les epoques (secondes SI)');
 WriteLn('  -n <nombre>    Nombre d''epoques (11 par defaut si debut < fin)');
 WriteLn('  -t <type>      Type de temps : UTC (defaut), ET ou ETS');
 WriteLn('  -d <sortie>    Detail par epoque : DUMPAA (angle et axe), DUMPM (matrice),');
 WriteLn('                 DUMPQS (quaternion SPICE) ou DUMPQO (quaternion usuel)');
 WriteLn('  -a <unite>     Unite des angles : RAD (defaut), DEG, ARCSEC ou MAS');
 WriteLn('  -p <chiffres>  Decimales significatives, 1 a 12 (9 par defaut)');
 WriteLn('  -l             Liste les reperes integres');
 WriteLn('  -h, /?         Aide (-help et --help acceptes)');
 WriteLn;
 WriteLn('Temps : AAAA-MM-JJ[THH:MM:SS[.fraction]] pour UTC et ET (entre');
 WriteLn('guillemets s''il y a une espace); ETS = secondes ET/TDB depuis J2000.');
 WriteLn('Sans -d, seules les statistiques de l''ecart angulaire sont affichees.');
 WriteLn('Axe et quaternions : rotation qui amene les axes du repere 1 sur ceux');
 WriteLn('du repere 2, axe exprime dans le repere 1.');
 WriteLn;
 WriteLn('Exemples :');
 WriteLn('  FRMDIFF -f1 J2000 -f2 ECLIPJ2000 -b 2000-01-01 -d DUMPAA');
 WriteLn('  FRMDIFF -f1 IAU_MOON -b 1969-07-20T20:17:40 -a ARCSEC');
 WriteLn('  FRMDIFF -f1 IAU_EARTH -f2 IAU_MOON -b 2024-01-01 -e 2024-01-02 -s 3600');
 WriteLn;
 WriteLn('Limites : aucun kernel lu (-k, -k1, -k2 refuses); reperes integres');
 WriteLn('seulement. UTC apres 2017 : dernier offset connu prolonge (37 s).');
 WriteLn('Codes retour : 0 succes, 1 erreur de donnees, 2 syntaxe.');
End;

Procedure ListFrames;
Var K: Integer;
Begin
 WriteLn('Reperes integres :');
 For K := 1 To FrameCount Do WriteLn('  ',FrameNames[K]);
 WriteLn('IAU_* : modele IAU 2009; periodiques seulement pour Lune, Mercure');
 WriteLn('et Neptune.');
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

{ Secondes depuis J2000 (midi) du debut du jour; temporaire Double pour
  eviter qu'un produit entier*constante reelle soit calcule en simple precision. }
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

Function Number(N: LongInt; Width: Integer): String;
Var S: String;
Begin
 Str(N,S);
 While Length(S) < Width Do S := '0'+S;
 Number := S;
End;

Function Calendar(Seconds: Double; LeapSecond: Boolean): String;
Var Day, L, N, Y, M, D, H, Minute, Sec, MS: LongInt;
    R: Double;
Begin
 R := Seconds+43200.0;
 If (Seconds < MinEpoch) Or (Seconds >= MaxEpoch) Then
  Fail('Date hors plage du calendrier (0001-9999). Utiliser -t ETS.',1);
 Day := Trunc(R/86400.0);
 If R < Day*86400.0 Then Dec(Day);
 R := R-Day*86400.0;
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
 Calendar := Number(Y,4)+'-'+Number(M,2)+'-'+Number(D,2)+'T'+
  Number(H,2)+':'+Number(Minute,2)+':'+Number(Sec,2)+'.'+Number(MS Mod 1000,3);
End;

Function ETtoUTCText(ET: Double): String;
Var Mean, TAI, U, Threshold, DaySeconds, Millis: Double; K, Delta: Integer;
    IsLeap: Boolean;
Begin
 If (ET < MinEpoch) Or (ET >= MaxEpoch) Then
  Fail('Date hors plage du calendrier (0001-9999). Utiliser -t ETS.',1);
 Mean := 6.239996+1.99096871E-7*ET;
 TAI := ET-32.184-0.001657*Sin(Mean+0.01671*Sin(Mean));
 { Arrondir sur l'echelle TAI continue avant de classer les secondes intercalaires. }
 DaySeconds := Int(TAI/86400.0)*86400.0;
 Millis := Round((TAI-DaySeconds)*1000.0);
 TAI := DaySeconds+Millis/1000.0;
 If TAI < LeapEpoch[1]+10 Then Fail('UTC avant 1972 non pris en charge.',1);
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
 ETtoUTCText := Calendar(U,IsLeap);
End;

Function TimeText(ET: Double): String;
Var S: String;
Begin
 If TimeMode = 'ETS' Then Begin
  Str(ET:0:6,S);
  TimeText := S;
 End Else If TimeMode = 'ET' Then TimeText := Calendar(ET,False)
 Else TimeText := ETtoUTCText(ET);
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

Function ReadTime(S: String; What: String): Double;
Var V: Double; Extra: Boolean;
Begin
 If TimeMode = 'ETS' Then V := RealNumber(S,What)
 Else Begin
  V := ParseCalendar(S,Extra);
  If TimeMode = 'UTC' Then V := UTCtoET(V,Extra)
  Else If Extra Then SyntaxError('Seconde 60 reservee a UTC.');
 End;
 If (V < MinEpoch) Or (V >= MaxEpoch) Then
  SyntaxError('Epoque hors plage du calendrier (0001-9999) pour '+What+'.');
 ReadTime := V;
End;

{ ------------------------------------------------------------------ }
{ Matrices de rotation et orientation des reperes                    }
{ ------------------------------------------------------------------ }

Procedure Ident(Var M: TMat);
Var R, C: Integer;
Begin
 For R := 1 To 3 Do For C := 1 To 3 Do
  If R = C Then M[R,C] := 1 Else M[R,C] := 0;
End;

{ Matrices de changement de coordonnees (rotation du repere). }
Procedure RotX(A: Double; Var M: TMat);
Begin
 Ident(M);
 M[2,2] := Cos(A); M[2,3] := Sin(A);
 M[3,2] := -Sin(A); M[3,3] := Cos(A);
End;

Procedure RotZ(A: Double; Var M: TMat);
Begin
 Ident(M);
 M[1,1] := Cos(A); M[1,2] := Sin(A);
 M[2,1] := -Sin(A); M[2,2] := Cos(A);
End;

Procedure Mul(Var A, B, C: TMat);
Var R, K, L: Integer; T: TMat; S: Double;
Begin
 For R := 1 To 3 Do For K := 1 To 3 Do Begin
  S := 0;
  For L := 1 To 3 Do S := S+A[R,L]*B[L,K];
  T[R,K] := S;
 End;
 C := T;
End;

Procedure Transpose(Var A, B: TMat);
Var R, K: Integer; T: TMat;
Begin
 For R := 1 To 3 Do For K := 1 To 3 Do T[R,K] := A[K,R];
 B := T;
End;

Function Degrees360(A: Double): Double;
Begin
 Degrees360 := (A-360.0*Int(A/360.0))*Pi/180.0;
End;

{ Pole (alpha0, delta0) et meridien W en degres, ET en secondes TDB. }
Procedure PoleOf(Idx: Integer; ET: Double; Var RA, Dec, W: Double);
Var D, T, N: Double; K: Integer; E: Array[1..13] Of Double;
Begin
 D := ET/86400.0; T := D/36525.0;
 Case Idx Of
  5: Begin RA := 286.13; Dec := 63.87; W := 84.176+14.1844000*D End;
  6: Begin
   RA := 281.0097-0.0328*T; Dec := 61.4143-0.0049*T;
   W := 329.5469+6.1385025*D;
   For K := 1 To 5 Do
    W := W+MercW[K]*Sin(Degrees360(MercAngle0[K]+MercRate[K]*D));
  End;
  7: Begin RA := 272.76; Dec := 67.16; W := 160.20-1.4813688*D End;
  8: Begin RA := 0.0-0.641*T; Dec := 90.0-0.557*T; W := 190.147+360.9856235*D End;
  9: Begin
   RA := 269.9949+0.0031*T; Dec := 66.5392+0.0130*T;
   W := 38.3213+13.17635815*D-1.4E-12*D*D;
   For K := 1 To 13 Do E[K] := Degrees360(MoonAngle0[K]+MoonRate[K]*D);
   For K := 1 To 13 Do Begin
    RA := RA+MoonRA[K]*Sin(E[K]);
    Dec := Dec+MoonDec[K]*Cos(E[K]);
    W := W+MoonW[K]*Sin(E[K]);
   End;
  End;
  10: Begin RA := 317.68143-0.1061*T; Dec := 52.88650-0.0609*T; W := 176.630+350.89198226*D End;
  11: Begin RA := 268.056595-0.006499*T; Dec := 64.495303+0.002413*T; W := 284.95+870.5366420*D End;
  12: Begin RA := 40.589-0.036*T; Dec := 83.537-0.004*T; W := 38.90+810.7939024*D End;
  13: Begin RA := 257.311; Dec := -15.175; W := 203.81-501.1600928*D End;
  14: Begin
   N := Degrees360(357.85+52.316*T);
   RA := 299.36+0.70*Sin(N); Dec := 43.46-0.51*Cos(N);
   W := 253.18+536.3128492*D-0.48*Sin(N);
  End;
  15: Begin RA := 132.993; Dec := -6.163; W := 302.695+56.3625225*D End;
 End;
End;

{ Matrice J2000 vers repere Idx a l'epoque ET. }
Procedure FrameMatrix(Idx: Integer; ET: Double; Var M: TMat);
Var RA, Dec, W: Double; A, B, C: TMat; R, K: Integer;
Begin
 If Idx = 1 Then Ident(M)
 Else If Idx = 2 Then RotX(Obliquity,M)
 Else If Idx = 3 Then Begin
  For R := 1 To 3 Do For K := 1 To 3 Do M[R,K] := B1950Matrix[R,K];
 End Else If Idx = 4 Then Begin
  For R := 1 To 3 Do For K := 1 To 3 Do M[R,K] := GalacticMatrix[R,K];
 End Else Begin
  PoleOf(Idx,ET,RA,Dec,W);
  RotZ(Degrees360(W),A);
  RotX(Pi/2-Dec*Pi/180.0,B);
  RotZ(Pi/2+Degrees360(RA),C);
  Mul(B,C,B);
  Mul(A,B,M);
 End;
End;

Function Atan2(Y, X: Double): Double;
Begin
 If X > 0 Then Atan2 := ArcTan(Y/X)
 Else If X < 0 Then Begin
  If Y >= 0 Then Atan2 := ArcTan(Y/X)+Pi Else Atan2 := ArcTan(Y/X)-Pi;
 End Else Begin
  If Y > 0 Then Atan2 := Pi/2
  Else If Y < 0 Then Atan2 := -Pi/2
  Else Atan2 := 0;
 End;
End;

{ Angle (0..Pi) et axe de la rotation directe R = D^T. }
Procedure Analyse(Var D: TMat; Var Ang: Double; Var Ax: TVec);
Var S, C, N, X, Y, Z, Dot: Double; K, L: Integer;
Begin
 X := D[2,3]-D[3,2]; Y := D[3,1]-D[1,3]; Z := D[1,2]-D[2,1];
 N := Sqrt(X*X+Y*Y+Z*Z);
 S := N/2;
 C := (D[1,1]+D[2,2]+D[3,3]-1.0)/2.0;
 Ang := Atan2(S,C);
 Ax[1] := 0; Ax[2] := 0; Ax[3] := 0;
 If Ang < 1.0E-15 Then Exit;
 If (C > 0) Or (S > 1.0E-6) Then Begin
  Ax[1] := X/N; Ax[2] := Y/N; Ax[3] := Z/N;
 End Else Begin
  { Voisinage de Pi : l'axe vient de la partie symetrique. }
  K := 1;
  If D[2,2] > D[K,K] Then K := 2;
  If D[3,3] > D[K,K] Then K := 3;
  Ax[K] := Sqrt(Abs((D[K,K]-C)/(1.0-C)));
  For L := 1 To 3 Do
   If L <> K Then Ax[L] := (D[L,K]+D[K,L])/(2.0*(1.0-C)*Ax[K]);
  N := Sqrt(Ax[1]*Ax[1]+Ax[2]*Ax[2]+Ax[3]*Ax[3]);
  Dot := Ax[1]*X+Ax[2]*Y+Ax[3]*Z;
  For L := 1 To 3 Do Begin
   Ax[L] := Ax[L]/N;
   If Dot < 0 Then Ax[L] := -Ax[L];
  End;
 End;
End;

{ ------------------------------------------------------------------ }
{ Affichage                                                          }
{ ------------------------------------------------------------------ }

{ Notation scientifique d.ddddE+XX avec Digits decimales. }
Function Sci(V: Double): String;
Var E, K: Integer; A, P: Double; S, Mant, ES: String; Negative: Boolean;
Begin
 Negative := V < 0;
 A := Abs(V);
 E := 0;
 If A = 0 Then Begin
  Mant := '0';
  For K := 1 To Digits Do Mant := Mant+'0';
 End Else Begin
  While A >= 10.0 Do Begin A := A/10.0; Inc(E) End;
  While A < 1.0 Do Begin A := A*10.0; Dec(E) End;
  P := 1.0;
  For K := 1 To Digits Do P := P*10.0;
  Str(Int(A*P+0.5):0:0,Mant);
  If Length(Mant) > Digits+1 Then Begin
   Inc(E);
   Mant := '1';
   For K := 1 To Digits Do Mant := Mant+'0';
  End;
 End;
 S := Copy(Mant,1,1)+'.'+Copy(Mant,2,Digits);
 If E < 0 Then ES := '-' Else ES := '+';
 ES := ES+Number(Abs(E),2);
 If Negative Then S := '-'+S;
 Sci := S+'E'+ES;
End;

Function Col(V: Double): String;
Begin
 Col := Pad(Sci(V),Digits+8);
End;

Procedure ShowEpoch(ET: Double; Var D: TMat; Ang: Double; Var Ax: TVec);
Var R: Integer; Half, Q0, S: Double; Sign: Integer;
Begin
 If DumpMode = 'DUMPAA' Then
  WriteLn(TimeText(ET),' ',Col(Ang*AngleFactor),' ',Col(Ax[1]),' ',Col(Ax[2]),' ',Col(Ax[3]))
 Else If DumpMode = 'DUMPM' Then Begin
  WriteLn(TimeText(ET));
  For R := 1 To 3 Do WriteLn('  ',Col(D[R,1]),' ',Col(D[R,2]),' ',Col(D[R,3]));
 End Else If (DumpMode = 'DUMPQS') Or (DumpMode = 'DUMPQO') Then Begin
  Half := Ang/2.0;
  Q0 := Cos(Half); S := Sin(Half);
  If DumpMode = 'DUMPQS' Then Sign := 1 Else Sign := -1;
  WriteLn(TimeText(ET),' ',Col(Q0),' ',Col(Sign*S*Ax[1]),' ',Col(Sign*S*Ax[2]),
          ' ',Col(Sign*S*Ax[3]));
 End;
End;

Function UnitLabel: String;
Begin
 If AngleName = 'DEG' Then UnitLabel := 'degres'
 Else If AngleName = 'ARCSEC' Then UnitLabel := 'secondes d''arc'
 Else If AngleName = 'MAS' Then UnitLabel := 'millisecondes d''arc'
 Else UnitLabel := 'radians';
End;

Function OptionValue: String;
Begin
 Inc(ArgIndex);
 If ArgIndex > ParamCount Then SyntaxError('Valeur manquante pour '+Arg+'.');
 OptionValue := ParamStr(ArgIndex);
End;

Function FrameIndex(Name: String): Integer;
Var K, Found: Integer;
Begin
 Name := Upper(Trim(Name));
 Found := 0;
 For K := 1 To FrameCount Do
  If FrameNames[K] = Name Then Found := K;
 If Found = 0 Then
  SyntaxError('Repere inconnu ou sans kernel : '+Name+'. Voir FRMDIFF -l.');
 FrameIndex := Found;
End;

Begin
 TimeMode := 'UTC'; DumpMode := ''; AngleName := 'RAD'; Digits := 9;
 HasStep := False; HasPoints := False; HasStop := False;
 HasFrame1 := False; HasFrame2 := False; StartText := ''; StopText := '';
 Frame2 := 1; Step := 0; Points := 0; Start := 0;
 For I := 1 To LeapCount Do
  LeapEpoch[I] := DayStart(LeapYears[I],LeapMonths[I],1);
 If ParamCount = 0 Then Begin Help; Halt(0) End;
 For ArgIndex := 1 To ParamCount Do Begin
  Arg := Upper(ParamStr(ArgIndex));
  If (Arg = '-H') Or (Arg = '-HELP') Or (Arg = '--HELP') Or (Arg = '/?') Then Begin
   Help; Halt(0);
  End;
  If Arg = '-L' Then Begin ListFrames; Halt(0) End;
 End;
 ArgIndex := 1;
 While ArgIndex <= ParamCount Do Begin
  Arg := Upper(ParamStr(ArgIndex));
  If Arg = '-F1' Then Begin Frame1 := FrameIndex(OptionValue); HasFrame1 := True End
  Else If Arg = '-F2' Then Begin Frame2 := FrameIndex(OptionValue); HasFrame2 := True End
  Else If Arg = '-B' Then StartText := OptionValue
  Else If Arg = '-E' Then Begin StopText := OptionValue; HasStop := True End
  Else If Arg = '-S' Then Begin Step := RealNumber(OptionValue,'-s'); HasStep := True End
  Else If Arg = '-N' Then Begin
   Step := RealNumber(OptionValue,'-n');
   If (Step < 1) Or (Step > MaxPoints) Or (Step <> Int(Step)) Then
    SyntaxError('-n doit etre un entier de 1 a 10000000.');
   Points := Trunc(Step); Step := 0; HasPoints := True;
  End
  Else If Arg = '-T' Then Begin
   TimeMode := Upper(Trim(OptionValue));
   If (TimeMode <> 'UTC') And (TimeMode <> 'ET') And (TimeMode <> 'ETS') Then
    SyntaxError('Type de temps inconnu : '+TimeMode+' (UTC, ET ou ETS).');
  End
  Else If Arg = '-D' Then Begin
   DumpMode := Upper(Trim(OptionValue));
   If (DumpMode <> 'DUMPAA') And (DumpMode <> 'DUMPM') And
      (DumpMode <> 'DUMPQS') And (DumpMode <> 'DUMPQO') Then
    SyntaxError('Sortie inconnue : '+DumpMode+'.');
  End
  Else If Arg = '-A' Then Begin
   AngleName := Upper(Trim(OptionValue));
   If (AngleName <> 'RAD') And (AngleName <> 'DEG') And
      (AngleName <> 'ARCSEC') And (AngleName <> 'MAS') Then
    SyntaxError('Unite d''angle inconnue : '+AngleName+'.');
  End
  Else If Arg = '-P' Then Begin
   Digits := Trunc(RealNumber(OptionValue,'-p'));
   If (Digits < 1) Or (Digits > 12) Then SyntaxError('-p doit etre de 1 a 12.');
  End
  Else If (Arg = '-K') Or (Arg = '-K1') Or (Arg = '-K2') Then
   SyntaxError('Les kernels ne sont pas pris en charge : reperes integres seulement.')
  Else SyntaxError('Option inconnue ou non prise en charge : '+ParamStr(ArgIndex));
  Inc(ArgIndex);
 End;
 If Not HasFrame1 Then SyntaxError('-f1 est requis.');
 If StartText = '' Then SyntaxError('-b est requis.');
 If HasStep And HasPoints Then SyntaxError('-s et -n sont incompatibles.');
 If HasStep And (Step <= 0) Then SyntaxError('-s doit etre positif.');
 If AngleName = 'DEG' Then AngleFactor := 180.0/Pi
 Else If AngleName = 'ARCSEC' Then AngleFactor := 180.0*3600.0/Pi
 Else If AngleName = 'MAS' Then AngleFactor := 180.0*3600000.0/Pi
 Else AngleFactor := 1.0;
 Start := ReadTime(StartText,'-b');
 If HasStop Then Stop := ReadTime(StopText,'-e') Else Stop := Start;
 If Stop < Start Then SyntaxError('-e doit etre posterieur ou egal a -b.');
 If Stop = Start Then Begin
  If HasStep Or (Points > 1) Then SyntaxError('-s et -n exigent -e posterieur a -b.');
  Points := 1; Step := 0;
 End Else If HasStep Then Begin
  If (Stop-Start)/Step > MaxPoints-1 Then SyntaxError('Trop d''epoques (maximum 10000000).');
  Points := Trunc((Stop-Start)/Step+1.0E-9)+1;
 End Else Begin
  If Points = 0 Then Points := 11;
  If Points = 1 Then Step := 0 Else Step := (Stop-Start)/(Points-1);
 End;

 WriteLn('FRMDIFF - comparaison d''orientations de reperes');
 WriteLn('Repere 1   : ',FrameNames[Frame1]);
 WriteLn('Repere 2   : ',FrameNames[Frame2]);
 If TimeMode = 'ETS' Then WriteLn('Temps       : ET/TDB, secondes depuis J2000')
 Else If TimeMode = 'ET' Then WriteLn('Temps       : ET/TDB, calendrier')
 Else WriteLn('Temps       : UTC (table NAIF0012 integree, 37 s apres 2017)');
 WriteLn('Epoques     : ',Points,', de ',TimeText(Start),' a ',TimeText(Start+Step*(Points-1)));
 If Points > 1 Then WriteLn('Pas          : ',Step:0:6,' s');
 WriteLn('Angles      : ',UnitLabel);
 WriteLn('Rotation    : axes du repere 1 vers axes du repere 2, axe dans le repere 1');
 WriteLn;
 If DumpMode = 'DUMPAA' Then
  WriteLn(Pad('Epoque',Length(TimeText(Start))),' ',Pad('Angle',Digits+8),' ',
          Pad('Axe X',Digits+8),' ',Pad('Axe Y',Digits+8),' ',Pad('Axe Z',Digits+8))
 Else If (DumpMode = 'DUMPQS') Or (DumpMode = 'DUMPQO') Then
  WriteLn(Pad('Epoque',Length(TimeText(Start))),' ',Pad('Q0',Digits+8),' ',
          Pad('Q1',Digits+8),' ',Pad('Q2',Digits+8),' ',Pad('Q3',Digits+8))
 Else If DumpMode = 'DUMPM' Then
  WriteLn('Epoque, puis matrice D (coordonnees repere 1 vers repere 2)');

 MinAngle := 0; MaxAngle := 0; MinET := Start; MaxET := Start;
 SumAngle := 0; SumSquare := 0;
 MaxAxis[1] := 0; MaxAxis[2] := 0; MaxAxis[3] := 0;
 For Index := 0 To Points-1 Do Begin
  ET := Start+Index*Step;
  FrameMatrix(Frame1,ET,M1);
  FrameMatrix(Frame2,ET,M2);
  Transpose(M1,T1);
  Mul(M2,T1,D);
  Analyse(D,Angle,Axis);
  If DumpMode <> '' Then ShowEpoch(ET,D,Angle,Axis);
  If (Index = 0) Or (Angle < MinAngle) Then Begin MinAngle := Angle; MinET := ET End;
  If (Index = 0) Or (Angle > MaxAngle) Then Begin
   MaxAngle := Angle; MaxET := ET; MaxAxis := Axis;
  End;
  SumAngle := SumAngle+Angle;
  SumSquare := SumSquare+Angle*Angle;
 End;

 If DumpMode <> '' Then WriteLn;
 WriteLn('Statistiques de l''ecart angulaire (',UnitLabel,') :');
 WriteLn('  Minimum : ',Sci(MinAngle*AngleFactor),'  a ',TimeText(MinET));
 WriteLn('  Maximum : ',Sci(MaxAngle*AngleFactor),'  a ',TimeText(MaxET));
 WriteLn('  Moyenne : ',Sci(SumAngle/Points*AngleFactor));
 WriteLn('  Quadratique : ',Sci(Sqrt(SumSquare/Points)*AngleFactor));
 If MaxAngle < 1.0E-12 Then
  WriteLn('Orientations identiques a la precision machine.')
 Else
  WriteLn('  Axe au maximum : ',Col(MaxAxis[1]),' ',Col(MaxAxis[2]),' ',Col(MaxAxis[3]));
End.
