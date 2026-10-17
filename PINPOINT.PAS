{ @author: Sylvain Maltais (support@gladir.com)
 @created: 2026
 @website(https://www.gladir.com/space-0)
 @abstract(Target: Turbo Pascal 7, Free Pascal 3.2) }

Program PINPOINT;

{$A-}
{$E+,N+}

{ Construction d'un fichier SPK pour des objets a position fixe ou a vitesse
  constante par rapport a leur centre, sous-ensemble autonome de PINPOINT
  NAIF : aucune unite, aucune inclusion.
  Entree : fichier de definitions (kernel texte, sections \begindata) :
    SITES = ( 'NOM1', 'NOM2' )
    <NOM>_CENTER, <NOM>_FRAME, <NOM>_IDCODE   (requis)
    <NOM>_XYZ (km) ou <NOM>_LATLON (latitude, longitude, altitude)
    <NOM>_VELOCITY (km/s) avec <NOM>_EPOCH, <NOM>_BOUNDS = ( debut, fin ),
    <NOM>_UNITS = ( 'DEGREES|RADIANS', 'KILOMETERS|METERS' ),
    <NOM>_UP, <NOM>_NORTH (axes du repere topocentrique, 'Z' et 'X' par defaut).
  LATLON (latitude planetodetique, longitude est, altitude au-dessus de
  l'ellipsoide) exige les rayons BODYnnn_RADII d'un PCK texte (-pck).
  Sortie : un segment SPK de type 8 (Lagrange, 2 etats, degre 1) par site,
  le debut et la fin etant les limites BOUNDS (1950-JAN-01 a 2050-JAN-01 TDB
  par defaut); un fichier FK de reperes topocentriques (-fk) facultatif.
  Dates : @AAAA-MOIS-JJ[/HH:MM:SS[.f]] ou nombre de secondes ET depuis J2000;
  les dates @ sont lues en UTC (table NAIF0012 integree, 1972-2017, puis
  37 s), ou en TDB avec -tdb.
  Reperes : J2000, B1950, ECLIPJ2000, GALACTIC, IAU_<corps>, ITRF93 et
  FRAME_<nom> = code des FK donnes avec -frames.
  Sources : naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/ug/pinpoint.html
            naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/spk.html
            naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/kernel.html
  Compilation : fpc -Mtp PINPOINT.PAS
  Limites DOS : 100 sites, 1100 variables de kernel, lignes de 255 caracteres. }

Const
 MaxSites = 100;
 MaxVars = 1100;
 MaxNums = 3000;
 MaxStrs = 1200;
 MaxFrames = 100;
 MaxTok = 120;
 LeapCount = 28;
 LeapYears: Array[1..LeapCount] Of Integer =
  (1972,1972,1973,1974,1975,1976,1977,1978,1979,1980,1981,1982,
   1983,1985,1988,1990,1991,1992,1993,1994,1996,1997,1999,2006,
   2009,2012,2015,2017);
 LeapMonths: Array[1..LeapCount] Of Integer =
  (1,7,1,1,1,1,1,1,1,1,7,7,7,7,1,1,1,7,7,7,1,7,1,1,1,7,7,1);
 MonthNames: Array[1..12] Of String[3] =
  ('JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC');
 MinEpoch = -63082324800.0;
 MaxEpoch = 252455572800.0;
 FtpString: String[28] =
  'FTPSTR:'#13':'#10':'#13#10':'#13#0':'#129':'#16#206':ENDFTP';
 BuiltinFrameCount = 33;
 BuiltinNames: Array[1..BuiltinFrameCount] Of String[16] =
  ('J2000','B1950','FK4','GALACTIC','MARSIAU','ECLIPJ2000','ECLIPB1950',
   'IAU_SUN','IAU_MERCURY','IAU_VENUS','IAU_EARTH','IAU_MARS','IAU_JUPITER',
   'IAU_SATURN','IAU_URANUS','IAU_NEPTUNE','IAU_PLUTO','IAU_MOON',
   'IAU_PHOBOS','IAU_DEIMOS','IAU_IO','IAU_EUROPA','IAU_GANYMEDE',
   'IAU_CALLISTO','IAU_ENCELADUS','IAU_TITAN','IAU_CHARON','IAU_EROS',
   'IAU_VESTA','IAU_ITOKAWA','IAU_CERES','IAU_BENNU','ITRF93');
 BuiltinCodes: Array[1..BuiltinFrameCount] Of LongInt =
  (1,2,3,13,16,17,18,10010,10011,10012,10013,10014,10015,10016,10017,10018,
   10019,10020,10021,10022,10023,10024,10025,10026,10040,10044,10079,10085,
   10099,10100,10101,10106,13000);
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
 TRecord = Array[0..1023] Of Byte;
 TBytes8 = Array[0..7] Of Byte;
 TVar = Record
  Name: String[47];
  Kind: Integer;
  First, Count: Integer;
 End;
 TVarArray = Array[1..MaxVars] Of TVar;
 TNumArray = Array[1..MaxNums] Of Double;
 TFlagArray = Array[1..MaxNums] Of Boolean;
 TStrArray = Array[1..MaxStrs] Of String[40];
 TSite = Record
  Name: String[40];
  Body, Center, Frame: LongInt;
  FrameName: String[40];
  P0, P1: Array[1..6] Of Double;
  T0, T1: Double;
  Lat, Lon: Double;
  Up, North: String[2];
 End;
 TFrameName = String[40];
 TTokens = Array[1..MaxTok] Of String[47];

Var
 LeapEpoch: Array[1..LeapCount] Of Double;
 SegStart, SegEnd: Array[1..MaxSites] Of LongInt;
 Vars: ^TVarArray;
 Nums: ^TNumArray;
 IsDate: ^TFlagArray;
 Strs: ^TStrArray;
 NVars, NNums, NStrs: Integer;
 ExtraFrameName: Array[1..MaxFrames] Of TFrameName;
 ExtraFrameCode: Array[1..MaxFrames] Of LongInt;
 NExtra: Integer;
 Sites: Array[1..MaxSites] Of TSite;
 NSites: Integer;
 TF: Text;
 TLine: String;
 TPos: Integer;
 TInData: Boolean;
 TPath: String;
 Filter: Integer;
 DefName, SpkName, FkName, Arg: String;
 PckNames, FrameFiles: Array[1..8] Of Integer;
 NPck, NFrameFiles, ArgIndex, I, J: Integer;
 TdbDates, Force, Quiet: Boolean;
 OutFile: File;
 OutOpen: Boolean;
 FkFile: Text;
 FkOpen: Boolean;
 NextRec: LongInt;
 Block: Array[0..127] Of Double;
 BlockFill: Integer;
 WordAddr: LongInt;

Procedure Cleanup(KeepOutput: Boolean);
Var Err: Word;
Begin
 If OutOpen Then Begin
  {$I-} Close(OutFile); Err := IOResult; {$I+}
  OutOpen := False;
  If Not KeepOutput Then Begin
   {$I-} Erase(OutFile); Err := IOResult; {$I+}
  End;
 End;
 If FkOpen Then Begin
  {$I-} Close(FkFile); Err := IOResult; {$I+}
  FkOpen := False;
  If Not KeepOutput Then Begin
   {$I-} Erase(FkFile); Err := IOResult; {$I+}
  End;
 End;
End;

Procedure Fail(S: String; Code: Integer);
Begin
 WriteLn('Erreur : ',S);
 Cleanup(False);
 Halt(Code);
End;

Procedure SyntaxError(S: String);
Begin
 Fail(S+' Voir PINPOINT -h.',2);
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

Function Real12(V: Double): String;
Var S: String;
Begin
 Str(V:0:12,S);
 Real12 := S;
End;

Procedure Help;
Begin
 WriteLn('PINPOINT - Cette commande permet de construire un fichier SPK pour des ',
         'objets '+#133+' position fixe ou '+#133+' vitesse constante relative '+#133+' leur centre.');
 WriteLn;
 WriteLn('Syntaxe : PINPOINT -def <definitions> -spk <fichier.bsp> [options]');
 WriteLn;
 WriteLn('Options :');
 WriteLn('  -def <fichier>     Fichier de definitions des sites (kernel texte, requis)');
 WriteLn('  -spk <fichier>     Fichier SPK a creer (requis)');
 WriteLn('  -pck <fichier>     PCK texte donnant BODYnnn_RADII (pour LATLON, FK)');
 WriteLn('  -frames <fichier>  FK texte dont les FRAME_<nom> = code sont connus');
 WriteLn('                     (jusqu''a 8 fois)');
 WriteLn('  -fk <fichier>      Ecrit aussi un FK de reperes topocentriques <SITE>_TOPO');
 WriteLn('  -tdb               Dates @ lues en TDB au lieu d''UTC');
 WriteLn('  -f                 Ecrase les fichiers de sortie existants');
 WriteLn('  -q                 Silencieux (aucun message de succes)');
 WriteLn('  -h, /?             Aide (-help et --help acceptes)');
 WriteLn;
 WriteLn('Exemple de fichier de definitions :');
 WriteLn('  \begindata');
 WriteLn('  SITES              = ( ''STATION1'' )');
 WriteLn('  STATION1_CENTER    = 399');
 WriteLn('  STATION1_FRAME     = ''ITRF93''');
 WriteLn('  STATION1_IDCODE    = 399001');
 WriteLn('  STATION1_LATLON    = ( 35.4 -116.9 1.0 )');
 WriteLn('  STATION1_BOUNDS    = ( @1990-JAN-01, @2030-JAN-01 )');
 WriteLn('  \begintext');
 WriteLn;
 WriteLn('Segments SPK de type 8, degre 1. Aucun kernel LSK requis (UTC integre).');
 WriteLn('Codes retour : 0 succes, 1 erreur de lecture/donnees, 2 syntaxe.');
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

Function ParseInt(S: String; Var V: LongInt): Boolean;
Var Code: Integer;
Begin
 ParseInt := False;
 If (S = '') Or (Length(S) > 9) Then Exit;
 Val(S,V,Code);
 ParseInt := Code = 0;
End;

Function NextField(Var S: String; Sep: Char): String;
Var P: Integer;
Begin
 P := Pos(Sep,S);
 If P = 0 Then Begin NextField := S; S := '' End
 Else Begin NextField := Copy(S,1,P-1); Delete(S,1,P) End;
End;

{ "@AAAA-MOIS-JJ[/HH:MM:SS[.f]]" (apres le @) : secondes depuis J2000 midi de
  l'echelle calendrier (sans conversion d'echelle de temps). }
Function ParseAt(S: String; Var V: Double): Boolean;
Var DatePart, TimePart, F: String; Y, M, D, H, Mi, K: LongInt; Sec: Double;
    P: Integer;
Begin
 ParseAt := False;
 P := Pos('/',S);
 If P = 0 Then P := Pos('T',S);
 If P = 0 Then Begin DatePart := S; TimePart := '' End
 Else Begin DatePart := Copy(S,1,P-1); TimePart := Copy(S,P+1,255) End;
 F := NextField(DatePart,'-'); If Not ParseInt(F,Y) Then Exit;
 F := NextField(DatePart,'-');
 M := 0;
 For K := 1 To 12 Do If F = MonthNames[K] Then M := K;
 If M = 0 Then If Not ParseInt(F,M) Then Exit;
 F := NextField(DatePart,'-'); If Not ParseInt(F,D) Then Exit;
 If DatePart <> '' Then Exit;
 If (Y < 1) Or (Y > 9999) Or (M < 1) Or (M > 12) Or (D < 1) Or
    (D > DaysInMonth(Y,M)) Then Exit;
 H := 0; Mi := 0; Sec := 0;
 If TimePart <> '' Then Begin
  F := NextField(TimePart,':'); If Not ParseInt(F,H) Then Exit;
  If TimePart <> '' Then Begin
   F := NextField(TimePart,':'); If Not ParseInt(F,Mi) Then Exit;
   If TimePart <> '' Then If Not ParseNumber(TimePart,Sec) Then Exit;
  End;
 End;
 If (H > 23) Or (Mi > 59) Or (Sec < 0) Or (Sec >= 60) Then Exit;
 V := DayStart(Y,M,D)+(H*3600.0+Mi*60.0)+Sec;
 ParseAt := True;
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

Function UTCtoET(U: Double): Double;
Var K, Offset: Integer;
Begin
 If U < LeapEpoch[1] Then
  Fail('Date UTC avant 1972 non prise en charge. Utiliser -tdb.',1);
 Offset := 10;
 For K := 2 To LeapCount Do If U >= LeapEpoch[K] Then Offset := K+9;
 UTCtoET := TAItoET(U+Offset);
End;

{ Valeur de date/temps d'une variable : @date en ET selon -tdb, nombre = ET. }
Function ToET(V: Double; Date: Boolean): Double;
Var R: Double;
Begin
 If Date And Not TdbDates Then R := UTCtoET(V) Else R := V;
 If (R < MinEpoch) Or (R >= MaxEpoch) Then Fail('Epoque hors plage du calendrier (0001-9999).',1);
 ToET := R;
End;

{ ------------------------------------------------------------------ }
{ Kernel texte                                                       }
{ ------------------------------------------------------------------ }

Function ReadTLine: Boolean;
Var Err: Word; T: String;
Begin
 ReadTLine := False;
 While Not EOF(TF) Do Begin
  {$I-} ReadLn(TF,TLine); Err := IOResult; {$I+}
  If Err <> 0 Then Fail('Erreur de lecture de '+TPath+'.',1);
  T := Upper(Trim(TLine));
  If T = '\BEGINDATA' Then TInData := True
  Else If T = '\BEGINTEXT' Then TInData := False
  Else If TInData Then Begin
   TPos := 1; ReadTLine := True; Exit;
  End;
 End;
End;

Function NextTok(Var T: String): Boolean;
Var C: Char;
Begin
 NextTok := False;
 T := '';
 While True Do Begin
  While (TPos <= Length(TLine)) And (TLine[TPos] In [' ',#9,',']) Do Inc(TPos);
  If TPos <= Length(TLine) Then Break;
  If Not ReadTLine Then Exit;
 End;
 C := TLine[TPos];
 If (C = '(') Or (C = ')') Or (C = '=') Then Begin
  T := C; Inc(TPos);
 End Else If (C = '+') And (TPos < Length(TLine)) And (TLine[TPos+1] = '=') Then Begin
  T := '+='; Inc(TPos,2);
 End Else If C = '''' Then Begin
  T := C; Inc(TPos);
  While (TPos <= Length(TLine)) And (TLine[TPos] <> '''') Do Begin
   T := T+TLine[TPos]; Inc(TPos);
  End;
  Inc(TPos);
  T := T+'''';
 End Else Begin
  While (TPos <= Length(TLine)) And Not (TLine[TPos] In [' ',#9,',','(',')','=']) Do Begin
   If Length(T) = 255 Then Fail('Jeton trop long dans '+TPath+'.',1);
   T := T+TLine[TPos]; Inc(TPos);
  End;
 End;
 T := Upper(T);
 NextTok := True;
End;

Function FindVar(Name: String): Integer;
Var K: Integer;
Begin
 FindVar := 0;
 For K := 1 To NVars Do
  If Vars^[K].Name = Name Then FindVar := K;
End;

{ Filter : 0 tout conserver (definitions), 1 seulement BODYnnn_RADII (PCK). }
Function Wanted(Name: String): Boolean;
Begin
 If Filter = 0 Then Wanted := True
 Else Wanted := (Copy(Name,1,4) = 'BODY') And (Copy(Name,Length(Name)-5,6) = '_RADII');
End;

Procedure AddNumber(V: Double; Date: Boolean);
Begin
 If NNums = MaxNums Then Fail('Trop de valeurs numeriques dans '+TPath+'.',1);
 Inc(NNums);
 Nums^[NNums] := V;
 IsDate^[NNums] := Date;
End;

Procedure AddString(S: String);
Begin
 If NStrs = MaxStrs Then Fail('Trop de chaines dans '+TPath+'.',1);
 Inc(NStrs);
 Strs^[NStrs] := Copy(S,2,Length(S)-2);
End;

{ Enregistre les jetons Tok[1..N] dans la variable Name ("=" : remplace,
  "+=" : ajoute). Les valeurs d'une variable sont contigues. }
Procedure Commit(Name: String; Fresh: Boolean; Var Tok: TTokens; N: Integer);
Var Idx, K, OldFirst, OldCount, Kind: Integer; V: Double; Quote: Char;
Begin
 Quote := Chr(39);
 Idx := FindVar(Name);
 If Idx = 0 Then Begin
  If NVars = MaxVars Then Fail('Plus de '+Num(MaxVars)+' variables dans '+TPath+'.',1);
  Inc(NVars); Idx := NVars;
  Vars^[Idx].Name := Name; Vars^[Idx].Kind := 0; Vars^[Idx].Count := 0;
  Vars^[Idx].First := 0;
  Fresh := True;
 End;
 Kind := 0;
 If N > 0 Then Begin
  If Tok[1][1] = Quote Then Kind := 2 Else Kind := 1;
  For K := 2 To N Do
   If (Tok[K][1] = Quote) <> (Kind = 2) Then
    Fail('Melange de chaines et de nombres pour '+Name+' dans '+TPath+'.',1);
 End;
 If Fresh Then Begin
  Vars^[Idx].Count := 0; Vars^[Idx].Kind := Kind;
  If Kind = 2 Then Vars^[Idx].First := NStrs+1 Else Vars^[Idx].First := NNums+1;
 End Else If N > 0 Then Begin
  If (Vars^[Idx].Count > 0) And (Vars^[Idx].Kind <> Kind) Then
   Fail('Melange de chaines et de nombres pour '+Name+' dans '+TPath+'.',1);
  OldFirst := Vars^[Idx].First; OldCount := Vars^[Idx].Count;
  Vars^[Idx].Kind := Kind;
  If Kind = 2 Then Begin
   Vars^[Idx].First := NStrs+1;
   For K := 0 To OldCount-1 Do Begin
    If NStrs = MaxStrs Then Fail('Trop de chaines dans '+TPath+'.',1);
    Inc(NStrs); Strs^[NStrs] := Strs^[OldFirst+K];
   End;
  End Else Begin
   Vars^[Idx].First := NNums+1;
   For K := 0 To OldCount-1 Do AddNumber(Nums^[OldFirst+K],IsDate^[OldFirst+K]);
  End;
 End;
 For K := 1 To N Do Begin
  If Kind = 2 Then AddString(Tok[K])
  Else If Tok[K][1] = '@' Then Begin
   If Not ParseAt(Copy(Tok[K],2,255),V) Then Fail('Date invalide pour '+Name+' : '+Tok[K],1);
   AddNumber(V,True);
  End Else Begin
   If Not ParseNumber(Tok[K],V) Then Fail('Valeur invalide pour '+Name+' : '+Tok[K],1);
   AddNumber(V,False);
  End;
  Inc(Vars^[Idx].Count);
 End;
End;

Procedure LoadKernel(Path: String);
Var Err: Word; Name, Op, T: String; N: Integer; Fresh, Keep: Boolean;
    Tok: ^TTokens;
Begin
 New(Tok);
 TPath := Path;
 Assign(TF,Path);
 FileMode := 0;
 {$I-} Reset(TF); Err := IOResult; {$I+}
 If Err <> 0 Then Fail('Impossible d''ouvrir '+Path+'.',1);
 TLine := ''; TPos := 1; TInData := False;
 While NextTok(Name) Do Begin
  If Not NextTok(Op) Then Fail('Affectation incomplete apres '+Name+' dans '+Path+'.',1);
  If (Op <> '=') And (Op <> '+=') Then Fail('"=" attendu apres '+Name+' dans '+Path+'.',1);
  Fresh := Op = '=';
  Keep := Wanted(Name);
  If Length(Name) > 47 Then Keep := False;
  If Not NextTok(T) Then Fail('Valeur manquante pour '+Name+' dans '+Path+'.',1);
  N := 0;
  If T = '(' Then Begin
   If Not NextTok(T) Then Fail('Parenthese non fermee pour '+Name+' dans '+Path+'.',1);
   While T <> ')' Do Begin
    If Keep Then Begin
     If N = MaxTok Then Fail('Liste trop longue pour '+Name+' (maximum '+Num(MaxTok)+').',1);
     Inc(N); Tok^[N] := Copy(T,1,47);
    End;
    If Not NextTok(T) Then Fail('Parenthese non fermee pour '+Name+' dans '+Path+'.',1);
   End;
  End Else If Keep Then Begin
   N := 1; Tok^[1] := Copy(T,1,47);
  End;
  If Keep Then Commit(Name,Fresh,Tok^,N);
 End;
 {$I-} Close(TF); Err := IOResult; {$I+}
 Dispose(Tok);
End;

Function NumCount(Name: String): Integer;
Var Idx: Integer;
Begin
 Idx := FindVar(Name);
 If (Idx = 0) Or (Vars^[Idx].Kind <> 1) Then NumCount := 0 Else NumCount := Vars^[Idx].Count;
End;

Function NumAt(Name: String; N: Integer): Double;
Begin
 NumAt := Nums^[Vars^[FindVar(Name)].First+N-1];
End;

Function DateAt(Name: String; N: Integer): Boolean;
Begin
 DateAt := IsDate^[Vars^[FindVar(Name)].First+N-1];
End;

Function StrCount(Name: String): Integer;
Var Idx: Integer;
Begin
 Idx := FindVar(Name);
 If (Idx = 0) Or (Vars^[Idx].Kind <> 2) Then StrCount := 0 Else StrCount := Vars^[Idx].Count;
End;

Function StrAt(Name: String; N: Integer): String;
Begin
 StrAt := Strs^[Vars^[FindVar(Name)].First+N-1];
End;

{ ------------------------------------------------------------------ }
{ Reperes, corps                                                     }
{ ------------------------------------------------------------------ }

Function FrameCode(Name: String; Var Code: LongInt): Boolean;
Var K: Integer;
Begin
 FrameCode := True;
 Name := Upper(Trim(Name));
 For K := 1 To BuiltinFrameCount Do
  If BuiltinNames[K] = Name Then Begin Code := BuiltinCodes[K]; Exit End;
 For K := 1 To NExtra Do
  If ExtraFrameName[K] = Name Then Begin Code := ExtraFrameCode[K]; Exit End;
 FrameCode := False;
End;

{ FRAME_<nom> = code dans un FK texte. }
Procedure LoadFrames(Path: String);
Var Err: Word; Name, Op, T: String; V: Double; Keep: Boolean; Rest: String; K: Integer;
Begin
 TPath := Path;
 Assign(TF,Path);
 FileMode := 0;
 {$I-} Reset(TF); Err := IOResult; {$I+}
 If Err <> 0 Then Fail('Impossible d''ouvrir '+Path+'.',1);
 TLine := ''; TPos := 1; TInData := False;
 While NextTok(Name) Do Begin
  If Not NextTok(Op) Then Fail('Affectation incomplete apres '+Name+' dans '+Path+'.',1);
  Keep := (Copy(Name,1,6) = 'FRAME_') And (Op = '=');
  Rest := Copy(Name,7,255);
  If Keep And (Rest <> '') And (Rest[1] In ['0'..'9','-']) Then Begin
   { FRAME_<code>_NAME, _CLASS... : pas une definition de nom. }
   For K := 1 To Length(Rest) Do If Rest[K] = '_' Then Keep := False;
  End;
  If Not NextTok(T) Then Fail('Valeur manquante pour '+Name+' dans '+Path+'.',1);
  If T = '(' Then Begin
   Keep := False;
   While T <> ')' Do
    If Not NextTok(T) Then Fail('Parenthese non fermee dans '+Path+'.',1);
  End Else If Keep And ParseNumber(T,V) And (V = Int(V)) And (Abs(V) < 2000000000.0) Then Begin
   If NExtra = MaxFrames Then Fail('Plus de '+Num(MaxFrames)+' reperes nommes.',1);
   Inc(NExtra);
   ExtraFrameName[NExtra] := Copy(Rest,1,40);
   ExtraFrameCode[NExtra] := Trunc(V);
  End;
 End;
 {$I-} Close(TF); Err := IOResult; {$I+}
End;

Function BodyFromText(S: String; Var ID: LongInt): Boolean;
Var Code, J: Integer; U: String;
Begin
 BodyFromText := True;
 U := Upper(Trim(S));
 Val(U,ID,Code);
 If (Code = 0) And (U <> '') Then Exit;
 For J := 1 To NameCount Do
  If NameText[J] = U Then Begin ID := NameCode[J]; Exit End;
 BodyFromText := False;
End;

{ ------------------------------------------------------------------ }
{ Geometrie                                                          }
{ ------------------------------------------------------------------ }

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

{ Latitude planetodetique, longitude, altitude -> x, y, z. }
Procedure GeodeticToRect(Lat, Lon, Alt, A, F: Double; Var X, Y, Z: Double);
Var E2, N, S, C: Double;
Begin
 E2 := F*(2.0-F);
 S := Sin(Lat); C := Cos(Lat);
 N := A/Sqrt(1.0-E2*S*S);
 X := (N+Alt)*C*Cos(Lon);
 Y := (N+Alt)*C*Sin(Lon);
 Z := (N*(1.0-E2)+Alt)*S;
End;

Procedure RectToGeodetic(X, Y, Z, A, F: Double; Var Lat, Lon: Double);
Var E2, P, N, S, H: Double; K: Integer;
Begin
 E2 := F*(2.0-F);
 Lon := Atan2(Y,X);
 P := Sqrt(X*X+Y*Y);
 If P < 1.0E-12*A Then Begin
  If Z >= 0 Then Lat := Pi/2 Else Lat := -Pi/2;
  Exit;
 End;
 Lat := Atan2(Z,P*(1.0-E2));
 For K := 1 To 12 Do Begin
  S := Sin(Lat);
  N := A/Sqrt(1.0-E2*S*S);
  H := P/Cos(Lat)-N;
  Lat := Atan2(Z,P*(1.0-E2*N/(N+H)));
 End;
End;

{ ------------------------------------------------------------------ }
{ Sites                                                              }
{ ------------------------------------------------------------------ }

Var
 RadA, RadF: Double;
 HaveRadii: Boolean;

{ Rayons du corps central pour LATLON et les reperes topocentriques. }
Function GetRadii(Center: LongInt; Var A, F: Double): Boolean;
Var Name: String; C: Double;
Begin
 GetRadii := False;
 Name := 'BODY'+Num(Center)+'_RADII';
 If NumCount(Name) < 3 Then Exit;
 A := NumAt(Name,1); C := NumAt(Name,3);
 If (A <= 0) Or (C <= 0) Or (C > A*1.0000001) Then
  Fail('Rayons invalides pour le corps '+Num(Center)+' (equatorial puis polaire).',1);
 F := (A-C)/A;
 GetRadii := True;
End;

Function AxisText(S: String; Var Axis: Integer; Var Sign: Integer): Boolean;
Begin
 AxisText := False;
 S := Upper(Trim(S));
 Sign := 1;
 If (S <> '') And (S[1] = '-') Then Begin Sign := -1; Delete(S,1,1) End
 Else If (S <> '') And (S[1] = '+') Then Delete(S,1,1);
 If Length(S) <> 1 Then Exit;
 Axis := Pos(S[1],'XYZ');
 AxisText := Axis > 0;
End;

Procedure ReadSites;
Var
 K, N, Idx: Integer;
 Site, Key, S: String;
 DistScale, AngScale, V1, V2, V3, Alt, X, Y, Z, Ep: Double;
 Date: Boolean;
 Center: LongInt;
 HaveXYZ, HaveLat, HaveVel: Boolean;
 A1, A2, S1, S2: Integer;
Begin
 N := StrCount('SITES');
 If N = 0 Then Fail('SITES absent ou vide dans '+DefName+'.',1);
 If N > MaxSites Then Fail('Plus de '+Num(MaxSites)+' sites.',1);
 NSites := 0;
 For K := 1 To N Do Begin
  Site := StrAt('SITES',K);
  If Site = '' Then Fail('Nom de site vide dans SITES.',1);
  For Idx := 1 To NSites Do
   If Sites[Idx].Name = Site Then Fail('Site '+Site+' en double dans SITES.',1);
  Inc(NSites);
  FillChar(Sites[NSites],SizeOf(TSite),0);
  Sites[NSites].Name := Copy(Site,1,40);
  { Centre }
  Key := Site+'_CENTER';
  If StrCount(Key) >= 1 Then Begin
   If Not BodyFromText(StrAt(Key,1),Center) Then Fail('Centre inconnu pour '+Site+' : '+StrAt(Key,1),1);
  End Else If NumCount(Key) >= 1 Then Center := Trunc(NumAt(Key,1))
  Else Fail(Key+' est requis.',1);
  Sites[NSites].Center := Center;
  { Repere }
  Key := Site+'_FRAME';
  If StrCount(Key) < 1 Then Fail(Key+' est requis (nom de repere).',1);
  S := StrAt(Key,1);
  Sites[NSites].FrameName := Copy(S,1,40);
  If Not FrameCode(S,Sites[NSites].Frame) Then
   Fail('Repere inconnu pour '+Site+' : '+S+'. Utiliser -frames avec le FK du repere.',1);
  { Identificateur }
  Key := Site+'_IDCODE';
  If NumCount(Key) < 1 Then Fail(Key+' est requis (code entier).',1);
  V1 := NumAt(Key,1);
  If (V1 <> Int(V1)) Or (Abs(V1) > 2000000000.0) Then Fail(Key+' doit etre entier.',1);
  Sites[NSites].Body := Trunc(V1);
  If Sites[NSites].Body = Center Then Fail(Site+' : le corps et le centre sont identiques.',1);
  { Unites }
  AngScale := Pi/180.0; DistScale := 1.0;
  Key := Site+'_UNITS';
  For Idx := 1 To StrCount(Key) Do Begin
   S := StrAt(Key,Idx);
   If S = 'DEGREES' Then AngScale := Pi/180.0
   Else If S = 'RADIANS' Then AngScale := 1.0
   Else If S = 'KILOMETERS' Then DistScale := 1.0
   Else If S = 'METERS' Then DistScale := 0.001
   Else Fail(Site+'_UNITS : unite inconnue '+S+' (DEGREES, RADIANS, KILOMETERS, METERS).',1);
  End;
  { Position }
  HaveXYZ := NumCount(Site+'_XYZ') >= 3;
  HaveLat := NumCount(Site+'_LATLON') >= 3;
  If HaveXYZ = HaveLat Then Fail(Site+' : donner exactement _XYZ ou _LATLON (3 valeurs).',1);
  HaveRadii := GetRadii(Center,RadA,RadF);
  If HaveXYZ Then Begin
   X := NumAt(Site+'_XYZ',1)*DistScale; Y := NumAt(Site+'_XYZ',2)*DistScale;
   Z := NumAt(Site+'_XYZ',3)*DistScale;
  End Else Begin
   If Not HaveRadii Then
    Fail(Site+' : LATLON exige BODY'+Num(Center)+'_RADII (option -pck).',1);
   V1 := NumAt(Site+'_LATLON',1)*AngScale;
   V2 := NumAt(Site+'_LATLON',2)*AngScale;
   Alt := NumAt(Site+'_LATLON',3)*DistScale;
   If (Abs(V1) > Pi/2+1.0E-12) Then Fail(Site+' : latitude hors de -90..90 degres.',1);
   GeodeticToRect(V1,V2,Alt,RadA,RadF,X,Y,Z);
  End;
  { Latitude/longitude pour le repere topocentrique }
  If HaveLat Then Begin
   Sites[NSites].Lat := V1; Sites[NSites].Lon := V2;
  End Else If HaveRadii Then RectToGeodetic(X,Y,Z,RadA,RadF,Sites[NSites].Lat,Sites[NSites].Lon)
  Else RectToGeodetic(X,Y,Z,1.0,0.0,Sites[NSites].Lat,Sites[NSites].Lon);
  { Vitesse et epoque }
  HaveVel := NumCount(Site+'_VELOCITY') >= 3;
  V1 := 0; V2 := 0; V3 := 0; Ep := 0;
  If HaveVel Then Begin
   V1 := NumAt(Site+'_VELOCITY',1)*DistScale; V2 := NumAt(Site+'_VELOCITY',2)*DistScale;
   V3 := NumAt(Site+'_VELOCITY',3)*DistScale;
   If NumCount(Site+'_EPOCH') < 1 Then Fail(Site+'_EPOCH est requis avec _VELOCITY.',1);
   Ep := ToET(NumAt(Site+'_EPOCH',1),DateAt(Site+'_EPOCH',1));
  End Else If NumCount(Site+'_VELOCITY') > 0 Then
   Fail(Site+'_VELOCITY doit contenir 3 valeurs.',1);
  { Bornes }
  Key := Site+'_BOUNDS';
  If NumCount(Key) = 0 Then Begin
   Sites[NSites].T0 := DayStart(1950,1,1);
   Sites[NSites].T1 := DayStart(2050,1,1);
  End Else Begin
   If NumCount(Key) <> 2 Then Fail(Key+' doit contenir deux valeurs (debut, fin).',1);
   Sites[NSites].T0 := ToET(NumAt(Key,1),DateAt(Key,1));
   Sites[NSites].T1 := ToET(NumAt(Key,2),DateAt(Key,2));
  End;
  If Sites[NSites].T1 <= Sites[NSites].T0 Then Fail(Key+' : la fin doit suivre le debut.',1);
  { Etats aux bornes }
  Sites[NSites].P0[1] := X+V1*(Sites[NSites].T0-Ep);
  Sites[NSites].P0[2] := Y+V2*(Sites[NSites].T0-Ep);
  Sites[NSites].P0[3] := Z+V3*(Sites[NSites].T0-Ep);
  Sites[NSites].P1[1] := X+V1*(Sites[NSites].T1-Ep);
  Sites[NSites].P1[2] := Y+V2*(Sites[NSites].T1-Ep);
  Sites[NSites].P1[3] := Z+V3*(Sites[NSites].T1-Ep);
  Sites[NSites].P0[4] := V1; Sites[NSites].P0[5] := V2; Sites[NSites].P0[6] := V3;
  Sites[NSites].P1[4] := V1; Sites[NSites].P1[5] := V2; Sites[NSites].P1[6] := V3;
  { Axes topocentriques }
  Sites[NSites].Up := 'Z'; Sites[NSites].North := 'X';
  If StrCount(Site+'_UP') >= 1 Then Sites[NSites].Up := Copy(StrAt(Site+'_UP',1),1,2);
  If StrCount(Site+'_NORTH') >= 1 Then Sites[NSites].North := Copy(StrAt(Site+'_NORTH',1),1,2);
  If Not AxisText(Sites[NSites].Up,A1,S1) Or Not AxisText(Sites[NSites].North,A2,S2) Then
   Fail(Site+' : _UP et _NORTH doivent etre X, Y ou Z (signe facultatif).',1);
  If A1 = A2 Then Fail(Site+' : _UP et _NORTH doivent etre des axes differents.',1);
 End;
End;

{ ------------------------------------------------------------------ }
{ Ecriture du SPK                                                    }
{ ------------------------------------------------------------------ }

Procedure WriteAt(RecNo: LongInt; Var Rec);
Var Err: Word;
Begin
 {$I-}
 Seek(OutFile,RecNo-1);
 BlockWrite(OutFile,Rec,1);
 Err := IOResult;
 {$I+}
 If Err <> 0 Then Fail('Erreur d''ecriture de '+SpkName+' (disque plein ?).',1);
End;

Procedure PutWord(V: Double);
Var Rec: TRecord;
Begin
 Block[BlockFill] := V;
 Inc(BlockFill);
 Inc(WordAddr);
 If BlockFill = 128 Then Begin
  Move(Block,Rec,1024);
  WriteAt(NextRec,Rec);
  Inc(NextRec);
  BlockFill := 0;
 End;
End;

Procedure PadRecord;
Begin
 While BlockFill <> 0 Do PutWord(0);
End;

Procedure SetInt(Var Rec: TRecord; Offset: Integer; L: LongInt);
Begin
 Move(L,Rec[Offset],4);
End;

Procedure SetDouble(Var Rec: TRecord; Offset: Integer; V: Double);
Begin
 Move(V,Rec[Offset],8);
End;

Procedure SetText(Var Rec: TRecord; Offset: Integer; S: String);
Var K: Integer;
Begin
 For K := 1 To Length(S) Do Rec[Offset+K-1] := Ord(S[K]);
End;

Function PadTo(S: String; Width: Integer): String;
Begin
 If Length(S) > Width Then S := Copy(S,1,Width);
 While Length(S) < Width Do S := S+' ';
 PadTo := S;
End;

Function Exists(Path: String): Boolean;
Var T: File; Err: Word;
Begin
 Assign(T,Path);
 FileMode := 0;
 {$I-} Reset(T,1); Err := IOResult; {$I+}
 If Err = 0 Then Begin
  {$I-} Close(T); Err := IOResult; {$I+}
  Exists := True;
 End Else Exists := False;
End;

Function OptionValue: String;
Begin
 Inc(ArgIndex);
 If ArgIndex > ParamCount Then SyntaxError('Valeur manquante pour '+Arg+'.');
 OptionValue := ParamStr(ArgIndex);
End;

Procedure WriteSPK;
Var
 Rec, SumBuf, NameBuf: TRecord;
 K, R, Total, Slot, N, Offset, L: Integer;
 Free: LongInt;
 Text: String;
 Err: Word;
Begin
 Assign(OutFile,SpkName);
 {$I-} Rewrite(OutFile,1024); Err := IOResult; {$I+}
 If Err <> 0 Then Fail('Impossible de creer '+SpkName+'.',1);
 OutOpen := True;
 Total := (NSites+24) Div 25;
 { Enregistrements reserves : fichier, commentaires, sommaires + noms. }
 FillChar(Rec,SizeOf(Rec),0);
 For K := 1 To 2+2*Total Do WriteAt(K,Rec);
 NextRec := 3+2*Total; BlockFill := 0;
 WordAddr := (2+2*Total)*128+1;
 Free := WordAddr;
 For K := 1 To NSites Do Begin
  SegStart[K] := WordAddr;
  For N := 1 To 6 Do PutWord(Sites[K].P0[N]);
  For N := 1 To 6 Do PutWord(Sites[K].P1[N]);
  PutWord(Sites[K].T0);
  PutWord(Sites[K].T1-Sites[K].T0);
  PutWord(1);
  PutWord(2);
  SegEnd[K] := WordAddr-1;
  Free := WordAddr;
 End;
 PadRecord;
 { Sommaires et noms. }
 For R := 1 To Total Do Begin
  FillChar(SumBuf,SizeOf(SumBuf),0);
  FillChar(NameBuf,SizeOf(NameBuf),0);
  FillChar(NameBuf,1000,32);
  If R < Total Then SetDouble(SumBuf,0,3+2*R) Else SetDouble(SumBuf,0,0);
  If R > 1 Then SetDouble(SumBuf,8,3+2*(R-2)) Else SetDouble(SumBuf,8,0);
  N := NSites-25*(R-1);
  If N > 25 Then N := 25;
  SetDouble(SumBuf,16,N);
  For Slot := 0 To N-1 Do Begin
   K := 25*(R-1)+Slot+1;
   Offset := 24+Slot*40;
   SetDouble(SumBuf,Offset,Sites[K].T0);
   SetDouble(SumBuf,Offset+8,Sites[K].T1);
   SetInt(SumBuf,Offset+16,Sites[K].Body);
   SetInt(SumBuf,Offset+20,Sites[K].Center);
   SetInt(SumBuf,Offset+24,Sites[K].Frame);
   SetInt(SumBuf,Offset+28,8);
   SetInt(SumBuf,Offset+32,SegStart[K]);
   SetInt(SumBuf,Offset+36,SegEnd[K]);
   SetText(NameBuf,Slot*40,PadTo(Sites[K].Name,40));
  End;
  WriteAt(3+2*(R-1),SumBuf);
  WriteAt(4+2*(R-1),NameBuf);
 End;
 { Commentaires (enregistrement 2) : lignes separees par NUL, fin par EOT. }
 FillChar(Rec,SizeOf(Rec),32);
 Text := 'Genere par PINPOINT a partir de '+DefName+Chr(0)+
         Num(NSites)+' site(s), segments SPK de type 8'+Chr(0)+Chr(4);
 L := Length(Text);
 If L > 1000 Then L := 1000;
 SetText(Rec,0,Copy(Text,1,L));
 WriteAt(2,Rec);
 { Enregistrement de fichier. }
 FillChar(Rec,SizeOf(Rec),0);
 SetText(Rec,0,'DAF/SPK ');
 SetInt(Rec,8,2); SetInt(Rec,12,6);
 SetText(Rec,16,PadTo('PINPOINT '+DefName,60));
 SetInt(Rec,76,3); SetInt(Rec,80,3+2*(Total-1)); SetInt(Rec,84,Free);
 SetText(Rec,88,'LTL-IEEE');
 SetText(Rec,699,FtpString);
 WriteAt(1,Rec);
 {$I-} Close(OutFile); Err := IOResult; {$I+}
 OutOpen := False;
End;

{ ------------------------------------------------------------------ }
{ Fichier FK de reperes topocentriques                               }
{ ------------------------------------------------------------------ }

Procedure PutFk(S: String);
Var Err: Word;
Begin
 {$I-} WriteLn(FkFile,S); Err := IOResult; {$I+}
 If Err <> 0 Then Fail('Erreur d''ecriture de '+FkName+'.',1);
End;

Function Deg12(Rad: Double): String;
Var D: Double;
Begin
 D := Rad*(180.0/Pi);
 Deg12 := Real12(D);
End;

Procedure WriteFK;
Var
 K, A, B, T, Up, Nth, SU, SN: Integer;
 Frame, Id, Item: String;
 Lat, Lon, SL, CL, SO, CO: Double;
 V: Array[1..3,1..3] Of Double;
 R: Array[1..3,1..3] Of Double;
 U, Nv: Array[1..3] Of Double;
 Err: Word;
 FId: LongInt;
 Q: Char;
Begin
 Q := Chr(39);
 Assign(FkFile,FkName);
 {$I-} Rewrite(FkFile); Err := IOResult; {$I+}
 If Err <> 0 Then Fail('Impossible de creer '+FkName+'.',1);
 FkOpen := True;
 PutFk('KPL/FK');
 PutFk('');
 PutFk('Reperes topocentriques generes par PINPOINT a partir de '+DefName+'.');
 PutFk('Chaque repere <SITE>_TOPO a pour axe "haut" la verticale locale');
 PutFk('(normale a l''ellipsoide) et pour axe "nord" la tangente au meridien.');
 PutFk('');
 For K := 1 To NSites Do Begin
  If Length(Sites[K].Name) > 26 Then
   Fail('Nom de site trop long pour un repere FK (26 caracteres maximum) : '+Sites[K].Name,1);
  FId := 1000000+Sites[K].Body;
  Frame := Sites[K].Name+'_TOPO';
  Id := Num(FId);
  Lat := Sites[K].Lat; Lon := Sites[K].Lon;
  SL := Sin(Lat); CL := Cos(Lat); SO := Sin(Lon); CO := Cos(Lon);
  PutFk('\begindata');
  PutFk('   FRAME_'+Frame+' = '+Id);
  PutFk('   FRAME_'+Id+'_NAME = '+Q+Frame+Q);
  PutFk('   FRAME_'+Id+'_CLASS = 4');
  PutFk('   FRAME_'+Id+'_CLASS_ID = '+Id);
  PutFk('   FRAME_'+Id+'_CENTER = '+Num(Sites[K].Body));
  PutFk('   OBJECT_'+Num(Sites[K].Body)+'_FRAME = '+Q+Frame+Q);
  PutFk('   TKFRAME_'+Id+'_RELATIVE = '+Q+Sites[K].FrameName+Q);
  AxisText(Sites[K].Up,Up,SU);
  AxisText(Sites[K].North,Nth,SN);
  If (Up = 3) And (Nth = 1) And (SU = 1) And (SN = 1) Then Begin
   PutFk('   TKFRAME_'+Id+'_SPEC = '+Q+'ANGLES'+Q);
   PutFk('   TKFRAME_'+Id+'_UNITS = '+Q+'DEGREES'+Q);
   PutFk('   TKFRAME_'+Id+'_AXES = ( 3, 2, 3 )');
   PutFk('   TKFRAME_'+Id+'_ANGLES = ( '+Deg12(-Lon)+' '+Deg12(-(Pi/2-Lat))+' 180.0 )');
  End Else Begin
   { Matrice REL -> TOPO : lignes = axes X, Y, Z du repere topocentrique. }
   U[1] := CL*CO; U[2] := CL*SO; U[3] := SL;
   Nv[1] := -SL*CO; Nv[2] := -SL*SO; Nv[3] := CL;
   For A := 1 To 3 Do Begin
    R[Up,A] := SU*U[A];
    R[Nth,A] := SN*Nv[A];
   End;
   T := 6-Up-Nth;
   { T = (a x b) avec (a,b,T) cyclique }
   A := T Mod 3+1; B := A Mod 3+1;
   R[T,1] := R[A,2]*R[B,3]-R[A,3]*R[B,2];
   R[T,2] := R[A,3]*R[B,1]-R[A,1]*R[B,3];
   R[T,3] := R[A,1]*R[B,2]-R[A,2]*R[B,1];
   PutFk('   TKFRAME_'+Id+'_SPEC = '+Q+'MATRIX'+Q);
   For A := 1 To 3 Do Begin
    Item := '';
    For B := 1 To 3 Do Item := Item+' '+Real12(R[A,B]);
    If A = 1 Then Item := '   TKFRAME_'+Id+'_MATRIX = (' + Item
    Else Item := '                           ' + Item;
    If A = 3 Then Item := Item+' )';
    PutFk(Item);
   End;
  End;
  PutFk('\begintext');
  PutFk('');
 End;
 {$I-} Close(FkFile); Err := IOResult; {$I+}
 FkOpen := False;
End;

Begin
 DefName := ''; SpkName := ''; FkName := ''; NPck := 0; NFrameFiles := 0;
 TdbDates := False; Force := False; Quiet := False;
 OutOpen := False; FkOpen := False; NExtra := 0; NVars := 0; NNums := 0; NStrs := 0;
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
  If Arg = '-DEF' Then DefName := OptionValue
  Else If Arg = '-SPK' Then SpkName := OptionValue
  Else If Arg = '-FK' Then FkName := OptionValue
  Else If Arg = '-PCK' Then Begin
   If NPck = 8 Then SyntaxError('Trop de PCK (8 au plus).');
   Inc(NPck); PckNames[NPck] := ArgIndex+1; Inc(ArgIndex);
   If ArgIndex > ParamCount Then SyntaxError('Valeur manquante pour -pck.');
  End Else If Arg = '-FRAMES' Then Begin
   If NFrameFiles = 8 Then SyntaxError('Trop de FK (8 au plus).');
   Inc(NFrameFiles); FrameFiles[NFrameFiles] := ArgIndex+1; Inc(ArgIndex);
   If ArgIndex > ParamCount Then SyntaxError('Valeur manquante pour -frames.');
  End
  Else If Arg = '-TDB' Then TdbDates := True
  Else If Arg = '-F' Then Force := True
  Else If Arg = '-Q' Then Quiet := True
  Else SyntaxError('Option inconnue ou non prise en charge : '+ParamStr(ArgIndex));
  Inc(ArgIndex);
 End;
 If DefName = '' Then SyntaxError('-def est requis.');
 If SpkName = '' Then SyntaxError('-spk est requis.');
 If (Upper(SpkName) = Upper(DefName)) Or ((FkName <> '') And
    ((Upper(FkName) = Upper(DefName)) Or (Upper(FkName) = Upper(SpkName)))) Then
  SyntaxError('Les fichiers d''entree et de sortie doivent differer.');
 If Exists(SpkName) And Not Force Then Fail(SpkName+' existe deja. Utiliser -f pour l''ecraser.',1);
 If (FkName <> '') And Exists(FkName) And Not Force Then
  Fail(FkName+' existe deja. Utiliser -f pour l''ecraser.',1);

 New(Vars); New(Nums); New(IsDate); New(Strs);
 Filter := 1;
 For I := 1 To NPck Do LoadKernel(ParamStr(PckNames[I]));
 For I := 1 To NFrameFiles Do LoadFrames(ParamStr(FrameFiles[I]));
 Filter := 0;
 LoadKernel(DefName);
 ReadSites;
 WriteSPK;
 If FkName <> '' Then WriteFK;
 Cleanup(True);
 If Not Quiet Then Begin
  WriteLn('SPK ecrit : ',SpkName,' (',NSites,' segment(s) de type 8).');
  If FkName <> '' Then WriteLn('FK ecrit  : ',FkName,' (',NSites,' repere(s) topocentrique(s)).');
 End;
End.
