{ @author: Sylvain Maltais (support@gladir.com)
 @created: 2026
 @website(https://www.gladir.com/space-0)
 @abstract(Target: Turbo Pascal 7, Free Pascal 3.2) }

Program DSKBRIEF;

{$A-}
{$E+,N+}

{ Resume de la couverture spatiale et des attributs de modeles de surface
  DSK (fichiers DAS/DSK), sous-ensemble autonome de DSKBRIEF NAIF : aucune
  unite, aucune inclusion.
  Lecture directe de la structure DAS (enregistrement de fichier, zone de
  commentaires, repertoires et groupes d'enregistrements) puis des
  descripteurs DLA (segments chaines) et du descripteur DSK de 24 reels :
  corps central, identifiant de surface, repere, classe, type, systeme de
  coordonnees (latitudinal, cylindrique, rectangulaire, planetodetique),
  bornes spatiales et intervalle de temps. Pour le type 2 (plaques) :
  nombres de sommets et de plaques, etendue des sommets, grille de voxels.
  Fichiers DSK LTL-IEEE ou BIG-IEEE fermes proprement (segregues), < 2 Gio.
  Pas de conversion, de verification de la geometrie ni de noms externes.
  Temps : ET/TDB calendrier ou secondes, ou UTC (table NAIF0012 integree,
  1972-2017, 37 s ensuite).
  Sources : naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/ug/dskbrief.html
            naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/dsk.html
            naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/das.html
            naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/dla.html
  Compilation : fpc -Mtp DSKBRIEF.PAS
  Limites DOS : 500 groupes DAS par fichier, 1000 segments. }

Const
 MaxClusters = 500;
 MaxSegments = 1000;
 LeapCount = 28;
 LeapYears: Array[1..LeapCount] Of Integer =
  (1972,1972,1973,1974,1975,1976,1977,1978,1979,1980,1981,1982,
   1983,1985,1988,1990,1991,1992,1993,1994,1996,1997,1999,2006,
   2009,2012,2015,2017);
 LeapMonths: Array[1..LeapCount] Of Integer =
  (1,7,1,1,1,1,1,1,1,1,7,7,7,7,1,1,1,7,7,7,1,7,1,1,1,7,7,1);
 MinEpoch = -63082324800.0;
 MaxEpoch = 252455572800.0;
 WordsPer: Array[1..3] Of Integer = (1024,128,256);

Type
 TBlock = Array[0..1023] Of Byte;
 TBytes8 = Array[0..7] Of Byte;
 TCluster = Record
  Typ: Integer;
  First, Count, Start: LongInt;
 End;
 TDesc = Array[1..24] Of Double;

Var
 Kernel: File;
 Buffer: TBlock;
 CacheRec: LongInt;
 Clusters: Array[1..MaxClusters] Of TCluster;
 NClusters: Integer;
 LeapEpoch: Array[1..LeapCount] Of Double;
 Opened, BigEndian, Tabular, Detail, Comments, NumericOnly: Boolean;
 FileBytes, NResvR, NComR, NComC: LongInt;
 TimeMode: Integer;
 CurPath, FormatName: String;
 Arg: String;
 ArgIndex, I, Files, SegTotal: Integer;

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
 Fail(S+' Voir DSKBRIEF -h.',2);
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

Function PadLeft(S: String; Width: Integer): String;
Begin
 While Length(S) < Width Do S := ' '+S;
 PadLeft := S;
End;

Function Real6(V: Double): String;
Var S: String;
Begin
 Str(V:0:6,S);
 Real6 := S;
End;

Procedure Help;
Begin
 WriteLn('DSKBRIEF - Cette commande permet d''examiner la couverture spatiale et ',
         'les attributs de mod'+#138+'les de surface DSK.');
 WriteLn;
 WriteLn('Syntaxe : DSKBRIEF [options] <fichier.bds> [<fichier.bds> ...]');
 WriteLn;
 WriteLn('Options :');
 WriteLn('  -t       Affichage en tableau (une ligne par segment)');
 WriteLn('  -d       Details : tailles DLA, etendue des sommets, grille de voxels');
 WriteLn('  -c       Zone de commentaires du fichier');
 WriteLn('  -utc     Dates UTC (table integree NAIF0012, depuis 1972)');
 WriteLn('  -etsec   Secondes ET/TDB depuis J2000, au lieu des dates');
 WriteLn('  -n       Identifiants NAIF numeriques uniquement');
 WriteLn('  -h, /?   Aide (-help et --help acceptes)');
 WriteLn;
 WriteLn('Sans option temporelle : calendrier ET/TDB, pas UTC.');
 WriteLn('Angles en degres, distances en km.');
 WriteLn;
 WriteLn('Exemples :');
 WriteLn('  DSKBRIEF phobos.bds');
 WriteLn('  DSKBRIEF -t -utc mars1.bds mars2.bds');
 WriteLn('  DSKBRIEF -d -c eros.bds');
 WriteLn;
 WriteLn('Limites : DAS/DSK LTL-IEEE/BIG-IEEE segreges, type 2, 1000 segments.');
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

{ TimeMode : 0 calendrier ET/TDB, 1 UTC, 2 secondes ET depuis J2000. }
Function TimeText(ET: Double): String;
Begin
 If TimeMode = 2 Then TimeText := Real6(ET)
 Else If TimeMode = 1 Then TimeText := UTCText(ET)
 Else TimeText := Calendar(ET,False);
End;

Function TimeLabel: String;
Begin
 If TimeMode = 2 Then TimeLabel := 'ET/TDB, secondes depuis J2000'
 Else If TimeMode = 1 Then TimeLabel := 'UTC (NAIF0012 integre, 37 s apres 2017)'
 Else TimeLabel := 'ET/TDB, calendrier';
End;

{ ------------------------------------------------------------------ }
{ Lecture DAS                                                        }
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
 If ((B[7] And $7F) = $7F) And ((B[6] And $F0) = $F0) Then
  Fail('Valeur flottante non finie dans '+CurPath+'.',1);
 Move(B,V,8);
 DoubleAt := V;
End;

Procedure ReadRecord(Number: LongInt);
Var Got, Err: Word;
Begin
 If CacheRec = Number Then Exit;
 CacheRec := 0;
 If (Number < 1) Or (Number > FileBytes Div 1024) Then
  Fail('Record DAS '+Num(Number)+' hors du fichier '+CurPath+'.',1);
 {$I-}
 Seek(Kernel,(Number-1)*1024);
 Err := IOResult;
 {$I+}
 If Err <> 0 Then Fail('Impossible de positionner la lecture.',1);
 {$I-} BlockRead(Kernel,Buffer,1024,Got); Err := IOResult; {$I+}
 If (Err <> 0) Or (Got <> 1024) Then Fail('Record DAS '+Num(Number)+' tronque.',1);
 CacheRec := Number;
End;

{ Position (record, octet) du mot logique A (base 1) du type T : 1 caractere,
  2 reel, 3 entier. }
Procedure Locate(T: Integer; A: LongInt; Var Rec: LongInt; Var Off: Integer);
Var K: Integer; Words, Rel: LongInt;
Begin
 For K := 1 To NClusters Do
  If Clusters[K].Typ = T Then Begin
   Words := Clusters[K].Count*WordsPer[T];
   If (A >= Clusters[K].Start) And (A < Clusters[K].Start+Words) Then Begin
    Rel := A-Clusters[K].Start;
    Rec := Clusters[K].First+Rel Div WordsPer[T];
    Off := Integer(Rel Mod WordsPer[T]);
    Exit;
   End;
  End;
 Fail('Adresse logique '+Num(A)+' hors des donnees DAS de '+CurPath+'.',1);
End;

Function IntWord(A: LongInt): LongInt;
Var Rec: LongInt; Off: Integer;
Begin
 Locate(3,A,Rec,Off);
 ReadRecord(Rec);
 IntWord := IntAt(Off*4);
End;

Function DblWord(A: LongInt): Double;
Var Rec: LongInt; Off: Integer;
Begin
 Locate(2,A,Rec,Off);
 ReadRecord(Rec);
 DblWord := DoubleAt(Off*8);
End;

Procedure OpenDAS(Path: String);
Var
 Err: Word;
 Rec, Fwd, Phys, Count, Visited: LongInt;
 T, W, Sign: Integer;
 LogStart: Array[1..3] Of LongInt;
Begin
 CurPath := Path;
 CloseKernel;
 Assign(Kernel,Path);
 FileMode := 0;
 {$I-} Reset(Kernel,1); Err := IOResult; {$I+}
 If Err <> 0 Then Fail('Impossible d''ouvrir '+Path+'.',1);
 Opened := True;
 {$I-} FileBytes := FileSize(Kernel); Err := IOResult; {$I+}
 If (Err <> 0) Or (FileBytes < 2048) Or (FileBytes Mod 1024 <> 0) Then
  Fail('Taille DAS invalide ou fichier tronque : '+Path,1);
 CacheRec := 0;
 BigEndian := False;
 ReadRecord(1);
 If TextAt(0,8) <> 'DAS/DSK ' Then Fail('Fichier non DAS/DSK : '+Path,1);
 If (TextAt(84,8) <> 'LTL-IEEE') And (TextAt(84,8) <> 'BIG-IEEE') Then
  Fail('Format binaire non pris en charge : '+TextAt(84,8),1);
 FormatName := TextAt(84,8);
 BigEndian := FormatName = 'BIG-IEEE';
 NResvR := IntAt(68); NComR := IntAt(76); NComC := IntAt(80);
 If (NResvR < 0) Or (NComR < 0) Or (NComC < 0) Or
    (2+NResvR+NComR > FileBytes Div 1024) Then
  Fail('Parametres du fichier DAS invalides : '+Path,1);
 { Repertoires et groupes d'enregistrements. }
 NClusters := 0;
 For T := 1 To 3 Do LogStart[T] := 1;
 Rec := 2+NResvR+NComR; Visited := 0;
 While Rec <> 0 Do Begin
  Inc(Visited);
  If Visited > FileBytes Div 1024 Then Fail('Repertoires DAS cycliques : '+Path,1);
  ReadRecord(Rec);
  Fwd := IntAt(4);
  T := IntAt(32);
  If (T < 1) Or (T > 3) Then Fail('Type du premier groupe DAS invalide : '+Path,1);
  Phys := Rec+1;
  W := 10;
  Sign := 1;
  While W <= 256 Do Begin
   ReadRecord(Rec);
   Count := IntAt((W-1)*4);
   If Count = 0 Then Break;
   { Le signe du descripteur donne le sens du changement de type dans le
     cycle caractere, reel, entier : + type suivant, - type precedent. }
   If Count < 0 Then Begin Sign := -1; Count := -Count End Else Sign := 1;
   If W > 10 Then T := (T-1+Sign+3) Mod 3+1;
   If NClusters = MaxClusters Then Fail('Plus de '+Num(MaxClusters)+' groupes DAS.',1);
   If Phys+Count-1 > FileBytes Div 1024 Then Fail('Groupe DAS hors fichier.',1);
   Inc(NClusters);
   Clusters[NClusters].Typ := T;
   Clusters[NClusters].First := Phys;
   Clusters[NClusters].Count := Count;
   Clusters[NClusters].Start := LogStart[T];
   LogStart[T] := LogStart[T]+Count*WordsPer[T];
   Phys := Phys+Count;
   Inc(W);
  End;
  If (Fwd < 0) Or (Fwd > FileBytes Div 1024) Then Fail('Pointeur de repertoire DAS invalide.',1);
  Rec := Fwd;
 End;
End;

{ ------------------------------------------------------------------ }
{ Descriptions                                                       }
{ ------------------------------------------------------------------ }

Function BodyName(ID: LongInt): String;
Var S: String;
Begin
 S := '';
 If Not NumericOnly Then Case ID Of
  10: S := 'SUN';
  199: S := 'MERCURY';
  299: S := 'VENUS';
  301: S := 'MOON';
  399: S := 'EARTH';
  401: S := 'PHOBOS';
  402: S := 'DEIMOS';
  499: S := 'MARS';
  501: S := 'IO';
  502: S := 'EUROPA';
  503: S := 'GANYMEDE';
  504: S := 'CALLISTO';
  599: S := 'JUPITER';
  602: S := 'ENCELADUS';
  606: S := 'TITAN';
  699: S := 'SATURN';
  799: S := 'URANUS';
  899: S := 'NEPTUNE';
  901: S := 'CHARON';
  999: S := 'PLUTO';
  2000001: S := 'CERES';
  2000004: S := 'VESTA';
  2000433: S := 'EROS';
  2025143: S := 'ITOKAWA';
  2101955: S := 'BENNU';
  2162173: S := 'RYUGU';
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
  13: S := 'GALACTIC';
  17: S := 'ECLIPJ2000';
  10010: S := 'IAU_SUN';
  10011: S := 'IAU_MERCURY';
  10012: S := 'IAU_VENUS';
  10013: S := 'IAU_EARTH';
  10014: S := 'IAU_MARS';
  10015: S := 'IAU_JUPITER';
  10016: S := 'IAU_SATURN';
  10017: S := 'IAU_URANUS';
  10018: S := 'IAU_NEPTUNE';
  10019: S := 'IAU_PLUTO';
  10020: S := 'IAU_MOON';
  10021: S := 'IAU_PHOBOS';
  10022: S := 'IAU_DEIMOS';
  10023: S := 'IAU_IO';
  10024: S := 'IAU_EUROPA';
  10025: S := 'IAU_GANYMEDE';
  10026: S := 'IAU_CALLISTO';
  10040: S := 'IAU_ENCELADUS';
  10044: S := 'IAU_TITAN';
  10079: S := 'IAU_CHARON';
  10085: S := 'IAU_EROS';
  10099: S := 'IAU_VESTA';
  10100: S := 'IAU_ITOKAWA';
  10101: S := 'IAU_CERES';
  10106: S := 'IAU_BENNU';
  10110: S := 'IAU_RYUGU';
  13000: S := 'ITRF93';
 End;
 If S = '' Then FrameName := Num(ID) Else FrameName := Num(ID)+' '+S;
End;

Function ClassName(C: LongInt): String;
Begin
 If C = 1 Then ClassName := '1 (surface a valeur unique)'
 Else If C = 2 Then ClassName := '2 (surface generale)'
 Else ClassName := Num(C);
End;

Function TypeName(T: LongInt): String;
Begin
 If T = 2 Then TypeName := '2 (plaques triangulaires)' Else TypeName := Num(T);
End;

Function SystemName(C: LongInt): String;
Begin
 Case C Of
  1: SystemName := '1 (latitudinal)';
  2: SystemName := '2 (cylindrique)';
  3: SystemName := '3 (rectangulaire)';
  4: SystemName := '4 (planetodetique)';
 Else SystemName := Num(C);
 End;
End;

Function Deg(V: Double): String;
Var T: Double;
Begin
 T := V*(180.0/Pi);
 Deg := Real6(T);
End;

{ ------------------------------------------------------------------ }
{ Commentaires, segments                                             }
{ ------------------------------------------------------------------ }

Procedure ShowComments;
Var Rec, Left: LongInt; K, Count: Integer; Line: String; Any: Boolean;
Begin
 WriteLn;
 If NComC = 0 Then Begin WriteLn('(aucun commentaire)'); Exit End;
 Line := ''; Any := False; Left := NComC;
 Rec := 2+NResvR;
 While (Left > 0) And (Rec < 2+NResvR+NComR) Do Begin
  ReadRecord(Rec);
  If Left > 1024 Then Count := 1024 Else Count := Left;
  For K := 0 To Count-1 Do Begin
   If Buffer[K] = 0 Then Begin WriteLn(Line); Line := ''; Any := True End
   Else If Length(Line) < 250 Then Begin
    If (Buffer[K] >= 32) And (Buffer[K] < 127) Then Line := Line+Chr(Buffer[K])
    Else Line := Line+' ';
   End;
  End;
  Left := Left-Count;
  Inc(Rec);
 End;
 If Line <> '' Then Begin WriteLn(Line); Any := True End;
 If Not Any Then WriteLn('(aucun commentaire)');
End;

Procedure ShowCoordinates(Var D: TDesc);
Var Sys: LongInt;
Begin
 Sys := Round(D[6]);
 If (Sys = 1) Or (Sys = 4) Then Begin
  WriteLn('    Longitude : ',Deg(D[17]),' a ',Deg(D[18]),' deg');
  WriteLn('    Latitude  : ',Deg(D[19]),' a ',Deg(D[20]),' deg');
  If Sys = 1 Then WriteLn('    Rayon     : ',Real6(D[21]),' a ',Real6(D[22]),' km')
  Else Begin
   WriteLn('    Altitude  : ',Real6(D[21]),' a ',Real6(D[22]),' km');
   WriteLn('    Rayon equatorial : ',Real6(D[7]),' km, aplatissement : ',Real6(D[8]));
  End;
 End Else If Sys = 2 Then Begin
  WriteLn('    Rayon     : ',Real6(D[17]),' a ',Real6(D[18]),' km');
  WriteLn('    Longitude : ',Deg(D[19]),' a ',Deg(D[20]),' deg');
  WriteLn('    Z         : ',Real6(D[21]),' a ',Real6(D[22]),' km');
 End Else Begin
  WriteLn('    X         : ',Real6(D[17]),' a ',Real6(D[18]),' km');
  WriteLn('    Y         : ',Real6(D[19]),' a ',Real6(D[20]),' km');
  WriteLn('    Z         : ',Real6(D[21]),' a ',Real6(D[22]),' km');
 End;
End;

Function RangeText(Var D: TDesc): String;
Var Sys: LongInt;
Begin
 Sys := Round(D[6]);
 If (Sys = 1) Or (Sys = 4) Then
  RangeText := 'lon '+Deg(D[17])+'..'+Deg(D[18])+' lat '+Deg(D[19])+'..'+Deg(D[20])
 Else If Sys = 2 Then RangeText := 'r '+Real6(D[17])+'..'+Real6(D[18])
 Else RangeText := 'x '+Real6(D[17])+'..'+Real6(D[18]);
End;

Procedure ShowSegment(Number: Integer; IBase, ISize, DBase, DSize, CSize: LongInt);
Var D: TDesc; K: Integer; NV, NP, A: LongInt; Typ: LongInt; HasCounts: Boolean;
Begin
 If DSize < 24 Then Fail('Segment '+Num(Number)+' : composante reelle trop courte.',1);
 For K := 1 To 24 Do D[K] := DblWord(DBase+K);
 Typ := Round(D[4]);
 HasCounts := False; NV := 0; NP := 0;
 If (Typ = 2) And (ISize >= 2) Then Begin
  NV := IntWord(IBase+1); NP := IntWord(IBase+2);
  HasCounts := True;
 End;
 If Tabular Then Begin
  Write(PadLeft(Num(Files),3),' ',PadLeft(Num(Number),3),' ',
        PadRight(BodyName(Round(D[2])),22),' ',PadLeft(Num(Round(D[1])),7),' ',
        PadRight(FrameName(Round(D[5])),20),' ',PadLeft(Num(Round(D[3])),2),' ',
        PadLeft(Num(Typ),2));
  If HasCounts Then WriteLn(' ',PadLeft(Num(NV),9),' ',PadLeft(Num(NP),9))
  Else WriteLn;
  WriteLn('        ',TimeText(D[23]),'  ->  ',TimeText(D[24]));
  Exit;
 End;
 WriteLn;
 WriteLn('Segment ',Number);
 WriteLn('  Corps central   : ',BodyName(Round(D[2])));
 WriteLn('  Surface         : ',Round(D[1]));
 WriteLn('  Repere          : ',FrameName(Round(D[5])));
 WriteLn('  Classe          : ',ClassName(Round(D[3])));
 WriteLn('  Type            : ',TypeName(Typ));
 WriteLn('  Coordonnees     : ',SystemName(Round(D[6])));
 ShowCoordinates(D);
 WriteLn('  Debut           : ',TimeText(D[23]));
 WriteLn('  Fin             : ',TimeText(D[24]));
 If HasCounts Then Begin
  WriteLn('  Sommets         : ',NV);
  WriteLn('  Plaques         : ',NP);
 End Else If Typ <> 2 Then WriteLn('  (type de donnees non decode par cet outil)');
 If Detail Then Begin
  WriteLn('  Composantes DLA : ',ISize,' entiers, ',DSize,' reels, ',CSize,' caracteres');
  If HasCounts And (ISize >= 10) And (DSize >= 34) Then Begin
   WriteLn('  Voxels (fins)   : ',IntWord(IBase+3),' (grille ',IntWord(IBase+4),' x ',
           IntWord(IBase+5),' x ',IntWord(IBase+6),')');
   WriteLn('  Echelle grossiere : ',IntWord(IBase+7));
   A := DBase+24;
   WriteLn('  Etendue des sommets (km) :');
   WriteLn('    X : ',Real6(DblWord(A+1)),' a ',Real6(DblWord(A+2)));
   WriteLn('    Y : ',Real6(DblWord(A+3)),' a ',Real6(DblWord(A+4)));
   WriteLn('    Z : ',Real6(DblWord(A+5)),' a ',Real6(DblWord(A+6)));
   WriteLn('  Taille de voxel (km) : ',Real6(DblWord(A+10)));
  End;
 End;
End;

Procedure ShowFile(Path: String);
Var Addr, First, Last, Fwd, IBase, ISize, DBase, DSize, CSize, Skip: LongInt;
    Seg: Integer;
Begin
 OpenDAS(Path);
 If Not Tabular Then Begin
  WriteLn;
  WriteLn('Fichier : ',Path);
  WriteLn('Format  : ',FormatName,', ',NClusters,' groupe(s) DAS');
  WriteLn('Temps   : ',TimeLabel);
 End;
 If Comments Then ShowComments;
 First := IntWord(2); Last := IntWord(3);
 If (First = -1) Or (First = 0) Then Begin
  If Not Tabular Then WriteLn('Aucun segment : fichier DSK vide.');
  CloseKernel; Exit;
 End;
 If (First < 4) Or (Last < First) Then Fail('Resume DLA invalide dans '+Path+'.',1);
 Addr := First; Seg := 0;
 While Addr > 0 Do Begin
  Inc(Seg); Inc(SegTotal);
  If Seg > MaxSegments Then Fail('Plus de '+Num(MaxSegments)+' segments.',1);
  Fwd := IntWord(Addr+1);
  IBase := IntWord(Addr+2); ISize := IntWord(Addr+3);
  DBase := IntWord(Addr+4); DSize := IntWord(Addr+5);
  Skip := IntWord(Addr+6); CSize := IntWord(Addr+7);
  If (IBase < 0) Or (ISize < 0) Or (DBase < 0) Or (DSize < 0) Or (CSize < 0) Then
   Fail('Descripteur DLA invalide (segment '+Num(Seg)+').',1);
  ShowSegment(Seg,IBase,ISize,DBase,DSize,CSize);
  If Fwd = -1 Then Addr := 0
  Else If Fwd <= Addr Then Fail('Chaine DLA incoherente (segment '+Num(Seg)+').',1)
  Else Addr := Fwd;
 End;
 If Not Tabular Then Begin
  WriteLn;
  WriteLn(Seg,' segment(s).');
 End;
 CloseKernel;
End;

Var
 Paths: Array[1..40] Of Integer;
 Count: Integer;

Begin
 Opened := False; Tabular := False; Detail := False; Comments := False;
 NumericOnly := False; TimeMode := 0; Count := 0; SegTotal := 0;
 For I := 1 To LeapCount Do
  LeapEpoch[I] := DayStart(LeapYears[I],LeapMonths[I],1);
 If ParamCount = 0 Then Begin Help; Halt(0) End;
 For ArgIndex := 1 To ParamCount Do Begin
  Arg := Upper(ParamStr(ArgIndex));
  If (Arg = '-H') Or (Arg = '-HELP') Or (Arg = '--HELP') Or (Arg = '/?') Then Begin
   Help; Halt(0);
  End;
 End;
 For ArgIndex := 1 To ParamCount Do Begin
  Arg := Upper(ParamStr(ArgIndex));
  If Arg = '-T' Then Tabular := True
  Else If Arg = '-D' Then Detail := True
  Else If Arg = '-C' Then Comments := True
  Else If Arg = '-UTC' Then TimeMode := 1
  Else If Arg = '-ETSEC' Then TimeMode := 2
  Else If Arg = '-N' Then NumericOnly := True
  Else If Arg = '' Then SyntaxError('Nom de fichier vide.')
  Else If Arg[1] = '-' Then SyntaxError('Option inconnue : '+ParamStr(ArgIndex))
  Else Begin
   If Count = 40 Then SyntaxError('Trop de fichiers (maximum 40).');
   Inc(Count); Paths[Count] := ArgIndex;
  End;
 End;
 If Count = 0 Then SyntaxError('Fichier DSK requis.');
 If Tabular Then Begin
  WriteLn('Fic Seg Corps central           Surface Repere               Cl Ty   Sommets   Plaques');
 End;
 For Files := 1 To Count Do ShowFile(ParamStr(Paths[Files]));
 CloseKernel;
End.
