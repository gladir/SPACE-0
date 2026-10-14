{ @author: Sylvain Maltais (support@gladir.com)
 @created: 2026
 @website(https://www.gladir.com/space-0)
 @abstract(Target: Turbo Pascal 7, Free Pascal 3.2) }

Program BRIEF;

{$A-}
{$E+,N+}

{ Resume des couvertures SPK, sans calcul des trajectoires.
  Sources : naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/ug/brief.html
            naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/daf.html
            naif.jpl.nasa.gov/pub/naif/generic_kernels/lsk/naif0012.tls
  Compilation : fpc -Mtp BRIEF.PAS
  Sous-ensemble autonome : SPK DAF IEEE, -t, -utc, -etsec, -n, -h.
  Pas de PCK, meta-kernels, LSK externes ni mappings de noms externes.
  UTC : modele DELTET NAIF0012, depuis 1972; offset final 37 secondes
  prolonge apres 2017 (hypothese, pas une prediction de secondes futures).
  Limites DOS : fichiers < 2 Gio, 1500 segments par fichier. }

Const
 MaxSegments = 1500;
 LeapCount = 28;
 LeapYears: Array[1..LeapCount] Of Integer =
  (1972,1972,1973,1974,1975,1976,1977,1978,1979,1980,1981,1982,
   1983,1985,1988,1990,1991,1992,1993,1994,1996,1997,1999,2006,
   2009,2012,2015,2017);
 LeapMonths: Array[1..LeapCount] Of Integer =
  (1,7,1,1,1,1,1,1,1,1,7,7,7,7,1,1,1,7,7,7,1,7,1,1,1,7,7,1);
Type
 TBlock = Array[0..1023] Of Byte;
 TBytes8 = Array[0..7] Of Byte;
 TCoverage = Record
  Body: LongInt;
  First, Last: Double;
 End;
Var
 Kernel: File;
 Buffer: TBlock;
 Coverage: Array[1..MaxSegments] Of TCoverage;
 LeapEpoch: Array[1..LeapCount] Of Double;
 Total: Integer;
 BigEndian, Tabular, UTC, ETSeconds, NumericOnly, Opened: Boolean;
 FileBytes: LongInt;
 Arg: String;
 ArgIndex, Files: Integer;

Procedure Fail(S: String);
Begin
 WriteLn('Erreur : ',S);
 If Opened Then Begin
  {$I-} Close(Kernel); {$I+}
 End;
 Halt(1);
End;

Function Upper(S: String): String;
Var J: Integer;
Begin
 For J := 1 To Length(S) Do S[J] := UpCase(S[J]);
 Upper := S;
End;

Procedure Help;
Begin
 WriteLn('BRIEF - Cette commande permet de r'+#130+'sumer de maniere concise des objets et ',
         'couvertures temporelles SPK.');
 WriteLn;
 WriteLn('Syntaxe : BRIEF [options] <fichier.bsp> [<fichier.bsp> ...]');
 WriteLn;
 WriteLn('Options :');
 WriteLn('  -t       Affichage en tableau');
 WriteLn('  -utc     Dates UTC (table integree NAIF0012, depuis 1972)');
 WriteLn('  -etsec   Secondes ET/TDB depuis J2000, au lieu des dates');
 WriteLn('  -n       Identifiants NAIF numeriques uniquement');
 WriteLn('  -h, /?   Aide (-help et --help acceptes)');
 WriteLn;
 WriteLn('Sans option temporelle : calendrier ET/TDB, pas UTC.');
 WriteLn('Intervalles fusionnes par objet; lacunes conservees.');
 WriteLn('Exemples :');
 WriteLn('  BRIEF mission.bsp');
 WriteLn('  BRIEF -t mission.bsp');
 WriteLn('  BRIEF -utc mission.bsp');
 WriteLn('  BRIEF -t -utc "repertoire avec espaces\mission.bsp"');
 WriteLn('Limites : SPK DAF LTL-IEEE/BIG-IEEE, 1500 segments/fichier, < 2 Gio.');
 WriteLn('Pas de PCK, meta-kernels, LSK externes ou calcul des positions.');
 WriteLn('UTC apres 2017 : dernier offset connu prolonge (37 s).');
 WriteLn('Codes retour : 0 succes, 1 erreur de lecture/format, 2 syntaxe.');
End;

Procedure SyntaxError(S: String);
Begin
 WriteLn('Erreur : ',S,' Voir BRIEF -h.');
 Halt(2);
End;

Function TextAt(Offset, Count: Integer): String;
Var J: Integer; S: String;
Begin
 S := '';
 For J := Offset To Offset + Count - 1 Do S := S + Chr(Buffer[J]);
 TextAt := S;
End;

Function IntAt(Offset: Integer): LongInt;
Var B: Array[0..3] Of Byte; J: Integer; V: LongInt;
Begin
 For J := 0 To 3 Do
  If BigEndian Then B[J] := Buffer[Offset+3-J]
  Else B[J] := Buffer[Offset+J];
 Move(B,V,4);
 IntAt := V;
End;

Function DoubleAt(Offset: Integer): Double;
Var B: TBytes8; J: Integer; V: Double;
Begin
 { Reject NaN and infinity before using floating point instructions. }
 For J := 0 To 7 Do
  If BigEndian Then B[J] := Buffer[Offset+7-J]
  Else B[J] := Buffer[Offset+J];
 If ((B[7] And $7F) = $7F) And ((B[6] And $F0) = $F0) Then
  Fail('Valeur flottante non finie dans un descripteur.');
 Move(B,V,8);
 DoubleAt := V;
End;

Function ControlAt(Offset: Integer; Maximum: LongInt): LongInt;
Var V: Double; N: LongInt;
Begin
 V := DoubleAt(Offset);
 If (V < 0) Or (V > Maximum) Then Fail('Controle DAF hors limites.');
 N := Trunc(V);
 If V <> N Then Fail('Controle DAF non entier.');
 ControlAt := N;
End;

Procedure ReadRecord(Number: LongInt);
Var Got, Err: Word;
Begin
 If (Number < 1) Or (Number > FileBytes Div 1024) Then
  Fail('Pointeur de record DAF hors fichier.');
 {$I-}
 Seek(Kernel,(Number-1)*1024);
 Err := IOResult;
 {$I+}
 If Err <> 0 Then Fail('Impossible de positionner la lecture.');
 {$I-} BlockRead(Kernel,Buffer,1024,Got); Err := IOResult; {$I+}
 If (Err <> 0) Or (Got <> 1024) Then Fail('Record DAF tronque.');
End;

Procedure ReadSPK(Path: String);
Var
 Err: Word;
 ForwardRec, BackwardRec, FreeWord, Current, Previous, NextRec,
 PrevRec, Visited, StartWord, EndWord, Body, Center, Frame, Kind: LongInt;
 N, J, Offset: Integer;
 A, B: Double;
 Format: String;
Begin
 Total := 0;
 Assign(Kernel,Path);
 FileMode := 0;
 {$I-} Reset(Kernel,1); Err := IOResult; {$I+}
 If Err <> 0 Then Fail('Impossible d''ouvrir '+Path);
 Opened := True;
 {$I-} FileBytes := FileSize(Kernel); Err := IOResult; {$I+}
 If Err <> 0 Then Fail('Impossible de lire la taille du fichier.');
 If (FileBytes < 1024) Or (FileBytes Mod 1024 <> 0) Then
  Fail('Taille DAF invalide ou fichier tronque.');
 ReadRecord(1);
 If TextAt(0,8) <> 'DAF/SPK ' Then Fail('Fichier non SPK DAF : '+Path);
 Format := TextAt(88,8);
 If (Format <> 'LTL-IEEE') And (Format <> 'BIG-IEEE') Then
  Fail('Format binaire non pris en charge : '+Format);
 BigEndian := Format = 'BIG-IEEE';
 If (IntAt(8) <> 2) Or (IntAt(12) <> 6) Then
  Fail('Dimensions des descripteurs SPK invalides.');
 ForwardRec := IntAt(76);
 BackwardRec := IntAt(80);
 FreeWord := IntAt(84);
 If (FreeWord < 129) Or (FreeWord > FileBytes Div 8 + 1) Then
  Fail('Adresse libre DAF hors fichier.');
 If (ForwardRec = 0) And (BackwardRec = 0) Then Begin
  {$I-} Close(Kernel); Err := IOResult; {$I+}
  Opened := False;
  If Err <> 0 Then Fail('Erreur de fermeture du fichier.');
  Exit;
 End;
 If (ForwardRec < 2) Or (BackwardRec < 2) Then
  Fail('Chaine de descripteurs DAF invalide.');
 Current := ForwardRec;
 Previous := 0;
 Visited := 0;
 While Current <> 0 Do Begin
  Inc(Visited);
  If Visited > FileBytes Div 1024 Then Fail('Cycle dans la chaine DAF.');
  ReadRecord(Current);
  If Current >= FileBytes Div 1024 Then Fail('Record de noms DAF absent.');
  NextRec := ControlAt(0,FileBytes Div 1024);
  PrevRec := ControlAt(8,FileBytes Div 1024);
  N := ControlAt(16,25);
  If PrevRec <> Previous Then Fail('Lien precedent DAF incoherent.');
  For J := 0 To N-1 Do Begin
   Offset := 24+J*40;
   A := DoubleAt(Offset);
   B := DoubleAt(Offset+8);
   If A > B Then Fail('Intervalle temporel inverse.');
   Body := IntAt(Offset+16);
   Center := IntAt(Offset+20);
   Frame := IntAt(Offset+24);
   Kind := IntAt(Offset+28);
   StartWord := IntAt(Offset+32);
   EndWord := IntAt(Offset+36);
   If (StartWord < 129) Or (EndWord < StartWord) Or
      (EndWord >= FreeWord) Or (EndWord > FileBytes Div 8) Then
    Fail('Adresses de segment SPK invalides.');
   If (Frame <= 0) Or (Kind <= 0) Or (Body = Center) Then
    Fail('Descripteur de segment SPK invalide.');
   If Total = MaxSegments Then Fail('Limite de 1500 segments depassee.');
   Inc(Total);
   Coverage[Total].Body := Body;
   Coverage[Total].First := A;
   Coverage[Total].Last := B;
  End;
  Previous := Current;
  Current := NextRec;
 End;
 If Previous <> BackwardRec Then Fail('Dernier record DAF incoherent.');
 {$I-} Close(Kernel); Err := IOResult; {$I+}
 Opened := False;
 If Err <> 0 Then Fail('Erreur de fermeture du fichier.');
End;

Procedure MergeCoverage;
Var J, K, OutCount: Integer; Item: TCoverage;
Begin
 { Insertion sort keeps the DOS implementation small. }
 For J := 2 To Total Do Begin
  Item := Coverage[J]; K := J-1;
  While K > 0 Do Begin
   If (Coverage[K].Body < Item.Body) Or
      ((Coverage[K].Body = Item.Body) And (Coverage[K].First <= Item.First)) Then Break;
   Coverage[K+1] := Coverage[K]; Dec(K);
  End;
  Coverage[K+1] := Item;
 End;
 OutCount := 0;
 For J := 1 To Total Do Begin
  If OutCount > 0 Then Begin
   If (Coverage[OutCount].Body = Coverage[J].Body) And
      (Coverage[J].First <= Coverage[OutCount].Last) Then Begin
    If Coverage[J].Last > Coverage[OutCount].Last Then
     Coverage[OutCount].Last := Coverage[J].Last;
   End Else Begin
    Inc(OutCount); Coverage[OutCount] := Coverage[J];
   End;
  End Else Begin
   Inc(OutCount); Coverage[OutCount] := Coverage[J];
  End;
 End;
 Total := OutCount;
End;

Function JulianDay(Y, M, D: LongInt): LongInt;
Var A: LongInt;
Begin
 A := (14-M) Div 12;
 Y := Y+4800-A; M := M+12*A-3;
 JulianDay := D+(153*M+2) Div 5+365*Y+Y Div 4-Y Div 100+Y Div 400-32045;
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
 { Supported printable Gregorian range; -etsec has no calendar restriction. }
 R := Seconds+43200.0;
 If (R < -63082281600.0) Or (R >= 252455616000.0) Then
  Fail('Date hors plage du calendrier (0001-9999). Utiliser -etsec.');
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
 Calendar := Number(Y,4)+'-'+Number(M,2)+'-'+Number(D,2)+' '+
  Number(H,2)+':'+Number(Minute,2)+':'+Number(Sec,2)+'.'+Number(MS Mod 1000,3);
End;

Function TimeText(ET: Double): String;
Var S: String; Mean, TAI, U, Threshold, DaySeconds: Double; J, Delta: Integer;
    IsLeap: Boolean;
Begin
 If ETSeconds Then Begin Str(ET:0:6,S); TimeText := S; Exit End;
 If Not UTC Then Begin TimeText := Calendar(ET,False); Exit End;
 If (ET < -63082281600.0) Or (ET >= 252455616000.0) Then
  Fail('Date hors plage du calendrier (0001-9999). Utiliser -etsec.');
 Mean := 6.239996+1.99096871E-7*ET;
 TAI := ET-32.184-0.001657*Sin(Mean+0.01671*Sin(Mean));
 { Round on the continuous TAI scale before classifying leap seconds. }
 DaySeconds := Trunc(TAI/86400.0)*86400.0;
 TAI := DaySeconds+Round((TAI-DaySeconds)*1000.0)/1000.0;
 If TAI < LeapEpoch[1]+10 Then Fail('UTC avant 1972 non pris en charge.');
 Delta := 10; IsLeap := False; U := 0;
 For J := 2 To LeapCount Do Begin
  Threshold := LeapEpoch[J]+J+9;
  If TAI >= Threshold Then Delta := J+9
  Else If TAI >= Threshold-1 Then Begin
   IsLeap := True;
   { Encode the extra second using the preceding calendar second. }
   U := LeapEpoch[J]-1+(TAI-(Threshold-1));
  End;
 End;
 If Not IsLeap Then U := TAI-Delta;
 TimeText := Calendar(U,IsLeap);
End;

Function BodyName(ID: LongInt): String;
Var S: String;
Begin
 Str(ID,S);
 If Not NumericOnly Then Case ID Of
  0: S := 'SOLAR SYSTEM BARYCENTER (0)';
  1: S := 'MERCURY BARYCENTER (1)';
  2: S := 'VENUS BARYCENTER (2)';
  3: S := 'EARTH BARYCENTER (3)';
  4: S := 'MARS BARYCENTER (4)';
  5: S := 'JUPITER BARYCENTER (5)';
  6: S := 'SATURN BARYCENTER (6)';
  7: S := 'URANUS BARYCENTER (7)';
  8: S := 'NEPTUNE BARYCENTER (8)';
  9: S := 'PLUTO BARYCENTER (9)';
  10: S := 'SUN (10)';
  199: S := 'MERCURY (199)';
  299: S := 'VENUS (299)';
  301: S := 'MOON (301)';
  399: S := 'EARTH (399)';
  499: S := 'MARS (499)';
  599: S := 'JUPITER (599)';
  635: S := 'DAPHNIS (635)';
  699: S := 'SATURN (699)';
  799: S := 'URANUS (799)';
  899: S := 'NEPTUNE (899)';
  999: S := 'PLUTO (999)';
  -21: S := 'SOHO (-21)';
 End;
 BodyName := S;
End;

Procedure ShowSummary(Path: String);
Var J, Segments: Integer; FirstText, LastText, LabelText: String;
Begin
 ReadSPK(Path);
 Segments := Total;
 MergeCoverage;
 WriteLn;
 WriteLn('Fichier : ',Path);
 WriteLn(Segments,' segment(s), ',Total,' intervalle(s) de couverture.');
 If ETSeconds Then LabelText := 'ET/TDB secondes depuis J2000'
 Else If UTC Then LabelText := 'UTC'
 Else LabelText := 'ET/TDB';
 WriteLn('Echelle temporelle : ',LabelText);
 If UTC Then Begin
  WriteLn('UTC : table NAIF0012 integree (1972-2017).');
  WriteLn('Apres 2017, offset TAI-UTC = 37 s prolonge; pas de LSK externe.');
 End;
 If Total = 0 Then WriteLn('Aucun objet : fichier SPK vide.');
 If Tabular Then
  WriteLn('Objet / ID NAIF                   Debut                     Fin');
 For J := 1 To Total Do Begin
  FirstText := TimeText(Coverage[J].First);
  LastText := TimeText(Coverage[J].Last);
  If Tabular Then
   WriteLn(BodyName(Coverage[J].Body):32,'  ',FirstText,'  ',LastText)
  Else Begin
   If J = 1 Then WriteLn('Objet : ',BodyName(Coverage[J].Body))
   Else If Coverage[J].Body <> Coverage[J-1].Body Then
    WriteLn('Objet : ',BodyName(Coverage[J].Body));
   WriteLn('  Debut : ',FirstText);
   WriteLn('  Fin   : ',LastText);
  End;
 End;
End;

BEGIN
 Opened := False; Tabular := False; UTC := False;
 ETSeconds := False; NumericOnly := False; Files := 0;
 For ArgIndex := 1 To LeapCount Do
  LeapEpoch[ArgIndex] := (JulianDay(LeapYears[ArgIndex],LeapMonths[ArgIndex],1)-2451545)*86400.0-43200.0;
 If ParamCount = 0 Then Begin Help; Halt(0) End;
 { Parse all options before processing files, regardless of their position. }
 For ArgIndex := 1 To ParamCount Do Begin
  Arg := Upper(ParamStr(ArgIndex));
  If (Arg = '-H') Or (Arg = '-HELP') Or (Arg = '--HELP') Or
     (Arg = '/?') Then Begin Help; Halt(0) End;
  If Arg = '-T' Then Tabular := True
  Else If Arg = '-UTC' Then UTC := True
  Else If Arg = '-ETSEC' Then ETSeconds := True
  Else If Arg = '-N' Then NumericOnly := True
  Else If Arg = '' Then SyntaxError('Nom de fichier vide.')
  Else If Arg[1] = '-' Then SyntaxError('Option inconnue : '+ParamStr(ArgIndex))
  Else Inc(Files);
 End;
 If Files = 0 Then SyntaxError('Fichier SPK requis.');
 If UTC And ETSeconds Then SyntaxError('-utc et -etsec sont incompatibles.');
 For ArgIndex := 1 To ParamCount Do Begin
  Arg := ParamStr(ArgIndex);
  If Arg[1] <> '-' Then ShowSummary(Arg);
 End;
END.
