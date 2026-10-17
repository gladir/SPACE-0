{ @author: Sylvain Maltais (support@gladir.com)
 @created: 2026
 @website(https://www.gladir.com/space-0)
 @abstract(Target: Turbo Pascal 7, Free Pascal 3.2) }

Program OEM2SPK;

{$A-}
{$E+,N+}

{ Conversion d'un message d'ephemerides CCSDS OEM (texte KVN, versions 1 a 3)
  en fichier SPK binaire DAF, sous-ensemble autonome de OEM2SPK NAIF :
  aucune unite, aucune inclusion.
  Chaque bloc META_START ... META_STOP et ses lignes d'etats devient un
  segment SPK de type 9 (Lagrange) ou 13 (Hermite), selon INTERPOLATION.
  Les accelerations (colonnes 8 a 10) et les sections de covariance sont
  ignorees. Km et km/s; fichier ecrit en LTL-IEEE.
  REF_FRAME : EME2000, J2000, ICRF ou GCRF (repere SPICE J2000), ECLIPJ2000.
  TIME_SYSTEM : UTC (table NAIF0012 integree, 1972-2017, 37 s ensuite),
  TAI, TT ou TDB. Epoques AAAA-MM-JJThh:mm:ss[.f] ou AAAA-JJJThh:mm:ss[.f].
  Les enregistrements d'etats sont ecrits en flux; les epoques passent par
  un fichier temporaire (.$$$) afin de respecter l'ordre des segments type
  9/13 (etats, epochs, repertoire, degre, nombre d'etats).
  Les enregistrements sommaires DAF (et leurs noms) sont reserves avant les
  donnees, apres la zone de commentaires, comme dans un SPK usuel; le nombre
  de blocs META est connu par une premiere lecture rapide de l'OEM.
  Sources : naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/ug/oem2spk.html
            public.ccsds.org/Pubs/502x0b2c1.pdf (CCSDS 502.0-B-2)
            naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/spk.html
            naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/daf.html
            naif.jpl.nasa.gov/pub/naif/generic_kernels/lsk/naif0012.tls
  Compilation : fpc -Mtp OEM2SPK.PAS
  Limites DOS : 100 segments, lignes de 255 caracteres. }

Const
 MaxSeg = 100;
 LeapCount = 28;
 LeapYears: Array[1..LeapCount] Of Integer =
  (1972,1972,1973,1974,1975,1976,1977,1978,1979,1980,1981,1982,
   1983,1985,1988,1990,1991,1992,1993,1994,1996,1997,1999,2006,
   2009,2012,2015,2017);
 LeapMonths: Array[1..LeapCount] Of Integer =
  (1,7,1,1,1,1,1,1,1,1,7,7,7,7,1,1,1,7,7,7,1,7,1,1,1,7,7,1);
 MinEpoch = -63082324800.0;
 MaxEpoch = 252455572800.0;
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
 { Chaine de validation FTP du DAF (28 octets, a partir de l'octet 700). }
 FtpString: String[28] =
  'FTPSTR:'#13':'#10':'#13#10':'#13#0':'#129':'#16#206':ENDFTP';

Type
 TRecord = Array[0..1023] Of Byte;
 TSegInfo = Record
  First, Last: Double;
  Body, Center, Frame, Kind, Start, Finish: LongInt;
  Name: String[40];
 End;

Var
 LeapEpoch: Array[1..LeapCount] Of Double;
 Segs: Array[1..MaxSeg] Of TSegInfo;
 SegCount: Integer;
 OemFile: Text;
 OutFile, TmpFile: File;
 OemOpen, OutOpen, TmpOpen: Boolean;
 Total, MetaCount: Integer;
 FreeWord: LongInt;
 OemName, OutName, TmpName: String;
 Block: Array[0..127] Of Double;
 BlockFill: Integer;
 WordAddr: LongInt;
 LineNo: LongInt;
 { Options }
 OptBody, OptCenter: LongInt;
 HasBody, HasCenter, Force, HasType, HasDegree: Boolean;
 OptType, OptDegree: Integer;
 { Metadonnees du bloc en cours }
 MObjName, MObjID, MCenter, MFrame, MTimeSys, MInterp, MDegree: String;
 InMeta, InData, InCov, SegOpen: Boolean;
 CurBody, CurCenter, CurFrame: LongInt;
 CurName: String;
 CurStart: LongInt;
 CurCount: LongInt;
 CurFirst, CurLast: Double;
 CurTimeSys: String;
 StateCount: LongInt;

Procedure Cleanup(KeepOutput: Boolean);
Var Err: Word;
Begin
 If TmpOpen Then Begin {$I-} Close(TmpFile); Err := IOResult; {$I+} TmpOpen := False End;
 If TmpName <> '' Then Begin
  Assign(TmpFile,TmpName);
  {$I-} Erase(TmpFile); Err := IOResult; {$I+}
 End;
 If OutOpen Then Begin {$I-} Close(OutFile); Err := IOResult; {$I+} OutOpen := False End;
 If (Not KeepOutput) And (OutName <> '') And (SegCount >= 0) Then Begin
  Assign(OutFile,OutName);
  {$I-} Erase(OutFile); Err := IOResult; {$I+}
 End;
 If OemOpen Then Begin {$I-} Close(OemFile); Err := IOResult; {$I+} OemOpen := False End;
End;

Procedure Fail(S: String; Code: Integer);
Begin
 If (LineNo > 0) And (Code = 1) Then WriteLn('Erreur (ligne ',LineNo,') : ',S)
 Else WriteLn('Erreur : ',S);
 Cleanup(False);
 Halt(Code);
End;

Procedure SyntaxError(S: String);
Begin
 LineNo := 0;
 Fail(S+' Voir OEM2SPK -h.',2);
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

Procedure Help;
Begin
 WriteLn('OEM2SPK - Cette commande permet de convertir un fichier d''',#130,'ph',#130,
         'm',#130,'rides OEM vers SPK.');
 WriteLn;
 WriteLn('Syntaxe : OEM2SPK [options] <fichier.oem> <fichier.bsp>');
 WriteLn;
 WriteLn('Options :');
 WriteLn('  -body <id>      Identifiant NAIF du corps (OBJECT_ID numerique ou');
 WriteLn('                  OBJECT_NAME connu par defaut)');
 WriteLn('  -center <id>    Identifiant NAIF du centre (CENTER_NAME par defaut)');
 WriteLn('  -ty <type>      Type de segment : 9 (Lagrange) ou 13 (Hermite);');
 WriteLn('                  sinon selon INTERPOLATION (LAGRANGE par defaut)');
 WriteLn('  -g <degre>      Degre d''interpolation, 1 a 27 (INTERPOLATION_DEGREE');
 WriteLn('                  par defaut, sinon 7); impair et ajuste pour le type 13');
 WriteLn('  -f              Ecrase le fichier SPK s''il existe');
 WriteLn('  -h, /?          Aide (-help et --help acceptes)');
 WriteLn;
 WriteLn('Noms de corps/centre : SSB, EMB, SUN, MOON, EARTH, MARS, JUPITER, etc.,');
 WriteLn('ou numeros NAIF.');
 WriteLn;
 WriteLn('Exemples :');
 WriteLn('  OEM2SPK mission.oem mission.bsp');
 WriteLn('  OEM2SPK -body -999 -center 399 -ty 13 -g 7 mission.oem mission.bsp');
 WriteLn;
 WriteLn('Un segment par bloc META; etats km et km/s; repere EME2000, ICRF, GCRF,');
 WriteLn('J2000 ou ECLIPJ2000; temps UTC, TAI, TT ou TDB. Accelerations et');
 WriteLn('covariances ignorees. Le SPK produit n''a pas de PCK ni de LSK.');
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

{ Secondes depuis J2000 (midi) du debut du jour de numero JD; temporaire
  Double : un produit entier*constante reelle serait calcule en simple
  precision par Free Pascal. }
Function JDStart(JD: LongInt): Double;
Var T: Double;
Begin
 T := JD-2451545;
 JDStart := T*86400.0-43200.0;
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

Function ParseInt(S: String; Var V: LongInt): Boolean;
Var Code: Integer;
Begin
 ParseInt := False;
 If (S = '') Or (Length(S) > 9) Then Exit;
 Val(S,V,Code);
 If (Code = 0) And (V >= 0) Then ParseInt := True;
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
 If (V < -1.0E30) Or (V > 1.0E30) Then Exit;
 ParseNumber := True;
End;

Function NextField(Var S: String; Sep: Char): String;
Var P: Integer;
Begin
 P := Pos(Sep,S);
 If P = 0 Then Begin NextField := S; S := '' End
 Else Begin NextField := Copy(S,1,P-1); Delete(S,1,P) End;
End;

{ Epoque OEM : secondes de l'echelle calendrier (midi J2000) et seconde 60. }
Function ParseEpoch(Text: String; Var U: Double; Var Extra: Boolean): Boolean;
Var S, DatePart, TimePart, F1, F2, F3: String;
    Y, M, D, H, Mi, DOY: LongInt; Sec: Double; P: Integer; Three: Boolean;
Begin
 ParseEpoch := False;
 Extra := False;
 S := Trim(Text);
 If (S <> '') And (UpCase(S[Length(S)]) = 'Z') Then Delete(S,Length(S),1);
 P := Pos('T',Upper(S));
 If P = 0 Then Exit;
 DatePart := Copy(S,1,P-1); TimePart := Copy(S,P+1,255);
 F1 := NextField(DatePart,'-'); F2 := NextField(DatePart,'-');
 F3 := DatePart;
 Three := F3 <> '';
 If Not ParseInt(F1,Y) Then Exit;
 If (Y < 1) Or (Y > 9999) Then Exit;
 If Three Then Begin
  If Not ParseInt(F2,M) Or Not ParseInt(F3,D) Then Exit;
  If (M < 1) Or (M > 12) Or (D < 1) Or (D > DaysInMonth(Y,M)) Then Exit;
  DOY := JulianDay(Y,M,D);
 End Else Begin
  If Not ParseInt(F2,DOY) Then Exit;
  If (DOY < 1) Or (DOY > 365+Ord(((Y Mod 4 = 0) And (Y Mod 100 <> 0)) Or (Y Mod 400 = 0))) Then Exit;
  DOY := JulianDay(Y,1,1)+DOY-1;
 End;
 F1 := NextField(TimePart,':');
 If Not ParseInt(F1,H) Then Exit;
 F1 := NextField(TimePart,':');
 If Not ParseInt(F1,Mi) Then Exit;
 Sec := 0;
 If TimePart <> '' Then
  If Not ParseNumber(TimePart,Sec) Then Exit;
 If (H > 23) Or (Mi > 59) Or (Sec < 0) Or (Sec >= 61) Then Exit;
 If Sec >= 60 Then Extra := True;
 U := JDStart(DOY)+(H*3600.0+Mi*60.0)+Sec;
 ParseEpoch := True;
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

Function UTCtoET(U: Double; Extra: Boolean): Double;
Var K, Offset: Integer; Found: Boolean;
Begin
 If U < LeapEpoch[1] Then Fail('UTC avant 1972 non pris en charge. Utiliser TIME_SYSTEM = TDB.',1);
 Offset := 10;
 If Extra Then Begin
  Found := False;
  For K := 2 To LeapCount Do
   If (U >= LeapEpoch[K]) And (U < LeapEpoch[K]+1) Then Begin
    Offset := K+8; Found := True;
   End;
  If Not Found Then Fail('Seconde intercalaire inexistante.',1);
 End Else
  For K := 2 To LeapCount Do If U >= LeapEpoch[K] Then Offset := K+9;
 UTCtoET := TAItoET(U+Offset);
End;

Function ToET(U: Double; Extra: Boolean): Double;
Var V: Double;
Begin
 If Extra And (CurTimeSys <> 'UTC') Then Fail('Seconde 60 reservee a UTC.',1);
 If CurTimeSys = 'UTC' Then V := UTCtoET(U,Extra)
 Else If CurTimeSys = 'TAI' Then V := TAItoET(U)
 Else If CurTimeSys = 'TT' Then V := TAItoET(U-32.184)
 Else V := U;
 If (V < MinEpoch) Or (V >= MaxEpoch) Then Fail('Epoque hors plage du calendrier.',1);
 ToET := V;
End;

{ ------------------------------------------------------------------ }
{ Ecriture DAF                                                       }
{ ------------------------------------------------------------------ }

Procedure WriteRecord(Var Rec);
Var Err: Word;
Begin
 {$I-} BlockWrite(OutFile,Rec,1); Err := IOResult; {$I+}
 If Err <> 0 Then Fail('Erreur d''ecriture de '+OutName+' (disque plein ?).',1);
End;

Procedure PutWord(V: Double);
Begin
 Block[BlockFill] := V;
 Inc(BlockFill);
 Inc(WordAddr);
 If BlockFill = 128 Then Begin
  WriteRecord(Block);
  BlockFill := 0;
 End;
End;

Procedure PadRecord;
Begin
 While BlockFill <> 0 Do PutWord(0);
End;

Procedure PutEpoch(V: Double);
Var Err: Word;
Begin
 {$I-} BlockWrite(TmpFile,V,1); Err := IOResult; {$I+}
 If Err <> 0 Then Fail('Erreur d''ecriture du fichier temporaire '+TmpName+'.',1);
End;

Function GetEpoch(Index: LongInt): Double;
Var V: Double; Got, Err: Word;
Begin
 {$I-}
 Seek(TmpFile,Index);
 BlockRead(TmpFile,V,1,Got);
 Err := IOResult;
 {$I+}
 If (Err <> 0) Or (Got <> 1) Then Fail('Erreur de lecture du fichier temporaire.',1);
 GetEpoch := V;
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

Function FrameFromText(S: String): LongInt;
Var U: String;
Begin
 U := Upper(Trim(S));
 FrameFromText := 0;
 If (U = 'EME2000') Or (U = 'J2000') Or (U = 'ICRF') Or (U = 'GCRF') Then
  FrameFromText := 1
 Else If U = 'ECLIPJ2000' Then FrameFromText := 17;
End;

Procedure ResetMeta;
Begin
 MObjName := ''; MObjID := ''; MCenter := ''; MFrame := ''; MTimeSys := '';
 MInterp := ''; MDegree := '';
End;

Procedure BeginSegment;
Var T: String; Code: Integer;
Begin
 If MFrame = '' Then Fail('REF_FRAME manquant dans le bloc META.',1);
 If MTimeSys = '' Then Fail('TIME_SYSTEM manquant dans le bloc META.',1);
 If (MCenter = '') And Not HasCenter Then Fail('CENTER_NAME manquant dans le bloc META.',1);
 CurFrame := FrameFromText(MFrame);
 If CurFrame = 0 Then
  Fail('REF_FRAME non pris en charge : '+MFrame+' (EME2000, ICRF, GCRF, J2000, ECLIPJ2000).',1);
 CurTimeSys := Upper(MTimeSys);
 If (CurTimeSys <> 'UTC') And (CurTimeSys <> 'TAI') And (CurTimeSys <> 'TT') And
    (CurTimeSys <> 'TDB') Then
  Fail('TIME_SYSTEM non pris en charge : '+MTimeSys+' (UTC, TAI, TT ou TDB).',1);
 If HasBody Then CurBody := OptBody
 Else If Not BodyFromText(MObjID,CurBody) Then Begin
  If Not BodyFromText(MObjName,CurBody) Then
   Fail('Corps inconnu (OBJECT_ID '+MObjID+', OBJECT_NAME '+MObjName+'). Utiliser -body.',1);
 End;
 If HasCenter Then CurCenter := OptCenter
 Else If Not BodyFromText(MCenter,CurCenter) Then
  Fail('Centre inconnu : '+MCenter+'. Utiliser -center.',1);
 If CurBody = CurCenter Then Fail('Le corps et le centre sont identiques.',1);
 T := MObjName;
 If T = '' Then T := Num(CurBody);
 If Length(T) > 40 Then T := Copy(T,1,40);
 CurName := T;
 CurStart := WordAddr;
 { Le fichier temporaire est reutilise a partir du debut pour chaque segment. }
 {$I-} Seek(TmpFile,0); Code := IOResult; {$I+}
 If Code <> 0 Then Fail('Positionnement impossible dans '+TmpName+'.',1);
 CurCount := 0;
 CurFirst := 0; CurLast := 0;
 SegOpen := True;
 InData := True;
End;

Procedure EndSegment;
Var Typ, Deg, Window: Integer; V: LongInt; Dir: LongInt; Code: Integer;
Begin
 If Not SegOpen Then Exit;
 If CurCount < 2 Then Fail('Un segment exige au moins 2 etats (bloc '+Num(SegCount+1)+').',1);
 If SegCount = MaxSeg Then Fail('Limite de '+Num(MaxSeg)+' segments depassee.',1);
 If HasType Then Typ := OptType
 Else If Upper(MInterp) = 'HERMITE' Then Typ := 13
 Else Typ := 9;
 If HasDegree Then Deg := OptDegree
 Else Begin
  Val(Trim(MDegree),V,Code);
  If (Code = 0) And (V >= 1) And (V <= 27) Then Deg := V Else Deg := 7;
 End;
 If Typ = 9 Then Begin
  If Deg > CurCount-1 Then Begin
   Deg := CurCount-1;
   WriteLn('Avertissement : degre ramene a ',Deg,' (', CurCount,' etats).');
  End;
 End Else Begin
  If Not Odd(Deg) Then Deg := Deg+1;
  If Deg > 27 Then Deg := 27;
  Window := (Deg+1) Div 2;
  If Window > CurCount Then Begin
   Deg := 2*CurCount-1;
   WriteLn('Avertissement : degre ramene a ',Deg,' (',CurCount,' etats).');
  End;
 End;
 { Epochs (a partir du fichier temporaire). }
 For V := 0 To CurCount-1 Do PutWord(GetEpoch(V));
 { Repertoire : epoques 100, 200, ... (sauf la derniere si N est multiple de 100). }
 Dir := (CurCount-1) Div 100;
 For V := 1 To Dir Do PutWord(GetEpoch(100*V-1));
 PutWord(Deg);
 PutWord(CurCount);
 Inc(SegCount);
 Segs[SegCount].First := CurFirst; Segs[SegCount].Last := CurLast;
 Segs[SegCount].Body := CurBody; Segs[SegCount].Center := CurCenter;
 Segs[SegCount].Frame := CurFrame; Segs[SegCount].Kind := Typ;
 Segs[SegCount].Start := CurStart; Segs[SegCount].Finish := WordAddr-1;
 Segs[SegCount].Name := CurName;
 StateCount := StateCount+CurCount;
 SegOpen := False; InData := False;
End;

Procedure ProcessMetaLine(S: String);
Var P: Integer; Key, Value: String;
Begin
 P := Pos('=',S);
 If P = 0 Then Fail('Ligne META sans signe "=" : '+S,1);
 Key := Upper(Trim(Copy(S,1,P-1)));
 Value := Trim(Copy(S,P+1,255));
 If Key = 'OBJECT_NAME' Then MObjName := Value
 Else If Key = 'OBJECT_ID' Then MObjID := Value
 Else If Key = 'CENTER_NAME' Then MCenter := Value
 Else If Key = 'REF_FRAME' Then MFrame := Value
 Else If Key = 'TIME_SYSTEM' Then MTimeSys := Value
 Else If Key = 'INTERPOLATION' Then MInterp := Value
 Else If Key = 'INTERPOLATION_DEGREE' Then MDegree := Value;
End;

Function Token(Var S: String): String;
Var P: Integer;
Begin
 While (Length(S) > 0) And (S[1] <= ' ') Do Delete(S,1,1);
 P := 1;
 While (P <= Length(S)) And (S[P] > ' ') Do Inc(P);
 Token := Copy(S,1,P-1);
 Delete(S,1,P-1);
End;

Procedure ProcessDataLine(S: String);
Var T: String; N, K: Integer; U, ET, V: Double; Extra: Boolean;
    X: Array[1..6] Of Double;
Begin
 T := Token(S);
 If Not ParseEpoch(T,U,Extra) Then Fail('Epoque invalide : '+T,1);
 ET := ToET(U,Extra);
 N := 0;
 While Trim(S) <> '' Do Begin
  T := Token(S);
  If Not ParseNumber(T,V) Then Fail('Nombre invalide : '+T,1);
  Inc(N);
  If N <= 6 Then X[N] := V;
 End;
 If (N <> 6) And (N <> 9) Then
  Fail('Ligne d''etat attendue avec 6 ou 9 valeurs apres l''epoque ('+Num(N)+' trouvees).',1);
 If (CurCount > 0) And (ET <= CurLast) Then
  Fail('Epoques non strictement croissantes.',1);
 If CurCount = 0 Then CurFirst := ET;
 CurLast := ET;
 For K := 1 To 6 Do PutWord(X[K]);
 PutEpoch(ET);
 Inc(CurCount);
End;

Procedure ProcessLine(Raw: String);
Var S, W: String;
Begin
 S := Trim(Raw);
 If S = '' Then Exit;
 W := Upper(S);
 If InCov Then Begin
  If Copy(W,1,14) = 'COVARIANCE_STO' Then InCov := False;
  Exit;
 End;
 If Copy(W,1,7) = 'COMMENT' Then Exit;
 If Copy(W,1,16) = 'COVARIANCE_START' Then Begin InCov := True; Exit End;
 If W = 'META_START' Then Begin
  EndSegment;
  ResetMeta; InMeta := True; InData := False;
  Exit;
 End;
 If W = 'META_STOP' Then Begin
  If Not InMeta Then Fail('META_STOP sans META_START.',1);
  InMeta := False;
  BeginSegment;
  Exit;
 End;
 If InMeta Then Begin ProcessMetaLine(S); Exit End;
 If InData Then Begin ProcessDataLine(S); Exit End;
 { En-tete : CCSDS_OEM_VERS, CREATION_DATE, ORIGINATOR, ... }
 If Pos('=',S) = 0 Then Fail('Ligne inattendue avant le premier bloc META : '+S,1);
End;

Function OptionValue(Var Index: Integer): String;
Begin
 Inc(Index);
 If Index > ParamCount Then SyntaxError('Valeur manquante pour '+ParamStr(Index-1)+'.');
 OptionValue := ParamStr(Index);
End;

Function ExistsFile(Path: String): Boolean;
Var T: File; Err: Word;
Begin
 Assign(T,Path);
 FileMode := 0;
 {$I-} Reset(T,1); Err := IOResult; {$I+}
 If Err = 0 Then Begin
  {$I-} Close(T); Err := IOResult; {$I+}
  ExistsFile := True;
 End Else ExistsFile := False;
End;

{ Nom du fichier temporaire : meme nom avec l'extension .$$$ }
Function TempName(Path: String): String;
Var K, Cut: Integer;
Begin
 Cut := 0;
 For K := Length(Path) DownTo 1 Do
  If (Path[K] = '\') Or (Path[K] = '/') Or (Path[K] = ':') Then Break
  Else If (Path[K] = '.') And (Cut = 0) Then Cut := K;
 If Cut = 0 Then TempName := Path+'.$$$'
 Else TempName := Copy(Path,1,Cut-1)+'.$$$';
End;

Procedure PutInt(Var Rec: TRecord; Offset: Integer; V: LongInt);
Begin
 Move(V,Rec[Offset],4);
End;

Procedure PutDouble(Var Rec: TRecord; Offset: Integer; V: Double);
Begin
 Move(V,Rec[Offset],8);
End;

Procedure WriteDirectory;
Var
 Rec: TRecord;
 R, J, K, N, Offset, FirstSum: Integer;
 Nm: String;
 Fwd, Bwd, Free: LongInt;
 Err: Word;
Begin
 { Enregistrements 1 (fichier), 2 (commentaires), puis sommaires + noms. }
 FirstSum := 3;
 Free := FreeWord;
 Fwd := FirstSum;
 Bwd := FirstSum+2*(Total-1);
 FillChar(Rec,SizeOf(Rec),0);
 Move('DAF/SPK ',Rec[0],8);
 PutInt(Rec,8,2); PutInt(Rec,12,6);
 Nm := 'OEM2SPK '+OemName;
 While Length(Nm) < 60 Do Nm := Nm+' ';
 For K := 1 To 60 Do Rec[15+K] := Ord(Nm[K]);
 PutInt(Rec,76,Fwd); PutInt(Rec,80,Bwd); PutInt(Rec,84,Free);
 Move('LTL-IEEE',Rec[88],8);
 For K := 1 To 28 Do Rec[698+K] := Ord(FtpString[K]);
 {$I-} Seek(OutFile,0); Err := IOResult; {$I+}
 If Err <> 0 Then Fail('Positionnement impossible dans '+OutName+'.',1);
 WriteRecord(Rec);
 FillChar(Rec,SizeOf(Rec),32);
 Nm := 'Genere par OEM2SPK a partir de '+OemName;
 For K := 1 To Length(Nm) Do Rec[K-1] := Ord(Nm[K]);
 Rec[Length(Nm)] := 0;
 Rec[Length(Nm)+1] := 4;
 WriteRecord(Rec);
 For R := 1 To Total Do Begin
  FillChar(Rec,SizeOf(Rec),0);
  If R < Total Then PutDouble(Rec,0,FirstSum+2*R) Else PutDouble(Rec,0,0);
  If R > 1 Then PutDouble(Rec,8,FirstSum+2*(R-2)) Else PutDouble(Rec,8,0);
  N := SegCount-25*(R-1);
  If N > 25 Then N := 25;
  PutDouble(Rec,16,N);
  For J := 0 To N-1 Do Begin
   K := 25*(R-1)+J+1;
   Offset := 24+J*40;
   PutDouble(Rec,Offset,Segs[K].First);
   PutDouble(Rec,Offset+8,Segs[K].Last);
   PutInt(Rec,Offset+16,Segs[K].Body);
   PutInt(Rec,Offset+20,Segs[K].Center);
   PutInt(Rec,Offset+24,Segs[K].Frame);
   PutInt(Rec,Offset+28,Segs[K].Kind);
   PutInt(Rec,Offset+32,Segs[K].Start);
   PutInt(Rec,Offset+36,Segs[K].Finish);
  End;
  WriteRecord(Rec);
  FillChar(Rec,SizeOf(Rec),32);
  For J := 0 To N-1 Do Begin
   K := 25*(R-1)+J+1;
   Nm := Segs[K].Name;
   For Offset := 1 To Length(Nm) Do Rec[J*40+Offset-1] := Ord(Nm[Offset]);
  End;
  WriteRecord(Rec);
 End;
End;

Var
 I, Code, ArgIndex: Integer;
 Arg, Line: String;
 PathCount: Integer;
 Zero: TRecord;
 Err: Word;

Begin
 OemOpen := False; OutOpen := False; TmpOpen := False;
 OemName := ''; OutName := ''; TmpName := ''; SegCount := -1; LineNo := 0;
 HasBody := False; HasCenter := False; Force := False; HasType := False;
 HasDegree := False; OptType := 9; OptDegree := 7; OptBody := 0; OptCenter := 0;
 InMeta := False; InData := False; InCov := False; SegOpen := False;
 StateCount := 0; PathCount := 0;
 For I := 1 To LeapCount Do
  LeapEpoch[I] := JDStart(JulianDay(LeapYears[I],LeapMonths[I],1));
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
  If Arg = '-BODY' Then Begin
   Line := OptionValue(ArgIndex);
   If Not BodyFromText(Line,OptBody) Then SyntaxError('Corps inconnu : '+Line);
   HasBody := True;
  End Else If Arg = '-CENTER' Then Begin
   Line := OptionValue(ArgIndex);
   If Not BodyFromText(Line,OptCenter) Then SyntaxError('Centre inconnu : '+Line);
   HasCenter := True;
  End Else If Arg = '-TY' Then Begin
   Line := OptionValue(ArgIndex);
   Val(Line,OptType,Code);
   If (Code <> 0) Or ((OptType <> 9) And (OptType <> 13)) Then
    SyntaxError('-ty doit valoir 9 ou 13.');
   HasType := True;
  End Else If Arg = '-G' Then Begin
   Line := OptionValue(ArgIndex);
   Val(Line,OptDegree,Code);
   If (Code <> 0) Or (OptDegree < 1) Or (OptDegree > 27) Then
    SyntaxError('-g doit etre de 1 a 27.');
   HasDegree := True;
  End Else If Arg = '-F' Then Force := True
  Else If Arg = '' Then SyntaxError('Nom de fichier vide.')
  Else If Arg[1] = '-' Then
   SyntaxError('Option inconnue ou non prise en charge : '+ParamStr(ArgIndex))
  Else Begin
   Inc(PathCount);
   If PathCount = 1 Then OemName := ParamStr(ArgIndex)
   Else If PathCount = 2 Then OutName := ParamStr(ArgIndex)
   Else SyntaxError('Deux fichiers seulement : OEM puis SPK.');
  End;
  Inc(ArgIndex);
 End;
 If PathCount < 2 Then SyntaxError('Fichier OEM et fichier SPK requis.');
 If Upper(OemName) = Upper(OutName) Then SyntaxError('Les fichiers OEM et SPK doivent differer.');
 If ExistsFile(OutName) And Not Force Then Begin
  WriteLn('Erreur : ',OutName,' existe deja. Utiliser -f pour l''ecraser.');
  Halt(1);
 End;

 { Ouvertures, puis premiere lecture : nombre de blocs META. }
 Assign(OemFile,OemName);
 FileMode := 0;
 {$I-} Reset(OemFile); Err := IOResult; {$I+}
 If Err <> 0 Then Fail('Impossible d''ouvrir '+OemName+'.',1);
 OemOpen := True;
 MetaCount := 0;
 While Not EOF(OemFile) Do Begin
  {$I-} ReadLn(OemFile,Line); Err := IOResult; {$I+}
  If Err <> 0 Then Fail('Erreur de lecture de '+OemName+'.',1);
  If Upper(Trim(Line)) = 'META_START' Then Inc(MetaCount);
 End;
 If MetaCount = 0 Then Fail('Aucun bloc META_START dans '+OemName+'.',1);
 If MetaCount > MaxSeg Then Fail('Limite de '+Num(MaxSeg)+' segments depassee.',1);
 Total := (MetaCount+24) Div 25;
 {$I-} Close(OemFile); Reset(OemFile); Err := IOResult; {$I+}
 If Err <> 0 Then Fail('Impossible de relire '+OemName+'.',1);
 Assign(OutFile,OutName);
 {$I-} Rewrite(OutFile,1024); Err := IOResult; {$I+}
 If Err <> 0 Then Fail('Impossible de creer '+OutName+'.',1);
 OutOpen := True;
 SegCount := 0;
 TmpName := TempName(OutName);
 Assign(TmpFile,TmpName);
 {$I-} Rewrite(TmpFile,8); Err := IOResult; {$I+}
 If Err <> 0 Then Fail('Impossible de creer le fichier temporaire '+TmpName+'.',1);
 TmpOpen := True;

 { Enregistrements reserves : fichier, commentaires, sommaires et noms. }
 FillChar(Zero,SizeOf(Zero),0);
 For I := 1 To 2+2*Total Do WriteRecord(Zero);
 BlockFill := 0; WordAddr := (2+2*Total)*128+1;

 { Lecture de l'OEM : un segment par bloc META. }
 While Not EOF(OemFile) Do Begin
  {$I-} ReadLn(OemFile,Line); Err := IOResult; {$I+}
  If Err <> 0 Then Fail('Erreur de lecture de '+OemName+'.',1);
  Inc(LineNo);
  If LineNo = 1 Then Begin
   If Copy(Upper(Trim(Line)),1,14) <> 'CCSDS_OEM_VERS' Then
    Fail('Le fichier ne commence pas par CCSDS_OEM_VERS : ce n''est pas un OEM.',1);
  End;
  ProcessLine(Line);
 End;
 If InMeta Then Fail('META_STOP manquant.',1);
 LineNo := 0;
 EndSegment;
 If SegCount = 0 Then Fail('Aucun bloc META avec des etats dans '+OemName+'.',1);

 FreeWord := WordAddr;
 PadRecord;
 WriteDirectory;
 Cleanup(True);
 WriteLn('SPK ecrit : ',OutName,' (',SegCount,' segment(s), ',StateCount,' etat(s)).');
End.
