{ @author: Sylvain Maltais (support@gladir.com)
  @created: 2026
  @website(https://www.gladir.com/space-0)
  @abstract(Target: Turbo Pascal 7, Free Pascal 3.2) }
Program CHRONOS;
{$IFNDEF FPC}
 {$N+,E+}
{$ENDIF}
Uses Dos;

{ Convertisseur autonome, sous-ensemble de CHRONOS NAIF.
  UTC/SCET, ET/SCET et ET/SECONDS; extensions TAI, TT, JD, MJD, DOY.
  Pas de SCLK, LST, ERT, ETT, LT, meta-kernels ni ephemerides.
  Modele DELTET de NAIF0012; LSK externe facultatif.
  Sources : naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/ug/chronos.html
            naif.jpl.nasa.gov/pub/naif/generic_kernels/lsk/naif0012.tls
  UTC a partir de la premiere entree LSK, dernier offset prolonge ensuite.
  Calendrier gregorien 0001-9999, precision sortie calendrier 1 ms.
  Compilation : fpc -Mtp CHRONOS.PAS }
Const
 MaxLeaps = 128;
 MaxKernelText = 16384;
 MinEpoch = -63082324800.0;
 MaxEpoch = 252455572800.0;
 BuiltinYears: Array[1..28] Of Integer =
  (1972,1972,1973,1974,1975,1976,1977,1978,1979,1980,1981,1982,
   1983,1985,1988,1990,1991,1992,1993,1994,1996,1997,1999,2006,
   2009,2012,2015,2017);
 BuiltinMonths: Array[1..28] Of Integer =
  (1,7,1,1,1,1,1,1,1,1,7,7,7,7,1,1,1,7,7,7,1,7,1,1,1,7,7,1);
 MonthNames: Array[1..12] Of String[3] =
  ('JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC');
Type
 TCalendar = Record
  Y, M, D, H, Minute, Second, MS, DOY: Integer;
 End;
Var
 LeapEpoch: Array[1..MaxLeaps] Of Double;
 LeapOffset: Array[1..MaxLeaps] Of Integer;
 LeapCount: Integer;
 DeltaTA, K, EB, M0, M1: Double;
 KernelText: Array[1..MaxKernelText] Of Char;
 KernelLength, TokenPos: Integer;
 FromSystem, ToSystem, FromType, ToType, Picture, TimeInput, SetupPath: String;
 Batch, NoLabel, Trace, HasTime: Boolean;
 ArgIndex: Integer;
 Arg, Line: String;

Procedure Fail(S: String; Code: Integer);
Begin
 WriteLn('Erreur : ',S);
 Halt(Code);
End;

Function Upper(S: String): String;
Var J: Integer;
Begin
 For J := 1 To Length(S) Do S[J] := UpCase(S[J]);
 Upper := S;
End;

Function Trim(S: String): String;
Begin
 While Length(S) > 0 Do Begin
  If Not (S[1] In [' ',#9,#13,#10]) Then Break;
  Delete(S,1,1);
 End;
 While Length(S) > 0 Do Begin
  If Not (S[Length(S)] In [' ',#9,#13,#10]) Then Break;
  Delete(S,Length(S),1);
 End;
 Trim := S;
End;

Function Num(N: LongInt; Width: Integer): String;
Var S: String;
Begin
 Str(N,S);
 While Length(S) < Width Do S := '0'+S;
 Num := S;
End;

Function RealNumber(S: String): Double;
Var V: Double; Error, J: Integer; B: Array[0..7] Of Byte;
Begin
 S := Upper(Trim(S));
 If S = '' Then Fail('Nombre manquant.',1);
 For J := 1 To Length(S) Do Begin
  If S[J] = 'D' Then S[J] := 'E';
  If Not (S[J] In ['0'..'9','+','-','.','E']) Then
   Fail('Nombre invalide : '+S,1);
 End;
 Val(S,V,Error);
 If Error <> 0 Then Fail('Nombre invalide : '+S,1);
 Move(V,B,8);
 If ((B[7] And $7F) = $7F) And ((B[6] And $F0) = $F0) Then
  Fail('Nombre non fini.',1);
 If Abs(V) > 1.0E12 Then Fail('Nombre hors limites.',1);
 RealNumber := V;
End;

Function IntegerNumber(S: String): Integer;
Var J, V, Error: Integer;
Begin
 If S = '' Then Fail('Champ de date manquant.',1);
 For J := 1 To Length(S) Do
  If Not (S[J] In ['0'..'9']) Then Fail('Champ de date invalide : '+S,1);
 Val(S,V,Error);
 If Error <> 0 Then Fail('Champ de date hors limites : '+S,1);
 IntegerNumber := V;
End;

Function IsLeapYear(Y: Integer): Boolean;
Begin
 IsLeapYear := (Y Mod 4 = 0) And ((Y Mod 100 <> 0) Or (Y Mod 400 = 0));
End;

Function DaysInMonth(Y, M: Integer): Integer;
Const Days: Array[1..12] Of Integer = (31,28,31,30,31,30,31,31,30,31,30,31);
Begin
 If (M < 1) Or (M > 12) Then Fail('Mois invalide.',1);
 DaysInMonth := Days[M];
 If (M = 2) And IsLeapYear(Y) Then DaysInMonth := 29;
End;

Function JulianDay(Y, M, D: LongInt): LongInt;
Var A: LongInt;
Begin
 A := (14-M) Div 12;
 Y := Y+4800-A; M := M+12*A-3;
 JulianDay := D+(153*M+2) Div 5+365*Y+Y Div 4-Y Div 100+Y Div 400-32045;
End;

Function DateSeconds(Y, M, D: Integer): Double;
Begin
 If (Y < 1) Or (Y > 9999) Then Fail('Annee hors limites (0001-9999).',1);
 If (D < 1) Or (D > DaysInMonth(Y,M)) Then Fail('Jour invalide.',1);
 DateSeconds := (JulianDay(Y,M,D)-2451545)*86400.0-43200.0;
End;

Function WordFrom(Var S: String): String;
Var J: Integer;
Begin
 S := Trim(S); J := 1;
 While J <= Length(S) Do Begin
  If S[J] In [' ',#9] Then Break;
  Inc(J);
 End;
 WordFrom := Copy(S,1,J-1);
 Delete(S,1,J);
End;

Procedure ReadTextLine(Var F: Text; Var S: String);
Var C: Char; Err: Word;
Begin
 S := '';
 While Not EOF(F) And Not EOLn(F) Do Begin
  If Length(S) = 255 Then Fail('Ligne trop longue (255 caracteres maximum).',1);
  {$I-} Read(F,C); Err := IOResult; {$I+}
  If Err <> 0 Then Fail('Erreur de lecture texte.',1);
  S := S+C;
 End;
 If Not EOF(F) Then Begin
  {$I-} ReadLn(F); Err := IOResult; {$I+}
  If Err <> 0 Then Fail('Erreur de lecture texte.',1);
 End;
End;

Function MonthNumber(S: String): Integer;
Var J: Integer;
Begin
 S := Upper(S);
 For J := 1 To 12 Do If S = MonthNames[J] Then Begin MonthNumber := J; Exit End;
 MonthNumber := IntegerNumber(S);
End;

Function ParseCalendar(S: String; Var ExtraSecond: Boolean): Double;
Var DatePart, Clock, Rest, Fraction: String;
    Y, M, D, DOY, J, P, H, Minute, Sec: Integer;
    F: Double;
Begin
 S := Trim(S);
 If Length(S) >= 11 Then If S[11] In ['T','t'] Then S[11] := ' ';
 If Length(S) > 0 Then If S[Length(S)] In ['Z','z'] Then Begin
  If FromSystem <> 'UTC' Then Fail('Suffixe Z reserve au systeme UTC.',1);
  Delete(S,Length(S),1);
 End;
 DatePart := WordFrom(S);
 Y := 0; M := 0; D := 0;
 If (Length(DatePart) = 10) And (Copy(DatePart,5,1) = '-') And
    (Copy(DatePart,8,1) = '-') Then Begin
  Y := IntegerNumber(Copy(DatePart,1,4));
  M := IntegerNumber(Copy(DatePart,6,2));
  D := IntegerNumber(Copy(DatePart,9,2));
 End Else If (Length(DatePart) = 8) And (Copy(DatePart,5,1) = '-') Then Begin
  Y := IntegerNumber(Copy(DatePart,1,4));
  DOY := IntegerNumber(Copy(DatePart,6,3));
  J := 365; If IsLeapYear(Y) Then Inc(J);
  If (DOY < 1) Or (DOY > J) Then Fail('Jour de l''annee invalide.',1);
  M := 1;
  While DOY > DaysInMonth(Y,M) Do Begin Dec(DOY,DaysInMonth(Y,M)); Inc(M) End;
  D := DOY;
 End Else Begin
  Y := IntegerNumber(DatePart);
  Rest := WordFrom(S); M := MonthNumber(Rest);
  Rest := WordFrom(S); D := IntegerNumber(Rest);
 End;
 F := DateSeconds(Y,M,D);
 Clock := WordFrom(S);
 If Trim(S) <> '' Then Fail('Texte supplementaire apres la date.',1);
 H := 0; Minute := 0; Sec := 0;
 ExtraSecond := False;
 If Clock <> '' Then Begin
  If (Length(Clock) < 8) Or (Copy(Clock,3,1) <> ':') Or
     (Copy(Clock,6,1) <> ':') Then Fail('Heure attendue : HH:MM:SS[.fraction].',1);
  H := IntegerNumber(Copy(Clock,1,2));
  Minute := IntegerNumber(Copy(Clock,4,2));
  Sec := IntegerNumber(Copy(Clock,7,2));
  If (H > 23) Or (Minute > 59) Or (Sec > 60) Then Fail('Heure invalide.',1);
  If Sec = 60 Then Begin
   If (H <> 23) Or (Minute <> 59) Then Fail('Seconde intercalaire hors fin de jour.',1);
   ExtraSecond := True;
  End;
  If Length(Clock) > 8 Then Begin
   If Copy(Clock,9,1) <> '.' Then Fail('Fraction de seconde invalide.',1);
   Fraction := Copy(Clock,10,255);
   If (Length(Fraction) < 1) Or (Length(Fraction) > 9) Then
    Fail('Fraction attendue : 1 a 9 chiffres.',1);
   For P := 1 To Length(Fraction) Do
    If Not (Fraction[P] In ['0'..'9']) Then Fail('Fraction invalide.',1);
   F := F+RealNumber('0.'+Fraction);
  End;
 End;
 ParseCalendar := F+H*3600.0+Minute*60.0+Sec;
End;

Procedure InitLSK;
Var J: Integer;
Begin
 DeltaTA := 32.184; K := 0.001657; EB := 0.01671;
 M0 := 6.239996; M1 := 1.99096871E-7;
 LeapCount := 28;
 For J := 1 To LeapCount Do Begin
  LeapEpoch[J] := DateSeconds(BuiltinYears[J],BuiltinMonths[J],1);
  LeapOffset[J] := J+9;
 End;
End;

Function NextToken: String;
Var S: String; C: Char;
Begin
 While TokenPos <= KernelLength Do Begin
  If Not (KernelText[TokenPos] In [' ',#9,#10,#13,',']) Then Break;
  Inc(TokenPos);
 End;
 S := '';
 If TokenPos <= KernelLength Then Begin
  C := KernelText[TokenPos];
  If C In ['(',')','='] Then Begin S := C; Inc(TokenPos) End
  Else While TokenPos <= KernelLength Do Begin
   C := KernelText[TokenPos];
   If C In [' ',#9,#10,#13,',','(',')','='] Then Break;
   If Length(S) = 255 Then Fail('Token LSK trop long.',1);
   S := S+C; Inc(TokenPos);
  End;
 End;
 NextToken := Upper(S);
End;

Procedure ExpectToken(Expected: String);
Begin
 If NextToken <> Expected Then Fail('Syntaxe LSK invalide : attendu '+Expected,1);
End;

Function KernelDate(S: String): Double;
Var Y, M, D, P: Integer; Rest: String;
Begin
 If Copy(S,1,1) <> '@' Then Fail('Date LSK attendue avec @.',1);
 Delete(S,1,1);
 P := Pos('-',S); If P = 0 Then Fail('Date LSK invalide.',1);
 Y := IntegerNumber(Copy(S,1,P-1)); Delete(S,1,P);
 P := Pos('-',S); If P = 0 Then Fail('Date LSK invalide.',1);
 Rest := Copy(S,1,P-1); M := MonthNumber(Rest); Delete(S,1,P);
 D := IntegerNumber(S);
 KernelDate := DateSeconds(Y,M,D);
End;

Procedure LoadLSK(Path: String);
Var F: Text; S, Name, Token: String; Err: Word; InData: Boolean;
    J, Offset: Integer; V: Double; Seen: Set Of 1..5;
Begin
 Assign(F,Path);
 {$I-} Reset(F); Err := IOResult; {$I+}
 If Err <> 0 Then Fail('Impossible d''ouvrir le LSK : '+Path,1);
 KernelLength := 0; InData := False;
 While Not EOF(F) Do Begin
  ReadTextLine(F,S);
  S := Trim(S);
  If Upper(S) = '\BEGINDATA' Then InData := True
  Else If Upper(S) = '\BEGINTEXT' Then InData := False
  Else If InData Then Begin
   If KernelLength+Length(S)+1 > MaxKernelText Then Begin
    Close(F); Fail('Section LSK trop longue (16 Kio maximum).',1);
   End;
   For J := 1 To Length(S) Do Begin Inc(KernelLength); KernelText[KernelLength] := S[J] End;
   Inc(KernelLength); KernelText[KernelLength] := ' ';
  End;
 End;
 {$I-} Close(F); Err := IOResult; {$I+}
 If Err <> 0 Then Fail('Erreur de fermeture LSK.',1);
 TokenPos := 1; Seen := [];
 Name := NextToken;
 While Name <> '' Do Begin
  ExpectToken('=');
  If Name = 'DELTET/DELTA_T_A' Then Begin DeltaTA := RealNumber(NextToken); Include(Seen,1) End
  Else If Name = 'DELTET/K' Then Begin K := RealNumber(NextToken); Include(Seen,2) End
  Else If Name = 'DELTET/EB' Then Begin EB := RealNumber(NextToken); Include(Seen,3) End
  Else If Name = 'DELTET/M' Then Begin
   ExpectToken('('); M0 := RealNumber(NextToken); M1 := RealNumber(NextToken);
   ExpectToken(')'); Include(Seen,4);
  End Else If Name = 'DELTET/DELTA_AT' Then Begin
   ExpectToken('('); LeapCount := 0;
   Token := NextToken;
   While Token <> ')' Do Begin
    If LeapCount = MaxLeaps Then Fail('Trop d''entrees de secondes intercalaires.',1);
    V := RealNumber(Token);
    If (V < 0) Or (V > 1000) Then Fail('Offset TAI-UTC invalide.',1);
    Offset := Trunc(V);
    If V <> Offset Then Fail('Offset TAI-UTC non entier.',1);
    Inc(LeapCount); LeapOffset[LeapCount] := Offset;
    LeapEpoch[LeapCount] := KernelDate(NextToken);
    If LeapCount > 1 Then Begin
     If LeapEpoch[LeapCount] <= LeapEpoch[LeapCount-1] Then Fail('Dates LSK non croissantes.',1);
     If LeapOffset[LeapCount] <> LeapOffset[LeapCount-1]+1 Then
      Fail('LSK : seules les secondes intercalaires positives sont prises en charge.',1);
    End;
    Token := NextToken;
   End;
   If LeapCount = 0 Then Fail('Table LSK vide.',1);
   Include(Seen,5);
  End Else Fail('Affectation LSK non prise en charge : '+Name,1);
  Name := NextToken;
 End;
 If Seen <> [1..5] Then Fail('LSK incomplet : les cinq affectations DELTET sont requises.',1);
 If (DeltaTA < 0) Or (DeltaTA > 1000) Or (Abs(K) > 1) Or
    (Abs(EB) > 1) Or (Abs(M1) > 1.0E-5) Then Fail('Constantes DELTET hors limites.',1);
End;

Function Periodic(ET: Double): Double;
Var Mean: Double;
Begin
 Mean := M0+M1*ET;
 Periodic := K*Sin(Mean+EB*Sin(Mean));
End;

Function TAItoET(TAI: Double): Double;
Var ET: Double; J: Integer;
Begin
 ET := TAI+DeltaTA;
 For J := 1 To 6 Do ET := TAI+DeltaTA+Periodic(ET);
 TAItoET := ET;
End;

Function UTCtoTAI(U: Double; ExtraSecond: Boolean): Double;
Var J, Offset: Integer;
Begin
 If U < LeapEpoch[1] Then Fail('UTC anterieur a la premiere entree LSK.',1);
 If ExtraSecond Then Begin
  For J := 2 To LeapCount Do
   If (U >= LeapEpoch[J]) And (U < LeapEpoch[J]+1) Then Begin
    UTCtoTAI := U+LeapOffset[J-1]; Exit;
   End;
  Fail('Seconde intercalaire absente de la table LSK.',1);
 End;
 Offset := LeapOffset[1];
 For J := 2 To LeapCount Do If U >= LeapEpoch[J] Then Offset := LeapOffset[J];
 UTCtoTAI := U+Offset;
End;

Function TAItoUTC(TAI: Double; Var ExtraSecond: Boolean): Double;
Var J, Offset: Integer; Threshold: Double;
Begin
 If TAI < LeapEpoch[1]+LeapOffset[1] Then Fail('UTC anterieur a la premiere entree LSK.',1);
 ExtraSecond := False; Offset := LeapOffset[1];
 For J := 2 To LeapCount Do Begin
  Threshold := LeapEpoch[J]+LeapOffset[J];
  If TAI >= Threshold Then Offset := LeapOffset[J]
  Else If TAI >= Threshold-1 Then Begin
   ExtraSecond := True;
   TAItoUTC := LeapEpoch[J]-1+(TAI-(Threshold-1)); Exit;
  End;
 End;
 TAItoUTC := TAI-Offset;
End;

Function ReadTime(S: String): Double;
Var V: Double; ExtraSecond: Boolean;
Begin
 ExtraSecond := False;
 If FromType = 'SECONDS' Then V := RealNumber(S)
 Else If FromType = 'JD' Then V := (RealNumber(S)-2451545.0)*86400.0
 Else If FromType = 'MJD' Then V := (RealNumber(S)-51544.5)*86400.0
 Else V := ParseCalendar(S,ExtraSecond);
 If (V < MinEpoch) Or (V >= MaxEpoch) Then
  Fail('Temps hors plage du calendrier (0001-9999).',1);
 If ExtraSecond And (FromSystem <> 'UTC') Then Fail('Seconde 60 reservee a UTC.',1);
 If FromSystem = 'UTC' Then V := TAItoET(UTCtoTAI(V,ExtraSecond))
 Else If FromSystem = 'TAI' Then V := TAItoET(V)
 Else If FromSystem = 'TT' Then V := TAItoET(V-DeltaTA);
 ReadTime := V;
End;

Procedure SplitCalendar(V: Double; ExtraSecond: Boolean; Var C: TCalendar);
Var Day, L, N, Y, M, D, MS: LongInt; R: Double;
Begin
 R := V+43200.0;
 If (V < MinEpoch) Or (V >= MaxEpoch) Then
  Fail('Date de sortie hors limites (0001-9999).',1);
 Day := Trunc(R/86400.0);
 If R < Day*86400.0 Then Dec(Day);
 MS := Round((R-Day*86400.0)*1000.0);
 If MS >= 86400000 Then Begin Inc(Day); MS := 0 End;
 L := Day+2451545+68569; N := (4*L) Div 146097;
 L := L-(146097*N+3) Div 4;
 Y := (4000*(L+1)) Div 1461001;
 L := L-(1461*Y) Div 4+31; M := (80*L) Div 2447;
 D := L-(2447*M) Div 80; L := M Div 11;
 M := M+2-12*L; Y := 100*(N-49)+Y+L;
 If (Y < 1) Or (Y > 9999) Then Fail('Date arrondie hors limites (0001-9999).',1);
 C.Y := Y; C.M := M; C.D := D;
 C.H := MS Div 3600000; C.Minute := (MS Div 60000) Mod 60;
 C.Second := (MS Div 1000) Mod 60; C.MS := MS Mod 1000;
 If ExtraSecond Then C.Second := 60;
 C.DOY := JulianDay(Y,M,D)-JulianDay(Y,1,1)+1;
End;

Function RoundMillis(V: Double): Double;
Var Day: Double;
Begin
 Day := Trunc(V/86400.0)*86400.0;
 RoundMillis := Day+Round((V-Day)*1000.0)/1000.0;
End;

Function FormatCalendar(C: TCalendar; Mask: String): String;
Var P: Integer; R, Token, Replacement: String;
Begin
 R := ''; P := 1;
 While P <= Length(Mask) Do Begin
  Token := Upper(Copy(Mask,P,4)); Replacement := '';
  If Token = 'YYYY' Then Begin Replacement := Num(C.Y,4); Inc(P,4) End
  Else Begin
   Token := Upper(Copy(Mask,P,3));
   If Token = 'DOY' Then Begin Replacement := Num(C.DOY,3); Inc(P,3) End
   Else If Token = 'MON' Then Begin Replacement := MonthNames[C.M]; Inc(P,3) End
   Else If Token = '###' Then Begin Replacement := Num(C.MS,3); Inc(P,3) End
   Else Begin
    Token := Upper(Copy(Mask,P,2));
    If Token = 'MM' Then Replacement := Num(C.M,2)
    Else If Token = 'DD' Then Replacement := Num(C.D,2)
    Else If Token = 'HR' Then Replacement := Num(C.H,2)
    Else If Token = 'MN' Then Replacement := Num(C.Minute,2)
    Else If Token = 'SC' Then Replacement := Num(C.Second,2);
    If Replacement <> '' Then Inc(P,2)
    Else Begin
     If Mask[P] In ['A'..'Z','a'..'z','#'] Then
      Fail('Token de format inconnu pres de '+Copy(Mask,P,10),2);
     Replacement := Mask[P]; Inc(P);
    End;
   End;
  End;
  If Length(R)+Length(Replacement) > 200 Then Fail('Format de sortie trop long.',2);
  R := R+Replacement;
 End;
 FormatCalendar := R;
End;

Procedure Convert(S: String);
Var ET, V, TAI: Double; C: TCalendar; R, Mask: String; ExtraSecond: Boolean;
Begin
 ET := ReadTime(S);
 TAI := ET-DeltaTA-Periodic(ET);
 ExtraSecond := False;
 V := ET;
 If ToSystem = 'TAI' Then V := TAI
 Else If ToSystem = 'TT' Then V := TAI+DeltaTA
 Else If ToSystem = 'UTC' Then V := TAItoUTC(RoundMillis(TAI),ExtraSecond);
 If ToType = 'SECONDS' Then Str(V:0:6,R)
 Else If ToType = 'JD' Then Str((2451545.0+V/86400.0):0:9,R)
 Else If ToType = 'MJD' Then Str((51544.5+V/86400.0):0:9,R)
 Else Begin
  SplitCalendar(V,ExtraSecond,C);
  Mask := Picture;
  If Mask = '' Then Begin
   If ToType = 'DOY' Then Mask := 'YYYY-DOY HR:MN:SC.###'
   Else Mask := 'YYYY-MM-DD HR:MN:SC.###';
  End;
  R := FormatCalendar(C,Mask);
 End;
 If Not NoLabel Then R := R+' ('+ToSystem+'/'+ToType+')';
 WriteLn(R);
End;

Function SystemName(S: String): String;
Begin
 S := Upper(Trim(S));
 If S = 'TDB' Then S := 'ET';
 If S = 'TDT' Then S := 'TT';
 If (S <> 'UTC') And (S <> 'ET') And (S <> 'TAI') And (S <> 'TT') Then
  Fail('Systeme non pris en charge : '+S+' (UTC, ET/TDB, TAI, TT/TDT).',2);
 SystemName := S;
End;

Procedure CheckType(System, Kind: String);
Begin
 If (Kind <> 'SCET') And (Kind <> 'DOY') And (Kind <> 'SECONDS') And
    (Kind <> 'JD') And (Kind <> 'MJD') Then
  Fail('Type non pris en charge : '+Kind,2);
 If (System = 'UTC') And (Kind <> 'SCET') And (Kind <> 'DOY') Then
  Fail('UTC accepte uniquement SCET et DOY; ses secondes ne sont pas uniformes.',2);
End;

Procedure Help;
Begin
 WriteLn('CHRONOS - Cette commande permet d''effectuer la conversion des ',
         'syst'+#138+'mes et formats temporels.');
 WriteLn;
 WriteLn('Syntaxe :');
 WriteLn('  CHRONOS -from <systeme> -to <systeme> -time "<temps>" [options]');
 WriteLn('  CHRONOS -from <systeme> -to <systeme> -batch [options]');
 WriteLn('  CHRONOS -h | -help | --help | /?');
 WriteLn('  CHRONOS -u | -usage');
 WriteLn;
 WriteLn('Systemes : UTC, ET (alias TDB), TAI, TT (alias TDT).');
 WriteLn('Options :');
 WriteLn('  -fromtype <type>  SCET par defaut; DOY, SECONDS, JD, MJD');
 WriteLn('  -totype <type>    SCET par defaut; DOY, SECONDS, JD, MJD');
 WriteLn('  -setup <LSK>      Fichier texte LSK (ou CHRONOS_SETUP_FILE)');
 WriteLn('  -format "<masque>" Format calendrier personnalise');
 WriteLn('  -nolabel          Affiche uniquement la valeur convertie');
 WriteLn('  -trace            Affiche les systemes et la source LSK');
 WriteLn('  -batch            Lit une date par ligne sur l''entree standard');
 WriteLn('UTC : types SCET/DOY seulement. SECONDS : secondes depuis midi');
 WriteLn('du 2000-01-01 dans le systeme uniforme selectionne.');
 WriteLn('JD : jour julien; MJD = JD - 2400000.5.');
 WriteLn('Entree calendrier : AAAA-MM-JJ [HH:MM:SS[.fraction]],');
 WriteLn('AAAA-JJJ [HH:MM:SS[.fraction]] ou AAAA MON JJ [HH:MM:SS[.fraction]].');
 WriteLn('ISO UTC avec T et Z accepte; pas de fuseaux horaires.');
 WriteLn('Masque : YYYY, MM, MON, DD, DOY, HR, MN, SC et ### (millisecondes).');
 WriteLn('Chevrons : parametres obligatoires a remplacer; crochets : options.');
 WriteLn;
 WriteLn('Exemples :');
 WriteLn('  CHRONOS -from UTC -to ET -totype SECONDS -time "2000-01-01 12:00:00"');
 WriteLn('  CHRONOS -from ET -fromtype SECONDS -to UTC -time 0');
 WriteLn('  CHRONOS -from TT -to TAI -time "2000-01-01 12:00:00"');
 WriteLn('  CHRONOS -setup naif0012.tls -from UTC -to ET -batch -nolabel');
 WriteLn('  CHRONOS -from ET -to ET -totype JD -time "2000-01-01 12:00:00"');
 WriteLn;
 WriteLn('Sans LSK externe : table NAIF0012 (1972-2017), dernier offset');
 WriteLn('TAI-UTC = 37 s prolonge apres 2017. Calendrier 0001-9999.');
 WriteLn('LSK : cinq affectations DELTET, 128 entrees, sauts positifs de 1 s.');
 WriteLn('Non pris en charge : SCLK, LST, ERT, ETT, LT, meta-kernels,');
 WriteLn('formats SPICE arbitraires et conversions dependant des ephemerides.');
 WriteLn('Codes retour : 0 succes, 1 entree/LSK invalide, 2 syntaxe invalide.');
End;

Function OptionValue: String;
Begin
 Inc(ArgIndex);
 If ArgIndex > ParamCount Then Fail('Valeur manquante pour '+Arg,2);
 OptionValue := ParamStr(ArgIndex);
End;

Function TextValue: String;
Var S, Next: String; StartsOption: Boolean;
Begin
 S := OptionValue;
 While ArgIndex < ParamCount Do Begin
  Next := ParamStr(ArgIndex+1);
  StartsOption := False;
  If Length(Next) > 1 Then
   StartsOption := (Next[1] = '-') And (Next[2] In ['A'..'Z','a'..'z','-']);
  If StartsOption Or (Next = '/?') Then Break;
  If Length(S)+Length(Next)+1 > 255 Then Fail('Argument texte trop long.',2);
  S := S+' '+Next; Inc(ArgIndex);
 End;
 TextValue := S;
End;

Begin
 InitLSK;
 FromSystem := ''; ToSystem := ''; FromType := 'SCET'; ToType := 'SCET';
 Picture := ''; SetupPath := ''; TimeInput := '';
 Batch := False; NoLabel := False; Trace := False; HasTime := False;
 If ParamCount = 0 Then Begin Help; Halt(0) End;
 For ArgIndex := 1 To ParamCount Do Begin
  Arg := Upper(ParamStr(ArgIndex));
  If (Arg = '/?') Or (Arg = '-H') Or (Arg = '-HELP') Or (Arg = '--HELP') Or
     (Arg = '-U') Or (Arg = '-USAGE') Then Begin Help; Halt(0) End;
 End;
 ArgIndex := 1;
 While ArgIndex <= ParamCount Do Begin
  Arg := Upper(ParamStr(ArgIndex));
  If Arg = '-FROM' Then FromSystem := OptionValue
  Else If Arg = '-TO' Then ToSystem := OptionValue
  Else If Arg = '-FROMTYPE' Then FromType := Upper(OptionValue)
  Else If Arg = '-TOTYPE' Then ToType := Upper(OptionValue)
  Else If Arg = '-SETUP' Then SetupPath := OptionValue
  Else If Arg = '-FORMAT' Then Picture := TextValue
  Else If Arg = '-TIME' Then Begin TimeInput := TextValue; HasTime := True End
  Else If Arg = '-BATCH' Then Batch := True
  Else If Arg = '-NOLABEL' Then NoLabel := True
  Else If Arg = '-TRACE' Then Trace := True
  Else Fail('Option inconnue ou non prise en charge : '+Arg+'. Voir CHRONOS /?',2);
  Inc(ArgIndex);
 End;
 If (Trim(FromSystem) = '') Or (Trim(ToSystem) = '') Then Fail('-from et -to sont requis.',2);
 FromSystem := SystemName(FromSystem); ToSystem := SystemName(ToSystem);
 FromType := Upper(Trim(FromType)); ToType := Upper(Trim(ToType));
 If FromType = '' Then FromType := 'SCET';
 If ToType = '' Then ToType := 'SCET';
 CheckType(FromSystem,FromType); CheckType(ToSystem,ToType);
 If (Picture <> '') And (ToType <> 'SCET') And (ToType <> 'DOY') Then
  Fail('-format est reserve aux sorties calendrier.',2);
 If Not Batch And Not HasTime Then Fail('-time ou -batch est requis.',2);
 If SetupPath = '' Then SetupPath := GetEnv('CHRONOS_SETUP_FILE');
 If SetupPath <> '' Then LoadLSK(SetupPath);
 If Trace Then Begin
  NoLabel := False;
  WriteLn('CHRONOS : ',FromSystem,'/',FromType,' -> ',ToSystem,'/',ToType);
  If SetupPath = '' Then WriteLn('LSK : NAIF0012 integre (1972-2017).')
  Else WriteLn('LSK : ',SetupPath);
  WriteLn('Entrees : ',LeapCount,'; dernier offset prolonge : ',LeapOffset[LeapCount],' s.');
 End;
 If Batch Then Begin
  While Not EOF(Input) Do Begin
   ReadTextLine(Input,Line);
   If Trim(Line) <> '' Then Convert(Line);
  End;
 End Else Convert(TimeInput);
End.
