{ @author: Sylvain Maltais (support@gladir.com)
 @created: 2026
 @website(https://www.gladir.com/space-0)
 @abstract(Target: Turbo Pascal 7, Free Pascal 3.2) }

Program TOBIN;

{$A-}
{$E+,N+}

{ Conversion de fichiers SPICE de transfert ASCII ("encoded transfer file",
  DAFETF et DASETF) vers les formats binaires DAF (SPK, CK, PCK) et DAS
  (DSK, EK), sous-ensemble autonome de TOBIN NAIF : aucune unite, aucune
  inclusion. C'est l'operation inverse de TOXFR.PAS.
  Le binaire produit est toujours LTL-IEEE (x86). Disposition identique a
  celle de DAFTB/DASTB de SPICELIB :
    DAF  enregistrement de fichier, enregistrement sommaire + noms, puis les
         donnees; un nouveau couple sommaire/noms est insere apres les
         donnees quand 25 tableaux (pour ND=2, NI=6) sont atteints.
    DAS  enregistrement de fichier, zone de commentaires (NCOMR records),
         repertoire, puis enregistrements caracteres, reels et entiers
         (fichier segrege).
  Reels : fraction hexadecimale x 16^N, arrondis au plus proche si la
  mantisse depasse 53 bits. Pas de zone reservee DAS (NRESVR/NRESVC).
  Les mots inutilises en fin d'enregistrement sont ecrits a zero (blancs pour
  les caracteres et la fin du dernier enregistrement de commentaires).
  Sources : naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/ug/tobin.html
            naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/daf.html
            naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/das.html
  Compilation : fpc -Mtp TOBIN.PAS
  Limites DOS : lignes de 255 caracteres, fichiers < 2 Gio. }

Const
 FtpString: String[28] =
  'FTPSTR:'#13':'#10':'#13#10':'#13#0':'#129':'#16#206':ENDFTP';
 HexDigits: String[16] = '0123456789ABCDEF';

Type
 TRecord = Array[0..1023] Of Byte;
 TBytes8 = Array[0..7] Of Byte;

Var
 XfrFile: Text;
 OutFile: File;
 XfrOpen, OutOpen, Force, Quiet: Boolean;
 XfrName, OutName, Arg, Line: String;
 LineNo: LongInt;
 NextRec: LongInt;
 Block: TRecord;
 BlockFill: Integer;
 ArgIndex, Paths, I: Integer;

Procedure Cleanup(KeepOutput: Boolean);
Var Err: Word;
Begin
 If XfrOpen Then Begin
  {$I-} Close(XfrFile); Err := IOResult; {$I+}
  XfrOpen := False;
 End;
 If OutOpen Then Begin
  {$I-} Close(OutFile); Err := IOResult; {$I+}
  OutOpen := False;
  If Not KeepOutput Then Begin
   {$I-} Erase(OutFile); Err := IOResult; {$I+}
  End;
 End;
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
 Fail(S+' Voir TOBIN -h.',2);
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

Function PadTo(S: String; Width: Integer): String;
Begin
 If Length(S) > Width Then S := Copy(S,1,Width);
 While Length(S) < Width Do S := S+' ';
 PadTo := S;
End;

Procedure Help;
Begin
 WriteLn('TOBIN - Cette commande permet de convertir les fichiers SPICE entre ',
         'formats binaires et formats de transfert.');
 WriteLn;
 WriteLn('Syntaxe : TOBIN [options] <fichier_transfert> [<fichier_binaire>]');
 WriteLn;
 WriteLn('Options :');
 WriteLn('  -f       Ecrase le fichier binaire s''il existe');
 WriteLn('  -q       Silencieux (aucun message de succes)');
 WriteLn('  -h, /?   Aide (-help et --help acceptes)');
 WriteLn;
 WriteLn('Sans nom de sortie, l''extension x... devient b... (mission.xsp ->');
 WriteLn('mission.bsp, .xc -> .bc, .xpc -> .bpc, .xds -> .bds, .xes -> .bes);');
 WriteLn('sinon .bin est ajoute.');
 WriteLn;
 WriteLn('Fichiers de transfert pris en charge : DAFETF (SPK, CK, PCK et autres');
 WriteLn('tableaux DAF) et DASETF (DSK, EK) produits par TOXFR ou DAFBT/DASBT.');
 WriteLn('Le binaire est ecrit en LTL-IEEE.');
 WriteLn;
 WriteLn('Exemples :');
 WriteLn('  TOBIN mission.xsp');
 WriteLn('  TOBIN -f eros.xds eros.bds');
 WriteLn;
 WriteLn('Codes retour : 0 succes, 1 erreur de lecture/format, 2 syntaxe.');
End;

{ ------------------------------------------------------------------ }
{ Lecture du fichier de transfert                                    }
{ ------------------------------------------------------------------ }

Procedure NextLine(Var S: String);
Var Err: Word;
Begin
 While True Do Begin
  If EOF(XfrFile) Then Fail('Fin de fichier inattendue : fichier de transfert incomplet.',1);
  {$I-} ReadLn(XfrFile,S); Err := IOResult; {$I+}
  If Err <> 0 Then Fail('Erreur de lecture de '+XfrName+'.',1);
  Inc(LineNo);
  If (Length(S) > 0) And (S[Length(S)] = #13) Then Delete(S,Length(S),1);
  If Trim(S) <> '' Then Exit;
 End;
End;

{ Contenu entre la premiere et la derniere apostrophe (les apostrophes
  internes ne sont pas doublees dans le format). }
Function Unquote(S: String): String;
Var A, B: Integer;
Begin
 A := 1;
 While (A <= Length(S)) And (S[A] <> '''') Do Inc(A);
 B := Length(S);
 While (B > A) And (S[B] <> '''') Do Dec(B);
 If (A > Length(S)) Or (B <= A) Then Fail('Chaine entre apostrophes attendue : '+S,1);
 Unquote := Copy(S,A+1,B-A-1);
End;

Function HexToLong(S: String; Var V: LongInt): Boolean;
Var K, D: Integer; Neg: Boolean; Acc: Double;
Begin
 HexToLong := False;
 S := Trim(S);
 Neg := False;
 If (S <> '') And (S[1] = '-') Then Begin Neg := True; Delete(S,1,1) End;
 If (S = '') Or (Length(S) > 8) Then Exit;
 Acc := 0;
 For K := 1 To Length(S) Do Begin
  D := Pos(UpCase(S[K]),HexDigits)-1;
  If D < 0 Then Exit;
  Acc := Acc*16+D;
 End;
 If Neg Then Acc := -Acc;
 If (Acc < -2147483648.0) Or (Acc > 2147483647.0) Then Exit;
 V := Trunc(Acc);
 HexToLong := True;
End;

Function ReadHexInt(Var S: String): LongInt;
Var V: LongInt;
Begin
 NextLine(S);
 If Not HexToLong(Unquote(S),V) Then Fail('Entier hexadecimal invalide : '+S,1);
 ReadHexInt := V;
End;

{ Reel "[-]HHHH^[-]NNN" : valeur = 0.HHHH x 16^N, assemblee bit a bit
  (I = valeur x 2^1074) puis arrondie au plus proche. }
Function HexToDP(S: String; Var V: Double): Boolean;
Var
 Bits: Array[0..2199] Of Byte;
 Sg: Array[0..52] Of Byte;
 B: TBytes8;
 P, K, J, Digs, D, L, Pos2, E, Carry, Idx, Expo: Integer;
 NExp: LongInt;
 Neg, ExpNeg, Round1, Sticky: Boolean;
 Mant, ExpS: String;
 Acc: Double;
Begin
 HexToDP := False;
 S := Trim(S);
 Neg := False;
 If (S <> '') And (S[1] = '-') Then Begin Neg := True; Delete(S,1,1) End;
 P := Pos('^',S);
 If P < 2 Then Exit;
 Mant := Copy(S,1,P-1); ExpS := Copy(S,P+1,255);
 If (Mant = '') Or (Length(Mant) > 40) Or (ExpS = '') Then Exit;
 ExpNeg := False;
 If ExpS[1] = '-' Then Begin ExpNeg := True; Delete(ExpS,1,1) End;
 If (ExpS = '') Or (Length(ExpS) > 4) Then Exit;
 Acc := 0;
 For K := 1 To Length(ExpS) Do Begin
  D := Pos(UpCase(ExpS[K]),HexDigits)-1;
  If D < 0 Then Exit;
  Acc := Acc*16+D;
 End;
 NExp := Trunc(Acc);
 If ExpNeg Then NExp := -NExp;
 If (NExp < -300) Or (NExp > 300) Then Exit;
 FillChar(Bits,SizeOf(Bits),0);
 FillChar(B,SizeOf(B),0);
 { Bit i (1 = poids fort du premier chiffre) a la position 4N-i+1074. }
 Digs := Length(Mant);
 For J := 1 To Digs Do Begin
  D := Pos(UpCase(Mant[J]),HexDigits)-1;
  If D < 0 Then Exit;
  For K := 0 To 3 Do
   If (D And (8 Shr K)) <> 0 Then Begin
    Pos2 := 4*NExp-(4*(J-1)+K+1)+1074;
    If (Pos2 >= 0) And (Pos2 < 2200) Then Bits[Pos2] := 1
    Else If Pos2 >= 2200 Then Exit;
   End;
 End;
 L := -1;
 For K := 2199 DownTo 0 Do If Bits[K] <> 0 Then Begin L := K; Break End;
 If L < 0 Then Begin V := 0; HexToDP := True; Exit End;
 FillChar(Sg,SizeOf(Sg),0);
 If L <= 51 Then Begin
  { Denormalise : exposant 0, fraction = bits 51..0 de I. }
  Expo := 0;
  For K := 0 To 51 Do Sg[K] := Bits[K];
 End Else Begin
  For K := 0 To 52 Do Sg[K] := Bits[L-52+K];
  Round1 := False; Sticky := False;
  If L-53 >= 0 Then Round1 := Bits[L-53] <> 0;
  For K := 0 To L-54 Do If Bits[K] <> 0 Then Sticky := True;
  If Round1 And (Sticky Or (Sg[0] <> 0)) Then Begin
   Carry := 1; Idx := 0;
   While (Carry <> 0) And (Idx <= 52) Do Begin
    If Sg[Idx] = 0 Then Begin Sg[Idx] := 1; Carry := 0 End
    Else Begin Sg[Idx] := 0; Inc(Idx) End;
   End;
   If Carry <> 0 Then Begin Sg[52] := 1; Inc(L) End;
  End;
  Expo := L-51;
  If Expo >= 2047 Then Exit;
 End;
 E := Expo;
 { Octets : B[7] signe + 7 bits forts de l'exposant, B[6] 4 bits faibles de
   l'exposant + fraction 51..48, B[5]..B[0] fraction 47..0. }
 B[7] := (E Shr 4);
 If Neg Then B[7] := B[7] Or $80;
 B[6] := (E And 15) Shl 4;
 For K := 0 To 3 Do If Sg[48+K] <> 0 Then B[6] := B[6] Or (1 Shl K);
 For J := 0 To 5 Do
  For K := 0 To 7 Do
   If Sg[J*8+K] <> 0 Then B[J] := B[J] Or (1 Shl K);
 Move(B,V,8);
 HexToDP := True;
End;

Function ReadDP: Double;
Var S: String; V: Double;
Begin
 NextLine(S);
 If Not HexToDP(Unquote(S),V) Then Fail('Reel hexadecimal invalide : '+S,1);
 ReadDP := V;
End;

{ Dernier mot (decimal) de la ligne, ex. "BEGIN_ARRAY 3 72" -> 72. }
Function WordN(S: String; N: Integer): String;
Var K: Integer; W: String;
Begin
 S := Trim(S);
 For K := 1 To N Do Begin
  W := '';
  While (S <> '') And (S[1] > ' ') Do Begin W := W+S[1]; Delete(S,1,1) End;
  S := Trim(S);
 End;
 WordN := W;
End;

Function DecWord(S: String; N: Integer): LongInt;
Var V: LongInt; Code: Integer; W: String;
Begin
 W := WordN(S,N);
 Val(W,V,Code);
 If (W = '') Or (Code <> 0) Or (V < 0) Then Fail('Nombre decimal attendu dans : '+S,1);
 DecWord := V;
End;

{ ------------------------------------------------------------------ }
{ Ecriture binaire                                                   }
{ ------------------------------------------------------------------ }

Procedure WriteAt(RecNo: LongInt; Var Rec);
Var Err: Word;
Begin
 {$I-}
 Seek(OutFile,RecNo-1);
 BlockWrite(OutFile,Rec,1);
 Err := IOResult;
 {$I+}
 If Err <> 0 Then Fail('Erreur d''ecriture de '+OutName+' (disque plein ?).',1);
End;

Procedure PutByte(B: Byte);
Begin
 Block[BlockFill] := B;
 Inc(BlockFill);
 If BlockFill = 1024 Then Begin
  WriteAt(NextRec,Block);
  Inc(NextRec);
  BlockFill := 0;
 End;
End;

Procedure PutDouble(V: Double);
Var B: TBytes8; K: Integer;
Begin
 Move(V,B,8);
 For K := 0 To 7 Do PutByte(B[K]);
End;

Procedure PutInt(L: LongInt);
Var B: Array[0..3] Of Byte; K: Integer;
Begin
 Move(L,B,4);
 For K := 0 To 3 Do PutByte(B[K]);
End;

{ Complete l'enregistrement en cours avec Fill (0 ou 32). }
Procedure PadRecord(Fill: Byte);
Begin
 While BlockFill <> 0 Do PutByte(Fill);
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

{ Enregistrement de noms : 1000 blancs puis 24 octets a zero. }
Procedure InitNames(Var Rec: TRecord);
Begin
 FillChar(Rec,SizeOf(Rec),0);
 FillChar(Rec,1000,32);
End;

{ Les caracteres de commande des noms sont remplaces par des blancs. }
Function CleanName(S: String): String;
Var K: Integer;
Begin
 For K := 1 To Length(S) Do If S[K] < ' ' Then S[K] := ' ';
 CleanName := S;
End;

Procedure CreateOutput;
Var Err: Word;
Begin
 Assign(OutFile,OutName);
 {$I-} Rewrite(OutFile,1024); Err := IOResult; {$I+}
 If Err <> 0 Then Fail('Impossible de creer '+OutName+'.',1);
 OutOpen := True;
 BlockFill := 0;
End;

{ ------------------------------------------------------------------ }
{ DAF                                                                }
{ ------------------------------------------------------------------ }

Procedure ConvertDAF;
Var
 IDWord, IFName, Name, S: String;
 ND, NI, SS, Cap, InRec, ArrayNo, Words, Done, Chunk, K, Slot, Offset: LongInt;
 SumRec, LastSum, NewSum, BeginAddr, EndAddr, FreeAddr, WordAddr: LongInt;
 SumBuf, NameBuf, Rec: TRecord;
 V: Double;
 IntVal: Array[1..250] Of LongInt;
 Fields: Array[1..250] Of Double;
 Total: LongInt;

 Procedure PutWord(X: Double);
 Begin
  PutDouble(X);
  Inc(WordAddr);
 End;

Begin
 NextLine(IDWord);
 IDWord := Unquote(IDWord);
 If Copy(IDWord,1,4) <> 'DAF/' Then Fail('Mot d''identification DAF attendu : '+IDWord,1);
 ND := ReadHexInt(S); NI := ReadHexInt(S);
 SS := ND+(NI+1) Div 2;
 If (ND < 0) Or (ND > 124) Or (NI < 2) Or (NI > 250) Or (SS < 1) Or (SS > 125) Then
  Fail('Dimensions ND/NI invalides.',1);
 NextLine(S);
 IFName := PadTo(CleanName(Unquote(S)),60);
 Cap := 1000 Div (8*SS);
 CreateOutput;
 { Enregistrement 1 (fichier), 2 (sommaire), 3 (noms) reserves. }
 FillChar(Rec,SizeOf(Rec),0);
 WriteAt(1,Rec); WriteAt(2,Rec); WriteAt(3,Rec);
 NextRec := 4; BlockFill := 0; WordAddr := 385; FreeAddr := 385;
 FillChar(SumBuf,SizeOf(SumBuf),0);
 InitNames(NameBuf);
 SumRec := 2; LastSum := 2; InRec := 0; ArrayNo := 0;
 NextLine(Line);
 While Copy(Upper(Line),1,11) = 'BEGIN_ARRAY' Do Begin
  Inc(ArrayNo);
  If DecWord(Line,2) <> ArrayNo Then Fail('Numero de tableau inattendu : '+Line,1);
  Words := DecWord(Line,3);
  If Words < 1 Then Fail('Tableau vide : '+Line,1);
  NextLine(S);
  Name := PadTo(CleanName(Unquote(S)),8*SS);
  For K := 1 To ND Do Fields[K] := ReadDP;
  For K := 1 To NI-2 Do IntVal[K] := ReadHexInt(S);
  BeginAddr := WordAddr;
  Done := 0;
  While Done < Words Do Begin
   NextLine(S);
   Chunk := DecWord(S,1);
   If (Chunk < 1) Or (Chunk > 1024) Or (Done+Chunk > Words) Then
    Fail('Nombre de mots de tranche invalide : '+S,1);
   For K := 1 To Chunk Do PutWord(ReadDP);
   Done := Done+Chunk;
  End;
  EndAddr := WordAddr-1;
  FreeAddr := EndAddr+1;
  NextLine(S);
  If (Copy(Upper(S),1,9) <> 'END_ARRAY') Or (DecWord(S,2) <> ArrayNo) Or
     (DecWord(S,3) <> Words) Then
   Fail('END_ARRAY attendu pour le tableau '+Num(ArrayNo)+' : '+S,1);
  Slot := InRec;
  Offset := 24+Slot*SS*8;
  For K := 1 To ND Do SetDouble(SumBuf,Offset+(K-1)*8,Fields[K]);
  For K := 1 To NI-2 Do SetInt(SumBuf,Offset+ND*8+(K-1)*4,IntVal[K]);
  SetInt(SumBuf,Offset+ND*8+(NI-2)*4,BeginAddr);
  SetInt(SumBuf,Offset+ND*8+(NI-1)*4,EndAddr);
  SetText(NameBuf,Slot*SS*8,Name);
  Inc(InRec);
  SetDouble(SumBuf,16,InRec);
  If InRec = Cap Then Begin
   { Record sommaire plein : nouveau couple sommaire/noms apres les donnees. }
   PadRecord(0);
   NewSum := NextRec;
   FillChar(Rec,SizeOf(Rec),0);
   WriteAt(NewSum,Rec); WriteAt(NewSum+1,Rec);
   NextRec := NewSum+2;
   WordAddr := (NextRec-1)*128+1;
   FreeAddr := WordAddr;
   SetDouble(SumBuf,0,NewSum);
   WriteAt(SumRec,SumBuf); WriteAt(SumRec+1,NameBuf);
   FillChar(SumBuf,SizeOf(SumBuf),0);
   InitNames(NameBuf);
   SetDouble(SumBuf,8,SumRec);
   SumRec := NewSum; LastSum := NewSum; InRec := 0;
  End;
  NextLine(Line);
 End;
 If Copy(Upper(Line),1,12) <> 'TOTAL_ARRAYS' Then
  Fail('BEGIN_ARRAY ou TOTAL_ARRAYS attendu : '+Line,1);
 If DecWord(Line,2) <> ArrayNo Then Fail('TOTAL_ARRAYS incoherent : '+Line,1);
 PadRecord(0);
 If ArrayNo > 0 Then Begin
  WriteAt(SumRec,SumBuf); WriteAt(SumRec+1,NameBuf);
 End;
 { Enregistrement de fichier. }
 FillChar(Rec,SizeOf(Rec),0);
 SetText(Rec,0,PadTo(IDWord,8));
 SetInt(Rec,8,ND); SetInt(Rec,12,NI);
 SetText(Rec,16,IFName);
 If ArrayNo > 0 Then Begin
  SetInt(Rec,76,2); SetInt(Rec,80,LastSum); SetInt(Rec,84,FreeAddr);
 End Else Begin
  SetInt(Rec,76,0); SetInt(Rec,80,0); SetInt(Rec,84,129);
 End;
 SetText(Rec,88,'LTL-IEEE');
 SetText(Rec,699,FtpString);
 WriteAt(1,Rec);
 Total := ArrayNo;
 If Not Quiet Then WriteLn('DAF ',Trim(IDWord),' : ',Total,' tableau(x).');
End;

{ ------------------------------------------------------------------ }
{ DAS                                                                }
{ ------------------------------------------------------------------ }

Function Records(N, PerRec: LongInt): LongInt;
Begin
 Records := (N+PerRec-1) Div PerRec;
End;

Procedure ConvertDAS;
Var
 IDWord, IFName, S, W: String;
 NResvR, NResvC, NComR, NComC, NChar, NDP, NInt: LongInt;
 Rec: TRecord;
 BlockNo, BlockLen, Done, Total, N, K, Used: LongInt;
 Name: String;
 Count: Integer;
 Kinds: Array[1..3] Of LongInt;
 Words: Array[1..3] Of LongInt;
 T, Prev, Step, NDesc, First: Integer;

 { Lit les lignes de texte d'un bloc et ecrit N caracteres bruts. }
 Procedure TextBlock(Marker: String; Pad: Byte);
 Var Tot, DoneAll, BlkNo, Len, Raw, J, Hi, Lo, Ch: LongInt; Enc: String; Pending: Integer;
 Begin
  Tot := 0; DoneAll := 0; BlkNo := 0;
  If Marker = 'COMMENT' Then Tot := NComC Else Tot := NChar;
  While DoneAll < Tot Do Begin
   NextLine(S);
   Inc(BlkNo);
   If (Copy(Upper(S),1,Length('BEGIN_'+Marker+'_BLOCK')) <> 'BEGIN_'+Marker+'_BLOCK') Or
      (DecWord(S,2) <> BlkNo) Then
    Fail('BEGIN_'+Marker+'_BLOCK '+Num(BlkNo)+' attendu : '+S,1);
   Len := DecWord(S,3);
   If (Len < 1) Or (DoneAll+Len > Tot) Or ((Marker = 'COMMENT') And (Len > 1024)) Or
      (Len > 4096) Then Fail('Taille de bloc invalide : '+S,1);
   Raw := 0;
   Pending := 0; Hi := 0;
   While Raw < Len Do Begin
    NextLine(S);
    Enc := Unquote(S);
    J := 1;
    While (J <= Length(Enc)) And (Raw < Len) Do Begin
     If Pending > 0 Then Begin
      Lo := Pos(UpCase(Enc[J]),HexDigits)-1;
      If Lo < 0 Then Fail('Echappement @HH invalide : '+S,1);
      If Pending = 2 Then Begin Hi := Lo; Pending := 1 End
      Else Begin Ch := Hi*16+Lo; PutByte(Ch); Inc(Raw); Pending := 0 End;
     End Else If Enc[J] = '@' Then Pending := 2
     Else Begin PutByte(Ord(Enc[J])); Inc(Raw) End;
     Inc(J);
    End;
   End;
   If Pending <> 0 Then Fail('Echappement @HH incomplet dans le bloc '+Num(BlkNo)+'.',1);
   NextLine(S);
   If (Copy(Upper(S),1,Length('END_'+Marker+'_BLOCK')) <> 'END_'+Marker+'_BLOCK') Or
      (DecWord(S,2) <> BlkNo) Or (DecWord(S,3) <> Len) Then
    Fail('END_'+Marker+'_BLOCK '+Num(BlkNo)+' attendu : '+S,1);
   DoneAll := DoneAll+Len;
  End;
  NextLine(S);
  If (Copy(Upper(S),1,Length('TOTAL_'+Marker+'_BLOCKS')) <> 'TOTAL_'+Marker+'_BLOCKS') Or
     (DecWord(S,2) <> BlkNo) Or (DecWord(S,3) <> Tot) Then
   Fail('TOTAL_'+Marker+'_BLOCKS '+Num(BlkNo)+' '+Num(Tot)+' attendu : '+S,1);
 End;

 Procedure NumberBlocks(Marker: String; IsInt: Boolean; Tot: LongInt);
 Var DoneAll, BlkNo, Len, J: LongInt; IV: LongInt;
 Begin
  DoneAll := 0; BlkNo := 0;
  While DoneAll < Tot Do Begin
   NextLine(S);
   Inc(BlkNo);
   If (Copy(Upper(S),1,Length('BEGIN_'+Marker+'_BLOCK')) <> 'BEGIN_'+Marker+'_BLOCK') Or
      (DecWord(S,2) <> BlkNo) Then
    Fail('BEGIN_'+Marker+'_BLOCK '+Num(BlkNo)+' attendu : '+S,1);
   Len := DecWord(S,3);
   If (Len < 1) Or (Len > 1024) Or (DoneAll+Len > Tot) Then Fail('Taille de bloc invalide : '+S,1);
   For J := 1 To Len Do Begin
    If IsInt Then Begin
     NextLine(S);
     If Not HexToLong(Unquote(S),IV) Then Fail('Entier hexadecimal invalide : '+S,1);
     PutInt(IV);
    End Else PutDouble(ReadDP);
   End;
   NextLine(S);
   If (Copy(Upper(S),1,Length('END_'+Marker+'_BLOCK')) <> 'END_'+Marker+'_BLOCK') Or
      (DecWord(S,2) <> BlkNo) Or (DecWord(S,3) <> Len) Then
    Fail('END_'+Marker+'_BLOCK '+Num(BlkNo)+' attendu : '+S,1);
   DoneAll := DoneAll+Len;
  End;
  NextLine(S);
  If (Copy(Upper(S),1,Length('TOTAL_'+Marker+'_BLOCKS')) <> 'TOTAL_'+Marker+'_BLOCKS') Or
     (DecWord(S,2) <> BlkNo) Or (DecWord(S,3) <> Tot) Then
   Fail('TOTAL_'+Marker+'_BLOCKS '+Num(BlkNo)+' '+Num(Tot)+' attendu : '+S,1);
 End;

Begin
 NextLine(IDWord);
 IDWord := Unquote(IDWord);
 If Copy(IDWord,1,4) <> 'DAS/' Then Fail('Mot d''identification DAS attendu : '+IDWord,1);
 NextLine(S);
 IFName := PadTo(Unquote(S),60);
 NResvR := ReadHexInt(S); NResvC := ReadHexInt(S);
 NComR := ReadHexInt(S); NComC := ReadHexInt(S);
 If (NResvR <> 0) Or (NResvC <> 0) Then
  Fail('Zone reservee DAS (NRESVR/NRESVC) non prise en charge.',1);
 If (NComR < 0) Or (NComC < 0) Or (NComC > NComR*1024) Then
  Fail('Parametres de commentaires DAS invalides (NCOMR '+Num(NComR)+', NCOMC '+Num(NComC)+').',1);
 CreateOutput;
 { Enregistrement de fichier : ecrit en dernier; place reservee. }
 FillChar(Rec,SizeOf(Rec),0);
 WriteAt(1,Rec);
 NextRec := 2; BlockFill := 0;
 { Zone de commentaires : texte, blancs, puis enregistrements a zero. }
 If NComC > 0 Then Begin
  { NextLine est lu dans TextBlock apres l'en-tete deja consomme : aucun. }
  TextBlock('COMMENT',32);
  PadRecord(32);
 End;
 While NextRec < 2+NComR Do Begin
  FillChar(Rec,SizeOf(Rec),0);
  WriteAt(NextRec,Rec);
  Inc(NextRec);
 End;
 NChar := ReadHexInt(S); NDP := ReadHexInt(S); NInt := ReadHexInt(S);
 If (NChar < 0) Or (NDP < 0) Or (NInt < 0) Then Fail('Nombres de mots DAS invalides.',1);
 { Repertoire : mots 3-8 = (premiere, derniere) adresses par type, mot 9 = type
   du premier groupe, puis nombres d'enregistrements signes (+ type suivant
   dans le cycle caractere, reel, entier; - type precedent). }
 FillChar(Rec,SizeOf(Rec),0);
 Words[1] := NChar; Words[2] := NDP; Words[3] := NInt;
 Kinds[1] := Records(NChar,1024); Kinds[2] := Records(NDP,128); Kinds[3] := Records(NInt,256);
 For T := 1 To 3 Do
  If Words[T] > 0 Then Begin
   SetInt(Rec,(2*T)*4,1);
   SetInt(Rec,(2*T+1)*4,Words[T]);
  End;
 First := 0; Prev := 0; NDesc := 0;
 For T := 1 To 3 Do
  If Words[T] > 0 Then Begin
   If First = 0 Then Begin
    First := T;
    SetInt(Rec,32,T);
    SetInt(Rec,(9)*4,Kinds[T]);
   End Else Begin
    Step := T-Prev;
    If Step = 1 Then SetInt(Rec,(9+NDesc)*4,Kinds[T])
    Else SetInt(Rec,(9+NDesc)*4,-Kinds[T]);
   End;
   Inc(NDesc);
   Prev := T;
  End;
 WriteAt(NextRec,Rec);
 Inc(NextRec);
 { Donnees : caracteres, reels, entiers. }
 If NChar > 0 Then Begin TextBlock('CHARACTER',32); PadRecord(32) End;
 If NDP > 0 Then Begin NumberBlocks('DP',False,NDP); PadRecord(0) End;
 If NInt > 0 Then Begin NumberBlocks('INTEGER',True,NInt); PadRecord(0) End;
 { Enregistrement de fichier. }
 FillChar(Rec,SizeOf(Rec),0);
 SetText(Rec,0,PadTo(IDWord,8));
 SetText(Rec,8,IFName);
 SetInt(Rec,68,NResvR); SetInt(Rec,72,NResvC); SetInt(Rec,76,NComR); SetInt(Rec,80,NComC);
 SetText(Rec,84,'LTL-IEEE');
 SetText(Rec,699,FtpString);
 WriteAt(1,Rec);
 If Not Quiet Then
  WriteLn('DAS ',Trim(IDWord),' : ',NChar,' caractere(s), ',NDP,' reel(s), ',NInt,' entier(s).');
End;

{ Nom de sortie : extension x... devenue b..., sinon .bin ajoute. }
Function DefaultName(Path: String): String;
Var K, Cut: Integer;
Begin
 Cut := 0;
 For K := Length(Path) DownTo 1 Do
  If (Path[K] = '\') Or (Path[K] = '/') Or (Path[K] = ':') Then Break
  Else If (Path[K] = '.') And (Cut = 0) Then Cut := K;
 If (Cut > 0) And (Cut < Length(Path)) And (UpCase(Path[Cut+1]) = 'X') Then Begin
  If Path[Cut+1] = 'X' Then Path[Cut+1] := 'B' Else Path[Cut+1] := 'b';
  DefaultName := Path;
 End Else DefaultName := Path+'.bin';
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

Var Err: Word; First: String;

Begin
 XfrOpen := False; OutOpen := False; Force := False; Quiet := False;
 XfrName := ''; OutName := ''; Paths := 0; LineNo := 0;
 If ParamCount = 0 Then Begin Help; Halt(0) End;
 For ArgIndex := 1 To ParamCount Do Begin
  Arg := Upper(ParamStr(ArgIndex));
  If (Arg = '-H') Or (Arg = '-HELP') Or (Arg = '--HELP') Or (Arg = '/?') Then Begin
   Help; Halt(0);
  End;
 End;
 For ArgIndex := 1 To ParamCount Do Begin
  Arg := Upper(ParamStr(ArgIndex));
  If Arg = '-F' Then Force := True
  Else If Arg = '-Q' Then Quiet := True
  Else If Arg = '' Then SyntaxError('Nom de fichier vide.')
  Else If Arg[1] = '-' Then SyntaxError('Option inconnue : '+ParamStr(ArgIndex))
  Else Begin
   Inc(Paths);
   If Paths = 1 Then XfrName := ParamStr(ArgIndex)
   Else If Paths = 2 Then OutName := ParamStr(ArgIndex)
   Else SyntaxError('Deux fichiers au plus : transfert puis binaire.');
  End;
 End;
 If Paths = 0 Then SyntaxError('Fichier de transfert requis.');
 If OutName = '' Then OutName := DefaultName(XfrName);
 If Upper(OutName) = Upper(XfrName) Then
  SyntaxError('Les fichiers de transfert et binaire doivent differer.');

 Assign(XfrFile,XfrName);
 FileMode := 0;
 {$I-} Reset(XfrFile); Err := IOResult; {$I+}
 If Err <> 0 Then Fail('Impossible d''ouvrir '+XfrName+'.',1);
 XfrOpen := True;
 If Exists(OutName) And Not Force Then
  Fail(OutName+' existe deja. Utiliser -f pour l''ecraser.',1);
 NextLine(First);
 If Copy(Upper(First),1,6) = 'DAFETF' Then ConvertDAF
 Else If Copy(Upper(First),1,6) = 'DASETF' Then ConvertDAS
 Else Fail('Premiere ligne inattendue (DAFETF ou DASETF attendu) : '+Copy(First,1,40),1);

 Cleanup(True);
 If Not Quiet Then WriteLn('Fichier binaire ecrit : ',OutName);
End.
