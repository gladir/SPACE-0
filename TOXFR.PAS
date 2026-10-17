{ @author: Sylvain Maltais (support@gladir.com)
 @created: 2026
 @website(https://www.gladir.com/space-0)
 @abstract(Target: Turbo Pascal 7, Free Pascal 3.2) }

Program TOXFR;

{$A-}
{$E+,N+}

{ Conversion de fichiers SPICE binaires (DAF : SPK, CK, PCK; DAS : DSK, EK)
  vers le format de transfert ASCII "encoded transfer file" (DAFETF et
  DASETF), sous-ensemble autonome de TOXFR NAIF : aucune unite, aucune
  inclusion. Le format inverse (TOBIN) n'est pas fourni.
  Format DAFETF : identificateur, ND, NI, nom interne, puis pour chaque
  tableau BEGIN_ARRAY n m, nom, sommaire (reels, entiers sans les adresses
  initiale et finale), nombre de mots et mots, END_ARRAY, TOTAL_ARRAYS.
  Format DASETF : identificateur, nom interne, NRESVR, NRESVC, NCOMR, NCOMC,
  bloc de commentaires, nombres de mots caracteres/reels/entiers, puis blocs
  de 4096 caracteres ou 1024 reels/entiers (BEGIN_xxx_BLOCK ... TOTAL_xxx).
  Reels : chaine hexadecimale fraction^exposant (valeur = 0.HHH x 16^N, N en
  hexadecimal signe), entiers en hexadecimal signe. Caracteres : lignes de 64,
  NUL, apostrophe, @ et non imprimables ecrits @HH.
  Fichiers LTL-IEEE ou BIG-IEEE, pas de zone reservee DAS (NRESVR/NRESVC).
  Les commentaires DAF ne sont pas transferes (comme DAFBT).
  Sources : naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/ug/toxfr.html
            naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/daf.html
            naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/das.html
  Compilation : fpc -Mtp TOXFR.PAS
  Limites DOS : fichiers < 2 Gio, 500 groupes DAS, DAF jusqu'a 1000 mots de
  sommaire par record. }

Const
 MaxClusters = 500;
 WordsPer: Array[1..3] Of Integer = (1024,128,256);

Type
 TBlock = Array[0..1023] Of Byte;
 TBytes8 = Array[0..7] Of Byte;
 TCluster = Record
  Typ: Integer;
  First, Count, Start: LongInt;
 End;

Var
 Kernel: File;
 Buffer: TBlock;
 CacheRec: LongInt;
 Clusters: Array[1..MaxClusters] Of TCluster;
 NClusters: Integer;
 Opened, BigEndian, OutOpen, Force, Quiet: Boolean;
 FileBytes, NResvR, NResvC, NComR, NComC, LastLA: LongInt;
 LastChar, LastDP, LastInt: LongInt;
 OutFile: Text;
 OutBuf: Array[0..16383] Of Char;
 BinName, OutName, IDWord, IFName, Arg: String;
 LineBuf: String;
 ArgIndex, Paths, I: Integer;

Procedure Cleanup(KeepOutput: Boolean);
Var Err: Word;
Begin
 If Opened Then Begin
  {$I-} Close(Kernel); Err := IOResult; {$I+}
  Opened := False;
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
 WriteLn('Erreur : ',S);
 Cleanup(False);
 Halt(Code);
End;

Procedure SyntaxError(S: String);
Begin
 Fail(S+' Voir TOXFR -h.',2);
End;

Function Upper(S: String): String;
Var K: Integer;
Begin
 For K := 1 To Length(S) Do S[K] := UpCase(S[K]);
 Upper := S;
End;

Function Num(N: LongInt): String;
Var S: String;
Begin
 Str(N,S);
 Num := S;
End;

Function PadRight(S: String; Width: Integer): String;
Begin
 While Length(S) < Width Do S := S+' ';
 PadRight := S;
End;

Procedure Help;
Begin
 WriteLn('TOXFR - Cette commande permet de convertir les fichiers SPICE entre ',
         'formats binaires et formats de transfert.');
 WriteLn;
 WriteLn('Syntaxe : TOXFR [options] <fichier_binaire> [<fichier_transfert>]');
 WriteLn;
 WriteLn('Options :');
 WriteLn('  -f       Ecrase le fichier de transfert s''il existe');
 WriteLn('  -q       Silencieux (aucun message de succes)');
 WriteLn('  -h, /?   Aide (-help et --help acceptes)');
 WriteLn;
 WriteLn('Sans nom de sortie, l''extension b... devient x... (mission.bsp ->');
 WriteLn('mission.xsp, .bc -> .xc, .bpc -> .xpc, .bds -> .xds, .bes -> .xes);');
 WriteLn('sinon .xfr est ajoute.');
 WriteLn;
 WriteLn('Fichiers pris en charge : DAF (SPK, CK, PCK et autres tableaux DAF) et');
 WriteLn('DAS (DSK, EK) binaires LTL-IEEE ou BIG-IEEE. Le sens inverse (TOBIN)');
 WriteLn('n''est pas fourni.');
 WriteLn;
 WriteLn('Exemples :');
 WriteLn('  TOXFR mission.bsp');
 WriteLn('  TOXFR -f eros.bds eros.xds');
 WriteLn;
 WriteLn('Les commentaires des fichiers DAF ne sont pas transferes (comme DAFBT).');
 WriteLn('Codes retour : 0 succes, 1 erreur de lecture/format, 2 syntaxe.');
End;

{ ------------------------------------------------------------------ }
{ Encodage hexadecimal                                               }
{ ------------------------------------------------------------------ }

Const
 HexDigits: String[16] = '0123456789ABCDEF';

Function HexStr(L: LongInt): String;
Var S: String; Neg: Boolean;
Begin
 If L = 0 Then Begin HexStr := '0'; Exit End;
 If L = -2147483647-1 Then Begin HexStr := '-80000000'; Exit End;
 Neg := L < 0;
 If Neg Then L := -L;
 S := '';
 While L > 0 Do Begin
  S := HexDigits[(L Mod 16)+1]+S;
  L := L Div 16;
 End;
 If Neg Then S := '-'+S;
 HexStr := S;
End;

Function Hex2(C: Integer): String;
Begin
 Hex2 := HexDigits[(C Div 16)+1]+HexDigits[(C Mod 16)+1];
End;

{ Reel en "0.HHHH x 16^N" : mantisse hexadecimale sans zero final et
  exposant hexadecimal signe (zero : 0^0). }
Function DPHex(V: Double): String;
Var
 B: TBytes8;
 Sg, M: Array[0..60] Of Byte;
 Exp11, I, P, Shift, Bexp, S, N, J, Digit, Last: Integer;
 Neg: Boolean;
 T: String;
 Fraction: Boolean;
Begin
 Move(V,B,8);
 Neg := (B[7] And $80) <> 0;
 Exp11 := Integer(B[7] And $7F)*16+Integer(B[6] Shr 4);
 If Exp11 = 2047 Then Fail('Valeur flottante non finie dans le fichier.',1);
 FillChar(Sg,SizeOf(Sg),0);
 FillChar(M,SizeOf(M),0);
 { Bits de la fraction : 4 bits de B[6] puis B[5]..B[0]. }
 For I := 0 To 3 Do
  If (B[6] And (1 Shl I)) <> 0 Then Sg[48+I] := 1;
 For J := 0 To 5 Do
  For I := 0 To 7 Do
   If (B[J] And (1 Shl I)) <> 0 Then Sg[J*8+I] := 1;
 Fraction := False;
 For I := 0 To 51 Do If Sg[I] <> 0 Then Fraction := True;
 If Exp11 = 0 Then Begin
  If Not Fraction Then Begin DPHex := '0^0'; Exit End;
  P := 51;
  While Sg[P] = 0 Do Dec(P);
  Shift := 52-P;
  For I := 51 DownTo 0 Do Begin
   If I+Shift <= 52 Then Sg[I+Shift] := Sg[I];
   If Shift > 0 Then Sg[I] := 0;
  End;
  Bexp := P-1074;
 End Else Begin
  Sg[52] := 1;
  Bexp := Exp11-1023;
 End;
 { b = 4*q + s avec 0 <= s <= 3 ; N = q + 1. }
 S := ((Bexp Mod 4)+4) Mod 4;
 N := (Bexp-S) Div 4+1;
 For I := 0 To 52 Do M[I+S] := Sg[I];
 T := '';
 Last := 0;
 For J := 0 To 13 Do Begin
  Digit := M[55-4*J]*8+M[54-4*J]*4+M[53-4*J]*2+M[52-4*J];
  If Digit <> 0 Then Last := J;
  T := T+HexDigits[Digit+1];
 End;
 T := Copy(T,1,Last+1);
 If Neg Then T := '-'+T;
 DPHex := T+'^'+HexStr(N);
End;

{ ------------------------------------------------------------------ }
{ Lecture des fichiers binaires                                      }
{ ------------------------------------------------------------------ }

Function TextAt(Offset, Count: Integer): String;
Var K: Integer; S: String;
Begin
 S := '';
 For K := Offset To Offset+Count-1 Do S := S+Chr(Buffer[K]);
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
  Fail('Valeur flottante non finie dans '+BinName+'.',1);
 Move(B,V,8);
 DoubleAt := V;
End;

Procedure ReadRecord(Number: LongInt);
Var Got, Err: Word;
Begin
 If CacheRec = Number Then Exit;
 CacheRec := 0;
 If (Number < 1) Or (Number > FileBytes Div 1024) Then
  Fail('Record '+Num(Number)+' hors du fichier '+BinName+'.',1);
 {$I-}
 Seek(Kernel,(Number-1)*1024);
 Err := IOResult;
 {$I+}
 If Err <> 0 Then Fail('Impossible de positionner la lecture.',1);
 {$I-} BlockRead(Kernel,Buffer,1024,Got); Err := IOResult; {$I+}
 If (Err <> 0) Or (Got <> 1024) Then Fail('Record '+Num(Number)+' tronque.',1);
 CacheRec := Number;
End;

Procedure OpenBinary(Path: String);
Var Err: Word;
Begin
 Assign(Kernel,Path);
 FileMode := 0;
 {$I-} Reset(Kernel,1); Err := IOResult; {$I+}
 If Err <> 0 Then Fail('Impossible d''ouvrir '+Path+'.',1);
 Opened := True;
 {$I-} FileBytes := FileSize(Kernel); Err := IOResult; {$I+}
 If (Err <> 0) Or (FileBytes < 2048) Or (FileBytes Mod 1024 <> 0) Then
  Fail('Taille invalide ou fichier tronque : '+Path,1);
 CacheRec := 0;
 BigEndian := False;
 ReadRecord(1);
 IDWord := TextAt(0,8);
End;

{ ------------------------------------------------------------------ }
{ Sortie                                                             }
{ ------------------------------------------------------------------ }

Procedure Put(S: String);
Var Err: Word;
Begin
 {$I-} WriteLn(OutFile,S); Err := IOResult; {$I+}
 If Err <> 0 Then Fail('Erreur d''ecriture de '+OutName+' (disque plein ?).',1);
End;

Procedure PutQuoted(S: String);
Begin
 Put(''''+S+'''');
End;

{ Lignes de 64 caracteres de texte encode. }
Procedure PushText(S: String);
Var K: Integer;
Begin
 For K := 1 To Length(S) Do Begin
  LineBuf := LineBuf+S[K];
  If Length(LineBuf) = 64 Then Begin PutQuoted(LineBuf); LineBuf := '' End;
 End;
End;

Procedure FlushText;
Begin
 If LineBuf <> '' Then Begin
  PutQuoted(PadRight(LineBuf,64));
  LineBuf := '';
 End;
End;

Procedure PushByte(C: Integer);
Begin
 If (C < 32) Or (C > 126) Or (C = 39) Or (C = 64) Then PushText('@'+Hex2(C))
 Else PushText(Chr(C));
End;

{ ------------------------------------------------------------------ }
{ DAF                                                                }
{ ------------------------------------------------------------------ }

Function DafWord(Addr: LongInt): Double;
Begin
 If (Addr < 1) Or (Addr > FileBytes Div 8) Then
  Fail('Adresse DAF '+Num(Addr)+' hors du fichier.',1);
 ReadRecord((Addr-1) Div 128+1);
 DafWord := DoubleAt(Integer((Addr-1) Mod 128)*8);
End;

Procedure ConvertDAF;
Var
 ND, NI, SS, J, K, N, Offset, Slot, NSum, ArrayNo: Integer;
 Fwd, Bwd, Free, Current, Previous, Visited, Next, Prev, Initial, Final, Words, A, Chunk: LongInt;
 V, Ctl: Double;
 Summary: Array[1..250] Of Double;
 Ints: Array[1..250] Of LongInt;
 Name: String;
 NameRec: LongInt;
Begin
 If TextAt(88,8) = 'LTL-IEEE' Then BigEndian := False
 Else If TextAt(88,8) = 'BIG-IEEE' Then BigEndian := True
 Else Fail('Format binaire DAF non pris en charge : '+TextAt(88,8),1);
 ND := IntAt(8); NI := IntAt(12);
 SS := ND+(NI+1) Div 2;
 If (ND < 0) Or (ND > 124) Or (NI < 2) Or (NI > 250) Or (SS < 1) Or (SS > 125) Then
  Fail('Dimensions ND/NI invalides dans '+BinName+'.',1);
 IFName := TextAt(16,60);
 Fwd := IntAt(76); Bwd := IntAt(80); Free := IntAt(84);
 If (Free < 129) Or (Free > FileBytes Div 8+1) Then
  Fail('Adresse libre DAF hors fichier.',1);
 Put('DAFETF NAIF DAF ENCODED TRANSFER FILE');
 PutQuoted(IDWord);
 PutQuoted(HexStr(ND));
 PutQuoted(HexStr(NI));
 PutQuoted(IFName);
 ArrayNo := 0;
 If (Fwd <> 0) Or (Bwd <> 0) Then Begin
  If (Fwd < 2) Or (Bwd < 2) Then Fail('Chaine de descripteurs DAF invalide.',1);
  Current := Fwd; Previous := 0; Visited := 0;
  While Current <> 0 Do Begin
   Inc(Visited);
   If (Visited > FileBytes Div 1024) Or (Current >= FileBytes Div 1024) Then
    Fail('Chaine de records DAF incoherente.',1);
   ReadRecord(Current);
   Ctl := DoubleAt(0); Next := Trunc(Ctl);
   Ctl := DoubleAt(8); Prev := Trunc(Ctl);
   Ctl := DoubleAt(16); NSum := Trunc(Ctl);
   If (Next < 0) Or (Next > FileBytes Div 1024) Or (Prev <> Previous) Or
      (NSum < 0) Or (NSum > (1000 Div (8*SS))) Then
    Fail('Record sommaire '+Num(Current)+' invalide.',1);
   NameRec := Current+1;
   For Slot := 0 To NSum-1 Do Begin
    ReadRecord(Current);
    Offset := 24+Slot*SS*8;
    For K := 1 To ND Do Summary[K] := DoubleAt(Offset+(K-1)*8);
    For K := 1 To NI Do Ints[K] := IntAt(Offset+ND*8+(K-1)*4);
    Initial := Ints[NI-1]; Final := Ints[NI];
    If (Initial < 129) Or (Final < Initial) Or (Final >= Free) Or
       (Final > FileBytes Div 8) Then
     Fail('Adresses de tableau DAF invalides (record '+Num(Current)+').',1);
    Words := Final-Initial+1;
    ReadRecord(NameRec);
    Name := TextAt(Slot*SS*8,SS*8);
    Inc(ArrayNo);
    Put('BEGIN_ARRAY '+Num(ArrayNo)+' '+Num(Words));
    PutQuoted(Name);
    For K := 1 To ND Do PutQuoted(DPHex(Summary[K]));
    For K := 1 To NI-2 Do PutQuoted(HexStr(Ints[K]));
    { Mots par tranches de 1024 precedees de leur nombre. }
    A := Initial;
    While A <= Final Do Begin
     Chunk := Final-A+1;
     If Chunk > 1024 Then Chunk := 1024;
     Put(Num(Chunk));
     For K := 1 To Chunk Do Begin
      PutQuoted(DPHex(DafWord(A)));
      Inc(A);
     End;
    End;
    Put('END_ARRAY '+Num(ArrayNo)+' '+Num(Words));
   End;
   ReadRecord(Current);
   Previous := Current; Current := Next;
  End;
  If Previous <> Bwd Then Fail('Dernier record sommaire incoherent.',1);
 End;
 Put('TOTAL_ARRAYS '+Num(ArrayNo));
End;

{ ------------------------------------------------------------------ }
{ DAS                                                                }
{ ------------------------------------------------------------------ }

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
 Fail('Adresse logique '+Num(A)+' hors des donnees DAS.',1);
End;

Procedure ReadDirectories;
Var
 Rec, Fwd, Phys, Count, Visited, L: LongInt;
 T, W, Sign: Integer;
 LogStart: Array[1..3] Of LongInt;
Begin
 NClusters := 0;
 For T := 1 To 3 Do LogStart[T] := 1;
 LastChar := 0; LastDP := 0; LastInt := 0;
 Rec := 2+NResvR+NComR; Visited := 0;
 While Rec <> 0 Do Begin
  Inc(Visited);
  If Visited > FileBytes Div 1024 Then Fail('Repertoires DAS cycliques.',1);
  ReadRecord(Rec);
  Fwd := IntAt(4);
  L := IntAt(12); If L > LastChar Then LastChar := L;
  L := IntAt(20); If L > LastDP Then LastDP := L;
  L := IntAt(28); If L > LastInt Then LastInt := L;
  T := IntAt(32);
  If (T < 1) Or (T > 3) Then Fail('Type du premier groupe DAS invalide.',1);
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
 If (LastChar > LogStart[1]-1) Or (LastDP > LogStart[2]-1) Or (LastInt > LogStart[3]-1) Then
  Fail('Compteurs de mots DAS superieurs aux groupes decrits.',1);
End;

{ Octet de la zone de commentaires (Mode 1) ou donnee caractere (Mode 2),
  d'index Index base 0. }
Function TextByte(Mode: Integer; Index: LongInt): Integer;
Var Rec: LongInt; Off: Integer;
Begin
 If Mode = 1 Then Begin
  Rec := 2+NResvR+Index Div 1024;
  Off := Integer(Index Mod 1024);
 End Else Locate(1,Index+1,Rec,Off);
 ReadRecord(Rec);
 TextByte := Buffer[Off];
End;

Procedure TextBlocks(Name: String; Mode: Integer; Total, BlockSize: LongInt);
Var BlockNo, Done, N, K: LongInt;
Begin
 BlockNo := 0; Done := 0; LineBuf := '';
 While Done < Total Do Begin
  N := Total-Done;
  If N > BlockSize Then N := BlockSize;
  Inc(BlockNo);
  Put('BEGIN_'+Name+'_BLOCK '+Num(BlockNo)+' '+Num(N));
  For K := 0 To N-1 Do PushByte(TextByte(Mode,Done+K));
  FlushText;
  Put('END_'+Name+'_BLOCK '+Num(BlockNo)+' '+Num(N));
  Done := Done+N;
 End;
 Put('TOTAL_'+Name+'_BLOCKS '+Num(BlockNo)+' '+Num(Total));
End;

Procedure NumberBlocks(Name: String; IsInt: Boolean; Total: LongInt);
Var BlockNo, Done, N, K, Rec: LongInt; Off: Integer;
Begin
 BlockNo := 0; Done := 0;
 While Done < Total Do Begin
  N := Total-Done;
  If N > 1024 Then N := 1024;
  Inc(BlockNo);
  Put('BEGIN_'+Name+'_BLOCK '+Num(BlockNo)+' '+Num(N));
  For K := 1 To N Do Begin
   If IsInt Then Begin
    Locate(3,Done+K,Rec,Off);
    ReadRecord(Rec);
    PutQuoted(HexStr(IntAt(Off*4)));
   End Else Begin
    Locate(2,Done+K,Rec,Off);
    ReadRecord(Rec);
    PutQuoted(DPHex(DoubleAt(Off*8)));
   End;
  End;
  Put('END_'+Name+'_BLOCK '+Num(BlockNo)+' '+Num(N));
  Done := Done+N;
 End;
 Put('TOTAL_'+Name+'_BLOCKS '+Num(BlockNo)+' '+Num(Total));
End;

Procedure ConvertDAS;
Begin
 If TextAt(84,8) = 'LTL-IEEE' Then BigEndian := False
 Else If TextAt(84,8) = 'BIG-IEEE' Then BigEndian := True
 Else Fail('Format binaire DAS non pris en charge : '+TextAt(84,8),1);
 IFName := TextAt(8,60);
 NResvR := IntAt(68); NResvC := IntAt(72); NComR := IntAt(76); NComC := IntAt(80);
 If (NResvR < 0) Or (NResvC < 0) Or (NComR < 0) Or (NComC < 0) Or
    (2+NResvR+NComR > FileBytes Div 1024) Or (NComC > NComR*1024) Then
  Fail('Parametres du fichier DAS invalides.',1);
 If (NResvR <> 0) Or (NResvC <> 0) Then
  Fail('Zone reservee DAS (NRESVR/NRESVC) non prise en charge.',1);
 ReadDirectories;
 Put('DASETF NAIF DAS ENCODED TRANSFER FILE');
 PutQuoted(IDWord);
 PutQuoted(IFName);
 PutQuoted(HexStr(NResvR));
 PutQuoted(HexStr(NResvC));
 PutQuoted(HexStr(NComR));
 PutQuoted(HexStr(NComC));
 If NComC > 0 Then TextBlocks('COMMENT',1,NComC,1024);
 PutQuoted(HexStr(LastChar));
 PutQuoted(HexStr(LastDP));
 PutQuoted(HexStr(LastInt));
 If LastChar > 0 Then TextBlocks('CHARACTER',2,LastChar,4096);
 If LastDP > 0 Then NumberBlocks('DP',False,LastDP);
 If LastInt > 0 Then NumberBlocks('INTEGER',True,LastInt);
End;

{ Nom de sortie : extension b... devenue x..., sinon .xfr ajoute. }
Function DefaultName(Path: String): String;
Var K, Cut: Integer;
Begin
 Cut := 0;
 For K := Length(Path) DownTo 1 Do
  If (Path[K] = '\') Or (Path[K] = '/') Or (Path[K] = ':') Then Break
  Else If (Path[K] = '.') And (Cut = 0) Then Cut := K;
 If (Cut > 0) And (Cut < Length(Path)) And (UpCase(Path[Cut+1]) = 'B') Then Begin
  If Path[Cut+1] = 'B' Then Path[Cut+1] := 'X' Else Path[Cut+1] := 'x';
  DefaultName := Path;
 End Else DefaultName := Path+'.xfr';
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

Begin
 Opened := False; OutOpen := False; Force := False; Quiet := False;
 BinName := ''; OutName := ''; Paths := 0; LineBuf := '';
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
   If Paths = 1 Then BinName := ParamStr(ArgIndex)
   Else If Paths = 2 Then OutName := ParamStr(ArgIndex)
   Else SyntaxError('Deux fichiers au plus : binaire puis transfert.');
  End;
 End;
 If Paths = 0 Then SyntaxError('Fichier binaire requis.');
 If OutName = '' Then OutName := DefaultName(BinName);
 If Upper(OutName) = Upper(BinName) Then SyntaxError('Les fichiers binaire et de transfert doivent differer.');

 OpenBinary(BinName);
 If Exists(OutName) And Not Force Then
  Fail(OutName+' existe deja. Utiliser -f pour l''ecraser.',1);
 Assign(OutFile,OutName);
 SetTextBuf(OutFile,OutBuf);
 {$I-} Rewrite(OutFile); I := IOResult; {$I+}
 If I <> 0 Then Fail('Impossible de creer '+OutName+'.',1);
 OutOpen := True;

 If Copy(IDWord,1,4) = 'DAF/' Then ConvertDAF
 Else If IDWord = 'NAIF/DAF' Then ConvertDAF
 Else If Copy(IDWord,1,4) = 'DAS/' Then ConvertDAS
 Else Begin
  For I := 1 To 8 Do
   If (IDWord[I] < ' ') Or (IDWord[I] > '~') Then IDWord[I] := '?';
  Fail('Fichier non reconnu (mot d''identification "'+IDWord+'") : ni DAF ni DAS.',1);
 End;

 Cleanup(True);
 If Not Quiet Then WriteLn('Fichier de transfert ecrit : ',OutName);
End.
