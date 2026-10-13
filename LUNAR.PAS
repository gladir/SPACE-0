{ @author: Sylvain Maltais (support@gladir.com)
 @created: 2026
 @website(https://www.gladir.com/space-0)
 @abstract(Target: Turbo Pascal 7, Free Pascal 3.2) }

Program LUNAR;

{ Catalogue historique hors ligne, volontairement non exhaustif (1959-1972).
  Compatible Turbo Pascal 7 et Free Pascal : aucune unite externe.
  Dates UTC. Coordonnees des modules Apollo, degres N/E signes,
  repere Mean Earth/Polar Axis DE421.
  Sources : https://science.nasa.gov/moon/missions/
            https://nssdc.gsfc.nasa.gov/planetary/lunar/lunar_sites.html
  Compilation : fpc -Mtp LUNAR.PAS }

Const
 MaxMissions = 18;

Type
 TMission = Record
  Name: String[24];
  Country, Agency: String[16];
  Launch, EventDate: String[10];
  Event, Site: String[60];
  Latitude, Longitude: String[12];
  Note: String[100];
  Surface: Boolean;
 End;

Var
 Missions: Array[1..MaxMissions] Of TMission;
 Count, I, Found: Integer;
 Command, Query: String;

Procedure AddMission(Name, Country, Agency, Launch, EventDate,  Event, Site, Latitude,
                     Longitude, Note: String; Surface: Boolean);Begin
 Inc(Count);
 Missions[Count].Name := Name;
 Missions[Count].Country := Country;
 Missions[Count].Agency := Agency;
 Missions[Count].Launch := Launch;
 Missions[Count].EventDate := EventDate;
 Missions[Count].Event := Event;
 Missions[Count].Site := Site;
 Missions[Count].Latitude := Latitude;
 Missions[Count].Longitude := Longitude;
 Missions[Count].Note := Note;
 Missions[Count].Surface := Surface;
End;

Procedure InitCatalogue;Begin
 Count := 0;
 AddMission('Luna 1','USSR','URSS','1959-01-02','1959-01-04',
 'Survol','', '', '', 'Premier survol lunaire; impact prevu manque.',False);
 AddMission('Luna 2','USSR','URSS','1959-09-12','1959-09-13',
 'Impact volontaire','', '', '', 'Premier engin a atteindre la surface lunaire.',True);
 AddMission('Luna 3','USSR','URSS','1959-10-04','1959-10-06',
 'Survol','', '', '', 'Premieres images de la face cachee.',False);
 AddMission('Ranger 7','USA','NASA','1964-07-28','1964-07-31',
 'Impact volontaire','Mare Cognitum', '', '', 'Photographies avant impact.',True);
 AddMission('Luna 9','USSR','URSS','1966-01-31','1966-02-03',
 'Alunissage en douceur','Oceanus Procellarum', '', '',
 'Premier alunissage en douceur; images de la surface.',True);
 AddMission('Luna 10','USSR','URSS','1966-03-31','1966-04-03',
 'Mise en orbite','', '', '', 'Premier satellite artificiel de la Lune.',False);
 AddMission('Surveyor 1','USA','NASA','1966-05-30','1966-06-02',
 'Alunissage en douceur','Oceanus Procellarum', '', '',
 'Premier alunissage americain en douceur.',True);
 AddMission('Surveyor 2','USA','NASA','1966-09-20','1966-09-23',
 'Impact accidentel','', '', '', 'Echec de la tentative d''alunissage en douceur.',True);
 AddMission('Surveyor 3','USA','NASA','1967-04-17','1967-04-20',
 'Alunissage en douceur','Oceanus Procellarum', '', '',
 'Visite par les astronautes d''Apollo 12.',True);
 AddMission('Apollo 8','USA','NASA','1968-12-21','1968-12-24',
 'Mise en orbite','', '', '', 'Premier equipage en orbite lunaire.',False);
 AddMission('Apollo 10','USA','NASA','1969-05-18','1969-05-21',
 'Mise en orbite','', '', '', 'Repetition de la mission d''alunissage.',False);
 AddMission('Apollo 11','USA','NASA','1969-07-16','1969-07-20',
 'Alunissage habite','Mare Tranquillitatis','0.67416','23.47314',
 'Premier alunissage habite : Neil Armstrong et Buzz Aldrin; module Eagle.',True);
 AddMission('Apollo 12','USA','NASA','1969-11-14','1969-11-19',
 'Alunissage habite','Oceanus Procellarum','-3.0128','-23.4219',
 'Pete Conrad et Alan Bean; module Intrepid.',True);
 AddMission('Apollo 13','USA','NASA','1970-04-11','1970-04-15',
 'Survol','', '', '', 'Alunissage annule apres un accident; retour de l''equipage.',False)
 ;
 AddMission('Apollo 14','USA','NASA','1971-01-31','1971-02-05',
 'Alunissage habite','Fra Mauro','-3.64589','-17.47194',
 'Alan Shepard et Edgar Mitchell; module Antares.',True);
 AddMission('Apollo 15','USA','NASA','1971-07-26','1971-07-30',
 'Alunissage habite','Hadley-Apennine','26.13239','3.63330',
 'David Scott et James Irwin; module Falcon; rover lunaire.',True);
 AddMission('Apollo 16','USA','NASA','1972-04-16','1972-04-21',
 'Alunissage habite','Descartes','-8.9734','15.5011',
 'John Young et Charles Duke; module Orion; rover lunaire.',True);
 AddMission('Apollo 17','USA','NASA','1972-12-07','1972-12-11',
 'Alunissage habite','Taurus-Littrow','20.1911','30.7723',
 'Eugene Cernan et Harrison Schmitt; module Challenger; rover lunaire.',True);
End;

Function Upper(S: String): String;
Var
 J: Integer;
Begin
 For J := 1 To Length(S) Do S[J] := UpCase(S[J]);
 Upper := S;
End;

Function Key(S: String): String;
Var
 J: Integer;
 R: String;
Begin
 R := '';
 S := Upper(S);
 For J := 1 To Length(S) Do    If Not (S[J] In [' ', '-', '_', #9]) Then R := R + S[J];
 Key := R;
End;

Procedure Fail(S: String; Code: Integer);Begin
 WriteLn('Erreur : ', S);
 Halt(Code);
End;

Procedure Help;Begin
 WriteLn('LUNAR - Cette commande permet d''effectuer une recherche des missions et ',
         'alunissages lunaires'); 
 WriteLn('Catalogue historique integre : 18 missions, 1959-1972, non exhaustif.');
 WriteLn;
 WriteLn('Syntaxe :');
 WriteLn('  LUNAR MISSIONS                   Liste le catalogue');
 WriteLn('  LUNAR LANDINGS                   Missions ayant atteint la surface');
 WriteLn('                                   (alunissages et impacts distingues)');
 WriteLn('  LUNAR LANDING <mission>           Informations sur une mission');
 WriteLn('  LUNAR SITE <mission>              Site et coordonnees disponibles');
 WriteLn('  LUNAR DATE <AAAA-MM-JJ>           Lancements et evenements en UTC');
 WriteLn('  LUNAR COUNTRY <pays-ou-organisme> Filtre par pays ou organisme');
 WriteLn('  LUNAR SEARCH <texte>             Recherche nom, site ou mot-cle');
 WriteLn('  LUNAR DATASET                    Liens vers les archives scientifiques');
 WriteLn('  LUNAR EXPORT CSV                 Catalogue CSV sur la sortie standard');
 WriteLn('  LUNAR HELP                       Cette aide');
 WriteLn;
 WriteLn('Les parametres entre < > sont obligatoires : remplacer leur contenu');
 WriteLn('sans saisir les chevrons. CSV est le seul format d''export accepte.');
 WriteLn('Utiliser des guillemets pour un nom ou un texte contenant des espaces.');
 WriteLn;
 WriteLn('Exemples :');
 WriteLn('  LUNAR LANDING APOLLO11');
 WriteLn('  LUNAR SITE "Apollo 11"');
 WriteLn('  LUNAR DATE 1969-07-20');
 WriteLn('  LUNAR COUNTRY USA');
 WriteLn('  LUNAR COUNTRY NASA');
 WriteLn('  LUNAR SEARCH "Apollo 11"');
 WriteLn('  LUNAR EXPORT CSV > missions.csv');
 WriteLn;
 WriteLn('Codes retour : 0 succes, 1 aucun resultat, 2 syntaxe invalide.');
End;

Function ValidDate(S: String): Boolean;
Const
 Days: Array[1..12] Of Integer = (31,28,31,30,31,30,31,31,30,31,30,31);
Var
 J, Y, M, D, E, Limit: Integer;
Begin
 ValidDate := False;
 If Length(S) <> 10 Then Exit;
 If (S[5] <> '-') Or (S[8] <> '-') Then Exit;
 For J := 1 To 10 Do    If (J <> 5) And (J <> 8) Then      If Not (S[J] In ['0'..'9'])
 Then Exit;
 Val(Copy(S,1,4),Y,E); If E <> 0 Then Exit;
 Val(Copy(S,6,2),M,E); If E <> 0 Then Exit;
 Val(Copy(S,9,2),D,E); If E <> 0 Then Exit;
 If (Y < 1) Or (M < 1) Or (M > 12) Then Exit;
 Limit := Days[M];
 If (M = 2) And (Y Mod 4 = 0) And
 ((Y Mod 100 <> 0) Or (Y Mod 400 = 0)) Then Inc(Limit);
 ValidDate := (D >= 1) And (D <= Limit);
End;

Procedure Row(N: Integer);Begin
 With Missions[N] Do    WriteLn(Name:12, ' | ',Country:4,' | ',EventDate,' | ',Event);
End;

Procedure SiteInfo(N: Integer);Begin
 With Missions[N] Do Begin
  If Not Surface Then WriteLn('Cette mission n''a pas atteint la surface.')
   Else
  Begin
   If Site = '' Then WriteLn('Site : non renseigne dans ce catalogue.')
                Else WriteLn('Site : ',Site);
   If Latitude = '' Then WriteLn('Coordonnees : non disponibles dans ce catalogue.')
    Else
   Begin
    WriteLn('Latitude : ',Latitude,' degres N; longitude : ',Longitude,' degres E.');
    WriteLn('Valeurs signees : negatif = sud/ouest. Repere ME/PA DE421.');
   End;
  End;
 End;
End;

Function Csv(S: String): String;
Var
 J: Integer;
 R: String;
Begin
 R := '"';
 For J := 1 To Length(S) Do Begin
  If S[J] = '"' Then R := R + '"';
  R := R + S[J];
 End;
 Csv := R + '"';
End;

Procedure ExportCsv;
Var
 N: Integer;
 Flag: String;
Begin
 WriteLn(
 'name,country,agency,launch_date,event_date,event,surface,site,latitude_n,longitude_e,note');
 For N := 1 To Count Do With Missions[N] Do Begin
  If Surface Then Flag := 'true' Else Flag := 'false';
  WriteLn(Csv(Name),',',Csv(Country),',',Csv(Agency),',',Csv(Launch),',',
  Csv(EventDate),',',Csv(Event),',',Flag,',',Csv(Site),',',
  Latitude,',',Longitude,',',Csv(Note));
 End;
End;

Procedure Datasets;Begin
 WriteLn('Archives scientifiques en ligne (acces avec un navigateur) :');
 WriteLn('NASA Planetary Data System : donnees planetaires et lunaires');
 WriteLn('  https://pds.nasa.gov/');
 WriteLn('PDS Geosciences : experiences Apollo, LRO et autres missions');
 WriteLn('  https://pds-geosciences.wustl.edu/');
 WriteLn('LROC : images de Lunar Reconnaissance Orbiter');
 WriteLn('  https://www.lroc.asu.edu/');
 WriteLn('Apollo Lunar Surface Journal : journaux, photos et transcriptions');
 WriteLn('  https://www.nasa.gov/history/alsj/');
 WriteLn('Sources du catalogue :');
 WriteLn('  https://science.nasa.gov/moon/missions/');
 WriteLn('  https://nssdc.gsfc.nasa.gov/planetary/lunar/lunar_sites.html');
End;

BEGIN
 InitCatalogue;
 If ParamCount = 0 Then Begin
  Help; Halt(0)
 End;
 Command := Upper(ParamStr(1));
 Query := '';
 For I := 2 To ParamCount Do Begin
  If Length(Query) + Length(ParamStr(I)) + 1 > 255 Then      Fail('Argument trop long.',2)
  ;
  If I > 2 Then Query := Query + ' ';
  Query := Query + ParamStr(I);
 End;
 If (Command = 'HELP') Or (Command = '/?') Or (Command = '--HELP') Then Begin
  If ParamCount <> 1 Then Fail('HELP ne prend aucun argument.',2);
  Help; Halt(0);
 End;
 If (Command = 'MISSIONS') Or (Command = 'LANDINGS') Or
 (Command = 'DATASET') Then Begin
  If ParamCount <> 1 Then Fail('Cette commande ne prend aucun argument.',2);
 End Else If (Command = 'LANDING') Or (Command = 'SITE') Or
 (Command = 'COUNTRY') Or (Command = 'SEARCH') Then Begin
  If Key(Query) = '' Then Fail('Argument de recherche requis. Voir LUNAR HELP.',2);
 End Else If Command = 'DATE' Then Begin
  If (ParamCount <> 2) Or Not ValidDate(Query) Then      Fail(
  'Date invalide : utiliser AAAA-MM-JJ.',2);
 End Else If Command = 'EXPORT' Then Begin
  If (ParamCount <> 2) Or (Upper(Query) <> 'CSV') Then      Fail(
  'Usage : LUNAR EXPORT CSV > missions.csv',2);
  ExportCsv; Halt(0);
 End Else Fail('Sous-commande inconnue. Voir LUNAR HELP.',2);
 If Command = 'DATASET' Then Begin
  Datasets; Halt(0)
 End;
 Found := 0;
 For I := 1 To Count Do With Missions[I] Do Begin
  If ((Command = 'MISSIONS') Or
  ((Command = 'LANDINGS') And Surface) Or
  (((Command = 'LANDING') Or (Command = 'SITE')) And (Key(Name) = Key(Query))) Or
  ((Command = 'COUNTRY') And ((Key(Country) = Key(Query)) Or (Key(Agency) = Key(Query))))
  Or
  ((Command = 'SEARCH') And
  (Pos(Key(Query),Key(Name+' '+Country+' '+Agency+' '+Event+' '+Site+' '+Note)) > 0)) Or
  ((Command = 'DATE') And ((Launch = Query) Or (EventDate = Query)))) Then Begin
   Inc(Found);
   If Command <> 'DATE' Then Row(I);
   If Command = 'DATE' Then Begin
    WriteLn(Name,' | ',Country,' | ',Query);
    If Launch = Query Then WriteLn('  Lancement (UTC)');
    If EventDate = Query Then WriteLn('  ',Event,' (UTC)');
   End;
   If Command = 'LANDING' Then Begin
    WriteLn('Organisme : ',Agency,'; lancement : ',Launch,' (UTC)');
    WriteLn(Note);
   End;
   If (Command = 'LANDING') Or (Command = 'SITE') Then SiteInfo(I);
  End;
 End;
 If Found = 0 Then Fail('Aucun resultat dans le catalogue historique non exhaustif.',1);
 WriteLn(Found,' mission(s). Catalogue historique non exhaustif (1959-1972).');
END.
