# SPACE-0
Suite de commande écrit en Turbo Pascal/Free Pascal pour le secteur de l'aérospatiale.

<h2>Liste des fichiers</h2>

Voici la liste des différents fichiers proposés dans SPACE-0 :

<table>
		<tr>
			<th>Nom</th>
			<th>Description</th>	
		</tr>
	    <tr>
			<td><b>BRIEF.PAS</b></td>
			<td>Cette commande permet de résumer de maniere concise des objets et couvertures temporelles SPK.</td>
		</tr>
		<tr>
			<td><b>CKBRIEF.PAS</b></td>
			<td>Cette commande permet de résumer le contenu et la couverture temporelle des fichiers d'orientation CK.</td>
		</tr>
		<tr>
			<td><b>CHRONOS.PAS</b></td>
			<td>Cette commande permet d'effectuer la conversion des systèmes et formats temporels.</td>
		</tr>
		<tr>
			<td><b>FRMDIFF.PAS</b></td>
			<td>Cette commande permet de comparer des repères de référence.</td>
		</tr>
		<tr>
			<td><b>DSKBRIEF.PAS</b></td>
			<td>Cette commande permet d'examiner la couverture spatiale et les attributs de modèles de surface DSK.</td>
		</tr>
        <tr>
			 <td><b>LUNAR.PAS</b></td>
			 <td>Cette commande permet d'effectuer une recherche des missions et alunissages lunaires.</td>
	   </tr>
		<tr>
			<td><b>SPACIT.PAS</b></td>
			<td>Cette commande permet d'examiner des fichiers SPICE.</td>
		</tr>
		<tr>
			<td><b>SPKDIFF.PAS</b></td>
			<td>Cette commande permet d'échantillonner une trajectoire ou de comparer deux trajectoires.</td>
		</tr>
		<tr>
			<td><b>SPK2OEM.PAS</b></td>
			<td>Cette commande permet d'exporter un fichier SPK vers un message d'éphémérides CCSDS OEM.</td>
		</tr>
		<tr>
			<td><b>OEM2SPK.PAS</b></td>
			<td>Cette commande permet de convertir un fichier d'éphémérides OEM vers SPK.</td>
		</tr>
		<tr>
			<td><b>TOXFR.PAS</b></td>
			<td>Cette commande permet de convertir les fichiers SPICE entre formats binaires et formats de transfert.</td>
		</tr>
</table>

<h2>Compilation</h2>
	
Les fichiers Pascal n'ont aucune dépendances, il suffit de télécharger le fichier désiré et de le compiler avec Free Pascal avec la syntaxe de commande  :

<pre><b>fpc</b> <i>LEFICHIER.PAS</i></pre>
	
Sinon, vous pouvez également le compiler avec le Turbo Pascal à l'aide de la syntaxe de commande suivante :	

<pre><b>tpc</b> <i>LEFICHIER.PAS</i></pre>
	
Par exemple, si vous voulez compiler LUNAR.PAS, vous devrez tapez la commande suivante :

<pre><b>fpc</b> LUNAR.PAS</pre>

<h2>Licence</h2>
<ul>
 <li>Le code source est publié sous la licence <a href="https://github.com/gladir/SPACE-0/blob/main/LICENSE">MIT</a>.</li>
 <li>Le paquet original est publié sous la licence <a href="https://github.com/gladir/SPACE-0/blob/main/LICENSE">MIT</a>.</li>
</ul>
