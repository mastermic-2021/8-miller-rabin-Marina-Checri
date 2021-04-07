# Miller-Rabin

En étudiant la construction de modules RSA, Alice
a remarqué que l'entier suivant avait réussi un tour
du test de Miller-Rabin avant d'être reconnu composé.

```
n = 3261094062925588835428404758136483378999194406919344223109041644688373666495040937774371565010477970695317049703777451857711700780537158406770085278676707854137430248280645667314654903546862503372876263133406556788205556423492376525076877700449993724338381905441608638088805417959664183613332207925442121
```

Quel est son plus petit facteur premier ?

## Format

Vous devez écrire un programme `main.gp` en Pari/GP dont l'exécution via
```
gp -fq < main.gp
```
affiche (uniquement) la solution cherchée.

taper `make check` permet de vérifier que votre solution est bonne.

Veillez à mettre des commentaires sur votre démarche et sa validité
dans le fichier ``main.gp``.

De manière alternative, vous pouvez écrire un programme `main.c` qui
fasse la même chose.

## Validation

Il reste à faire un commit et à envoyer votre solution pour l'enregistrer
```
git add main.gp
git commit -m 'ma solution'
git push
```

