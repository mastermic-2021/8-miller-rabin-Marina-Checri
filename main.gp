n = read("input.txt");


/* ****************************************************************************** */
/* ******************************* Explications : ******************************* */
/* ****************************************************************************** */


/*
 * Le nombre n est, d'après Alice, reconnu composé par le test de Rabin-Miller.
 * On peut en effet vérifier cela à l'aide des fonctions
 * test_miller(n) et est_temoin_de_miller(n,2).
 * Aussi, on cherche à factoriser ce nombre n.


 * D'après le cours et grace aux indications, on a une solution non triviale.

 * D'une part, on a n-1 = 2^3 * m, d'après la fonction val_dyadique (m=(n-1)>>3).
 * D'autre part, 2 est un témoin de Miller. Posons a=2.
 * Donc pour tout i de [[0, r-1]], a^{2^i m} != +- 1 [n].
 
 * On construit b = a^m = 2^m [n].

 * Alors b (= 3223960951504927716756369891467875268509886518131932299090264338451092197921323724109858561516195564589773896411712089507983921552235988315572681767849912647471667356535458606375713618592908694709401172341242602721541372768011268403589726342363312931279617652798321100361950609265695556193609186130351589) != +- 1 [n]

 * Et b_ = b^2 = a^{2^1 m} (= 1438477581641362493823055732912333520561048159443119256308750199160786379205832594105988459753995295344300252500914364729216188815475037292209360924930070922527815951806306837036540290985094366236921941491225626968296997110557077364850377552696646144983118150213875792235240939309543091974431182559525105) != +- 1[n]

 * Il s'agit d'une solution non triviale (b_) de x^2 -1 modulo n ... !!

 * On peut alors extraire une factorisation de n en écrivant :
 * n = gcd(n, lift(b_-1))*gcd(n, lift(b_+1))

*/




/* ****************************************************************************** */
/* ******************************** Fonctions : ********************************* */
/* ****************************************************************************** */


/*
 * Fonction retournant la valuation dyadique de n.
 * En fait il existe déjà une fonction en pari gp ...
*/
val_dyadique(n)= valuation(n-1, 2);
/* my(m,r);
   while(!(m&1), m=m>>1;r=r+1);
   return r;
*/


/*
 * Fonction renvoyant 1 si a est un témoin de Miller de n, faux sinon.
*/
est_temoin_de_miller(n, {a=0} ) = {
  my(r,m);
  
  if (n==2, return (0));
  if(gcd(n,a)!=1, return (0));\\print(a," n’est pas premier avec ", n); return (0));
  
  \\si a est un argument, on le regarde mod n, sinon, on en tire un au hasard mod n.
  if(a!=0, a=Mod(a,n), a=random(Mod(1,n)));

  r = val_dyadique(n-1);
  m = (n-1) >> r; \\Comme en C, on peut décaler un nombre.
  \\ n-1 = 2^r * m, m impair

  a = a^m;
  
  \\ si n premier, soit a^m = 1[n]
  if( a == 1, return(0));
  \\ soit il existe i entre 0 et r-1 tq a^{2^i m}= -1 [n]
  for(j=0,r,
    if(a==-1, return(0));
    a = a^2
    );
  \\ sinon, c'est un témoin de Miller
  return(1);
}


/*
 * Test de Miller-Rabin :
 * affiche si n est composé (car il existe un témoin de Miller)
 * ou s'il PEUT être premier (test qui est normalement probabiliste).

 * Remarque : dans cette fonction, on choisi de vérifier chaque nombre
 * plutôt que de prendre une valeur aléatoire
 * (pour s'assurer du résultat annoncé par Alice :
 * nous utiliserons cette fonction dans un but très particulier).
*/
test_miller(n, {nb_tests=0})={
  if (nb_tests==0, nb_tests = n-1);
  for(i=1, nb_tests,
    if(est_temoin_de_miller(n,i), \\on vérifie chaque nombre
      print(n " est composé : " i, " est un témoin de Miller pour ce nombre."); return(0);)
  );
  print(n, " a passé le test de Miller-Rabin ! Il est PEUT ETRE premier.");
  return (1);
};



/* ****************************************************************************** */
/* ******************************* Application : ******************************** */
/* ****************************************************************************** */


\\test_miller(n);

k = val_dyadique(n-1);
m = (n-1)>>k;
a=2;
b = Mod(a,n)^m;
b_ = b^2;

f1 = gcd(n, lift(b_-1));
f2 = gcd(n, lift(b_+1));

if(f1*f2 == n, print(min(f1,f2)));