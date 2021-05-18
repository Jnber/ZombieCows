:-dynamic rocher/2,arbre/2, vache/4, dimitri/2,nombre_rochers/1, nombre_arbres/1, nombre_vaches/2, largeur/1, hauteur/1, tour/2.
occupe(X,Y):-rocher(X,Y).
occupe(X,Y):-arbre(X,Y).
occupe(X,Y):-vache(X,Y,_,_).
occupe(X,Y):-dimitri(X,Y).

libre(X,Y):-
largeur(L),
hauteur(H),
repeat,
X is random(L),
Y is random(H),
(not(occupe(X,Y))),!.

placer_rochers(0).

placer_rochers(N):-
N>0,
libre(X,Y),
assert(rocher(X,Y)),
N1 is N-1 ,
placer_rochers(N1).

placer_arbres(0).

placer_arbres(N):-
N>0,
libre(X,Y),
assert(arbre(X,Y)),
N1 is N-1 ,
placer_arbres(N1).

placer_vaches(_,0).

placer_vaches(R,N):-
N>0,
libre(X,Y),
assert(vache(X,Y,R,vivante)),
N1 is N-1 ,
placer_vaches(R,N1).

placer_dimitri:-
libre(X,Y),
assert(dimitri(X,Y)).

gett([],[],[]).

gett(L2,L1,L):-
  L2\==[],L1\==[],
  [X1|K]=L2,
  [Y1|K1]=L1,
  gett(K,K1,L3),
  append([[X1,Y1]],L3,L).

vaches(L):-
  findall(X,vache(X,Y,_,_),L2),
  findall(Y,vache(X,Y,_,_),L1),
  gett(L2,L1,L).

creer_zombie:-
  vaches(L),
  length(L,Len),X is random(Len),
  nth0(X,L,Val),nth0(0,Val,X1),
  nth0(1,Val,Y1),
  vache(X1,Y1,Race,vivante),
  assert(vache(X1,Y1,Race,zombie)),
  retract(vache(X1,Y1,Race,vivante)).

question(R):-
  write("Dans quelle direction déplacer Dimitri?"),
  nl,
  read(R).

zombification(X,Y):-
  X1 is X-1,
  vache(X1,Y,Race,vivante),
  assert(vache(X1,Y,Race,zombie)),
  retract(vache(X1,Y,_,vivante)).
zombification(X,Y):-
  X1 is X+1,
  vache(X1,Y,Race,vivante),
  assert(vache(X1,Y,Race,zombie)),
  retract(vache(X1,Y,_,vivante)).
zombification(X,Y):-
  Y1 is Y-1,
  vache(X,Y1,Race,vivante),
  assert(vache(X,Y1,Race,zombie))
  ,retract(vache(X,Y1,_,vivante)).
zombification(X,Y):-
  Y1 is Y+1,
  vache(X,Y1,Race,vivante),
  assert(vache(X,Y1,Race,zombie)),
  retract(vache(X,Y1,_,vivante)).
zombification(_,_).

iszombie(_,_,_,vivante).
iszombie(X,Y,_,zombie):-zombification(X,Y).

zombification([]).
zombification(L):-
  L\==[],
  [Val|K]=L,
  nth0(0,Val,X1),
  nth0(1,Val,Y1),
  vache(X1,Y1,Race,State),
  iszombie(X1,Y1,Race,State),
  zombification(K).

zombification:-
  vaches(L),
  zombification(L).
deplacement_vache(_,_,reste).
deplacement_vache(X,Y,est):-
  X1 is X+1,
  largeur(L),
  X1 < L,
  not(occupe(X1,Y)),
  vache(X,Y,Race,Etat)
  ,assert(vache(X1,Y,Race,Etat)),
  retract(vache(X,Y,_,_)).

deplacement_vache(X,Y,ouest):-
  X1 is X-1,
  X1 >= 0 ,
  not(occupe(X1,Y)),
  vache(X,Y,Race,Etat),
  assert(vache(X1,Y,Race,Etat)),
  retract(vache(X,Y,_,_)).
deplacement_vache(X,Y,nord):-
  Y1 is Y-1,
  Y1 >= 0 ,
  not(occupe(X,Y1)),
  vache(X,Y,Race,Etat),
  assert(vache(X,Y1,Race,Etat)),
  retract(vache(X,Y,_,_)).
deplacement_vache(X,Y,sud):-
  Y1 is Y+1,
  hauteur(H),
  Y1 < H ,
  not(occupe(X,Y1)),
  vache(X,Y,Race,Etat),
  assert(vache(X,Y1,Race,Etat)),
  retract(vache(X,Y,_,_)).


deplacement_vaches(R):-
  vache(X,Y,_,_),
  deplacement_vache(X,Y,R).
deplacement_vaches(_).

deplacement_dimitri(reste).
deplacement_dimitri(est):-
  dimitri(X,Y),
  X1 is X+1, largeur(L),
  X1 < L,
  not(occupe(X1,Y)),
  assert(dimitri(X1,Y)),
  retract(dimitri(X,Y)).
deplacement_dimitri(ouest):-
  dimitri(X,Y),
  X1 is X-1,
  X1 >=0,
  not(occupe(X1,Y)),
  assert(dimitri(X1,Y)),
  retract(dimitri(X,Y)).
deplacement_dimitri(nord):-
  dimitri(X,Y),
  Y1 is Y-1,
  Y1 >=0,
  not(occupe(X,Y1)),
  assert(dimitri(X,Y1)),
  retract(dimitri(X,Y)).
deplacement_dimitri(sud):-
  dimitri(X,Y),
  Y1 is Y+1,
  hauteur(H),
  Y1 <H,
  not(occupe(X,Y1)),
  assert(dimitri(X,Y1)),
  retract(dimitri(X,Y)).
verification:-
  dimitri(X,Y),
  X1 is X+1,
  not(vache(X1,Y,_,zombie)),
  X2 is X-1,not(vache(X2,Y,_,zombie)),
  Y1 is Y-1,not(vache(X,Y1,_,zombie)),
  Y2 is Y+1,not(vache(X,Y2,_,zombie)).

initialisation :-
  write("Donner l'hauteur de la maquette"),nl,
  read(N),
  assert(hauteur(N)),
  write("Donner la largeur de la maquette"),nl,
  read(N0),
  assert(largeur(N0)),
  write("Combien de rochers?"),nl,
  read(N1), assert(nombre_rochers(N1)),
  placer_rochers(N1),write("Combien d'arbre?"),nl,
  read(N2),
  assert(nombre_arbres(N2)),
  placer_arbres(N2),
  write("Combien de vaches de race brune?"),nl,
  read(N3),
  assert(nombre_vaches(brune,N3)),
  placer_vaches(brune,N3),
  write("Combien de vaches de race simmental?"),nl,
  read(N4), assert(nombre_vaches(simmental,N4)),
  placer_vaches(simmental,N4),
  write("Combien de vaches de race alpine_herens?"),nl,
  read(N5),
  placer_vaches(alpine_herens,N5),
  assert(nombre_vaches(alpins_herens,N5)),
  placer_dimitri,
  creer_zombie.

% le reste est le code prédéfini du jeu

affichage(L, _) :-
  largeur(L),
  nl.

affichage(L, H) :-
  rocher(L, H),
  print('R'),
  L_ is L + 1,
  affichage(L_, H).

affichage(L, H) :-
  arbre(L, H),
  print('T'),
  L_ is L + 1,
  affichage(L_, H).

affichage(L, H) :-
  dimitri(L, H),
  print('D'),
  L_ is L + 1,
  affichage(L_, H).

affichage(L, H) :-
  vache(L, H, brune, vivante),
  print('B'),
  L_ is L + 1,
  affichage(L_, H).
affichage(L, H) :-
  vache(L, H, brune, zombie),
  print(' b '),
  L_ is L + 1,
  affichage(L_, H).

affichage(L, H) :-
  vache(L, H, simmental, vivante),
  print('S'),
  L_ is L + 1,
  affichage(L_, H).
affichage(L, H) :-
  vache(L, H, simmental, zombie),
  print(' s '),
  L_ is L + 1,
  affichage(L_, H).

affichage(L, H) :-
  vache(L, H, alpine_herens, vivante),
  print('H'),
  L_ is L + 1,
  affichage(L_, H).
affichage(L, H) :-
  vache(L, H, alpine_herens, zombie),
  print(' h '),
  L_ is L + 1,
  affichage(L_, H).

affichage(L, H) :-
  \+ occupe(L, H),
  print('.'),
  L_ is L + 1,
  affichage(L_, H).

affichage(H) :-
  hauteur(H).

affichage(H) :-
  hauteur(HMax),
  H < HMax,
  affichage(0, H),
  H_ is H + 1,
  affichage(H_).

affichage :-
  affichage(0),!.

jouer :-
  initialisation,
  tour(0, _).

tour_(_, _) :-
  \+ verification,
  write('Dimitri s\'est fait mordre'),!.
tour_(N, _) :-
  verification,
  M is N + 1,
  tour(M, _).

tour(N, R) :-
  affichage,
  question(R),
  deplacement_dimitri(R),
  deplacement_vaches(R),
  zombification,
  tour_(N, R).
