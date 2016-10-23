open Dag;;
open DAG;;
let print_vertex fmt v = Format.fprintf fmt "%s " (let (a, b) = V.label v in a);;
#install_printer print_vertex;;

type trace = (DAG.vertex list) list;;


(*------------------------- FONCTIONS AUXILIAIRES -------------------------*)


(*
 * Obtenir toutes les sources du DAG
 * INPUT :
 *  - dag : le dag dont on veut les sources
 *
 * OUTPUT :
 *  - une file des sources de dag
*)
let obtenir_sources dag =
    let q = Queue.create () in
    DAG.iter_vertex (fun v -> if (DAG.pred dag v = []) then Queue.add v q) dag;
    q
;;

(*
 * Défiler les premiers éléments d'une file
 * INPUT :
 *  - q : la file
 *  - r : le nombre de tâches à défiler ; si r est plus grand que la taille de q, on renvoie q
 *
 * OUTPUT :
 *  - la liste des r premiers éléments de q
*)
let rec defiler q r =
    if ((r <= 0) || (Queue.is_empty q)) then
        []
    else
	let x = Queue.take q in
	let reste_defilement = defiler q (r-1) in
	x::reste_defilement
;;

(*
 * Supprimer les doublons dans une liste
 * INPUT :
 *  - l : la liste
 *
 * OUTPUT :
 *  - la liste l sans doublons
*)
let supprimer_doublons l = List.fold_right
    (fun x s ->
        if (List.mem x s) then
            s
        else
            x::s
    ) l []
;;


(*------------------------- FONCTION PRINCIPALE -------------------------*)


(*
 * Appliquer une itération de l'ordonnanceur sans heuristique
 * INPUT :
 *  - dag : le dag sur lequel appliquer l'itération
 *  - y : une file représentant Y au début de l'itération
 *  - z : une liste représentant Z au début de l'itération
 *  - r : le nombre de ressources disponibles
 * 
 * OUTPUT :
 *  - Une liste représentant Z à la fin de l'itération
 *)
let rec iteration_ordonnanceur_sans_heuristique dag y z r =
    if (Queue.is_empty y) then
        z
    else
        begin
            let liste_vi = defiler y r in
            let new_z = z@[liste_vi] in
            let successeurs = supprimer_doublons (List.fold_right (fun x l -> l@(DAG.succ dag x)) liste_vi []) in
            let successeurs_valides = Queue.create () in
            let validation vj =
                let predecesseurs_vj = DAG.pred dag vj in
                let dans_ligne l x = List.mem x l in
                let dans_z x = List.exists (fun v -> dans_ligne v x) new_z in
                let vj_valide = List.for_all dans_z predecesseurs_vj in
                if (vj_valide) then Queue.add vj successeurs_valides
            in
            List.iter validation successeurs;
            Queue.transfer successeurs_valides y;
            iteration_ordonnanceur_sans_heuristique dag y new_z r;
        end
;;

(* entrees: 
   - un nombre entier de ressources r
   - un DAG
   sorties:
   - une trace d'execution du DAG 
   specifs: 
   - le DAG est suppose non pondere
   - pas de contrainte mémoire (section 3)
   - vous n'utiliserez pas d'heuristique
   *)
let ordonnanceur_sans_heuristique r dag =
    iteration_ordonnanceur_sans_heuristique dag (obtenir_sources dag) [] r
;;
