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
 *  - une liste des sources de dag
*)
let obtenir_sources dag = DAG.fold_vertex (fun v l -> if (DAG.pred dag v = []) then v::l else l) dag [];;

(*
 * Obtenir tous les puits du DAG
 * INPUT :
 *  - dag : le dag dont on veut les puits
 *
 * OUTPUT :
 *  - une liste des puits de dag
*)
let obtenir_puits dag = DAG.fold_vertex (fun v l -> if (DAG.succ dag v = []) then v::l else l) dag [];;

(*
 * Obtenir les premiers éléments d'une liste
 * INPUT :
 *  - l : la liste
 *  - r : le nombre de tâches souhaité ; si r est plus grand que la taille de l, on renvoie l
 *  - m : mémoire disponible
 *
 * OUTPUT :
 *  - la liste des r premiers éléments de l
*)
let rec obtenir_premiers_elements_contrainte_memoire_bonus l r m =
    if (r = 0 || m = 0) then
        []
    else
        match l with
        | [] -> []
        | t::q ->	let mem = Vertex.memory (V.label t) in
			if (mem > m) then
				obtenir_premiers_elements_contrainte_memoire_bonus q r m
			else
				t::(obtenir_premiers_elements_contrainte_memoire_bonus q (r-1) (m-mem))
;;

(*
 * Enlever des éléments d'une liste
 * INPUT :
 *  - l : la liste
 *  - points_retires : les points à retirer de la liste
 *
 * OUTPUT :
 *  - la liste l privée des éléments de points_retirés
*)
let rec enlever_elements l points_retires =
	let retirer elt liste = List.fold_right (fun x v -> if (x = elt) then v else x::v) liste [] in
	List.fold_right (fun x v -> retirer x v) points_retires l
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

(*
 * Itérer le marquage
 * INPUT :
 *  - dag : le dag que l'on souhaite étudier
 *  - v : le vertex de l'itération
*)
let rec iteration_marquage dag v =
    Mark.set v (DAG.fold_succ (fun x r -> r+(Mark.get x)+1) dag v 0);
    DAG.iter_pred (fun x -> iteration_marquage dag x) dag v
;;

(*
 * Initialiser le marquage
 * INPUT :
 *  - dag : le dag que l'on souhaite étudier
*)
let rec initialiser_marquage dag =
    DAG.iter_vertex (fun x -> (Mark.set x (-1))) dag;
    List.iter (fun x -> iteration_marquage dag x) (obtenir_puits dag)
;;

(*
 * Comparer deux vertex
 * INPUT :
 *  - x : le 1er vertex
 *  - y : le 2ème vertex
 *
 * OUTPUT :
 *  - un entier négatif si x<y, nul si x=y, positif si x>y.
*)
let comparaison x y = (Mark.get y) - (Mark.get x);;

(*------------------------- FONCTION PRINCIPALE -------------------------*)

(*
 * Appliquer une itération de l'ordonnanceur avec heuristique et contrainte mémoire bonus
 * INPUT :
 *  - dag : le dag sur lequel appliquer l'itération
 *  - y : une file représentant Y au début de l'itération
 *  - z : une liste représentant Z au début de l'itération
 *  - r : le nombre de ressources disponibles
 * 
 * OUTPUT :
 *  - Une liste représentant Z à la fin de l'itération
 *)
 let rec iteration_ordonnanceur_contrainte_memoire_bonus dag y z r m =
    match y with
    | [] -> z
    | _ ->   let y_trie = List.fast_sort comparaison y in
                let liste_vi = obtenir_premiers_elements_contrainte_memoire_bonus y_trie r m in
                let y_moins = enlever_elements y_trie liste_vi in
                let new_z = z@[liste_vi] in
                let successeurs = supprimer_doublons (List.fold_right (fun x l -> l@(DAG.succ dag x)) liste_vi []) in
                let validation vj valides =
                    let predecesseurs_vj = DAG.pred dag vj in
                    let dans_ligne l x = List.mem x l in
                    let dans_z x = List.exists (fun v -> dans_ligne v x) new_z in
                    let valide = List.for_all dans_z predecesseurs_vj in
                    if (valide) then
                        vj::valides
                    else
                        valides
                in
                let successeurs_valides = List.fold_right validation successeurs [] in
                let new_y = y_moins@successeurs_valides in
                iteration_ordonnanceur_contrainte_memoire_bonus dag new_y new_z r m;
;;

(* entrees: 
   - un nombre entier de ressources r
   - memoire disponible M
   - un DAG
   sorties:
   - une trace d'execution du DAG 
   specifs: 
   - le DAG est suppose non pondere
   - on suppose une contrainte mémoire (section 4)
   - vous utiliserez une heuristique specifique au cas contraint
   *)
let ordonnanceur_contrainte_memoire_bonus r m dag =
    initialiser_marquage dag;
    iteration_ordonnanceur_contrainte_memoire_bonus dag (obtenir_sources dag) [] r m
;;
