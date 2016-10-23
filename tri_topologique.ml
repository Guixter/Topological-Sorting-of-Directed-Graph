open Dag;;
open DAG;;
let print_vertex fmt v = Format.fprintf fmt "%s " (let (a, b) = V.label v in a);;
#install_printer print_vertex;;


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


(*------------------------- FONCTION PRINCIPALE -------------------------*)


(*
 * Appliquer une itération de l'algorithme de tri topologique
 * INPUT :
 *  - dag : le dag sur lequel appliquer l'itération
 *  - y : une file représentant Y au début de l'itération
 *  - z : une liste représentant Z au début de l'itération
 * 
 * OUTPUT :
 *  - Une liste représentant Z à la fin de l'itération
 *)
let rec iteration_tri_topologique dag y z =
    if (Queue.is_empty y) then
        z
    else
        begin
            let vi = Queue.take y in
            let new_z = z@[vi] in
            let successeurs_vi = DAG.succ dag vi in
            let successeurs_valides = Queue.create () in
            let validation vj =
                let predecesseurs_vj = DAG.pred dag vj in
                let dans_z x = List.mem x new_z in
                let vj_valide = List.for_all dans_z predecesseurs_vj in
                if (vj_valide) then Queue.add vj successeurs_valides
            in
            List.iter validation successeurs_vi;
            Queue.transfer successeurs_valides y;
            iteration_tri_topologique dag y new_z;
        end
;;

(* entrees: 
   - un DAG
   sorties:
   - une liste des sommets du DAG ordonnées selon un tri topologique 
   specifs: 
   - vous implementerez l'algorithme 1 de l'enonce, en utilisant un format de file pour Y (section 1)
   *)
let tri_topologique dag = iteration_tri_topologique dag (obtenir_sources dag) [];;
