(*
 * dot -Tsvg dag1.dot -o dag1.svg
 *)

open Dag;;
open DAG;;


(* Exemples fournis avec le sujet *)
let dag1 = create();;

let vs1 = V.create("s",1);;
let vp1 = V.create("p",1);;
let vi1 = V.create("i",1);;
let vd1 = V.create("d",1);;
let ve1 = V.create("e",1);;
let vr1 = V.create("r",1);;
let vb1 = V.create("b",1);;
let vc1 = V.create("c",1);;
let vf1 = V.create("f",1);;
let vg1 = V.create("g",1);;
let vh1 = V.create("h",1);;
let vj1 = V.create("j",1);;
let vk1 = V.create("k",1);;
let vl1 = V.create("l",1);;
let vm1 = V.create("m",1);;
let vn1 = V.create("n",1);;
let vo1 = V.create("o",1);;
let vq1 = V.create("q",1);;

add_vertex dag1 vb1;;
add_vertex dag1 vc1;;
add_vertex dag1 vf1;;
add_vertex dag1 vg1;;
add_vertex dag1 vh1;;
add_vertex dag1 vj1;;
add_vertex dag1 vk1;;
add_vertex dag1 vl1;;
add_vertex dag1 vm1;;
add_vertex dag1 vn1;;
add_vertex dag1 vo1;;
add_vertex dag1 vq1;;
add_vertex dag1 vs1;;
add_vertex dag1 vp1;;
add_vertex dag1 vi1;;
add_vertex dag1 vd1;;
add_vertex dag1 ve1;;
add_vertex dag1 vr1;;


add_edge dag1 vs1 vk1;;
add_edge dag1 vs1 vp1;;
add_edge dag1 vj1 vi1;;
add_edge dag1 vp1 vh1;;
add_edge dag1 vg1 vh1;;
add_edge dag1 vh1 vc1;;
add_edge dag1 vp1 vg1;;
add_edge dag1 vp1 vi1;;
add_edge dag1 vf1 vo1;;
add_edge dag1 vi1 vd1;;
add_edge dag1 vi1 vf1;;
add_edge dag1 vd1 vl1;;
add_edge dag1 vd1 ve1;;
add_edge dag1 vd1 vm1;;
add_edge dag1 vb1 vl1;;
add_edge dag1 vn1 vm1;;
add_edge dag1 ve1 vr1;;
add_edge dag1 vq1 vr1;;

dot_output dag1 "dag1.dot";;

let dag2 = create();;

let vx2 = V.create("x",1);;
let vb2 = V.create("b",1);;
let vc2 = V.create("c",1);;
let vo2 = V.create("o",1);;
let ve2 = V.create("e",1);;
let vf2 = V.create("f",1);;
let vi2 = V.create("i",1);;
let vh2 = V.create("h",1);;
let vr2 = V.create("r",1);;
let vj2 = V.create("j",1);;
let va2 = V.create("a",1);;
let vn2 = V.create("n",1);;

add_vertex dag2 vx2;;
add_vertex dag2 vb2;;
add_vertex dag2 vc2;;
add_vertex dag2 vo2;;
add_vertex dag2 ve2;;
add_vertex dag2 vf2;;
add_vertex dag2 vi2;;
add_vertex dag2 vh2;;
add_vertex dag2 vr2;;
add_vertex dag2 vj2;;
add_vertex dag2 va2;;
add_vertex dag2 vn2;;

add_edge dag2 va2 vj2;;
add_edge dag2 va2 vn2;;
add_edge dag2 vj2 vf2;;
add_edge dag2 vn2 vo2;;
add_edge dag2 vn2 vr2;;
add_edge dag2 vo2 vr2;;
add_edge dag2 vo2 ve2;;
add_edge dag2 vr2 vf2;;
add_edge dag2 ve2 vh2;;
add_edge dag2 vr2 ve2;;
add_edge dag2 ve2 vx2;;
add_edge dag2 vi2 vb2;;
add_edge dag2 vi2 vc2;;
add_edge dag2 vx2 vi2;;

dot_output dag2 "dag2.dot";;

let dag3 = create();;

let va3 = V.create("a",1);;
let vv3 = V.create("v",1);;
let vc3 = V.create("c",1);;
let vd3 = V.create("d",1);;
let ve3 = V.create("e",1);;
let vf3 = V.create("f",1);;
let vi3 = V.create("i",1);;
let vh3 = V.create("h",1);;
let vg3 = V.create("g",1);;
let vj3 = V.create("j",1);;
let vm3 = V.create("m",1);;
let vn3 = V.create("n",1);;
let vk3 = V.create("k",1);;
let vl3 = V.create("l",1);;

add_vertex dag3 va3;;
add_vertex dag3 vv3;;
add_vertex dag3 vc3;;
add_vertex dag3 vd3;;
add_vertex dag3 ve3;;
add_vertex dag3 vf3;;
add_vertex dag3 vg3;;
add_vertex dag3 vh3;;
add_vertex dag3 vi3;;
add_vertex dag3 vj3;;
add_vertex dag3 vk3;;
add_vertex dag3 vl3;;
add_vertex dag3 vm3;;
add_vertex dag3 vn3;;

add_edge dag3 vg3 vf3;;
add_edge dag3 vn3 vf3;;
add_edge dag3 vn3 va3;;
add_edge dag3 vd3 va3;;
add_edge dag3 vd3 vl3;;
add_edge dag3 vj3 vl3;;
add_edge dag3 vj3 vm3;;
add_edge dag3 vk3 vm3;;
add_edge dag3 vf3 vc3;;
add_edge dag3 va3 vc3;;
add_edge dag3 va3 vv3;;
add_edge dag3 vl3 vv3;;
add_edge dag3 vl3 ve3;;
add_edge dag3 vm3 ve3;;
add_edge dag3 vc3 vi3;;
add_edge dag3 vv3 vi3;;
add_edge dag3 vv3 vh3;;
add_edge dag3 ve3 vh3;;

dot_output dag3 "dag3.dot";;

let dag4 = create();;

let va4 = V.create("a",2);;
let vb4 = V.create("b",2);;
let vc4 = V.create("c",2);;
let vd4 = V.create("d",2);;
let ve4 = V.create("e",2);;
let vf4 = V.create("f",2);;
let vo4 = V.create("o",2);;
let vp4 = V.create("p",2);;
let vq4 = V.create("q",2);;
let vr4 = V.create("r",2);;
let vg4 = V.create("g",2);;
let vh4 = V.create("h",1);;
let vi4 = V.create("i",1);;
let vj4 = V.create("j",1);;
let vk4 = V.create("k",1);;
let vl4 = V.create("l",1);;
let vm4 = V.create("m",1);;
let vs4 = V.create("s",1);;
let vt4 = V.create("t",1);;
let vu4 = V.create("u",1);;
let vn4 = V.create("n",3);;

add_vertex dag4 va4;;
add_vertex dag4 vb4;;
add_vertex dag4 vc4;;
add_vertex dag4 vd4;;
add_vertex dag4 ve4;;
add_vertex dag4 vf4;;
add_vertex dag4 vo4;;
add_vertex dag4 vp4;;
add_vertex dag4 vq4;;
add_vertex dag4 vr4;;
add_vertex dag4 vg4;;
add_vertex dag4 vh4;;
add_vertex dag4 vi4;;
add_vertex dag4 vj4;;
add_vertex dag4 vk4;;
add_vertex dag4 vl4;;
add_vertex dag4 vm4;;
add_vertex dag4 vs4;;
add_vertex dag4 vt4;;
add_vertex dag4 vu4;;
add_vertex dag4 vn4;;

add_edge dag4 va4 vb4;;
add_edge dag4 vb4 vc4;;
add_edge dag4 vc4 vd4;;
add_edge dag4 vd4 ve4;;
add_edge dag4 ve4 vf4;;
add_edge dag4 vf4 vo4;;
add_edge dag4 vo4 vp4;;
add_edge dag4 vp4 vq4;;
add_edge dag4 vq4 vr4;;
add_edge dag4 vg4 vh4;;
add_edge dag4 vg4 vi4;;
add_edge dag4 vg4 vj4;;
add_edge dag4 vg4 vk4;;
add_edge dag4 vg4 vl4;;
add_edge dag4 vg4 vm4;;
add_edge dag4 vg4 vs4;;
add_edge dag4 vg4 vt4;;
add_edge dag4 vg4 vu4;;
add_edge dag4 vr4 vn4;;
add_edge dag4 vh4 vn4;;
add_edge dag4 vi4 vn4;;
add_edge dag4 vj4 vn4;;
add_edge dag4 vk4 vn4;;
add_edge dag4 vl4 vn4;;
add_edge dag4 vm4 vn4;;
add_edge dag4 vs4 vn4;;
add_edge dag4 vt4 vn4;;
add_edge dag4 vu4 vn4;;

dot_output dag4 "dag4.dot";;


(* Non utilisé finalement *)
let dag5 = create();;

let va5 = V.create("a",3);;
let vb5 = V.create("b",1);;
let vc5 = V.create("c",2);;
let vd5 = V.create("d",1);;
let ve5 = V.create("e",1);;
let vf5 = V.create("f",1);;
let vg5 = V.create("g",1);;
let vh5 = V.create("h",2);;
let vi5 = V.create("i",2);;
let vj5 = V.create("j",2);;

add_vertex dag5 va5;;
add_vertex dag5 vb5;;
add_vertex dag5 vc5;;
add_vertex dag5 vd5;;
add_vertex dag5 ve5;;
add_vertex dag5 vf5;;
add_vertex dag5 vg5;;
add_vertex dag5 vh5;;
add_vertex dag5 vi5;;
add_vertex dag5 vj5;;

add_edge dag5 va5 vb5;;
add_edge dag5 va5 vc5;;
add_edge dag5 vb5 vd5;;
add_edge dag5 vb5 ve5;;
add_edge dag5 vb5 vf5;;
add_edge dag5 vb5 vg5;;
add_edge dag5 vc5 vh5;;
add_edge dag5 vh5 vi5;;
add_edge dag5 vi5 vj5;;

dot_output dag5 "dag5.dot";;


(* Contre-exemple pour la question 13 *)
let dag6 = create();;

let va6 = V.create("a",4);;
let vb6 = V.create("b",4);;
let vc6 = V.create("c",4);;
let vd6 = V.create("d",1);;
let ve6 = V.create("e",1);;
let vf6 = V.create("f",3);;
let vg6 = V.create("g",2);;
let vh6 = V.create("h",2);;
let vi6 = V.create("i",1);;
let vj6 = V.create("j",0);;

add_vertex dag6 va6;;
add_vertex dag6 vb6;;
add_vertex dag6 vc6;;
add_vertex dag6 vd6;;
add_vertex dag6 ve6;;
add_vertex dag6 vf6;;
add_vertex dag6 vg6;;
add_vertex dag6 vh6;;
add_vertex dag6 vi6;;
add_vertex dag6 vj6;;

add_edge dag6 va6 vd6;;
add_edge dag6 va6 ve6;;
add_edge dag6 vb6 vd6;;
add_edge dag6 vb6 ve6;;
add_edge dag6 vc6 vd6;;
add_edge dag6 vc6 ve6;;
add_edge dag6 vd6 vj6;;
add_edge dag6 ve6 vj6;;
add_edge dag6 vf6 vh6;;
add_edge dag6 vg6 vi6;;
add_edge dag6 vh6 vi6;;
add_edge dag6 vi6 vj6;;


dot_output dag6 "dag6.dot";;


(* Contre-exemple pour la question 16 *)
let dag7 = create();;

let va7 = V.create("a",3);;
let vb7 = V.create("b",2);;
let vc7 = V.create("c",3);;
let vd7 = V.create("d",1);;
let ve7 = V.create("e",1);;
let vf7 = V.create("f",1);;
let vg7 = V.create("g",1);;

add_vertex dag7 va7;;
add_vertex dag7 vb7;;
add_vertex dag7 vc7;;
add_vertex dag7 vd7;;
add_vertex dag7 ve7;;
add_vertex dag7 vf7;;
add_vertex dag7 vg7;;

add_edge dag7 va7 vb7;;
add_edge dag7 va7 vc7;;
add_edge dag7 va7 vd7;;
add_edge dag7 vb7 ve7;;
add_edge dag7 vb7 vf7;;
add_edge dag7 vc7 vg7;;


dot_output dag7 "dag7.dot";;
