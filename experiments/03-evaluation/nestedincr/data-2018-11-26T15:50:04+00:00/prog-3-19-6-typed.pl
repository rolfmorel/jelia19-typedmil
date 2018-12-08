:- use_module('../../metagol-typed').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P:[Ta,Tb],Q:[Ta,Tc],R:[Tc,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Tc],[R,C,B]:[Tc,Tb]])).
%metarule(dident,[P:[Ta,Tb],Q:[Ta,Tb],R:[Ta,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,B]:[Ta,Tb],[R,A,B]:[Ta,Tb]])).
metarule(tohigherorder,[P:[Ta,Tb],Q:[Ta,Tb,Tf],F:Tf],([P,A,B]:[Ta,Tb] :- [[Q,A,B,F]:[Ta,Tb,Tf]])).
%metarule(tailrec,[P:[Ta,Tb],Q:[Ta,Ta]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Ta],[P,C,B]:[Ta,Tb]])).

map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).
interpreted(map/3).

inter(map_base,([map,[],[],_]:[list(S),list(T),[S,T]]:-[])).
inter(map_ind,([map,[H1|T1],[H2|T2],F]:[list(S),list(T),[S,T]]:-[[F,H1,H2]:[S,T],[map,T1,T2,F]:[list(S),list(T),[S,T]]])).

my_succ1(A,B):-succ(A,B),B =< 10.
my_double2(N,M):-M is 2*N,M =< 10.
my_even3(A):-0 is A mod 2.
my_last4(A,B):-last(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_element6(A,B):-member(B,A).
my_max_list7(A,B):-max_list(A,B).
my_len8(A,B):-length(A,B).
my_flatten9(A,B):-flatten(A,B).
my_toupper10(A,B):-upcase_atom(A,B),char_code(A,_).
my_pred11(A,B):-succ(B,A),A > 0.
my_set12(A):-list_to_set(A,A).
my_head13([H|_],H).
my_tail14([_|TL],TL).
my_reverse15(A,B):-reverse(A,B).
my_min_list16(A,B):-min_list(A,B).
my_odd17(A):-1 is A mod 2.
my_msort18(A,B):-msort(A,B).
my_uppercase19(A):-upcase_atom(A,A),char_code(A,_).
my_list_to_set20(A,B):-list_to_set(A,B).
prim(my_succ1,[int,int]).
prim(my_double2,[int,int]).
prim(my_even3,[int]).
prim(my_last4,[list(T),T]).
prim(my_sumlist5,[list(int),int]).
prim(my_element6,[list(T),T]).
prim(my_max_list7,[list(int),int]).
prim(my_len8,[list(_),int]).
prim(my_flatten9,[list(list(T)),list(T)]).
prim(my_toupper10,[char,char]).
prim(my_pred11,[int,int]).
prim(my_set12,[list(_)]).
prim(my_head13,[list(T),T]).
prim(my_tail14,[list(T),list(T)]).
prim(my_reverse15,[list(T),list(T)]).
prim(my_min_list16,[list(int),int]).
prim(my_odd17,[int]).
prim(my_msort18,[list(int),list(int)]).
prim(my_uppercase19,[char]).
prim(my_list_to_set20,[list(T),list(T)]).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learntyped(Pos,Neg,[list(list(int)),list(list(int))],H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,True\n").
p([[0,6,6,0],[7,2,3],[2,0,4],[7,5,1,1]],[[2,8,8,2],[9,4,5],[4,2,6],[9,7,3,3]]).
p([[3,7,2],[6,5,3,6],[0,7,1]],[[5,9,4],[8,7,5,8],[2,9,3]]).
p([[1,0,0,6],[5,6,2,5],[7,4,1]],[[3,2,2,8],[7,8,4,7],[9,6,3]]).
p([[4,1,2,7],[3,3,4,3],[4,5,6]],[[6,3,4,9],[5,5,6,5],[6,7,8]]).
p([[0,0,3,2],[1,4,1,7],[4,1,4,2]],[[2,2,5,4],[3,6,3,9],[6,3,6,4]]).
q([[7,6,5],[0,7,1],[2,4,4,1],[5,0,7]],[[9,8,7],[2,9,3],[2,4,4,1],[5,0,7]]).
q([[7,6,4,2],[3,1,5]],[[7,6,4,2],[5,3,7]]).
q([[2,5,2,0],[2,3,3,0]],[[4,7,4,2],[2,3,3,0]]).
q([[1,0,5,2],[1,1,3],[7,7,6]],[[3,2,7,4],[3,3,5],[7,7,6]]).
q([[5,0,7],[6,4,3,3],[6,2,6]],[[7,2,9],[6,4,3,3],[8,4,8]]).
