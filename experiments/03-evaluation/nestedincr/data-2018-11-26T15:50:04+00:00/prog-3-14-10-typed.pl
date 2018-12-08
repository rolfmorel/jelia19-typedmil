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
my_reverse2(A,B):-reverse(A,B).
my_list_to_set3(A,B):-list_to_set(A,B).
my_last4(A,B):-last(A,B).
my_len5(A,B):-length(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_lowercase7(A):-downcase_atom(A,A),char_code(A,_).
my_uppercase8(A):-upcase_atom(A,A),char_code(A,_).
my_tail9([_|TL],TL).
my_set10(A):-list_to_set(A,A).
my_msort11(A,B):-msort(A,B).
my_toupper12(A,B):-upcase_atom(A,B),char_code(A,_).
my_even13(A):-0 is A mod 2.
my_odd14(A):-1 is A mod 2.
my_min_list15(A,B):-min_list(A,B).
prim(my_succ1,[int,int]).
prim(my_reverse2,[list(T),list(T)]).
prim(my_list_to_set3,[list(T),list(T)]).
prim(my_last4,[list(T),T]).
prim(my_len5,[list(_),int]).
prim(my_sumlist6,[list(int),int]).
prim(my_lowercase7,[char]).
prim(my_uppercase8,[char]).
prim(my_tail9,[list(T),list(T)]).
prim(my_set10,[list(_)]).
prim(my_msort11,[list(int),list(int)]).
prim(my_toupper12,[char,char]).
prim(my_even13,[int]).
prim(my_odd14,[int]).
prim(my_min_list15,[list(int),int]).
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
p([[6,6,5],[5,2,5],[1,6,0,7]],[[8,8,7],[7,4,7],[3,8,2,9]]).
p([[3,6,6,1],[2,2,6]],[[5,8,8,3],[4,4,8]]).
p([[3,0,3,7],[7,0,1,0],[4,4,0,7]],[[5,2,5,9],[9,2,3,2],[6,6,2,9]]).
p([[0,6,0,6],[6,5,6]],[[2,8,2,8],[8,7,8]]).
p([[2,1,4,2],[1,3,5],[5,4,7]],[[4,3,6,4],[3,5,7],[7,6,9]]).
q([[5,1,1],[5,0,7],[0,5,4,1]],[[7,3,3],[5,0,7],[2,7,6,3]]).
q([[2,5,7,5],[7,4,0,5]],[[4,7,9,7],[7,4,0,5]]).
q([[3,4,3],[7,6,4],[1,1,2,0]],[[5,6,5],[7,6,4],[3,3,4,2]]).
q([[2,5,1,4],[5,4,5,4],[0,7,6,1]],[[4,7,3,6],[7,6,7,6],[0,7,6,1]]).
q([[7,0,3,0],[4,5,4],[0,3,5,2]],[[9,2,5,2],[4,5,4],[2,5,7,4]]).
