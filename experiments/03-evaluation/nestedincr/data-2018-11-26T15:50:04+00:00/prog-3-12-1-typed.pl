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
my_uppercase2(A):-upcase_atom(A,A),char_code(A,_).
my_flatten3(A,B):-flatten(A,B).
my_msort4(A,B):-msort(A,B).
my_len5(A,B):-length(A,B).
my_list_to_set6(A,B):-list_to_set(A,B).
my_element7(A,B):-member(B,A).
my_even8(A):-0 is A mod 2.
my_toupper9(A,B):-upcase_atom(A,B),char_code(A,_).
my_tail10([_|TL],TL).
my_tolower11(A,B):-downcase_atom(A,B),char_code(A,_).
my_lowercase12(A):-downcase_atom(A,A),char_code(A,_).
my_sumlist13(A,B):-sumlist(A,B).
prim(my_succ1,[int,int]).
prim(my_uppercase2,[char]).
prim(my_flatten3,[list(list(T)),list(T)]).
prim(my_msort4,[list(int),list(int)]).
prim(my_len5,[list(_),int]).
prim(my_list_to_set6,[list(T),list(T)]).
prim(my_element7,[list(T),T]).
prim(my_even8,[int]).
prim(my_toupper9,[char,char]).
prim(my_tail10,[list(T),list(T)]).
prim(my_tolower11,[char,char]).
prim(my_lowercase12,[char]).
prim(my_sumlist13,[list(int),int]).
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
p([[1,1,2],[5,6,0]],[[3,3,4],[7,8,2]]).
p([[7,7,3,7],[1,0,6,1],[1,0,1,7],[5,7,7,2]],[[9,9,5,9],[3,2,8,3],[3,2,3,9],[7,9,9,4]]).
p([[1,3,4],[3,2,7,1]],[[3,5,6],[5,4,9,3]]).
p([[2,4,5,4],[3,6,5,2],[0,5,7,7],[5,2,7,5]],[[4,6,7,6],[5,8,7,4],[2,7,9,9],[7,4,9,7]]).
p([[4,3,5,2],[4,5,0,0],[5,0,2,7]],[[6,5,7,4],[6,7,2,2],[7,2,4,9]]).
q([[2,5,2],[0,5,1,3],[0,0,5],[1,2,3,6]],[[4,7,4],[0,5,1,3],[2,2,7],[3,4,5,8]]).
q([[6,1,1,7],[6,2,7]],[[8,3,3,9],[6,2,7]]).
q([[5,3,3],[0,4,6],[4,2,6]],[[7,5,5],[0,4,6],[6,4,8]]).
q([[0,1,2],[2,0,6,2],[2,2,3],[1,3,7]],[[0,1,2],[4,2,8,4],[2,2,3],[3,5,9]]).
q([[5,4,4,4],[7,5,2],[5,1,0,0],[4,0,6,1]],[[5,4,4,4],[9,7,4],[7,3,2,2],[6,2,8,3]]).
