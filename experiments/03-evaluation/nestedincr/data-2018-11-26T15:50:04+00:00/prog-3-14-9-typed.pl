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
my_len2(A,B):-length(A,B).
my_uppercase3(A):-upcase_atom(A,A),char_code(A,_).
my_pred4(A,B):-succ(B,A),A > 0.
my_max_list5(A,B):-max_list(A,B).
my_element6(A,B):-member(B,A).
my_tolower7(A,B):-downcase_atom(A,B),char_code(A,_).
my_list_to_set8(A,B):-list_to_set(A,B).
my_min_list9(A,B):-min_list(A,B).
my_lowercase10(A):-downcase_atom(A,A),char_code(A,_).
my_tail11([_|TL],TL).
my_toupper12(A,B):-upcase_atom(A,B),char_code(A,_).
my_even13(A):-0 is A mod 2.
my_double14(N,M):-M is 2*N,M =< 10.
my_last15(A,B):-last(A,B).
prim(my_succ1,[int,int]).
prim(my_len2,[list(_),int]).
prim(my_uppercase3,[char]).
prim(my_pred4,[int,int]).
prim(my_max_list5,[list(int),int]).
prim(my_element6,[list(T),T]).
prim(my_tolower7,[char,char]).
prim(my_list_to_set8,[list(T),list(T)]).
prim(my_min_list9,[list(int),int]).
prim(my_lowercase10,[char]).
prim(my_tail11,[list(T),list(T)]).
prim(my_toupper12,[char,char]).
prim(my_even13,[int]).
prim(my_double14,[int,int]).
prim(my_last15,[list(T),T]).
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
p([[0,7,7],[6,5,2,7]],[[2,9,9],[8,7,4,9]]).
p([[2,7,6],[6,2,3],[1,5,0,2],[7,6,6,4]],[[4,9,8],[8,4,5],[3,7,2,4],[9,8,8,6]]).
p([[0,4,2],[3,2,7],[4,4,5,0]],[[2,6,4],[5,4,9],[6,6,7,2]]).
p([[5,0,5,3],[0,7,4],[0,5,7],[4,2,3]],[[7,2,7,5],[2,9,6],[2,7,9],[6,4,5]]).
p([[7,7,7],[1,4,5,7],[7,3,6,6]],[[9,9,9],[3,6,7,9],[9,5,8,8]]).
q([[3,3,3],[4,4,4]],[[5,5,5],[4,4,4]]).
q([[6,5,1],[4,6,2,6],[3,7,7]],[[8,7,3],[4,6,2,6],[5,9,9]]).
q([[0,2,6],[0,6,2,4],[6,4,3],[6,6,4]],[[2,4,8],[0,6,2,4],[6,4,3],[8,8,6]]).
q([[6,1,2],[3,2,5],[7,1,7],[3,6,6,0]],[[8,3,4],[3,2,5],[9,3,9],[5,8,8,2]]).
q([[5,6,0,5],[4,3,6,2],[5,1,3,0],[4,4,4,3]],[[5,6,0,5],[4,3,6,2],[7,3,5,2],[6,6,6,5]]).
