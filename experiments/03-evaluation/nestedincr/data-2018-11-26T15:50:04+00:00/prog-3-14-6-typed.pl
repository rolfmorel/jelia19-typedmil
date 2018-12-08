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
my_list_to_set2(A,B):-list_to_set(A,B).
my_uppercase3(A):-upcase_atom(A,A),char_code(A,_).
my_pred4(A,B):-succ(B,A),A > 0.
my_lowercase5(A):-downcase_atom(A,A),char_code(A,_).
my_head6([H|_],H).
my_tolower7(A,B):-downcase_atom(A,B),char_code(A,_).
my_last8(A,B):-last(A,B).
my_reverse9(A,B):-reverse(A,B).
my_flatten10(A,B):-flatten(A,B).
my_len11(A,B):-length(A,B).
my_tail12([_|TL],TL).
my_odd13(A):-1 is A mod 2.
my_msort14(A,B):-msort(A,B).
my_even15(A):-0 is A mod 2.
prim(my_succ1,[int,int]).
prim(my_list_to_set2,[list(T),list(T)]).
prim(my_uppercase3,[char]).
prim(my_pred4,[int,int]).
prim(my_lowercase5,[char]).
prim(my_head6,[list(T),T]).
prim(my_tolower7,[char,char]).
prim(my_last8,[list(T),T]).
prim(my_reverse9,[list(T),list(T)]).
prim(my_flatten10,[list(list(T)),list(T)]).
prim(my_len11,[list(_),int]).
prim(my_tail12,[list(T),list(T)]).
prim(my_odd13,[int]).
prim(my_msort14,[list(int),list(int)]).
prim(my_even15,[int]).
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
p([[3,2,2],[0,4,0],[1,6,4]],[[5,4,4],[2,6,2],[3,8,6]]).
p([[7,5,3,1],[3,0,1],[6,2,0,4],[6,5,0]],[[9,7,5,3],[5,2,3],[8,4,2,6],[8,7,2]]).
p([[4,4,1,3],[6,7,4,1],[5,7,4,5],[4,0,0]],[[6,6,3,5],[8,9,6,3],[7,9,6,7],[6,2,2]]).
p([[6,4,3,3],[5,2,7,6],[0,7,6],[6,0,2]],[[8,6,5,5],[7,4,9,8],[2,9,8],[8,2,4]]).
p([[1,4,7,6],[6,7,1,6],[7,3,7,5]],[[3,6,9,8],[8,9,3,8],[9,5,9,7]]).
q([[3,0,3],[6,7,0,4],[2,6,6]],[[5,2,5],[6,7,0,4],[4,8,8]]).
q([[7,0,3],[6,3,5],[3,0,7]],[[9,2,5],[6,3,5],[5,2,9]]).
q([[5,3,5],[7,5,0],[3,1,0,4],[3,6,5]],[[5,3,5],[9,7,2],[5,3,2,6],[3,6,5]]).
q([[6,1,0],[3,0,0,5],[0,3,2]],[[6,1,0],[5,2,2,7],[2,5,4]]).
q([[3,2,2],[4,6,1,3]],[[5,4,4],[4,6,1,3]]).
