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
my_sumlist2(A,B):-sumlist(A,B).
my_flatten3(A,B):-flatten(A,B).
my_lowercase4(A):-downcase_atom(A,A),char_code(A,_).
my_len5(A,B):-length(A,B).
my_pred6(A,B):-succ(B,A),A > 0.
my_tail7([_|TL],TL).
my_set8(A):-list_to_set(A,A).
my_even9(A):-0 is A mod 2.
my_max_list10(A,B):-max_list(A,B).
my_head11([H|_],H).
my_element12(A,B):-member(B,A).
my_odd13(A):-1 is A mod 2.
my_last14(A,B):-last(A,B).
my_tolower15(A,B):-downcase_atom(A,B),char_code(A,_).
my_msort16(A,B):-msort(A,B).
my_min_list17(A,B):-min_list(A,B).
prim(my_succ1,[int,int]).
prim(my_sumlist2,[list(int),int]).
prim(my_flatten3,[list(list(T)),list(T)]).
prim(my_lowercase4,[char]).
prim(my_len5,[list(_),int]).
prim(my_pred6,[int,int]).
prim(my_tail7,[list(T),list(T)]).
prim(my_set8,[list(_)]).
prim(my_even9,[int]).
prim(my_max_list10,[list(int),int]).
prim(my_head11,[list(T),T]).
prim(my_element12,[list(T),T]).
prim(my_odd13,[int]).
prim(my_last14,[list(T),T]).
prim(my_tolower15,[char,char]).
prim(my_msort16,[list(int),list(int)]).
prim(my_min_list17,[list(int),int]).
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
p([[3,5,6],[6,1,0,0],[0,5,7]],[[5,7,8],[8,3,2,2],[2,7,9]]).
p([[3,2,2,3],[6,6,6]],[[5,4,4,5],[8,8,8]]).
p([[1,1,6],[2,3,2,5],[2,5,0],[0,1,6,1]],[[3,3,8],[4,5,4,7],[4,7,2],[2,3,8,3]]).
p([[7,0,6,4],[1,7,2],[2,6,5]],[[9,2,8,6],[3,9,4],[4,8,7]]).
p([[2,1,7],[3,4,7,7]],[[4,3,9],[5,6,9,9]]).
q([[2,0,7],[6,2,0],[5,2,2],[7,5,3]],[[2,0,7],[8,4,2],[5,2,2],[9,7,5]]).
q([[6,2,0],[6,4,4]],[[8,4,2],[6,4,4]]).
q([[6,2,3],[4,3,4],[2,0,1],[2,5,2]],[[6,2,3],[6,5,6],[2,0,1],[4,7,4]]).
q([[7,7,2,2],[3,7,5]],[[7,7,2,2],[5,9,7]]).
q([[6,3,5],[5,4,2,7]],[[8,5,7],[5,4,2,7]]).
