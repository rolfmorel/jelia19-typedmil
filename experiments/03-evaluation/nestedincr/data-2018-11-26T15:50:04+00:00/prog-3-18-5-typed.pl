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
my_tolower2(A,B):-downcase_atom(A,B),char_code(A,_).
my_odd3(A):-1 is A mod 2.
my_msort4(A,B):-msort(A,B).
my_element5(A,B):-member(B,A).
my_tail6([_|TL],TL).
my_list_to_set7(A,B):-list_to_set(A,B).
my_head8([H|_],H).
my_last9(A,B):-last(A,B).
my_sumlist10(A,B):-sumlist(A,B).
my_lowercase11(A):-downcase_atom(A,A),char_code(A,_).

filter([],[],_F).
filter([A|T1],[A|T2],F):-
  call(F,A),
  filter(T1,T2,F).
filter([_|T1],T2,F):-
  filter(T1,T2,F).
interpreted(filter/3).

inter(filter_base,([filter,[],[],_]:[list(T),list(T),[T]]:-[])).
inter(filter_ind_incl,([filter,[H1|T1],[H1|T2],F]:[list(T),list(T),[T]]:-[[F,H1]:[T],[filter,T1,T2,F]:[list(T),list(T),[T]]])).
inter(filter_ind_excl,([filter,[_|T1],T2,F]:[list(T),list(T),[T]]:-[[filter,T1,T2,F]:[list(T),list(T),[T]]])).

my_uppercase13(A):-upcase_atom(A,A),char_code(A,_).
my_set14(A):-list_to_set(A,A).
my_min_list15(A,B):-min_list(A,B).
my_pred16(A,B):-succ(B,A),A > 0.
my_flatten17(A,B):-flatten(A,B).
my_even18(A):-0 is A mod 2.
my_toupper19(A,B):-upcase_atom(A,B),char_code(A,_).
prim(my_succ1,[int,int]).
prim(my_tolower2,[char,char]).
prim(my_odd3,[int]).
prim(my_msort4,[list(int),list(int)]).
prim(my_element5,[list(T),T]).
prim(my_tail6,[list(T),list(T)]).
prim(my_list_to_set7,[list(T),list(T)]).
prim(my_head8,[list(T),T]).
prim(my_last9,[list(T),T]).
prim(my_sumlist10,[list(int),int]).
prim(my_lowercase11,[char]).
prim(my_uppercase13,[char]).
prim(my_set14,[list(_)]).
prim(my_min_list15,[list(int),int]).
prim(my_pred16,[int,int]).
prim(my_flatten17,[list(list(T)),list(T)]).
prim(my_even18,[int]).
prim(my_toupper19,[char,char]).
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
p([[6,3,6],[5,1,5]],[[8,5,8],[7,3,7]]).
p([[1,5,7,5],[0,0,1,0],[6,7,0]],[[3,7,9,7],[2,2,3,2],[8,9,2]]).
p([[4,6,3,3],[1,0,4,6],[7,1,5,3]],[[6,8,5,5],[3,2,6,8],[9,3,7,5]]).
p([[1,1,1,6],[6,6,5,0],[4,6,1,4],[6,5,6,3]],[[3,3,3,8],[8,8,7,2],[6,8,3,6],[8,7,8,5]]).
p([[7,7,3],[3,0,3,7]],[[9,9,5],[5,2,5,9]]).
q([[2,7,6,1],[0,3,3,1],[1,3,6]],[[2,7,6,1],[2,5,5,3],[3,5,8]]).
q([[6,5,4,4],[6,4,7],[1,4,0,2],[0,2,3]],[[6,5,4,4],[8,6,9],[3,6,2,4],[2,4,5]]).
q([[7,5,7,1],[6,0,7],[1,5,2],[7,2,3]],[[9,7,9,3],[6,0,7],[3,7,4],[9,4,5]]).
q([[7,0,6],[7,5,0],[0,0,2],[5,5,2]],[[9,2,8],[9,7,2],[0,0,2],[7,7,4]]).
q([[5,1,3,0],[3,0,4],[0,4,4,4],[2,6,1,5]],[[7,3,5,2],[3,0,4],[2,6,6,6],[4,8,3,7]]).
