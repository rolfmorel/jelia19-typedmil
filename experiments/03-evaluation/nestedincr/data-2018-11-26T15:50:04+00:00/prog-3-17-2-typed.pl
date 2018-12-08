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
my_toupper3(A,B):-upcase_atom(A,B),char_code(A,_).
my_flatten4(A,B):-flatten(A,B).
my_lowercase5(A):-downcase_atom(A,A),char_code(A,_).
my_len6(A,B):-length(A,B).
my_pred7(A,B):-succ(B,A),A > 0.
my_list_to_set8(A,B):-list_to_set(A,B).
my_last9(A,B):-last(A,B).
my_min_list10(A,B):-min_list(A,B).

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

my_head12([H|_],H).
my_max_list13(A,B):-max_list(A,B).
my_double14(N,M):-M is 2*N,M =< 10.
my_sumlist15(A,B):-sumlist(A,B).
my_tail16([_|TL],TL).
my_tolower17(A,B):-downcase_atom(A,B),char_code(A,_).
my_even18(A):-0 is A mod 2.
prim(my_succ1,[int,int]).
prim(my_reverse2,[list(T),list(T)]).
prim(my_toupper3,[char,char]).
prim(my_flatten4,[list(list(T)),list(T)]).
prim(my_lowercase5,[char]).
prim(my_len6,[list(_),int]).
prim(my_pred7,[int,int]).
prim(my_list_to_set8,[list(T),list(T)]).
prim(my_last9,[list(T),T]).
prim(my_min_list10,[list(int),int]).
prim(my_head12,[list(T),T]).
prim(my_max_list13,[list(int),int]).
prim(my_double14,[int,int]).
prim(my_sumlist15,[list(int),int]).
prim(my_tail16,[list(T),list(T)]).
prim(my_tolower17,[char,char]).
prim(my_even18,[int]).
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
p([[5,6,2,7],[0,6,4,1],[2,7,6],[1,7,2,0]],[[7,8,4,9],[2,8,6,3],[4,9,8],[3,9,4,2]]).
p([[2,2,7,7],[7,6,5]],[[4,4,9,9],[9,8,7]]).
p([[2,1,7],[6,2,7]],[[4,3,9],[8,4,9]]).
p([[6,4,1],[7,0,4],[2,3,7,6],[1,1,5]],[[8,6,3],[9,2,6],[4,5,9,8],[3,3,7]]).
p([[3,4,4],[2,6,2,7]],[[5,6,6],[4,8,4,9]]).
q([[3,2,5],[4,6,5,4],[3,2,5]],[[5,4,7],[4,6,5,4],[5,4,7]]).
q([[0,3,5,3],[5,7,5]],[[2,5,7,5],[5,7,5]]).
q([[1,2,2,6],[6,3,3,7],[6,3,5,7],[2,5,6]],[[3,4,4,8],[8,5,5,9],[6,3,5,7],[2,5,6]]).
q([[1,7,2],[2,0,0],[0,5,1,0],[3,0,6]],[[1,7,2],[4,2,2],[0,5,1,0],[5,2,8]]).
q([[1,6,3],[4,7,4,3],[7,0,5,5],[4,5,3]],[[3,8,5],[6,9,6,5],[7,0,5,5],[4,5,3]]).
