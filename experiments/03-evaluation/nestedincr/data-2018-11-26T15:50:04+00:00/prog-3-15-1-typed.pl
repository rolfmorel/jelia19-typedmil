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
my_even2(A):-0 is A mod 2.
my_max_list3(A,B):-max_list(A,B).
my_lowercase4(A):-downcase_atom(A,A),char_code(A,_).
my_odd5(A):-1 is A mod 2.
my_head6([H|_],H).
my_reverse7(A,B):-reverse(A,B).
my_sumlist8(A,B):-sumlist(A,B).
my_double9(N,M):-M is 2*N,M =< 10.
my_uppercase10(A):-upcase_atom(A,A),char_code(A,_).
my_set11(A):-list_to_set(A,A).

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

my_msort13(A,B):-msort(A,B).
my_flatten14(A,B):-flatten(A,B).
my_last15(A,B):-last(A,B).
my_len16(A,B):-length(A,B).
prim(my_succ1,[int,int]).
prim(my_even2,[int]).
prim(my_max_list3,[list(int),int]).
prim(my_lowercase4,[char]).
prim(my_odd5,[int]).
prim(my_head6,[list(T),T]).
prim(my_reverse7,[list(T),list(T)]).
prim(my_sumlist8,[list(int),int]).
prim(my_double9,[int,int]).
prim(my_uppercase10,[char]).
prim(my_set11,[list(_)]).
prim(my_msort13,[list(int),list(int)]).
prim(my_flatten14,[list(list(T)),list(T)]).
prim(my_last15,[list(T),T]).
prim(my_len16,[list(_),int]).
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
p([[1,4,5,5],[1,2,5],[5,4,7,7],[4,2,0,0]],[[3,6,7,7],[3,4,7],[7,6,9,9],[6,4,2,2]]).
p([[7,6,2,1],[7,4,0,0],[0,4,6,6]],[[9,8,4,3],[9,6,2,2],[2,6,8,8]]).
p([[2,6,1,0],[7,6,1]],[[4,8,3,2],[9,8,3]]).
p([[3,5,6],[6,2,1],[3,3,4]],[[5,7,8],[8,4,3],[5,5,6]]).
p([[1,1,2,4],[0,2,3],[0,0,3]],[[3,3,4,6],[2,4,5],[2,2,5]]).
q([[4,7,2,5],[0,0,4,7],[0,0,1]],[[6,9,4,7],[0,0,4,7],[2,2,3]]).
q([[5,3,7],[7,5,4,6]],[[5,3,7],[9,7,6,8]]).
q([[6,2,4],[3,7,0,1],[1,1,0]],[[8,4,6],[5,9,2,3],[1,1,0]]).
q([[3,0,5,1],[2,2,1]],[[3,0,5,1],[4,4,3]]).
q([[0,0,3,2],[5,7,0,0],[4,5,5],[4,3,6]],[[2,2,5,4],[7,9,2,2],[4,5,5],[4,3,6]]).
