:- use_module('../../metagol-typed').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P:[Ta,Tb],Q:[Ta,Tc],R:[Tc,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Tc],[R,C,B]:[Tc,Tb]])).
%metarule(dident,[P:[Ta,Tb],Q:[Ta,Tb],R:[Ta,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,B]:[Ta,Tb],[R,A,B]:[Ta,Tb]])).
metarule(tohigherorder,[P:[Ta,Tb],Q:[Ta,Tb,Tf],F:Tf],([P,A,B]:[Ta,Tb] :- [[Q,A,B,F]:[Ta,Tb,Tf]])).
%metarule(tailrec,[P:[Ta,Tb],Q:[Ta,Ta]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Ta],[P,C,B]:[Ta,Tb]])).
my_tail0([_|TL],TL).
my_reverse1(A,B):-reverse(A,B).

map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).
interpreted(map/3).

inter(map_base,([map,[],[],_]:[list(S),list(T),[S,T]]:-[])).
inter(map_ind,([map,[H1|T1],[H2|T2],F]:[list(S),list(T),[S,T]]:-[[F,H1,H2]:[S,T],[map,T1,T2,F]:[list(S),list(T),[S,T]]])).

my_pred3(A,B):-succ(B,A),A > 0.
my_head4([H|_],H).
my_flatten5(A,B):-flatten(A,B).
my_list_to_set6(A,B):-list_to_set(A,B).
my_last7(A,B):-last(A,B).
my_double8(N,M):-M is 2*N,M =< 10.
my_max_list9(A,B):-max_list(A,B).
my_uppercase10(A):-upcase_atom(A,A).
my_tolower11(A,B):-downcase_atom(A,B).
my_element12(A,B):-member(B,A).
my_succ13(A,B):-succ(A,B),B =< 10.
my_odd14(A):-1 is A mod 2.
my_min_list15(A,B):-min_list(A,B).
my_msort16(A,B):-msort(A,B).
my_sumlist17(A,B):-sumlist(A,B).

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

my_set19(A):-list_to_set(A,A).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_pred3,[int,int]).
prim(my_head4,[list(T),T]).
prim(my_flatten5,[list(list(T)),list(T)]).
prim(my_list_to_set6,[list(T),list(T)]).
prim(my_last7,[list(T),T]).
prim(my_double8,[int,int]).
prim(my_max_list9,[list(int),int]).
prim(my_uppercase10,[char]).
prim(my_tolower11,[char,char]).
prim(my_element12,[list(T),T]).
prim(my_succ13,[int,int]).
prim(my_odd14,[int]).
prim(my_min_list15,[list(int),int]).
prim(my_msort16,[list(int),list(int)]).
prim(my_sumlist17,[list(int),int]).
prim(my_set19,[list(_)]).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learntyped(Pos,Neg,[list(list(char)),list(list(char))],H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,True\n").
p([['y','q','l','E'],['R','E','p','M']],[['y','q','l'],['R','E','p']]).
p([['q','O','u'],['f','q','C'],['Q','u','f','H']],[['q','O'],['f','q'],['Q','u','f']]).
p([['A','D','K'],['o','f','h']],[['A','D'],['o','f']]).
p([['B','c','r','v'],['z','u','I']],[['B','c','r'],['z','u']]).
p([['j','M','L','x'],['o','W','R','d']],[['j','M','L'],['o','W','R']]).
q([['W','W','H','j'],['C','x','X','c']],[['W','W','H'],['C','x','X','c']]).
q([['F','Q','Y'],['r','S','y','E'],['s','U','u'],['D','U','g']],[['F','Q'],['r','S','y','E'],['s','U','u'],['D','U','g']]).
q([['u','q','w','C'],['Q','F','x'],['x','U','o']],[['u','q','w','C'],['Q','F','x'],['x','U']]).
q([['S','n','h'],['w','v','Q','J'],['V','S','K']],[['S','n'],['w','v','Q','J'],['V','S','K']]).
q([['P','q','F','p'],['x','f','h','d']],[['P','q','F','p'],['x','f','h']]).
