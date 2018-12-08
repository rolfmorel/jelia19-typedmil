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

my_last3(A,B):-last(A,B).
my_toupper4(A,B):-upcase_atom(A,B).
my_head5([H|_],H).
my_flatten6(A,B):-flatten(A,B).
my_double7(N,M):-M is 2*N,M =< 10.
my_element8(A,B):-member(B,A).
my_sumlist9(A,B):-sumlist(A,B).
my_set10(A):-list_to_set(A,A).
my_msort11(A,B):-msort(A,B).
my_pred12(A,B):-succ(B,A),A > 0.
my_lowercase13(A):-downcase_atom(A,A).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_last3,[list(T),T]).
prim(my_toupper4,[char,char]).
prim(my_head5,[list(T),T]).
prim(my_flatten6,[list(list(T)),list(T)]).
prim(my_double7,[int,int]).
prim(my_element8,[list(T),T]).
prim(my_sumlist9,[list(int),int]).
prim(my_set10,[list(_)]).
prim(my_msort11,[list(int),list(int)]).
prim(my_pred12,[int,int]).
prim(my_lowercase13,[char]).
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
p([['h','q','V'],['C','P','R','n'],['E','j','a'],['r','o','O']],[['h','q'],['C','P','R'],['E','j'],['r','o']]).
p([['j','n','v'],['N','h','H'],['u','U','P']],[['j','n'],['N','h'],['u','U']]).
p([['p','W','Q','U'],['P','T','W','l']],[['p','W','Q'],['P','T','W']]).
p([['J','n','t'],['T','D','t'],['u','d','R','B']],[['J','n'],['T','D'],['u','d','R']]).
p([['f','T','i','M'],['i','X','e'],['o','m','I'],['R','g','W','y']],[['f','T','i'],['i','X'],['o','m'],['R','g','W']]).
q([['f','H','d','X'],['M','X','t','l'],['D','K','D','B']],[['f','H','d'],['M','X','t','l'],['D','K','D','B']]).
q([['c','E','q','T'],['u','m','z'],['q','b','k'],['D','k','t']],[['c','E','q','T'],['u','m'],['q','b','k'],['D','k']]).
q([['L','j','v','V'],['p','L','f','k']],[['L','j','v','V'],['p','L','f']]).
q([['K','x','k','H'],['X','q','b'],['c','j','e'],['t','c','J','q']],[['K','x','k'],['X','q','b'],['c','j'],['t','c','J','q']]).
q([['W','h','O','M'],['j','Y','T'],['U','I','e','O']],[['W','h','O','M'],['j','Y','T'],['U','I','e']]).
