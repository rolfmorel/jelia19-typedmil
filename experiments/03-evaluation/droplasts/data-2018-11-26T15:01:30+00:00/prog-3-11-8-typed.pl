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

my_lowercase3(A):-downcase_atom(A,A).
my_element4(A,B):-member(B,A).
my_succ5(A,B):-succ(A,B),B =< 10.
my_head6([H|_],H).
my_min_list7(A,B):-min_list(A,B).
my_list_to_set8(A,B):-list_to_set(A,B).
my_flatten9(A,B):-flatten(A,B).
my_msort10(A,B):-msort(A,B).
my_sumlist11(A,B):-sumlist(A,B).
my_double12(N,M):-M is 2*N,M =< 10.
my_last13(A,B):-last(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_lowercase3,[char]).
prim(my_element4,[list(T),T]).
prim(my_succ5,[int,int]).
prim(my_head6,[list(T),T]).
prim(my_min_list7,[list(int),int]).
prim(my_list_to_set8,[list(T),list(T)]).
prim(my_flatten9,[list(list(T)),list(T)]).
prim(my_msort10,[list(int),list(int)]).
prim(my_sumlist11,[list(int),int]).
prim(my_double12,[int,int]).
prim(my_last13,[list(T),T]).
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
p([['E','g','q'],['I','Y','M','H'],['i','x','q','E'],['W','d','K']],[['E','g'],['I','Y','M'],['i','x','q'],['W','d']]).
p([['O','X','M'],['s','Y','q','n'],['X','G','Y','X'],['C','P','X']],[['O','X'],['s','Y','q'],['X','G','Y'],['C','P']]).
p([['i','p','E'],['P','e','L','q']],[['i','p'],['P','e','L']]).
p([['X','S','M'],['C','W','P','F'],['w','V','e','E']],[['X','S'],['C','W','P'],['w','V','e']]).
p([['t','n','B'],['N','g','y','C'],['i','R','b'],['T','g','s','a']],[['t','n'],['N','g','y'],['i','R'],['T','g','s']]).
q([['d','s','V'],['I','T','S','o'],['V','H','h'],['F','w','o','n']],[['d','s','V'],['I','T','S','o'],['V','H'],['F','w','o']]).
q([['u','Z','g','d'],['d','h','q'],['v','E','Z','R'],['D','N','I','s']],[['u','Z','g','d'],['d','h','q'],['v','E','Z','R'],['D','N','I']]).
q([['t','K','Y','h'],['z','e','e'],['J','O','Z','t'],['r','O','z']],[['t','K','Y','h'],['z','e'],['J','O','Z','t'],['r','O','z']]).
q([['g','j','P'],['y','P','t','k'],['Z','G','t','Q'],['o','U','Q','H']],[['g','j','P'],['y','P','t'],['Z','G','t','Q'],['o','U','Q','H']]).
q([['b','o','G'],['M','p','O','N'],['d','p','u','T']],[['b','o','G'],['M','p','O'],['d','p','u','T']]).
