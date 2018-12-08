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

my_list_to_set3(A,B):-list_to_set(A,B).
my_len4(A,B):-length(A,B).
my_flatten5(A,B):-flatten(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_toupper7(A,B):-upcase_atom(A,B).
my_double8(N,M):-M is 2*N,M =< 10.
my_odd9(A):-1 is A mod 2.
my_set10(A):-list_to_set(A,A).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_list_to_set3,[list(T),list(T)]).
prim(my_len4,[list(_),int]).
prim(my_flatten5,[list(list(T)),list(T)]).
prim(my_sumlist6,[list(int),int]).
prim(my_toupper7,[char,char]).
prim(my_double8,[int,int]).
prim(my_odd9,[int]).
prim(my_set10,[list(_)]).
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
p([['Y','k','P'],['u','m','K'],['C','z','v','b'],['i','u','C']],[['Y','k'],['u','m'],['C','z','v'],['i','u']]).
p([['L','K','D'],['Y','M','y']],[['L','K'],['Y','M']]).
p([['S','b','n','S'],['T','K','v'],['o','h','m','l']],[['S','b','n'],['T','K'],['o','h','m']]).
p([['v','b','U'],['A','s','a'],['B','p','K']],[['v','b'],['A','s'],['B','p']]).
p([['z','L','K'],['L','R','y','d'],['S','F','H','u']],[['z','L'],['L','R','y'],['S','F','H']]).
q([['G','D','d','M'],['j','r','J']],[['G','D','d','M'],['j','r']]).
q([['X','p','X','S'],['N','O','q']],[['X','p','X'],['N','O','q']]).
q([['h','r','h','y'],['V','Y','x','b']],[['h','r','h'],['V','Y','x','b']]).
q([['s','m','w'],['i','M','n','r'],['B','Y','H','a'],['S','P','F','g']],[['s','m','w'],['i','M','n'],['B','Y','H'],['S','P','F','g']]).
q([['l','M','X'],['B','T','b'],['S','A','R'],['J','J','L','P']],[['l','M'],['B','T','b'],['S','A','R'],['J','J','L']]).
