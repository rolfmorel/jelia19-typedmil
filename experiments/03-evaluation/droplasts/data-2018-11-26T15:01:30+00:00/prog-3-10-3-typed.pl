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

my_msort3(A,B):-msort(A,B).
my_list_to_set4(A,B):-list_to_set(A,B).
my_set5(A):-list_to_set(A,A).
my_head6([H|_],H).
my_pred7(A,B):-succ(B,A),A > 0.

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

my_double9(N,M):-M is 2*N,M =< 10.
my_odd10(A):-1 is A mod 2.
my_min_list11(A,B):-min_list(A,B).
my_lowercase12(A):-downcase_atom(A,A).
prim(my_tail0,[list(T),list(T)]).
prim(my_reverse1,[list(T),list(T)]).
prim(my_msort3,[list(int),list(int)]).
prim(my_list_to_set4,[list(T),list(T)]).
prim(my_set5,[list(_)]).
prim(my_head6,[list(T),T]).
prim(my_pred7,[int,int]).
prim(my_double9,[int,int]).
prim(my_odd10,[int]).
prim(my_min_list11,[list(int),int]).
prim(my_lowercase12,[char]).
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
p([['w','L','M'],['Y','j','p','k'],['s','N','P']],[['w','L'],['Y','j','p'],['s','N']]).
p([['y','z','z'],['s','K','S'],['m','c','H','L'],['K','e','S']],[['y','z'],['s','K'],['m','c','H'],['K','e']]).
p([['v','m','i','B'],['r','j','t'],['w','i','m'],['q','q','Y','j']],[['v','m','i'],['r','j'],['w','i'],['q','q','Y']]).
p([['t','a','T','E'],['P','L','C']],[['t','a','T'],['P','L']]).
p([['v','J','y'],['J','U','H','F'],['R','X','H','L']],[['v','J'],['J','U','H'],['R','X','H']]).
q([['q','Q','W','w'],['c','g','q']],[['q','Q','W','w'],['c','g']]).
q([['w','f','a'],['j','T','F'],['n','Y','W','L']],[['w','f'],['j','T','F'],['n','Y','W','L']]).
q([['h','L','C'],['x','t','n','C']],[['h','L'],['x','t','n','C']]).
q([['J','k','H'],['G','R','Y','O'],['a','s','y','v']],[['J','k','H'],['G','R','Y'],['a','s','y','v']]).
q([['H','k','b'],['U','w','g']],[['H','k'],['U','w','g']]).
