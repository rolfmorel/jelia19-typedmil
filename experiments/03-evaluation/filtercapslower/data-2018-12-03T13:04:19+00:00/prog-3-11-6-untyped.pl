:- use_module('../../metagol').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P,Q,R],([P,A,B] :- [[Q,A,C],[R,C,B]])).
%metarule(dident,[P,Q,R],([P,A,B] :- [[Q,A,B],[R,A,B]])).
metarule(tohigherorder,[P,Q,F],([P,A,B] :- [[Q,A,B,F]])).
%metarule(tailrec,[P,Q],([P,A,B]:-[[Q,A,C],[P,C,B]])).
my_uppercase0(A):-upcase_atom(A,A).
my_tolower1(A,B):-downcase_atom(A,B).

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


map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).
interpreted(map/3).

inter(map_base,([map,[],[],_]:[list(S),list(T),[S,T]]:-[])).
inter(map_ind,([map,[H1|T1],[H2|T2],F]:[list(S),list(T),[S,T]]:-[[F,H1,H2]:[S,T],[map,T1,T2,F]:[list(S),list(T),[S,T]]])).

my_odd4(A):-1 is A mod 2.
my_list_to_set5(A,B):-list_to_set(A,B).
my_len6(A,B):-length(A,B).
my_max_list7(A,B):-max_list(A,B).
my_tail8([_|TL],TL).
my_toupper9(A,B):-upcase_atom(A,B).
my_even10(A):-0 is A mod 2.
my_last11(A,B):-last(A,B).
my_double12(N,M):-M is 2*N,M =< 10.
my_lowercase13(A):-downcase_atom(A,A).
my_succ14(A,B):-succ(A,B),B =< 10.
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_odd4/1).
prim(my_list_to_set5/2).
prim(my_len6/2).
prim(my_max_list7/2).
prim(my_tail8/2).
prim(my_toupper9/2).
prim(my_even10/1).
prim(my_last11/2).
prim(my_double12/2).
prim(my_lowercase13/1).
prim(my_succ14/2).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learn(Pos,Neg,H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,False\n").
p([i,'V',i,t,m,'R',y],[v,r]).
p(['C','F',h,'U','F'],[c,f,u,f]).
p(['Z','J','L','V',c,x,'V',r,'V'],[z,j,l,v,v,v]).
p(['E','Q','F','O',g,'O',x,'C','G'],[e,q,f,o,o,c,g]).
p(['R',k,m,'P',l,c,'V'],[r,p,v]).
q([y,y,i,g,b],['S']).
q([p,'T','N','L'],[k,l,n,t]).
q([t,s,e,'E'],[e,'Z']).
q(['N',t,u,t,'V','H','E',g],[h,n,v,'D',e]).
q(['P','U','A','G'],[p,t,u,g,a]).
