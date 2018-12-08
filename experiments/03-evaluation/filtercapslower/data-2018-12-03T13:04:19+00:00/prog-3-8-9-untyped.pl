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

my_even4(A):-0 is A mod 2.
my_list_to_set5(A,B):-list_to_set(A,B).
my_len6(A,B):-length(A,B).
my_succ7(A,B):-succ(A,B),B =< 10.
my_double8(N,M):-M is 2*N,M =< 10.
my_reverse9(A,B):-reverse(A,B).
my_odd10(A):-1 is A mod 2.
my_msort11(A,B):-msort(A,B).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_even4/1).
prim(my_list_to_set5/2).
prim(my_len6/2).
prim(my_succ7/2).
prim(my_double8/2).
prim(my_reverse9/2).
prim(my_odd10/1).
prim(my_msort11/2).
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
p([d,t,'Z',n,'H',l,a,'J','E'],[z,h,j,e]).
p(['L',j,'A','X','G'],[l,a,x,g]).
p(['X','P','R',h,'V',g,'M'],[x,p,r,v,m]).
p([n,'P',a,q,'U','B',s],[p,u,b]).
p(['P',u,'Q','A','V'],[p,q,a,v]).
q([a,'Q',u,p,'E',v,'L',u],['H',e,q,l]).
q(['K',m,x,'D',l],[k,'J',d]).
q(['Q',a,v,'Z'],['I',q,z]).
q([c,'Z','A',e],[z,'G',a]).
q(['R',c,h,'F'],[f,'F',r]).
