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

my_double4(N,M):-M is 2*N,M =< 10.
my_sumlist5(A,B):-sumlist(A,B).
my_max_list6(A,B):-max_list(A,B).
my_element7(A,B):-member(B,A).
my_flatten8(A,B):-flatten(A,B).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_double4/2).
prim(my_sumlist5/2).
prim(my_max_list6/2).
prim(my_element7/2).
prim(my_flatten8/2).
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
p(['S',y,a,y],[s]).
p([d,r,'Y','R','W'],[y,r,w]).
p(['R',s,'J','Q',g,'Z','R'],[r,j,q,z,r]).
p(['T','J','A',h,'Q'],[t,j,a,q]).
p(['U',r,'T','U'],[u,t,u]).
q([p,k,'N','Y','P',e,z,'F',f],[n,f,y,'K',p]).
q(['D','F',z,'J',j,'V'],['V',v,d,j,f]).
q([r,v,'R',p,m],[r,'F']).
q(['D',v,f,n,q,u],[d,z]).
q([r,'C',w,'V',j,'L',l,'Z'],[z,r,l,c,v]).
