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

my_msort4(A,B):-msort(A,B).
my_even5(A):-0 is A mod 2.
my_reverse6(A,B):-reverse(A,B).
my_lowercase7(A):-downcase_atom(A,A).
my_double8(N,M):-M is 2*N,M =< 10.
my_set9(A):-list_to_set(A,A).
my_odd10(A):-1 is A mod 2.
my_tail11([_|TL],TL).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_msort4/2).
prim(my_even5/1).
prim(my_reverse6/2).
prim(my_lowercase7/1).
prim(my_double8/2).
prim(my_set9/1).
prim(my_odd10/1).
prim(my_tail11/2).
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
p(['G',h,'T',s,i,'A','W',o,'Y'],[g,t,a,w,y]).
p([o,'M','L','O',q,l,'K','T'],[m,l,o,k,t]).
p(['D','Z','A','M','Y',b],[d,z,a,m,y]).
p(['W',z,y,i,'Z'],[w,z]).
p(['N','Z','X','Y','L'],[n,z,x,y,l]).
q(['B',u,p,'J','W','F'],[j,h,f,w,b]).
q(['B',p,e,o,v,'G',k,'T','G'],[t,g,b,g,i]).
q([e,'D','E','H','E',l],[e,h,n,d,e]).
q([t,'H',j,'T',c,'A','E',n],['X',h,e,t,a]).
q([h,z,c,'N','M',j,'V'],['I',m,n,v]).
