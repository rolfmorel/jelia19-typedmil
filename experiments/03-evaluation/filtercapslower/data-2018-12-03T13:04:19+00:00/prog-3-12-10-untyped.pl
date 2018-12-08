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
my_len5(A,B):-length(A,B).
my_element6(A,B):-member(B,A).
my_list_to_set7(A,B):-list_to_set(A,B).
my_last8(A,B):-last(A,B).
my_toupper9(A,B):-upcase_atom(A,B).
my_head10([H|_],H).
my_tail11([_|TL],TL).
my_odd12(A):-1 is A mod 2.
my_double13(N,M):-M is 2*N,M =< 10.
my_max_list14(A,B):-max_list(A,B).
my_sumlist15(A,B):-sumlist(A,B).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_msort4/2).
prim(my_len5/2).
prim(my_element6/2).
prim(my_list_to_set7/2).
prim(my_last8/2).
prim(my_toupper9/2).
prim(my_head10/2).
prim(my_tail11/2).
prim(my_odd12/1).
prim(my_double13/2).
prim(my_max_list14/2).
prim(my_sumlist15/2).
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
p(['I','Y',s,a,'U'],[i,y,u]).
p([y,i,'D',w,y,'H',m,'D','E'],[d,h,d,e]).
p([t,l,d,'G','P',x],[g,p]).
p(['C','O','A','A'],[c,o,a,a]).
p(['W','T','Y',j,'U','K','Z','W'],[w,t,y,u,k,z,w]).
q(['T',l,'P',j,k,o,y,q],[p,t,'X']).
q(['A','B',z,'K',q,'L','K','O'],[b,a,k,l,x,o,k]).
q(['B',n,v,'F'],[s,f,b]).
q([l,f,d,'I','O','Z',k],['Z',z,i,o]).
q(['M',m,v,u,b],[m,d]).
