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

my_element4(A,B):-member(B,A).
my_list_to_set5(A,B):-list_to_set(A,B).
my_odd6(A):-1 is A mod 2.
my_pred7(A,B):-succ(B,A),A > 0.
my_double8(N,M):-M is 2*N,M =< 10.
my_succ9(A,B):-succ(A,B),B =< 10.
my_flatten10(A,B):-flatten(A,B).
my_set11(A):-list_to_set(A,A).
my_min_list12(A,B):-min_list(A,B).
my_toupper13(A,B):-upcase_atom(A,B).
my_last14(A,B):-last(A,B).
my_reverse15(A,B):-reverse(A,B).
my_sumlist16(A,B):-sumlist(A,B).
my_msort17(A,B):-msort(A,B).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_element4/2).
prim(my_list_to_set5/2).
prim(my_odd6/1).
prim(my_pred7/2).
prim(my_double8/2).
prim(my_succ9/2).
prim(my_flatten10/2).
prim(my_set11/1).
prim(my_min_list12/2).
prim(my_toupper13/2).
prim(my_last14/2).
prim(my_reverse15/2).
prim(my_sumlist16/2).
prim(my_msort17/2).
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
p([a,v,'L','X','Q','C',y,g],[l,x,q,c]).
p([s,q,'Q','S','V',r],[q,s,v]).
p(['I','F','X',j,m,a,x,f],[i,f,x]).
p([p,r,i,'X','U'],[x,u]).
p(['G',m,'B','L','Z',o],[g,b,l,z]).
q(['B','M','X','A','W','P'],['C',w,m,b,a,x,p]).
q(['V',q,o,'Q','V',b,'M'],[m,q,v,v,'Z']).
q(['P','W',n,'H','S','T','M'],[p,s,h,m,w,t,'S']).
q(['Z','H','X','W',u],[h,z,q,w,x]).
q([b,'G','A',g,'S','E'],[e,a,r,g,s]).
