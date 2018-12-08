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
my_set5(A):-list_to_set(A,A).
my_min_list6(A,B):-min_list(A,B).
my_element7(A,B):-member(B,A).
my_lowercase8(A):-downcase_atom(A,A).
my_max_list9(A,B):-max_list(A,B).
my_reverse10(A,B):-reverse(A,B).
my_pred11(A,B):-succ(B,A),A > 0.
my_even12(A):-0 is A mod 2.
my_flatten13(A,B):-flatten(A,B).
my_len14(A,B):-length(A,B).
my_last15(A,B):-last(A,B).
my_double16(N,M):-M is 2*N,M =< 10.
my_succ17(A,B):-succ(A,B),B =< 10.
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_msort4/2).
prim(my_set5/1).
prim(my_min_list6/2).
prim(my_element7/2).
prim(my_lowercase8/1).
prim(my_max_list9/2).
prim(my_reverse10/2).
prim(my_pred11/2).
prim(my_even12/1).
prim(my_flatten13/2).
prim(my_len14/2).
prim(my_last15/2).
prim(my_double16/2).
prim(my_succ17/2).
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
p([z,'B',s,k,'F',z,'T','H'],[b,f,t,h]).
p([a,q,'R',t,'F'],[r,f]).
p(['A','A','H','T',u,'Q','Y'],[a,a,h,t,q,y]).
p(['B','Z','N',w],[b,z,n]).
p([z,q,y,'Q',n,'U','B','E'],[q,u,b,e]).
q([v,'K',s,'N',w,'V'],[k,v,o,n]).
q(['J',c,'D',w,h,s,'E'],[d,j,e,'S']).
q(['Q','R','P','N','D',t,'U','G'],[g,u,p,n,'U',r,q,d]).
q([x,'K','X','S','X','B',q,'A'],[x,k,a,'L',b,x,s]).
q(['Y',k,k,c],[y,w]).
