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

my_flatten4(A,B):-flatten(A,B).
my_succ5(A,B):-succ(A,B),B =< 10.
my_even6(A):-0 is A mod 2.
my_len7(A,B):-length(A,B).
my_set8(A):-list_to_set(A,A).
my_max_list9(A,B):-max_list(A,B).
my_lowercase10(A):-downcase_atom(A,A).
my_pred11(A,B):-succ(B,A),A > 0.
my_toupper12(A,B):-upcase_atom(A,B).
my_last13(A,B):-last(A,B).
my_double14(N,M):-M is 2*N,M =< 10.
my_msort15(A,B):-msort(A,B).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_flatten4/2).
prim(my_succ5/2).
prim(my_even6/1).
prim(my_len7/2).
prim(my_set8/1).
prim(my_max_list9/2).
prim(my_lowercase10/1).
prim(my_pred11/2).
prim(my_toupper12/2).
prim(my_last13/2).
prim(my_double14/2).
prim(my_msort15/2).
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
p([z,'P',h,'Y','E'],[p,y,e]).
p(['A','H','D','Z','V'],[a,h,d,z,v]).
p([x,h,y,'G'],[g]).
p([m,d,'M',u,n,z],[m]).
p(['U',l,'S',g,'J','E','W',f,n],[u,s,j,e,w]).
q(['Y','L','E','N'],[n,l,i,e,y]).
q(['X',v,u,'Y','B','L',l],[l,b,y,j,x]).
q(['R',d,'L','R','I','A'],[i,a,r,'H',l,r]).
q([o,'D',n,w,w],[l,d]).
q([e,'L','F','W',b],[w,f,l,x]).
