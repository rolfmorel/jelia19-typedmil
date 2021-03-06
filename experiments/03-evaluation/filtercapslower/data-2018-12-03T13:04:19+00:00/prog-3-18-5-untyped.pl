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

my_last4(A,B):-last(A,B).
my_set5(A):-list_to_set(A,A).
my_odd6(A):-1 is A mod 2.
my_even7(A):-0 is A mod 2.
my_flatten8(A,B):-flatten(A,B).
my_double9(N,M):-M is 2*N,M =< 10.
my_min_list10(A,B):-min_list(A,B).
my_tail11([_|TL],TL).
my_sumlist12(A,B):-sumlist(A,B).
my_toupper13(A,B):-upcase_atom(A,B).
my_max_list14(A,B):-max_list(A,B).
my_len15(A,B):-length(A,B).
my_msort16(A,B):-msort(A,B).
my_head17([H|_],H).
my_lowercase18(A):-downcase_atom(A,A).
my_list_to_set19(A,B):-list_to_set(A,B).
my_element20(A,B):-member(B,A).
my_reverse21(A,B):-reverse(A,B).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_last4/2).
prim(my_set5/1).
prim(my_odd6/1).
prim(my_even7/1).
prim(my_flatten8/2).
prim(my_double9/2).
prim(my_min_list10/2).
prim(my_tail11/2).
prim(my_sumlist12/2).
prim(my_toupper13/2).
prim(my_max_list14/2).
prim(my_len15/2).
prim(my_msort16/2).
prim(my_head17/2).
prim(my_lowercase18/1).
prim(my_list_to_set19/2).
prim(my_element20/2).
prim(my_reverse21/2).
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
p([h,q,i,g,'N','L'],[n,l]).
p(['P','O','O',w],[p,o,o]).
p([l,'R',m,'Q',y,'E','R'],[r,q,e,r]).
p([s,c,'C','X','K','F'],[c,x,k,f]).
p([l,'S','B','Z','F',j],[s,b,z,f]).
q(['F','E',z,w,v,y],[f,t,e]).
q(['Y','Q','J','R','R',b,'N'],[u,y,n,r,q,j,r]).
q([m,'E',z,'A','D',k,b,w],[a,d,v,e]).
q(['L','E',u,r,'K',t,i],[l,k,e,'B']).
q([n,n,n,k,g,'C',m,f,'A'],[c,a,i]).
