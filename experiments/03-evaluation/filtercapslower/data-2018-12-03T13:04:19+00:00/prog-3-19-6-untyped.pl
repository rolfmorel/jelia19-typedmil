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

my_lowercase4(A):-downcase_atom(A,A).
my_reverse5(A,B):-reverse(A,B).
my_toupper6(A,B):-upcase_atom(A,B).
my_succ7(A,B):-succ(A,B),B =< 10.
my_even8(A):-0 is A mod 2.
my_head9([H|_],H).
my_last10(A,B):-last(A,B).
my_double11(N,M):-M is 2*N,M =< 10.
my_set12(A):-list_to_set(A,A).
my_element13(A,B):-member(B,A).
my_pred14(A,B):-succ(B,A),A > 0.
my_tail15([_|TL],TL).
my_min_list16(A,B):-min_list(A,B).
my_max_list17(A,B):-max_list(A,B).
my_flatten18(A,B):-flatten(A,B).
my_odd19(A):-1 is A mod 2.
my_list_to_set20(A,B):-list_to_set(A,B).
my_sumlist21(A,B):-sumlist(A,B).
my_len22(A,B):-length(A,B).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_lowercase4/1).
prim(my_reverse5/2).
prim(my_toupper6/2).
prim(my_succ7/2).
prim(my_even8/1).
prim(my_head9/2).
prim(my_last10/2).
prim(my_double11/2).
prim(my_set12/1).
prim(my_element13/2).
prim(my_pred14/2).
prim(my_tail15/2).
prim(my_min_list16/2).
prim(my_max_list17/2).
prim(my_flatten18/2).
prim(my_odd19/1).
prim(my_list_to_set20/2).
prim(my_sumlist21/2).
prim(my_len22/2).
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
p([s,'D',q,j],[d]).
p([s,'O',b,a,b,'K',f,'R',b],[o,k,r]).
p([h,'V',z,'S',t],[v,s]).
p([i,k,'P','T',i,'Q',f,'D','Z'],[p,t,q,d,z]).
p([i,u,n,'K',o],[k]).
q([s,'Q',j,g,'V','K',z,'L','H'],[k,q,'T',h,v,l]).
q([u,i,n,'U'],['N',u]).
q([f,'C',h,f,y,'O'],[c,e,o]).
q([l,c,b,'L'],[l,'W']).
q(['W','B','R',s,'L','O','K','K'],[k,k,o,r,w,l,b,'Y']).
