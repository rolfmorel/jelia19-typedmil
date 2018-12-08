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

my_pred4(A,B):-succ(B,A),A > 0.
my_flatten5(A,B):-flatten(A,B).
my_element6(A,B):-member(B,A).
my_min_list7(A,B):-min_list(A,B).
my_reverse8(A,B):-reverse(A,B).
my_even9(A):-0 is A mod 2.
my_double10(N,M):-M is 2*N,M =< 10.
my_msort11(A,B):-msort(A,B).
my_len12(A,B):-length(A,B).
my_set13(A):-list_to_set(A,A).
my_head14([H|_],H).
my_succ15(A,B):-succ(A,B),B =< 10.
my_list_to_set16(A,B):-list_to_set(A,B).
my_tail17([_|TL],TL).
my_sumlist18(A,B):-sumlist(A,B).
my_odd19(A):-1 is A mod 2.
my_max_list20(A,B):-max_list(A,B).
my_toupper21(A,B):-upcase_atom(A,B).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_pred4/2).
prim(my_flatten5/2).
prim(my_element6/2).
prim(my_min_list7/2).
prim(my_reverse8/2).
prim(my_even9/1).
prim(my_double10/2).
prim(my_msort11/2).
prim(my_len12/2).
prim(my_set13/1).
prim(my_head14/2).
prim(my_succ15/2).
prim(my_list_to_set16/2).
prim(my_tail17/2).
prim(my_sumlist18/2).
prim(my_odd19/1).
prim(my_max_list20/2).
prim(my_toupper21/2).
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
p(['D',x,'M','W',v],[d,m,w]).
p(['Q','M',t,q,i,'Y','L'],[q,m,y,l]).
p(['A',x,'I','P',g,'F',a],[a,i,p,f]).
p(['Y',b,b,t,'W','O','V',w],[y,w,o,v]).
p(['H',l,'T','L','O','K',l,'L'],[h,t,l,o,k,l]).
q(['I','W',d,'Y','M'],[k,w,y,m,i]).
q(['C','X',o,q,n,'R',j],['O',c,r,x]).
q([d,s,'U',c,e,s,'S'],[t,u,s]).
q([b,g,r,'G','B',x],['V',b,g]).
q(['B',i,m,'U',y,'S',b,o],[a,b,u,s]).
