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

my_tail4([_|TL],TL).
my_pred5(A,B):-succ(B,A),A > 0.
my_len6(A,B):-length(A,B).
my_reverse7(A,B):-reverse(A,B).
my_list_to_set8(A,B):-list_to_set(A,B).
my_flatten9(A,B):-flatten(A,B).
my_succ10(A,B):-succ(A,B),B =< 10.
my_double11(N,M):-M is 2*N,M =< 10.
my_last12(A,B):-last(A,B).
my_head13([H|_],H).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_tail4/2).
prim(my_pred5/2).
prim(my_len6/2).
prim(my_reverse7/2).
prim(my_list_to_set8/2).
prim(my_flatten9/2).
prim(my_succ10/2).
prim(my_double11/2).
prim(my_last12/2).
prim(my_head13/2).
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
p(['T',z,'O','H','Q'],[t,o,h,q]).
p([b,'D','R','A','C','U','V'],[d,r,a,c,u,v]).
p(['K',q,v,'A','Q','B',v],[k,a,q,b]).
p(['V','P',s,x,'L',r,'R',d,'R'],[v,p,l,r,r]).
p(['D',m,n,'G',b,'V'],[d,g,v]).
q(['L',o,v,o,'I','I',o,'R'],[l,i,i,'X',r]).
q(['L','A',a,w],[a,'O',l]).
q(['I','L',z,q,'K',i],[i,i,k,l]).
q(['N','Q','D','X',u,'H'],[x,d,n,q,h,'C']).
q([c,f,q,n,k],['W']).
