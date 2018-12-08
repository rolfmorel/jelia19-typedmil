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
my_set5(A):-list_to_set(A,A).
my_odd6(A):-1 is A mod 2.
my_msort7(A,B):-msort(A,B).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_pred4/2).
prim(my_set5/1).
prim(my_odd6/1).
prim(my_msort7/2).
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
p([g,o,x,'C',b,'N','Y'],[c,n,y]).
p([b,s,'L','U','B',w,z,'G','B'],[l,u,b,g,b]).
p([u,'E','M','B',x,'H',h,w,k],[e,m,b,h]).
p(['W','S','L',n,l,k,'H',i],[w,s,l,h]).
p(['S',y,h,'Z'],[s,z]).
q(['W',y,'J','Q',s,b,'K'],[q,j,k,w,r]).
q(['F','K','H','P',q,q,'Y'],[p,y,h,f,k,'J']).
q([c,'Z',e,c,'M','O','Y',g],[z,'H',m,y,o]).
q([l,'U','X',b],[u,'O',x]).
q([i,d,n,o,h,g,y,'I','R'],[i,r,f]).
