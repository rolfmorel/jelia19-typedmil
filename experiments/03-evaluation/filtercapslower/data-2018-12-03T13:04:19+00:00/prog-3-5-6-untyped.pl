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
my_sumlist5(A,B):-sumlist(A,B).
my_reverse6(A,B):-reverse(A,B).
my_double7(N,M):-M is 2*N,M =< 10.
my_len8(A,B):-length(A,B).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_pred4/2).
prim(my_sumlist5/2).
prim(my_reverse6/2).
prim(my_double7/2).
prim(my_len8/2).
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
p([b,d,g,e,'U','T'],[u,t]).
p([y,s,s,'E','G','E','L','N','F'],[e,g,e,l,n,f]).
p(['H','C','Z','X','J'],[h,c,z,x,j]).
p(['L',a,z,'C','V'],[l,c,v]).
p([m,'G','H','Z','S'],[g,h,z,s]).
q(['R','P','W','W',x,'O'],[p,o,r,w,w,'S']).
q([y,c,z,'T',y,q],[t,r]).
q([i,'G','Z',w,'A','J'],[j,a,g,z,'N']).
q([a,k,c,n,'W','G'],[w,g,f]).
q([q,'U',r,'M','Q'],[u,q,m,l]).
