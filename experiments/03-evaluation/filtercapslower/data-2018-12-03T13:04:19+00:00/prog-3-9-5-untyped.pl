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

my_min_list4(A,B):-min_list(A,B).
my_msort5(A,B):-msort(A,B).
my_len6(A,B):-length(A,B).
my_succ7(A,B):-succ(A,B),B =< 10.
my_odd8(A):-1 is A mod 2.
my_max_list9(A,B):-max_list(A,B).
my_double10(N,M):-M is 2*N,M =< 10.
my_tail11([_|TL],TL).
my_head12([H|_],H).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_min_list4/2).
prim(my_msort5/2).
prim(my_len6/2).
prim(my_succ7/2).
prim(my_odd8/1).
prim(my_max_list9/2).
prim(my_double10/2).
prim(my_tail11/2).
prim(my_head12/2).
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
p(['R',w,e,'J',o,f,'W','I'],[r,j,w,i]).
p([h,'C',y,z,'O',o],[c,o]).
p(['K',w,'H',d,'T','I'],[k,h,t,i]).
p(['X','K',b,q,'M'],[x,k,m]).
p(['G','A','X',i,q,q],[g,a,x]).
q(['M',l,d,g],[m,'P']).
q([c,g,'J','P',p,'P',g,'U'],[j,u,p,'U',p]).
q(['W','K',f,'Q',m,'I','B',e,'Y'],['I',b,y,k,w,i,q]).
q(['P',l,e,'C','X','Q'],[q,x,'C',p,c]).
q([h,z,r,n,'A'],['L',a]).
