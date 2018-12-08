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

my_len4(A,B):-length(A,B).
my_even5(A):-0 is A mod 2.
my_head6([H|_],H).
my_toupper7(A,B):-upcase_atom(A,B).
my_pred8(A,B):-succ(B,A),A > 0.
my_min_list9(A,B):-min_list(A,B).
my_succ10(A,B):-succ(A,B),B =< 10.
my_tail11([_|TL],TL).
my_double12(N,M):-M is 2*N,M =< 10.
my_max_list13(A,B):-max_list(A,B).
my_msort14(A,B):-msort(A,B).
my_element15(A,B):-member(B,A).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_len4/2).
prim(my_even5/1).
prim(my_head6/2).
prim(my_toupper7/2).
prim(my_pred8/2).
prim(my_min_list9/2).
prim(my_succ10/2).
prim(my_tail11/2).
prim(my_double12/2).
prim(my_max_list13/2).
prim(my_msort14/2).
prim(my_element15/2).
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
p(['X',b,z,n,'Y','V','Z',o,r],[x,y,v,z]).
p(['C','E','E',w,'X',b,'G','S','T'],[c,e,e,x,g,s,t]).
p([f,g,'C',y,q],[c]).
p([i,s,z,'S','I',t],[s,i]).
p(['Q',c,'L',o,s,q,e,'O'],[q,l,o]).
q([w,l,'Y',t,'G',m],['N',g,y]).
q(['R',z,k,'K','J'],[n,j,r,k]).
q([u,'R',t,'C',f,m,'K',l],[k,r,c,'R']).
q(['M','K',l,'T','K'],[k,k,t,'O',m]).
q([z,e,'C','J',b,t,m,'Q','N'],['V',n,j,q,c]).
