:- use_module('../../metagol-typed').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P:[Ta,Tb],Q:[Ta,Tc],R:[Tc,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Tc],[R,C,B]:[Tc,Tb]])).
%metarule(dident,[P:[Ta,Tb],Q:[Ta,Tb],R:[Ta,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,B]:[Ta,Tb],[R,A,B]:[Ta,Tb]])).
metarule(tohigherorder,[P:[Ta,Tb],Q:[Ta,Tb,Tf],F:Tf],([P,A,B]:[Ta,Tb] :- [[Q,A,B,F]:[Ta,Tb,Tf]])).
%metarule(tailrec,[P:[Ta,Tb],Q:[Ta,Ta]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Ta],[P,C,B]:[Ta,Tb]])).
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

my_even4(A):-0 is A mod 2.
my_list_to_set5(A,B):-list_to_set(A,B).
my_len6(A,B):-length(A,B).
my_flatten7(A,B):-flatten(A,B).
my_odd8(A):-1 is A mod 2.
my_last9(A,B):-last(A,B).
my_head10([H|_],H).
my_succ11(A,B):-succ(A,B),B =< 10.
my_sumlist12(A,B):-sumlist(A,B).
my_max_list13(A,B):-max_list(A,B).
my_set14(A):-list_to_set(A,A).
my_lowercase15(A):-downcase_atom(A,A).
my_element16(A,B):-member(B,A).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_even4,[int]).
prim(my_list_to_set5,[list(T),list(T)]).
prim(my_len6,[list(_),int]).
prim(my_flatten7,[list(list(T)),list(T)]).
prim(my_odd8,[int]).
prim(my_last9,[list(T),T]).
prim(my_head10,[list(T),T]).
prim(my_succ11,[int,int]).
prim(my_sumlist12,[list(int),int]).
prim(my_max_list13,[list(int),int]).
prim(my_set14,[list(_)]).
prim(my_lowercase15,[char]).
prim(my_element16,[list(T),T]).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learntyped(Pos,Neg,[list(char),list(char)],H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,True\n").
p(['U','I',d,b,'F'],[u,i,f]).
p(['S','Q',a,e,'X',i,'F',u],[s,q,x,f]).
p(['Y',b,i,'X',l,r,w],[y,x]).
p([x,'Z','M','W',u],[z,m,w]).
p([z,o,g,l,q,'D','Z','L','Q'],[d,z,l,q]).
q([o,q,q,'X',e,n,b,e],[a,x]).
q(['K',q,k,j,c,'X',q],[k,x,'M']).
q(['L','P',u,r,k,k,'C',l],[l,v,c,p]).
q([c,n,'C',t,p],['O',c]).
q([w,'T','N','U',n],[t,u,u,n]).
