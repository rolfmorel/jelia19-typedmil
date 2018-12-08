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

my_len4(A,B):-length(A,B).
my_even5(A):-0 is A mod 2.
my_max_list6(A,B):-max_list(A,B).
my_succ7(A,B):-succ(A,B),B =< 10.
my_set8(A):-list_to_set(A,A).
my_tail9([_|TL],TL).
my_msort10(A,B):-msort(A,B).
my_last11(A,B):-last(A,B).
my_pred12(A,B):-succ(B,A),A > 0.
my_list_to_set13(A,B):-list_to_set(A,B).
my_min_list14(A,B):-min_list(A,B).
my_head15([H|_],H).
my_sumlist16(A,B):-sumlist(A,B).
my_element17(A,B):-member(B,A).
my_odd18(A):-1 is A mod 2.
my_double19(N,M):-M is 2*N,M =< 10.
my_toupper20(A,B):-upcase_atom(A,B).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_len4,[list(_),int]).
prim(my_even5,[int]).
prim(my_max_list6,[list(int),int]).
prim(my_succ7,[int,int]).
prim(my_set8,[list(_)]).
prim(my_tail9,[list(T),list(T)]).
prim(my_msort10,[list(int),list(int)]).
prim(my_last11,[list(T),T]).
prim(my_pred12,[int,int]).
prim(my_list_to_set13,[list(T),list(T)]).
prim(my_min_list14,[list(int),int]).
prim(my_head15,[list(T),T]).
prim(my_sumlist16,[list(int),int]).
prim(my_element17,[list(T),T]).
prim(my_odd18,[int]).
prim(my_double19,[int,int]).
prim(my_toupper20,[char,char]).
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
p(['F','V','F','X','U'],[f,v,f,x,u]).
p([r,'S','F',k,s,b,k,'Y'],[s,f,y]).
p([u,m,'B','H',t],[b,h]).
p([j,z,v,'J','Y',w,t,a,l],[j,y]).
p(['S','G',s,l,c,b,r],[s,g]).
q(['D',c,'Q',i,z,i,x,'E',k],['W',e,d,q]).
q(['X',m,l,'R',b,'E','B','O','A'],[z,x,b,r,a,e,o]).
q([v,'M',k,'K',f],[m,k,u]).
q([y,a,m,'M',m,d,r],[m,s]).
q([j,r,t,'Z','B'],[z,'X',b]).
