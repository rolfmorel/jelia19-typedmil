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

my_pred4(A,B):-succ(B,A),A > 0.
my_min_list5(A,B):-min_list(A,B).
my_odd6(A):-1 is A mod 2.
my_max_list7(A,B):-max_list(A,B).
my_tail8([_|TL],TL).
my_sumlist9(A,B):-sumlist(A,B).
my_toupper10(A,B):-upcase_atom(A,B).
my_reverse11(A,B):-reverse(A,B).
my_len12(A,B):-length(A,B).
my_head13([H|_],H).
my_lowercase14(A):-downcase_atom(A,A).
my_double15(N,M):-M is 2*N,M =< 10.
my_list_to_set16(A,B):-list_to_set(A,B).
my_succ17(A,B):-succ(A,B),B =< 10.
my_msort18(A,B):-msort(A,B).
my_even19(A):-0 is A mod 2.
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_pred4,[int,int]).
prim(my_min_list5,[list(int),int]).
prim(my_odd6,[int]).
prim(my_max_list7,[list(int),int]).
prim(my_tail8,[list(T),list(T)]).
prim(my_sumlist9,[list(int),int]).
prim(my_toupper10,[char,char]).
prim(my_reverse11,[list(T),list(T)]).
prim(my_len12,[list(_),int]).
prim(my_head13,[list(T),T]).
prim(my_lowercase14,[char]).
prim(my_double15,[int,int]).
prim(my_list_to_set16,[list(T),list(T)]).
prim(my_succ17,[int,int]).
prim(my_msort18,[list(int),list(int)]).
prim(my_even19,[int]).
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
p([d,'K',o,e,m,b,t],[k]).
p(['X',g,y,d,'V','Q'],[x,v,q]).
p([s,n,f,'E','I'],[e,i]).
p([i,'W',d,'Q',p],[w,q]).
p(['Z','I','H','N','F','J'],[z,i,h,n,f,j]).
q(['H',q,w,f,p,z,'P',o,'Z'],[z,p,'T',h]).
q(['E','G','M','N','A','Z'],[e,a,g,z,'M',n,m]).
q([x,'O','I','J','Q'],[q,i,o,'L',j]).
q(['D',p,x,'W','S','C','Y',z,b],[s,'Z',c,y,d,w]).
q(['H',k,'F',p,n,s],[f,h,z]).
