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

my_lowercase4(A):-downcase_atom(A,A).
my_tail5([_|TL],TL).
my_succ6(A,B):-succ(A,B),B =< 10.
my_list_to_set7(A,B):-list_to_set(A,B).
my_head8([H|_],H).
my_toupper9(A,B):-upcase_atom(A,B).
my_set10(A):-list_to_set(A,A).
my_last11(A,B):-last(A,B).
my_sumlist12(A,B):-sumlist(A,B).
my_max_list13(A,B):-max_list(A,B).
my_odd14(A):-1 is A mod 2.
my_flatten15(A,B):-flatten(A,B).
my_reverse16(A,B):-reverse(A,B).
my_element17(A,B):-member(B,A).
my_pred18(A,B):-succ(B,A),A > 0.
my_len19(A,B):-length(A,B).
my_even20(A):-0 is A mod 2.
my_msort21(A,B):-msort(A,B).
my_double22(N,M):-M is 2*N,M =< 10.
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_lowercase4,[char]).
prim(my_tail5,[list(T),list(T)]).
prim(my_succ6,[int,int]).
prim(my_list_to_set7,[list(T),list(T)]).
prim(my_head8,[list(T),T]).
prim(my_toupper9,[char,char]).
prim(my_set10,[list(_)]).
prim(my_last11,[list(T),T]).
prim(my_sumlist12,[list(int),int]).
prim(my_max_list13,[list(int),int]).
prim(my_odd14,[int]).
prim(my_flatten15,[list(list(T)),list(T)]).
prim(my_reverse16,[list(T),list(T)]).
prim(my_element17,[list(T),T]).
prim(my_pred18,[int,int]).
prim(my_len19,[list(_),int]).
prim(my_even20,[int]).
prim(my_msort21,[list(int),list(int)]).
prim(my_double22,[int,int]).
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
p(['A',x,'G','A','A',t,'A'],[a,g,a,a,a]).
p(['P',x,'M',x,t,j],[p,m]).
p([r,q,q,'E',o,'X',n,'D'],[e,x,d]).
p(['P','Q',j,a],[p,q]).
p([k,'U','Z',p,o,'O'],[u,z,o]).
q([m,'T',j,x,z],[q,t]).
q([s,o,'F',a,'F','P',f,j],[y,f,f,p]).
q([j,r,'X',s,'H',n,'F',h],[x,h,c,f]).
q([h,'L','K',t,y,a,t],[k,l,'T']).
q([w,f,z,v,'K',p],[k,'P']).
