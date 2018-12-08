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

my_reverse4(A,B):-reverse(A,B).
my_toupper5(A,B):-upcase_atom(A,B).
my_even6(A):-0 is A mod 2.
my_head7([H|_],H).
my_max_list8(A,B):-max_list(A,B).
my_sumlist9(A,B):-sumlist(A,B).
my_last10(A,B):-last(A,B).
my_len11(A,B):-length(A,B).
my_flatten12(A,B):-flatten(A,B).
my_element13(A,B):-member(B,A).
my_list_to_set14(A,B):-list_to_set(A,B).
my_set15(A):-list_to_set(A,A).
my_odd16(A):-1 is A mod 2.
my_tail17([_|TL],TL).
my_min_list18(A,B):-min_list(A,B).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_reverse4,[list(T),list(T)]).
prim(my_toupper5,[char,char]).
prim(my_even6,[int]).
prim(my_head7,[list(T),T]).
prim(my_max_list8,[list(int),int]).
prim(my_sumlist9,[list(int),int]).
prim(my_last10,[list(T),T]).
prim(my_len11,[list(_),int]).
prim(my_flatten12,[list(list(T)),list(T)]).
prim(my_element13,[list(T),T]).
prim(my_list_to_set14,[list(T),list(T)]).
prim(my_set15,[list(_)]).
prim(my_odd16,[int]).
prim(my_tail17,[list(T),list(T)]).
prim(my_min_list18,[list(int),int]).
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
p(['P',c,'O',q,b,g,x,x,'E'],[p,o,e]).
p(['A','H',u,v,c],[a,h]).
p([i,'W','Y','J',r],[w,y,j]).
p([v,r,'D','M','W'],[d,m,w]).
p(['M','E',h,'H',d],[m,e,h]).
q(['D','W','E','U',l,'B','K','F'],[b,b,d,k,e,u,w,f]).
q(['M',n,n,f,'G',z,'D'],[d,g,n,m]).
q(['T','A',i,c,n,c],[a,t,q]).
q(['R',n,'Q',j,'D',z],[q,r,'A',d]).
q(['R','E',j,'M',z,b,b],[e,m,'C',r]).
