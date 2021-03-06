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

my_double4(N,M):-M is 2*N,M =< 10.
my_odd5(A):-1 is A mod 2.
my_lowercase6(A):-downcase_atom(A,A).
my_even7(A):-0 is A mod 2.
my_reverse8(A,B):-reverse(A,B).
my_min_list9(A,B):-min_list(A,B).
my_toupper10(A,B):-upcase_atom(A,B).
my_last11(A,B):-last(A,B).
my_msort12(A,B):-msort(A,B).
my_flatten13(A,B):-flatten(A,B).
my_element14(A,B):-member(B,A).
my_head15([H|_],H).
my_set16(A):-list_to_set(A,A).
my_tail17([_|TL],TL).
my_sumlist18(A,B):-sumlist(A,B).
my_pred19(A,B):-succ(B,A),A > 0.
my_succ20(A,B):-succ(A,B),B =< 10.
my_max_list21(A,B):-max_list(A,B).
my_list_to_set22(A,B):-list_to_set(A,B).
my_len23(A,B):-length(A,B).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_double4,[int,int]).
prim(my_odd5,[int]).
prim(my_lowercase6,[char]).
prim(my_even7,[int]).
prim(my_reverse8,[list(T),list(T)]).
prim(my_min_list9,[list(int),int]).
prim(my_toupper10,[char,char]).
prim(my_last11,[list(T),T]).
prim(my_msort12,[list(int),list(int)]).
prim(my_flatten13,[list(list(T)),list(T)]).
prim(my_element14,[list(T),T]).
prim(my_head15,[list(T),T]).
prim(my_set16,[list(_)]).
prim(my_tail17,[list(T),list(T)]).
prim(my_sumlist18,[list(int),int]).
prim(my_pred19,[int,int]).
prim(my_succ20,[int,int]).
prim(my_max_list21,[list(int),int]).
prim(my_list_to_set22,[list(T),list(T)]).
prim(my_len23,[list(_),int]).
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
p(['J','T','U',e],[j,t,u]).
p([c,p,'E',t,'S','B',b],[e,s,b]).
p([q,'I',k,f,'H',q,l,'B'],[i,h,b]).
p(['K',n,'G','E',c,a,d],[k,g,e]).
p([l,'E',m,q,'G'],[e,g]).
q(['L',u,c,t],['Z',l]).
q([a,b,'Y',z,a,'R'],[r,q,y]).
q([d,a,'Y',q,p,n,'L','S','C'],[s,l,c,j,y]).
q([q,'X',y,g,h],['J',x]).
q(['V',y,'O','P','Y','M',t,'S'],[m,'W',v,s,o,y,p]).
