% #!/usr/bin/env ciao-shell
% -*- mode: ciao; -*-

:- use_package(pillow).

:- use_module(library(aggregates)).

main :-
  get_form_input(Info),
  get_form_value(Info,mood,Mood0),
  get_form_value(Info,potatoes,Potatoes),
  get_form_value(Info,pizza,Pizza),
  get_form_value(Info,coke,Coke),
  get_form_value(Info,pass,Password),
  get_form_value(Info,text,Text),
  get_form_value(Info,menu1,Number),
  findall(Color,member(menu2=Color,Info),Colors),
  compute_reply(Mood0,Potatoes,Pizza,Coke,Password,Text,Number,Colors,Reply),
  form_default(Mood0,happy,Mood),
  output_html([
    cgi_reply,
    start,
    title('Sample Form generated from Prolog'),
    h1('Sample HTML form'),
    Reply,
    --,
    h2('This is a form:'),
    start_form,
    'Please select mood:',
    radio(mood,happy,Mood),nl,
    image('http://www.clip.dia.fi.upm.es/images/smile.happy.gif',
      [align=middle]),
    nl,
    radio(mood,sad,Mood),nl,
    image('http://www.clip.dia.fi.upm.es/images/smile.sad.gif',
      [align=middle]),
    $,
    'What ',strong(do),' you want?',\\,
    checkbox(potatoes,Potatoes), 'Potato(e)s',\\,
    checkbox(pizza,Pizza), 'Pizza',\\,
    checkbox(coke,Coke), 'Coke',
    $,
    'Write here a password:',nl,
    input(password,[name=pass,size=9,maxlength=8]),$,
    textinput(text,[rows=5,cols=20],"Write here something"),$,
    'You can choose here: ', menu(menu1,[],[one,$two,three]),nl,
    'Also here: ', menu(menu2,[multiple],[$red,$green,blue]),$,
    input(submit,[value='Send values']),nl,
    input(reset,[value='Reset values']),
    end_form,
    --,
    address('clip@dia.fi.upm.es'),
    end]).

compute_reply('',_,_,_,_,_,_,_,'') :- !.
compute_reply(Mood,Potatoes,Pizza,Coke,Passwd0,Text0,Number,Colors0,Reply) :-
  compute_food(Potatoes,Pizza,Coke,Food),
  compute_text(Text0,Text),
  compute_password(Passwd0,Passwd),
  compute_colors(Colors0,Colors),
  Reply = [
    --,
    heading(2,'Submitted data:'),
    'You are ',strong(Mood),'.',\\,
    'You want ',strong(Food),\\,
    'The password is ',tt(verbatim(Passwd)),'.',\\,
    'In the text area you wrote: ',Text,$,
    'You chose number ',tt(Number),' and color(s):',
    preformatted(Colors)].

compute_food('','','','no food.').
compute_food(on,'','','potatoes.').
compute_food('',on,'','pizza.').
compute_food('','',on,'coke.').
compute_food(on,on,'','potatoes and pizza.').
compute_food('',on,on,'pizza with coke.').
compute_food(on,'',on,'potatoes with coke.').
compute_food(on,on,on,'potatoes and pizza with coke.').

compute_text(T0,T) :-
  form_empty_value(T0), !,
    T = '{Nothing}'.
compute_text(T,preformatted(T)) :- T = [_|_], !.
compute_text(T,preformatted([T])).

compute_password(P0,P) :-
  P0 = '$empty' -> P = '{none}'; P = P0.

compute_colors(C0,C) :-
  C0 = [] -> C = [none] ; C = C0.
