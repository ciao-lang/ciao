:- use_package(pillow).

main :-
    get_form_input(Input),
    get_form_value(Input,person_name,Name),
    response(Name,Response),
    output_html([
        cgi_reply,
        start,
        title('Telephone database'),
        heading(2,'Telephone database'),
        --,
        Response,
        start_form,
        'Click here, enter name of clip member, and press Return:', 
        \\,
        input(text,[name=person_name,size=20]),
        end_form,
        end]).

response(Name, Response) :-
    form_empty_value(Name) ->
       Response = []
  ; phone(Name, Phone) ->
       Response = ['Telephone number of ',b(Name),': ',Phone,$]
  ; Response = ['No telephone number available for ',b(Name),'.',$].

phone(daniel, '336-7448').
phone(manuel, '336-7435').
phone(sacha,  '543-5316').
