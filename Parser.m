import "Globals.m": JSON_COMMA, JSON_COLON, JSON_LEFTBRACKET, JSON_RIGHTBRACKET, JSON_LEFTBRACE, JSON_RIGHTBRACE, JSON_QUOTE, JSON_WHITESPACE, JSON_SYNTAX, FALSE_LEN, TRUE_LEN, NULL_LEN;

declare type JsonNull;

intrinsic Print(X::JsonNull)
{Print X}
  printf("JsonNull");
end intrinsic;

function lex_string(string)
    json_string := "";
    if string[1] eq JSON_QUOTE then
        string := string[2..#string];
    else
      return false, -1, string;
    end if;

      for i in [1..#string] do
        c := string[i];
        if c eq JSON_QUOTE then
            return true, json_string, string[#json_string+2.. #string];
        else
            json_string cat:= c;
        end if;
    end for;
    error("Expected end-of-string quote");
end function;

function lex_number(string)
    json_number := "";
    number_characters := [IntegerToString(d) : d in [0..9]] cat ["-", "e", "."];

    for i in [1..#string] do
      c := string[i];
        if c in number_characters then
            json_number := json_number cat c;
        else
            break;
        end if;
    end for;

    if #json_number eq 0 then
        return false, -1 , string;
    end if;
    

    rest := string[#json_number+1.. #string];

    if "." in json_number or "e" in json_number then
        test, _, _ := Regexp("^[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?$", json_number);
        if test then
          A := Split(json_number, ".e");
          R := RealField();
          d := #A[2];
          Int_part := Abs(StringToInteger(A[1]));
          Dec_part := StringToInteger(A[2]);
          first_char:=json_number[1];
          
          sign := 1;
          if first_char eq "-" then
            sign:=-1;
          end if;
          
          n:=0;
          if #A eq 3 then
            n:= StringToInteger(A[3]);
          end if;
          
          json_number := sign*(R!Int_part + R!Dec_part*10^(-d))*10^n;
          return true, json_number, rest;
        else
          error("Not a valid integer or floating point number.");
        end if;
    end if;
    
    return true, StringToInteger(json_number), rest;

end function;

function lex_bool(string)
    string_len := #string;

    if string_len ge TRUE_LEN and string[1..TRUE_LEN] eq "true" then
        return true, true, string[TRUE_LEN+1..string_len];
    elif string_len ge FALSE_LEN and string[1..FALSE_LEN] eq "false" then
        return true, false, string[FALSE_LEN+1..string_len];
    end if;
    return false, -1, string;
end function;

function lex_null(string)
    string_len := #string;

    if string_len ge NULL_LEN and string[1..NULL_LEN] eq "null" then
        return true, New(JsonNull), string[NULL_LEN+1..string_len];
    end if;
    return false, -1, string;
end function;


intrinsic lex(string::MonStgElt) -> List
{Lexer to parse JSON Code}
    tokens := [* *];

    while #string ne 0 do

        test, json_string, string := lex_string(string);
        if test then
            Append(~tokens, json_string);
            
            continue;
        end if;
        test, json_number, string := lex_number(string);
        if test then
            Append(~tokens, json_number);
            continue;
        end if;

        test, json_bool, string := lex_bool(string);
        if test then
            Append(~tokens, json_bool);
            continue;
        end if;

        test, json_null, string := lex_null(string);
        if test then
            Append(~tokens, json_null);
            continue;
        end if;

        c := string[1];

        if c in JSON_WHITESPACE then
            // Ignore whitespace
            string := string[2..#string];
        elif c in JSON_SYNTAX then
            Append(~tokens, c);
            string := string[2..#string];
        else
            error("Unexpected character: {}" cat c);
        end if;
    end while;
    return tokens;
end intrinsic;


function parse_array(tokens)
    json_array := [* *];

    t := tokens[1];
    if Type(t) eq MonStgElt and t eq JSON_RIGHTBRACKET then
        return json_array, tokens[2..#tokens];
    end if;
    while true do
        json, tokens := parse(tokens);
        Append(~json_array, json);

        t := tokens[1];
        if Type(t) eq MonStgElt and t eq JSON_RIGHTBRACKET then
            return json_array, tokens[2..#tokens];
        elif Type(t) eq MonStgElt and t ne JSON_COMMA then
            error "Expected comma after object in array";
        else
            tokens := tokens[2..#tokens];
        end if;
        
    end while;
    error "Expected end-of-array bracket";
end function;

function parse_object(tokens)
    json_object := AssociativeArray();

    t := tokens[1];
    if Type(t) eq MonStgElt and t eq JSON_RIGHTBRACE then
        return json_object, tokens[2.. #tokens];
    end if;

    while true do
        json_key := tokens[1];
        if Type(json_key) eq MonStgElt then
          tokens := tokens[2..#tokens];
        else
          error "Expected string key, got: ", Type(tokens[1]);
        end if;

        if Type(tokens[1]) eq MonStgElt and tokens[1] ne JSON_COLON then
            error "Expected colon after key in object, got: ", Type(tokens[1]);
        end if;

        json_value, tokens := parse(tokens[2..#tokens]);

        json_object[json_key] := json_value;

        t := tokens[1];
        
        if Type(t) eq MonStgElt and t eq JSON_RIGHTBRACE then
            return json_object, tokens[2..#tokens];
        elif Type(t) eq MonStgElt and t ne JSON_COMMA then
            error "Expected comma after pair in object, got: ", t;
        end if;
        tokens := tokens[2..#tokens];
    end while;
     error "Expected end-of-object bracket";
end function;

intrinsic parse(tokens::List: is_root:=false) -> .
{Parse tokens}
    t := tokens[1];

    if is_root and t ne JSON_LEFTBRACE then
        error "Root must be an object";
    end if;
    if Type(t) eq MonStgElt then
      if t eq JSON_LEFTBRACKET then
        return parse_array(tokens[2..#tokens]);
      elif t eq JSON_LEFTBRACE then
        return parse_object(tokens[2..#tokens]);
      end if;
    end if;
    return t, tokens[2..#tokens];
end intrinsic;

intrinsic json_parse(str::MonStgElt) -> .
{Parse Json string}
  tok := lex(str);
  obj := parse(tok);
  return obj;
end intrinsic;

forward ArrayToString;

function AssociativeArrayToString(obj)
  string:= JSON_LEFTBRACE;
  for s in Keys(obj) do
    string *:= JSON_QUOTE cat s cat JSON_QUOTE cat JSON_COLON cat " ";
    t := obj[s];
    if Type(t) eq MonStgElt then
      string *:= (JSON_QUOTE cat Sprint(t) cat JSON_QUOTE);
    elif Type(t) eq BoolElt then
      string *:= Sprint(t);
    elif Type(t) eq RngIntElt then
      string *:= Sprint(t);
    elif Type(t) eq FldReElt then
      //Will cause trouble
      string *:= Sprint(t);
    elif Type(t) eq Assoc then
      string *:= AssociativeArrayToString(t);
    elif Type(t) eq List then
      string *:= ArrayToString(t);
    elif Type(t) eq SeqEnum then
      string *:= ArrayToString(t);
   end if;
   string *:= JSON_COMMA cat " ";
  end for;
  string := Substring(string, 1, #string-2);
  string *:= JSON_RIGHTBRACE;
  return string;
end function;

function ArrayToString(obj)
  string:= JSON_LEFTBRACKET;
  for t in obj do
    if Type(t) eq MonStgElt then
      string *:= (JSON_QUOTE cat Sprint(t) cat JSON_QUOTE);
    elif Type(t) eq BoolElt then
      string *:= Sprint(t);
    elif Type(t) eq RngIntElt then
      string *:= Sprint(t);
    elif Type(t) eq FldReElt then
      //Will cause trouble
      string *:= Sprint(t);
    elif Type(t) eq Assoc then
      string *:= AssociativeArrayToString(t);
    elif Type(t) eq List then
      string *:= ArrayToString(t);
    elif Type(t) eq SeqEnum then
      string *:= ArrayToString(t);
   end if;
   string *:= JSON_COMMA cat " ";
  end for;
  string := Substring(string, 1, #string-2);
  string *:= JSON_RIGHTBRACKET;
  return string;
end function;

intrinsic JsonToString(obj::Assoc) -> . 
{Turn Json object into string}
  
  return AssociativeArrayToString(obj);

end intrinsic;
